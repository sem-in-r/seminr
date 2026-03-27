# Shiny module: Visual model builder with interactive canvas
# Part of seminr_app()
#
# Supports: Reflective, Composite (A/B), Higher-Order (Composite/Reflective),
#           Interaction Terms, click-to-connect path drawing
#
# Architecture notes:
# - Builder state (constructs_rv, paths_rv) is serializable for future
#   model comparison / save-load features
# - Module interface is clean for future seminrExtras integration

#' Builder Module UI
#' @param id Module namespace id
#' @keywords internal
mod_builder_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$head(
      # CSS
      shiny::tags$style(shiny::HTML(paste0(
        "#", ns("item_palette"), " .item-chip {",
        "  display: inline-block; padding: 4px 10px; margin: 2px;",
        "  border: 1px solid #333; border-radius: 4px; cursor: pointer;",
        "  background: #fff; font-size: 12px; user-select: none;",
        "}",
        "#", ns("item_palette"), " .item-chip:hover { background: #e8f4fd; }",
        "#", ns("item_palette"), " .item-chip.selected {",
        "  background: #cce5ff; border-color: #004085; font-weight: bold;",
        "}",
        "#", ns("item_palette"), " .item-chip.used {",
        "  background: #f0f0f0; color: #999; border-color: #ccc;",
        "}",
        ".construct-card {",
        "  padding: 6px 10px; margin: 3px 0; border: 1px solid #ddd;",
        "  border-radius: 4px; cursor: pointer; background: #fafafa;",
        "}",
        ".construct-card:hover { background: #e8f4fd; }",
        ".construct-card.selected { background: #cce5ff; border-color: #004085; }",
        ".path-item {",
        "  padding: 4px 8px; margin: 2px 0; display: flex; align-items: center;",
        "  justify-content: space-between; border-radius: 3px;",
        "}",
        ".path-item:hover { background: #f0f0f0; }",
        ".path-item.selected { background: #cce5ff; }",
        ".connect-banner {",
        "  background: #cce5ff; padding: 8px 12px; border-radius: 4px;",
        "  margin-bottom: 8px; font-weight: bold; display: flex;",
        "  align-items: center; justify-content: space-between;",
        "}",
        ".selection-panel {",
        "  background: #f8f9fa; border: 1px solid #ddd; border-radius: 4px;",
        "  padding: 10px; margin-top: 8px;",
        "}"
      ))),
      # Escape key handler for cancelling connection mode
      shiny::tags$script(shiny::HTML(paste0(
        "document.addEventListener('keydown', function(e) {",
        "  if (e.key === 'Escape') {",
        "    Shiny.setInputValue('", ns("escape_pressed"), "',",
        "      Date.now(), {priority: 'event'});",
        "  }",
        "});"
      )))
    ),

    shiny::fluidRow(
      # --- Sidebar ---
      shiny::column(3,
        # Section 1: Create / Edit Construct
        shiny::wellPanel(
          shiny::uiOutput(ns("panel_title")),
          shiny::selectInput(ns("construct_type"), "Type",
            choices = c(
              "Reflective" = "reflective",
              "Composite (Mode A)" = "composite_A",
              "Composite (Mode B)" = "composite_B",
              "Higher-Order Composite" = "higher_composite",
              "Higher-Order Reflective" = "higher_reflective",
              "Interaction Term" = "interaction"
            )
          ),
          # Name input — hidden for interaction type (auto-generated)
          shiny::conditionalPanel(
            condition = paste0("input['", ns("construct_type"),
                               "'] != 'interaction'"),
            shiny::textInput(ns("construct_name"), "Name",
                             placeholder = "e.g., Image")
          ),
          # Type-specific form (items / dimensions / interaction config)
          shiny::uiOutput(ns("construct_form"))
        ),

        # Section 2: Model Overview
        shiny::wellPanel(
          shiny::h4("Constructs"),
          shiny::uiOutput(ns("construct_list")),
          shiny::hr(),
          shiny::h4("Paths"),
          shiny::uiOutput(ns("path_list")),
          shiny::uiOutput(ns("path_form"))
        ),

        # Section 3: Selection Details (conditional)
        shiny::uiOutput(ns("selection_panel"))
      ),

      # --- Canvas ---
      shiny::column(9,
        shiny::uiOutput(ns("connect_banner")),
        shiny::div(
          style = "border: 1px solid #ddd; border-radius: 4px; background: #fafafa;",
          visNetwork::visNetworkOutput(ns("model_canvas"), height = "600px")
        ),
        shiny::div(
          style = paste0("padding: 8px; display: flex;",
                         " justify-content: space-between;"),
          shiny::div(
            shiny::actionButton(ns("draw_path"), "Draw Path",
                                class = "btn-default btn-sm")
          ),
          shiny::div(
            shiny::actionButton(ns("clear_model"), "Clear All",
                                class = "btn-warning btn-sm")
          )
        )
      )
    )
  )
}

#' Builder Module Server
#' @param id Module namespace id
#' @param data_reactive Reactive data.frame from data module
#' @return A reactive list with elements measurement_model and structural_model
#' @keywords internal
mod_builder_server <- function(id, data_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ================================================================
    # Reactive State
    # ================================================================
    # Core model state — serializable for future model comparison
    constructs_rv <- shiny::reactiveVal(list())
    paths_rv <- shiny::reactiveVal(
      data.frame(from = character(), to = character(),
                 stringsAsFactors = FALSE)
    )

    # UI state
    selected_items_rv <- shiny::reactiveVal(character())
    selected_node_rv <- shiny::reactiveVal(NULL)
    selected_edge_rv <- shiny::reactiveVal(NULL)
    connect_mode_rv <- shiny::reactiveVal(FALSE)
    connect_source_rv <- shiny::reactiveVal(NULL)
    editing_rv <- shiny::reactiveVal(NULL)  # NULL = create, name = editing

    # ================================================================
    # Derived Reactives
    # ================================================================
    construct_names_reactive <- shiny::reactive({
      csts <- constructs_rv()
      if (length(csts) == 0) return(character())
      vapply(csts, function(c) c$name, character(1))
    })

    # Items used by regular constructs (not HOC or interaction)
    used_items_reactive <- shiny::reactive({
      csts <- constructs_rv()
      if (length(csts) == 0) return(character())
      regular <- Filter(
        function(c) c$type %in% c("reflective", "composite_A", "composite_B"),
        csts
      )
      unique(unlist(lapply(regular, function(c) c$items)))
    })

    available_columns <- shiny::reactive({
      d <- data_reactive()
      if (is.null(d)) return(character())
      colnames(d)
    })

    # Non-HOC, non-interaction construct names (for HOC dims / interaction)
    regular_construct_names <- shiny::reactive({
      csts <- constructs_rv()
      if (length(csts) == 0) return(character())
      regular <- Filter(
        function(c) c$type %in% c("reflective", "composite_A", "composite_B",
                                   "higher_composite", "higher_reflective"),
        csts
      )
      vapply(regular, function(c) c$name, character(1))
    })

    # ================================================================
    # UI: Panel Title (Create vs Edit)
    # ================================================================
    output$panel_title <- shiny::renderUI({
      editing <- editing_rv()
      if (is.null(editing)) {
        shiny::h4("Create Construct")
      } else {
        shiny::h4(paste0("Edit: ", editing))
      }
    })

    # ================================================================
    # UI: Construct Form (type-specific inputs + action button)
    # ================================================================
    output$construct_form <- shiny::renderUI({
      type <- input$construct_type
      if (is.null(type)) return(NULL)

      editing <- editing_rv()
      edit_cst <- NULL
      if (!is.null(editing)) {
        edit_cst <- Find(function(c) c$name == editing, constructs_rv())
      }

      elements <- list()

      if (type %in% c("reflective", "composite_A", "composite_B")) {
        # --- Item palette ---
        elements <- c(elements, list(
          shiny::helpText("Select indicators from your data:"),
          shiny::uiOutput(ns("item_palette"))
        ))

      } else if (type %in% c("higher_composite", "higher_reflective")) {
        # --- HOC dimension selector ---
        choices <- construct_names_reactive()
        if (!is.null(editing)) choices <- setdiff(choices, editing)
        # Exclude interaction constructs
        csts <- constructs_rv()
        intxn_names <- vapply(
          Filter(function(c) c$type == "interaction", csts),
          function(c) c$name, character(1)
        )
        choices <- setdiff(choices, intxn_names)

        selected_dims <- character()
        if (!is.null(edit_cst) &&
            edit_cst$type %in% c("higher_composite", "higher_reflective")) {
          selected_dims <- edit_cst$items
        }

        if (length(choices) == 0) {
          elements <- c(elements, list(
            shiny::helpText("Create regular constructs first to use as dimensions.")
          ))
        } else {
          elements <- c(elements, list(
            shiny::helpText("Select dimension constructs:"),
            shiny::checkboxGroupInput(ns("hoc_dimensions"), NULL,
                                      choices = choices,
                                      selected = selected_dims)
          ))
        }

      } else if (type == "interaction") {
        # --- Interaction config ---
        choices <- regular_construct_names()

        iv_sel <- if (!is.null(edit_cst) &&
                      edit_cst$type == "interaction") edit_cst$iv else NULL
        mod_sel <- if (!is.null(edit_cst) &&
                       edit_cst$type == "interaction") edit_cst$moderator else NULL
        method_sel <- if (!is.null(edit_cst) &&
                          edit_cst$type == "interaction") {
          edit_cst$method
        } else {
          "two_stage"
        }

        if (length(choices) < 2) {
          elements <- c(elements, list(
            shiny::helpText("Create at least 2 constructs first.")
          ))
        } else {
          elements <- c(elements, list(
            shiny::helpText("Name auto-generated as 'IV*Moderator'."),
            shiny::selectInput(ns("intxn_iv"), "Independent Variable",
                               choices = choices, selected = iv_sel),
            shiny::selectInput(ns("intxn_moderator"), "Moderator",
                               choices = choices, selected = mod_sel),
            shiny::selectInput(ns("intxn_method"), "Method",
              choices = c("Two Stage" = "two_stage",
                          "Product Indicator" = "product_indicator",
                          "Orthogonal" = "orthogonal"),
              selected = method_sel
            )
          ))
        }
      }

      # Action button
      btn_label <- if (!is.null(editing)) "Save Changes" else "Create Construct"
      elements <- c(elements, list(
        shiny::actionButton(ns("add_construct"), btn_label,
                            class = "btn-primary btn-sm",
                            style = "width: 100%; margin-top: 8px;")
      ))

      # Cancel link when editing
      if (!is.null(editing)) {
        elements <- c(elements, list(
          shiny::div(style = "text-align: center; margin-top: 4px;",
            shiny::actionLink(ns("cancel_edit"), "Cancel editing")
          )
        ))
      }

      do.call(shiny::tagList, elements)
    })

    # ================================================================
    # UI: Item Palette
    # ================================================================
    output$item_palette <- shiny::renderUI({
      cols <- available_columns()
      if (length(cols) == 0) {
        return(shiny::helpText("Load data first."))
      }

      used <- used_items_reactive()
      selected <- selected_items_rv()

      chips <- lapply(cols, function(col) {
        css_class <- "item-chip"
        if (col %in% used) css_class <- paste(css_class, "used")
        if (col %in% selected) css_class <- paste(css_class, "selected")

        shiny::tags$span(
          class = css_class,
          onclick = paste0(
            "Shiny.setInputValue('", ns("toggle_item"),
            "', {col: '", col, "', ts: Date.now()}, {priority: 'event'})"
          ),
          col
        )
      })

      shiny::div(
        id = ns("item_palette"),
        style = "max-height: 200px; overflow-y: auto; padding: 4px;",
        chips
      )
    })

    # Toggle item selection
    shiny::observeEvent(input$toggle_item, {
      col <- input$toggle_item$col
      current <- selected_items_rv()
      if (col %in% current) {
        selected_items_rv(setdiff(current, col))
      } else {
        selected_items_rv(c(current, col))
      }
    })

    # ================================================================
    # UI: Construct List
    # ================================================================
    output$construct_list <- shiny::renderUI({
      csts <- constructs_rv()
      sel <- selected_node_rv()

      if (length(csts) == 0) {
        return(shiny::helpText("No constructs yet."))
      }

      badge_style <- "color:#fff; padding:1px 5px; border-radius:3px; font-size:10px;"
      cards <- lapply(csts, function(cst) {
        badge <- switch(cst$type,
          "reflective" = shiny::tags$span(
            style = paste0("background:#6c757d;", badge_style), "R"),
          "composite_A" = shiny::tags$span(
            style = paste0("background:#17a2b8;", badge_style), "A"),
          "composite_B" = shiny::tags$span(
            style = paste0("background:#ffc107;color:#333;", badge_style), "B"),
          "higher_composite" = shiny::tags$span(
            style = paste0("background:#e67e22;", badge_style), "HC"),
          "higher_reflective" = shiny::tags$span(
            style = paste0("background:#9b59b6;", badge_style), "HR"),
          "interaction" = shiny::tags$span(
            style = paste0("background:#27ae60;", badge_style), "\u00d7"),
          NULL
        )

        desc <- if (cst$type == "interaction") {
          paste0("IV: ", cst$iv, ", Mod: ", cst$moderator)
        } else if (cst$type %in% c("higher_composite", "higher_reflective")) {
          paste0("\u2192 ", paste(cst$items, collapse = ", "))
        } else {
          paste(cst$items, collapse = ", ")
        }

        css <- if (identical(sel, cst$name)) {
          "construct-card selected"
        } else {
          "construct-card"
        }

        shiny::div(
          class = css,
          onclick = paste0(
            "Shiny.setInputValue('", ns("select_construct"),
            "', {name: '", cst$name,
            "', ts: Date.now()}, {priority: 'event'})"
          ),
          shiny::tags$strong(cst$name), " ", badge,
          shiny::tags$br(),
          shiny::tags$small(style = "color: #666;", desc)
        )
      })

      do.call(shiny::tagList, cards)
    })

    # ================================================================
    # UI: Path List (with inline delete)
    # ================================================================
    output$path_list <- shiny::renderUI({
      p <- paths_rv()
      sel_edge <- selected_edge_rv()

      if (nrow(p) == 0) {
        return(shiny::helpText(
          "No paths yet. Use 'Draw Path' on the canvas or the form below."
        ))
      }

      items <- lapply(seq_len(nrow(p)), function(i) {
        css <- if (identical(sel_edge, i)) "path-item selected" else "path-item"

        shiny::div(
          class = css,
          shiny::tags$span(
            style = "cursor: pointer;",
            onclick = paste0(
              "Shiny.setInputValue('", ns("select_path"),
              "', {idx: ", i, ", ts: Date.now()}, {priority: 'event'})"
            ),
            paste0(p$from[i], " \u2192 ", p$to[i])
          ),
          shiny::tags$span(
            style = paste0("cursor: pointer; color: #dc3545;",
                           " font-weight: bold; padding: 0 4px;"),
            onclick = paste0(
              "Shiny.setInputValue('", ns("delete_path"),
              "', {idx: ", i, ", ts: Date.now()}, {priority: 'event'})"
            ),
            "\u00d7"
          )
        )
      })

      do.call(shiny::tagList, items)
    })

    # ================================================================
    # UI: Path Form (secondary dropdown method)
    # ================================================================
    output$path_form <- shiny::renderUI({
      choices <- construct_names_reactive()
      if (length(choices) < 2) return(NULL)

      shiny::tagList(
        shiny::hr(),
        shiny::fluidRow(
          shiny::column(5,
            shiny::selectInput(ns("path_source"), "From", choices = choices)
          ),
          shiny::column(5,
            shiny::selectInput(ns("path_target"), "To", choices = choices)
          ),
          shiny::column(2,
            shiny::div(style = "margin-top: 25px;",
              shiny::actionButton(ns("add_path"), "+",
                                  class = "btn-primary btn-sm")
            )
          )
        )
      )
    })

    # ================================================================
    # UI: Selection Panel
    # ================================================================
    output$selection_panel <- shiny::renderUI({
      sel <- selected_node_rv()
      sel_edge <- selected_edge_rv()

      if (is.null(sel) && is.null(sel_edge)) return(NULL)

      if (!is.null(sel)) {
        cst <- Find(function(c) c$name == sel, constructs_rv())
        if (is.null(cst)) return(NULL)

        type_label <- switch(cst$type,
          "reflective"        = "Reflective",
          "composite_A"       = "Composite (Mode A)",
          "composite_B"       = "Composite (Mode B)",
          "higher_composite"  = "Higher-Order Composite",
          "higher_reflective" = "Higher-Order Reflective",
          "interaction"       = "Interaction Term"
        )

        desc <- if (cst$type == "interaction") {
          paste0("IV: ", cst$iv, ", Moderator: ", cst$moderator,
                 ", Method: ", cst$method)
        } else {
          paste(cst$items, collapse = ", ")
        }

        return(shiny::div(class = "selection-panel",
          shiny::tags$strong(cst$name),
          shiny::tags$br(),
          shiny::tags$small(style = "color: #666;", type_label),
          shiny::tags$br(),
          shiny::tags$small(desc),
          shiny::div(style = "margin-top: 8px;",
            shiny::actionButton(ns("edit_selected"), "Edit",
                                class = "btn-default btn-sm",
                                style = "margin-right: 4px;"),
            shiny::actionButton(ns("delete_selected"), "Delete",
                                class = "btn-danger btn-sm")
          )
        ))
      }

      if (!is.null(sel_edge)) {
        p <- paths_rv()
        if (sel_edge > nrow(p)) return(NULL)

        return(shiny::div(class = "selection-panel",
          shiny::tags$strong("Path"),
          shiny::tags$br(),
          paste0(p$from[sel_edge], " \u2192 ", p$to[sel_edge]),
          shiny::div(style = "margin-top: 8px;",
            shiny::actionButton(ns("delete_selected"), "Delete Path",
                                class = "btn-danger btn-sm")
          )
        ))
      }
    })

    # ================================================================
    # UI: Connection Banner
    # ================================================================
    output$connect_banner <- shiny::renderUI({
      if (!connect_mode_rv()) return(NULL)

      src <- connect_source_rv()
      msg <- if (is.null(src)) {
        "Click a source construct on the canvas"
      } else {
        paste0("Now click a target construct to connect from '", src, "'")
      }

      shiny::div(class = "connect-banner",
        shiny::tags$span(msg),
        shiny::actionLink(ns("cancel_connect"), "Cancel")
      )
    })

    # ================================================================
    # Actions: Selection
    # ================================================================
    shiny::observeEvent(input$select_construct, {
      selected_node_rv(input$select_construct$name)
      selected_edge_rv(NULL)
    })

    shiny::observeEvent(input$select_path, {
      selected_edge_rv(input$select_path$idx)
      selected_node_rv(NULL)
    })

    # ================================================================
    # Actions: Create / Save Construct
    # ================================================================
    shiny::observeEvent(input$add_construct, {
      type <- input$construct_type
      editing <- editing_rv()

      # --- Resolve name ---
      if (type == "interaction") {
        iv <- input$intxn_iv
        moderator <- input$intxn_moderator
        if (is.null(iv) || is.null(moderator) || iv == moderator) {
          shiny::showNotification("Select different IV and Moderator.",
                                 type = "warning")
          return()
        }
        name <- paste0(iv, "*", moderator)
      } else {
        name <- trimws(input$construct_name)
        if (name == "") {
          shiny::showNotification("Enter a construct name.", type = "warning")
          return()
        }
      }

      # --- Check name uniqueness ---
      existing <- construct_names_reactive()
      if (is.null(editing) && name %in% existing) {
        shiny::showNotification("Construct name already exists.", type = "error")
        return()
      }
      if (!is.null(editing) && name != editing && name %in% existing) {
        shiny::showNotification("Construct name already exists.", type = "error")
        return()
      }

      # --- Build construct record ---
      if (type %in% c("reflective", "composite_A", "composite_B")) {
        items <- selected_items_rv()
        if (length(items) == 0) {
          shiny::showNotification("Select items from the palette.",
                                 type = "warning")
          return()
        }
        new_cst <- list(name = name, type = type, items = items)

      } else if (type %in% c("higher_composite", "higher_reflective")) {
        dims <- input$hoc_dimensions
        if (is.null(dims) || length(dims) == 0) {
          shiny::showNotification("Select dimension constructs.",
                                 type = "warning")
          return()
        }
        new_cst <- list(name = name, type = type, items = dims)

      } else if (type == "interaction") {
        method <- input$intxn_method
        if (is.null(method)) method <- "two_stage"
        new_cst <- list(
          name = name, type = type,
          iv = input$intxn_iv, moderator = input$intxn_moderator,
          method = method, items = character()
        )
      }

      # --- Update state ---
      csts <- constructs_rv()

      if (is.null(editing)) {
        # Create mode
        csts[[length(csts) + 1]] <- new_cst
      } else {
        # Edit mode: replace in place
        idx <- which(vapply(csts, function(c) c$name, character(1)) == editing)
        if (length(idx) > 0) {
          csts[[idx]] <- new_cst

          # Update references if name changed
          if (editing != name) {
            p <- paths_rv()
            if (nrow(p) > 0) {
              p$from[p$from == editing] <- name
              p$to[p$to == editing] <- name
              paths_rv(p)
            }

            # Update HOC dimensions and interaction refs
            for (j in seq_along(csts)) {
              if (csts[[j]]$type %in% c("higher_composite",
                                         "higher_reflective")) {
                csts[[j]]$items[csts[[j]]$items == editing] <- name
              }
              if (csts[[j]]$type == "interaction") {
                if (identical(csts[[j]]$iv, editing)) csts[[j]]$iv <- name
                if (identical(csts[[j]]$moderator, editing)) {
                  csts[[j]]$moderator <- name
                }
                csts[[j]]$name <- paste0(csts[[j]]$iv, "*",
                                         csts[[j]]$moderator)
              }
            }
          }
        }
        editing_rv(NULL)
        selected_node_rv(name)
      }

      constructs_rv(csts)

      # Reset form
      shiny::updateTextInput(session, "construct_name", value = "")
      selected_items_rv(character())
    })

    # ================================================================
    # Actions: Edit Construct
    # ================================================================
    shiny::observeEvent(input$edit_selected, {
      sel <- selected_node_rv()
      if (is.null(sel)) return()

      cst <- Find(function(c) c$name == sel, constructs_rv())
      if (is.null(cst)) return()

      # Enter editing mode — this triggers form re-render with values
      editing_rv(sel)
      shiny::updateSelectInput(session, "construct_type", selected = cst$type)

      if (cst$type != "interaction") {
        shiny::updateTextInput(session, "construct_name", value = cst$name)
      }

      if (cst$type %in% c("reflective", "composite_A", "composite_B")) {
        selected_items_rv(cst$items)
      } else {
        selected_items_rv(character())
      }
      # HOC dims and interaction inputs are pre-populated via construct_form
    })

    # ================================================================
    # Actions: Cancel Edit
    # ================================================================
    shiny::observeEvent(input$cancel_edit, {
      editing_rv(NULL)
      shiny::updateTextInput(session, "construct_name", value = "")
      shiny::updateSelectInput(session, "construct_type",
                               selected = "reflective")
      selected_items_rv(character())
    })

    # ================================================================
    # Actions: Delete
    # ================================================================
    shiny::observeEvent(input$delete_selected, {
      sel <- selected_node_rv()
      sel_edge <- selected_edge_rv()

      if (!is.null(sel_edge)) {
        # Delete a path
        p <- paths_rv()
        if (sel_edge <= nrow(p)) {
          paths_rv(p[-sel_edge, , drop = FALSE])
          selected_edge_rv(NULL)
          shiny::showNotification("Path deleted.", type = "message")
        }
        return()
      }

      if (!is.null(sel)) {
        csts <- constructs_rv()
        idx <- which(vapply(csts, function(c) c$name, character(1)) == sel)
        if (length(idx) == 0) return()

        csts <- csts[-idx]

        # Remove referencing paths
        p <- paths_rv()
        if (nrow(p) > 0) {
          paths_rv(p[!(p$from == sel | p$to == sel), , drop = FALSE])
        }

        # Clean up HOC dimensions that reference deleted construct
        for (j in seq_along(csts)) {
          if (csts[[j]]$type %in% c("higher_composite",
                                     "higher_reflective")) {
            csts[[j]]$items <- setdiff(csts[[j]]$items, sel)
          }
        }

        # Remove interactions that reference deleted construct
        csts <- Filter(function(c) {
          if (c$type == "interaction") {
            return(!(c$iv == sel || c$moderator == sel))
          }
          TRUE
        }, csts)

        # Remove HOCs with no remaining dimensions
        csts <- Filter(function(c) {
          if (c$type %in% c("higher_composite", "higher_reflective")) {
            return(length(c$items) > 0)
          }
          TRUE
        }, csts)

        constructs_rv(csts)
        selected_node_rv(NULL)
        shiny::showNotification(paste0("Deleted '", sel, "'."),
                                type = "message")
      }
    })

    # Inline path delete (x button in path list)
    shiny::observeEvent(input$delete_path, {
      idx <- input$delete_path$idx
      p <- paths_rv()
      if (idx <= nrow(p)) {
        paths_rv(p[-idx, , drop = FALSE])
        if (identical(selected_edge_rv(), idx)) selected_edge_rv(NULL)
      }
    })

    # ================================================================
    # Actions: Add Path (dropdown form)
    # ================================================================
    shiny::observeEvent(input$add_path, {
      src <- input$path_source
      tgt <- input$path_target
      if (is.null(src) || is.null(tgt) || src == tgt) {
        shiny::showNotification("Select different source and target.",
                                type = "warning")
        return()
      }

      p <- paths_rv()
      if (nrow(p) > 0 && any(p$from == src & p$to == tgt)) {
        shiny::showNotification("Path already exists.", type = "warning")
        return()
      }

      p <- rbind(p, data.frame(from = src, to = tgt,
                               stringsAsFactors = FALSE))
      paths_rv(p)
    })

    # ================================================================
    # Actions: Connection Mode (click-to-connect on canvas)
    # ================================================================
    shiny::observeEvent(input$draw_path, {
      if (length(construct_names_reactive()) < 2) {
        shiny::showNotification("Create at least 2 constructs first.",
                                type = "warning")
        return()
      }
      connect_mode_rv(!connect_mode_rv())
      connect_source_rv(NULL)
    })

    shiny::observeEvent(input$cancel_connect, {
      connect_mode_rv(FALSE)
      connect_source_rv(NULL)
    })

    shiny::observeEvent(input$escape_pressed, {
      if (connect_mode_rv()) {
        connect_mode_rv(FALSE)
        connect_source_rv(NULL)
      }
    })

    # ================================================================
    # Actions: Clear All
    # ================================================================
    shiny::observeEvent(input$clear_model, {
      constructs_rv(list())
      paths_rv(data.frame(from = character(), to = character(),
                          stringsAsFactors = FALSE))
      selected_items_rv(character())
      selected_node_rv(NULL)
      selected_edge_rv(NULL)
      connect_mode_rv(FALSE)
      connect_source_rv(NULL)
      editing_rv(NULL)
    })

    # ================================================================
    # Canvas: Node Click
    # ================================================================
    shiny::observeEvent(input$model_canvas_selected, {
      sel <- input$model_canvas_selected
      if (is.null(sel) || sel == "") return()

      # Only handle construct nodes (not item nodes)
      if (!(sel %in% construct_names_reactive())) return()

      if (connect_mode_rv()) {
        # Connection mode
        src <- connect_source_rv()
        if (is.null(src)) {
          connect_source_rv(sel)
        } else if (sel != src) {
          # Create path
          p <- paths_rv()
          if (nrow(p) == 0 || !any(p$from == src & p$to == sel)) {
            p <- rbind(p, data.frame(from = src, to = sel,
                                     stringsAsFactors = FALSE))
            paths_rv(p)
          }
          connect_source_rv(NULL)
          connect_mode_rv(FALSE)
        }
      } else {
        # Normal selection
        selected_node_rv(sel)
        selected_edge_rv(NULL)
      }
    })

    # ================================================================
    # Canvas: Edge Click
    # ================================================================
    shiny::observeEvent(input$model_canvas_selectedEdge, {
      sel <- input$model_canvas_selectedEdge
      if (is.null(sel) || sel == "") return()

      # Only handle SM edges (not MM or HOC edges)
      if (grepl("^sm_", sel)) {
        idx <- as.integer(sub("^sm_", "", sel))
        selected_edge_rv(idx)
        selected_node_rv(NULL)
      }
    })

    # ================================================================
    # Canvas: Deselect
    # ================================================================
    shiny::observeEvent(input$model_canvas_deselected, {
      if (!connect_mode_rv()) {
        selected_node_rv(NULL)
      }
    })

    shiny::observeEvent(input$model_canvas_deselectedEdge, {
      if (!connect_mode_rv()) {
        selected_edge_rv(NULL)
      }
    })

    # ================================================================
    # Canvas: Render
    # ================================================================
    output$model_canvas <- visNetwork::renderVisNetwork({
      csts <- constructs_rv()
      p <- paths_rv()

      if (length(csts) == 0) {
        empty_nodes <- data.frame(
          id = "placeholder",
          label = "Add constructs to build your model",
          shape = "text", font.size = 18, font.color = "#999",
          stringsAsFactors = FALSE
        )
        return(
          visNetwork::visNetwork(empty_nodes, data.frame()) |>
            visNetwork::visInteraction(dragNodes = FALSE,
                                       zoomView = FALSE) |>
            visNetwork::visOptions(nodesIdSelection = FALSE)
        )
      }

      # -- Compute hierarchical levels from SM paths --
      cst_names <- vapply(csts, function(c) c$name, character(1))
      cst_levels <- stats::setNames(rep(2L, length(cst_names)), cst_names)

      if (nrow(p) > 0) {
        for (pass in seq_along(cst_names)) {
          for (i in seq_len(nrow(p))) {
            src <- p$from[i]
            tgt <- p$to[i]
            if (src %in% cst_names && tgt %in% cst_names) {
              if (cst_levels[tgt] <= cst_levels[src]) {
                cst_levels[tgt] <- cst_levels[src] + 2L
              }
            }
          }
        }
      }

      # Adjust HOC levels: must be after their dimensions
      for (cst in csts) {
        if (cst$type %in% c("higher_composite", "higher_reflective")) {
          dim_lvls <- cst_levels[cst$items[cst$items %in% cst_names]]
          if (length(dim_lvls) > 0) {
            min_lvl <- max(dim_lvls) + 2L
            if (cst_levels[cst$name] < min_lvl) {
              cst_levels[cst$name] <- min_lvl
            }
          }
        }
      }

      # -- Build nodes --
      nodes_list <- list()

      for (cst in csts) {
        # Construct node shape and color by type
        shape <- switch(cst$type,
          "reflective"        = "ellipse",
          "composite_A"       = "hexagon",
          "composite_B"       = "hexagon",
          "higher_composite"  = "hexagon",
          "higher_reflective" = "ellipse",
          "interaction"       = "diamond",
          "ellipse"
        )

        fill <- switch(cst$type,
          "higher_composite"  = "#fff3cd",
          "higher_reflective" = "#f3e5f5",
          "interaction"       = "#d4edda",
          "#ffffff"
        )

        nodes_list[[length(nodes_list) + 1]] <- data.frame(
          id               = cst$name,
          label            = cst$name,
          group            = "construct",
          shape            = shape,
          color.background = fill,
          color.border     = "#000000",
          color.highlight.background = "#cce5ff",
          color.highlight.border     = "#004085",
          font.size        = 14,
          font.color       = "#000000",
          borderWidth      = 2,
          level            = cst_levels[cst$name],
          stringsAsFactors = FALSE
        )

        # Item nodes (only for regular constructs)
        if (cst$type %in% c("reflective", "composite_A", "composite_B")) {
          for (item in cst$items) {
            nodes_list[[length(nodes_list) + 1]] <- data.frame(
              id               = item,
              label            = item,
              group            = "item",
              shape            = "box",
              color.background = "#f8f9fa",
              color.border     = "#000000",
              color.highlight.background = "#e2e6ea",
              color.highlight.border     = "#004085",
              font.size        = 11,
              font.color       = "#333333",
              borderWidth      = 1,
              level            = cst_levels[cst$name] - 1L,
              stringsAsFactors = FALSE
            )
          }
        }
      }

      nodes <- do.call(rbind, nodes_list)
      # Deduplicate (safety — items shouldn't be shared)
      nodes <- nodes[!duplicated(nodes$id), ]

      # -- Build edges --
      edges_list <- list()
      edge_id <- 1

      for (cst in csts) {
        if (cst$type %in% c("reflective", "composite_A", "composite_B")) {
          # MM edges: item <-> construct
          arrow_dir <- switch(cst$type,
            "reflective"  = "from",
            "composite_A" = "from",
            "composite_B" = "to",
            "from"
          )

          for (item in cst$items) {
            edges_list[[length(edges_list) + 1]] <- data.frame(
              id          = paste0("mm_", edge_id),
              from        = item,
              to          = cst$name,
              arrows      = arrow_dir,
              dashes      = FALSE,
              width       = 1,
              color.color = "#888888",
              color.highlight = "#004085",
              smooth      = FALSE,
              stringsAsFactors = FALSE
            )
            edge_id <- edge_id + 1
          }

        } else if (cst$type %in% c("higher_composite",
                                     "higher_reflective")) {
          # HOC edges: dimension constructs -> HOC (dashed)
          for (dim_name in cst$items) {
            if (dim_name %in% cst_names) {
              edges_list[[length(edges_list) + 1]] <- data.frame(
                id          = paste0("hoc_", edge_id),
                from        = dim_name,
                to          = cst$name,
                arrows      = "to",
                dashes      = TRUE,
                width       = 1.5,
                color.color = "#e67e22",
                color.highlight = "#d35400",
                smooth      = FALSE,
                stringsAsFactors = FALSE
              )
              edge_id <- edge_id + 1
            }
          }
        }
        # Interaction: no measurement edges
      }

      # SM edges (structural paths)
      if (nrow(p) > 0) {
        for (i in seq_len(nrow(p))) {
          edges_list[[length(edges_list) + 1]] <- data.frame(
            id          = paste0("sm_", i),
            from        = p$from[i],
            to          = p$to[i],
            arrows      = "to",
            dashes      = FALSE,
            width       = 2.5,
            color.color = "#000000",
            color.highlight = "#004085",
            smooth      = TRUE,
            stringsAsFactors = FALSE
          )
        }
      }

      edges <- if (length(edges_list) > 0) {
        do.call(rbind, edges_list)
      } else {
        data.frame(from = character(), to = character(),
                   stringsAsFactors = FALSE)
      }

      # -- Build visNetwork --
      visNetwork::visNetwork(nodes, edges, background = "#fafafa") |>
        visNetwork::visHierarchicalLayout(
          direction = "LR",
          sortMethod = "directed",
          levelSeparation = 180,
          nodeSpacing = 80,
          treeSpacing = 100
        ) |>
        visNetwork::visInteraction(
          hover = TRUE,
          multiselect = FALSE,
          selectConnectedEdges = FALSE,
          tooltipDelay = 200
        ) |>
        visNetwork::visOptions(
          highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
          nodesIdSelection = FALSE
        ) |>
        visNetwork::visEvents(
          selectNode = paste0("function(params) {
            if (params.nodes.length > 0) {
              Shiny.setInputValue('", ns("model_canvas_selected"), "',
                params.nodes[0], {priority: 'event'});
            }
          }"),
          selectEdge = paste0("function(params) {
            if (params.edges.length > 0 && params.nodes.length === 0) {
              Shiny.setInputValue('", ns("model_canvas_selectedEdge"), "',
                params.edges[0], {priority: 'event'});
            }
          }"),
          deselectNode = paste0("function(params) {
            Shiny.setInputValue('", ns("model_canvas_deselected"), "',
              Date.now(), {priority: 'event'});
          }"),
          deselectEdge = paste0("function(params) {
            Shiny.setInputValue('", ns("model_canvas_deselectedEdge"), "',
              Date.now(), {priority: 'event'});
          }")
        )
    })

    # ================================================================
    # Output: SEMinR Model Specification
    # ================================================================
    model_spec <- shiny::reactive({
      csts <- constructs_rv()
      p <- paths_rv()

      if (length(csts) == 0) return(NULL)

      # Build measurement model
      mm_specs <- lapply(csts, function(cst) {
        switch(cst$type,
          "reflective"        = reflective(cst$name, cst$items),
          "composite_A"       = composite(cst$name, cst$items),
          "composite_B"       = composite(cst$name, cst$items,
                                          weights = mode_B),
          "higher_composite"  = higher_composite(cst$name, cst$items),
          "higher_reflective" = higher_reflective(cst$name, cst$items),
          "interaction"       = {
            method_fn <- switch(cst$method,
              "two_stage"         = two_stage,
              "product_indicator" = product_indicator,
              "orthogonal"        = orthogonal,
              two_stage
            )
            interaction_term(iv = cst$iv, moderator = cst$moderator,
                             method = method_fn)
          }
        )
      })
      mm <- do.call(constructs, mm_specs)

      # Build structural model
      sm <- NULL
      if (nrow(p) > 0) {
        path_specs <- list()
        sources <- unique(p$from)
        for (src in sources) {
          targets <- p$to[p$from == src]
          path_specs[[length(path_specs) + 1]] <- paths(from = src,
                                                         to = targets)
        }
        sm <- do.call(relationships, path_specs)
      }

      list(measurement_model = mm, structural_model = sm)
    })

    model_spec
  })
}
