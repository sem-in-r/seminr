# Shiny module: Visual model builder with interactive canvas
# Part of seminr_app()

#' Builder Module UI
#' @param id Module namespace id
#' @keywords internal
mod_builder_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Custom CSS for the builder
    shiny::tags$head(shiny::tags$style(shiny::HTML(paste0(
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
      ".construct-list-item {",
      "  padding: 6px 10px; margin: 3px 0; border: 1px solid #ddd;",
      "  border-radius: 4px; cursor: pointer; background: #fafafa;",
      "}",
      ".construct-list-item:hover { background: #e8f4fd; }",
      ".construct-list-item.selected { background: #cce5ff; border-color: #004085; }"
    )))),

    shiny::fluidRow(
      # Left panel: item palette + construct controls
      shiny::column(3,
        # Item palette
        shiny::wellPanel(
          shiny::h4("Available Items"),
          shiny::helpText("Select items below, then click 'Create Construct'."),
          shiny::uiOutput(ns("item_palette")),
          shiny::hr(),
          shiny::textInput(ns("construct_name"), "Construct Name", placeholder = "e.g., Image"),
          shiny::selectInput(ns("construct_type"), "Type",
            choices = c("Reflective" = "reflective",
                        "Composite (Mode A)" = "composite_A",
                        "Composite (Mode B)" = "composite_B")
          ),
          shiny::actionButton(ns("add_construct"), "Create Construct",
                              class = "btn-primary btn-sm",
                              style = "width: 100%;")
        ),

        # Existing constructs list
        shiny::wellPanel(
          shiny::h4("Constructs"),
          shiny::uiOutput(ns("construct_list")),
          shiny::hr(),
          shiny::h4("Paths"),
          shiny::helpText("Select a source, then a target construct above, or use the controls below."),
          shiny::uiOutput(ns("path_source_ui")),
          shiny::uiOutput(ns("path_target_ui")),
          shiny::actionButton(ns("add_path"), "Add Path",
                              class = "btn-primary btn-sm",
                              style = "width: 100%;")
        ),

        # Edit / Delete controls
        shiny::wellPanel(
          shiny::actionButton(ns("edit_selected"), "Edit Selected",
                              class = "btn-default btn-sm",
                              style = "width: 100%; margin-bottom: 4px;"),
          shiny::actionButton(ns("delete_selected"), "Delete Selected",
                              class = "btn-danger btn-sm",
                              style = "width: 100%; margin-bottom: 4px;"),
          shiny::actionButton(ns("clear_model"), "Clear All",
                              class = "btn-warning btn-sm",
                              style = "width: 100%;")
        )
      ),

      # Right panel: canvas
      shiny::column(9,
        shiny::div(
          style = "border: 1px solid #ddd; border-radius: 4px; background: #fafafa;",
          visNetwork::visNetworkOutput(ns("model_canvas"), height = "600px")
        ),
        shiny::fluidRow(
          shiny::column(6,
            shiny::verbatimTextOutput(ns("selection_info"))
          ),
          shiny::column(6,
            shiny::div(style = "text-align: right; padding: 8px;",
              shiny::helpText("Click nodes/edges to select. Shift+click for multi-select.")
            )
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

    # -- Reactive state --
    constructs_rv <- shiny::reactiveVal(list())
    paths_rv <- shiny::reactiveVal(data.frame(from = character(), to = character(),
                                              stringsAsFactors = FALSE))
    selected_items_rv <- shiny::reactiveVal(character())
    selected_node_rv <- shiny::reactiveVal(NULL)
    selected_edge_rv <- shiny::reactiveVal(NULL)

    # -- Derived reactives --
    construct_names_reactive <- shiny::reactive({
      csts <- constructs_rv()
      if (length(csts) == 0) return(character())
      sapply(csts, function(c) c$name)
    })

    used_items_reactive <- shiny::reactive({
      csts <- constructs_rv()
      if (length(csts) == 0) return(character())
      unique(unlist(lapply(csts, function(c) c$items)))
    })

    available_columns <- shiny::reactive({
      d <- data_reactive()
      if (is.null(d)) return(character())
      colnames(d)
    })

    # -- Item palette UI --
    output$item_palette <- shiny::renderUI({
      cols <- available_columns()
      if (length(cols) == 0) {
        return(shiny::helpText("Load data first."))
      }

      used <- used_items_reactive()
      selected <- selected_items_rv()

      # Build clickable item chips
      chips <- lapply(cols, function(col) {
        css_class <- "item-chip"
        if (col %in% used) css_class <- paste(css_class, "used")
        if (col %in% selected) css_class <- paste(css_class, "selected")

        shiny::tags$span(
          class = css_class,
          onclick = paste0("Shiny.setInputValue('", ns("toggle_item"), "',
            {col: '", col, "', ts: Date.now()}, {priority: 'event'})"),
          col
        )
      })

      shiny::div(id = ns("item_palette"),
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

    # -- Construct list UI --
    output$construct_list <- shiny::renderUI({
      csts <- constructs_rv()
      if (length(csts) == 0) {
        return(shiny::helpText("No constructs yet."))
      }

      items <- lapply(seq_along(csts), function(i) {
        cst <- csts[[i]]
        type_badge <- switch(cst$type,
          "reflective"  = shiny::tags$span(class = "badge", style = "background: #6c757d;", "R"),
          "composite_A" = shiny::tags$span(class = "badge", style = "background: #17a2b8;", "A"),
          "composite_B" = shiny::tags$span(class = "badge", style = "background: #ffc107; color: #333;", "B"),
          NULL
        )

        shiny::div(
          class = "construct-list-item",
          onclick = paste0("Shiny.setInputValue('", ns("select_construct"), "',
            {name: '", cst$name, "', ts: Date.now()}, {priority: 'event'})"),
          shiny::tags$strong(cst$name), " ", type_badge,
          shiny::tags$br(),
          shiny::tags$small(
            style = "color: #666;",
            paste(cst$items, collapse = ", ")
          )
        )
      })

      shiny::tagList(items)
    })

    # Handle construct selection from list
    shiny::observeEvent(input$select_construct, {
      selected_node_rv(input$select_construct$name)
      selected_edge_rv(NULL)
    })

    # -- Path source/target dropdowns --
    output$path_source_ui <- shiny::renderUI({
      choices <- construct_names_reactive()
      if (length(choices) == 0) return(NULL)
      shiny::selectInput(ns("path_source"), "From", choices = choices)
    })

    output$path_target_ui <- shiny::renderUI({
      choices <- construct_names_reactive()
      if (length(choices) == 0) return(NULL)
      shiny::selectInput(ns("path_target"), "To", choices = choices)
    })

    # -- Add construct --
    shiny::observeEvent(input$add_construct, {
      name <- trimws(input$construct_name)
      items <- selected_items_rv()

      if (name == "") {
        shiny::showNotification("Enter a construct name.", type = "warning")
        return()
      }
      if (length(items) == 0) {
        shiny::showNotification("Select items from the palette first.", type = "warning")
        return()
      }
      if (name %in% construct_names_reactive()) {
        shiny::showNotification("Construct name already exists.", type = "error")
        return()
      }

      csts <- constructs_rv()
      csts[[length(csts) + 1]] <- list(
        name  = name,
        type  = input$construct_type,
        items = items
      )
      constructs_rv(csts)

      # Reset
      shiny::updateTextInput(session, "construct_name", value = "")
      selected_items_rv(character())
    })

    # -- Add path --
    shiny::observeEvent(input$add_path, {
      src <- input$path_source
      tgt <- input$path_target
      if (is.null(src) || is.null(tgt) || src == tgt) {
        shiny::showNotification("Select different source and target.", type = "warning")
        return()
      }

      p <- paths_rv()
      if (nrow(p) > 0 && any(p$from == src & p$to == tgt)) {
        shiny::showNotification("Path already exists.", type = "warning")
        return()
      }

      p <- rbind(p, data.frame(from = src, to = tgt, stringsAsFactors = FALSE))
      paths_rv(p)
    })

    # -- Edit selected construct --
    shiny::observeEvent(input$edit_selected, {
      sel <- selected_node_rv()
      if (is.null(sel)) {
        shiny::showNotification("Select a construct first.", type = "warning")
        return()
      }

      csts <- constructs_rv()
      idx <- which(sapply(csts, function(c) c$name) == sel)
      if (length(idx) == 0) {
        shiny::showNotification("Selected construct not found.", type = "warning")
        return()
      }

      cst <- csts[[idx]]
      # Pre-populate the form for editing
      shiny::updateTextInput(session, "construct_name", value = cst$name)
      shiny::updateSelectInput(session, "construct_type", selected = cst$type)
      selected_items_rv(cst$items)

      # Remove the old construct so it can be re-added
      csts <- csts[-idx]
      constructs_rv(csts)

      # Also remove paths referencing the old construct
      p <- paths_rv()
      if (nrow(p) > 0) {
        paths_rv(p[!(p$from == sel | p$to == sel), , drop = FALSE])
      }

      shiny::showNotification(paste0("Editing '", sel, "'. Modify and click 'Create Construct'."),
                              type = "message")
    })

    # -- Delete selected --
    shiny::observeEvent(input$delete_selected, {
      sel <- selected_node_rv()
      sel_edge <- selected_edge_rv()

      if (!is.null(sel_edge)) {
        # Delete a path
        p <- paths_rv()
        if (nrow(p) > 0 && sel_edge <= nrow(p)) {
          paths_rv(p[-sel_edge, , drop = FALSE])
          selected_edge_rv(NULL)
          shiny::showNotification("Path deleted.", type = "message")
        }
        return()
      }

      if (!is.null(sel)) {
        # Delete a construct
        csts <- constructs_rv()
        idx <- which(sapply(csts, function(c) c$name) == sel)
        if (length(idx) > 0) {
          csts <- csts[-idx]
          constructs_rv(csts)

          # Also remove referencing paths
          p <- paths_rv()
          if (nrow(p) > 0) {
            paths_rv(p[!(p$from == sel | p$to == sel), , drop = FALSE])
          }

          selected_node_rv(NULL)
          shiny::showNotification(paste0("Deleted '", sel, "'."), type = "message")
        }
        return()
      }

      shiny::showNotification("Select a construct or path first.", type = "warning")
    })

    # -- Clear all --
    shiny::observeEvent(input$clear_model, {
      constructs_rv(list())
      paths_rv(data.frame(from = character(), to = character(), stringsAsFactors = FALSE))
      selected_items_rv(character())
      selected_node_rv(NULL)
      selected_edge_rv(NULL)
    })

    # -- Canvas: visNetwork node click event --
    shiny::observeEvent(input$model_canvas_selected, {
      sel <- input$model_canvas_selected
      if (!is.null(sel) && length(sel) > 0) {
        # Check if it's a construct node
        if (sel %in% construct_names_reactive()) {
          selected_node_rv(sel)
          selected_edge_rv(NULL)
        }
      }
    })

    # -- Canvas: visNetwork edge click event --
    shiny::observeEvent(input$model_canvas_selectedEdge, {
      # Edge ID is the row index
      sel <- input$model_canvas_selectedEdge
      if (!is.null(sel) && length(sel) > 0) {
        selected_edge_rv(as.integer(sel))
        selected_node_rv(NULL)
      }
    })

    # -- Selection info --
    output$selection_info <- shiny::renderPrint({
      sel <- selected_node_rv()
      sel_edge <- selected_edge_rv()

      if (!is.null(sel)) {
        csts <- constructs_rv()
        idx <- which(sapply(csts, function(c) c$name) == sel)
        if (length(idx) > 0) {
          cst <- csts[[idx]]
          cat("Selected: ", cst$name, "\n")
          cat("Type: ", switch(cst$type,
            "reflective" = "Reflective",
            "composite_A" = "Composite (Mode A)",
            "composite_B" = "Composite (Mode B)"), "\n")
          cat("Items: ", paste(cst$items, collapse = ", "), "\n")
        }
      } else if (!is.null(sel_edge)) {
        p <- paths_rv()
        if (sel_edge <= nrow(p)) {
          cat("Selected path: ", p$from[sel_edge], " -> ", p$to[sel_edge], "\n")
        }
      } else {
        cat("No selection")
      }
    })

    # -- Canvas render --
    output$model_canvas <- visNetwork::renderVisNetwork({
      csts <- constructs_rv()
      p <- paths_rv()

      if (length(csts) == 0) {
        # Empty canvas
        empty_nodes <- data.frame(
          id = "placeholder", label = "Add constructs to build your model",
          shape = "text", font.size = 18, font.color = "#999",
          stringsAsFactors = FALSE
        )
        return(
          visNetwork::visNetwork(empty_nodes, data.frame()) |>
            visNetwork::visInteraction(dragNodes = FALSE, zoomView = FALSE) |>
            visNetwork::visOptions(nodesIdSelection = FALSE)
        )
      }

      # -- Build nodes --
      nodes_list <- list()
      level_counter <- 1

      for (i in seq_along(csts)) {
        cst <- csts[[i]]

        # Construct node
        shape <- switch(cst$type,
          "reflective"  = "ellipse",
          "composite_A" = "hexagon",
          "composite_B" = "hexagon",
          "ellipse"
        )

        nodes_list[[length(nodes_list) + 1]] <- data.frame(
          id               = cst$name,
          label            = cst$name,
          group            = "construct",
          shape            = shape,
          color.background = "#ffffff",
          color.border     = "#000000",
          color.highlight.background = "#cce5ff",
          color.highlight.border     = "#004085",
          font.size        = 14,
          font.color       = "#000000",
          borderWidth      = 2,
          level            = i,
          stringsAsFactors = FALSE
        )

        # Item nodes for this construct
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
            level            = i,
            stringsAsFactors = FALSE
          )
        }
      }

      nodes <- do.call(rbind, nodes_list)

      # -- Build edges --
      edges_list <- list()
      edge_id <- 1

      # MM edges (item -> construct): grey, thin
      for (cst in csts) {
        arrow_dir <- switch(cst$type,
          "reflective"  = "from",
          "composite_A" = "from",
          "composite_B" = "to",
          "from"
        )

        for (item in cst$items) {
          edges_list[[length(edges_list) + 1]] <- data.frame(
            id           = paste0("mm_", edge_id),
            from         = item,
            to           = cst$name,
            arrows       = arrow_dir,
            dashes       = FALSE,
            width        = 1,
            color.color  = "#888888",
            color.highlight = "#004085",
            smooth       = FALSE,
            edge_type    = "mm",
            stringsAsFactors = FALSE
          )
          edge_id <- edge_id + 1
        }
      }

      # SM edges (construct -> construct): black, thicker
      if (nrow(p) > 0) {
        for (i in seq_len(nrow(p))) {
          edges_list[[length(edges_list) + 1]] <- data.frame(
            id           = paste0("sm_", i),
            from         = p$from[i],
            to           = p$to[i],
            arrows       = "to",
            dashes       = FALSE,
            width        = 2.5,
            color.color  = "#000000",
            color.highlight = "#004085",
            smooth       = list(enabled = TRUE, type = "curvedCW", roundness = 0.15),
            edge_type    = "sm",
            stringsAsFactors = FALSE
          )
        }
      }

      edges <- if (length(edges_list) > 0) do.call(rbind, edges_list) else
        data.frame(from = character(), to = character(), stringsAsFactors = FALSE)

      # -- Build visNetwork --
      vis <- visNetwork::visNetwork(nodes, edges, background = "#fafafa") |>
        visNetwork::visHierarchicalLayout(
          direction = "LR",
          sortMethod = "directed",
          levelSeparation = 180,
          nodeSpacing = 80,
          treeSpacing = 100
        ) |>
        visNetwork::visInteraction(
          hover = TRUE,
          multiselect = TRUE,
          selectConnectedEdges = FALSE,
          tooltipDelay = 200
        ) |>
        visNetwork::visOptions(
          highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
          nodesIdSelection = FALSE,
          selectedBy = FALSE
        ) |>
        visNetwork::visEvents(
          selectNode = paste0("function(params) {
            if (params.nodes.length > 0) {
              Shiny.setInputValue('", ns("model_canvas_selected"), "',
                params.nodes[0], {priority: 'event'});
            }
          }"),
          selectEdge = paste0("function(params) {
            if (params.edges.length > 0) {
              Shiny.setInputValue('", ns("model_canvas_selectedEdge"), "',
                params.edges[0], {priority: 'event'});
            }
          }"),
          deselectNode = paste0("function(params) {
            Shiny.setInputValue('", ns("model_canvas_selected"), "', null, {priority: 'event'});
          }"),
          deselectEdge = paste0("function(params) {
            Shiny.setInputValue('", ns("model_canvas_selectedEdge"), "', null, {priority: 'event'});
          }")
        ) |>
        visNetwork::visLegend(
          addNodes = list(
            list(label = "Construct", shape = "ellipse",
                 color = list(background = "#ffffff", border = "#000000"),
                 borderWidth = 2, font = list(size = 12)),
            list(label = "Item", shape = "box",
                 color = list(background = "#f8f9fa", border = "#000000"),
                 borderWidth = 1, font = list(size = 11))
          ),
          addEdges = list(
            list(label = "Structural path", color = "#000000", width = 2.5,
                 arrows = "to"),
            list(label = "Measurement", color = "#888888", width = 1)
          ),
          useGroups = FALSE,
          position = "right",
          width = 0.12
        )

      vis
    })

    # -- Build and return SEMinR model spec --
    model_spec <- shiny::reactive({
      csts <- constructs_rv()
      p <- paths_rv()

      if (length(csts) == 0) return(NULL)

      # Build measurement model
      mm_specs <- lapply(csts, function(cst) {
        if (cst$type == "reflective") {
          reflective(cst$name, cst$items)
        } else if (cst$type == "composite_B") {
          composite(cst$name, cst$items, weights = mode_B)
        } else {
          composite(cst$name, cst$items)
        }
      })
      mm <- do.call(constructs, mm_specs)

      # Build structural model
      sm <- NULL
      if (nrow(p) > 0) {
        path_specs <- list()
        sources <- unique(p$from)
        for (src in sources) {
          targets <- p$to[p$from == src]
          path_specs[[length(path_specs) + 1]] <- paths(from = src, to = targets)
        }
        sm <- do.call(relationships, path_specs)
      }

      list(measurement_model = mm, structural_model = sm)
    })

    model_spec
  })
}
