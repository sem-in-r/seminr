colhex2col <- function(colhex) {
  # Convert hex to RGB
  mycol   <- grDevices::col2rgb(colhex)
  # Convert all x11 colors to RGB, adn transform
  all_colors <- data.frame(grDevices::col2rgb(grDevices::colors()))
  all_colors <- stats::setNames(all_colors, grDevices::colors())

  by_rgb <- data.frame(t(all_colors))
  by_rgb <- sort(apply(by_rgb,1, function(x) sum(abs(x - mycol)) ))

  names(by_rgb[1])
}

#
# TODO: Addin.css ?
# Manual Tool tips
#
#



#' Starts the model viewer in RStudio
#'
#' @return the updated dot code
#' @export
#'
custom_theme_addin <- function() {

  #shiny::addResourcePath("sbs", system.file("www", package = "shinyBS"))
  # starting theme
  initial_theme <- seminr_theme_create()
  # these options can not be set in themes
  ignored_theme_options <-
    c("plot.title",
      "mm.node.height",
      "mm.node.width",
      "sm.node.height",
      "sm.node.width")

  all_shapes <- c("box", "circle", "ellipse", "hexagon", "pentagon", "octogon", "triangle")

  theme_doc <- rbind(get_theme_doc(),
                     param = c("plot.title", "The plot title. Not saved in theme!"))

  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- miniUI::miniPage(
    #includeCSS("addin.css"),
    miniUI::gadgetTitleBar("SEMinR Theme Builder",
                     right = miniUI::miniTitleBarButton("done", "Copy to Clipboard", primary = TRUE)),
    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel("General Settings", icon = shiny::icon("sliders"),
                           miniUI::miniContentPanel(scrollable = TRUE,
                                      shiny::fillRow(flex = c(1,2),
                                            shiny::uiOutput("theme_editor_ui_plot"),
                                            DiagrammeR::grVizOutput("dot_plot", width = "100%", height = "100%")
                                            )
                                    )
                   ),
      miniUI::miniTabPanel("Construct Settings", icon = shiny::icon("box"),
                           miniUI::miniContentPanel(scrollable = TRUE,
                                  shiny::fillRow(flex = c(1,2),
                                          shiny::uiOutput("theme_editor_ui_sm_node"),
                                          DiagrammeR::grVizOutput("dot_sm_node", width = "100%", height = "100%")
                                  )
                 )
      ),
      miniUI::miniTabPanel("Path Settings", icon = shiny::icon("project-diagram"),
                           miniUI::miniContentPanel(scrollable = TRUE,
                                    shiny::fillRow(flex = c(1,2),
                                            shiny::uiOutput("theme_editor_ui_sm_edge"),
                                            DiagrammeR::grVizOutput("dot_sm_edge", width = "100%", height = "100%")
                                    )
                   )
      ),
      miniUI::miniTabPanel("Manifest Variable Settings", icon = shiny::icon("ruler"),
                           miniUI::miniContentPanel(scrollable = TRUE,
                                   shiny::fillRow(flex = c(1,2),
                                           shiny::uiOutput("theme_editor_ui_mm_node"),
                                           DiagrammeR::grVizOutput("dot_mm_node", width = "100%", height = "100%")
                                   )
                  )
      ),
      miniUI::miniTabPanel("Loadings Settings", icon = shiny::icon("weight-hanging"),
                           miniUI::miniContentPanel(scrollable = TRUE,
                                    shiny::fillRow(flex = c(1,2),
                                            shiny::uiOutput("theme_editor_ui_mm_edge"),
                                            DiagrammeR::grVizOutput("dot_mm_edge", width = "100%", height = "100%")
                                    )
                   )
      ),
      miniUI::miniTabPanel("Inspect Theme Code", icon = shiny::icon("clipboard"),
                           miniUI::miniContentPanel(scrollable = TRUE,
                                   shiny::fillRow(flex = c(1,1),
                                           shiny::verbatimTextOutput("theme_code"),
                                           DiagrammeR::grVizOutput("dot_code", width = "100%", height = "100%")
                                   )
                  )
     )

    )
  )




  server <- function(input, output, session) {


    rv <- shiny::reactiveValues()
    rv$sem <- 0

    my_model <- shiny::reactive({
      set.seed(123)
      mobi <- mobi

      #seminr syntax for creating measurement model
      mobi_mm <- constructs(
        reflective("Image",        multi_items("IMAG", 1:5)),
        composite("Expectation",  multi_items("CUEX", 1:3)),
        reflective("Quality",      multi_items("PERQ", 1:7)),
        reflective("Loyalty",      multi_items("CUSL", 1:3))
      )
      #seminr syntax for creating structural model
      mobi_sm <- relationships(
        paths(from = "Image",        to = c("Expectation", "Loyalty")),
        paths(from = "Expectation",  to = c("Quality"))
      )

      mobi_pls <- estimate_pls(data = mobi,
                               measurement_model = mobi_mm,
                               structural_model = mobi_sm)

      mobi_boot <- bootstrap_model(mobi_pls, nboot = 5, cores = 1)
      mobi_boot
    })


    theme_elements <- shiny::reactive({
      elem_names <- names(initial_theme)
      elem_names
    })


    # adjust the theme ----
    my_theme <- shiny::reactive({
      allinputs <- shiny::reactiveValuesToList(input)
      elem_names <- names(allinputs)

      params <- c()
      param_names <- c()
      for (i in 1:length(allinputs)) {
        if (startsWith(elem_names[i], "ui_elem_")) {
          idx <- as.numeric(substr(elem_names[i], 9, 12))
          var_label <- theme_elements()[idx]
          input_content <- allinputs[i]

          #print(paste(var_label, ":", input_content))

          params <- c(params, input_content)
          param_names <- c(param_names, var_label)
        }
      }

      params <- structure(as.list(params), names = param_names)


      initial_theme <<- utils::modifyList(initial_theme, params)

      initial_theme
    })


    all_colors <- reactive({
      grDevices::colors()
    })

    color_picker <- function(ui_name, label, value){
      list(
      colourpicker::colourInput(ui_name, label, value = value,
                                palette = "limited", returnName = TRUE,
                                allowedCols = grDevices::colours(distinct = T))
      )
    }

    shape_picker <- function(ui_name, label, value) {
      shiny::selectInput(ui_name, label, choices = all_shapes, selected = value)
    }


    # generate ui ----
    generate_ui <- function(prefix = "plot.") {
      thm <- initial_theme

      output_elements <- c()
      elem_count <- length(thm)
      elem_names <- theme_elements()

      for (elem in 1:elem_count) {
        elem_type <- typeof(thm[[elem]])
        elem_name <- elem_names[[elem]]
        ui_name <- paste0("ui_elem_", elem)


        ui_elem <- NULL

        # allow for vector of prefixes (complicated?)
        pref_contained <- 0
        for (pref in prefix) {
          if (startsWith(elem_name, pref)) pref_contained <- pref_contained + 1
        }
        if (pref_contained > 0) {

        if (elem_type == "logical") {
          ui_elem <- shiny::checkboxInput(ui_name, label = elem_name, value = thm[[elem]])
        } else
          if (elem_type == "character") {
            if (thm[[elem]] %in% c(all_colors(), "transparent")) {
              ui_elem <- color_picker(ui_name, label = elem_name, value = thm[[elem]])
            } else if (endsWith(elem_name, "shape")) {
              ui_elem <- shape_picker(ui_name, label = elem_name, value = thm[[elem]])
            } else {
              ui_elem <- shiny::textInput(ui_name, label = elem_name, value = thm[[elem]])
            }
          } else
            if (elem_type == "double") {
              ui_elem <- shiny::numericInput(ui_name, label = elem_name, value = thm[[elem]])
            } else
              if (elem_type == "integer") {
                ui_elem <- shiny::numericInput(ui_name, label = elem_name, value = thm[[elem]])
              } else
              {

              print(paste(elem_type, "no idea?"))
            }

        # tooltips ----
          res <- tags$div(title = get_doc_string(elem_name), ui_elem)
          tt <- shinyBS::bsTooltip(ui_name, get_doc_string(elem_name), placement = "top", trigger = "hover",
                  options = NULL)
        #tt <- htmltools::p(get_doc_string(elem_name))
        output_elements <- list(output_elements, res) #ui_elem, tt)
        }
      }

      list(output_elements)
    }

    get_doc_string <- function(x) {
      theme_doc[theme_doc$param == x,]$description
    }















    # outputs----

    output$theme_editor_ui_plot <- shiny::renderUI({
      generate_ui()
    })

    output$dot_plot <- DiagrammeR::renderGrViz({
      plot(my_model(), theme = my_theme())
    })

    output$theme_editor_ui_sm_node <- shiny::renderUI({
      generate_ui(c("sm.node", "construct."))
    })

    output$dot_sm_node <- DiagrammeR::renderGrViz({
      plot(my_model(), theme = my_theme())
    })

    output$theme_editor_ui_sm_edge <- shiny::renderUI({
      generate_ui("sm.edge")
    })

    output$dot_sm_edge <- DiagrammeR::renderGrViz({
      plot(my_model(), theme = my_theme())
    })

    output$theme_editor_ui_mm_node <- shiny::renderUI({
      generate_ui(c("mm.node", "manifest."))
    })

    output$dot_mm_node <- DiagrammeR::renderGrViz({
      plot(my_model(), theme = my_theme())
    })

    output$theme_editor_ui_mm_edge <- shiny::renderUI({
      generate_ui("mm.edge")
    })

    output$dot_mm_edge <- DiagrammeR::renderGrViz({
      plot(my_model(), theme = my_theme())
    })


    # generate output code ----
    gen_code <- function(){
      thm <- my_theme()

      param_string <- ""

      for (i in 1:length(thm)) {
        params <- thm[[i]]
        param_names <- names(thm)[[i]]
        if (param_names %in% ignored_theme_options) {
          next
        }
        if (typeof(params) == "character") {
          param_string <- paste0(param_string,
                                 param_names, " = ",
                                 "\"", params, "\",\n\t")

        } else {
          param_string <- paste0(param_string,
                                 param_names, " = ",
                                 params, ",\n\t")
        }
      }
      # remove additional comma
      param_string <- substr(param_string, 1, nchar(param_string) - 3)

      res <- paste0(
        "seminr_theme_create(\n\t",
        param_string,
        "\n\t)"
      )
      res
    }

    output$theme_code <- shiny::renderPrint({
      cat(gen_code())
    })

    output$dot_code <- DiagrammeR::renderGrViz({
      plot(my_model(), theme = my_theme())
    })



    shiny::observeEvent(input$done, {

      # Here is where your Shiny application might now go an affect the
      # contents of a document open in RStudio, using the `rstudioapi` package.
      #
      # At the end, your application should call 'stopApp()' here, to ensure that
      # the gadget is closed after 'done' is clicked.
      clipr::write_clip(gen_code())
      shiny::stopApp()
    })

  }

  viewer <- shiny::dialogViewer(width = 1400, height = 800, dialogName = "SEMinR Plot")
   shiny::runGadget(ui, server, viewer = viewer)


}

if (FALSE) {
  custom_theme_addin()
}

