colhex2col <- function(colhex) {
  # Convert hex to RGB
  mycol   <- col2rgb(colhex)
  # Convert all x11 colors to RGB, adn transform
  all_colors <- data.frame(col2rgb(grDevices::colors()))
  all_colors <- setNames(all_colors, grDevices::colors())

  by_rgb <- data.frame(t(all_colors))
  by_rgb <- sort(apply(by_rgb,1, function(x) sum(abs(x - mycol)) ))

  names(by_rgb[1])
}


#' Starts the model viewer in RStudio
#'
#' @return the updated dot code
#' @import shiny
#' @import shinyAce
#' @import miniUI
#' @export
#'
custom_theme_addin <- function() {
  library(shiny)
  library(miniUI)

  initial_theme <- seminr_theme_create()

  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- miniPage(
    gadgetTitleBar("SEMinR Model Builder"),
    miniTabstripPanel(
      miniTabPanel("Plot Parameters", icon = icon("sliders"),
                   miniContentPanel(scrollable = TRUE,
                                      fillRow(flex = c(1,2),
                                            uiOutput("theme_editor_ui_plot"),
                                            DiagrammeR::grVizOutput("dot_plot", width = "100%", height = "100%")
                                            )
                                    )
                   ),
      miniTabPanel("Structural Model Parameters", icon = icon("sliders"),
                 miniContentPanel(scrollable = TRUE,
                                  fillRow(flex = c(1,2),
                                          uiOutput("theme_editor_ui_sm"),
                                          DiagrammeR::grVizOutput("dot_sm", width = "100%", height = "100%")
                                  )
                 )
      ),
     miniTabPanel("Measurement Model Parameters", icon = icon("sliders"),
                  miniContentPanel(scrollable = TRUE,
                                   fillRow(flex = c(1,2),
                                           uiOutput("theme_editor_ui_mm"),
                                           DiagrammeR::grVizOutput("dot_mm", width = "100%", height = "100%")
                                   )
                  )
      ),
     miniTabPanel("Copy Theme Code", icon = icon("clipboard"),
                  miniContentPanel(scrollable = TRUE,
                                   fillRow(flex = c(1,2),
                                           verbatimTextOutput("theme_code"),
                                           DiagrammeR::grVizOutput("dot_code", width = "100%", height = "100%")
                                   )
                  )
     )

    )
  )




  server <- function(input, output, session) {


    rv <- reactiveValues()
    rv$sem <- 0

    my_model <- reactive({
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


    theme_elements <- reactive({
      elem_names <- names(initial_theme)
      elem_names
    })


    # adjust the theme ----
    my_theme <- reactive({
      allinputs <- reactiveValuesToList(input)
      elem_names <- names(allinputs)

      params <- c()
      param_names <- c()
      for (i in 1:length(allinputs)) {
        if (startsWith(elem_names[i], "ui_elem_")) {
          idx <- as.numeric(substr(elem_names[i], 9, 12))
          var_label <- theme_elements()[idx]
          input_content <- allinputs[i]
          print(paste(var_label, ":", input_content))

          params <- c(params, input_content)
          param_names <- c(param_names, var_label)
        }
      }

      params <- structure(as.list(params), names = param_names)


      initial_theme <<- modifyList(initial_theme, params)

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
      #shinyWidgets::colorSelectorDrop(ui_name, label, choices = all_colors(),
      #                                 selected = value, display_label = T,
      #                                circle = FALSE, ncol = 20)

      )
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
        if (startsWith(elem_name, prefix)) {

        if (elem_type == "logical") {
          ui_elem <- checkboxInput(ui_name, label = elem_name, value = thm[[elem]])
        } else
          if (elem_type == "character") {
            if (thm[[elem]] %in% c(all_colors(), "transparent")) {
              ui_elem <- color_picker(ui_name, label = elem_name, value = thm[[elem]])
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
        }

        output_elements <- list(output_elements, ui_elem)
      }

      list(output_elements)
    }



    # outputs----

    output$theme_editor_ui_plot <- renderUI({
      generate_ui()
    })

    output$dot_plot <- DiagrammeR::renderGrViz({
      plot(my_model(), theme = my_theme())
    })

    output$theme_editor_ui_sm <- renderUI({
      generate_ui("sm")
    })

    output$dot_sm <- DiagrammeR::renderGrViz({
      plot(my_model(), theme = my_theme())
    })

    output$theme_editor_ui_mm <- renderUI({
      generate_ui("mm")
    })

    output$dot_mm <- DiagrammeR::renderGrViz({
      plot(my_model(), theme = my_theme())
    })


    output$theme_code <- renderPrint({
      print(my_theme())
    })

    output$dot_code <- DiagrammeR::renderGrViz({
      plot(my_model(), theme = my_theme())
    })



    observeEvent(input$done, {

      # Here is where your Shiny application might now go an affect the
      # contents of a document open in RStudio, using the `rstudioapi` package.
      #
      # At the end, your application should call 'stopApp()' here, to ensure that
      # the gadget is closed after 'done' is clicked.
      stopApp()
    })

  }

  viewer <- shiny::dialogViewer(width = 1400, height = 800, dialogName = "SEMinR Plot")
   shiny::runGadget(ui, server, viewer = viewer)


}

if (TRUE) {
  custom_theme_addin()
}

