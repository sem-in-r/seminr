#' Starts the model viewer in RStudio
#'
#' @return the updated dot code
#' @import shiny
#' @import shinyAce
#' @import miniUI
#' @export
#'
model_viz <- function() {
  library(shiny)
  library(miniUI)

  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- miniPage(
    gadgetTitleBar("SEMinR Model Builder"),
    miniTabstripPanel(
      miniTabPanel("Select Data Frame", icon = icon("area-chart"),
                   miniContentPanel(
                     fillRow(


                       DiagrammeR::grVizOutput("dot_mm")
                     )
                   )
      ),
      miniTabPanel("Create Measurement Model", icon = icon("area-chart"),
                   miniContentPanel(
                     fillRow(


                      DiagrammeR::grVizOutput("dot_mm")
                     )
                   )
      ),
      miniTabPanel("Create Structural Model", icon = icon("area-chart"),
                   miniContentPanel(
                     DiagrammeR::grVizOutput("dot_sm")
                   )
      ),
      miniTabPanel("Parameters", icon = icon("sliders"),
                   miniContentPanel(scrollable = TRUE,
                     fillCol(flex = c(1,9),
                       uiOutput("model_select_ui"),
                       fillRow(flex = c(1,2),
                         shinyAce::aceEditor("dot_code_ace", mode = "dot", height = "600px", fontSize = 10,
                                             autoScrollEditorIntoView = T,
                                             minLines = 30
                                             ),
                         DiagrammeR::grVizOutput("dot", width = "100%", height = "100%")
                       )

                    )
                   )
      ),
      miniTabPanel("Visualize", icon = icon("area-chart"),
                   miniContentPanel(
                     DiagrammeR::grVizOutput("dot2")
                   )
      )

    )
  )


  # server ----
  server <- function(input, output, session) {


    all_data_frames <- reactive({
      get_ge <- ls(envir = globalenv())
      dd <- purrr::map(get_ge, get) # get object contents
      names(dd) <- get_ge
      dd <- dd[purrr::map_lgl(dd, inherits, what = 'data.frame')]
      dd
    })



      all_models <- reactive({
        get_ge <- ls(envir = globalenv())
        dd <- purrr::map(get_ge, get) # get object contents
        names(dd) <- get_ge
        dd <- dd[purrr::map_lgl(dd, inherits, what = 'seminr_model')]
        dd
      })

      observeEvent(input$model, {
        shinyAce::updateAceEditor(session, "dot_code_ace",
                                  value = {
          model <- dd[[input$model]]
          dot_graph(model)
        })
      })


      # outputs ----
      output$model_select_ui <- renderUI({

        selectInput("model", 'Choose a model to tweak',
                    choices = names(all_models()))


      })

      output$dot <- DiagrammeR::renderGrViz({
        DiagrammeR::grViz(input$dot_code_ace)
      })
      output$dot2 <- DiagrammeR::renderGrViz({
        DiagrammeR::grViz(input$dot_code_ace)
      })

  }

  viewer <- shiny::dialogViewer(width = 1400, height = 800, dialogName = "SEMinR Plot")
  shiny::runGadget(ui, server, viewer = viewer)

}

if (FALSE) {
  model_viz()
}

