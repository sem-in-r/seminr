context("SEMinR dot_graph themes can be created.\n")
test_that("Basic themes are created without error", {

  expect_error({
    seminr::seminr_theme_default()
    seminr::seminr_theme_smart()
  },
               NA)

})


test_that("Modify theme edge multipliers", {

  set.seed(123)
  mobi <- mobi

  #seminr syntax for creating measurement model
  mobi_mm <- constructs(
    reflective("Image",        multi_items("IMAG", 1:5)),
    reflective("Expectation",  multi_items("CUEX", 1:3)),
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

  mobi_boot <- bootstrap_model(mobi_pls, nboot = 100, cores = 1)

 thm <- seminr_theme_create()
 thm$sm.edge.width_multiplier <- 1
 thm$mm.edge.width_multiplier <- 1
 thm$plot.greekletters <- T
 thm$sm.edge.boot.template <- edge_template_default()
 thm$sm.edge.boot.show_ci <- T
 thm$sm.edge.boot.show_p_stars <- T

 thm$mm.edge.boot.template <- edge_template_minimal()
 thm$mm.edge.boot.show_p_stars <- T
 thm$mm.edge.boot.show_p_value <- T
 thm$mm.edge.boot.show_t_value <- T
 thm$mm.edge.boot.show_ci <- T

 thm$sm.edge.positive.color <- "red"
 thm$sm.edge.negative.color <- "blue"
 thm$sm.node.fill <- "gray"
 thm$mm.node.fill <- "red"

 thm$sm.node.label.fontcolor <- "red"
 thm$mm.node.label.fontcolor <- "green"
 thm$sm.edge.label.fontcolor <- "blue"
 thm$mm.edge.label.fontcolor <- "gray"
 thm$plot.bgcolor <- "black"
 thm$plot.title.fontcolor <- "white"

 testthat::expect_error(dot_graph(mobi_pls, theme = thm), NA)
 testthat::expect_error(plot(mobi_boot, theme = thm), NA)


 plot <- plot(mobi_boot, title = "Dark theme", theme = thm)
 #vdiffr::expect_doppelganger(title = "Plot with other theme", fig = plot, writer = write_test)

 testthat::expect_warning(plot(mobi_boot, thm))
 #testing
 if (FALSE) {
   DiagrammeR::grViz(dot_graph(mobi_pls, theme = thm))
   DiagrammeR::grViz(dot_graph(mobi_pls, theme = thm, measurement_only = TRUE))
   DiagrammeR::grViz(dot_graph(mobi_pls, theme = thm, structure_only = TRUE))
   DiagrammeR::grViz(dot_graph(mobi_mm, theme = thm))
   DiagrammeR::grViz(dot_graph(mobi_sm, theme = thm))
 }
})
 unlink("Rplots.pdf")
