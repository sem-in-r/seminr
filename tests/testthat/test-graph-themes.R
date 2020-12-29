test_that("Basic themes are created without error", {

  expect_error({
    seminr::seminr_theme_default()
    seminr::seminr_theme_smartpls()
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

 thm <- seminr_theme_create()
 thm$sm.edge.width_multiplier <- 1
 thm$mm.edge.width_multiplier <- 1

 DiagrammeR::grViz(dot_graph(mobi_pls, theme = thm))
 DiagrammeR::grViz(dot_graph(mobi_pls, theme = thm, measurement_only = T))
 DiagrammeR::grViz(dot_graph(mobi_pls, theme = thm, structure_only = T))
 DiagrammeR::grViz(dot_graph(mobi_mm, theme = thm))
 DiagrammeR::grViz(dot_graph(mobi_sm, theme = thm))
 testthat::expect_error(dot_graph(mobi_pls, theme = thm), NA)
})
