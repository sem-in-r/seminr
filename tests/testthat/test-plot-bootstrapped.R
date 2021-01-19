context("SEMinR dot_graph does not fail for a bootstrapped models.\n")
test_that("bootstrapped models work", {

  set.seed(123)
  mobi <- mobi

  #seminr syntax for creating measurement model
  mobi_mm <- constructs(
    reflective("Satisfaction", multi_items("CUSA", 1:3)),
    composite("Complaints",   single_item("CUSCO")),
    reflective("Loyalty",      multi_items("CUSL", 1:3))
  )
  #seminr syntax for creating structural model
  mobi_sm <- relationships(
    paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
    paths(from = "Complaints",   to = "Loyalty")
  )

  mobi_pls <- estimate_pls(data = mobi,
                           measurement_model = mobi_mm,
                           structural_model = mobi_sm)

  mobi_boot <- bootstrap_model(mobi_pls, nboot = 100, cores = 1)

  expect_error(dot_graph(mobi_boot), NA)
  expect_error(plot(mobi_boot), NA)



  # As a backup for testing manually
  if (FALSE) {
    seminr::seminr_theme_set(seminr::seminr_theme_create(sm.edge.boot.show_t_value = F,
                                                         sm.edge.boot.show_ci = F))
    DiagrammeR::grViz(dot_graph(mobi_boot))

    seminr::seminr_theme_set(seminr::seminr_theme_create(sm.edge.boot.show_t_value = T,
                                                         sm.edge.boot.show_ci = T,
                                                         sm.edge.boot.template =
                                                           "<B>{variable} = {value}</B><BR/><FONT POINT-SIZE='7'><I>{tvalue}, {pvalue}</I><BR/>{civalue}</FONT>"))
    DiagrammeR::grViz(dot_graph(mobi_boot))

  }

})

unlink("Rplots.pdf")
