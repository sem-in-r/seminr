test_that("Graphing confirmatory factory analysis works", {
  mobi <- mobi

  #seminr syntax for creating measurement model
  mobi_mm <- constructs(
    reflective("Image",        multi_items("IMAG", 1:5)),
    reflective("Expectation",  multi_items("CUEX", 1:3)),
    reflective("Value",        multi_items("PERV", 1:2)),
    reflective("Loyalty",      multi_items("CUSL", 1:3))
  )

   mobi_cfa <- seminr::estimate_cfa(mobi, mobi_mm)

   testthat::expect_error(dot_graph(mobi_cfa), NA)
   if (FALSE) {
     # interactive tests
     plot_model(mobi_cfa)
   }

   # TODO: prevent creation?
   unlink("Rplots.pdf")
})
