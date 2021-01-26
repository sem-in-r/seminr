context("SEMinR dot_graph plots minimal model\n")
test_that("dot_graph does not fail for a demo data set", {
  set.seed(123)
   mobi <- mobi

   #seminr syntax for creating measurement model
   mobi_mm <- constructs(
                reflective("Image",        multi_items("IMAG", 1:3)),
                composite("Expectation",  multi_items("CUEX", 1:3))
              )
   #seminr syntax for creating structural model
   mobi_sm <- relationships(
     paths(from = "Image",        to = c("Expectation"))
   )

   model <- estimate_pls(data = mobi,
                            measurement_model = mobi_mm,
                            structural_model = mobi_sm)
   # This creates an unwanted Rplots.pdf file? Why?
  expect_error(dot_graph(model), NA)
  expect_error(plot(model), NA)

  plot <- plot(model)
  #vdiffr::expect_doppelganger(title = "Plot minimal model", fig = plot, writer = write_test)

})



# Cleanup
unlink("Rplots.pdf")







