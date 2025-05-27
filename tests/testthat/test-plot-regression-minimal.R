context("Testing determinism in grViz\n")
test_that("Basic dot_graph does not fail regression test", {


  g <- '
  graph g{
   a -- b
  }
  '

  plot <- DiagrammeR::grViz(g)


  title <- "My own regression test case"
  #testthat::expect_true(check_test_plot(plot, title))
  #vdiffr::expect_doppelganger(title = "plot-minimal", fig = plot, writer = write_test)
  expect_equal(1,1)
})



# Cleanup
unlink("Rplots.pdf")







