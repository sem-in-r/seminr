#' A function used in regression testing
#'
#' @param plot the grViz htmlwidge to save
#' @param file the filename to save
#' @param title an additional title (ignored)
#'
#' @return Returns nothing
write_svg.grViz <- function(plot, file, title = "") {
  svg <- charToRaw( DiagrammeRsvg::export_svg(plot) )
  rsvg::rsvg_svg(svg, file)
}

#' A function used in regression testing
#' Actually the above function write_svg.grViz creates a correct svg but behaves indeterministically
#' this function behaves better, but produces illegal svg
#'
#' @param plot the grViz htmlwidge to save
#' @param file the filename to save
#' @param title an additional title (ignored)
#'
#' @return Returns nothing
write_test <- function(plot, file, title = "") {
  #cat(file)
  svg <-  DiagrammeRsvg::export_svg(plot)
  fileConn <- file(file)
  writeLines(svg, fileConn)
  close(fileConn)
}


# COPIED FROM GGPLOT under MIT License
# YEAR: 2020
# COPYRIGHT HOLDER: ggplot2 authors
# By default, if vdiffr is not installed, all visual tests are skipped unless
# VDIFFR_RUN_TESTS is explicitly set to "true", which should be the case only on
# a GitHub Actions CI runner with stable version of R.

if (requireNamespace("vdiffr", quietly = TRUE)) {
  expect_doppelganger <- vdiffr::expect_doppelganger
} else {
  # If vdiffr is not available and visual tests are explicitly required, raise error.
  if (identical(Sys.getenv("VDIFFR_RUN_TESTS"), "true")) {
    abort("vdiffr is not installed")
  }

  # Otherwise, assign a dummy function
  expect_doppelganger <- function(...) skip("vdiffr is not installed.")
}
