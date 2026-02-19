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


# --- Plot regression testing (moved from R/plot_test_utils.R) ---

str_standardise <- function(s, sep = "-") {
  stopifnot(length(s) == 1 && is.character(s))
  s <- gsub("[^a-z0-9]", sep, tolower(s))
  s <- gsub(paste0(sep, sep, "+"), sep, s)
  s <- gsub(paste0("^", sep, "|", sep, "$"), "", s)
  s
}

# Compare a plot against a saved reference PNG via MD5 checksum.
# If no reference exists, creates one and returns FALSE.
check_test_plot <- function(plot, title, plot_dir = "regression_plots", refresh = FALSE) {
  path <- testthat::test_path("..", plot_dir)
  if (!dir.exists(path)) {
    dir.create(path)
  }
  testthat::skip_on_ci()

  filename <- paste0(str_standardise(title), ".png")
  full_name <- file.path(path, filename)

  if (file.exists(full_name)) {
    test_name <- file.path(tempdir(), filename)
    save_plot(test_name, plot = plot)
    return(tools::md5sum(test_name) == tools::md5sum(full_name))
  } else {
    save_plot(full_name, plot = plot)
    message(paste("New reference plot created for:", title))
    return(FALSE)
  }
}
