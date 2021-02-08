str_standardise <- function(s, sep = "-")
{
  stopifnot(length(s) == 1 && is.character(s))
  s <- gsub("[^a-z0-9]", sep, tolower(s))
  s <- gsub(paste0(sep, sep, "+"), sep, s)
  s <- gsub(paste0("^", sep, "|", sep, "$"), "", s)
  s
}


#' A function to create regression plots (maybe not needed?)
#'
#' @param plot the plot
#' @param title a unique title
#' @param plot_dir optional directory name
#' @param refresh whether to refresh all test cases
#'
#'
#' @return TRUE if plots were the same, FALSE if they did not exist, or failed
#' @export
#'
check_test_plot <- function(plot, title, plot_dir = "regression_plots", refresh = FALSE){

  path <- testthat::test_path("..", plot_dir)
  if (!dir.exists(path)) {
    dir.create(path)
  }
  testthat::skip_on_ci()
  #testthat::skip_on_cran()
  #testthat::skip_on_covr()
  filename <- paste0(str_standardise(title), ".png")
  full_name <- file.path(path, filename)



  if (file.exists(full_name)) {
    # Testing
    filename <- paste0(str_standardise(title), ".png")
    test_name <- file.path(tempdir(), filename)
    save_plot(test_name, plot = plot)

    return(tools::md5sum(test_name) == tools::md5sum(full_name))

  } else {
    # Setup
    save_plot(full_name, plot = plot)
    message(paste("New reference plot created for:", title))
    return(FALSE)
  }
}
