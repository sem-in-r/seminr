#' Return all path bootstraps as a long dataframe.
#' Columns of the dataframes are specified paths and rows are the estimated
#' coefficients for the paths at each bootstrap iteration.
#'
#' @param pls_boot bootstrapped PLS model
#'
#' @examples
#' data(mobi)
#'
#' mobi_mm <- constructs(
#'   composite("Image",        multi_items("IMAG", 1:5)),
#'   composite("Expectation",  multi_items("CUEX", 1:3)),
#'   composite("Satisfaction", multi_items("CUSA", 1:3))
#' )
#'
#' mobi_sm <- relationships(
#'   paths(from = c("Image", "Expectation"), to = "Satisfaction")
#' )
#'
#' pls_model <- estimate_pls(data = mobi,
#'                           measurement_model = mobi_mm,
#'                           structural_model = mobi_sm)
#'
#' pls_boot <- bootstrap_model(seminr_model = pls_model,
#'                             nboot = 50, cores = 2, seed = NULL)
#'
#' boot_paths_df(pls_boot)
#'
#' @export
boot_paths_df <- function(pls_boot) {
  path_names <- apply(pls_boot$smMatrix, 1, \(path) {
    paste(path['source'], '->', path['target'])
  })

  boot_paths <- apply(pls_boot$smMatrix, 1, \(path) {
    pls_boot$boot_paths[path['source'], path['target'], 1:pls_boot$boots]
  })

  colnames(boot_paths) <- path_names
  boot_paths
}
