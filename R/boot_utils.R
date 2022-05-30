#' Return all path bootstraps as a long table
#'
#' @param pls_boot bootstrapped PLS model
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
