# Create a parallel PSOCK cluster with seminr loaded on workers
#
# Centralizes cluster setup to ensure workers can always find and load
# seminr, regardless of the user's library path configuration.
# Fixes issue #318: "there is no package called 'seminr'" on Windows.
#
# @param cores Number of worker cores. NULL uses at most two, per CRAN policy.
# @return A parallel cluster object with seminr loaded on all workers.
setup_parallel_cluster <- function(cores = NULL) {
  # CRAN policy: a package must never use more than two cores simultaneously.
  # An explicit request from the user is honoured; the IMPLICIT default must
  # stay inside the cap, because tests, examples and vignettes run on the CRAN
  # check farm with cores unset.
  n_cores <- if (is.null(cores)) min(2L, parallel::detectCores()) else cores
  cl <- suppressWarnings(parallel::makeCluster(n_cores))

  # Propagate library paths so workers can find installed packages (issue #318)
  lib_paths <- .libPaths()
  parallel::clusterExport(cl, "lib_paths", envir = environment())
  parallel::clusterEvalQ(cl, .libPaths(lib_paths))
  parallel::clusterEvalQ(cl, library(seminr))

  cl
}
