# Create a parallel PSOCK cluster with seminr loaded on workers
#
# Centralizes cluster setup to ensure workers can always find and load
# seminr, regardless of the user's library path configuration.
# Fixes issue #318: "there is no package called 'seminr'" on Windows.
#
# @param cores Number of worker cores. NULL uses all detected cores.
# @return A parallel cluster object with seminr loaded on all workers.
setup_parallel_cluster <- function(cores = NULL) {
  n_cores <- if (is.null(cores)) parallel::detectCores() else cores
  cl <- suppressWarnings(parallel::makeCluster(n_cores))

  # Propagate library paths so workers can find installed packages (issue #318)
  lib_paths <- .libPaths()
  parallel::clusterExport(cl, "lib_paths", envir = environment())
  parallel::clusterEvalQ(cl, .libPaths(lib_paths))
  parallel::clusterEvalQ(cl, library(seminr))

  cl
}
