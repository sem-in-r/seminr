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
  cl <- make_cluster_on_free_port(n_cores)

  # Propagate library paths so workers can find installed packages (issue #318)
  lib_paths <- .libPaths()
  parallel::clusterExport(cl, "lib_paths", envir = environment())
  parallel::clusterEvalQ(cl, .libPaths(lib_paths))
  parallel::clusterEvalQ(cl, library(seminr))

  cl
}

# Starts a PSOCK cluster, retrying on other ports when the port is taken ----
# Sessions started close together can draw the same default port, and another
# process may hold it, so parallel::makeCluster() fails to open its server
# socket. Retry ports are derived from the process id and the attempt, not
# drawn with sample(), so the caller's random number stream (e.g. the fold
# shuffles of predict_pls()) is left untouched.
make_cluster_on_free_port <- function(n_cores, retries = 5) {
  start <- function(...) {
    tryCatch(suppressWarnings(parallel::makeCluster(n_cores, ...)), error = function(e) e)
  }
  port_taken <- function(cl) inherits(cl, "error") && grepl("port|socket", conditionMessage(cl))
  cl <- start()
  attempt <- 0
  while (port_taken(cl) && attempt < retries) {
    attempt <- attempt + 1
    cl <- start(port = 11000L + (Sys.getpid() * 7L + attempt * 211L) %% 1000L)
  }
  if (inherits(cl, "error")) {
    stop("Could not start a parallel cluster: ", conditionMessage(cl),
         " Run again, or use cores = 1 to run sequentially.", call. = FALSE)
  }
  cl
}
