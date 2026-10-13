# Create a parallel PSOCK cluster with seminr loaded on workers
#
# Centralizes cluster setup to ensure workers can always find and load
# seminr, regardless of the user's library path configuration.
# Fixes issue #318: "there is no package called 'seminr'" on Windows.
#
# @param cores Number of worker cores. NULL uses default_cores().
# @return A parallel cluster object with seminr loaded on all workers.
setup_parallel_cluster <- function(cores = NULL) {
  n_cores <- if (is.null(cores)) default_cores() else cores
  cl <- make_cluster_on_free_port(n_cores)

  # Propagate library paths so workers can find installed packages (issue #318)
  lib_paths <- .libPaths()
  parallel::clusterExport(cl, "lib_paths", envir = environment())
  parallel::clusterEvalQ(cl, .libPaths(lib_paths))
  parallel::clusterEvalQ(cl, library(seminr))

  cl
}

# Number of cores used when cores = NULL ----
# Every core, for users. Under R CMD check with CRAN's core limit
# (_R_CHECK_LIMIT_CORES_, set by --as-cran and in CRAN's checks) at most two:
# CRAN policy allows no more on its shared check farm, and parallel itself then
# refuses to start more workers ("simultaneous processes spawned"). An explicit
# cores argument is never changed.
default_cores <- function() {
  n <- parallel::detectCores()
  if (is.na(n) || n < 1L) n <- 1L
  if (check_limits_cores()) n <- min(2L, n)
  as.integer(n)
}

# TRUE under CRAN's core limit, by the rule parallel's own check uses
# (parallel:::.check_ncores)
check_limits_cores <- function() {
  chk <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
  nzchar(chk) && chk != "false"
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
