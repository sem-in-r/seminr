# Tests for issue #318: "no package called 'seminr'" on parallel workers
# https://github.com/sem-in-r/seminr/issues/318
#
# Root cause: bootstrap_model() and predict_pls() use parSapply with
# worker functions that call seminr::estimate_pls(). The seminr::
# prefix forces namespace resolution on each worker. When workers
# can't find seminr in their .libPaths() (common on Windows where
# user library paths don't propagate), this fails.
#
# On macOS, .Library contains seminr so workers always find it.
# We simulate the restricted environment by overriding .lib.loc
# to reproduce the exact error reported in the issue.
skip_on_cran()

# Helper: force-restrict a worker's library search to tempdir()
# (a directory with no R packages). This simulates Windows
# environments where the user library doesn't propagate to
# workers. We override .lib.loc directly because .libPaths()
# always appends .Library.
restrict_worker_lib_paths <- function(cl) {
  parallel::clusterEvalQ(cl, {
    env <- environment(.libPaths)
    unlockBinding(".lib.loc", env)
    assign(".lib.loc", tempdir(), envir = env)
  })
}

context("Parallel workers can find seminr (issue #318)\n")

test_that("seminr:: self-reference via parSapply fails on restricted workers", {
  cl <- parallel::makeCluster(1)
  on.exit(parallel::stopCluster(cl))

  restrict_worker_lib_paths(cl)

  # Replicate bootstrap_model's pattern: a worker function that
  # calls seminr::estimate_pls() via parSapply
  worker_fn <- function(i) {
    is.function(seminr::estimate_pls)
  }

  # This is the exact mechanism that causes issue #318
  expect_error(
    parallel::parSapply(cl, 1, worker_fn),
    "no package called.*seminr"
  )
})

test_that("propagating lib paths and loading seminr fixes the issue", {
  cl <- parallel::makeCluster(1)
  on.exit(parallel::stopCluster(cl))

  restrict_worker_lib_paths(cl)

  # The fix: propagate parent's .libPaths() and load seminr
  lib_paths <- .libPaths()
  parallel::clusterExport(cl, "lib_paths", envir = environment())
  parallel::clusterEvalQ(cl, .libPaths(lib_paths))
  parallel::clusterEvalQ(cl, library(seminr))

  # Same worker function now succeeds
  worker_fn <- function(i) {
    is.function(estimate_pls)
  }

  result <- parallel::parSapply(cl, 1, worker_fn)
  expect_true(result[[1]])
})

# ---- Integration: bootstrap_model with cores=1 ----

context("bootstrap_model works with cores=1 (issue #318)\n")

test_that("bootstrap_model completes with cores=1", {
  set.seed(42)

  mm <- constructs(
    composite("Image",        multi_items("IMAG", 1:5)),
    composite("Expectation",  multi_items("CUEX", 1:3)),
    composite("Satisfaction", multi_items("CUSA", 1:3))
  )

  sm <- relationships(
    paths(to = "Satisfaction",
          from = c("Image", "Expectation"))
  )

  model <- estimate_pls(
    data = mobi,
    measurement_model = mm,
    structural_model = sm
  )

  boot <- bootstrap_model(
    seminr_model = model,
    nboot = 10, cores = 1, seed = 42
  )

  expect_s3_class(boot, "boot_seminr_model")
  expect_false(is.null(boot$paths_descriptives))
  expect_false(is.null(boot$boot_paths))
})
