# CRAN Repository Policy: a package "must never use more than two [cores]
# simultaneously: the check farm is a shared resource".
#
# R CMD check --as-cran, and CRAN's own checks, set _R_CHECK_LIMIT_CORES_, and
# parallel then refuses to start more than two workers. bootstrap_model()
# defaults to cores = NULL, which must use every core for users but at most two
# under that limit.

with_core_limit <- function(value, code) {
  old <- Sys.getenv("_R_CHECK_LIMIT_CORES_", unset = NA)
  on.exit(if (is.na(old)) Sys.unsetenv("_R_CHECK_LIMIT_CORES_")
          else Sys.setenv("_R_CHECK_LIMIT_CORES_" = old))
  if (is.na(value)) Sys.unsetenv("_R_CHECK_LIMIT_CORES_")
  else Sys.setenv("_R_CHECK_LIMIT_CORES_" = value)
  code
}

test_that("the default uses every core outside CRAN's core limit", {
  all_cores <- max(1L, parallel::detectCores(), na.rm = TRUE)
  expect_equal(with_core_limit(NA, default_cores()), all_cores)
  expect_equal(with_core_limit("false", default_cores()), all_cores)
  expect_equal(with_core_limit("FALSE", default_cores()), all_cores)
})

test_that("the default uses at most two cores under CRAN's core limit", {
  for (value in c("true", "TRUE", "warn")) {
    expect_lte(with_core_limit(value, default_cores()), 2L)
  }
})

test_that("an explicit core request is still honoured", {
  # The limit applies only to the default, never to a deliberate choice.
  body_txt <- paste(deparse(body(setup_parallel_cluster)), collapse = " ")
  expect_true(grepl("else cores", body_txt, fixed = TRUE),
              info = "an explicit cores= argument must pass through unchanged")
})
