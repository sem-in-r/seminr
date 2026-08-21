# CRAN Repository Policy: a package "must never use more than two [cores]
# simultaneously: the check farm is a shared resource".
#
# bootstrap_model() and predict_pls() both default to cores = NULL. That default
# is what runs on the check farm, because tests, examples and vignettes rarely
# pin cores. It must therefore stay inside the cap.

test_that("the implicit core default never exceeds two", {
  # setup_parallel_cluster() resolves cores = NULL. Read the resolution rule
  # rather than starting a cluster, so the test is fast and side-effect free.
  body_txt <- paste(deparse(body(setup_parallel_cluster)), collapse = " ")
  expect_true(grepl("min(2", body_txt, fixed = TRUE),
              info = "cores = NULL must be capped at 2, not detectCores()")
  expect_false(grepl("if (is.null(cores)) parallel::detectCores()", body_txt, fixed = TRUE),
               info = "the uncapped detectCores() default has come back")
})

test_that("an explicit core request is still honoured", {
  # Capping the default must not silently override a deliberate choice.
  body_txt <- paste(deparse(body(setup_parallel_cluster)), collapse = " ")
  expect_true(grepl("else cores", body_txt, fixed = TRUE),
              info = "an explicit cores= argument must pass through unchanged")
})
