# Regression test for issue #318: "no package called 'seminr'" on parallel workers
# https://github.com/sem-in-r/seminr/issues/318
skip_on_cran()

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

test_that("setup_parallel_cluster() retries on another port when the default port is taken", {
  skip_on_cran()
  # Hold the port the cluster would open first, as another session might
  port <- utils::getFromNamespace("defaultClusterOptions", "parallel")$port
  blocker <- serverSocket(port)
  on.exit(close(blocker), add = TRUE)
  set.seed(1)
  seed_before <- .Random.seed
  cl <- setup_parallel_cluster(1)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  expect_s3_class(cl, "cluster")
  # The retry does not draw from the caller's random number stream
  expect_identical(.Random.seed, seed_before)
})
