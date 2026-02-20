# HOC + Bootstrap tests — skipped on CRAN (bootstrap runs are too slow)
# Reproduces issues #299 and #205: bootstrap_model() errors with PLSc and HOC
skip_on_cran()

# ── HOC composite + reflective + bootstrap (PLSc) ───────────────────────────
# Uses a small dataset (50 rows) to force some PLSc iterations to fail,
# exercising the error-recovery code path where the length bug manifests.
# This is the core reproduction of issues #299 and #205.
context("SEMinR bootstraps HOC composite model with PLSc (#299, #205)\n")

set.seed(42)
small_mobi <- mobi[sample(nrow(mobi), 50), ]

mobi_mm_hoc_plsc <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3)),
  composite("Quality",       multi_items("PERQ", 1:7)),
  composite("Value",         multi_items("PERV", 1:2)),
  higher_composite("Satisfaction", dimensions = c("Image", "Value"),
                   method = two_stage, weights = mode_A),
  composite("Complaints",    single_item("CUSCO")),
  composite("Loyalty",       multi_items("CUSL", 1:3))
)

mobi_sm_hoc_plsc <- relationships(
  paths(from = c("Expectation", "Quality"), to = "Satisfaction"),
  paths(from = "Satisfaction", to = c("Complaints", "Loyalty"))
)

pls_hoc_plsc <- estimate_pls(data = small_mobi,
                             measurement_model = mobi_mm_hoc_plsc,
                             structural_model = mobi_sm_hoc_plsc)

test_that("bootstrap_model() succeeds with HOC composite + PLSc (#299, #205)", {
  boot_hoc_plsc <- bootstrap_model(pls_hoc_plsc, nboot = 100, cores = 2, seed = 42)

  expect_s3_class(boot_hoc_plsc, "boot_seminr_model")
  expect_equal(dim(boot_hoc_plsc$boot_paths)[1:2], dim(pls_hoc_plsc$path_coef))
  expect_equal(dim(boot_hoc_plsc$boot_loadings)[1:2], dim(pls_hoc_plsc$outer_loadings))
  expect_equal(dim(boot_hoc_plsc$boot_weights)[1:2], dim(pls_hoc_plsc$outer_weights))
})

test_that("summary() works on bootstrapped HOC + PLSc model", {
  boot_hoc_plsc <- bootstrap_model(pls_hoc_plsc, nboot = 100, cores = 2, seed = 42)
  expect_no_error(summary(boot_hoc_plsc))
})
