#!/usr/bin/env Rscript
# ==============================================================================
# SEMinR Profiling — attribute time inside hot paths with Rprof
# ==============================================================================
# Profiles the installed seminr. Prints by-total and by-self summaries for:
#   1. estimate_pls repeated (composite, large N)
#   2. bootstrap_model serial (spec-rebuild overhead visibility)
#   3. predict_pls LOOCV, cores = 1 (so worker time is visible to Rprof)
#
# Usage: Rscript bench/profile.R [--largeN N] [--nboot N] [--top N]
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)
opt <- list(largeN = 2000L, nboot = 100L, top = 25L)
i <- 1
while (i <= length(args)) {
  switch(args[i],
    "--largeN" = { opt$largeN <- as.integer(args[i + 1]); i <- i + 2 },
    "--nboot"  = { opt$nboot  <- as.integer(args[i + 1]); i <- i + 2 },
    "--top"    = { opt$top    <- as.integer(args[i + 1]); i <- i + 2 },
    { message("Unknown argument: ", args[i]); i <- i + 1 }
  )
}

library(seminr)

mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Quality",      multi_items("PERQ", 1:7)),
  composite("Value",        multi_items("PERV", 1:2)),
  composite("Satisfaction", multi_items("CUSA", 1:3)),
  composite("Complaints",   single_item("CUSCO")),
  composite("Loyalty",      multi_items("CUSL", 1:3))
)
mobi_sm <- relationships(
  paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
  paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
  paths(from = "Quality",      to = c("Value", "Satisfaction")),
  paths(from = "Value",        to = c("Satisfaction")),
  paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
  paths(from = "Complaints",   to = "Loyalty")
)

set.seed(123)
mobi_num <- as.matrix(mobi)
mobi_large <- as.data.frame(
  mobi_num[sample.int(nrow(mobi_num), opt$largeN, replace = TRUE), ] +
    matrix(rnorm(opt$largeN * ncol(mobi_num), sd = 0.1), nrow = opt$largeN)
)

profile_block <- function(label, expr_fn) {
  prof_file <- tempfile(fileext = ".out")
  Rprof(prof_file, interval = 0.005, line.profiling = FALSE)
  expr_fn()
  Rprof(NULL)
  cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
  cat("PROFILE:", label, "\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  s <- summaryRprof(prof_file)
  cat(sprintf("Total sampled time: %.2f s\n\n", s$sampling.time))
  cat("-- by.self (top", opt$top, ") --\n")
  print(head(s$by.self, opt$top))
  cat("\n-- by.total (top", opt$top, ") --\n")
  print(head(s$by.total, opt$top))
  unlink(prof_file)
}

# 1. Repeated estimation, large N — where does simplePLS spend time?
profile_block(sprintf("estimate_pls x20 (composite, N=%d)", opt$largeN), function() {
  for (i in 1:20) {
    estimate_pls(data = mobi_large,
                 measurement_model = mobi_mm, structural_model = mobi_sm)
  }
})

# 2. Serial bootstrap — how much is estimation vs spec-rebuild vs HTMT?
mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm, structural_model = mobi_sm)
profile_block(sprintf("bootstrap_model (nboot=%d, cores=1)", opt$nboot), function() {
  bootstrap_model(seminr_model = mobi_pls,
                  nboot = opt$nboot, cores = 1, seed = 42)
})

# 3. LOOCV with cores unset: runs sequentially in this process (the parallel
# path only engages when cores is passed), so Rprof samples the fold work.
profile_block("predict_pls LOOCV (sequential)", function() {
  set.seed(42)
  predict_pls(model = mobi_pls, technique = predict_DA,
              noFolds = NULL, reps = 1)
})
