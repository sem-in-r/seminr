#!/usr/bin/env Rscript
# ==============================================================================
# SEMinR Performance Benchmark
# ==============================================================================
# Benchmarks heavy operations (estimation, bootstrap, prediction) using the
# currently installed version of seminr. Designed for comparing across branches.
#
# Usage:
#   # 1. Install the branch you want to benchmark:
#   devtools::install()
#
#   # 2. Run the benchmark:
#   Rscript bench/benchmark.R                   # defaults
#   Rscript bench/benchmark.R --reps 10         # more repetitions
#   Rscript bench/benchmark.R --nboot 500       # larger bootstrap
#   Rscript bench/benchmark.R --tag develop      # custom label
#
# Arguments:
#   --reps N      Number of timing repetitions per operation (default: 5)
#   --nboot N     Bootstrap iterations (default: 200)
#   --folds N     Cross-validation folds for predict (default: 10)
#   --tag LABEL   Label for output file (default: git branch name)
#   --outdir DIR  Output directory for results (default: bench/)
# ==============================================================================

# ── Parse arguments ──────────────────────────────────────────────
parse_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  defaults <- list(reps = 5L, nboot = 200L, folds = 10L, tag = NULL, outdir = "bench")
  i <- 1
  while (i <= length(args)) {
    switch(args[i],
      "--reps"   = { defaults$reps   <- as.integer(args[i + 1]); i <- i + 2 },
      "--nboot"  = { defaults$nboot  <- as.integer(args[i + 1]); i <- i + 2 },
      "--folds"  = { defaults$folds  <- as.integer(args[i + 1]); i <- i + 2 },
      "--tag"    = { defaults$tag    <- args[i + 1];              i <- i + 2 },
      "--outdir" = { defaults$outdir <- args[i + 1];              i <- i + 2 },
      { message("Unknown argument: ", args[i]); i <- i + 1 }
    )
  }
  defaults
}

opts <- parse_args()

# ── Detect branch/commit info ───────────────────────────────────
git_info <- function() {
  safe <- function(cmd) tryCatch(trimws(system(cmd, intern = TRUE)), error = function(e) "unknown")
  list(
    branch = safe("git rev-parse --abbrev-ref HEAD"),
    commit = safe("git rev-parse --short HEAD")
  )
}

gi <- git_info()
tag <- if (!is.null(opts$tag)) opts$tag else gi$branch

# ── Banner ───────────────────────────────────────────────────────
divider <- paste(rep("=", 65), collapse = "")
cat(divider, "\n")
cat("SEMinR Performance Benchmark\n")
cat(divider, "\n")
cat(sprintf("  Tag:          %s\n", tag))
cat(sprintf("  Branch:       %s (%s)\n", gi$branch, gi$commit))
cat(sprintf("  Reps:         %d\n", opts$reps))
cat(sprintf("  Bootstrap:    %d iterations\n", opts$nboot))
cat(sprintf("  CV folds:     %d\n", opts$folds))
cat(sprintf("  R version:    %s\n", R.version.string))
cat(sprintf("  Platform:     %s\n", R.version$platform))
cat(sprintf("  Timestamp:    %s\n", Sys.time()))
cat(sprintf("  seminr ver:   %s\n", as.character(packageVersion("seminr"))))
cat(paste(rep("-", 65), collapse = ""), "\n\n")

library(seminr)

# ── Model specification (ECSI model on mobi data) ───────────────
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

mobi_mm_plsc <- constructs(
  reflective("Image",        multi_items("IMAG", 1:5)),
  reflective("Expectation",  multi_items("CUEX", 1:3)),
  reflective("Quality",      multi_items("PERQ", 1:7)),
  reflective("Value",        multi_items("PERV", 1:2)),
  reflective("Satisfaction", multi_items("CUSA", 1:3)),
  reflective("Complaints",   single_item("CUSCO")),
  reflective("Loyalty",      multi_items("CUSL", 1:3))
)

# ── Timing harness ───────────────────────────────────────────────
bench_op <- function(label, expr_fn, reps = opts$reps) {
  times <- numeric(reps)
  cat(sprintf("  %-42s", label))
  for (i in seq_len(reps)) {
    gc(verbose = FALSE)
    t <- system.time(expr_fn())
    times[i] <- t["elapsed"]
  }
  med <- median(times)
  cat(sprintf("median = %7.3f s  (range: %.3f - %.3f)\n",
              med, min(times), max(times)))
  list(label = label, median = med, min = min(times),
       max = max(times), times = times)
}

results <- list()

# ── 1. PLS Estimation ───────────────────────────────────────────
cat("1. PLS Estimation (composite, ECSI model, 250 obs)\n")
results$estimate_pls <- bench_op("estimate_pls [composite]", function() {
  estimate_pls(data = mobi,
               measurement_model = mobi_mm,
               structural_model  = mobi_sm)
})

# ── 2. PLSc Estimation ──────────────────────────────────────────
cat("\n2. PLSc Estimation (reflective constructs)\n")
results$estimate_plsc <- bench_op("estimate_pls [PLSc/reflective]", function() {
  estimate_pls(data = mobi,
               measurement_model = mobi_mm_plsc,
               structural_model  = mobi_sm)
})

# ── 3. Bootstrap ────────────────────────────────────────────────
mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         structural_model  = mobi_sm)

cat(sprintf("\n3. Bootstrap (nboot=%d, cores=1)\n", opts$nboot))
results$bootstrap <- bench_op(
  sprintf("bootstrap_model [nboot=%d]", opts$nboot),
  function() {
    bootstrap_model(seminr_model = mobi_pls,
                    nboot = opts$nboot, cores = 1, seed = 42)
  }
)

# ── 4. Bootstrap summary ────────────────────────────────────────
boot_pls <- bootstrap_model(seminr_model = mobi_pls,
                            nboot = opts$nboot, cores = 1, seed = 42)

cat("\n4. Summary of bootstrapped model\n")
results$boot_summary <- bench_op("summary(boot_model)", function() {
  invisible(capture.output(summary(boot_pls)))
})

# ── 5. PLSpredict k-fold CV ─────────────────────────────────────
cat(sprintf("\n5. PLSpredict (%d-fold CV)\n", opts$folds))
results$predict_kfold <- bench_op(
  sprintf("predict_pls [%d-fold]", opts$folds),
  function() {
    predict_pls(model     = mobi_pls,
                technique = predict_DA,
                noFolds   = opts$folds,
                reps      = 1)
  },
  reps = 3L  # fewer reps — CV has inherent variance
)

# ── Summary table ────────────────────────────────────────────────
cat("\n", divider, "\n", sep = "")
cat("SUMMARY\n")
cat(divider, "\n")
cat(sprintf("%-45s %10s\n", "Operation", "Median (s)"))
cat(paste(rep("-", 57), collapse = ""), "\n")
for (r in results) {
  cat(sprintf("%-45s %10.3f\n", r$label, r$median))
}
cat(paste(rep("-", 57), collapse = ""), "\n")
cat(sprintf("Tag: %s | Branch: %s (%s)\n", tag, gi$branch, gi$commit))

# ── Save results ─────────────────────────────────────────────────
dir.create(opts$outdir, showWarnings = FALSE, recursive = TRUE)
safe_tag <- gsub("[^a-zA-Z0-9._-]", "_", tag)
out_file <- file.path(opts$outdir, sprintf("results_%s_%s.rds", safe_tag, gi$commit))
saveRDS(list(
  tag     = tag,
  branch  = gi$branch,
  commit  = gi$commit,
  time    = Sys.time(),
  opts    = opts,
  results = results
), out_file)
cat(sprintf("\nResults saved to: %s\n", out_file))
