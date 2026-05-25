#!/usr/bin/env Rscript
# ==============================================================================
# SEMinR Benchmark Comparison
# ==============================================================================
# Compares two benchmark result files side-by-side.
#
# Usage:
#   Rscript bench/compare.R bench/results_A.rds bench/results_B.rds
#
# The first file is treated as the baseline; the second as the variant.
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  # Auto-discover: find the two most recent .rds files in bench/
  rds_files <- sort(list.files("bench", pattern = "^results_.*\\.rds$",
                               full.names = TRUE),
                    decreasing = TRUE)
  if (length(rds_files) < 2) {
    stop("Usage: Rscript bench/compare.R <baseline.rds> <variant.rds>\n",
         "  Or run two benchmarks first so auto-discovery can find them.")
  }
  args <- rds_files[2:1]  # older = baseline, newer = variant
  message("Auto-discovered files:")
  message("  Baseline: ", args[1])
  message("  Variant:  ", args[2])
}

a <- readRDS(args[1])
b <- readRDS(args[2])

divider <- paste(rep("=", 78), collapse = "")
cat("\n", divider, "\n", sep = "")
cat("SEMinR Benchmark Comparison\n")
cat(divider, "\n")
cat(sprintf("  Baseline:  %-25s  branch: %s (%s)\n", a$tag, a$branch, a$commit))
cat(sprintf("  Variant:   %-25s  branch: %s (%s)\n", b$tag, b$branch, b$commit))
cat(sprintf("  Baseline run: %s\n", a$time))
cat(sprintf("  Variant run:  %s\n", b$time))
cat(paste(rep("-", 78), collapse = ""), "\n\n")

# Match operations by label
labels_a <- sapply(a$results, `[[`, "label")
labels_b <- sapply(b$results, `[[`, "label")
common <- intersect(labels_a, labels_b)

cat(sprintf("%-42s %10s %10s %10s\n",
            "Operation", "Baseline", "Variant", "Change"))
cat(paste(rep("-", 74), collapse = ""), "\n")

for (lab in common) {
  ra <- a$results[[which(labels_a == lab)]]
  rb <- b$results[[which(labels_b == lab)]]
  pct <- (rb$median - ra$median) / ra$median * 100
  sign_char <- ifelse(pct > 0, "+", "")
  flag <- ifelse(abs(pct) > 5, " *", "")
  cat(sprintf("%-42s %9.3fs %9.3fs %+9.1f%%%s\n",
              lab, ra$median, rb$median, pct, flag))
}

cat(paste(rep("-", 74), collapse = ""), "\n")
cat("  * = change > 5%\n")
cat("  Positive % = variant is slower; negative % = variant is faster\n\n")
