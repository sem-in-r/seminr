#!/usr/bin/env Rscript
# Build an HTML table attributing per-routine speedups to each incremental
# change step. Args: baseline.rds step1.rds step2.rds ... (in order applied)
# Emits table rows to stdout for inclusion in the performance report.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("Usage: attribution_table.R baseline.rds step1.rds ...")

runs <- lapply(args, readRDS)
tags <- sapply(runs, `[[`, "tag")

labels <- sapply(runs[[1]]$results, `[[`, "label")
medians <- sapply(runs, function(r) {
  labs <- sapply(r$results, `[[`, "label")
  sapply(labels, function(l) r$results[[which(labs == l)]]$median)
})
rownames(medians) <- labels

cat("<thead><tr><th>Routine</th>")
cat(sprintf('<th class="num">%s</th>', tags[1]))
for (t in tags[-1]) cat(sprintf('<th class="num">+ %s</th>', t))
cat("</tr></thead>\n<tbody>\n")
for (l in labels) {
  cat(sprintf("<tr><td><code>%s</code></td>", l))
  cat(sprintf('<td class="num">%.3f</td>', medians[l, 1]))
  for (j in 2:ncol(medians)) {
    pct <- (medians[l, j] - medians[l, j - 1]) / medians[l, j - 1] * 100
    cls <- if (pct <= -3) ' class="num faster"' else ' class="num"'
    cat(sprintf('<td%s>%.3f (%+.0f%%)</td>', cls, medians[l, j], pct))
  }
  cat("</tr>\n")
}
cat("</tbody>\n")

# Also a plain-text summary of each step's total contribution
cat("\n<!-- step contributions vs baseline:\n")
for (j in 2:ncol(medians)) {
  tot <- (medians[, j] - medians[, 1]) / medians[, 1] * 100
  cat(sprintf("%s: mean %+.1f%% (range %+.0f%% .. %+.0f%%)\n",
              tags[j], mean(tot), min(tot), max(tot)))
}
cat("-->\n")
