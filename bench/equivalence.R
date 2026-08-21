#!/usr/bin/env Rscript
# ==============================================================================
# SEMinR Behavioral-Equivalence Checker
# ==============================================================================
# Captures reference outputs of all user-facing PLS routines on a baseline
# build, then verifies a modified build reproduces them BIT-IDENTICALLY.
# Used by the `performance` branch to prove optimizations change nothing
# but speed. Runs against the INSTALLED seminr (devtools::install() first).
#
# Usage:
#   Rscript bench/equivalence.R --capture     # save baseline fixtures
#   Rscript bench/equivalence.R --check       # compare against fixtures
#
# Options:
#   --file PATH   Fixture file (default: bench/equiv_baseline.rds)
#   --tol X       Tolerance for --check (default: 0 = bit-identical).
#                 Use a tiny tolerance only for changes documented as
#                 floating-point-noise-only (record in the plan/report).
#
# Fixtures are machine-specific (BLAS-dependent): capture and check must run
# on the same machine. Fixture .rds files are gitignored (bench/*.rds).
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)
opt <- list(mode = NULL, file = "bench/equiv_baseline.rds", tol = 0)
i <- 1
while (i <= length(args)) {
  switch(args[i],
    "--capture" = { opt$mode <- "capture"; i <- i + 1 },
    "--check"   = { opt$mode <- "check";   i <- i + 1 },
    "--file"    = { opt$file <- args[i + 1]; i <- i + 2 },
    "--tol"     = { opt$tol  <- as.numeric(args[i + 1]); i <- i + 2 },
    { stop("Unknown argument: ", args[i]) }
  )
}
if (is.null(opt$mode)) stop("Specify --capture or --check")

suppressMessages(library(seminr))
cat(sprintf("seminr %s | mode: %s | fixtures: %s\n\n",
            packageVersion("seminr"), opt$mode, opt$file))

# ── Model specs ─────────────────────────────────────────────────
mm_composite <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Quality",      multi_items("PERQ", 1:7)),
  composite("Value",        multi_items("PERV", 1:2)),
  composite("Satisfaction", multi_items("CUSA", 1:3)),
  composite("Complaints",   single_item("CUSCO")),
  composite("Loyalty",      multi_items("CUSL", 1:3))
)
mm_reflective <- as.reflective(mm_composite)
sm_full <- relationships(
  paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
  paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
  paths(from = "Quality",      to = c("Value", "Satisfaction")),
  paths(from = "Value",        to = c("Satisfaction")),
  paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
  paths(from = "Complaints",   to = "Loyalty")
)
make_mm_interaction <- function(method) {
  constructs(
    composite("Image",        multi_items("IMAG", 1:5)),
    composite("Expectation",  multi_items("CUEX", 1:3)),
    composite("Satisfaction", multi_items("CUSA", 1:3)),
    interaction_term(iv = "Image", moderator = "Expectation", method = method)
  )
}
sm_interaction <- relationships(
  paths(from = c("Image", "Expectation", "Image*Expectation"),
        to = "Satisfaction")
)
mm_hoc <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Quality",      multi_items("PERQ", 1:7)),
  composite("Value",        multi_items("PERV", 1:2)),
  higher_composite("Perception", dimensions = c("Quality", "Value"),
                   method = two_stage),
  composite("Satisfaction", multi_items("CUSA", 1:3))
)
sm_hoc <- relationships(
  paths(from = c("Image", "Expectation", "Perception"), to = "Satisfaction")
)

# ── Extractors: the user-visible numeric surface of each object ─
model_surface <- function(m) {
  list(path_coef       = m$path_coef,
       outer_weights   = m$outer_weights,
       outer_loadings  = m$outer_loadings,
       rSquared        = m$rSquared,
       construct_scores = m$construct_scores)
}
boot_surface <- function(b) {
  list(paths_descriptives    = b$paths_descriptives,
       loadings_descriptives = b$loadings_descriptives,
       weights_descriptives  = b$weights_descriptives,
       HTMT_descriptives     = b$HTMT_descriptives,
       total_paths_descriptives = b$total_paths_descriptives,
       boot_paths            = b$boot_paths)
}
predict_surface <- function(p) {
  list(items = p$items, composites = p$composites)
}

# ── Compute all reference outputs ───────────────────────────────
compute_all <- function() {
  out <- list()
  run <- function(label, fn) {
    cat(sprintf("  computing: %-38s", label))
    t <- system.time(res <- fn())
    cat(sprintf("(%.2f s)\n", t[["elapsed"]]))
    out[[label]] <<- res
  }

  pls_comp <- estimate_pls(mobi, mm_composite, sm_full)
  run("estimate_composite", function() model_surface(pls_comp))
  run("estimate_plsc", function()
    model_surface(estimate_pls(mobi, mm_reflective, sm_full)))
  run("estimate_interaction_orthogonal", function()
    model_surface(estimate_pls(mobi, make_mm_interaction(orthogonal), sm_interaction)))
  run("estimate_interaction_two_stage", function()
    model_surface(estimate_pls(mobi, make_mm_interaction(two_stage), sm_interaction)))
  run("estimate_interaction_prodind", function()
    model_surface(estimate_pls(mobi, make_mm_interaction(product_indicator), sm_interaction)))
  run("estimate_hoc_two_stage", function()
    model_surface(estimate_pls(mobi, mm_hoc, sm_hoc)))

  run("bootstrap_cores1", function()
    boot_surface(bootstrap_model(pls_comp, nboot = 50, cores = 1, seed = 42)))
  run("bootstrap_cores2", function()
    boot_surface(bootstrap_model(pls_comp, nboot = 50, cores = 2, seed = 42)))

  pls_int <- estimate_pls(mobi, make_mm_interaction(orthogonal), sm_interaction)
  run("bootstrap_interaction", function()
    boot_surface(bootstrap_model(pls_int, nboot = 50, cores = 1, seed = 42)))

  run("predict_kfold", function() {
    set.seed(42)
    predict_surface(predict_pls(pls_comp, technique = predict_DA,
                                noFolds = 10, reps = 1))
  })
  run("predict_loocv_subset", function() {
    # LOOCV on a subset (rerun on 80 rows) to keep runtime manageable
    pls_small <- rerun(pls_comp, data = mobi[1:80, ])
    set.seed(42)
    predict_surface(predict_pls(pls_small, technique = predict_DA,
                                noFolds = NULL, reps = 1))
  })
  run("predict_ea", function() {
    set.seed(42)
    predict_surface(predict_pls(pls_comp, technique = predict_EA,
                                noFolds = 10, reps = 1))
  })

  run("predict_interaction_orthogonal", function() {
    pls_io <- estimate_pls(mobi, make_mm_interaction(orthogonal), sm_interaction)
    set.seed(42)
    predict_surface(predict_pls(pls_io, technique = predict_DA, noFolds = 10, reps = 1))
  })
  run("predict_interaction_prodind", function() {
    pls_ip <- estimate_pls(mobi, make_mm_interaction(product_indicator), sm_interaction)
    set.seed(42)
    predict_surface(predict_pls(pls_ip, technique = predict_DA, noFolds = 10, reps = 1))
  })
  run("predict_interaction_two_stage", function() {
    pls_i2 <- estimate_pls(mobi, make_mm_interaction(two_stage), sm_interaction)
    set.seed(42)
    predict_surface(predict_pls(pls_i2, technique = predict_DA, noFolds = 10, reps = 1))
  })

  run("mga", function()
    suppressMessages(estimate_pls_mga(pls_comp, mobi$CUEX1 < 8,
                                      nboot = 50, cores = 1, seed = 42)))

  run("summary_boot", function() {
    b <- bootstrap_model(pls_comp, nboot = 50, cores = 1, seed = 42)
    s <- summary(b)
    list(bootstrapped_paths = s$bootstrapped_paths,
         bootstrapped_loadings = s$bootstrapped_loadings,
         bootstrapped_weights = s$bootstrapped_weights,
         bootstrapped_HTMT = s$bootstrapped_HTMT,
         bootstrapped_total_paths = s$bootstrapped_total_paths)
  })

  out
}

# ── Deep comparison with reporting ──────────────────────────────
max_diff <- function(a, b, path = "") {
  if (is.list(a) && is.list(b)) {
    if (!identical(names(a), names(b)))
      return(data.frame(path = path, diff = Inf,
                        note = "names/structure differ"))
    do.call(rbind, lapply(names(a), function(n)
      max_diff(a[[n]], b[[n]], paste0(path, "$", n))))
  } else if (is.numeric(a) && is.numeric(b)) {
    if (!identical(dim(a), dim(b)) || length(a) != length(b))
      return(data.frame(path = path, diff = Inf, note = "dims differ"))
    d <- suppressWarnings(max(abs(as.numeric(a) - as.numeric(b)), na.rm = TRUE))
    na_mismatch <- !identical(is.na(a), is.na(b))
    data.frame(path = path, diff = if (na_mismatch) Inf else d,
               note = if (na_mismatch) "NA pattern differs" else "")
  } else {
    data.frame(path = path, diff = if (identical(a, b)) 0 else Inf,
               note = if (identical(a, b)) "" else "non-numeric mismatch")
  }
}

# ── Main ────────────────────────────────────────────────────────
if (opt$mode == "capture") {
  fixtures <- compute_all()
  dir.create(dirname(opt$file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(list(fixtures = fixtures,
               seminr_version = as.character(packageVersion("seminr")),
               r_version = R.version.string,
               blas = extSoftVersion()[["BLAS"]],
               commit = tryCatch(trimws(system("git rev-parse --short HEAD",
                                               intern = TRUE)),
                                 error = function(e) "unknown"),
               time = Sys.time()),
          opt$file)
  cat(sprintf("\nFixtures saved to %s\n", opt$file))
} else {
  ref <- readRDS(opt$file)
  cat(sprintf("Baseline: seminr %s @ %s (%s)\n\n",
              ref$seminr_version, ref$commit, ref$time))
  cur <- compute_all()

  cat("\nComparison (tolerance:", format(opt$tol), ")\n")
  failures <- 0
  for (label in names(ref$fixtures)) {
    if (!label %in% names(cur)) {
      cat(sprintf("  %-38s MISSING\n", label)); failures <- failures + 1; next
    }
    if (identical(ref$fixtures[[label]], cur[[label]])) {
      cat(sprintf("  %-38s IDENTICAL\n", label)); next
    }
    diffs <- max_diff(ref$fixtures[[label]], cur[[label]])
    worst <- diffs[which.max(diffs$diff), ]
    if (worst$diff <= opt$tol) {
      cat(sprintf("  %-38s within tol (max diff %.3g at %s)\n",
                  label, worst$diff, worst$path))
    } else {
      cat(sprintf("  %-38s FAILED (max diff %.3g at %s %s)\n",
                  label, worst$diff, worst$path, worst$note))
      failures <- failures + 1
    }
  }
  cat(sprintf("\n%s\n", if (failures == 0) "ALL EQUIVALENT" else
    sprintf("%d FAILURE(S) — outputs changed!", failures)))
  quit(status = if (failures > 0) 1L else 0L)
}
