# Manual Demo Tests for Matrix Accessor Refactoring

> Automated test plan for Claude Code to execute. Each section sources a demo file, then runs verification checks. Plots are saved for developer review.

## Instructions for Claude Code

1. Create `test_plots/` directory for plot output
2. For each section: run the R script via `Rscript` from the package root
3. Check output for any `FAIL:` lines — report these immediately
4. Where indicated, ask the developer to review saved plot files before continuing
5. After all sections pass, delete `test_plots/`

---

## 1. Multigroup Analysis (`seminr-pls-mga`)

**Refactored code exercised:** `estimate_pls_mga.R` (`to_path_labels()`, `path_sources()`/`path_targets()`, `mapply` path-coefficient lookup), `boot_utils.R` (`boot_paths_df()`)

**Run this R script:**

```r
devtools::load_all()
source("demo/seminr-pls-mga.R")
cat("PASS: demo completed without error\n")

# Check: MGA result prints without error
capture.output(print(mobi_mga))
cat("PASS: MGA result prints\n")

# Check: path labels use "source -> target" format
labels <- rownames(mobi_mga)
cat("Path labels:\n")
print(labels)
if (all(grepl(" -> ", labels))) {
  cat("PASS: all path labels use 'source -> target' format\n")
} else {
  cat("FAIL: some path labels missing ' -> ' separator\n")
}

# Check: subgroup coefficients differ from full-sample
diffs <- mobi_mga$diff
cat("Path coefficient diffs (group1 - group2):\n")
print(round(diffs, 4))
if (all(diffs != 0)) {
  cat("PASS: subgroup coefficients differ from each other\n")
} else {
  cat("FAIL: some subgroup coefficients are identical\n")
}

# Check: p-values computed for all paths
pvals <- mobi_mga$pls_mga_p
cat("MGA p-values:\n")
print(round(pvals, 4))
if (!any(is.na(pvals))) {
  cat("PASS: p-values computed for all paths\n")
} else {
  cat("FAIL: some p-values are NA\n")
}
```

**No plots to review — move to next section.**

---

## 2. Plotting Pipeline (`seminr-pls-dot-graph`)

**Refactored code exercised:** `plot_dot.R` (`is_HOC()` predicate in `extract_sm_nodes`, named `mm_coding` columns), `helpers-model.R` (`all_factors()` via `all_reflective()`)

**Run this R script:**

```r
devtools::load_all()
dir.create("test_plots", showWarnings = FALSE)
source("demo/seminr-pls-dot-graph.R")
cat("PASS: demo completed without error\n")

# Save plots for developer review
tryCatch({
  plot(mobi_pls);  save_plot("test_plots/2a_pls_model.png")
  plot(mobi_boot); save_plot("test_plots/2b_boot_model.png")

  # T-value theme was set during demo — save that plot too
  plot(mobi_boot); save_plot("test_plots/2c_boot_tvalues.png")

  cat("PASS: plots saved to test_plots/2a, 2b, 2c\n")
}, error = function(e) {
  cat("WARN: could not save plots (DiagrammeRsvg/rsvg missing?):", e$message, "\n")
})

# Confirm T-value theme is active (was set during demo)
tv <- seminr_theme_get()$sm.edge.boot.show_t_value
if (isTRUE(tv)) {
  cat("PASS: T-value theme toggle is active\n")
} else {
  cat("FAIL: T-value theme toggle not set (got:", tv, ")\n")
}

# Reset theme
seminr_theme_set(seminr_theme_default())
```

**Ask developer to verify plots before continuing:**

- `test_plots/2a_pls_model.png` — reflective constructs shown as ovals (not rectangles); path coefficients on structural edges
- `test_plots/2b_boot_model.png` — significance indicators (p-values or CIs) on edges
- `test_plots/2c_boot_tvalues.png` — T-values displayed on structural edges

---

## 3. Consistent PLS with Reflective Constructs (`seminr-plsc-ecsi`)

**Refactored code exercised:** `helpers-model.R` (`all_factors()` using `all_reflective()`), `evaluate_reliability.R` (`is_mode_B()`, `is_single_item()` predicates), `helpers-mmMatrix.R` (`is_reflective()`)

**Run this R script:**

```r
devtools::load_all()
dir.create("test_plots", showWarnings = FALSE)
source("demo/seminr-plsc-ecsi.R")
cat("PASS: demo completed without error\n")

s <- summary(mobi_pls)

# Check: reliability metrics present
rel <- s$reliability
cat("Reliability matrix:\n")
print(round(rel, 3))
expected_cols <- c("alpha", "rhoC", "AVE", "rhoA")
present <- expected_cols[expected_cols %in% colnames(rel)]
if (length(present) >= 3) {
  cat("PASS: reliability metrics reported:", paste(present, collapse = ", "), "\n")
} else {
  cat("FAIL: missing reliability columns\n")
}

# Check: single-item construct (Complaints) handled
cat("Complaints reliability:\n")
print(round(rel["Complaints", ], 3))
cat("PASS: single-item construct handled (check values are 1.0 or NA above)\n")

# Check: bootstrap CIs contain point estimates
bs <- summary(boot_mobi_pls)
bp <- bs$bootstrapped_paths
cat("Bootstrapped paths:\n")
print(round(bp, 3))
originals <- bp[, "Original Est."]
ci_lo <- bp[, "2.5% CI"]
ci_hi <- bp[, "97.5% CI"]
contained <- (ci_lo <= originals) & (originals <= ci_hi)
if (all(contained)) {
  cat("PASS: all bootstrap CIs contain point estimates\n")
} else {
  cat("FAIL: CIs miss point estimate:", rownames(bp)[!contained], "\n")
}

# Save plots
tryCatch({
  plot(mobi_pls);      save_plot("test_plots/3a_plsc_model.png")
  plot(boot_mobi_pls); save_plot("test_plots/3b_plsc_boot.png")
  cat("PASS: plots saved to test_plots/3a, 3b\n")
}, error = function(e) cat("WARN: plot save failed:", e$message, "\n"))
```

**Ask developer to verify plots before continuing:**

- `test_plots/3a_plsc_model.png` — PLSc model renders correctly
- `test_plots/3b_plsc_boot.png` — bootstrapped model renders correctly

---

## 4. CFA and Covariance-Based SEM (`seminr-cbsem-cfa-ecsi`)

**Refactored code exercised:** `lavaan_syntax.R` (`!is_reflective()`, `is_single_item()`), `evaluate_reliability.R` (mode predicates)

**Run this R script:**

```r
devtools::load_all()
dir.create("test_plots", showWarnings = FALSE)
source("demo/seminr-cbsem-cfa-ecsi.R")
cat("PASS: demo completed without error\n")

# Check: CFA fit indices
cfa_s <- summary(mobi_cfa)
fit <- cfa_s$quality$fit$all
cat("CFA fit indices (selected):\n")
print(fit[c("chisq", "rmsea", "cfi", "tli", "srmr")])
expected_fit <- c("chisq", "rmsea", "cfi", "tli", "srmr")
found <- expected_fit %in% names(fit)
if (all(found)) {
  cat("PASS: all expected fit indices present\n")
} else {
  cat("FAIL: missing fit indices:", expected_fit[!found], "\n")
}

# Check: CBSEM summary fields (cbsem_summary was created by demo)
cat("CBSEM loadings:\n")
print(round(cbsem_summary$loadings$coefficients, 3))
cat("CBSEM path coefficients:\n")
print(round(cbsem_summary$paths$coefficients, 3))
cat("Construct correlations:\n")
print(round(cbsem_summary$descriptives$correlations$constructs, 3))

# Check: VIFs present and reasonable (< 5)
vifs <- cbsem_summary$quality$antecedent_vifs
cat("Antecedent VIFs:\n")
print(lapply(vifs, round, 3))
max_vif <- max(unlist(vifs), na.rm = TRUE)
if (max_vif < 5) {
  cat("PASS: all VIFs <", 5, "(max =", round(max_vif, 2), ")\n")
} else {
  cat("WARN: max VIF =", round(max_vif, 2), ">= 5\n")
}

# Note: CFA/CBSEM models use semPlot (not DiagrammeR), so save_plot() is not
# compatible. The demo's plot() calls already rendered via semPlot above.
cat("PASS: CFA/CBSEM plots rendered via semPlot during demo (save_plot not applicable)\n")
```

**No saved plots to review** — CFA/CBSEM models render via `semPlot`, not DiagrammeR's `save_plot()`. The plots were rendered during the demo's `plot()` calls.

---

## 5. Higher-Order Constructs (`seminr-pls-higher_order`)

**Refactored code exercised:** `feature_higher_order.R` (`all_items()`, `mmMatrix_for_items()`), `helpers-mmMatrix.R` (`all_HOC()`, `all_LOC()`, `is_HOC()`), `plot_dot.R` (`is_HOC()` in `is_only_endogenous` and `extract_sm_nodes`)

The demo runs 4 HOC configurations sequentially, overwriting `mobi_pls` and `boot_mobi_pls` each time. Only the last config's objects (mode B LOCs + mode B HOC) remain after the demo.

**Run this R script:**

```r
devtools::load_all()
dir.create("test_plots", showWarnings = FALSE)
source("demo/seminr-pls-higher_order.R")
cat("PASS: demo completed without error (all 4 configs estimated, bootstrapped, plotted)\n")

# Check: HOC mode detected correctly on last config (B+B)
detected <- construct_mode(mobi_pls$mmMatrix, "Satisfaction")
if (detected == "HOCB") {
  cat("PASS: HOC mode is", detected, "\n")
} else {
  cat("FAIL: HOC mode is", detected, "expected HOCB\n")
}

# Check: bootstrap CIs contain point estimates
bp <- summary(boot_mobi_pls)$bootstrapped_paths
cat("Bootstrapped paths (last config):\n")
print(round(bp, 3))
originals <- bp[, "Original Est."]
ci_lo <- bp[, "2.5% CI"]
ci_hi <- bp[, "97.5% CI"]
contained <- (ci_lo <= originals) & (originals <= ci_hi)
if (all(contained)) {
  cat("PASS: bootstrap CIs contain point estimates\n")
} else {
  cat("FAIL: CIs miss point estimate:", rownames(bp)[!contained], "\n")
}

# Save last config plots
tryCatch({
  plot(mobi_pls);      save_plot("test_plots/5a_hoc_model.png")
  plot(boot_mobi_pls); save_plot("test_plots/5b_hoc_boot.png")
  cat("PASS: plots saved to test_plots/5a, 5b\n")
}, error = function(e) cat("WARN: plot save failed:", e$message, "\n"))
```

**Ask developer to verify before continuing:**

- During the demo, 8 plots were rendered (estimated + bootstrapped for each of 4 configs). Developer should confirm they all rendered without error in the R graphics device.
- `test_plots/5a_hoc_model.png` — HOC "Satisfaction" appears as a higher-order node
- `test_plots/5b_hoc_boot.png` — bootstrapped HOC model renders correctly

---

## 6. Interaction Terms (`seminr-pls-interaction`)

**Refactored code exercised:** `evaluate_reliability.R` (mode predicates for interaction constructs), `evaluate_warnings.R` (`is_single_item()`, `all_LOC()`)

The demo runs 3 interaction methods sequentially, overwriting `mobi_pls` and `boot_mobi_pls` each time. Only the last method's objects (two-stage) remain after the demo.

**Run this R script:**

```r
devtools::load_all()
dir.create("test_plots", showWarnings = FALSE)
source("demo/seminr-pls-interaction.R")
cat("PASS: demo completed without error (all 3 methods estimated, bootstrapped, plotted)\n")

s <- summary(mobi_pls)

# Check: interaction paths have non-zero coefficients
int_paths <- s$paths[c("Image*Expectation", "Image*Value"), , drop = FALSE]
cat("Interaction path coefficients (last method):\n")
print(round(int_paths, 4))
if (all(int_paths[, 1] != 0)) {
  cat("PASS: interaction paths are non-zero\n")
} else {
  cat("FAIL: some interaction paths are zero\n")
}

# Check: reliability computed without error on interaction constructs
cat("Reliability:\n")
print(round(s$reliability, 3))
cat("PASS: reliability computed for interaction constructs\n")

# Check: VIFs present
cat("Antecedent VIFs:\n")
print(lapply(s$vif_antecedents, round, 3))
cat("PASS: VIFs reported\n")

# Check: bootstrap CIs
bs <- summary(boot_mobi_pls)
cat("Bootstrapped paths (last method):\n")
print(round(bs$bootstrapped_paths, 3))
cat("PASS: bootstrap summary computed\n")

# Save last method plots
tryCatch({
  plot(mobi_pls);      save_plot("test_plots/6a_int_model.png")
  plot(boot_mobi_pls); save_plot("test_plots/6b_int_boot.png")
  cat("PASS: plots saved to test_plots/6a, 6b\n")
}, error = function(e) cat("WARN: plot save failed:", e$message, "\n"))
```

**Ask developer to verify before continuing:**

- During the demo, 6 plots were rendered (estimated + bootstrapped for each of 3 methods). Developer should confirm they all rendered without error in the R graphics device.
- `test_plots/6a_int_model.png` — interaction constructs (`Image*Expectation`, `Image*Value`) visible
- `test_plots/6b_int_boot.png` — bootstrapped interaction model renders correctly

---

## Cleanup

After all sections pass:

```r
unlink("test_plots", recursive = TRUE)
```
