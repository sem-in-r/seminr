# seminr 2.6.0

### New features

* `predict()` now works for CB-SEM models from `estimate_cbsem()`, using the
  model-implied conditional expectation of de Rooij et al. (2023), the rule of
  `lavaan::lavPredictY()`. `predict_DA` and `predict_EA` choose the predictor
  items as for PLS and PLSc, so the three estimators' predictions are
  comparable. Models with interaction terms, higher-order constructs, ordinal
  indicators or multiple groups, and non-converged or inadmissible fits, stop
  with the reason (#426).

### Fixed

* `predict()` and `predict_pls()` pushed composite scores, built from the
  uncorrected outer weights, through the PLSc-corrected path coefficients and
  loadings whenever a model had `reflective()` constructs. PLSc coefficients
  describe the common factors, not the composites, so the predictions were
  over-dispersed (by 1/sqrt(rho_A) with a single antecedent) or reweighted
  across correlated antecedents. PLSc models are now predicted with the
  model-implied conditional expectation of de Rooij et al. (2023), using the
  indicator correlations implied by the PLSc estimates; `predict_DA` and
  `predict_EA` set the predictor items. Inadmissible PLSc solutions stop with
  the reason (the checks are listed in `?predict.seminr_model`), and PLSc
  models with interaction terms are no longer predicted (#427). Models without
  reflective constructs are unchanged (#425).

* In `predict_pls()`, a PLSc solution can be inadmissible in a training fold
  even when the full-sample solution is admissible, especially with few folds
  (for the mobi ECSI model, in about 70% of 5-fold and 30% of 10-fold runs).
  Such folds are skipped with one warning that names them: their test rows are
  `NA` for both PLS and the LM benchmark, and the prediction metrics use the
  remaining rows. If every fold is inadmissible, `predict_pls()` stops. The PLS
  chain is never used as a fallback. Cross-validation also no longer turns any
  error or warning inside a fold into a failure with an unrelated message
  ("row names supplied are of the wrong length"); they now reach the caller.

* Models estimated by seminr < 2.6.0 and saved (e.g. with `saveRDS()`) keep
  their old PLSc estimates. `predict()` on such a model that mixes `reflective()`
  constructs with Mode A or unit-weighted composites now warns to re-estimate
  it. `estimate_pls()` stamps models with `seminr_version` for this purpose.

* PLSc (`estimate_pls()` with `reflective()` constructs) corrected the paths
  into and out of Mode A and unit-weighted composites as if they were common
  factors, by dividing their correlations by sqrt(rho_A). Only Mode B composites
  were left uncorrected. Consistent PLS corrects only common factors and treats
  composites as fully reliable (Dijkstra & Henseler, 2015), as cSEM does. All
  composites now use rho_A = 1 in PLSc. **Path coefficients and R² change for
  models that mix `reflective()` constructs with Mode A or unit-weighted
  `composite()` constructs.** Loadings, all-reflective models, models with only
  Mode B composites next to reflective constructs, and models without
  reflective constructs are unchanged. The `rho_A` reported by `summary()` is
  unchanged. Calling `PLSc()` directly on a model without reflective constructs
  now leaves it unchanged (it used to disattenuate its composites) (#430).

* PLSc corrected a two-stage `higher_composite()` with rho_A computed on its
  lower-order construct (LOC) scores, as if the HOC were a common factor of
  them. A HOC of composite LOCs is now treated as fully reliable (rho = 1). A
  HOC of common-factor LOCs is corrected with the reliability of its stage-2
  proxy, a weighted sum of error-laden LOC scores: w'S*w, with the LOCs'
  stage-1 reliabilities on the diagonal of S* (van Riel et al., 2017), as
  cSEM's two-stage approach does. **Path coefficients and R² change for PLSc
  models with `higher_composite()` constructs** (on mobi, IE -> Satisfaction
  0.953 -> 0.920 with reflective LOCs; cSEM 0.918). A HOC of common factors is
  now corrected even when no stage-2 construct is reflective, and a
  `reflective()` construct that is declared but not used in the structural model
  no longer makes `estimate_pls()` run PLSc.

* `predict_pls()` on a model from `estimate_cbsem()` failed with an unrelated
  low-level error (the CB-SEM object inherits `seminr_model` but has no weights
  or loadings). It now stops with a message pointing to `predict()`.
  `predict()` and `predict_pls()` on a model from `estimate_cfa()` failed the
  same way; they now stop and say that a CFA has no structural model to predict
  from (#426).

* `plot.reliability_table()` drew its reference line at 0.708, which is the
  indicator **loading** threshold (0.708² ≈ 0.50 explained variance). The metrics
  that plot shows — Cronbach's alpha, rhoA and rhoC — are construct-level
  internal consistency reliabilities, judged against **0.70**. The line is now
  drawn at 0.70, and the value is exposed as a `threshold` argument so it can be
  overridden and so the intended value is documented rather than buried
  (#421). Reported by Marko Sarstedt.

* `predict_pls(reps = k)` repeated the cross-validation on the same folds, so
  every repetition gave the same predictions and `reps` had no effect. Each
  repetition now draws new random folds, and the predictions are averaged over
  the repetitions. **Results change for any call with `reps` greater than 1**,
  including `assess_cvpat()`, `assess_cvpat_compare()`, `assess_pcm()` and
  `assess_coa()` in seminrExtras, which pass `reps` on to `predict_pls()`.
  `reps = NULL` and `reps = 1` give the same results as before. A fold skipped
  as inadmissible (PLSc) in one repetition is averaged over the others.

* `predict()` on test data with a missing value returned `NA` for every
  prediction in that case's row, because the matrix products spread the `NA`
  through zero weights, loadings and paths (`NA * 0` is `NA`). Only the
  predictions that use the missing value are `NA` now: a missing item makes its
  own construct's score `NA` and the predictions that depend on it, and leaves
  the others. Predictions for complete data are unchanged. PLSc models already
  behaved this way.

### Changed

* The package maintainer address is now `seminrgroup@gmail.com` (#420).

### Performance

Substantial internal speedups across the heavy routines. **These are refactors
only — no result changes.** Verified against the PLS-SEM R book code: with seeds
pinned, 2.5.0 and 2.6.0 agree to `max|diff| = 0` on every deterministic quantity
(path coefficients, loadings, weights, R², reliability, HTMT, AVE, VIF) and every
bootstrap quantity, for both `cores = 1` and `cores = 2`.

* `simplePLS()` skips redundant matrix work in the estimation loop.
* `HTMT()` reuses one correlation block per construct instead of recomputing.
* `predict_pls()` avoids redundant model refits and per-item `lm` fits.
* Cross-validation reuses the full-model reference scores.
* PLS-MGA p-values are computed without `expand.grid`.
* `bootstrap_model(cores = 1)` now runs replicates in-process rather than
  shipping them to a one-worker PSOCK cluster, and saves and restores the
  caller's RNG state. Replicates continue to use `set.seed(seed + i)`, so
  bootstrap results remain identical across core counts and across versions.
* Added a benchmark harness covering the heavy PLS routines.

# seminr 2.5.0

### Added
* **Prediction for all interaction methods**: `predict()` and `predict_pls()` now support
  `product_indicator` and `orthogonal` interaction models, in addition to `two_stage`.
  Previously, only `two_stage` interactions could generate out-of-sample predictions;
  the other methods threw an error. All three methods now fully support single predictions
  (`predict()`), k-fold cross-validation, and LOOCV via `predict_pls()`.
* **Quadratic term prediction**: `quadratic_term()` models (using any interaction method)
  can now generate predictions.
* **Parallel k-fold cross-validation**: `predict_pls()` now supports parallel execution
  for k-fold CV when `cores` is specified (e.g., `predict_pls(model, noFolds = 50, cores = 4)`).
  Previously, parallelization was only available for LOOCV.
* **Interaction method detection**: New internal `detect_interaction_method()` function
  provides clean dispatch based on interaction class attributes.
* **Custom confidence levels in plots**: `plot()` accepts a user-specified confidence
  level for bootstrapped models, allowing displays at any alpha (e.g., 90%, 99%) instead
  of the fixed 95% default (#407).
* **Public accessor API for constructs and measurement-model elements**: A set of
  helper functions is now exported and documented for use by downstream packages and
  user scripts. Container-first argument order (model or measurement-model first):
  `construct_items(x, construct_name)` (S3 generic),
  `construct_names(x)` (S3 generic),
  `construct_name(construct)`,
  `construct_mode(mmMatrix, construct)`,
  `construct_type(model, construct)`,
  `all_factors(model)`, `all_composites(model)`,
  `all_non_interactions(measurement_model)`. These replace and consolidate a set of
  non-exported internal helpers; downstream code should migrate off `seminr:::`
  triple-colon access and use these exported functions instead.

### Changed
* `predict.seminr_model()` dispatch refactored: uses `switch()` on detected interaction
  method instead of pattern-matching on measurement model names.
* Interaction estimation now stores prediction-relevant parameters on the model object
  (`model$interaction_params`), including orthogonalization regression coefficients
  needed for out-of-sample prediction of orthogonal models.
* Mixed interaction methods (e.g., one `two_stage` and one `product_indicator` in the
  same model) produce an informative error at prediction time.

### Fixed
* Plot significance stars now use bootstrap p-values for consistency with reported
  significance (#412).
* `construct_items()` and `all_LOC_items()` return a character vector instead of a
  single-column matrix, restoring expected downstream behavior (#364).
* Construct/item name collision check now correctly detects name conflicts that were
  previously missed (#402).
* CBSEM summary path significance now displays in the conventional IV → DV direction
  (#404).
* Quadratic interaction terms with a single indicator no longer fail due to
  matrix-to-vector coercion (#327).

# seminr 2.4.2

### Fixed
* PLSpredict now works correctly with non-standard (character) rownames (#390)
* Plot symbols use BMP-compatible Greek letters for cross-platform rendering (#226)
* Plot displays capital R² for coefficient of determination (#389)
* Summary reports now work correctly for PLS-SEM models with higher-order constructs (#369)
* `vif_items()` always returns a named list structure (#377)

### Changed
* Modernized GitHub Actions CI workflow for Ubuntu 24.04

# seminr 2.4.0
* (See previous CRAN release notes)
