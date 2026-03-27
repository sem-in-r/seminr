# seminr (development version)

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

### Changed
* `predict.seminr_model()` dispatch refactored: uses `switch()` on detected interaction
  method instead of pattern-matching on measurement model names.
* Interaction estimation now stores prediction-relevant parameters on the model object
  (`model$interaction_params`), including orthogonalization regression coefficients
  needed for out-of-sample prediction of orthogonal models.
* Mixed interaction methods (e.g., one `two_stage` and one `product_indicator` in the
  same model) produce an informative error at prediction time.

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
