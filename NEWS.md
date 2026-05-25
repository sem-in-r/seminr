# seminr (development version)

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
