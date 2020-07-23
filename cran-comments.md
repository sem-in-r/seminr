## Resubmission
This is a resubmission. In this version I have:

## [1.1.0] - 2020-07-09
### [1.1.0] Added

- A new feature to process reflective CFA/CBSEM models through Lavaan
  - Measurement/structural models converted into Lavaan syntax
  - New item error covariances specification created for CFA/CBSEM measurement
  - By default, MLR (robust ML) estimation used
  - User can pass parameters to Lavaan's estimate_* functions (e.g., fiml)
  - `summary()` of CFA/CBSEM estimation uses information from Lavaan summaries
- A new feature to extract ten Berge scores for CFA/CBSEM models
- Tests added for new features
- Demos added to show new features

### [1.1.0] Changed

- Updated several features to work for both PLS and CFA/CBSEM
  - Interactions work for PLS and CBSEM
  - Two_stage adapts to CBSEM, using CFA to get ten Berge scores in first stage
  - R^2 and VIFs computed using correlation matrices instead of `lm()`
- Updated summary objects to hold meta information of estimation method
- Vignette updated to show new features
- Fixed a broken URI: Found the following (possibly) invalid file URI:
   URI: comparing-cbsem-and-pls-pm-example
     From: README.md

## [1.0.1] - 2019-12-11
### Changed
- Patched if() conditionals including class() to reflect new CRAN class of matrix as c("matrix","array") in R V4.0.0

## [1.0.2] - 2020-05-08
### Changed
- Attended to a bug in the code.