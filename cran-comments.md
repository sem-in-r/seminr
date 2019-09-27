## Resubmission
This is a resubmission. In this version I have:

## [0.1.0] - 2019-09-27
### Added
- A changelog
- A new feature for automated calculation of HOC
- A new feature for two-stage calculation of interactions
- A file for all references and citations
- A return object in summary(boot_seminr_model) containing boot mean, SD, tvalue, and CIs for bootstrapped paths, loadings, weights and HTMT, 
- A test for the bootstrap summary return object
- Descriptive statistics for item and construct data
- S3 print method for class "table_output" for printing generic tables
- new method interaction_term() for specifying a interaction construct
- A fSquare function to calculating fSquared
- A test for fSquared function

### Changed
- Fixtures for evaluating bootstrap HTMT for versions of R < 3.6.0
- Changed the R/* file naming to R/estimate_ R/feature_ R/evaluate_ etc.
- Summary S3 method to return data descriptives in summary object
- constructs() method now returns a list with classes
- Changed references to include Cohen (2013)
- Updated vignette to reflect fSquare function

### Fixed
- Modified calculation of HTMT to use absolute correlation matrices in order to make HTMT stable


## Test environments
* macOS High Sierra 10.13.3 (on travis-ci), R 3.5.3
* macOS High Sierra 10.13.3 (on travis-ci), R 3.6.1
* Ubuntu 16.04.6 LTS (on travis-ci), R 3.5.3
* Ubuntu 16.04.6 LTS (on travis-ci), R 3.6.1
* win-builder (3.6.1 and 3.5.3)

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs. 
