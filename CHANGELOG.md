# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
- Change code syntax to remove interactions() function and add interactions and HOC to composites()
- Document all the syntax and features

## [0.7.0] - 2019-09-19
### Added
- A changelog
- A new feature for automated calculation of HOC
- A new feature for two-stage calculation of interactions
- A file for all references and citations
- A return object in summary(boot_seminr_model) containing boot mean, SD, tvalue, and CIs for bootstrapped paths, loadings, weights and HTMT, 
- A test for the bootstrap summary return object

### Changed
- Fixtures for evaluating bootstrap HTMT for versions of R < 3.6.0
- Changed the R/* file naming to R/estimate_ R/feature_ R/evaluate_ etc.

### Fixed
- Modified calculation of HTMT to use absolute correlation matrices in order to make HTMT stable
