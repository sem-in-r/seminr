## Resubmission

This is a resubmission. In this version I have:

* Reduced test timings by consolidating shared test setups and skipping
  computationally intensive tests on CRAN (LOOCV cross-validation,
  bootstrap numerical precision, and bootstrap plot tests).
  All tests continue to be vetted on local and CI environments.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

We checked 1 reverse dependency (seminrExtras), comparing R CMD check
results across CRAN and dev versions of this package.

* We saw 0 new problems
* We failed to check 0 packages
