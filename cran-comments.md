## R CMD check results

0 errors | 0 warnings | 0 notes

(Local NOTE "unable to verify current time" is an environmental
clock-check artifact; not seen on win-builder or macOS builder.)

## Reverse dependencies

We checked 1 reverse dependency (seminrExtras), comparing R CMD check
results across the CRAN (2.4.2) and dev (2.5.0) versions of seminr.

* seminrExtras 1.0.0 has 2 test failures (`tests/testthat/test-cipma-comprehensive.R`)
  caused by calls to `seminr:::items_of_construct()`, a non-exported internal
  helper that was refactored in this release.

  seminr 2.5.0 introduces a new exported public S3 generic
  `construct_items(model, construct_name)` that replaces this internal helper.
  The seminrExtras production code is unaffected (it maintains its own local
  copy of `items_of_construct`); only three test lines reach into seminr's
  internals.

  The seminrExtras maintainer has been notified of the migration path and
  a corresponding update is in flight.

* We saw 0 other new problems.
* We failed to check 0 packages.
