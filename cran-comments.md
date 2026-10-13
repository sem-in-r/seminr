# CRAN submission: seminr 2.6.0

## Release summary

Minor release: prediction for consistent PLS (PLSc) and CB-SEM models, bug
fixes (several of which change results; listed below), the corrected
reliability-plot threshold, a new maintainer address, a two-core limit for
parallel work under CRAN's checks, and internal performance refactors. Every change is described
in NEWS.md.

**Maintainer address.** CRAN reported that mail to the previous maintainer
address bounced. The address is now `seminrgroup@gmail.com`. The `Maintainer`
field continues to name an individual; only the address has changed.

**Results that change.** These are corrections, each documented in NEWS.md:

* `predict()` and `predict_pls()` on PLSc models (`reflective()` constructs)
  applied corrected path coefficients to uncorrected composite scores. They now
  use the model-implied conditional expectation (de Rooij et al., 2023).
* PLSc corrected Mode A and unit-weighted composites as if they were common
  factors. Composites are now treated as fully reliable (Dijkstra & Henseler,
  2015), as cSEM does; higher-order composites of common factors are corrected
  with the reliability of their stage-2 proxy. Path coefficients and R-squared
  change for models that mix reflective constructs with such composites.
* `predict_pls(reps = k)` repeated the cross-validation on the same folds; it now
  draws new folds for each repetition.
* `bootstrap_model()` now excludes inadmissible PLSc resamples from the
  bootstrap statistics and reports how many were dropped; `estimate_pls()` warns
  when a PLSc solution is inadmissible.

Models without reflective constructs give the same estimates as in 2.5.0.

**New.** `predict()` works for CB-SEM models from `estimate_cbsem()`.

**Bug fix.** `plot.reliability_table()` drew its reference line at 0.708, the
indicator loading threshold; it now draws 0.70, the threshold for the
construct reliabilities it displays, and exposes a `threshold` argument.

**Core usage.** When `_R_CHECK_LIMIT_CORES_` is set, as in CRAN's checks, the
default for parallel work uses at most two cores, so tests and vignettes stay
within the two-core limit. Otherwise it uses all available cores, as before; an
explicit `cores` argument is always honoured.

**Performance.** Internal refactors of `simplePLS()`, `HTMT()`,
`predict_pls()`, cross-validation, PLS-MGA and the single-core bootstrap path.
These change no results: on the code accompanying the PLS-SEM R book (Hair et
al.), 2.5.0 and the refactored code agree to `max|diff| = 0` on all
deterministic and seeded bootstrap quantities, for `cores = 1` and `cores = 2`.

## Test environments

- local: macOS (arm64), R 4.5.1
- GitHub Actions: macOS-latest, windows-latest and ubuntu-latest (R release),
  ubuntu-latest (R devel)
- win-builder: R-release and R-devel

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE: maintainer address changed, as
  explained above.

## Reverse dependencies

We checked the three CRAN packages that depend on seminr against this version:

* seminrExtras 1.0.3: OK (seminrExtras 1.0.4, submitted alongside, also passes)
* surveyframe 0.4.2: OK
* semPlot 1.2.0: OK (checked with its suggested package MplusAutomation installed;
  without it, one Mplus example cannot run, which is unrelated to seminr)
