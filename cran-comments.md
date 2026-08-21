# CRAN submission: seminr 2.6.0

## Release summary

Minor release. Three user-facing changes and a set of internal performance
refactors.

**Maintainer address.** CRAN reported that mail to the previous maintainer
address bounced. The address is now `seminrgroup@gmail.com`. The `Maintainer`
field continues to name an individual; only the address has changed.

**Bug fix.** `plot.reliability_table()` drew its reference line at 0.708, the
indicator *loading* threshold. The metrics that plot displays — Cronbach's
alpha, rhoA and rhoC — are construct-level reliabilities, judged against 0.70.
The line is now drawn at 0.70 and the value is exposed as a `threshold`
argument.

**Core usage.** The implicit default in `setup_parallel_cluster()` resolved
`cores = NULL` to `parallel::detectCores()`. Since tests and vignettes call
`bootstrap_model()` without pinning `cores`, checks could exceed the two-core
limit in the CRAN policy. The implicit default is now `min(2L, detectCores())`.
An explicit `cores` argument is still honoured.

**Performance.** Internal refactors to `simplePLS()`, `HTMT()`, `predict_pls()`,
cross-validation, PLS-MGA and the single-core bootstrap path. These change no
results — see below.

## Verification that results are unchanged

The refactors were checked against the code accompanying the PLS-SEM R book
(Hair et al.), comparing 2.5.0 with 2.6.0 on the same models with bootstrap
seeds pinned:

* deterministic quantities — path coefficients, outer loadings, outer weights,
  construct score correlations, R-squared, reliability, HTMT, AVE, VIF, and
  iteration counts — agree to `max|diff| = 0`
* bootstrap quantities — CIs, SDs and t-statistics — agree to `max|diff| = 0`
* verified for `cores = 1` and `cores = 2` independently, identical to twelve
  decimal places

## Test environments

- local: macOS 15.5 (arm64), R 4.6.0
- GitHub Actions: macOS-latest, ubuntu-latest and windows-latest, R release and
  R devel
- win-builder: R-devel (2026-08-17 r90424 ucrt), checked 2026-08-21

## R CMD check results

Local and GitHub Actions: 0 errors | 0 warnings | 0 notes
win-builder (R-devel): 0 errors | 0 warnings | 1 note

The note is the expected maintainer-address change described above:

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Nicholas Patrick Danks <seminrgroup@gmail.com>'

New maintainer:
  Nicholas Patrick Danks <seminrgroup@gmail.com>
Old maintainer(s):
  Nicholas Patrick Danks <nicholasdanks@hotmail.com>
```

The previous address bounced, which CRAN reported. The same change was made for
seminrExtras, confirmed by CRAN on 2026-08-19 and published the same day; this
submission brings seminr onto the same address. The `Maintainer` field names an
individual, not a mailing list, and the address is monitored.

win-builder installation time 7s, check time 251s.

## Reverse dependencies

One reverse dependency, seminrExtras (1.0.3, published 2026-08-19). Checked
against the development version of seminr with 0 new problems.
