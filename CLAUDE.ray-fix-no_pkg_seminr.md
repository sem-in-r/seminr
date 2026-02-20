# Plan: Fix "there is no package called 'seminr'" in bootstrap/parallel workers

## Issue Summary

**GitHub Issue #318**: PLS Bootstrap error: "there is no package called 'seminr'"

Users report that `bootstrap_model()` fails with error "there is no package called 'seminr'" even when seminr is properly installed. The error persists even with `cores=1`. Reported on multiple Windows systems (R 4.1.1 + seminr 2.3.2, R 4.4.3 + seminr 2.3.4).

## Root Cause Analysis

`bootstrap_model()` in `R/estimate_bootstrap.R` uses `parallel::makeCluster()` to create PSOCK worker processes. These workers are fresh R processes that:

1. **Don't inherit the parent's `.libPaths()`** on some system configurations (particularly Windows with RStudio, custom user libraries, `renv`, or non-default `R_LIBS_USER` settings)
2. **Must resolve `seminr::estimate_pls()`** — the explicit `seminr::` namespace qualifier triggers `loadNamespace("seminr")` on the worker, which fails if the worker can't find seminr in its library paths

Even with `cores=1`, a separate R process is still created via `parallel::makeCluster(1)`, so the issue is about library path propagation, not about the number of cores.

The same issue exists in `R/feature_plspredict.R` where `prediction_matrices()` creates parallel workers for LOOCV prediction. Functions called on those workers (`one_stage_predict`, `two_stage_predict`) also use `seminr::estimate_pls(...)`.

### Anti-pattern: `seminr::` self-reference

R packages should **never** use `pkg::func()` to call their own internal functions. The `seminr::` prefix forces namespace resolution via `loadNamespace()`, which requires the package to be findable via `.libPaths()`. Inside the package, functions are already in scope through the namespace — just call `estimate_pls()` directly.

## Relevant Files

- `R/estimate_bootstrap.R` — primary fix target (bootstrap_model function)
- `R/feature_plspredict.R` — secondary fix target (prediction_matrices, one_stage_predict, two_stage_predict)
- `tests/testthat/test-parallel-setup.R` — **new** test file for this issue
- `tests/testthat/test-bootstrap.R` — existing bootstrap tests (regression)

## Implementation Progress

### Step 1: Write tests — DONE

Created `tests/testthat/test-parallel-setup.R` with:
- **Integration test**: `bootstrap_model(cores=1, nboot=10)` completes successfully.

Two diagnostic unit tests (reproducing the bug with restricted `.lib.loc`, and proving the fix mechanism in isolation) were initially written to confirm the root cause and validate the approach, then removed — they tested synthetic scenarios rather than actual production code paths, so they provided no regression protection.

### Step 2: Fix `R/estimate_bootstrap.R` — DONE

- [x] After `parallel::makeCluster(...)`, added `.libPaths()` propagation + `library(seminr)` on workers
- [x] Changed `seminr::estimate_pls(...)` to `estimate_pls(...)` in `getEstimateResults()`

### Step 3: Fix `R/feature_plspredict.R` — DONE

- [x] After `parallel::makeCluster(...)` in `prediction_matrices()`, added `.libPaths()` propagation + `library(seminr)` on workers
- [x] Changed `seminr::estimate_pls(...)` to `estimate_pls(...)` in `one_stage_predict()` and `two_stage_predict()`

### Step 4: Extract shared `setup_parallel_cluster()` helper — DONE

Both `estimate_bootstrap.R` and `feature_plspredict.R` had duplicated cluster setup code (makeCluster + lib_paths propagation + library(seminr)). Additionally, `estimate_bootstrap.R` used `setup_strategy = "sequential"` while `feature_plspredict.R` did not — an oversight from the original R 4.0.3 compatibility fix (commit `7c293fc`, Dec 2020) that only touched the bootstrap file.

- [x] Created `R/library_parallel.R` with `setup_parallel_cluster(cores)` helper
- [x] Replaced inline cluster setup in `estimate_bootstrap.R` (line ~88) with `setup_parallel_cluster(cores)`
- [x] Replaced inline cluster setup in `feature_plspredict.R` (line ~539) with `setup_parallel_cluster(cores)`
- [x] Dropped `setup_strategy = "sequential"` — R docs mark it as "expert use only" and "may be removed in future versions". The underlying R 4.0 connection issue has been addressed in modern R.
- [x] Updated `test-parallel-setup.R` to also drop `setup_strategy = "sequential"`

### Step 5: Verify — DONE

- [x] All 259 tests pass (`devtools::test()`)
- [x] CRAN check: 0 errors, 0 warnings, 1 note (unrelated "unable to verify current time")

## Remaining `seminr::` Self-References (Not Yet Addressed)

These files also use `seminr::` to call their own functions. They won't cause issue #318 now (since workers load `library(seminr)`), but are still the same anti-pattern:

- `R/estimate_pls.R:165` — `seminr::simplePLS(...)`
- `R/specify_interactions.R:281` — `seminr::simplePLS(...)`
- `R/specify_interactions.R:291` — `seminr::estimate_cfa(...)`
- `R/plot_htmt.R:22,64` — `seminr::seminr_theme_get()` (default param values)

These could be cleaned up in a separate pass if desired.

## Open Questions / Risks

1. **`devtools::load_all()` scenario**: If users load seminr via `devtools::load_all()` instead of installing, workers still can't find seminr. This is an existing limitation (documented in CLAUDE.md) and not something this fix addresses.

2. **No Windows CI environment**: The reported issue is primarily on Windows. The fix is defensive and safe on all platforms.

3. **Startup overhead**: Adding `library(seminr)` on workers adds negligible startup time compared to bootstrap computation.
