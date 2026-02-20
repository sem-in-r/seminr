# Plan: Fix quadratic interaction terms (Issue #327)

## Issue Summary

**Title:** `interaction_term()` errors when applying a quadratic with 1 IV only
**Issue:** [#327](https://github.com/sem-in-r/seminr/issues/327)
**Labels:** bug

When using `interaction_term("X", "X", method = two_stage)` to create a quadratic term (X*X) with a single-IV structural model, `estimate_pls()` fails with:

```text
Error in structural_model[, 1] : incorrect number of dimensions
```

The maintainer comment suggests creating a new `quadratic_term()` convenience function rather than modifying `interaction_term()`.

## Root Cause

In `R/specify_interactions.R` line 259, inside `two_stage()`:

```r
structural_model <- structural_model[ !grepl("\\*", structural_model[,"source"]), ]
```

When the structural model has only one non-interaction path (e.g., `X -> Y`), filtering out the interaction row leaves a single row. R's default behavior drops the matrix to a vector when subsetting yields one row, causing `structural_model[,1]` on line 260 to fail with "incorrect number of dimensions".

**Historical note:** This was fixed once before in commit `5ab7cc2` (Aug 2019) in old code (`R/interactions.R`), but the fix (`drop=FALSE`) was not carried forward when the code was refactored into `R/specify_interactions.R`.

## Relevant Files

| File | Lines | Role |
| ---- | ----- | ---- |
| `R/specify_interactions.R` | 254-278 | `two_stage()` - **primary bug** |
| `R/specify_interactions.R` | 50-55 | `interaction_term()` - entry point |
| `R/specify_interactions.R` | 298-346 | `process_interactions()` / `process_cbsem_interactions()` |
| `R/feature_plspredict.R` | 115 | **secondary bug** - same `drop=FALSE` issue |
| `R/feature_higher_order.R` | 13, 22, 52, 67 | **defensive fixes** - same `drop=FALSE` pattern (only reachable with degenerate 1-dimension HOC models) |
| `R/specify_relationships.R` | - | `paths()` / `relationships()` |
| `R/estimate_pls.R` | - | `estimate_pls()` orchestration |

### Not affected

- **`orthogonal()` and `product_indicator()`** don't subset `structural_model`, so they don't have the `drop=FALSE` bug. However, they may have other issues with quadratic terms (duplicate item products) that should be tested.
- **`library.R:99,140`**, **`evaluate_model.R:10,70,72`**, **`feature_consistent.R:70`** - These extract a single column (e.g., `smMatrix[condition, "source"]`) which correctly returns a character vector. Not a bug.
- **Plot functions** (`R/plot_dot.R`) - Safe extraction patterns using `construct_names()` and `%in%`.
- **Report/summary functions** - Safe column extraction with `unique()`.

## Fix-to-Test Mapping

| Fix | File:Line | Test | Status |
| --- | --------- | ---- | ------ |
| **A** | `specify_interactions.R:259` | Two-stage quadratic with minimal SM | Failed pre-fix, passes post-fix |
| **B** | `feature_plspredict.R:115` | PLSpredict on quadratic model | Failed pre-fix (blocked by A), passes post-fix |
| **C-F** | `feature_higher_order.R:13,22,52,67` | *(no tests — defensive only)* | Fixes kept; tests removed (only reachable with degenerate 1-dimension HOC models that no real user would create) |

## Implementation Steps

### Phase 1: Write failing tests

- [x] **1a.** Create `tests/testthat/test-quadratic.R` with tests for fixes A and B

- [x] **1b.** ~~Create `tests/testthat/test-hoc-drop-false.R`~~ — Created and verified failures, then **removed** during cleanup (see Phase 5)

- [x] **1c.** Run all new tests to confirm they fail with the expected errors

#### Phase 1 Test Results (pre-fix)

**test-quadratic.R** — 3 FAIL, 2 PASS:

| Test | Result | Error |
| ---- | ------ | ----- |
| two_stage quadratic (X*X) estimates without error | **FAIL** | `incorrect number of dimensions` (fix A) |
| orthogonal quadratic (X*X) estimates without error | PASS | (orthogonal doesn't subset SM — baseline guard) |
| product_indicator quadratic (X*X) estimates without error | PASS | (PI doesn't subset SM — baseline guard) |
| bootstrap of two_stage quadratic (X*X) works without error | **FAIL** | `incorrect number of dimensions` (fix A, via bootstrap) |
| predict_pls works on two_stage quadratic model | **FAIL** | `incorrect number of dimensions` (fix A blocks fix B from being reached) |

### Phase 2: Apply fixes

- [x] **2a.** Fix A: `two_stage()` in `R/specify_interactions.R` line 259 — add `, drop=FALSE`
- [x] **2b.** Fix B: `feature_plspredict.R` line 115 — add `, drop=FALSE`
- [x] **2c.** Fix C: `feature_higher_order.R` line 52 — add `, drop=FALSE` (defensive)
- [x] **2d.** Fixes D & E: `feature_higher_order.R` lines 13 and 22 — add `, drop=FALSE` (defensive)
- [x] **2f.** Fix F: `feature_higher_order.R` line 67 — add `, drop=FALSE` (defensive, discovered during testing)
- [x] **2e.** All tests pass after fixes

### Phase 3: Add `quadratic_term()` convenience function

- [x] **3a.** Write test for `quadratic_term()` in `test-quadratic.R`
- [x] **3b.** Create `quadratic_term()` in `R/specify_interactions.R`
- [x] **3c.** Run `devtools::document()` to update NAMESPACE
- [x] **3d.** All `quadratic_term()` tests pass

### Phase 4: Verify

- [x] **4a.** `devtools::test()` — all 272 tests pass (0 failures)
- [ ] **4b.** Run `devtools::check()` for full CRAN-style validation

### Phase 5: Cleanup

- [x] **5a.** Removed `test-hoc-drop-false.R` — tests exercised degenerate 1-dimension HOC models that no real user would create. The `drop=FALSE` fixes in HOC code are kept as defensive hardening but don't need dedicated tests.
- [x] **5b.** Plan document updated with final status

## Final Test Suite (`test-quadratic.R`)

| # | Test | Purpose |
| - | ---- | ------- |
| 1 | two_stage quadratic estimates without error | Core regression test for fix A (#327) |
| 2 | orthogonal quadratic estimates without error | Baseline: confirms quadratics work across methods |
| 3 | product_indicator quadratic estimates without error | Baseline: confirms quadratics work across methods |
| 4 | bootstrap of two_stage quadratic works | Regression test for fix A via bootstrap entry point |
| 5 | predict_pls works on two_stage quadratic model | Regression test for fix B |
| 6-7 | quadratic_term() produces same model as interaction_term(X, X) | New convenience function validation |

## Open Questions / Risks

1. **Quadratic naming in structural model:** The quadratic term `interaction_term("X", "X")` produces `"X*X"`. Users must reference this in `paths(from = "X*X")`. The new `quadratic_term()` should document this clearly.

2. **Product indicator with quadratic:** When `iv == moderator`, `product_indicator()` creates products of items with themselves (e.g., `x1*x1`, `x1*x2`, `x2*x1`, `x2*x2`). The `x1*x2` and `x2*x1` pairs would be duplicate. Need to verify this doesn't cause issues — but this is existing behavior, not something introduced by our fix.

3. **CBSEM quadratic:** The `two_stage` method is also used for CBSEM via `process_cbsem_interactions()`. The `drop=FALSE` fix in `two_stage()` applies to both PLS and CBSEM paths. Should test CBSEM quadratic if straightforward.

4. **MGA with quadratic:** MGA uses `rerun()` which calls `estimate_pls()`, so it passes through the same `two_stage()` code path. The `drop=FALSE` fix in `two_stage()` covers this. No separate MGA test needed.
