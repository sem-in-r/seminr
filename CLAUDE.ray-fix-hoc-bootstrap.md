# Plan: Fix HOC Bootstrap (#299 and #205)

**Branch**: `ray/fix-hoc-bootstrap`
**Issues**: [#299](https://github.com/sem-in-r/seminr/issues/299) — bootstrap_model() errors when PLSc and HOC in model, [#205](https://github.com/sem-in-r/seminr/issues/205) — PLSc bootstrap errors with HOC

## Issue Summary

Both issues report that `bootstrap_model()` fails when the model contains both Higher-Order Constructs (HOC) and reflective constructs (triggering PLSc). The root cause is a miscalculated `length` variable in the bootstrap error-recovery code.

## Root Cause Analysis

### The `length` formula bug (`estimate_bootstrap.R:111`)

```r
length <- 3*nrow(seminr_model$path_coef)^2 + 2*nrow(seminr_model$outer_loadings)*ncol(seminr_model$outer_loadings)
```

This formula computes the expected size of the flattened bootstrap result vector. Each bootstrap iteration returns (lines 125-129):

```r
c(path_coef, outer_loadings, outer_weights, htmt, total_effects)
```

**Actual sizes:**
| Component       | Size                                    |
|-----------------|-----------------------------------------|
| path_coef       | `nrow(path_coef) * ncol(path_coef)`     |
| outer_loadings  | `nrow(loadings) * ncol(loadings)`       |
| outer_weights   | `nrow(loadings) * ncol(loadings)`       |
| HTMT            | `nrow(HTMT) * ncol(HTMT)`              |
| total_effects   | `nrow(path_coef) * ncol(path_coef)`     |

**Actual total** = `2 * nrow(path)^2 + 2 * nrow(load) * ncol(load) + nrow(HTMT) * ncol(HTMT)`

**Formula assumes** = `3 * nrow(path)^2 + 2 * nrow(load) * ncol(load)`

The formula assumes HTMT has the same dimensions as `path_coef`. This is wrong because:

1. **HTMT excludes interaction constructs**: Interaction terms (e.g., `Image*Expectation`) appear in path_coef but not in HTMT (they're not in `mmMatrix`).
2. **HOC models add extra constructs to HTMT**: The HTMT function's HOC branch (`evaluate_validity.R:51`) includes constructs from `first_stage_model$smMatrix`, making HTMT potentially larger or differently sized than `path_coef`.

### Why it manifests as a crash

The `length` variable is only used in the error handler (line 134) to create an NA vector when a bootstrap iteration fails:

```r
error = function(cond) { return(rep(NA, length)) }
```

When PLSc `solve()` fails on a bootstrap sample (common with HOC due to higher dimensionality), the NA vector is `length`-sized while successful iterations return the actual-sized vector. `parSapply` cannot combine columns of different lengths, causing the entire bootstrap to crash.

### Concrete example with the existing HOC test model

- path_coef: 5x5 = 25 elements
- outer_loadings: 23x7 = 161 elements
- outer_weights: 23x7 = 161 elements
- HTMT: 7x7 = 49 elements (includes Image, Value from first-stage)
- total_effects: 5x5 = 25 elements
- **Actual total**: 421
- **Formula `length`**: 397
- **Mismatch**: 24 elements

### Why PLSc fails in bootstrap iterations (Issue #205)

PLSc corrects path coefficients using `solve()` (`feature_consistent.R:73`) which requires the correlation matrix to be non-singular. Bootstrap samples (drawn with replacement) can produce degenerate correlation matrices, especially with HOC models where dimensionality is higher. This is expected behavior — the existing error handling (tryCatch + NA vector + exclusion) is the correct approach, but it only works when `length` is correct.

## Relevant Files

| File | Role |
|------|------|
| `R/estimate_bootstrap.R:111` | **Bug location**: `length` formula |
| `R/estimate_bootstrap.R:114-142` | Bootstrap iteration function with error handler |
| `R/estimate_bootstrap.R:254` | HTMT dimensions used correctly in reconstruction |
| `R/evaluate_validity.R:47-80` | HTMT function with HOC-aware construct selection |
| `R/estimate_pls.R:140-195` | HOC two-stage estimation in estimate_pls |
| `R/feature_higher_order.R:33-79` | `prepare_higher_order_model()` |
| `R/feature_higher_order.R:105-153` | `combine_first_order_second_order_matrices()` |
| `R/feature_consistent.R:48-105` | PLSc implementation (`solve()` failure point) |
| `R/feature_consistent.R:108-119` | `model_consistent()` triggers PLSc for mode "C" |
| `tests/testthat/test-hoc.R` | Existing HOC tests (no bootstrap tests) |
| `tests/testthat/test-bootstrap.R` | Existing bootstrap tests (no HOC tests) |

## Test Plan

### Automated tests (new test file: `tests/testthat/test-hoc-bootstrap.R`)

1. **HOC composite + reflective + bootstrap (PLSc)**: Bootstrap a model with `higher_composite()` and at least one `reflective()` construct. This triggers PLSc, causing some bootstrap iterations to fail, which exercises the `boot_vec_len`-dependent error recovery. This is the core reproduction of issues #299 and #205.

2. **Verify summary() works on bootstrapped HOC model**: Ensure downstream reporting functions handle the expanded dimensions correctly.

## Implementation Steps

- [x] **Step 1: Write failing tests**
  Created `tests/testthat/test-hoc-bootstrap.R` with the PLSc reproduction test and summary integration check. Dropped HOC composite-only and all-reflective tests (redundant coverage).

- [x] **Step 2: Fix the `length` calculation and rename the variable**
  Replaced incorrect formula in `R/estimate_bootstrap.R` with computation based on actual matrix dimensions. Renamed variable from `length` to `boot_vec_len` to avoid shadowing R's built-in `length()`.

- [x] **Step 3: Verify HTMT and total_effects are exported/available in parallel workers**
  Confirmed: `boot_vec_len` is computed on the main process before the cluster starts, so no export issue.

- [x] **Step 4: Run tests and verify**
  All 47 tests pass across test-hoc-bootstrap, test-bootstrap, and test-hoc.

- [x] **Step 5: Run full test suite**
  `devtools::test()`: 259 passed, 0 failed, 2 skipped (pre-existing empty plot test files).

## Open Questions / Risks

1. **Non-HOC models with interactions**: The `length` formula is also wrong for interaction models (HTMT excludes interaction constructs), but this only matters if bootstrap iterations fail. For all-composite models with interactions, PLSc isn't triggered and iterations rarely fail, so the bug is latent. The fix will correct this case too.

2. **Performance of computing HTMT before bootstrap loop**: Computing `HTMT(seminr_model)` is cheap (correlation-based, no estimation), so the overhead is negligible.

3. **Bootstrap iteration failure rate**: With PLSc + HOC, many bootstrap iterations may fail. The existing warning message (line 152) reports the count. If failure rates are very high (e.g., >50%), the bootstrap results may not be meaningful. This is a pre-existing concern and out of scope for this fix, but worth noting.

4. **`higher_reflective()` + bootstrap**: The `higher_reflective()` function creates a reflective HOC (mode "C" directly on the HOC construct). Need to verify this scenario works correctly after the fix — the HTMT dimensions may differ again from the composite HOC case.
