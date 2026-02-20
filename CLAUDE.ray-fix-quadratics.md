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
| `R/feature_higher_order.R` | 52 | **same-class bug** - HOC first-stage SM filtering |
| `R/feature_higher_order.R` | 13, 22 | **same-class bug** - HOC substitution row removal |
| `R/specify_relationships.R` | - | `paths()` / `relationships()` |
| `R/estimate_pls.R` | - | `estimate_pls()` orchestration |

### Not affected

- **`orthogonal()` and `product_indicator()`** don't subset `structural_model`, so they don't have the `drop=FALSE` bug. However, they may have other issues with quadratic terms (duplicate item products) that should be tested.
- **`library.R:99,140`**, **`evaluate_model.R:10,70,72`**, **`feature_consistent.R:70`** - These extract a single column (e.g., `smMatrix[condition, "source"]`) which correctly returns a character vector. Not a bug.
- **Plot functions** (`R/plot_dot.R`) - Safe extraction patterns using `construct_names()` and `%in%`.
- **Report/summary functions** - Safe column extraction with `unique()`.

## Fix-to-Test Mapping

Every planned fix must have a failing test that demonstrates the bug before the fix is applied.

| Fix | File:Line | Failing Test | Trigger Scenario |
| --- | --------- | ------------ | ---------------- |
| **A** | `specify_interactions.R:259` | Two-stage quadratic with minimal SM | `interaction_term("Image","Image", method=two_stage)` with 1 non-interaction path → vector error |
| **B** | `feature_plspredict.R:115` | PLSpredict on quadratic model | `predict_pls()` on model with quadratic term and 1 non-interaction path → vector error in first-stage SM filtering |
| **C** | `feature_higher_order.R:52` | HOC + interaction with minimal SM | 1-dimension HOC + interaction where only 1 non-interaction path remains after HOC substitution and interaction removal |
| **D** | `feature_higher_order.R:13` | HOC with 1-dimension, single antecedent, no outcomes | After adding dimension paths and removing HOC target paths, 1 row remains |
| **E** | `feature_higher_order.R:22` | HOC with 1-dimension, single outcome, no antecedents | After adding dimension paths and removing HOC source paths, 1 row remains |

**Note on HOC tests (C, D, E):** These require degenerate 1-dimension HOC models (a HOC with only one first-order dimension). Such models are semantically unusual but syntactically valid. The existing HOC + interaction tests (`test-plot-hoc-2stage-interaction.R`) use rich models with 5+ paths and never trigger the single-row scenario.

## Implementation Steps

### Phase 1: Write failing tests

- [ ] **1a.** Create `tests/testthat/test-quadratic.R` with failing tests for fixes A and B:
  - **Test for fix A:** Two-stage quadratic estimation with `mobi` data and a minimal SM (only `Image → Satisfaction` plus `Image*Image → Satisfaction`). Should fail with "incorrect number of dimensions" before fix.
  - **Test for fix A (orthogonal):** Orthogonal quadratic - same structure. Should pass (orthogonal doesn't subset SM), serves as a baseline and regression guard.
  - **Test for fix A (product indicator):** Product indicator quadratic - same structure. Should pass (PI doesn't subset SM), serves as a baseline and regression guard.
  - **Test for fix A (bootstrap):** Bootstrap the two-stage quadratic model (`nboot = 50, cores = 1`). Should fail (each bootstrap iteration calls `two_stage()`) before fix.
  - **Test for fix B:** Call `predict_pls()` on a two-stage quadratic model. Should fail at `feature_plspredict.R:115` before fix.
  - All using `mobi` dataset

- [ ] **1b.** Create `tests/testthat/test-hoc-drop-false.R` with failing tests for fixes C, D, E:
  - **Test for fix C:** 1-dimension HOC + interaction with a minimal SM where only 1 non-interaction path remains after HOC substitution. Should fail with dimension error before fix.
  - **Test for fix D:** 1-dimension HOC with a single antecedent path and no outcome paths. After substitution removes the HOC target row, 1 row remains. Should fail before fix.
  - **Test for fix E:** 1-dimension HOC with a single outcome path and no antecedent paths. After substitution removes the HOC source row, 1 row remains. Should fail before fix.
  - Using `mobi` dataset with minimal construct subsets

- [ ] **1c.** Run all new tests to confirm they fail with the expected errors, documenting the failures

### Phase 2: Apply fixes

- [ ] **2a.** Fix A: `two_stage()` in `R/specify_interactions.R` line 259 - add `, drop=FALSE`

    ```r
    structural_model <- structural_model[ !grepl("\\*", structural_model[,"source"]), , drop=FALSE]
    ```

- [ ] **2b.** Fix B: `feature_plspredict.R` line 115 - add `, drop=FALSE`

    ```r
    first_stage_sm <- pls_model$structural_model[ !(pls_model$structural_model[,"source"] %in% interactions), , drop=FALSE]
    ```

- [ ] **2c.** Fix C: `feature_higher_order.R` line 52 - add `, drop=FALSE`

    ```r
    sm <- sm[sm[, "source"] %in% unique(new_mm[, "construct"]), , drop=FALSE]
    ```

- [ ] **2d.** Fixes D & E: `feature_higher_order.R` lines 13 and 22 - add `, drop=FALSE`

    ```r
    # Line 13 (fix D):
    sm <- sm[-which(sm[, "target"] == construct[1]), , drop=FALSE]
    # Line 22 (fix E):
    sm <- sm[-which(sm[, "source"] == construct[1]), , drop=FALSE]
    ```

- [ ] **2e.** Run all tests from Phase 1 to confirm they now pass

### Phase 3: Add `quadratic_term()` convenience function

- [ ] **3a.** Write test for `quadratic_term()` in `test-quadratic.R`:
  - Verify `quadratic_term("Image", method = two_stage)` produces the same model as `interaction_term("Image", "Image", method = two_stage)`

- [ ] **3b.** Create `quadratic_term()` in `R/specify_interactions.R`
  - Signature: `quadratic_term(construct, method = two_stage, weights = mode_A)`
  - Internally calls `interaction_term(construct, construct, method, weights)`
  - Add roxygen documentation with `@export`
  - The quadratic term name in the structural model will be `"X*X"` (consistent with existing naming)

- [ ] **3c.** Run `devtools::document()` to update NAMESPACE with the new export

- [ ] **3d.** Run `quadratic_term()` test to confirm it passes

### Phase 4: Verify

- [ ] **4a.** Run `devtools::test()` to verify all tests pass (new and existing)
- [ ] **4b.** Run `devtools::check()` for full CRAN-style validation

### Phase 5: Cleanup

- [ ] **5a.** Review test suite - remove any purely diagnostic tests, keep only regression tests
- [ ] **5b.** Update this plan document with final status

## Open Questions / Risks

1. **Quadratic naming in structural model:** The quadratic term `interaction_term("X", "X")` produces `"X*X"`. Users must reference this in `paths(from = "X*X")`. The new `quadratic_term()` should document this clearly.

2. **Product indicator with quadratic:** When `iv == moderator`, `product_indicator()` creates products of items with themselves (e.g., `x1*x1`, `x1*x2`, `x2*x1`, `x2*x2`). The `x1*x2` and `x2*x1` pairs would be duplicate. Need to verify this doesn't cause issues - but this is existing behavior, not something introduced by our fix.

3. **CBSEM quadratic:** The `two_stage` method is also used for CBSEM via `process_cbsem_interactions()`. The `drop=FALSE` fix in `two_stage()` applies to both PLS and CBSEM paths. Should test CBSEM quadratic if straightforward.

4. **1-dimension HOC realism:** Tests for fixes C, D, E use 1-dimension HOC models. While semantically unusual (why have a HOC with just one dimension?), they are syntactically valid and represent the minimal model that triggers the bug. The `drop=FALSE` fix is correct regardless of whether the edge case is common in practice.

5. **MGA with quadratic:** MGA uses `rerun()` which calls `estimate_pls()`, so it passes through the same `two_stage()` code path. The `drop=FALSE` fix in `two_stage()` covers this. No separate MGA test needed.
