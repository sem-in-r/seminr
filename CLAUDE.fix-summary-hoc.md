# Fix: summary() fails with Higher-Order Constructs (Issue #369)

**Status**: Fixed - cherry-picked commit 6ab7d16 from PR #374 (author: Lilikon/kojan)

## Problem Summary

When calling `summary()` on a PLS model containing higher-order constructs (HOCs), users encounter:

```
Error in `[.data.frame`(seminr_model$rawdata, , no_int_mmvars) :
   undefined columns selected
```

**Confirmed from issue #369**: Estimation works fine; error occurs specifically when calling `summary()` on the model.

**Root Cause**: For HOC models, `mmVariables` includes HOC dimension names (construct names like "Image", "Value") as "measurements" for the HOC. When `report_missing()` (called by `summary()` at line 30 of report_summary.R) tries to subset `rawdata` with these names, it fails because `rawdata` only contains actual measurement item columns, not construct names.

**Evidence**: There are exactly 2 places where `seminr_model$rawdata` is subset:

1. `R/report_descriptives.R:4` - uses `all_loc_non_int_items()`
2. `R/compute_metrics.R:131` - uses `no_int_mmvars`

The error message explicitly mentions `no_int_mmvars`, uniquely identifying the error location as `compute_metrics.R:131` in `report_missing()`.

## Files to Modify

1. **tests/testthat/test-hoc.R** - Add failing regression test (Step 1)
2. **R/compute_metrics.R** (line ~129-131) - Fix location (Step 2)

## Implementation Plan (Test-First Approach)

### Step 1: Add Failing Test in tests/testthat/test-hoc.R

First, add a test that reproduces the bug using the existing `mobi` dataset and `seminr_model` already defined in the test file. This test should fail before the fix:

```r
test_that("summary() works with HOC models", {
  expect_no_error(summary(seminr_model))
})
```

Add this test after line 55 (after the "estimates weights correctly" test) in the first HOC test context. This uses the existing model:

- Dataset: `mobi` (built-in seminr dataset)
- HOC: "Satisfaction" with dimensions "Image" and "Value"

### Step 2: Verify the Test Fails

Run the test to confirm it reproduces the error:

```r
testthat::test_file("tests/testthat/test-hoc.R")
```

Expected failure: `Error in [.data.frame(seminr_model$rawdata, , no_int_mmvars) : undefined columns selected`

### Step 3: Fix `report_missing()` in R/compute_metrics.R

Current problematic code (lines 129-131):

```r
no_int_mmvars <- seminr_model$mmVariables[!grepl("\\*", seminr_model$mmVariables)]
data_subset <- seminr_model$rawdata[, no_int_mmvars]
```

#### Alternative A (Claude's suggested fix)

Filter `no_int_mmvars` to only include columns that actually exist in `rawdata`:

```r
no_int_mmvars <- seminr_model$mmVariables[!grepl("\\*", seminr_model$mmVariables)]
# Filter to only items that exist in rawdata (excludes HOC dimension names)
no_int_mmvars <- intersect(no_int_mmvars, colnames(seminr_model$rawdata))
data_subset <- seminr_model$rawdata[, no_int_mmvars]
```

#### Alternative B (Chosen - from PR #374 by Lilikon)

Use the `first_stage_model`'s variables for HOC models, with additional filtering:

```r
# extract variables for analysis based on whether there is a higher-order model
if (is.null(seminr_model$first_stage_model)) {
  no_int_mmvars <- seminr_model$mmVariables[!grepl("\\*", seminr_model$mmVariables)]
} else {
  no_int_mmvars <- seminr_model$first_stage_model$mmVariables[!grepl("\\*", seminr_model$first_stage_model$mmVariables)]
}
# only subset raw data for available variables
any_no_int_mmvars <- no_int_mmvars[no_int_mmvars %in% names(seminr_model$rawdata)]
# subset raw data for missing analysis
data_subset <- seminr_model$rawdata[, any_no_int_mmvars]
```

**Decision**: Alternative B was chosen as it was contributed by the original author of PR #374 (Lilikon/kojan). This fix is more comprehensive as it explicitly handles HOC models by checking for `first_stage_model` and using its measurement variables. The commit was cherry-picked from PR #374 (commit 6ab7d16) to preserve author attribution.

### Step 4: Verify the Test Passes

Run the test again to confirm the fix works:

```r
testthat::test_file("tests/testthat/test-hoc.R")
```

## Verification

1. Run the HOC test file and confirm the new test fails:

   ```r
   testthat::test_file("tests/testthat/test-hoc.R")
   ```

   Expected: Test "summary() works with HOC models" fails with "undefined columns selected"

2. After applying fix, run the HOC test file again:

   ```r
   testthat::test_file("tests/testthat/test-hoc.R")
   ```

   Expected: All tests pass including "summary() works with HOC models"

3. Run the full test suite to ensure no regressions:

   ```r
   devtools::test()
   ```
