# Fix VIF Items Output Inconsistency

## Issue Reference
GitHub Issue: https://github.com/sem-in-r/seminr/issues/377

## Problem Summary

The `summary(model)$validity$vif_items` output has inconsistent structure depending on whether composite constructs have equal or unequal numbers of indicators.

### What Users See

**Case 1: Unequal indicator counts (EXPECTED behavior)**
```
LV1 :
csor_1 csor_2

LV2 :
csor_4 csor_5

LV3 :
attr_1 attr_2 attr_3    # <- 3 indicators vs 2 above
```
VIFs are grouped by construct with indicator names.

**Case 2: Equal indicator counts (PROBLEMATIC behavior)**
```
Component 1 : ...
Component 2 : ...
...
```
VIFs lose construct and indicator names entirely.

## Root Cause

**File:** `R/evaluate_validity.R` (lines 21-23)

```r
item_vifs <- sapply(model_constructs$construct_names, independent_vifs,
                    items_of_construct, seminr_model,
                    data = seminr_model$data)
```

The `sapply()` function **without `simplify = FALSE`** automatically simplifies results:
- When constructs have **different** indicator counts: returns a **list** (cannot simplify)
- When constructs have **equal** indicator counts: returns a **matrix** (simplifies, losing names)

### Why This Happens

R's `sapply()` attempts to simplify results into the simplest possible structure:
- A list of vectors with equal lengths becomes a matrix
- Matrix columns are named "Component 1", "Component 2", etc. by R's default behavior

### Contrast with Correct Implementation

The `antecedent_vifs()` function in the same file (line 31-38) correctly uses `simplify=FALSE`:

```r
ret <- sapply(endogenous_names, function(outcome) {
    # ... calculation logic ...
  }, simplify=FALSE, USE.NAMES=TRUE)  # <-- Correct approach
```

## Fix

Add `simplify = FALSE` to the `sapply()` call in `item_vifs()`:

```r
item_vifs <- sapply(model_constructs$construct_names, independent_vifs,
                    items_of_construct, seminr_model,
                    data = seminr_model$data,
                    simplify = FALSE)  # <-- Add this
```

This ensures the output is always a named list, regardless of indicator distribution.

## Testing Approach

1. Run existing tests to ensure no regressions
2. Create a specific test case with equal indicator counts across constructs
3. Verify the output structure is a named list with indicator names preserved

## Files to Modify

- `R/evaluate_validity.R` - Add `simplify = FALSE` to `sapply()` call
- `tests/testthat/test-validity.R` - Add test case for equal indicator counts
