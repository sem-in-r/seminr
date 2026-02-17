# Fix predict_pls Subscript Indexing for Non-Standard Rownames

> **IMPORTANT**: This plan must be kept up-to-date at all times. Assume context can be cleared at any time — this file is the single source of truth for the current state of this work. Update this plan before and after task and subtask implementations.

## Branch

`ray/fix-plspredict-indexing`

## Goal

Add regression tests for `predict_pls()` with non-standard rownames (GitHub issue #347). The bug was already fixed by PR #368 — this branch adds test coverage to prevent regressions.

## Strategy

PR #368 already fixed the root cause. This branch adds regression tests only. No code changes to `feature_plspredict.R` are needed.

## Current State

- [x] Plan created
- [x] Investigation complete — confirmed fix already in PR #368
- [x] Regression tests written and passing (all 9 tests pass)
- [ ] Decision: ship test-only branch, or close and note on issue #347

## Key Findings

### Root Cause (Issue #347)

The user reported `subscript out of bounds` when calling `predict_pls()` on a model estimated from a data frame with non-sequential rownames (e.g., after filtering rows from a larger dataset). The old code used `as.character(c(1:nrow(model$data)))` to index into prediction matrices — this assumed rownames were always `"1", "2", ...`.

### Already Fixed by PR #368

PR #368 changed all instances of `as.character(c(1:nrow(model$data)))` to `rownames(model$data)` in `feature_plspredict.R`. This works because `estimate_pls()` keeps `model$data` and `model$construct_scores` rownames in sync — whatever rownames the input data has, they propagate consistently through:

1. `in_and_out_sample_predictions()` creates result matrices with `dimnames = list(rownames(ordered_data), ...)` — preserving shuffled rownames
2. `do.call(cbind, ...)` on fold matrices preserves rownames
3. `rowSums()` in `mean_rows`/`sum_rows` preserves rownames from the input
4. `sapply()` wrapping preserves the names from the first result
5. The reindexing `pred_matrices[rownames(model$data),]` correctly maps back to original order

Verified working with: non-sequential numeric rownames, character rownames, and `reps` mode.

### Why No Code Fix Is Needed

The fix from PR #368 is correct and sufficient. We confirmed this by:

- Manually reproducing the original error scenario (non-sequential rownames) — no error
- Testing character rownames — no error
- Testing `reps` mode with non-sequential rownames — no error
- Comparing prediction values: models estimated from identical data with different rownames (standard, non-sequential numeric, character) produce numerically identical predictions when given the same random seed

An earlier test attempt that swapped `model$data` rownames without re-estimating did trigger the original `subscript out of bounds` error in `model$construct_scores[rownames(model$data), ]`. However, this is an artificial scenario — `estimate_pls()` always keeps these in sync, and users don't manually reassign `model$data`.

### Test Design

The regression test estimates three models from identical data with different rowname styles (standard `1:N`, non-sequential numeric, character strings), runs `predict_pls` with the same seed on each, and asserts the prediction values are numerically identical. This catches both crash errors and silent row-misalignment bugs. The `reps` path is also covered. The test adds ~2 seconds (~2.5% of the test file's 91-second runtime).

### Additional Fragile Patterns Noted (Out of Scope)

- **Inconsistent mmMatrix indexing**: Line 288 uses column names (`"construct"`, `"measurement"`); line 656 uses numeric indices (`[,1]`, `[,2]`)
- **Complex odd/even positional indexing** (lines 501-503, 554-561): relies on exact ordering from `sapply`/`parSapply` results
- These are pre-existing patterns — not addressed in this branch

## Questions

- ~~Is the fix already in place from PR #368?~~ Yes — confirmed by manual reproduction and automated tests. No code change needed.

## Scope

**In scope**:

- Regression test for non-standard rownames in `predict_pls()`

**Out of scope**:

- Defensive rowname sanitization (not needed — fix is already correct)
- Refactoring fragile indexing patterns
- HOC prediction support (issue #222)

## Tasks

- [x] 1 Regression test: `predict_pls()` with non-standard rownames produces identical results to standard rownames (covers non-sequential numeric, character, and `reps` paths)
- [ ] 2 Decision: ship test-only PR, or close branch and note fix on issue #347

## Completed

- Task 1: Single consolidated regression test added to `tests/testthat/test-plspredict.R`

---

Last updated: 2026-02-17
