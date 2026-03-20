# Fix CBSEM summary path direction (Issue #404)

> **IMPORTANT**: This plan must be kept up-to-date at all times. Assume context can be cleared at any time — this file is the single source of truth for the current state of this work. Update this plan before and after task and subtask implementations.

## Branch

`fix/cbsem-summary-path-direction`

## Goal

Fix `summary.cbsem_model` so structural path significance row names read `IV -> DV` (i.e., `rhs -> lhs`) instead of the current reversed `DV -> IV` (`lhs -> rhs`), consistent with PLS summary and causal arrow convention.

## Strategy: Vertical Slice

Single-file fix with focused test coverage:

1. **Test** — Write failing test that asserts path row names follow `IV -> DV` format (red)
2. **Fix** — Swap `lhs`/`rhs` in row name construction at line 84 of `R/report_lavaan.R` (green)
3. **Verify** — Run full CBSEM test suite to confirm no regressions

## Current State

- [x] Plan created
- [ ] Investigation complete
- [ ] Tests written
- [ ] Fix implemented
- [ ] Verification passed

## Key Findings

- **Root cause**: `R/report_lavaan.R` line 84 — `paste(lhs, "->", rhs)`. In lavaan's `~` operator, `lhs` is the DV (outcome) and `rhs` is the IV (predictor). So the current code produces `DV -> IV`.
- **Fix location**: `summarize_cb_structure()` in `R/report_lavaan.R`, line 84 only.
- **Measurement model unaffected**: Line 21 uses `paste(lhs, "->", rhs)` for the `=~` operator, where `lhs` is the construct and `rhs` is the indicator — `Construct -> Indicator` is the correct convention.
- **Path coefficients matrix**: The `path_matrix` (lines 71-76) already correctly uses `rhs` for row names (antecedents) and `lhs` for column names (outcomes) via `df_xtab_matrix`. Only the `significance` row names are wrong.
- **Existing tests**: `test-summary-cbsem.R` checks summary tree structure but not row name content.
- **Model used in tests**: ECSI mobi model with paths like `Image -> Value`, `Quality -> Satisfaction`, etc.

## Questions

> Questions must be crossed off when resolved. Note the decision made.

(none)

## Scope

**In scope**:

- Fix row name direction in `summarize_cb_structure()` significance table
- Add test asserting correct path direction in CBSEM summary

**Out of scope**:

- Measurement model row names (line 21) — `Construct -> Indicator` is correct for `=~`
- PLS summary (already correct)
- Any other summary formatting changes

## Tasks

> **Test-first**: Write or update tests that fail (red) before writing the implementation to make them pass (green).

- [ ] 1a. Add test to `test-summary-cbsem.R` that checks structural path significance row names follow `IV -> DV` format (e.g., `"Image -> Value"` not `"Value -> Image"`)
- [ ] 2. Fix line 84 in `R/report_lavaan.R`: change `paste(lhs, "->", rhs)` to `paste(rhs, "->", lhs)`
- [ ] 3. Run `testthat::test_file("tests/testthat/test-summary-cbsem.R")` — confirm new test passes
- [ ] 4. Run full test suite `devtools::test()` — confirm no regressions

## Completed

(none yet)

---

Last updated: 2026-03-20
