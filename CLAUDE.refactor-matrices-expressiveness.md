# Second Wave Item 1: Matrix Accessor Expressiveness

> Analysis of call sites that bypass the accessor layer or leak internal matrix structure into business logic. Covers predicates (mode-string comparisons), raw column access, and missing selectors/helpers.

## Related Documents

- `CLAUDE.refactor-matrices-secondwave.md` — Second-wave overview (this is item 1)
- `CLAUDE.function-naming.md` — Naming conventions (predicate pattern: `is_`/`has_`/`are_`)
- `CLAUDE.matrix-access-patterns.md` — Exhaustive access-pattern catalog

---

## Category 1: Predicate Expressiveness

Call sites use `construct_mode()` then pattern-match on raw mode strings (`"A"`, `"B"`, `"C"`, `"HOCA"`, `"HOCB"`, `"UNIT"`). Replace with named predicates in `helpers-mmMatrix.R`.

### New predicates to add to `helpers-mmMatrix.R`

All take signature `(mmMatrix, construct)`.

| Predicate | Mode(s) | Definition |
| --- | --- | --- |
| `is_reflective` | `"C"` | `construct_mode(mm, x) == "C"` |
| `is_LOC_A` | `"A"` | `construct_mode(mm, x) == "A"` |
| `is_LOC_B` | `"B"` | `construct_mode(mm, x) == "B"` |
| `is_HOC_A` | `"HOCA"` | `construct_mode(mm, x) == "HOCA"` |
| `is_HOC_B` | `"HOCB"` | `construct_mode(mm, x) == "HOCB"` |
| `is_unit_weighted` | `"UNIT"` | `construct_mode(mm, x) == "UNIT"` |
| `is_mode_A` | `"A"`, `"HOCA"` | `is_LOC_A \|\| is_HOC_A` |
| `is_mode_B` | `"B"`, `"HOCB"` | `is_LOC_B \|\| is_HOC_B` |
| `is_HOC` | `"HOCA"`, `"HOCB"` | `is_HOC_A \|\| is_HOC_B` |
| `is_single_item` | *(any)* | `length(construct_items(mm, x)) == 1` |

### Call-site replacements

| Location | Current code | Replacement |
| --- | --- | --- |
| `evaluate_warnings.R:4` | `construct_mode(mmMatrix, construct) == "B"` | `is_LOC_B(mmMatrix, construct)` |
| `evaluate_warnings.R:4` | `length(construct_items(mmMatrix, construct)) == 1` | `is_single_item(mmMatrix, construct)` |
| `evaluate_warnings.R:12` | `c(all_constructs_of_mode(mm, "HOCA"), all_constructs_of_mode(mm, "HOCB"))` | `all_HOC(mmMatrix)` (see Cat. 5) |
| `lavaan_syntax.R:17` | `construct_mode(mmMatrix, construct) != "C"` | `!is_reflective(mmMatrix, construct)` |
| `lavaan_syntax.R:26` | `length(items) == 1` | `is_single_item(mmMatrix, construct)` |
| `helpers-mmMatrix.R:69` | `mode %in% c("A", "C", "HOCA")` | `is_mode_A(mm, x) \|\| is_reflective(mm, x)` |
| `helpers-mmMatrix.R:71` | `mode %in% c("B", "HOCB")` | `is_mode_B(mm, x)` |
| `helpers-model.R:95` | `modes[modes %in% "C"]` | Use `all_reflective(mmMatrix)` or predicate |
| `evaluate_reliability.R:61` | `mode %in% c("B", "HOCB")` | `is_mode_B(mm, x)` |
| `evaluate_reliability.R:65` | `mode %in% c("C", "A", "HOCA", "UNIT")` | `!is_mode_B(mm, x)` |
| `evaluate_reliability.R:67` | `length(construct_items(mmMatrix, i)) == 1` | `is_single_item(mmMatrix, i)` |
| `evaluate_reliability.R:111,137,163` | `mode %in% c("A","B","HOCA","HOCB","C","UNIT")` | `TRUE` (remove always-true guard) |
| `plot_dot.R:1160` | `startsWith(parent_mode, "HOC")` | `is_HOC(model$mmMatrix, parent_construct)` |
| `plot_dot.R:1212` | `mm_coding[index, 2] == "C"` | *(deferred to Cat. 4 — plot-internal)* |
| `plot_dot.R:1320-1328` | `switch(c_type, "C"=..., "A"=..., ...)` | *(deferred to Cat. 4 — plot-internal)* |
| `plot_dot.R:1346,1349,1352` | Three-way `if` on mode strings | *(deferred to Cat. 4 — plot-internal)* |
| `plot_dot.R:1444,1447,1450` | Three-way `if` (duplicate) | *(deferred to Cat. 4 — plot-internal)* |

---

## Category 2: Raw smMatrix Column Access

Call sites hard-code `"source"` and `"target"` column names inside `apply()` lambdas and direct subsetting. Replace with `to_path_labels()` decorator and existing `path_sources()`/`path_targets()` selectors.

### New helper to add to `helpers-smMatrix.R`

```r
to_path_labels <- function(smMatrix) {
  paste(path_sources(smMatrix), "->", path_targets(smMatrix))
}
```

### Call-site replacements

| Location | Current code | Replacement |
| --- | --- | --- |
| `boot_utils.R:31-33` | `apply(smMatrix, 1, \(path) { paste(path['source'], '->', path['target']) })` | `to_path_labels(pls_boot$smMatrix)` |
| `boot_utils.R:35-37` | `apply(smMatrix, 1, \(path) { boot_paths[path['source'], path['target'], ...] })` | `mapply(\(s, t) pls_boot$boot_paths[s, t, 1:pls_boot$boots], path_sources(pls_boot$smMatrix), path_targets(pls_boot$smMatrix))` |
| `estimate_pls_mga.R:49-51` | `path_estimate <- function(path, path_coef) { path_coef[path["source"], path["target"]] }` | Remove; use `mapply(\(s, t) path_coef[s, t], path_sources(smMatrix), path_targets(smMatrix))` at call sites |
| `estimate_pls_mga.R:67-68` | `as.data.frame(smMatrix[, c("source", "target"), drop = F])` + `do.call(paste0, cbind(beta["source"], " -> ", beta["target"]))` | `to_path_labels(pls_model$smMatrix)` for labels; `path_sources()`/`path_targets()` + `mapply` for coefficient lookup |

---

## Category 3: Raw mmMatrix Column Access Outside Helpers

Code in `feature_higher_order.R` directly subsets mmMatrix by column name, bypassing accessors.

### New helpers to add to `helpers-mmMatrix.R`

```r
all_items <- function(mmMatrix) {
  unique(mmMatrix[, "measurement"])
}

mmMatrix_for_items <- function(mmMatrix, items) {
  mmMatrix[mmMatrix[, "measurement"] %in% items, , drop = FALSE]
}
```

### Call-site replacements

| Location | Current code | Replacement |
| --- | --- | --- |
| `feature_higher_order.R:3` | `setdiff(mmMatrix[, "measurement"], names(data))` | `setdiff(all_items(mmMatrix), names(data))` |
| `feature_higher_order.R:164` | `mmMatrix[mmMatrix[, "measurement"] %in% hoc_measure_constructs, , drop = FALSE]` | `mmMatrix_for_items(mmMatrix, hoc_measure_constructs)` |

---

## Category 4: Plot-Internal `mm_coding` Matrix — DONE

Replaced all numeric column indices on `mm_coding` with named columns (`"name"`, `"type"`). Seven sites updated across `is_only_endogenous`, `dot_subcomponent_mm`, `extract_mm_nodes`, and `extract_mm_edges`.

---

## Category 5: Missing Compound Selectors

### Problem

Some call sites compose multiple accessor calls to achieve a filtering intent that could be a single selector.

### Call-Site Inventory

| Location | Current code | Intent | Proposed selector |
| --- | --- | --- | --- |
| `evaluate_warnings.R:11-12` | `setdiff(all_constructs(mmMatrix), c(all_constructs_of_mode(mmMatrix, "HOCA"), all_constructs_of_mode(mmMatrix, "HOCB")))` | All lower-order constructs | `all_LOC(mmMatrix)` |

The inverse (`all_HOC(mmMatrix)`) is also useful, matching the compound predicate `is_HOC`.

### Design Decisions

Adding selector companions for the compound predicates:

| Predicate | Companion selector |
| --- | --- |
| `is_HOC` | `all_HOC(mmMatrix)` |
| — | `all_LOC(mmMatrix)` |
| `is_mode_A` | `all_mode_A(mmMatrix)` (if needed) |
| `is_mode_B` | `all_mode_B(mmMatrix)` (if needed) |

These are thin wrappers over `all_constructs_of_mode()` but they hide the mode strings from callers. Only `all_HOC` and `all_LOC` have demonstrated call sites; `all_mode_A`/`all_mode_B` can be added if needed.

---

## Category 6: Dead Code with Raw Access

### Problem

Several files contain commented-out code that uses old raw-access patterns. While not active violations, they clutter the codebase and could mislead future readers into re-adopting the raw patterns.

### Inventory

| Location | Commented-out code |
| --- | --- |
| `evaluate_warnings.R:30-31,35` | `unique(as.vector(mmMatrix[, 1]))` and `setdiff(as.vector(mmMatrix[, 1:2]), ...)` |
| `evaluate_validity.R:16,18` | `intersect(unique(seminr_model$smMatrix), unique(seminr_model$mmMatrix[,1]))` |
| `feature_higher_order.R:10,75-76` | Complex raw matrix filtering with type comparisons |
| `evaluate_effects.R:51-54` | `unique(seminr_model$smMatrix[, "target"])` |

### Recommendation

Remove all commented-out dead code. These are vestiges of pre-refactoring patterns and serve no documentary purpose now that the accessor layer exists.

---

## Summary: Missing Accessors and Helpers

| Gap | Call sites affected | Proposed addition | Target file |
| --- | --- | --- | --- |
| Mode predicates | 13+ sites across 5 files | `is_reflective`, `is_LOC_A`, `is_LOC_B`, `is_HOC_A`, `is_HOC_B`, `is_unit_weighted` | `helpers-mmMatrix.R` |
| Compound predicates | 8+ sites | `is_mode_A`, `is_mode_B`, `is_HOC` | `helpers-mmMatrix.R` |
| Item-count predicate | 3 sites | `is_single_item` | `helpers-mmMatrix.R` |
| Compound selectors | 1+ sites | `all_HOC`, `all_LOC` | `helpers-mmMatrix.R` |
| All items selector | 1 site | `all_items` | `helpers-mmMatrix.R` |
| Row-filter selector | 1 site | `mmMatrix_for_items` | `helpers-mmMatrix.R` |
| Path label decorator | 1 site | `to_path_labels` | `helpers-smMatrix.R` |
| Path coefficient lookup | 2 sites | Use `path_sources()`/`path_targets()` + `mapply` | (no new function) |
| Dead code cleanup | 4 files | Remove commented-out raw access | Various |
| ~~Plot `mm_coding` naming~~ | ~~7 sites~~ | ~~Named columns~~ | ~~`plot_dot.R`~~ — **DONE** |

---

## Implementation Plan

Execution order chosen for dependency safety: dead code first (zero risk), then predicates (foundation for everything else), then selectors and helpers that depend on predicates, then smMatrix refactors (independent of mmMatrix work).

Mark each step `[x]` as completed. If context is cleared, resume from the first unchecked step.

### Phase A: Dead Code Cleanup (Category 6)

No new functions, no behavior change. Delete commented-out raw-access code.

- [x] **A1.** `evaluate_warnings.R` — delete lines 29-43 (commented `warning_struc_meas_model_complete`)
- [x] **A2.** `evaluate_validity.R` — delete lines 15-19 (commented `item_vifs` block) and lines 85-90 (commented `fl_criteria_table` block)
- [x] **A3.** `feature_higher_order.R` — delete line 10 (commented raw matrix filter), lines 61 and 75-76 (commented dimension/type rewrites)
- [x] **A4.** `evaluate_effects.R` — delete lines 51-60 (commented `fSquared` LM alternative)
- [x] **A5.** `devtools::test()` — confirm no regressions from dead code removal

### Phase B: mmMatrix Predicates (Category 1, part 1 — definitions)

Add all new predicates to `helpers-mmMatrix.R`. No call-site changes yet.

- [x] **B1.** Add base predicates: `is_reflective`, `is_LOC_A`, `is_LOC_B`, `is_HOC_A`, `is_HOC_B`, `is_unit_weighted` — all `(mmMatrix, construct)` signature
- [x] **B2.** Add compound predicates: `is_mode_A`, `is_mode_B`, `is_HOC` — compose base predicates
- [x] **B3.** Add item-count predicate: `is_single_item` — `length(construct_items(mm, x)) == 1`
- [x] **B4.** `devtools::test()` — confirm adding functions causes no side effects

### Phase C: Compound Selectors (Category 5 — definitions)

Add selectors to `helpers-mmMatrix.R`. Depends on Phase B predicates.

- [x] **C1.** Add `all_HOC(mmMatrix)` — wraps `c(all_constructs_of_mode(mm, "HOCA"), all_constructs_of_mode(mm, "HOCB"))`
- [x] **C2.** Add `all_LOC(mmMatrix)` — wraps `setdiff(all_constructs(mm), all_HOC(mm))`

### Phase D: mmMatrix Helpers (Category 3 — definitions)

Add helpers to `helpers-mmMatrix.R`. Independent of Phases B/C.

- [x] **D1.** Add `all_items(mmMatrix)` — `unique(mmMatrix[, "measurement"])`
- [x] **D2.** Add `mmMatrix_for_items(mmMatrix, items)` — row-filter by measurement column

### Phase E: Call-Site Replacements — Predicates (Category 1, part 2)

Replace raw mode-string comparisons with predicates from Phase B. Grouped by file.

- [x] **E1.** `evaluate_warnings.R` — replace `construct_mode(...) == "B"` → `is_LOC_B(...)`, `length(construct_items(...)) == 1` → `is_single_item(...)`, compound mode filter → `all_LOC(mmMatrix)` (Cat 5 call site)
- [x] **E2.** `lavaan_syntax.R` — replace `construct_mode(...) != "C"` → `!is_reflective(...)`, `length(items) == 1` → `is_single_item(...)`
- [x] **E3.** `evaluate_reliability.R` — replace `mode %in% c("B", "HOCB")` → `is_mode_B(...)`, `mode %in% c("C","A","HOCA","UNIT")` → `!is_mode_B(...)`, `length(construct_items(...)) == 1` → `is_single_item(...)`, remove three always-true guards (lines 111, 137, 163)
- [x] **E4.** `helpers-mmMatrix.R` (`construct_mode_fn`) — replace `mode %in% c("A","C","HOCA")` → `is_mode_A(...) || is_reflective(...)`, `mode %in% c("B","HOCB")` → `is_mode_B(...)`. Note: predicates are defined in the same file, so they're available
- [x] **E5.** `helpers-model.R` (`all_factors`) — replace `modes[modes %in% "C"]` → use `all_reflective(mmMatrix)` (already exists in `helpers-mmMatrix.R`)
- [x] **E6.** `plot_dot.R` (`is_only_endogenous`) — replace `startsWith(parent_mode, "HOC")` → `is_HOC(model$mmMatrix, parent_construct)`. Also replaced `extract_sm_nodes` HOC check (line 861) and reordered short-circuit guard for structural-model-only plots.
- [x] **E7.** `devtools::test()` — confirm all predicate call-site replacements pass

### Phase F: Call-Site Replacements — mmMatrix Helpers (Category 3)

- [x] **F1.** `feature_higher_order.R:3` — replace `mmMatrix[, "measurement"]` → `all_items(mmMatrix)`
- [x] **F2.** `feature_higher_order.R:164` — replace raw row-filter → `mmMatrix_for_items(mmMatrix, hoc_measure_constructs)`

### Phase G: smMatrix Refactors (Category 2)

Add helper to `helpers-smMatrix.R`, then replace call sites.

- [x] **G1.** Add `to_path_labels(smMatrix)`, `path_sources(smMatrix)`, `path_targets(smMatrix)` to `helpers-smMatrix.R`
- [x] **G2.** `boot_utils.R` — replace `apply(smMatrix, 1, ...)` path-label construction → `to_path_labels(...)`, replace `apply` for boot_paths lookup → `mapply` with `path_sources()`/`path_targets()`
- [x] **G3.** `estimate_pls_mga.R` — remove `path_estimate()` closure, replace with `mapply`; replace `as.data.frame(smMatrix[,...])` + `paste0` → `to_path_labels(...)` + `mapply` for coefficient lookup
- [x] **G4.** `devtools::test()` — confirm all smMatrix refactors pass

### Phase H: Final Verification

- [x] **H1.** `devtools::test()` — full test suite green (298 pass, 0 fail)
- [x] **H2.** Verified no remaining raw mode-string comparisons outside: (a) predicate definitions in `helpers-mmMatrix.R`, (b) deferred plot-internal sites. One additional raw-mode site noted in `helpers-model.R:return_only_composite_scores` (out of plan scope).
- [x] **H3.** Update this document: mark all categories complete, update "Last updated" date

### Notes

- **`all_LOCs()` vs `all_LOC()`**: `all_LOCs()` in `helpers-mmMatrix.R` operates on the specification-time `measurement_model` list. The new `all_LOC(mmMatrix)` operates on the estimation-time matrix. Different abstractions, no conflict.
- **`all_reflective()` already exists**: takes `(mmMatrix, constructs)` — used for `helpers-model.R:all_factors` replacement.
- **`path_sources()`/`path_targets()` added**: The plan referenced these but they didn't exist. Added as row-level (non-unique) smMatrix accessors alongside `to_path_labels`.
- **Deferred plot-internal sites**: `plot_dot.R` lines 1211, 1345-1351, 1443-1449 remain deferred (Cat 4 scope, not this plan).
- **Duplicate `rhoC_AVE.boot_seminr_model`**: `evaluate_reliability.R` has two identical S3 method definitions. This is a separate bug, not addressed here but noted for future cleanup.
- **`return_only_composite_scores`**: `helpers-model.R` still uses `composite_modes <- c("A", "B", "HOCA", "HOCB", "UNIT")`. Out of scope for this plan but noted for future cleanup.

---

Last updated: 2026-02-28
