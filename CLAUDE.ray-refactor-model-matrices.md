# Refactor Model Matrices: Encapsulate mmMatrix and smMatrix Access

> **IMPORTANT**: This plan must be kept up-to-date at all times. Assume context can be cleared at any time — this file is the single source of truth for the current state of this work. Update this plan before and after task and subtask implementations.

## Branch

`ray/refactor-model-matrices`

## Related Documents

- `CLAUDE.matrix-access-patterns.md` — Exhaustive catalog of all 16 distinct access patterns, stability analysis, helper adoption status, and the 28-issue bug catalog
- `CLAUDE.function-naming.md` — Function naming conventions, proposed renames, S3 dispatch plan, and `constructs_in_model` split design
- `CLAUDE.oop-systems.md` — OOP systems analysis: R6 vs S3 vs environments vs plain functions

## Motivation

`mmMatrix` and `smMatrix` are the two core internal data structures — every estimation, evaluation, and plotting function depends on them. Today, ~175 call sites across ~48 files access them via raw `matrix[row, col]` subsetting. This causes three concrete problems:

- **Bugs.** 28 GitHub issues (15 confirmed, 13 probable) trace to raw matrix access — primarily `drop=FALSE` omissions and column-index mismatches. The worst-hit area is `summary()` with 14 issues alone. See `CLAUDE.matrix-access-patterns.md` § "Related Issues and Bugs" for the full catalog.
- **Code duplication.** Helpers exist but developers copy-paste raw patterns instead. `construct_indicators()` has 2 call sites vs. 18 raw duplicates; `measure_mode()` has 3 call sites vs. 12 duplicates. Each duplicate is a potential bug site.
- **Representation lock-in.** Any change to the matrix structure (adding columns, switching to data.frame, caching lookups) requires touching all ~175 sites. Accessor functions let the internal representation evolve by changing only the accessor internals.

## Goal

Replace all direct matrix access of `mmMatrix` and `smMatrix` with accessor/mutator functions. This is an internal refactoring with no user-facing API changes.

## Current State

- [x] Plan created
- [x] Research completed: cataloged all direct access patterns
- [x] Phase 1: smMatrix read-side encapsulation
- [x] Phase 2: mmMatrix read-side encapsulation (2A ✓, 2B ✓, 2C ✓, 2D ✓)
- [x] Phase 3: Write-side encapsulation
- [x] Phase 4: Cleanup, file consolidation, and verification (4.1 ✓, 4.2 ✓, 4.3 ✓, 4.4 ✓, 4.5 ✓, 4.6 ✓)
- [x] Phase 5: Update CLAUDE.md to document accessor helpers for internal matrices

## Key Decisions

1. **Full encapsulation over partial.** Only 16 distinct access patterns exist and the set has been stable since 2020. Existing helpers are severely under-adopted (`construct_indicators()` has 2 call sites vs. 18 raw duplicates). Full encapsulation removes raw patterns that developers copy-paste. See `CLAUDE.matrix-access-patterns.md` § "Implications for Refactoring Approach" for alternatives considered.

2. **Plain functions, not R6/S3 objects.** The matrices are immutable lookup tables, not stateful objects. Plain functions preserve value semantics (avoiding aliasing bugs from `mmMatrix <- model$mmMatrix`), add no dependencies, and match the codebase's functional style. See `CLAUDE.oop-systems.md` § "Recommendation" for the full tradeoff analysis. R6 can be revisited if matrices evolve into richer stateful objects.

3. **Naming convention: `object_qualifier`.** Accessors named as `construct_items`, `construct_mode`, etc. Selectors use `all_`/`only_` prefixes. Predicates use `is_`/`has_`/`are_all_`. Mutators use `verb_noun`. See `CLAUDE.function-naming.md` § "Proposed Conventions" for the full scheme and § "Proposed Renames" for every rename.

4. **S3 generics for polymorphic accessors.** `construct_items` and `construct_names` unify multiple existing functions across types (mmMatrix, construct vector, model, measurement_model list). Created as S3 generics with dispatched methods. See `CLAUDE.function-naming.md` § "Accessor S3 dispatch".

5. **`constructs_in_model()` split.** Separated into spec-level accessors (`construct_names`, `construct_type`) and estimation-level accessor (`construct_scores`). See `CLAUDE.function-naming.md` § "Split: `constructs_in_model`".

6. **New accessors go in `inspect_` files.** Helpers currently in `library.R` migrate to `inspect_mmMatrix.R` or `inspect_smMatrix.R`. S3 generics + all methods co-located in the primary type's inspect file.

7. **No input validation for now.** Internal functions; raw subsetting already surfaces bad input downstream. Validation deferred to avoid breaking reverse dependencies.

8. **Mutator wrappers for writes.** Thin `verb_noun` wrappers replace raw `rbind()` and matrix construction to encapsulate column-order assumptions. Only ~15 write sites. Names deferred to Phase 3.

9. **Performance: unmeasurable impact.** Accessor overhead is ~1-2μs per call; PLS hot path dominated by vectorized matrix algebra. Precomputation before the PLS loop (task 2.10) eliminates most per-iteration lookups. See `CLAUDE.oop-systems.md` § "Performance analysis confirms".

10. **Helper file placement deferred.** Five smMatrix helpers remain in `feature_plspredict.R` (`subset_by_construct`, `construct_antecedent_in_list`, `depends_on`, `antecedents_in_list`, `construct_order`) and predicates in `evaluate_model.R`. Moving them to `inspect_smMatrix.R` is desirable but deferred — functions may move later as the structure stabilizes.

11. **Interaction predicates.** `grepl("\\*", ...)` patterns appear ~13 times across R/ files for interaction detection. Two new predicates cover these: `has_interactions(smMatrix, outcome = NULL)` (existence test on smMatrix) and `is_interaction(construct_name)` (single-name test). Added to Phase 2 since most sites co-occur with mmMatrix access patterns.

## Target File Organization

After all renames and consolidation, matrix helper functions live in these files. This section is the single reference for "where does this function belong?"

### `inspect_smMatrix.R` — smMatrix accessors, selectors, predicates, mutators

All functions that take smMatrix as their primary argument.

- S3 generic + methods: `construct_names` (generic, `.structural_model`, `.seminr_model`, `.measurement_model`, `.mmMatrix`, `.list`, `.default`)
- Accessors: `construct_antecedents`, `construct_targets`, `construct_interactions`
- Computed: `construct_order`, `construct_antecedents_all`, `have_antecedents_in`
- Selectors: `all_endogenous`, `all_exogenous`, `only_exogenous`, `only_endogenous`, `all_interactions`
- Predicates: `is_interaction`, `has_interactions`, `has_paths_to`, `has_direct_effects`, `are_construct_names_valid`
- Mutators: `remove_paths_to`, `remove_paths_from`, `keep_paths_from`, `remove_path`
- Internal helpers: `subset_by_construct`, `construct_antecedent_in_list`

### `inspect_mmMatrix.R` — mmMatrix accessors, selectors, converters; measurement model list helpers

All functions that take mmMatrix or a measurement model list as their primary argument.

- S3 generic + methods: `construct_items` (generic, `.mmMatrix`, `.matrix`, `.construct`, `.seminr_model`, `.measurement_model`, `.list`)
- mmMatrix accessors: `construct_mode`, `construct_mode_fn`, `all_constructs`, `all_constructs_of_mode`, `construct_of_item`
- mmMatrix selectors: `all_reflective`
- mmMatrix predicate: `are_indicators_in_data`
- Measurement model list accessors: `construct_name`
- Measurement model list selectors: `all_LOC_items`, `all_non_interactions`, `all_LOCs`, `all_interaction_fns`
- Computed: `item_count`
- Mutator: `append_mm_rows`
- Converters: `mm2matrix`, `as.reflective` (generic + methods)

### `library.R` — Model-level accessors and selectors

Functions that take a model object (`pls_model`, `cbsem_model`, etc.) as their primary argument.

- Accessors: `construct_type`, `constructs_in_model`, `construct_scores`
- Selectors: `all_factors`, `all_composites`

### `plot_dot.R` — Plot-specific model helpers

- Predicate: `is_only_endogenous` (tightly coupled to `extract_mm_coding`, stays here)

### `feature_higher_order.R` — HOC-specific helpers (stay per Key Decision #10)

- Selector: `all_HOCs`
- Accessor: `all_HOC_measures` (mmMatrix items not in data — HOC construct measures)
- Mutators: `expand_HOC_to_LOCs`, `remove_HOC` (currently dead code, retained for intended use)

### Deleted (Phase 4.2)

- `items_per_mode` — never called
- `mmMatrix_per_construct` — leaked row structure

## Scope

**In scope:** Create missing accessors, replace all direct mmMatrix/smMatrix subsetting in R/ and test files, consolidate duplicates, rename per conventions.

**Out of scope:** Changing the underlying matrix representation, user-facing API changes, S3 class hierarchy changes, R6/reference semantics.

## Tasks

### Phase 1: Baseline and smMatrix Read-Side (Low Risk)

smMatrix has fewer access points (~75) and existing helpers cover most patterns — good warmup.

> **Test-first**: Run full test suite before and after each batch of changes.

- [x] 1.1 Run full test suite, confirm all tests pass (baseline: 269 PASS, 0 FAIL)
- [x] 1.2 Replace direct smMatrix reads with existing helpers across R/ files
  - `all_endogenous`, `construct_antecedents`, `construct_names`, `only_exogenous`, `all_exogenous`
  - Fixed artificial smMatrix in `plot_dot.R` `dot_graph.measurement_model` to include dimnames
  - Renamed local variables shadowing helpers (`only_exogenous` → `only_exo`, `construct_names` → `all_constructs`)
- [x] 1.3 Add missing smMatrix helpers
  - `construct_targets(smMatrix, source)` for `smMatrix[smMatrix[,"source"]==x, "target"]`
  - `only_endogenous(smMatrix)` for `setdiff(all_endogenous(smMatrix), all_exogenous(smMatrix))`
- [x] 1.4 Rename smMatrix accessors per naming conventions
  - `antecedents_of` → `construct_antecedents`
  - `interactions_of` → `construct_interactions`
  - **Reordered arguments to container-first**: `construct_antecedents(smMatrix, outcome)`, `construct_interactions(smMatrix, outcome)`
  - See `CLAUDE.function-naming.md` § "Proposed Renames" for full list
- [x] 1.5 Rename smMatrix predicates per naming conventions
  - `direct_effects_are_specified` → `has_direct_effects`
  - `construct_names_are_valid` → `are_construct_names_valid`
- [x] 1.6 Run full test suite, confirm all tests still pass (269 PASS, 0 FAIL)

### Phase 2: mmMatrix Read-Side (Main Effort)

mmMatrix has ~100+ direct access points across ~28 R source files and ~34 test files.

#### 2A. Create new accessors

- [x] 2.1a Write tests for new mmMatrix accessor functions
- [x] 2.1b Create missing helpers
  - `all_constructs(mmMatrix)` — wraps `unique(mmMatrix[,"construct"])`
  - `all_constructs_of_mode(mmMatrix, mode)` — wraps mode-column filtering (selector convention)
  - `construct_of_item(mmMatrix, item)` — reverse item lookup (container-first)
- [x] 2.1c Split `constructs_in_model()` into:
  - `construct_names(model)` — manual class dispatch on model (spec-level, handles HOC). Note: uses `inherits()` dispatch rather than S3 `UseMethod()` because NAMESPACE registration requires exported generic; deferred to Phase 2D when generic is ready for export.
  - `construct_scores(model)` — estimation-level accessor (handles HOC first-stage score merging)
  - `constructs_in_model()` retained as thin wrapper using matrix-level accessors (works with unclassed model lists during estimation)
  - `construct_type(model, name)` deferred to Phase 2C rename of `get_construct_type`
- [x] 2.1d Add interaction predicates
  - `is_interaction(construct_name)` — tests if a name contains `*` (vectorized)
  - `has_interactions(smMatrix, outcome = NULL)` — tests if smMatrix (or a specific DV) has interaction terms
- [x] 2.1e Run full test suite, confirm new accessors pass and no regressions (294 PASS, 0 FAIL)

#### 2B. Replace raw access with accessors

- [x] 2.2 Replace `construct_indicators()` pattern
  - `specify_interactions.R`: 4 raw patterns → `construct_indicators()`
  - `library.R` (`unit_weights`): numeric index `mmMatrix[,1]` → `construct_indicators()`
  - `feature_higher_order.R`: raw filter on HOC subset → `construct_indicators(name, HOCs)`
  - `lavaan_syntax.R`: refactored `lavaan_construct()` to take construct name + mmMatrix, replaced column accesses with `measure_mode()` and `construct_indicators()`
  - `feature_higher_order.R`: `matrix(construct, ...)[,2]` → `construct_items(construct)` for construct vector
- [x] 2.3 Replace `construct_type()` / `measure_mode()` patterns
  - All call sites already use `measure_mode()` or `get_construct_type()` — no remaining raw `[,"type"]` patterns outside accessor definitions
  - `lavaan_syntax.R`: `construct_matrix[, "type"]` eliminated by `lavaan_construct()` refactor
- [x] 2.4 Replace `all_constructs()` pattern
  - `feature_higher_order.R:52`: `unique(new_mm[, "construct"])` → `all_constructs(new_mm)`
  - `feature_higher_order.R:160`: `unique(HOCs[,"construct"])` → `all_constructs(HOCs)`
  - Remaining `all_constructs()` call sites already use the accessor
- [x] 2.5 Replace remaining read patterns
  - `estimate_simplePLS.R`: row filter → `intersect(all_constructs, sm_constructs)` + `construct_indicators()` (preserves mmMatrix declaration order)
  - `evaluate_warnings.R`: complex filter → `all_constructs_of_mode()` exclusion + `construct_indicators()` + `is_interaction()` filter
  - `plot_dot.R` (`is_sink`): reverse item→type lookup → `construct_of_item()` + `measure_mode()`
  - `plot_dot.R` (`extract_mm_edges`): row iteration over subset → `construct_indicators()` item loop
  - `plot_dot.R` (`dot_graph.measurement_model`): `unique(c(model[,1], model[,2]))` → `construct_names(model)` (smMatrix)
  - `estimate_pls.R`, `specify_interactions.R`: smMatrix `unique(c(sm[,1], sm[,2]))` → `construct_names()`
  - **Remaining deferred:** `feature_higher_order.R:157,159` — HOC identification patterns (`setdiff(mmMatrix[,"measurement"], ...)` and row filter) require dedicated HOC accessors → Phase 3/4
- [x] 2.5a Replace `grepl("\\*", ...)` interaction detection patterns
  - All active patterns already use `is_interaction()` (added in Phase 2A)
  - Only remaining `grepl("\\*", ...)` is inside `is_interaction()` definition itself
  - One commented-out pattern in `evaluate_measurement_model.R:5` — inactive
- [x] 2.5b Run full test suite, confirm replacements introduce no regressions (294 PASS, 0 FAIL)

#### 2C. Rename per conventions

All renames listed in `CLAUDE.function-naming.md` § "Proposed Renames". Key groups:

- [x] 2.6 mmMatrix helpers (partial): `measure_mode` → `construct_mode`, `get_measure_mode` → `construct_mode_fn`, `get_construct_type` → `construct_type`, `all_indicator_names_are_in_data` → `are_indicators_in_data`, `is_sink` → `is_only_endogenous`
  - **Reorder arguments to container-first**: `construct_mode(mmMatrix, construct)`, `construct_mode_fn(mmMatrix, construct)`, `construct_type(model, construct)` (already container-first)
  - Fixed sapply calls with anonymous function wrappers for container-first reordering
  - Renamed local variables `construct_type` → `c_type` in `plot_dot.R` to avoid shadowing accessor
  - Fixed redundant double-`sapply` in `all_factors()` — now computes modes once
  - **Deferred to 2D:** `construct_indicators` → `construct_items` (requires S3 generic for name collision resolution)
- [x] 2.7 Model-level selectors: `get_factors` → `all_factors`, `get_composites` → `all_composites`, `HOCs_in_model` → `all_HOCs`
- [x] 2.8 Measurement model list helpers (partial): `all_loc_non_int_items` → `all_LOC_items`, `mm_constructs` → `all_non_interactions`, `loc_constructs` → `all_LOCs`, `mm_interactions` → `all_interaction_fns`, `number_of_items` → `item_count`
  - **Deferred to 2D:** `all_construct_names` → `construct_names` (S3), `all_items` → `construct_items` (S3) — requires S3 generic creation to resolve name collisions
- [x] 2.8a Run full test suite, confirm renames introduce no regressions (294 PASS, 0 FAIL)

#### 2D. S3 generics and optimizations

- [x] 2.9 Create S3 generics for `construct_items` and `construct_names`
  - See `CLAUDE.function-naming.md` § "Accessor S3 dispatch" for dispatch table
  - Includes deferred renames from 2C that require S3 dispatch to resolve name collisions:
    - `construct_indicators` → `construct_items` (mmMatrix method) + container-first arg reorder
    - `all_construct_names` → `construct_names` (measurement_model method)
    - `all_items` → `construct_items` (measurement_model method)
    - `items_of_construct` → `construct_items` (model method)
  - Added `.matrix` fallback for `construct_items` (rbind strips mmMatrix class)
  - Added `.list` fallbacks for both generics (append strips measurement_model class)
- [x] 2.10 Precompute mappings in `simplePLS()` before PLS loop (~5 lines, eliminates ~14 per-iteration lookups)
  - Built `construct_item_map` once before the loop; replaced 2 in-loop `construct_items()` calls with map lookups
- [x] 2.11 ~~Fix `get_measure_mode()` redundancy (6 identical lookups → extract type once)~~ — resolved: `all_factors()` double-`sapply` eliminated in 2C
- [x] 2.12 Run full test suite, confirm all tests still pass (294 PASS, 0 FAIL)

### Phase 3: Write-Side Encapsulation

~15 write sites: matrix creation (`mm2matrix`, `matrix()`), row appending (`rbind`), indirect writes (mmMatrix as index into other matrices), local copies. Also includes remaining smMatrix row-filtering patterns deferred from Phase 1.

- [x] 3.1 Audit all write sites and design builder/mutator API
  - Cataloged 22 remaining patterns across 8 files (9 Phase 3, 6 Phase 4.1, 2 Phase 4.2, 2 Phase 3/4.1, 3 acceptable)
  - Designed mutator API: smMatrix mutators (`remove_paths_to`, `remove_paths_from`, `keep_paths_from`, `remove_path`), predicate (`has_paths_to`), mmMatrix mutator (`append_mm_rows`), HOC helper (`all_HOC_measures`)
- [x] 3.2 Implement write-side functions
  - Added 4 smMatrix mutators + 1 predicate to `inspect_smMatrix.R`
  - Added `append_mm_rows` to `inspect_mmMatrix.R` (preserves "mmMatrix" class after rbind)
  - Added `all_HOC_measures` to `feature_higher_order.R` (HOC identification helper)
- [x] 3.3 Replace write-site call sites
  - `feature_higher_order.R:18` — `sm[-which(sm[, "target"] == ...)]` → `remove_paths_to(sm, construct[1])`
  - `feature_higher_order.R:27` — `sm[-which(sm[, "source"] == ...)]` → `remove_paths_from(sm, construct[1])`
  - `feature_higher_order.R:57` — `sm[sm[, "source"] %in% ...]` → `keep_paths_from(sm, all_constructs(new_mm))`
  - `feature_higher_order.R:162` — `setdiff(mmMatrix[,"measurement"], names(rawdata))` → `all_HOC_measures(mmMatrix, rawdata)`
  - `feature_higher_order.R:164` — `mmMatrix[which(...)]` → safe `%in%` filter with `drop=FALSE`
  - `feature_higher_order.R:34` — `remove_HOC_in_measurement_model`: fixed `!mm[,"construct"] == x` → `mm[,"construct"] != x` + added `drop=FALSE`
  - `feature_plspredict.R:115` — `sm[!(sm[,"source"] %in% interactions), ...]` → `remove_paths_from(sm, interactions)`
  - `specify_interactions.R:302` — `sm[!is_interaction(sm[,"source"]), ...]` → `remove_paths_from(sm, all_interactions(sm))`
  - `specify_interactions.R:358` — `rbind(mmMatrix, intxns_mm)` → `append_mm_rows(mmMatrix, intxns_mm)`
  - `specify_interactions.R:384` — `rbind(mmMatrix, as.reflective(...))` → `append_mm_rows(mmMatrix, as.reflective(...))`
  - `evaluate_effects.R:48` — `subset(with_sm, !(source == iv & target == dv))` → `remove_path(with_sm, iv, dv)`
  - `evaluate_effects.R:73` — `any(without_sm[,"target"] == dv)` → `has_paths_to(without_sm, dv)`
  - **Left as-is:** `estimate_cbsem.R:125` — `structural_model[, "source"] <- sapply(...)` — uses named column access, one-off CBSEM-specific
- [x] 3.4 Rename mutators: `substitute_dimensions_for_HOC` → `expand_HOC_to_LOCs`, `remove_HOC_in_measurement_model` → `remove_HOC`
  - `expand_HOC_to_LOCs`: 1 definition + 1 call site in `feature_higher_order.R`
  - `remove_HOC`: 1 definition, 0 call sites (dead code, retained for intended use)
- [x] 3.5 Run full test suite, confirm all tests still pass (294 PASS, 0 FAIL)

### Phase 4: Cleanup and Verification

- [x] 4.1 Eliminate numeric column indices inside accessor functions (use named only)
  - `inspect_smMatrix.R`: Replaced `x[,1]`/`x[,2]` with `x[, "source"]`/`x[, "target"]` in `construct_names.structural_model`, `only_exogenous`, `construct_antecedents`, `construct_targets`
  - `inspect_mmMatrix.R`: Replaced `x[,1]`/`x[,2]` with `x[, "construct"]`/`x[, "measurement"]` in `construct_items.mmMatrix`, `construct_items.matrix`
  - Added `construct_names.mmMatrix` S3 method (delegates to `all_constructs()`) + made `.default` handle both matrix shapes
  - `as.reflective.matrix()`: Added defensive `colnames()` assignment as temporary fix
  - **Note:** `as.reflective.matrix()` shouldn't have to add column names — they should be assigned at matrix creation time (in `measure_interaction` / `two_stage` / `orthogonal` closures) and preserved throughout. Defer proper fix to future work.
- [x] 4.2 Delete dead code: `items_per_mode` (never called), `mmMatrix_per_construct` (leaks row structure)
  - Both removed from `library.R`; confirmed `remove_HOC` still dead but retained for intended future use
- [x] 4.3 Consolidate any remaining duplicate accessors
  - No remaining duplicates found after Phase 2D S3 unification
- [x] 4.4 Consolidate helper functions into target files per § "Target File Organization"
  - Moved `has_direct_effects` from `evaluate_model.R` → `inspect_smMatrix.R`
  - Moved `are_construct_names_valid` from `evaluate_model.R` → `inspect_smMatrix.R`
  - Moved `construct_order`, `depends_on` (→ `construct_antecedents_all`), `antecedents_in_list` (→ `have_antecedents_in`), `subset_by_construct`, `construct_antecedent_in_list` from `feature_plspredict.R` → `inspect_smMatrix.R`
  - Moved `construct_mode`, `construct_mode_fn` from `library.R` → `inspect_mmMatrix.R`
  - Moved `are_indicators_in_data` from `evaluate_model.R` → `inspect_mmMatrix.R`
  - Moved `construct_type` from `plot_dot.R` → `library.R` (near `constructs_in_model` which calls it)
  - `is_only_endogenous` stays in `plot_dot.R` — only used within that file, tightly coupled to `extract_mm_coding`
  - Did NOT create `inspect_model.R` — model-level accessors kept in `library.R` alongside existing model utilities
- [x] 4.5 Final full test suite run (294 PASS, 0 FAIL)
- [x] 4.6 Verify no remaining direct matrix access patterns (grep audit)

### Phase 5: Documentation

- [x] 5.1 Update CLAUDE.md to document that internal matrices (`mmMatrix`, `smMatrix`) must be accessed via accessor helpers, not raw subsetting
  - Added "Internal Matrices: mmMatrix and smMatrix" section to Architecture
  - Updated module organization table to include `inspect_*.R` files
  - Documented accessor locations, S3 generics, naming conventions, and reference to full catalog

## Completed

### Phase 1: smMatrix read-side encapsulation (2026-02-27)

**Files modified:** `inspect_smMatrix.R`, `evaluate_model.R`, `library.R`, `feature_consistent.R`, `estimate_simplePLS.R`, `feature_higher_order.R`, `feature_plspredict.R`, `report_paths_and_intervals.R`, `evaluate_validity.R`, `evaluate_effects.R`, `lavaan_syntax.R`, `compute_metrics.R`, `plot_dot.R`

**Changes:**
- Replaced ~25 direct smMatrix reads with helpers (`construct_antecedents`, `all_endogenous`, `all_exogenous`, `only_exogenous`, `construct_names`, `construct_targets`)
- Added new helpers: `construct_targets(smMatrix, source)`, `only_endogenous(smMatrix)`
- Renamed `antecedents_of` → `construct_antecedents` (container-first args) across 12 call sites
- Renamed `interactions_of` → `construct_interactions` (container-first args) across 2 call sites
- Renamed `direct_effects_are_specified` → `has_direct_effects`
- Renamed `construct_names_are_valid` → `are_construct_names_valid`
- Fixed missing dimnames on artificial smMatrix in `plot_dot.R` `dot_graph.measurement_model`
- Renamed local variables shadowing helper functions in `feature_plspredict.R`

**Remaining direct smMatrix access (deferred):**
- Write-side patterns in `feature_higher_order.R` (rows 13, 22, 52) → Phase 3
- Write-side pattern in `evaluate_effects.R:48` (subset) → Phase 3
- Write-side pattern in `feature_plspredict.R:115` (row filtering) → Phase 3
- Predicate in `evaluate_effects.R:73` (`any(without_sm[,"target"] == dv)`) → Phase 3
- MGA data.frame conversion in `estimate_pls_mga.R:67` — safe named access, left as-is
- Numeric indices inside helper definitions (`inspect_smMatrix.R`) → Phase 4

### Phase 2B: mmMatrix read-side raw access replacement (2026-02-27)

**Files modified:** `specify_interactions.R`, `library.R`, `feature_higher_order.R`, `lavaan_syntax.R`, `estimate_simplePLS.R`, `evaluate_warnings.R`, `plot_dot.R`, `estimate_pls.R`

**Changes:**
- Replaced ~20 raw mmMatrix/smMatrix column-access patterns with accessor function calls
- `specify_interactions.R`: 4 raw `measurement_model[, "construct"]`/`[, "measurement"]` → `construct_indicators()`
- `library.R` (`unit_weights`): `sum(mmMatrix[,1] == i)` → `length(construct_indicators(i, mmMatrix))`
- `feature_higher_order.R`: `unique(HOCs[,"construct"])` → `all_constructs()`, `HOCs[..., "measurement"]` → `construct_indicators()`, `unique(new_mm[, "construct"])` → `all_constructs()`, `matrix(construct, ...)[,2]` → `construct_items()`
- `lavaan_syntax.R`: Refactored `lavaan_construct()` to take construct name + mmMatrix instead of pre-filtered subset; eliminated `mm_sub_matrix` intermediary; replaced column accesses with `measure_mode()` and `construct_indicators()`
- `estimate_simplePLS.R`: Row filter `mmMatrix[mmMatrix[,"construct"] %in% sm_constructs, "measurement"]` → `intersect(all_constructs(mmMatrix), sm_constructs)` + `construct_indicators()` (preserves mmMatrix declaration order via `intersect`)
- `evaluate_warnings.R`: Complex non-HOC/non-interaction filter → `all_constructs_of_mode()` exclusion + `construct_indicators()` + `is_interaction()` filter
- `plot_dot.R` (`is_sink`): Raw reverse lookup `mmMatrix[, "measurement"] == x` / `mmMatrix[idx, "type"]` → `construct_of_item()` + `measure_mode()`
- `plot_dot.R` (`extract_mm_edges`): Row iteration `for (i in 1:nrow(mm_matrix_subset))` → `for (manifest_variable in construct_indicators(construct, mmMatrix))`
- `plot_dot.R`, `estimate_pls.R`, `specify_interactions.R`: `unique(c(sm[,1], sm[,2]))` → `construct_names()`

**Remaining raw mmMatrix access (deferred):**
- Inside accessor function definitions (`inspect_mmMatrix.R`, `library.R:construct_mode`) → Phase 4.1
- Dead code (`items_per_mode`, `mmMatrix_per_construct` in `library.R`) → Phase 4.2
- Write-side pattern (`feature_higher_order.R:29` row removal) → Phase 3
- HOC identification patterns (`feature_higher_order.R:157,159` — `setdiff(mmMatrix[,"measurement"], ...)` and row filter) → Phase 3/4
- `inspect_mmMatrix.R:179` (`as.reflective.matrix` write) → Phase 3
- Commented-out patterns in `evaluate_warnings.R`, `evaluate_validity.R`, `feature_higher_order.R` — inactive code

### Phase 2C: Rename per conventions (2026-02-27)

**Files modified:** `inspect_mmMatrix.R`, `library.R`, `evaluate_measurement_model.R`, `evaluate_model.R`, `evaluate_reliability.R`, `evaluate_warnings.R`, `feature_higher_order.R`, `estimate_pls.R`, `estimate_cbsem.R`, `lavaan_syntax.R`, `plot_dot.R`, `report_descriptives.R`, `specify_interactions.R`, `NAMESPACE`

**Changes (13 renames across 14 files):**
- `measure_mode` → `construct_mode` (container-first: `(mmMatrix, construct)`) — 9 call sites
- `get_measure_mode` → `construct_mode_fn` (container-first: `(mmMatrix, construct)`) — 3 call sites
- `get_construct_type` → `construct_type` — 9 call sites; internal `construct_type` local var → `c_type`
- `get_factors` → `all_factors` — 3 call sites; fixed redundant double-`sapply` (was computing modes twice)
- `get_composites` → `all_composites` — 3 call sites
- `HOCs_in_model` → `all_HOCs` — 4 call sites
- `all_indicator_names_are_in_data` → `are_indicators_in_data` — 2 call sites
- `is_sink` → `is_only_endogenous` — 3 call sites
- `number_of_items` → `item_count` — 2 call sites (internal to inspect_mmMatrix.R)
- `mm_constructs` → `all_non_interactions` — 4 call sites (internal to inspect_mmMatrix.R)
- `loc_constructs` → `all_LOCs` — 2 call sites (internal to inspect_mmMatrix.R)
- `mm_interactions` → `all_interaction_fns` — 3 call sites
- `all_loc_non_int_items` → `all_LOC_items` — 4 call sites

**sapply container-first pattern:** Where renamed functions had argument reordering and were previously passed directly to `sapply(X, FUN, ...)`, call sites now use anonymous function wrappers: `sapply(X, function(c) construct_mode(mmMatrix, c))`.

**Deferred to Phase 2D (require S3 generic creation):**
- `construct_indicators` → `construct_items` (collides with existing `construct_items` on construct vectors)
- `all_construct_names` → `construct_names` (collides with existing `construct_names` on smMatrix)
- `all_items` → `construct_items` (same collision)
- `items_of_construct` → `construct_items` (same collision)

### Phase 2D: S3 generics and optimizations (2026-02-28)

**Files modified:** `inspect_smMatrix.R`, `inspect_mmMatrix.R`, `library.R`, `estimate_simplePLS.R`, `estimate_cbsem.R`, `evaluate_measurement_model.R`, `evaluate_model.R`, `evaluate_validity.R`, `evaluate_warnings.R`, `evaluate_reliability.R`, `feature_consistent.R`, `feature_higher_order.R`, `feature_plspredict.R`, `compute_metrics.R`, `lavaan_syntax.R`, `specify_interactions.R`, `plot_dot.R`, `report_lavaan.R`, `NAMESPACE`

**Changes:**

**S3 generic `construct_names` (inspect_smMatrix.R):**
- Converted manual `inherits()` dispatch to `UseMethod()` S3 generic
- Methods: `.structural_model` (smMatrix), `.seminr_model` (estimated models), `.measurement_model` (measurement model list), `.list` (fallback for lists losing class after `append()`), `.default` (unclassed matrices)
- Removed `all_construct_names()` from `inspect_mmMatrix.R` — replaced by `.measurement_model` method
- Replaced 4 `all_construct_names()` call sites: `estimate_cbsem.R` (2), `evaluate_model.R` (1), `report_lavaan.R` (1)

**S3 generic `construct_items` (inspect_mmMatrix.R):**
- Created `UseMethod()` S3 generic with 7 methods:
  - `.mmMatrix` — replaces `construct_indicators()` (container-first: `(mmMatrix, construct_name)`)
  - `.matrix` — fallback for matrices losing "mmMatrix" class after `rbind()`
  - `.construct` — items from a construct vector (was old `construct_items()`)
  - `.seminr_model` — replaces `items_of_construct()` from `library.R`
  - `.measurement_model` — replaces `all_items()` (all item names across all constructs)
  - `.list` — fallback for measurement model lists losing class after `append()`
- Removed: `construct_indicators()`, old `construct_items()`, `all_items()` from `inspect_mmMatrix.R`; `items_of_construct()` from `library.R`
- Replaced ~50 call sites across 18 files

**Key class-stripping discoveries:**
- `rbind()` strips "mmMatrix" class from matrices → `.matrix` fallback handles this
- `append()` strips "measurement_model" class from lists → `.list` fallback handles this
- Also fixed `as.reflective.measurement_model` to preserve class on returned list

**simplePLS precomputation (estimate_simplePLS.R):**
- Built `construct_item_map` once before the PLS loop
- Replaced 2 in-loop `construct_items()` calls with direct map lookups

**NAMESPACE registrations added:**
- `S3method(construct_items, construct)`, `S3method(construct_items, list)`, `S3method(construct_items, matrix)`, `S3method(construct_items, measurement_model)`, `S3method(construct_items, mmMatrix)`, `S3method(construct_items, seminr_model)`
- `S3method(construct_names, default)`, `S3method(construct_names, list)`, `S3method(construct_names, measurement_model)`, `S3method(construct_names, seminr_model)`, `S3method(construct_names, structural_model)`

### Phase 3: Write-side encapsulation (2026-02-28)

**Files modified:** `inspect_smMatrix.R`, `inspect_mmMatrix.R`, `feature_higher_order.R`, `feature_plspredict.R`, `specify_interactions.R`, `evaluate_effects.R`

**New functions created:**

- `inspect_smMatrix.R`: 4 mutators (`remove_paths_to`, `remove_paths_from`, `keep_paths_from`, `remove_path`) + 1 predicate (`has_paths_to`)
- `inspect_mmMatrix.R`: 1 mutator (`append_mm_rows` — rbind wrapper preserving "mmMatrix" class)
- `feature_higher_order.R`: 1 accessor (`all_HOC_measures` — HOC identification helper)

**Call site replacements:**

- `feature_higher_order.R`: 3 smMatrix row-filter patterns → `remove_paths_to`, `remove_paths_from`, `keep_paths_from`; 2 HOC identification patterns → `all_HOC_measures` + safe filter with `drop=FALSE`; fixed `remove_HOC` implementation (`!==` → `!=`, added `drop=FALSE`)
- `feature_plspredict.R`: 1 interaction-source filter → `remove_paths_from`
- `specify_interactions.R`: 1 interaction-source filter → `remove_paths_from`; 2 rbind patterns → `append_mm_rows`
- `evaluate_effects.R`: 1 `subset()` → `remove_path`; 1 `any(target == dv)` → `has_paths_to`

**Renames (2 mutators):**

- `substitute_dimensions_for_HOC` → `expand_HOC_to_LOCs` (1 definition + 1 call site)
- `remove_HOC_in_measurement_model` → `remove_HOC` (definition only, 0 call sites — dead code)

**Left as-is:**

- `estimate_cbsem.R:125` — `structural_model[, "source"] <- sapply(...)` — uses named column access, one-off CBSEM lavaan name conversion
- `feature_higher_order.R:164` — one remaining `mmMatrix[condition, , drop=FALSE]` filter inside HOC-specific function using `all_HOC_measures` result

### Phase 4: Cleanup and verification (2026-02-28, partial — 4.5-4.6 pending)

**Files modified:** `inspect_smMatrix.R`, `inspect_mmMatrix.R`, `library.R`, `evaluate_model.R`, `feature_plspredict.R`, `plot_dot.R`, `NAMESPACE`

**4.1 — Numeric column indices eliminated:**
- `inspect_smMatrix.R`: `x[,1]`/`x[,2]` → `x[, "source"]`/`x[, "target"]` in 4 functions
- `inspect_mmMatrix.R`: `x[,1]`/`x[,2]` → `x[, "construct"]`/`x[, "measurement"]` in 2 functions
- Added `construct_names.mmMatrix` S3 method + column-aware `.default` fallback (exposed by `estimate_cfa` passing mmMatrix as `measurement_model`)
- `as.reflective.matrix()`: temporary defensive `colnames()` assignment (proper fix: assign names at matrix creation time)

**4.2 — Dead code deleted:**
- `items_per_mode` and `mmMatrix_per_construct` removed from `library.R`

**4.3 — No remaining duplicate accessors** (resolved by Phase 2D S3 unification)

**4.4 — File consolidation:**

Moves into `inspect_smMatrix.R`:
- `has_direct_effects` (from `evaluate_model.R`)
- `are_construct_names_valid` (from `evaluate_model.R`)
- `construct_order` (from `feature_plspredict.R`)
- `depends_on` → `construct_antecedents_all` (from `feature_plspredict.R`, renamed)
- `antecedents_in_list` → `have_antecedents_in` (from `feature_plspredict.R`, renamed)
- `subset_by_construct` (from `feature_plspredict.R`, internal helper)
- `construct_antecedent_in_list` (from `feature_plspredict.R`, internal helper)

Moves into `inspect_mmMatrix.R`:
- `construct_mode` (from `library.R`)
- `construct_mode_fn` (from `library.R`)
- `are_indicators_in_data` (from `evaluate_model.R`)

Moves into `library.R`:
- `construct_type` (from `plot_dot.R`)

Stayed in place:
- `is_only_endogenous` — stays in `plot_dot.R` (only used there, coupled to `extract_mm_coding`)

Decision: No `inspect_model.R` created — model-level accessors kept in `library.R`.

**4.5 — Final test suite:** 294 PASS, 0 FAIL (run after 4.4 consolidation)

**4.6 — Grep audit for remaining direct matrix access:**

Fixed: 4 numeric-index patterns in `plot_dot.R:extract_sm_edges()` — `sm[i, 1]`/`sm[i, 2]` → `sm[i, "source"]`/`sm[i, "target"]`

Audit confirmed no remaining direct matrix access outside the encapsulation layer, with three documented exceptions (all use named columns):
- `estimate_cbsem.R:125` — one-off CBSEM lavaan name mutation
- `estimate_pls_mga.R:67` — MGA data.frame conversion
- `feature_higher_order.R:164` — HOC filter inside HOC-specific function

Test suite: 294 PASS, 0 FAIL

---

## Future Considerations (Second-Wave Refactoring)

After the current immediate refactoring is complete, consider a second-wave pass where call sites that use accessors like `construct_mode()` are examined for whether they really need the raw mode value or would be better served by predicates (e.g., `is_reflective()`, `is_formative()`). Many call sites pattern-match on mode strings (`"C"`, `"B"`, etc.) and could be replaced with more expressive predicate calls. This is out of scope for the current refactoring but worth tracking.

Additionally, patterns like `if (length(construct_items(mmMatrix, i)) == 1)` could be replaced with a dedicated predicate such as `is_single_item_construct(mmMatrix, i)` — or even accept a model object instead of mmMatrix. This would further improve expressiveness and decouple callers from knowing about item counts.

---

Last updated: 2026-02-28 (All phases complete)
