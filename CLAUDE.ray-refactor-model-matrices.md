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
- [ ] Phase 1: smMatrix read-side encapsulation
- [ ] Phase 2: mmMatrix read-side encapsulation
- [ ] Phase 3: Write-side encapsulation
- [ ] Phase 4: Cleanup and verification

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

## Scope

**In scope:** Create missing accessors, replace all direct mmMatrix/smMatrix subsetting in R/ and test files, consolidate duplicates, rename per conventions.

**Out of scope:** Changing the underlying matrix representation, user-facing API changes, S3 class hierarchy changes, R6/reference semantics.

## Tasks

### Phase 1: Baseline and smMatrix Read-Side (Low Risk)

smMatrix has fewer access points (~75) and existing helpers cover most patterns — good warmup.

> **Test-first**: Run full test suite before and after each batch of changes.

- [ ] 1.1 Run full test suite, confirm all tests pass (baseline)
- [ ] 1.2 Replace direct smMatrix reads with existing helpers across R/ files
  - `all_endogenous`, `antecedents_of`, `construct_names`, `only_exogenous`, `all_exogenous`
- [ ] 1.3 Add missing smMatrix helpers
  - `construct_targets(smMatrix, source)` for `smMatrix[smMatrix[,"source"]==x, "target"]`
- [ ] 1.4 Rename smMatrix accessors per naming conventions
  - `antecedents_of` → `construct_antecedents`
  - `interactions_of` → `construct_interactions`
  - **Reorder arguments to container-first**: `construct_antecedents(smMatrix, target)`, `construct_interactions(smMatrix, target)`
  - See `CLAUDE.function-naming.md` § "Proposed Renames" for full list
- [ ] 1.5 Rename smMatrix predicates per naming conventions
  - `direct_effects_are_specified` → `has_direct_effects`
  - `construct_names_are_valid` → `are_construct_names_valid`
- [ ] 1.6 Run full test suite, confirm all tests still pass

### Phase 2: mmMatrix Read-Side (Main Effort)

mmMatrix has ~100+ direct access points across ~28 R source files and ~34 test files.

#### 2A. Create new accessors

- [ ] 2.1a Write tests for new mmMatrix accessor functions
- [ ] 2.1b Create missing helpers
  - `all_constructs(mmMatrix)` — wraps `unique(mmMatrix[,"construct"])`
  - `all_constructs_of_mode(mmMatrix, mode)` — wraps mode-column filtering (selector convention)
  - `construct_of_item(item, mmMatrix)` — reverse item lookup (~3 sites)
  - Others as identified from tail patterns in `CLAUDE.matrix-access-patterns.md`
- [ ] 2.1c Split `constructs_in_model()` into:
  - `construct_names(model)` — S3 method on model (spec-level, handles HOC)
  - `construct_type(model, name)` — spec-level accessor
  - `construct_scores(model)` — estimation-level accessor (handles HOC first-stage score merging)
  - See `CLAUDE.function-naming.md` § "Split: `constructs_in_model`"

#### 2B. Replace raw access with accessors

- [ ] 2.2 Replace `construct_indicators()` pattern (~30 sites)
- [ ] 2.3 Replace `construct_type()` pattern (~10 sites)
- [ ] 2.4 Replace `all_constructs()` pattern (~6 sites)
- [ ] 2.5 Replace remaining read patterns (type filtering, SM membership filter, HOC-specific, etc.)

#### 2C. Rename per conventions

All renames listed in `CLAUDE.function-naming.md` § "Proposed Renames". Key groups:

- [ ] 2.6 mmMatrix helpers: `construct_indicators` → `construct_items`, `measure_mode` → `construct_mode`, `get_measure_mode` → `construct_mode_fn`, `get_construct_type` → `construct_type`, `all_indicator_names_are_in_data` → `are_indicators_in_data`, `is_sink` → `is_only_endogenous`
  - **Reorder arguments to container-first**: `construct_items(mmMatrix, name)`, `construct_mode(mmMatrix, name)`, `construct_mode_fn(mmMatrix, name)`, `construct_type(model, name)` (already container-first for model), etc.
- [ ] 2.7 Model-level selectors: `get_factors` → `all_factors`, `get_composites` → `all_composites`, `HOCs_in_model` → `all_HOCs`
- [ ] 2.8 Measurement model list helpers: `all_construct_names` → `construct_names` (S3), `all_items` → `construct_items` (S3), `all_loc_non_int_items` → `all_LOC_items`, `mm_constructs` → `all_non_interactions`, `loc_constructs` → `all_LOCs`, `mm_interactions` → `all_interaction_fns`, `number_of_items` → `item_count`

#### 2D. S3 generics and optimizations

- [ ] 2.9 Create S3 generics for `construct_items` and `construct_names`
  - See `CLAUDE.function-naming.md` § "Accessor S3 dispatch" for dispatch table
- [ ] 2.10 Precompute mappings in `simplePLS()` before PLS loop (~5 lines, eliminates ~14 per-iteration lookups)
- [ ] 2.11 Fix `get_measure_mode()` redundancy (6 identical lookups → extract type once)
- [ ] 2.12 Run full test suite, confirm all tests still pass

### Phase 3: Write-Side Encapsulation

~15 write sites: matrix creation (`mm2matrix`, `matrix()`), row appending (`rbind`), indirect writes (mmMatrix as index into other matrices), local copies.

- [ ] 3.1 Audit all write sites and design builder/mutator API
- [ ] 3.2 Implement write-side functions
- [ ] 3.3 Replace write-site call sites
- [ ] 3.4 Rename mutators: `substitute_dimensions_for_HOC` → `expand_HOC_to_LOCs`, `remove_HOC_in_measurement_model` → `remove_HOC`
  - See `CLAUDE.function-naming.md` § "Mutators"
- [ ] 3.5 Run full test suite, confirm all tests still pass

### Phase 4: Cleanup and Verification

- [ ] 4.1 Eliminate numeric column indices inside accessor functions (use named only)
- [ ] 4.2 Delete dead code: `items_per_mode` (never called), `mmMatrix_per_construct` (leaks row structure)
  - See `CLAUDE.function-naming.md` § "Dead code"
- [ ] 4.3 Consolidate any remaining duplicate accessors
- [ ] 4.4 Final full test suite run
- [ ] 4.5 Verify no remaining direct matrix access patterns (grep audit)

## Completed

(none yet)

---

Last updated: 2026-02-27
