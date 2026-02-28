# Second Wave Refactoring: mmMatrix and smMatrix

> Follow-on work identified during the first-wave encapsulation refactoring on branch `ray/refactor-model-matrices`. These items are independent of the completed first-wave phases and can be tackled in any order.

## Related Documents

- `CLAUDE.ray-refactor-model-matrices.md` — First-wave refactoring plan (Phases 1-5, all complete)
- `CLAUDE.function-naming.md` — Function naming conventions, proposed renames, S3 dispatch plan
- `CLAUDE.file-naming.md` — Helper file naming conventions (decided: `helpers-{object}.R`)
- `CLAUDE.matrix-access-patterns.md` — Exhaustive catalog of all 16 distinct access patterns, stability analysis, helper adoption status

## Context

The first-wave refactoring replaced ~175 raw `matrix[row, col]` access sites across ~48 files with accessor/mutator functions. All phases are complete (298 tests passing). One documented exception remains:
- `estimate_cbsem.R:125` — one-off CBSEM lavaan name mutation (uses named columns)

Current accessor naming conventions: `object_qualifier` for accessors, `all_`/`only_` for selectors, `is_`/`has_`/`are_all_` for predicates, `verb_noun` for mutators. Container-first argument order throughout. See `CLAUDE.function-naming.md` for the full scheme.

---

## Status Summary

| # | Item | Status |
| --- | --- | --- |
| 1 | Predicate expressiveness | ✓ Complete |
| 2 | Helper file naming conventions | ✓ Complete |
| 3 | Convention storage for AI-assisted development | Planned |

---

## Items

### 1. Predicate expressiveness ✓

> **Status:** Complete. See `CLAUDE.refactor-matrices-expressiveness.md` for the full plan and implementation details.

**What was done:**

- **Predicates added to `helpers-mmMatrix.R`:** `is_reflective`, `is_LOC_A`, `is_LOC_B`, `is_HOC_A`, `is_HOC_B`, `is_unit_weighted`, `is_mode_A`, `is_mode_B`, `is_HOC`, `is_single_item`
- **Compound selectors added to `helpers-mmMatrix.R`:** `all_HOC`, `all_LOC`, `all_items`, `mmMatrix_for_items`
- **smMatrix row-level accessors added to `helpers-smMatrix.R`:** `path_sources`, `path_targets`, `to_path_labels`
- **Call-site replacements:** Raw mode-string comparisons replaced across `evaluate_warnings.R`, `lavaan_syntax.R`, `evaluate_reliability.R`, `helpers-mmMatrix.R`, `helpers-model.R`, `plot_dot.R`
- **smMatrix refactors:** `apply`-based path iteration replaced with `mapply` + accessors in `boot_utils.R` and `estimate_pls_mga.R`
- **Dead code removed:** Commented-out raw-access code deleted from `evaluate_warnings.R`, `evaluate_validity.R`, `feature_higher_order.R`, `evaluate_effects.R`
- **Always-true guards removed:** Three `rhoC_AVE` method bodies simplified

**Remaining (deferred to plot-internal refactoring):** `plot_dot.R` lines using `mm_coding` and `construct_type` mode strings (lines 1211, 1345-1351, 1443-1449). One additional raw-mode site in `helpers-model.R:return_only_composite_scores` noted for future cleanup.

### 2. Helper file naming conventions ✓

> **Status:** Complete. Convention decided and file renames implemented. See `CLAUDE.file-naming.md` for the full convention and implementation checklist.

**Convention:** `helpers-{object}.R` — plural `helpers-` prefix, hyphen-delimited, suffixed by the data structure the file encapsulates.

**Completed renames:**

- `inspect_smMatrix.R` → `helpers-smMatrix.R`
- `inspect_mmMatrix.R` → `helpers-mmMatrix.R`
- Created `helpers-model.R` from model-level functions in `library.R`

### 3. Convention storage for AI-assisted development — Pending

The naming conventions in `CLAUDE.function-naming.md` and accessor catalog in `CLAUDE.matrix-access-patterns.md` were invaluable during the first-wave refactoring — they kept every session consistent despite context resets. But these are branch-local planning documents that won't persist into `master`. The question: how should these conventions be made available to future Claude sessions working on this codebase?

#### Decision: Brief CLAUDE.md policy + code-level comments in helper files

**Options evaluated** (via structured debate between three advocate agents):

1. **Claude Code skill** — Rejected. Overkill for a reference document; conventions should be always-available, not on-demand.
2. **Merge into CLAUDE.md** — Rejected as primary home. CLAUDE.md is already ~200 lines; adding the full catalog would dilute it. However, a brief policy statement belongs there.
3. **Separate checked-in CLAUDE file** — Rejected. Claude Code does NOT auto-load `CLAUDE*.md` by glob pattern; only `CLAUDE.md` and `CLAUDE.local.md` are auto-loaded. A separate file would require an `@` import reference in CLAUDE.md, creating a dependency that breaks if the reference is removed. Also loads the full ~300-line catalog into every session's context regardless of relevance.
4. **Code-level documentation** — **Selected.** Conventions documented as comments in the helper files where they apply. Overview blocks at the top of each file; naming patterns in category separator comments.

**Rationale:** The conventions govern how functions in `helpers-mmMatrix.R`, `helpers-smMatrix.R`, and `helpers-model.R` are named and organized. Code-level comments are guaranteed to be in context exactly when CC is editing these files. CLAUDE.md provides the policy ("use helpers, no raw access") that directs CC to open the right files in the first place.

#### Implementation Plan

Mark each step `[x]` as completed. If context is cleared, resume from the first unchecked step.

##### Phase A: Condense CLAUDE.md "Internal Matrices" section

Replace the current section (lines 113-146) with a ~15-line policy statement:

- [ ] **A1.** Keep matrix descriptions (mmMatrix columns, smMatrix columns)
- [ ] **A2.** Keep the rule: always use helper functions, never raw `matrix[row, col]` subsetting
- [ ] **A3.** Note that `seminr_model$field` member access is permitted (e.g., `model$mmMatrix`, `model$construct_scores`)
- [ ] **A4.** List the three helper files and what each covers: `helpers-mmMatrix.R` (mmMatrix accessors, selectors, predicates, converters, mutators; `construct_items` S3 generic), `helpers-smMatrix.R` (smMatrix accessors, selectors, predicates, mutators; `construct_names` S3 generic), `helpers-model.R` (model-level accessors and selectors; S3 methods dispatching on `seminr_model`)
- [ ] **A5.** State that helpers are organized into categories (accessors, selectors, predicates, mutators, converters) with naming conventions documented in comments at the top of each helper file
- [ ] **A6.** Remove the "Naming conventions at a glance" table (lines 135-143)
- [ ] **A7.** Remove the "Key S3 generics" detail (lines 130-133)
- [ ] **A8.** Remove the reference to `CLAUDE.function-naming.md` (line 146)

##### Phase B: Add overview comment blocks to helper files

Expand the existing 2-line "Purpose" comment at the top of each file into a ~15-line overview:

- [ ] **B1.** `R/helpers-mmMatrix.R` — Add overview block listing: file purpose, naming conventions used (accessor: `object_qualifier`; selector: `all_`/`only_`; predicate: `is_`/`has_`/`are_`; mutator: `verb_noun`; converter: `to_`/`as.`/abbreviation), container-first argument order rule, cross-reference to other `helpers-*.R` files
- [ ] **B2.** `R/helpers-smMatrix.R` — Same structure, scoped to smMatrix categories
- [ ] **B3.** `R/helpers-model.R` — Same structure, scoped to model-level categories

##### Phase C: Expand category separator comments

Each file already has separators like `# -- Accessors ---`. Expand each to include the naming pattern as a brief parenthetical:

- [ ] **C1.** `R/helpers-mmMatrix.R` — Update all category separators. Example: `# -- Predicates (is_/has_/are_: return logical) --------`
- [ ] **C2.** `R/helpers-smMatrix.R` — Update all category separators
- [ ] **C3.** `R/helpers-model.R` — Update all category separators

##### Phase D: Verification

- [ ] **D1.** `devtools::test()` — Comments-only changes, should be zero risk, but confirm no regressions
- [ ] **D2.** Verify CLAUDE.md reads cleanly as a brief policy statement
- [ ] **D3.** Verify each helper file's top block gives CC enough context to follow conventions

#### Design Notes

- **What the function catalog becomes:** The detailed ~250-line function catalog from `CLAUDE.function-naming.md` is NOT preserved as a separate document. The catalog is implicitly captured by the code itself — each function has a comment, and the category separators group them. CC reads the file and sees everything.
- **Cross-cutting concern:** Naming conventions apply across all three helper files. Each file's overview block includes the full convention table (it's only ~6 lines), so CC sees the rules regardless of which file it opens first.
- **`CLAUDE.function-naming.md` fate:** It's a `.local.md` gitignored file. It stays on the branch as a historical reference but doesn't go to `master`. No action needed.
- **The `seminr_model$...` exception:** Direct member access on model objects (e.g., `model$mmMatrix`, `model$smMatrix`, `model$construct_scores`) is permitted and common throughout the codebase. The "no raw access" rule applies to the internal structure of mmMatrix and smMatrix themselves, not to accessing them as fields on the model object.

---

Last updated: 2026-02-28 (item 3 plan finalized)
