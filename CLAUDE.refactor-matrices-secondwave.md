# Second Wave Refactoring: mmMatrix and smMatrix

> Follow-on work identified during the first-wave encapsulation refactoring on branch `ray/refactor-model-matrices`. These items are independent of the completed first-wave phases and can be tackled in any order.

## Related Documents

- `CLAUDE.ray-refactor-model-matrices.md` — First-wave refactoring plan (Phases 1-5, all complete)
- `CLAUDE.function-naming.md` — Function naming conventions, proposed renames, S3 dispatch plan
- `CLAUDE.file-naming.md` — Helper file naming conventions (decided: `helpers-{object}.R`)
- `CLAUDE.matrix-access-patterns.md` — Exhaustive catalog of all 16 distinct access patterns, stability analysis, helper adoption status

## Context

The first-wave refactoring replaced ~175 raw `matrix[row, col]` access sites across ~48 files with accessor/mutator functions. All phases are complete (294 tests passing). Three documented exceptions remain (all use named columns):
- `estimate_cbsem.R:125` — one-off CBSEM lavaan name mutation
- `estimate_pls_mga.R:67` — MGA data.frame conversion
- `feature_higher_order.R:164` — HOC filter inside HOC-specific function

Current accessor naming conventions: `object_qualifier` for accessors, `all_`/`only_` for selectors, `is_`/`has_`/`are_all_` for predicates, `verb_noun` for mutators. Container-first argument order throughout. See `CLAUDE.function-naming.md` for the full scheme.

---

## Items

### 1. Predicate expressiveness

Many call sites that use accessors like `construct_mode()` pattern-match on raw mode strings (`"C"`, `"B"`, etc.) when they really just need a boolean answer. A second-wave pass could replace these with more expressive predicates:

- `construct_mode(mmMatrix, x) == "C"` → `is_reflective(mmMatrix, x)`
- `construct_mode(mmMatrix, x) == "B"` → `is_formative(mmMatrix, x)`
- Similar for `"A"`, `"HOCA"`, `"HOCB"`, `"UNIT"` modes

Additionally, patterns like `if (length(construct_items(mmMatrix, i)) == 1)` could be replaced with a dedicated predicate such as `is_single_item_construct(mmMatrix, i)` — or even accept a model object instead of mmMatrix. This would further improve expressiveness and decouple callers from knowing about item counts.

### 2. Helper file naming conventions ✓

> **Status:** Complete. Convention decided and file renames implemented. See `CLAUDE.file-naming.md` for the full convention and implementation checklist.

**Convention:** `helpers-{object}.R` — plural `helpers-` prefix, hyphen-delimited, suffixed by the data structure the file encapsulates.

**Completed renames:**

- `inspect_smMatrix.R` → `helpers-smMatrix.R`
- `inspect_mmMatrix.R` → `helpers-mmMatrix.R`
- Created `helpers-model.R` from model-level functions in `library.R`

### 3. Convention storage for AI-assisted development

The naming conventions in `CLAUDE.function-naming.md` and accessor catalog in `CLAUDE.matrix-access-patterns.md` were invaluable during the first-wave refactoring — they kept every session consistent despite context resets. But these are branch-local planning documents that won't persist into `master`. The question: how should these conventions be made available to future Claude sessions working on this codebase?

- **Options considered:**
  - **Claude Code skill:** A `/function-naming` skill that loads the naming conventions on demand. Keeps conventions out of the main CLAUDE.md (which is already long) while making them accessible in any session. Skills are stored in `.claude/skills/` and can reference external files.
  - **Merge into CLAUDE.md:** Add a condensed conventions section directly to CLAUDE.md. Simplest approach, but CLAUDE.md is already substantial and conventions would add significant bulk. Risk of the section becoming stale if it's buried in a long file.
  - **Separate checked-in CLAUDE file:** Keep `CLAUDE.function-naming.md` as a checked-in file (currently it's `.local.md` and gitignored). Referenced from CLAUDE.md via `@CLAUDE.function-naming.md`. Keeps conventions accessible but separate.
  - **Code-level documentation:** Embed conventions as comments in the helper files themselves (e.g., a header block in `helpers-mmMatrix.R`). Self-documenting but only visible when reading those specific files.
- **Open questions:**
  - Is a skill the right mechanism, or is it overkill for a reference document?
  - Should the conventions be prescriptive (enforced by the skill's prompt) or descriptive (a reference to consult)?
  - Which conventions are branch-specific vs. project-wide? The naming scheme (`object_qualifier`, `all_`, `is_`, etc.) is project-wide, but the specific function catalog is branch-specific until merged.

---

Last updated: 2026-02-28
