# Helper File Naming Conventions

> **Status:** Decided. Convention: `helpers-{object}.R` organized by data structure.

## Decision

**Convention:** `helpers-{object}.R` — plural `helpers-` prefix (acts as namespace/folder), hyphen-delimited, suffixed by the data structure the file encapsulates.

### Target files

| File | Contents | ~Lines |
|---|---|---|
| `helpers-smMatrix.R` | All smMatrix accessors, selectors, predicates, mutators; `construct_names` S3 generic + methods | ~220 |
| `helpers-mmMatrix.R` | All mmMatrix accessors, selectors, converters, mutators; `construct_items` S3 generic + methods | ~260 |
| `helpers-model.R` | Model-level accessors (`construct_type`, `construct_scores`, `all_factors`, `all_composites`) | ~60 |

### Rationale

- **`helpers-` prefix** — clearly signals "internal support," distinct from the public verb-pipeline (`specify_`, `estimate_`, `evaluate_`, `report_`, `plot_`). Plural form reads as a namespace grouping. Familiar from cSEM's `helper_*` convention.
- **Organized by data structure** — clean ownership boundaries (smMatrix and mmMatrix functions have zero overlap). A developer asking "what can I do with smMatrix?" opens one file. Matches lavaan's `lav_{structure}_utils.R` approach.
- **No category suffix** — at 220-260 lines per file, internal section headers suffice for navigation. Adding `-accessors`/`-mutators` suffixes would fragment S3 generics across too many files.
- **Hyphen delimiter** — consistent with existing `library-*.R` satellite files; distinguishes from underscore-delimited verb-prefix files (`estimate_pls.R`).

### S3 generics span files by design

S3 generics like `construct_items` and `construct_names` have methods dispatching on different object types. Each method lives in the file for its dispatch object:

- `construct_items` generic + `.mmMatrix` method → `helpers-mmMatrix.R`
- `construct_items.pls_model` method → `helpers-model.R`
- `construct_names` generic + `.smMatrix` method → `helpers-smMatrix.R`
- `construct_names.pls_model` method → `helpers-model.R`

This is standard R practice (dplyr, ggplot2 all split methods by dispatch object). Use `@rdname` in roxygen to link methods to the generic's documentation.

### Internal file sections

Each `helpers-*.R` file uses section headers for navigation:

```r
# ── Accessors ──────────────────────────────────────────
# ── Selectors ──────────────────────────────────────────
# ── Predicates ─────────────────────────────────────────
# ── Mutators ───────────────────────────────────────────
# ── Converters ─────────────────────────────────────────
```

## `library.R` Disposition

Split the current 390-line catch-all:

| Current content | Destination | Rationale |
|---|---|---|
| `construct_type`, `construct_scores`, `constructs_in_model`, `all_factors`, `all_composites` | `helpers-model.R` | Model-level accessors/selectors |
| `path_factorial`, `path_weighting`, `mode_A`, `mode_B`, `mode_plsc`, `unit_weights` | `estimate_weights.R` (new) | Exported weighting schemes + modes |
| `calculate_loadings`, `estimate_path_coef`, `standardize_outer_weights`, `adjust_interaction` | `estimate_weights.R` (new) | Internal estimation helpers |
| `total_effects`, `total_indirect_effects`, `error_cov_matrix` | `evaluate_effects.R` (exists) or `compute_effects.R` | Effects calculation |
| `measure_interaction` | `specify_interactions.R` | Interaction-specific |
| `conf_int`, `kurt`, `skew`, `desc`, `mult`, `name_items`, `convert_to_table_output` | `compute_stats.R` (new) or small `utils.R` | Statistical utilities |
| `return_only_composite_scores` | `helpers-model.R` | Score extraction helper |

### Satellite `library-*.R` files

| File | Destination |
|---|---|
| `library_data_structures.R` | Consumer file or `utils.R` |
| `library_parallel.R` | `estimate_bootstrap.R` or `feature_plspredict.R` |
| `library-errors-warnings.R` | `plot_dot.R` (only consumer) |
| `library-references.R` | Remove or move to dev utilities |
| `library-traverse.R` | `helpers-model.R` |

## Placement Rules

1. **Feature-specific helpers: co-locate.** If a function is only used within one feature file, keep it there (e.g., HOC helpers stay in `feature_higher_order.R`).
2. **Shared data-structure helpers: centralize.** If a function operates on mmMatrix/smMatrix and is called from 2+ files, it goes in the corresponding `helpers-*.R` file.
3. **Catch-all prevention:** the `helpers-{object}` suffix constrains what belongs. A function goes in `helpers-smMatrix.R` only if smMatrix is its primary argument.

## Implementation Checklist

- [ ] Rename `inspect_smMatrix.R` → `helpers-smMatrix.R`
- [ ] Rename `inspect_mmMatrix.R` → `helpers-mmMatrix.R`
- [ ] Create `helpers-model.R` from `library.R` model-level functions
- [ ] Split remaining `library.R` content per disposition table
- [ ] Relocate satellite `library-*.R` files
- [ ] Update `CLAUDE.md` § "Key Module Organization" and § "Internal Matrices"
- [ ] Update `CLAUDE.function-naming.md` file references
- [ ] Run full test suite (file renames only — no code changes needed)

---

Last updated: 2026-02-28
