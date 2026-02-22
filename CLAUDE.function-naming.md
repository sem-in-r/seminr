# Function Naming Conventions

## Current Helper Functions

### smMatrix helpers (`inspect_smMatrix.R`)

| Function | Type | Signature | Returns | Convention |
| --- | --- | --- | --- | --- |
| `construct_names` | accessor | `(smMatrix)` | all unique construct names | object_qualifier |
| `all_endogenous` | selector | `(smMatrix)` | target constructs | all_ |
| `all_exogenous` | selector | `(smMatrix)` | source constructs | all_ |
| `only_exogenous` | selector | `(smMatrix)` | constructs that are only sources | only_ |
| `antecedents_of` | accessor | `(outcome, smMatrix)` | sources for a given target | qualifier_prep |
| `interactions_of` | accessor | `(outcome, smMatrix)` | interaction antecedents of a target | qualifier_prep |
| `all_interactions` | selector | `(smMatrix)` | all interaction terms | all_ |

### mmMatrix helpers (`inspect_mmMatrix.R`)

| Function | Type | Signature | Returns | Convention |
| --- | --- | --- | --- | --- |
| `construct_indicators` | accessor | `(construct_name, mmMatrix)` | indicator names for a construct | object_qualifier |
| `all_reflective` | selector | `(mmMatrix, constructs)` | reflective construct names | all_ | **Note:** `constructs` parameter is unused dead code — remove during refactoring |
| `mm2matrix` | converter | `(measurement_model)` | mmMatrix from measurement model list | abbreviation_noun |

### Measurement model list helpers (`inspect_mmMatrix.R`)

These operate on the pre-matrix measurement model list, not mmMatrix:

| Function | Type | Signature | Returns | Convention |
| --- | --- | --- | --- | --- |
| `construct_name` | accessor | `(construct)` | name of a single construct vector | object_qualifier |
| `number_of_items` | computed | `(construct)` | count of items in a construct vector | noun_preposition |
| `construct_items` | accessor | `(construct)` | item names from a construct vector | object_qualifier |
| `all_construct_names` | selector | `(measurement_model)` | all construct names from model list | all_ |
| `all_items` | selector | `(measurement_model)` | all item names from model list | all_ |
| `all_loc_non_int_items` | selector | `(measurement_model)` | items excluding HOC/interactions | all_ |
| `mm_constructs` | selector | `(measurement_model)` | constructs (excluding interactions) | abbreviation_noun |
| `loc_constructs` | selector | `(measurement_model)` | constructs (excluding HOC) | abbreviation_noun |
| `mm_interactions` | selector | `(measurement_model)` | interaction closures only | abbreviation_noun |

### mmMatrix helpers (`library.R`)

| Function | Type | Signature | Returns | Convention |
| --- | --- | --- | --- | --- |
| `measure_mode` | accessor | `(construct, mmMatrix)` | mode letter ("A", "B", "C", ...) | noun_noun |
| `get_measure_mode` | accessor | `(construct, mmMatrix)` | mode function (mode_A, mode_B, ...) | verb_noun |
| `items_per_mode` | accessor | `(construct, mode, mmMatrix)` | items matching a specific mode | noun_preposition |
| `mmMatrix_per_construct` | accessor | `(construct, mmMatrix)` | mmMatrix subset for one construct | noun_preposition |
| `items_of_construct` | accessor | `(construct, model)` | item names (duplicate of `construct_indicators`) | noun_preposition |
| `constructs_in_model` | accessor | `(model)` | list: names, types, scores | noun_preposition |

### Model-level helpers (`library.R`)

| Function | Type | Signature | Returns | Convention |
| --- | --- | --- | --- | --- |
| `get_factors` | selector | `(seminr_model)` | reflective construct names | verb_noun |
| `get_composites` | selector | `(seminr_model)` | composite construct names | verb_noun |

### HOC-specific helpers (`feature_higher_order.R`)

| Function | Type | Signature | Returns | Convention |
| --- | --- | --- | --- | --- |
| `HOCs_in_model` | selector | `(measurement_model, structural_model)` | HOC constructs | noun_preposition |
| `substitute_dimensions_for_HOC` | mutator | `(construct, sm, mm)` | updated sm + dimensions | verb_noun |
| `remove_HOC_in_measurement_model` | mutator | `(construct, mm)` | mm without HOC rows | verb_noun |

### Validation helpers (`evaluate_model.R`)

| Function | Type | Signature | Returns | Convention |
| --- | --- | --- | --- | --- |
| `direct_effects_are_specified` | predicate | `(smMatrix)` | logical | sentence_predicate |
| `all_indicator_names_are_in_data` | predicate | `(measurement_model, data)` | logical | sentence_predicate |
| `construct_names_are_valid` | predicate | `(measurement_model, structural_model)` | logical | sentence_predicate |

### Plot helpers (`plot_dot.R`)

| Function | Type | Signature | Returns | Convention |
| --- | --- | --- | --- | --- |
| `get_construct_type` | accessor | `(model, construct)` | measurement type string | verb_noun_noun |
| `is_sink` | predicate | `(model, index)` | logical | is_ |

### Prediction helpers (`feature_plspredict.R`)

| Function | Type | Signature | Returns | Convention |
| --- | --- | --- | --- | --- |
| `construct_order` | computed | `(smMatrix)` | ordered construct names | object_qualifier |
| `depends_on` | accessor | `(constructs_vector, smMatrix)` | character vector of unique antecedent names | verb_preposition |
| `antecedents_in_list` | predicate | `(constructs_vector, list, smMatrix)` | logical vector | noun_preposition |

---

## Proposed Conventions

Six function categories, each with its own naming convention:

| Category | Convention | Returns | Example |
| --- | --- | --- | --- |
| **Accessor** | `object_qualifier` | property value | `construct_items(mm, name)`, `construct_mode(mm, name)` |
| **Computed** | object_computation | derived value | `item_count(construct)`, `construct_order(smMatrix)` |
| **Selector** | `all_` / `only_` + adjective | set of objects | `all_endogenous(smMatrix)` |
| **Predicate** | `is_` / `any_` / `are_all_` / `has_` + adjective | logical | `is_reflective(construct, mmMatrix)` |
| **Converter** | verb_noun or `to_`/`as.` | transformed representation | `mm2matrix(measurement_model)` |
| **Mutator** | verb_noun | modified structure | `remove_HOC(construct, mm)` |

### Accessor conventions

Accessors retrieve a property from an object. Named as `object_qualifier` — the domain object comes first, followed by what's being retrieved (e.g., `construct_items`, `construct_mode`, `construct_antecedents`).

**Container-first argument order**: Place the container (mmMatrix, smMatrix, model) as the first argument. This enables both S3 dispatch on the container type and natural piping (`mmMatrix |> construct_items("name")`).

Any accessor can be an S3 generic. Create S3 generics preemptively when a function unifies multiple implementations, even if only one method exists initially.

### Selector conventions

Selectors return **sets of objects** matching a property. Use `all_` or `only_` prefix + adjective.

- `all_` — all objects matching a property
- `only_` — objects that exclusively match (not in any other role)

Selectors are also candidates for S3 dispatch (e.g., `all_endogenous(smMatrix)` vs `all_endogenous(model)`).

### Predicate conventions

Predicates return **logical values** testing whether a property holds.

- `is_` — tests a single object
- `are_all_` — tests whether all objects in a collection have a property
- `any_` — tests whether any object in a collection has a property
- `has_` — tests whether an object possesses a feature
- `have_` — vectorized test on a collection of objects: does each have a property? (returns logical vector)

### Distinguishing selectors from predicates

| Intent | Convention | Example | Returns |
| --- | --- | --- | --- |
| Get the set | `all_` + adjective | `all_reflective(mmMatrix)` | character vector |
| Test one object | `is_` + adjective | `is_reflective(construct, mmMatrix)` | logical scalar |
| Test existence | `any_` + adjective | `any_reflective(mmMatrix)` | logical scalar |
| Test universality | `are_all_` + adjective | `are_all_reflective(mmMatrix)` | logical scalar |

---

## Proposed Renames

### Accessors (`object_qualifier`)

Named as `object_qualifier` with container-first argument order. Any can be an S3 generic.

| Current name | Proposed name | Object(s) | What it does |
| --- | --- | --- | --- |
| `construct_name` | `construct_name` | construct vector | name of a construct vector |
| `construct_names` | `construct_names` | smMatrix | all construct names |
| `all_construct_names` | `construct_names` | measurement_model list | all construct names |
| `construct_indicators` | `construct_items` | mmMatrix | items for a construct |
| `construct_items` | `construct_items` | construct vector | items from a construct vector |
| `items_of_construct` | `construct_items` | model | items for a construct |
| `all_items` | `construct_items` | measurement_model list | all item names |
| `measure_mode` | `construct_mode` | mmMatrix | estimation mode letter ("A", "B", "C", "HOCA", "HOCB", "UNIT") |
| `get_measure_mode` | `construct_mode_fn` | mmMatrix | estimation mode function (mode_A, mode_B, unit_weights) |
| `get_construct_type` | `construct_type` | model | user-facing measurement type ("reflective", "composite", "interaction") |
| `constructs_in_model` | **split** (see below) | model | names, types, scores from model |
| `antecedents_of` | `construct_antecedents` | smMatrix | sources for a given target |
| `depends_on` | `construct_antecedents_all` | smMatrix | all unique antecedents of a set of constructs |
| `interactions_of` | `construct_interactions` | smMatrix | interaction antecedents of a target |
| (new) | `construct_targets` | smMatrix | targets of a given source construct |
| (new) | `construct_of_item` | mmMatrix | reverse lookup: construct containing a given item |
| (new) | `construct_scores` | model | estimated construct score matrix (handles HOC first-stage merging) |

### Computed (`object_computation`)

Named as `object_computation` — returns a derived value, not a direct property. E.g., `item_count`, `construct_order`.

| Current name | Proposed name | Object(s) | What it does |
| --- | --- | --- | --- |
| `number_of_items` | `item_count` | construct vector | item count of a construct vector |
| `construct_order` | `construct_order` | smMatrix | topological ordering of constructs |

### Selectors (`all_` / `only_`)

Prefixed with `all_` (matching a property) or `only_` (exclusively matching). Return sets of objects as character vectors.

| Current name | Proposed name | Object(s) | What it does |
| --- | --- | --- | --- |
| `all_endogenous` | `all_endogenous` | smMatrix | target constructs |
| `all_exogenous` | `all_exogenous` | smMatrix | source constructs |
| `only_exogenous` | `only_exogenous` | smMatrix | constructs that are only sources |
| `all_interactions` | `all_interactions` | smMatrix | all interaction terms |
| `all_reflective` | `all_reflective` | mmMatrix | reflective construct names | **Note:** remove unused `constructs` parameter during refactoring |
| (new) | `all_constructs` | mmMatrix | all unique construct names |
| (new) | `all_constructs_of_mode` | mmMatrix | constructs matching a given estimation mode |
| `all_loc_non_int_items` | `all_LOC_items` | measurement_model list | items excluding HOC/interactions |
| `mm_constructs` | `all_non_interactions` | measurement_model list | constructs excluding interactions |
| `loc_constructs` | `all_LOCs` | measurement_model list | constructs excluding HOC |
| `mm_interactions` | `all_interaction_fns` | measurement_model list | interaction closures only |
| `get_factors` | `all_factors` | model | reflective construct names |
| `get_composites` | `all_composites` | model | composite construct names |
| `HOCs_in_model` | `all_HOCs` | measurement_model, structural_model | HOC constructs |

### Predicates (`is_` / `has_` / `are_all_`)

Prefixed with `is_` (single object), `has_` (feature presence), or `are_all_` (universality). Return logical values.

| Current name | Proposed name | Object(s) | What it does |
| --- | --- | --- | --- |
| `direct_effects_are_specified` | `has_direct_effects` | smMatrix | checks if direct effects exist |
| `all_indicator_names_are_in_data` | `are_indicators_in_data` | measurement_model, data | checks all indicators exist in data |
| `construct_names_are_valid` | `are_construct_names_valid` | measurement_model, structural_model | checks construct names are valid |
| `is_sink` | `is_only_endogenous` | model, index | checks if construct is purely endogenous (never a source) |
| `antecedents_in_list` | `have_antecedents_in` | constructs_vector, list, smMatrix | per-construct check: are antecedents in list? |

### Converters (keep as-is)

Transform one representation into another. No renames — only one converter exists.

| Current name | Proposed name | Object(s) | What it does |
| --- | --- | --- | --- |
| `mm2matrix` | `mm2matrix` | measurement_model list | converts measurement model list to mmMatrix |

### Mutators (defer renames to Phase 3)

Named as `verb_noun` (e.g., `remove_HOC`). Return modified structures. Renames deferred until write-side encapsulation.

| Current name | Proposed name | Object(s) | What it does |
| --- | --- | --- | --- |
| `substitute_dimensions_for_HOC` | `expand_HOC_to_LOCs` | construct, sm, mm | replaces HOC with dimensions in sm |
| `remove_HOC_in_measurement_model` | `remove_HOC` | construct, mm | removes HOC rows from mm |

### Dead code (delete after refactoring call sites)

| Current name | Object(s) | Why | Refactor to |
| --- | --- | --- | --- |
| `items_per_mode` | mmMatrix | never called, never tested | — |
| `mmMatrix_per_construct` | mmMatrix | leaks internal row structure; callers only need item count or mode | `length(construct_items(mmMatrix, name))` for count; `construct_mode(mmMatrix, name)` for mode |

### Accessor S3 dispatch

Any accessor can be an S3 generic regardless of naming pattern. The following accessors unify multiple current functions and should be created as S3 generics preemptively, even if only one method exists initially.

**`construct_items`:**

| Method | Dispatches on | Current function |
| --- | --- | --- |
| `construct_items.mmMatrix` | mmMatrix | `construct_indicators()` |
| `construct_items.construct` | construct vector | `construct_items()` |
| `construct_items.pls_model` | model object | `items_of_construct()` |
| `construct_items.measurement_model` | measurement_model list | `all_items()` |

**`construct_names`:**

| Method | Dispatches on | Current function |
| --- | --- | --- |
| `construct_names.smMatrix` | smMatrix | `construct_names()` |
| `construct_names.measurement_model` | measurement model list | `all_construct_names()` |
| `construct_names.pls_model` | model object | `constructs_in_model()$construct_names` logic |

### Split: `constructs_in_model` → spec-level accessors + `construct_scores`

`constructs_in_model(model)` returns a bundle of three fields (`$construct_names`, `$construct_types`, `$construct_scores`), but most callers only need the spec-level fields:

| Caller | Fields used | Spec-level? |
| --- | --- | --- |
| `reliability()` | `$construct_names` | Yes |
| `feature_consistent.R` | `$construct_names` | Yes |
| `extract_mm_coding()` (plot) | `$construct_names` + `$construct_types` | Yes |
| `validity()` → `cross_loadings()` | `$construct_scores` | No |
| `validity()` → `item_vifs()`, `fl_criteria_table()` | passed as bundle | Mixed |

Construct names and types are known at specification time — they come from the measurement model list and smMatrix. Only `construct_scores` is truly estimation output. The function conflates spec-level queries with estimation results.

**Resolution:** Split into separate accessors:

- **`construct_names(model)`** — spec-level; S3 generic already planned (dispatches on smMatrix, measurement_model list, and model objects). HOC logic (merging first-stage constructs) belongs here.
- **`construct_type(model, name)`** — spec-level; already planned as accessor. Returns `"reflective"`, `"composite"`, or `"interaction"`.
- **`construct_scores(model)`** — estimation-level; new accessor on estimated models only. Handles HOC first-stage score merging.

Non-score callers (`reliability`, `feature_consistent`, `extract_mm_coding`) should migrate to the spec-level accessors. `validity()` sub-functions should accept scores directly rather than receiving the bundle.

---

## Resolved Questions

- ~Should S3 generics be created preemptively?~ → **Yes**, create preemptively when a function unifies multiple implementations
- ~Which selectors/predicates should get S3 dispatch now vs. later?~ → **Not now.** Selectors/predicates stay as plain functions. Revisit only if a real use case emerges.
- ~Should converters follow a single convention?~ → **Keep `mm2matrix` as-is.** It's internal, there's only one converter, and renaming adds churn with zero benefit. Establish a convention if more converters appear later.
- ~Should mutators follow a uniform convention?~ → **Yes, adopt `verb_noun`**, but defer actual renames to Phase 3 (write-side encapsulation). E.g., `substitute_dimensions_for_HOC` → `expand_HOC_to_LOCs`; `remove_HOC_in_measurement_model` → `remove_HOC`.

---

Last updated: 2026-02-27
