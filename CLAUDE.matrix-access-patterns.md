# Matrix Access Patterns Catalog

> **Related to:** `CLAUDE.ray-refactor-model-matrices.md` (main refactoring plan)
> **Purpose:** Exhaustive catalog of all distinct access patterns for mmMatrix and smMatrix, with stability analysis

## Summary

- **16 distinct patterns total** (9 mmMatrix, 7 smMatrix)
- **5-6 core patterns** account for >90% of all ~175 access sites
- **Pattern set is closed/stable** — last new pattern shape introduced in 2020
- **New features exclusively reuse existing patterns** (evidence from 5+ years of development)

## mmMatrix Patterns (9 distinct)

### Core Patterns (>90% of access)

| # | Pattern | Description | Count | Files | Existing helper |
|---|---------|-------------|-------|-------|-----------------|
| 1 | `mmMatrix[mmMatrix[,"construct"]==x, "measurement"]` | Get indicators for a construct | ~20 | 9 | `construct_indicators()` — barely adopted (2 call sites) |
| 2 | `mmMatrix[mmMatrix[,"construct"]==x, "type"][1]` | Get type of a construct | ~12 | 3 | `measure_mode()` — partially covers this |
| 3 | `unique(mmMatrix[,1])` / `unique(mmMatrix[,"construct"])` | Get all construct names | ~6 | 5 | None — needs `all_constructs()` |
| 4 | `mmMatrix[which(mmMatrix[,3]=="A"),1]` | Get constructs of a specific type | ~7 | 2 | None — needs `constructs_of_type()` |
| 5 | `"C" %in% mmMatrix[,"type"]` | Check if any reflective constructs exist | 2 | 1 | None |

### Tail Patterns (<10% of access)

| # | Pattern | Description | Count | Files |
|---|---------|-------------|-------|-------|
| 6 | `mmMatrix[mmMatrix[,"construct"]==x, c("measurement","type")]` | Subset of mmMatrix for one construct | ~3 | 1 |
| 7 | `mmMatrix[,2] == item_name` | Reverse lookup: find construct for an item | ~3 | 2 |
| 8 | `mmMatrix[which(!grepl(...) & !(type conditions)),2]` | Filtered items with complex predicate | 1 | 1 |
| 9 | `mmMatrix[,1] %in% construct_names(smMatrix)` | Filter mmMatrix constructs by SM membership | ~3 | 2 |

## smMatrix Patterns (7 distinct)

### Core Patterns (>90% of access)

| # | Pattern | Description | Count | Files | Existing helper |
|---|---------|-------------|-------|-------|-----------------|
| 1 | `smMatrix[smMatrix[,"target"]==x, "source"]` | Antecedents of a target | ~7 | 6 | `antecedents_of()` — moderately adopted |
| 2 | `unique(smMatrix[,"target"])` / `unique(smMatrix[,2])` | All endogenous constructs | ~8 | 5 | `all_endogenous()` — moderately adopted |
| 3 | `unique(smMatrix[,"source"])` / `unique(smMatrix[,1])` | All exogenous constructs | ~3 | 2 | `all_exogenous()` — lightly adopted |
| 4 | `unique(c(smMatrix[,1], smMatrix[,2]))` | All construct names | ~3 | 2 | `construct_names()` — well adopted |
| 5 | `setdiff(unique(smMatrix[,1]), unique(smMatrix[,2]))` | Only-exogenous constructs | ~4 | 2 | `only_exogenous()` — lightly adopted |

### Tail Patterns

| # | Pattern | Description | Count | Files | Existing helper |
|---|---------|-------------|-------|-------|-----------------|
| 6 | `smMatrix[smMatrix[,"source"]==x, "target"]` | Targets of a source | 2 | 2 | None — needs `targets_of()` |
| 7 | `smMatrix[,c("source","target"), drop=F]` | Full matrix as data frame | 1 | 1 | None |


## Related Issues and Bugs (28 issues)

GitHub issues caused by raw matrix access, grouped by affected function. Confidence: **confirmed** = root cause traced in code; **probable** = symptoms match the bug class but not fully traced.

| Area | Confirmed | Probable | Total |
| --- | ---: | ---: | ---: |
| `summary()` | 9 | 5 | 14 |
| `estimate_pls()` | 2 | 4 | 6 |
| `predict_pls()` | 0 | 4 | 4 |
| `bootstrap_model()` | 2 | 0 | 2 |
| `plot()` | 1 | 0 | 1 |
| `total_effects()` | 1 | 0 | 1 |
| **Total** | **15** | **13** | **28** |

### `summary()` (14 issues)

- Confirmed: #142, #161, #214, #247, #271, #286, #289, #341, #377
- Probable: #130, #330, #353, #369, #373

### `estimate_pls()` (6 issues)

- Confirmed: #327, #364
- Probable: #322, #325, #328, #344

### `predict_pls()` (4 issues)

- Probable: #270, #331, #333, #347

### `bootstrap_model()` (2 issues)

- Confirmed: #154, #310

### `plot()` (1 issue)

- Confirmed: #305

### `total_effects()` (1 issue)

- Confirmed: #309

### Reviewed and excluded (3 issues)

- #28, #317, #339 — not matrix access bugs (design decision, user data mismatch, statistical issue)

## Helper Adoption Status

| Accessor | Times called | Raw duplicates still in codebase |
|----------|-------------|----------------------------------|
| `construct_indicators()` | 2 | ~18 |
| `all_endogenous()` | 12 | ~8 |
| `antecedents_of()` | 5 | ~7 |
| `construct_names()` | 10 | ~3 |
| `only_exogenous()` | 1 | ~4 |
| `all_exogenous()` | 3 | ~3 |
| `measure_mode()` | 3 | ~12 |
| `mmMatrix_per_construct()` | 2 | ~3 |

**smMatrix helpers** are moderately adopted. **mmMatrix helpers** are severely under-adopted — `construct_indicators()` has 2 call sites vs. ~18 raw duplicates.

## Adoption by File Age

| Era | Files | Behavior |
|-----|-------|----------|
| Pre-2020 | `library.R`, `estimate_simplePLS.R`, `feature_consistent.R` | Almost exclusively raw access (written before helpers existed) |
| 2020 | `lavaan_syntax.R`, `estimate_cbsem.R` | Mixed — smMatrix helpers used, mmMatrix raw |
| 2022-2025 | Edits to `feature_plspredict.R`, `evaluate_validity.R` | Mixed — newer functions in files use helpers; older functions in same files still raw |

## Pattern Stability Analysis

### Timeline of New Pattern Introduction

| Date | Feature | New pattern? |
|------|---------|-------------|
| 2017-07 | Core PLS engine | Established patterns 1-6 (mm), 1-5 (sm) |
| 2017-11 | PLSc | mm pattern 5 (type existence check) |
| 2019-09 | Higher-order constructs | mm pattern 7 (reverse item lookup) |
| 2020-06 | CBSEM/Lavaan | No — reused existing patterns, used helpers |
| 2020-10 | PLSpredict | sm pattern 6 (`targets_of`) — **last new pattern** |
| 2021-04 | Visualization | mm pattern 9 (mm/sm intersection) — combination of existing |
| 2022-01 | PLS-MGA | sm pattern 7 (minor variant) |
| 2022-05 | Unit weighting | No — extended type values, reused pattern 4 |
| 2025-07 | Predict PLS v2 | No — reused existing patterns |
| 2025-09 | Composite Predict | No — reused existing patterns, moved toward helpers |

### Why Patterns Are Stable

The matrices are fixed-schema lookup tables:
- **mmMatrix**: 3 columns (construct, measurement, type)
- **smMatrix**: 2 columns (source, target)

The useful operations on a 2-3 column lookup table are inherently limited:
- Filter by column X, return column Y
- Get unique values of column X
- Check if value exists in column X
- Combinations of the above

These patterns exhaust the meaningful queries. Growth comes from **new type values** (e.g., "UNIT", "HOCA", "HOCB" added to original "A", "B", "C"), not new access shapes. Parameterized accessors like `constructs_of_type(type, mmMatrix)` naturally absorb this growth.

## Implications for Refactoring Approach

The closed/stable pattern set significantly affects the cost-benefit analysis of the proposed refactoring (see Decision Rationale in `CLAUDE.ray-refactor-model-matrices.md`).

### Full encapsulation is more justified than it initially appeared

The original rationale was representation independence. But pattern stability reveals a different — and stronger — payoff:

1. **New feature velocity.** Evidence shows developers copy-paste raw access patterns rather than discovering helpers. `construct_indicators()` has 2 call sites vs. 18 raw duplicates. Complete encapsulation means new feature code naturally uses helpers because there's no raw pattern to copy from nearby code.

2. **The accessor API is designable once.** 16 patterns = 16 functions, and that set won't grow. This eliminates the usual risk of accessor-based refactoring (an ever-expanding API surface). We're wrapping a finite, known set.

3. **Type-value growth is handled automatically.** When someone adds a new type like "UNIT", a parameterized `constructs_of_type(type, mmMatrix)` just works — no new code needed. With raw access, every site that switches on type values needs manual updating.

### The hybrid approach remains pragmatic

The hybrid approach (S3 `[` override + adopt existing helpers only) gives ~80% of the benefit with ~20% of the churn. Since patterns are stable, the ~54 sites covered by existing helpers are the high-frequency core patterns, and the remaining ~120 sites are tail patterns (1-3 occurrences each) where encapsulation overhead barely pays for itself.

### The decision reduces to a judgment call

The stable pattern set makes full encapsulation *safe* (the API won't need to keep expanding) and makes even partial encapsulation *effective* (the core 5-6 helpers cover most access). The remaining question is whether the churn of touching ~120 additional tail-pattern sites is worth the completeness — a tradeoff between code review burden and tidiness.

---

Last updated: 2026-02-25
