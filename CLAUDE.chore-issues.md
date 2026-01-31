# SEMinR Development Queue

**Repository**: [sem-in-r/seminr](https://github.com/sem-in-r/seminr)
**Updated**: 2026-01-31

## Summary

| Ready to Merge | Ready to Work | Needs Investigation | Backlog |
|----------------|---------------|---------------------|---------|
| 2 PRs          | 12 issues     | 5 issues            | 20+     |

---

## 🔥 Immediate Actions

### PRs Ready to Merge

| PR | Fixes | Author | Status | Action |
|----|-------|--------|--------|--------|
| **#374** | #373 (HOC summary bug) | @Lilikon | ⚠️ Conflicts | Rebase → Review → Merge |
| **#358** | #141, #372 (p-values) | @soumyaray | 🔍 Review | Complete 2 checklist items → Merge |

### Quick Wins (< 1 hour each)

| # | Title | Action |
|---|-------|--------|
| **#377** | VIF output structure inconsistent | Fix `simplify2array` issue |
| **#240** | summary() fails with factor column | Filter to numeric before cor() |
| **#298** | predict_pls() rownames error | Normalize rownames in predict |
| **#226** | Lambda symbol not rendered | Encoding fix |

---

## 📋 Ready to Work On

### High Priority Bugs

| # | Title | Notes | Cluster |
|---|-------|-------|---------|
| **#369** | summary() regression in v2.3.7 | 6 users affected | summary |
| **#353** | summary() subscript out of bounds | Complex models with interactions | summary |
| **#347** | predict_pls subscript error | Related to #298 | predict |
| **#341** | HTMT Inf error in bootstrap | Small sample sizes | bootstrap |
| **#327** | interaction_term() quadratic error | Single IV edge case | — |
| **#299** | PLSc + HOC bootstrap length error | Known pattern | hoc + bootstrap |

### Moderate Priority Bugs

| # | Title | Notes | Cluster |
|---|-------|-------|---------|
| **#364** | all_loc_non_int_items() issue | Internal function | — |
| **#348** | Duplicate paths in plot() | Construct defined twice | plot |
| **#318** | Bootstrap "no package seminr" | Parallel env issue | bootstrap |
| **#262** | Ten Berge fails non-numeric | Missing data type check | — |
| **#213** | save_plot() HOC + moderation | Plot export issue | hoc + plot |
| **#205** | PLSc bootstrap HOC errors | Related to #299 | hoc + bootstrap |

### High-Demand Features

| # | Title | Notes |
|---|-------|-------|
| **#375 + #136** | Export results to file | Combine into `export_results()` |
| **#90** | Bootstrap indirect effects | Core mediation feature |
| **#110** | Categorical variable support | Long-standing request |

---

## 🔍 Needs Investigation

| # | Title | What's Needed |
|---|-------|---------------|
| **#350** | "Bootstrapping keeps running" | Reproducible example |
| **#329** | Bootstrap error | Reproducible example |
| **#339** | Near-zero-variance binary | Decide: fix or document limitation |
| **#256** | Bootstrapping issue | Check if already addressed (tagged `doing`) |
| **#291** | Dissimilar p-values in MGA | Methodology clarification |

---

## 📦 Backlog

### Features

| # | Title | Notes |
|---|-------|-------|
| **#335** | Time series bootstrapping | Block bootstrap support |
| **#326 + #320** | Weighted PLS | Survey weights / post-stratification |
| **#319** | BCa confidence intervals | Bias-corrected bootstrap |
| **#261** | Invariance testing | MICOM support |
| **#254** | Bootstrap reliability CIs | CI for measurement metrics |
| **#248** | Invalid solution warnings | Solution quality checks |
| **#236** | Mediated-moderation | Complex moderation patterns |
| **#163** | Sign indeterminacy | Handle sign flipping in bootstrap |
| **#160** | Tidy CFA/EFA output | Tidyverse compatibility |
| **#151** | GSCA estimation | Alternative method |
| **#146** | as.composite() | Convert reflective to composite |
| **#145** | SRMR fit index | Additional CBSEM fit index |
| **#134** | Construct class names order | Code refactoring |
| **#55** | Model simulation | Monte Carlo support |
| **#53** | MGA improvements | Enhanced multi-group analysis |

### Lower Priority Bugs

| # | Title | Notes |
|---|-------|-------|
| **#233** | fSquared missing for indirect | May be feature not bug |
| **#250** | Bootstrap 1 antecedent, many outcomes | Edge case |
| **#224** | Unfriendly item name errors | Better error messages |
| **#257** | Parallel error reporting | Internal improvement |

---

## 🗑️ Candidates for Closing

### Stale PRs (>2 years)

| PR | Title | Decision Needed |
|----|-------|-----------------|
| **#313** | Issue templates update | Close or complete |
| **#221** | Unstandardization features | Evaluate if still needed |
| **#220** | Bootstrap significance | Close - superseded by #358 |
| **#217** | Package paper | Evaluate: still want paper? |

### Stale Issues

| # | Title | Reason |
|---|-------|--------|
| **#150** | Repeated indicators HOC | Already tagged wontfix |
| **#223** | semPlot not installing | External dependency |
| **#209** | MKL CRAN test | Old CI issue |
| **#222** | pls_predict HOC error | Likely covered by other HOC fixes |

---

## Reference: Issue Clusters

Understanding these patterns helps prioritize fixes that address multiple issues.

### HOC/Higher-Order Construct Issues
- **Issues**: #373, #369, #353, #299, #222, #213, #205
- **Pattern**: HOC + bootstrap or HOC + summary failures
- **Recommendation**: Review all HOC-related code paths together

### Bootstrap Issues
- **Issues**: #350, #341, #339, #329, #318, #299, #256, #250, #205, #163
- **Pattern**: Many edge cases and environment-specific issues
- **Recommendation**: Consider bootstrap robustness refactor

### Export/Reporting Issues
- **Issues**: #377, #375, #372, #141, #136, #90
- **Pattern**: Users want easy export and complete reporting
- **Recommendation**: Create comprehensive `export_results()` utilities

### Prediction Issues
- **Issues**: #347, #298, #222
- **Pattern**: predict_pls() fragile with non-standard data
- **Recommendation**: Add input validation and rowname normalization

### Summary Function Issues
- **Issues**: #373, #369, #353, #341, #240
- **Pattern**: summary() breaks on edge cases
- **Recommendation**: Fix together - likely shared root causes
