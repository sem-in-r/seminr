# SEMinR GitHub Issues Triage

**Repository**: [sem-in-r/seminr](https://github.com/sem-in-r/seminr)
**Date**: 2026-01-31
**Total Open Issues**: 53

---

## Summary Statistics

| Category | Count |
|----------|-------|
| Bugs | 15 |
| Feature Requests | 17 |
| Discussion/Other | 21 |

---

## Priority Tiers

### Tier 1: Critical Bugs (High Priority)
These are bugs that break core functionality and should be addressed first.

| # | Title | Created | Notes |
|---|-------|---------|-------|
| **377** | Inconsistent structure of vif_items output | 2026-01-29 | **NEW** - VIF output structure changes unpredictably when all constructs have same # of indicators vs different. Makes automated extraction unreliable. Likely a `simplify2array` issue. |
| **373** | Dr. Kidd's Bug - summary() fails with HOC | 2025-12-05 | `summary(pls)` fails with HOC: "undefined columns selected". Likely a column name mismatch between raw data and HOC names. |
| **369** | seminr 2.3.7 - issue with summary() | 2025-09-22 | Users reporting summary() regression after upgrade. Has 6 comments indicating multiple affected users. |
| **353** | Error when using summary() function | 2024-10-23 | "subscript out of bounds" in summary() with complex models including interactions. Model can be bootstrapped but not summarized. |
| **347** | Error in predict_pls function | 2024-02-07 | predict_pls fails with "subscript out of bounds". Related to #298 rownames issue. |
| **341** | Error calling summary() for HTMT in bootstrap_model | 2023-10-16 | Inf values in boot_HTMT cause division error. Affects small sample sizes or certain bootstrap configurations. |
| **327** | interaction_term() errors with quadratic (1 IV only) | 2023-03-29 | Using X*X quadratic term with only 1 IV causes "incorrect number of dimensions" error in structural_model. |
| **299** | bootstrap_model() errors with PLSc and HOC | 2022-07-07 | Bootstrap length calculation incorrect for HOC models. Known issue pattern. |
| **298** | predict_pls() error when rownames not sequential | 2022-07-01 | Root cause identified: non-sequential rownames in data cause subscript errors. User workaround exists but package should handle. |

**Common Pattern**: Many summary() and predict() issues stem from:
1. Non-sequential rownames in data
2. HOC construct name handling
3. Edge cases with equal-sized constructs

---

### Tier 2: Moderate Bugs (Medium Priority)

| # | Title | Created | Notes |
|---|-------|---------|-------|
| **364** | Problems with all_loc_non_int_items() | 2025-08-01 | Internal function issue affecting item location. |
| **348** | plot() displays duplicate paths for interaction constructs | 2024-04-17 | Visual bug - construct defined twice in measurement model causes double paths. Likely user error but error message could be clearer. |
| **339** | bootstrap_model errors with near-zero-variance binary | 2023-09-26 | Edge case with binary variables that have near-zero variance. Tagged `discuss`. |
| **318** | Bootstrap error: "no package called 'seminr'" | 2022-11-30 | Parallel execution issue - package not loaded in worker nodes. May be environment-specific. |
| **262** | Ten Berge scores fail with non-numeric data | 2022-01-14 | Non-numeric columns cause Ten Berge estimation to fail. |
| **240** | summary() fails with factor column in data | 2021-08-16 | Factor columns in data cause cor() to fail. Should filter to numeric only. |
| **233** | Effect size for indirect/total effects missing | 2021-07-23 | fSquared doesn't account for mediators. May be mislabeled as bug vs feature. |
| **226** | Lambda symbol not rendered appropriately | 2021-07-12 | Unicode/encoding issue in output. |
| **213** | save_plot() not working with HOC + moderation | 2021-04-28 | Plot export issue for complex models. |
| **205** | PLSc bootstrap errors with HOC | 2021-03-24 | Related to #299 - bootstrap + HOC issues. |

---

### Tier 3: High-Demand Feature Requests

| # | Title | Created | Notes | Demand |
|---|-------|---------|-------|--------|
| **375** | Export results for model comparison | 2026-01-05 | User wants to export path coefficients and R2 for comparison across models. You mentioned working on this. | High |
| **372** | P-value for specific effect in mediation | 2025-11-29 | Common request for mediation analysis reporting. | High |
| **141** | Report p-values for PLS bootstrapping | 2020-06-08 | Long-standing request. Many users want p-values alongside t-stats. | Very High |
| **136** | Saving results to file (Excel/CSV) | 2020-04-27 | Multiple users want easy export functionality. | Very High |
| **110** | Handle ordinal/nominal categorical variables | 2019-06-20 | Support for categorical data types. Long-standing. | High |
| **90** | Bootstrap indirect effects for mediation | 2018-11-06 | Core mediation testing feature. Very old issue. | Very High |

---

### Tier 4: Nice-to-Have Features

| # | Title | Created | Notes |
|---|-------|---------|-------|
| **335** | Allow bootstrapping for time series | 2023-08-04 | Block bootstrapping for time series data. |
| **326** | Weighted PLS for stratified sampling | 2023-03-27 | Survey weights for complex sampling designs. |
| **320** | Post-stratification weights | 2022-12-29 | Similar to #326 - weighting support. |
| **319** | Bootstrap with bias correction | 2022-12-14 | BCa confidence intervals. |
| **261** | Invariance test function | 2022-01-12 | MICOM/measurement invariance testing. |
| **254** | Bootstrapped reliability/validity metrics | 2021-11-04 | CI for measurement model metrics. |
| **248** | Notify if PLS solution invalid | 2021-09-20 | Solution quality warnings. |
| **236** | Mediated-moderation for CBSEM + PLS | 2021-07-28 | Complex moderation patterns. |
| **163** | Sign indeterminacy during bootstrapping | 2020-09-13 | Handle sign flipping in bootstrap. |
| **160** | Tidy CFA/EFA output | 2020-08-05 | Tidyverse-compatible output. |
| **151** | Support GSCA estimation | 2020-07-01 | Alternative estimation method. |
| **146** | as.composite() function | 2020-06-26 | Convert reflective to composite. |
| **145** | Add SRMR fit index for CBSEM | 2020-06-26 | Additional fit index. |
| **134** | Order of construct class names | 2020-04-22 | Code refactoring. |
| **55** | Simulation/generation of models | 2018-01-25 | Monte Carlo simulation support. |
| **53** | Multi-group analysis improvements | 2018-01-19 | Enhanced MGA features. |

---

### Tier 5: Low Priority / Stale

| # | Title | Created | Notes |
|---|-------|---------|-------|
| **350** | Bootstrapping not working | 2024-06-02 | Vague issue - "keeps running". May be performance/timeout. |
| **329** | Bootstrap error in seminr model | 2023-04-12 | Tagged `discuss`, likely needs reproduction. |
| **291** | Dissimilar p-values in MGA | 2022-06-23 | Documentation issue - methodology clarification. |
| **257** | Investigate error reporting from parallel | 2022-01-03 | Internal improvement task. |
| **256** | Bootstrapping issue | 2022-01-02 | Tagged `doing` - may already be addressed? |
| **250** | Bug in bootstrap with 1 antecedent, many outcomes | 2021-10-26 | Edge case. |
| **224** | Item name errors unfriendly | 2021-06-14 | Better error messages for item mismatches. |
| **223** | semPlot not installing | 2021-06-14 | External dependency issue. |
| **222** | Error in pls_predict with HOC | 2021-06-03 | Related to other HOC issues. |
| **150** | Support repeated indicators for HOC | 2020-06-30 | Tagged `wontfix`. |
| **209** | MKL CRAN test | 2021-04-25 | CI/infrastructure. |

---

## Recommended Action Plan

### Phase 1: Quick Wins (Fix immediately)
1. **#377** - VIF inconsistency: Likely a simple fix to ensure consistent output structure
2. **#240** - Factor column crash: Filter to numeric before cor()
3. **#298** - Rownames issue: Normalize rownames before predict operations
4. **#226** - Lambda symbol: Encoding fix

### Phase 2: Core Bug Fixes
1. **#373, #369, #353** - summary() issues cluster: These may share a root cause related to HOC/interaction handling
2. **#341** - HTMT Inf handling: Add NA/Inf checks before division
3. **#327** - Quadratic interaction: Handle single-IV case in interaction_term()
4. **#299, #205** - PLSc + HOC bootstrap: Fix length calculation

### Phase 3: High-Value Features
1. **#141 + #372** - P-values for bootstrap: Add p-value calculation from t-stats
2. **#136 + #375** - Export functionality: Create export_results() function
3. **#90** - Bootstrap indirect effects: Complete mediation testing

### Phase 4: Maintenance
1. Close stale issues with no recent activity (>2 years)
2. Merge duplicate issues (HOC-related, export-related)
3. Add labels to unlabeled issues
4. Request reproducible examples for vague issues

---

## Issue Clusters (Related Issues)

### HOC/Higher-Order Construct Issues
- #373, #369, #353, #299, #222, #213, #205
- **Pattern**: Many issues involve HOC + bootstrap or HOC + summary
- **Recommendation**: Review all HOC-related code paths

### Bootstrap Issues
- #350, #341, #339, #329, #318, #299, #256, #250, #205, #163
- **Pattern**: Bootstrap has many edge cases and environment issues
- **Recommendation**: Consider bootstrap robustness refactor

### Export/Reporting Issues
- #377, #375, #372, #141, #136, #90
- **Pattern**: Users want easy export and complete reporting
- **Recommendation**: Create comprehensive export utilities

### Prediction Issues
- #347, #298, #222
- **Pattern**: predict_pls() fragile with non-standard data
- **Recommendation**: Add input validation and normalization

---

## Quick Reference: Labels Needed

Issues without labels that should be categorized:
- **#377**: needs `bug` label
- **#372**: needs `new feature` label
- **#369**: needs `bug` label
- **#353**: needs `bug` label
- **#350**: needs investigation/`discuss` label
- **#348**: needs `bug` or `documentation` label
- **#341**: needs `bug` label
- **#261**: needs `new feature` label
- **#254**: needs `new feature` label
- **#250**: needs `bug` label
- **#224**: needs `enhancement` label
- **#222**: needs `bug` label

---

## Next Steps

1. [ ] Apply missing labels to issues
2. [ ] Close #150 (already tagged wontfix)
3. [ ] Request reproducible examples for #350, #329
4. [ ] Begin with Phase 1 quick wins
5. [ ] Consider milestone for v2.5.0 with grouped fixes
