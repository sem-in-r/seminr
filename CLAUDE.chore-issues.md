# SEMinR Development Queue

**Repository**: [sem-in-r/seminr](https://github.com/sem-in-r/seminr)
**Updated**: 2026-02-01

## Summary

| Ready to Merge | In Progress | v2.4.0 Staged | Experimental | Ready to Work | Needs Investigation | Backlog |
|----------------|-------------|---------------|--------------|---------------|---------------------|---------|
| —              | 1 PR        | 1 PR          | 2 PRs        | 10 issues     | 5 issues            | 20+     |

---

## 🔥 Immediate Actions

### PRs Ready to Merge

_(none currently)_

### PRs for Future Release (v2.4.0)

| PR | Fixes | Author | Status | Notes |
|----|-------|--------|--------|-------|
| [**#374**](https://github.com/sem-in-r/seminr/pull/374) | [#373](https://github.com/sem-in-r/seminr/issues/373) (HOC summary bug) | @Lilikon | 📦 Staged | Targets `version_2_4_0` branch |

### PRs In Progress

| PR | Fixes | Author | Status | Action |
|----|-------|--------|--------|--------|
| [**#379**](https://github.com/sem-in-r/seminr/pull/379) | [#377](https://github.com/sem-in-r/seminr/issues/377) (VIF output) | @soumyaray | 🚧 Open | Review → Merge |

### Quick Wins (< 1 hour each)

| Issue | Title | Status | Action |
|-------|-------|--------|--------|
| ~~[**#377**](https://github.com/sem-in-r/seminr/issues/377)~~ | ~~VIF output structure inconsistent~~ | ✅ PR [#379](https://github.com/sem-in-r/seminr/pull/379) | Review PR |
| ~~[**#240**](https://github.com/sem-in-r/seminr/issues/240)~~ | ~~summary() fails with factor column~~ | ✅ Closed | Fixed in v2.3.1 (PR #285) |
| [**#298**](https://github.com/sem-in-r/seminr/issues/298) | predict_pls() rownames error | 📋 Open | Normalize rownames in predict |
| [**#226**](https://github.com/sem-in-r/seminr/issues/226) | Lambda symbol not rendered | 📋 Open | Encoding fix |

---

## 📋 Ready to Work On

### High Priority Bugs

| Issue | Title | Status | Notes | Cluster |
|-------|-------|--------|-------|---------|
| [**#369**](https://github.com/sem-in-r/seminr/issues/369) | summary() regression in v2.3.7 | 📋 Open | 6 users affected | summary |
| [**#353**](https://github.com/sem-in-r/seminr/issues/353) | summary() subscript out of bounds | 📋 Open | Complex models with interactions | summary |
| [**#347**](https://github.com/sem-in-r/seminr/issues/347) | predict_pls subscript error | 📋 Open | Related to [#298](https://github.com/sem-in-r/seminr/issues/298) | predict |
| [**#341**](https://github.com/sem-in-r/seminr/issues/341) | HTMT Inf error in bootstrap | 📋 Open | Small sample sizes | bootstrap |
| [**#327**](https://github.com/sem-in-r/seminr/issues/327) | interaction_term() quadratic error | 📋 Open | Single IV edge case | — |
| [**#299**](https://github.com/sem-in-r/seminr/issues/299) | PLSc + HOC bootstrap length error | 📋 Open | Known pattern | hoc + bootstrap |

### Moderate Priority Bugs

| Issue | Title | Status | Notes | Cluster |
|-------|-------|--------|-------|---------|
| [**#364**](https://github.com/sem-in-r/seminr/issues/364) | all_loc_non_int_items() issue | 📋 Open | Internal function | — |
| [**#348**](https://github.com/sem-in-r/seminr/issues/348) | Duplicate paths in plot() | 📋 Open | Construct defined twice | plot |
| [**#318**](https://github.com/sem-in-r/seminr/issues/318) | Bootstrap "no package seminr" | 📋 Open | Parallel env issue | bootstrap |
| [**#262**](https://github.com/sem-in-r/seminr/issues/262) | Ten Berge fails non-numeric | 📋 Open | Missing data type check | — |
| [**#213**](https://github.com/sem-in-r/seminr/issues/213) | save_plot() HOC + moderation | 📋 Open | Plot export issue | hoc + plot |
| [**#205**](https://github.com/sem-in-r/seminr/issues/205) | PLSc bootstrap HOC errors | 📋 Open | Related to [#299](https://github.com/sem-in-r/seminr/issues/299) | hoc + bootstrap |

### High-Demand Features

| Issue | Title | Status | Notes |
|-------|-------|--------|-------|
| [**#375**](https://github.com/sem-in-r/seminr/issues/375) + [**#136**](https://github.com/sem-in-r/seminr/issues/136) | Export results to file | 📋 Open | Combine into `export_results()` |
| [**#90**](https://github.com/sem-in-r/seminr/issues/90) | Bootstrap indirect effects | 📋 Open | Core mediation feature |
| [**#110**](https://github.com/sem-in-r/seminr/issues/110) | Categorical variable support | 📋 Open | Long-standing request |

---

## 🔍 Needs Investigation

| Issue | Title | Status | What's Needed |
|-------|-------|--------|---------------|
| [**#350**](https://github.com/sem-in-r/seminr/issues/350) | "Bootstrapping keeps running" | 📋 Open | Reproducible example |
| [**#329**](https://github.com/sem-in-r/seminr/issues/329) | Bootstrap error | 📋 Open | Reproducible example |
| [**#339**](https://github.com/sem-in-r/seminr/issues/339) | Near-zero-variance binary | 📋 Open | Decide: fix or document limitation |
| [**#256**](https://github.com/sem-in-r/seminr/issues/256) | Bootstrapping issue | 📋 Open | Check if already addressed (tagged `doing`) |
| [**#291**](https://github.com/sem-in-r/seminr/issues/291) | Dissimilar p-values in MGA | 📋 Open | Methodology clarification |

---

## 📦 Backlog

### Features

| Issue | Title | Notes |
|-------|-------|-------|
| [**#335**](https://github.com/sem-in-r/seminr/issues/335) | Time series bootstrapping | Block bootstrap support |
| [**#326**](https://github.com/sem-in-r/seminr/issues/326) + [**#320**](https://github.com/sem-in-r/seminr/issues/320) | Weighted PLS | Survey weights / post-stratification |
| [**#319**](https://github.com/sem-in-r/seminr/issues/319) | BCa confidence intervals | Bias-corrected bootstrap |
| [**#261**](https://github.com/sem-in-r/seminr/issues/261) | Invariance testing | MICOM support |
| [**#254**](https://github.com/sem-in-r/seminr/issues/254) | Bootstrap reliability CIs | CI for measurement metrics |
| [**#248**](https://github.com/sem-in-r/seminr/issues/248) | Invalid solution warnings | Solution quality checks |
| [**#236**](https://github.com/sem-in-r/seminr/issues/236) | Mediated-moderation | Complex moderation patterns |
| [**#163**](https://github.com/sem-in-r/seminr/issues/163) | Sign indeterminacy | Handle sign flipping in bootstrap |
| [**#160**](https://github.com/sem-in-r/seminr/issues/160) | Tidy CFA/EFA output | Tidyverse compatibility |
| [**#151**](https://github.com/sem-in-r/seminr/issues/151) | GSCA estimation | Alternative method |
| [**#146**](https://github.com/sem-in-r/seminr/issues/146) | as.composite() | Convert reflective to composite |
| [**#145**](https://github.com/sem-in-r/seminr/issues/145) | SRMR fit index | Additional CBSEM fit index |
| [**#134**](https://github.com/sem-in-r/seminr/issues/134) | Construct class names order | Code refactoring |
| [**#55**](https://github.com/sem-in-r/seminr/issues/55) | Model simulation | Monte Carlo support |
| [**#53**](https://github.com/sem-in-r/seminr/issues/53) | MGA improvements | Enhanced multi-group analysis |

### Lower Priority Bugs

| Issue | Title | Notes |
|-------|-------|-------|
| [**#233**](https://github.com/sem-in-r/seminr/issues/233) | fSquared missing for indirect | May be feature not bug |
| [**#250**](https://github.com/sem-in-r/seminr/issues/250) | Bootstrap 1 antecedent, many outcomes | Edge case |
| [**#224**](https://github.com/sem-in-r/seminr/issues/224) | Unfriendly item name errors | Better error messages |
| [**#257**](https://github.com/sem-in-r/seminr/issues/257) | Parallel error reporting | Internal improvement |

---

## 🧪 Experimental PRs

Low priority explorations — not actively being addressed.

| PR | Title | Author | Notes |
|----|-------|--------|-------|
| [**#358**](https://github.com/sem-in-r/seminr/pull/358) | Wald test for bootstrap paths | @soumyaray | Addresses [#141](https://github.com/sem-in-r/seminr/issues/141), [#372](https://github.com/sem-in-r/seminr/issues/372) (p-values) |
| [**#221**](https://github.com/sem-in-r/seminr/pull/221) | Unstandardization features | @soumyaray | Targets `develop` branch |

---

## 🗑️ Candidates for Closing

### Stale PRs (>2 years)

| PR | Title | Decision Needed |
|----|-------|-----------------|
| [**#313**](https://github.com/sem-in-r/seminr/pull/313) | Issue templates update | Close or complete |
| [**#220**](https://github.com/sem-in-r/seminr/pull/220) | Bootstrap significance | Close - superseded by [#358](https://github.com/sem-in-r/seminr/pull/358) |
| [**#217**](https://github.com/sem-in-r/seminr/pull/217) | Package paper | Evaluate: still want paper? |

### Stale Issues

| Issue | Title | Reason |
|-------|-------|--------|
| [**#150**](https://github.com/sem-in-r/seminr/issues/150) | Repeated indicators HOC | Already tagged wontfix |
| [**#223**](https://github.com/sem-in-r/seminr/issues/223) | semPlot not installing | External dependency |
| [**#209**](https://github.com/sem-in-r/seminr/issues/209) | MKL CRAN test | Old CI issue |
| [**#222**](https://github.com/sem-in-r/seminr/issues/222) | pls_predict HOC error | Likely covered by other HOC fixes |

---

## Reference: Issue Clusters

Understanding these patterns helps prioritize fixes that address multiple issues.

### HOC/Higher-Order Construct Issues
- **Issues**: [#373](https://github.com/sem-in-r/seminr/issues/373), [#369](https://github.com/sem-in-r/seminr/issues/369), [#353](https://github.com/sem-in-r/seminr/issues/353), [#299](https://github.com/sem-in-r/seminr/issues/299), [#222](https://github.com/sem-in-r/seminr/issues/222), [#213](https://github.com/sem-in-r/seminr/issues/213), [#205](https://github.com/sem-in-r/seminr/issues/205)
- **Pattern**: HOC + bootstrap or HOC + summary failures
- **Recommendation**: Review all HOC-related code paths together

### Bootstrap Issues
- **Issues**: [#350](https://github.com/sem-in-r/seminr/issues/350), [#341](https://github.com/sem-in-r/seminr/issues/341), [#339](https://github.com/sem-in-r/seminr/issues/339), [#329](https://github.com/sem-in-r/seminr/issues/329), [#318](https://github.com/sem-in-r/seminr/issues/318), [#299](https://github.com/sem-in-r/seminr/issues/299), [#256](https://github.com/sem-in-r/seminr/issues/256), [#250](https://github.com/sem-in-r/seminr/issues/250), [#205](https://github.com/sem-in-r/seminr/issues/205), [#163](https://github.com/sem-in-r/seminr/issues/163)
- **Pattern**: Many edge cases and environment-specific issues
- **Recommendation**: Consider bootstrap robustness refactor

### Export/Reporting Issues
- **Issues**: [#377](https://github.com/sem-in-r/seminr/issues/377), [#375](https://github.com/sem-in-r/seminr/issues/375), [#372](https://github.com/sem-in-r/seminr/issues/372), [#141](https://github.com/sem-in-r/seminr/issues/141), [#136](https://github.com/sem-in-r/seminr/issues/136), [#90](https://github.com/sem-in-r/seminr/issues/90)
- **Pattern**: Users want easy export and complete reporting
- **Recommendation**: Create comprehensive `export_results()` utilities

### Prediction Issues
- **Issues**: [#347](https://github.com/sem-in-r/seminr/issues/347), [#298](https://github.com/sem-in-r/seminr/issues/298), [#222](https://github.com/sem-in-r/seminr/issues/222)
- **Pattern**: predict_pls() fragile with non-standard data
- **Recommendation**: Add input validation and rowname normalization

### Summary Function Issues
- **Issues**: [#373](https://github.com/sem-in-r/seminr/issues/373), [#369](https://github.com/sem-in-r/seminr/issues/369), [#353](https://github.com/sem-in-r/seminr/issues/353), [#341](https://github.com/sem-in-r/seminr/issues/341)
- **Pattern**: summary() breaks on edge cases
- **Recommendation**: Fix together - likely shared root causes
