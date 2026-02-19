# SEMinR Development Queue

**Repository**: [sem-in-r/seminr](https://github.com/sem-in-r/seminr)
**Updated**: 2026-02-19
**Issue closing policy**: Fixed in develop, close on release
**Completed items**: See [CLAUDE.issues-completed.md](CLAUDE.issues-completed.md) for release history, closed issues, and archived PRs

## Summary

| Current Version | PRs to Review | Experimental | Ready to Work | Needs Investigation | Backlog |
| --------------- | ------------- | ------------ | ------------- | ------------------- | ------- |
| v2.4.2          | 1 (community) | 2 PRs        | 3 high + 5 moderate | 4 issues + 1 close candidate | 20+     |

---

## 🔥 Immediate Actions

### PRs Ready to Review

| PR | Author | Status | Notes |
| --- | --- | --- | --- |
| [**#394**](https://github.com/sem-in-r/seminr/pull/394) | @Marwolaeth | 📋 Open | Enable custom confidence levels for plotting bootstrapped models; community contribution |

### PRs In Progress

| Branch | Fixes | Author | Status | Notes |
| --- | --- | --- | --- | --- |
| `ray/chore-actions-macos-pak` | [#386](https://github.com/sem-in-r/seminr/issues/386) (macOS CI failures) | @soumyaray | 📋 Open ([PR #384](https://github.com/sem-in-r/seminr/pull/384)) | Waiting on upstream pak fix ([r-lib/pak#840](https://github.com/r-lib/pak/issues/840)); added workflow_dispatch + setup-pandoc@v2 |

---

## 📋 Ready to Work On

### High Priority Bugs

| Issue | Title | Status | Notes | Cluster |
| --- | --- | --- | --- | --- |
| [**#318**](https://github.com/sem-in-r/seminr/issues/318) | Bootstrap "no package seminr" | 📋 Open | 6 comments; parallel env issue affecting users; PR #388 may partially address | bootstrap |
| [**#299**](https://github.com/sem-in-r/seminr/issues/299) + [**#205**](https://github.com/sem-in-r/seminr/issues/205) | PLSc + HOC bootstrap errors | 📋 Open | Related issues — work together | hoc + bootstrap |
| [**#327**](https://github.com/sem-in-r/seminr/issues/327) | interaction_term() quadratic error | 📋 Open | Single IV edge case | — |

### CI/Infrastructure

| Issue | Title | Status | Notes |
| --- | --- | --- | --- |
| [**#386**](https://github.com/sem-in-r/seminr/issues/386) | macOS CI pak installation failures | 📋 Open | Upstream CRAN metadata sync issue; [PR #384](https://github.com/sem-in-r/seminr/pull/384) adds workflow_dispatch + setup-pandoc@v2; waiting on [r-lib/pak#840](https://github.com/r-lib/pak/issues/840) |

### Moderate Priority Bugs

| Issue | Title | Status | Notes | Cluster |
| --- | --- | --- | --- | --- |
| [**#364**](https://github.com/sem-in-r/seminr/issues/364) | all_loc_non_int_items() issue | 📋 Open | Internal function; recent (2025-08) | — |
| [**#341**](https://github.com/sem-in-r/seminr/issues/341) | HTMT Inf error in bootstrap | 📋 Open | Small sample sizes; low engagement | bootstrap |
| [**#348**](https://github.com/sem-in-r/seminr/issues/348) | Duplicate paths in plot() | 📋 Open | Construct defined twice | plot |
| [**#262**](https://github.com/sem-in-r/seminr/issues/262) | Ten Berge fails non-numeric | 📋 Open | Missing data type check | — |
| [**#213**](https://github.com/sem-in-r/seminr/issues/213) | save_plot() HOC + moderation | 📋 Open | Plot export issue | hoc + plot |

### High-Demand Features

| Issue | Title | Status | Notes |
| --- | --- | --- | --- |
| [**#375**](https://github.com/sem-in-r/seminr/issues/375) + [**#136**](https://github.com/sem-in-r/seminr/issues/136) | Export results to file | 📋 Open | Combine into `export_results()` |
| [**#90**](https://github.com/sem-in-r/seminr/issues/90) | Bootstrap indirect effects | 📋 Open | Core mediation feature |
| [**#110**](https://github.com/sem-in-r/seminr/issues/110) | Categorical variable support | 📋 Open | Long-standing request |

---

## 🔍 Needs Investigation

| Issue | Title | Status | What's Needed |
| --- | --- | --- | --- |
| [**#350**](https://github.com/sem-in-r/seminr/issues/350) | "Bootstrapping keeps running" | 📋 Open | Reproducible example needed |
| [**#256**](https://github.com/sem-in-r/seminr/issues/256) | Bootstrapping issue | 📋 Open | Tagged `doing` since 2022; check if still relevant |
| [**#339**](https://github.com/sem-in-r/seminr/issues/339) | Near-zero-variance binary | 📋 Open | Left open for discoverability; consider documenting as known limitation |
| [**#291**](https://github.com/sem-in-r/seminr/issues/291) | Dissimilar p-values in MGA | 📋 Open | Methodology clarification; tagged `documentation` |
| [**#329**](https://github.com/sem-in-r/seminr/issues/329) | Bootstrap error | 📋 Open | Resolved by reporter — haven package interference, not a seminr bug; **candidate for closing** |

---

## 📦 Backlog

### Features

| Issue | Title | Notes |
| --- | --- | --- |
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
| --- | --- | --- |
| [**#233**](https://github.com/sem-in-r/seminr/issues/233) | fSquared missing for indirect | May be feature not bug |
| [**#250**](https://github.com/sem-in-r/seminr/issues/250) | Bootstrap 1 antecedent, many outcomes | Edge case |
| [**#224**](https://github.com/sem-in-r/seminr/issues/224) | Unfriendly item name errors | Better error messages |
| [**#257**](https://github.com/sem-in-r/seminr/issues/257) | Parallel error reporting | Internal improvement |

---

## 🧪 Experimental PRs

Low priority explorations — not actively being addressed.

| PR | Title | Author | Notes |
| --- | --- | --- | --- |
| [**#358**](https://github.com/sem-in-r/seminr/pull/358) | Wald test for bootstrap paths | @soumyaray | Addresses [#141](https://github.com/sem-in-r/seminr/issues/141), [#372](https://github.com/sem-in-r/seminr/issues/372) (p-values) |
| [**#221**](https://github.com/sem-in-r/seminr/pull/221) | Unstandardization features | @soumyaray | Targets `develop` branch |

---

## 🗑️ Candidates for Closing

### Stale PRs (>2 years)

| PR | Title | Decision Needed |
| --- | --- | --- |
| [**#313**](https://github.com/sem-in-r/seminr/pull/313) | Issue templates update | Close or complete |
| [**#220**](https://github.com/sem-in-r/seminr/pull/220) | Bootstrap significance | Close - superseded by [#358](https://github.com/sem-in-r/seminr/pull/358) |
| [**#217**](https://github.com/sem-in-r/seminr/pull/217) | Package paper | Evaluate: still want paper? |

### Stale Issues

| Issue | Title | Reason |
| --- | --- | --- |
| [**#329**](https://github.com/sem-in-r/seminr/issues/329) | Bootstrap error | Resolved by reporter — haven package issue, not seminr |
| [**#150**](https://github.com/sem-in-r/seminr/issues/150) | Repeated indicators HOC | Already tagged wontfix |
| [**#223**](https://github.com/sem-in-r/seminr/issues/223) | semPlot not installing | External dependency |
| [**#209**](https://github.com/sem-in-r/seminr/issues/209) | MKL CRAN test | Old CI issue |
| [**#222**](https://github.com/sem-in-r/seminr/issues/222) | pls_predict HOC error | Likely covered by other HOC fixes |

---

## Reference: Issue Clusters

Understanding these patterns helps prioritize fixes that address multiple issues. Only open issues listed.

### HOC/Higher-Order Construct Issues

- **Open**: [#299](https://github.com/sem-in-r/seminr/issues/299), [#222](https://github.com/sem-in-r/seminr/issues/222), [#213](https://github.com/sem-in-r/seminr/issues/213), [#205](https://github.com/sem-in-r/seminr/issues/205)
- **Pattern**: HOC + bootstrap failures
- **Recommendation**: Review all HOC-related code paths together

### Bootstrap Issues

- **Open**: [#318](https://github.com/sem-in-r/seminr/issues/318), [#350](https://github.com/sem-in-r/seminr/issues/350), [#341](https://github.com/sem-in-r/seminr/issues/341), [#339](https://github.com/sem-in-r/seminr/issues/339), [#299](https://github.com/sem-in-r/seminr/issues/299)+[#205](https://github.com/sem-in-r/seminr/issues/205), [#256](https://github.com/sem-in-r/seminr/issues/256), [#250](https://github.com/sem-in-r/seminr/issues/250), [#163](https://github.com/sem-in-r/seminr/issues/163)
- **Likely closeable**: #329 (haven interference, not seminr)
- **Pattern**: Many edge cases and environment-specific issues
- **Recommendation**: Consider bootstrap robustness refactor; #318 is highest impact

### Export/Reporting Issues

- **Open**: [#375](https://github.com/sem-in-r/seminr/issues/375), [#372](https://github.com/sem-in-r/seminr/issues/372), [#141](https://github.com/sem-in-r/seminr/issues/141), [#136](https://github.com/sem-in-r/seminr/issues/136), [#90](https://github.com/sem-in-r/seminr/issues/90)
- **Pattern**: Users want easy export and complete reporting
- **Recommendation**: Create comprehensive `export_results()` utilities

### Prediction Issues

- **Open**: [#222](https://github.com/sem-in-r/seminr/issues/222)
- **Recommendation**: Add input validation

### Summary Function Issues

- **Open**: [#341](https://github.com/sem-in-r/seminr/issues/341) (HTMT Inf with small samples)
- **Recommendation**: Edge case — small sample size handling
