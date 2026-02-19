# SEMinR Completed Issues & PRs

**Repository**: [sem-in-r/seminr](https://github.com/sem-in-r/seminr)
**Purpose**: Archive of closed issues, merged PRs, and past releases. See [CLAUDE.chore-issues.md](CLAUDE.chore-issues.md) for active work.

---

## Releases

### v2.4.2 (2026-02-19)

Released to CRAN via v2.4.1 ([PR #392](https://github.com/sem-in-r/seminr/pull/392)) then v2.4.2 (test consolidation for CRAN timing). Tagged [v2.4.2](https://github.com/sem-in-r/seminr/releases/tag/v2.4.2).

**Fixes included:**

| PR | Fixes | Author | Notes |
| --- | --- | --- | --- |
| [**#379**](https://github.com/sem-in-r/seminr/pull/379) | [#377](https://github.com/sem-in-r/seminr/issues/377) (VIF output) | @soumyaray | Fix vif_items named list structure |
| [**#383**](https://github.com/sem-in-r/seminr/pull/383) | [#369](https://github.com/sem-in-r/seminr/issues/369), [#373](https://github.com/sem-in-r/seminr/issues/373) (HOC summary) | @soumyaray | Fix + linting for HOC summary |
| [**#388**](https://github.com/sem-in-r/seminr/pull/388) | PLSpredict parallel testing | @soumyaray | Fix parallel worker behavior on local macOS |
| [**#380**](https://github.com/sem-in-r/seminr/pull/380) | CI workflow | @soumyaray | Fix GitHub Actions for Ubuntu 24.04 |
| [**#389**](https://github.com/sem-in-r/seminr/pull/389) | [#226](https://github.com/sem-in-r/seminr/issues/226) (Plot symbols) | @soumyaray | Fix BMP Greek letters for λ, β, γ in plot exports; capital R² |
| [**#390**](https://github.com/sem-in-r/seminr/pull/390) | [#347](https://github.com/sem-in-r/seminr/issues/347) (predict_pls rownames) | @soumyaray | Regression test confirming fix from PR #368 |

### v2.4.0 (2026-02-04)

Released to master via [PR #385](https://github.com/sem-in-r/seminr/pull/385).

---

## Closed Issues by Problem Area

### HOC / Summary

Issues related to `summary()` failures with higher-order constructs and general summary regressions.

| Issue | Title | Fixed By | Release |
| --- | --- | --- | --- |
| [**#369**](https://github.com/sem-in-r/seminr/issues/369) | summary() regression in v2.3.7 | PR #383 | v2.4.2 |
| [**#373**](https://github.com/sem-in-r/seminr/issues/373) | HOC summary undefined columns | PR #383 | v2.4.2 |
| [**#353**](https://github.com/sem-in-r/seminr/issues/353) | summary() subscript out of bounds | Closed — model misspecification | — |
| [**#240**](https://github.com/sem-in-r/seminr/issues/240) | summary() fails with factor column | PR #285 | v2.3.1 |

**Related PRs:** [#383](https://github.com/sem-in-r/seminr/pull/383) (fix), [#381](https://github.com/sem-in-r/seminr/pull/381) (test), [#374](https://github.com/sem-in-r/seminr/pull/374) (superseded by #383)

### Prediction

Issues related to `predict_pls()` errors with non-standard data.

| Issue | Title | Fixed By | Release |
| --- | --- | --- | --- |
| [**#347**](https://github.com/sem-in-r/seminr/issues/347) | predict_pls subscript error | PR #368 (regression test in PR #390) | v2.4.2 |
| [**#298**](https://github.com/sem-in-r/seminr/issues/298) | predict_pls() rownames error | PR #368 | v2.4.2 |

**Related PRs:** [#368](https://github.com/sem-in-r/seminr/pull/368) (fix), [#390](https://github.com/sem-in-r/seminr/pull/390) (regression test)

### Reporting / Output

Issues related to model output formatting and structure.

| Issue | Title | Fixed By | Release |
| --- | --- | --- | --- |
| [**#377**](https://github.com/sem-in-r/seminr/issues/377) | VIF output structure inconsistent | PR #379 | v2.4.2 |

**Related PRs:** [#379](https://github.com/sem-in-r/seminr/pull/379) (fix)

### Plot / Visualization

Issues related to plot rendering and symbol display.

| Issue | Title | Fixed By | Release |
| --- | --- | --- | --- |
| [**#226**](https://github.com/sem-in-r/seminr/issues/226) | Lambda symbol not rendered | PR #389 | v2.4.2 |

**Related PRs:** [#389](https://github.com/sem-in-r/seminr/pull/389) (BMP Greek letters + capital R²)

### CI / Infrastructure

| Issue | Title | Fixed By | Release |
| --- | --- | --- | --- |
| — | GitHub Actions Ubuntu 24.04 | PR #380 | v2.4.2 |
| — | PLSpredict parallel testing on macOS | PR #388 | v2.4.2 |

---

## Closed PRs

### Superseded

| PR | Title | Superseded By |
| --- | --- | --- |
| [**#374**](https://github.com/sem-in-r/seminr/pull/374) | Fixed summary error (missing data + HOC) | PR #383 |
| [**#393**](https://github.com/sem-in-r/seminr/pull/393) | Custom confidence levels for plotting | PR #394 |

### Community PRs (closed without merge)

| PR | Author | Title | Notes |
| --- | --- | --- | --- |
| [**#391**](https://github.com/sem-in-r/seminr/pull/391) | @Marwolaeth | Fix column selection in report_missing | Closed without merge |
