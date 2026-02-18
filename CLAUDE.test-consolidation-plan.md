# Test Consolidation Plan — seminr 2.4.1 CRAN Resubmission

## Goal

Reduce test suite timing from 493s (CRAN) / 131s (local) to satisfy CRAN's request.

## Baseline Timing (local, 2026-02-18)

| File | Time | Operations |
|------|------|-----------|
| test-plspredict.R | 91.5s | 5 estimate_pls + 2 predict_pls(noFolds=344) + 1 predict_pls(noFolds=NULL) + 6 predict_pls(noFolds=10) |
| test-bootstrap.R | 10.7s | 6 estimate_pls + 6 bootstrap_model(nboot=200) |
| test-summary.R | 6.4s | 4 estimate_pls + 3 bootstrap_model(nboot=500) |
| test-plot-hoc-2stage-interaction.R | 4.7s | 2 estimate_pls + 1 bootstrap_model(nboot=100) |
| test-plot-interaction.R | 2.9s | 1 estimate_pls + 1 bootstrap_model(nboot=100) |
| test-plot-htmt.R | 2.0s | 1 estimate_pls + 1 bootstrap_model(nboot=100) |
| test-plot-themes.R | 1.9s | 1 estimate_pls + 1 bootstrap_model(nboot=100) |
| test-pls-mga.R | 1.9s | 1 estimate_pls + 1 estimate_pls_mga(nboot=100) |
| test-plsc-fsquared.R | 1.5s | 2 estimate_pls |
| test-plot-bootstrapped.R | 1.3s | 1 estimate_pls + 1 bootstrap_model(nboot=100) |
| test-plot-save-plot.R | 1.0s | 1 estimate_pls |
| Everything else | <5s | Fast tests |
| **Total** | **~131s** | |

## Exact Parameter Inventory of All Expensive Operations

### test-bootstrap.R — 6 distinct bootstraps

| # | Constructs | Weights | inner_weights | SM | nboot | Extra |
|---|-----------|---------|---------------|-----|-------|-------|
| B1 | 4 composite | mode_A | path_factorial | 3→Sat | 200 | set.seed(123) |
| B2 | 4 composite + 2 interaction(orthogonal) | correlation_weights / mode_A | path_factorial | 5→Sat | 200 | |
| B3 | 4 composite | correlation_weights | path_factorial | 3→Sat | 200 | |
| B4 | 4 reflective | — | path_weighting | 3→Sat | 200 | Also tests HTMT (reuses bootmodel) |
| B5 | 4 composite | mode_A | path_factorial | 3→Sat + Image→Exp | 200 | Different SM from B1 |
| B6 | 8 composite (corp_rep) | mode_B/default | path_factorial | complex | 200 | missing=-99, stopCriterion=1 |

**All 6 are genuinely different** (different weights, construct types, structural models, or data). No two can be merged.

### test-summary.R — 3 distinct bootstraps + 1 PLS-only

| # | Constructs | Weights | inner_weights | SM | nboot | Extra |
|---|-----------|---------|---------------|-----|-------|-------|
| S1 | 4 composite | mode_A | path_weighting | 3→Sat | — | PLS-only (no bootstrap) |
| S2 | 4 composite | mode_A | path_weighting | 3→Sat | 500 | |
| S3 | 4 composite | mode_A | path_weighting | Sat←[Image,Exp], Sat→Value | 500 | Different SM from S2 |
| S4 | 2 reflective + 2 composite | mode_A | path_weighting | 3→Sat | 500 | Mixed measurement |

**S1 and S2 share the same base PLS model** (S2 adds bootstrap). Minor optimization possible.

**S2 vs B1**: Same constructs/weights but different `inner_weights` (path_weighting vs path_factorial). **Cannot merge.**

### test-plspredict.R — 5+ distinct estimate_pls calls

| # | Data | Constructs | inner_weights | SM | Prediction | Extra |
|---|------|-----------|---------------|-----|-----------|-------|
| P1 | corp_rep | 8 composite (mode_B) | default (path_weighting) | complex | predict_DA/EA, noFolds=344 | missing=-99 |
| P2 | corp_rep | 4 composite + two_stage | default | 4→COMP | predict() with testData | missing=-99 |
| P3 | corp_rep | 4 composite + orthogonal | default | 4→COMP | expect_error | missing=-99 |
| P4 | corp_rep | 4 composite + product_indicator | default | 4→COMP | expect_error | missing=-99 |
| P5 | corp_rep | 4 composite + two_stage | default | 4→COMP | predict_DA, noFolds=NULL | missing=-99, SAME as P2 |
| P6 | corp_rep[1:100] | 6 composite | default | 2-level | predict_DA, noFolds=10 | rowname variants ×3 |

**P1 vs B6**: Same data/constructs/SM but different `inner_weights` (default vs path_factorial) and different `stopCriterion`. **Cannot merge.**

**P2 and P5 are the same base model** — only prediction differs. Could share the base estimate_pls. Minor saving.

### Plot test bootstraps — 5 distinct setups

| # | File | Constructs | nboot | Unique feature |
|---|------|-----------|-------|----------------|
| PL1 | test-plot-bootstrapped.R | 4 mixed (unit_weights, reflective, composite) | 100 | Unique mix |
| PL2 | test-plot-interaction.R | 4 reflective + product_indicator interaction | 100 | Unique interaction |
| PL3 | test-plot-htmt.R | 7 reflective, full ECSI paths | 100 | HTMT plotting |
| PL4 | test-plot-themes.R | 4 reflective, 3 paths | 100 | Theme testing |
| PL5 | test-plot-hoc-2stage-interaction.R | 10 constructs (composites + 2 HOCs + interaction) | 100 | HOC+interaction |

**All 5 use different model specs.** They're testing plot rendering for different model configurations — they need different models.

**PL3 base model = same as test-plot-save-plot** (7 reflective, full ECSI). Could share base estimate_pls. Saves ~1s.

## Consolidation Findings

### Cross-file sharing opportunities are LIMITED

Your concern was correct — the tests deliberately use different estimation parameters:
- `inner_weights`: path_factorial (test-bootstrap) vs path_weighting (test-summary) vs default (test-plspredict)
- Construct types: composite mode_A vs correlation_weights vs reflective vs mixed
- Structural models: different paths in nearly every test
- Data: mobi vs corp_rep_data

### What CAN be consolidated (small wins)

| Opportunity | Files | Saving (est.) |
|-------------|-------|---------------|
| S1/S2 share base PLS model | test-summary.R (within file) | ~0.1s |
| P2/P5 share base PLS model | test-plspredict.R (within file) | ~0.2s |
| PL3 + save-plot share base PLS model | test-plot-htmt.R + test-plot-save-plot.R | ~1.0s |
| **Total estimated saving from consolidation** | | **~1.3s local / ~5s CRAN** |

### Why consolidation alone is insufficient

Even with perfect consolidation, we'd save ~5s on CRAN (493s → ~488s). This is because:
1. **test-plspredict.R alone is 91.5s** (70% of total) — all its models are unique
2. **test-bootstrap.R has 6 genuinely distinct models** that cannot be merged
3. **test-summary.R has 3 genuinely distinct bootstraps** with different model specs
4. **Plot tests each need their specific model configuration** to test rendering

## Recommended Strategy

### Phase 1: Do the small consolidation wins (Strategy C)
- Within-file dedup in test-summary.R and test-plspredict.R
- Share base model between test-plot-htmt.R and test-plot-save-plot.R via helper

### Phase 2: skip_on_cran() for heavy tests (Strategy A)
This is where the real savings come from:

| Candidate for skip_on_cran() | Local time | CRAN time (est.) | What we lose on CRAN |
|-------------------------------|-----------|-------------------|---------------------|
| test-plspredict.R (LOOCV portions) | 91.5s | ~350s | Prediction accuracy validation |
| test-bootstrap.R | 10.7s | ~41s | Bootstrap matrix validation |
| test-summary.R (bootstrap portions) | ~5s | ~19s | Bootstrap summary validation |
| Plot bootstrap tests (4 files) | ~9s | ~34s | Boot plot rendering checks |
| **Total potential CRAN savings** | | **~444s** | |

### Alternative: Reduce nboot on CRAN instead of skipping entirely
- Could keep bootstrap tests but with nboot=20 instead of 200/500
- Tests would still validate bootstrap mechanics, just with less statistical power
- This preserves _some_ CRAN coverage while dramatically cutting time

## Decision needed

Which combination of Phase 1 + Phase 2 to pursue?
