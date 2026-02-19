# CRAN Release: seminr 2.4.1

release_type: patch

## Submit

- [x] 1. Create GitHub release issue
- [x] 2. Confirm branch and starting state
- [x] 3. Update dependencies
- [x] 4. Polish NEWS.md
- [x] 5. Check URLs
- [x] 6. Rebuild README
- [x] 7. Run local R CMD check
- [x] 8. Submit win-devel check
- [x] 9. Verify win-devel results
- [x] 10. Submit mac check
- [x] 11. Verify mac check results
- [x] 12. Reverse dependency check
- [x] 13. Bump version
- [x] 14. Update cran-comments.md
- [x] 15. Final check
- [x] 16. Commit release changes
- [x] 17. Submit to CRAN (update GitHub issue)

## Resubmission (CRAN requested changes)

- [x] R1. Reduce test timings (CRAN: 493s → need significant reduction)
  - [x] R1a. Consolidate test-summary.R: shared base PLS model, nboot 500→200 for reporting tests
  - [x] R1b. Consolidate test-plspredict.R: eliminate duplicate two_stage model estimation
  - [x] R1c. Regenerate fixtures for nboot=200 changes
  - [x] R1d. Add skip_on_cran() for heavy tests (Tier 1+2 + plot bootstraps)
- [x] R2. Bump patch version (2.4.1 → 2.4.2)
- [x] R3. Run local R CMD check (0 errors, 0 warnings, 0 notes; 128s with all tests)
- [x] R4. Update cran-comments.md (add resubmission note)
- [x] R5. Final check (same as R3 — cran-comments.md is in .Rbuildignore)
- [x] R6. Commit resubmission changes
- [x] R7. Resubmit to CRAN

## Post-Accept

- [x] 18. Confirm acceptance
- [x] 19. Create GitHub release
- [ ] 20. Bump to dev version
- [ ] 21. Push all changes
- [ ] 22. Clean up (close GitHub issue)

## Notes

> When tool output contains URLs, email addresses, or other references the developer needs to act on, always explicitly present them — do not assume the developer saw raw tool output.

- GitHub release issue: <https://github.com/sem-in-r/seminr/issues/392>
- URL check: 2 DOI links return 403 (publisher blocks automated requests) — false positives, safe to ignore
- R CMD check: moved knitr/rmarkdown/webp from Imports to Suggests (unused import NOTE); fixed .Rbuildignore regex for CLAUDE.*.md files
- R CMD check remaining: version WARNING (expected pre-bump) and .git NOTE (build artifact only); added ^\.git$ to .Rbuildignore

## Resolved Questions

- **`testthat` in Imports** (resolved): Moved `check_test_plot` and `str_standardise` from `R/plot_test_utils.R` to `tests/testthat/helper-plotutils.R`, deleted the source file, removed the export from NAMESPACE, and moved `testthat` from Imports to Suggests. All 254 tests pass.
- Win-builder R-devel: submitted 2026-02-18 — **passed** (0 errors, 0 warnings, 0 notes; only WARNING was version match which is expected pre-bump). Results at <https://win-builder.r-project.org/xc41MlR6xrC3/>
- macOS builder: submitted 2026-02-18 — **passed** (0 errors, 0 warnings, 0 notes; macOS 14.4, R-devel, M1). Results at <https://mac.R-project.org/macbuilder/results/1771381799-81ffc16ea0293d81/>
- Revdep check: 1 reverse dependency (`seminrExtras 0.9.0`) — **0 new problems**, 0 failures. Existing error in CRAN version (not caused by our changes).
- CRAN submission: maintainer ran `devtools::submit_cran()`; `CRAN-SUBMISSION` file committed and pulled.
- **CRAN feedback (2026-02-18)**: Tests take 493s. Requested: reduce timings using small toy data, fewer iterations, or conditionally skip slow tests via environment variable.
- **Strategy**: (C) Consolidate tests with shared setups first, then (A) skip_on_cran() for remaining slow tests if needed.
- **Baseline timing (local, 2026-02-18)**:
  - Total: 131s local (493s on CRAN — ~3.8x slower hardware)
  - Top offenders:
    - test-plspredict.R: 91.5s (70% of total!) — LOOCV with 344 folds
    - test-bootstrap.R: 10.7s — 6 bootstrap_model() calls × 200 resamples
    - test-summary.R: 6.4s — 3 bootstrap_model() calls × 500 resamples
    - test-plot-hoc-2stage-interaction.R: 4.7s — bootstrap + HOC estimation
    - test-plot-interaction.R: 2.9s
    - Everything else: <2s each
- **After consolidation (Strategy C)**: 127.6s local (~3.5s saved)
  - test-summary.R: 6.4s → 4.6s (shared base model + nboot 500→200 for reporting tests; kept nboot=500 for statistical t/p-value tests)
  - test-plspredict.R: 91.5s → 89.6s (eliminated duplicate two_stage estimation)
  - Full analysis showed cross-file consolidation is limited: all 6 bootstraps in test-bootstrap.R are genuinely distinct (different weights, construct types, inner_weights, structural models). See CLAUDE.test-consolidation-plan.md for details.
- **skip_on_cran() applied (Tier 1+2 + plot bootstraps)**:
  - test-plspredict.R: LOOCV section (344-fold predict_pls ×2) wrapped in skip_on_cran test_that; non-LOOCV tests (two_stage, error handling, rowname regression) still run on CRAN
  - test-bootstrap.R: entire file skipped (file-level skip_on_cran); bootstrap mechanism still tested via test-summary.R
  - 5 plot files with bootstrap: test-plot-bootstrapped.R, test-plot-hoc-2stage-interaction.R (2nd test only), test-plot-interaction.R, test-plot-htmt.R, test-plot-themes.R (2nd test only)
  - Estimated CRAN time: ~91s (down from 486s)
  - All 254 tests still pass locally; 7 skips confirmed in CRAN simulation
