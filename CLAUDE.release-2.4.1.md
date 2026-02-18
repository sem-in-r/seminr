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
- [ ] 9. Verify win-devel results
- [x] 10. Submit mac check
- [x] 11. Verify mac check results
- [x] 12. Reverse dependency check
- [ ] 13. Bump version
- [ ] 14. Update cran-comments.md ⚠️ blocked on step 9 (win-devel results)
- [ ] 15. Final check
- [ ] 16. Commit release changes
- [ ] 17. Submit to CRAN (update GitHub issue)

## Post-Accept

- [ ] 18. Confirm acceptance
- [ ] 19. Create GitHub release
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
- Win-builder R-devel: submitted 2026-02-18, results to nicholasdanks@hotmail.com
- macOS builder: submitted 2026-02-18 — **passed** (0 errors, 0 warnings, 0 notes; macOS 14.4, R-devel, M1). Results at <https://mac.R-project.org/macbuilder/results/1771381799-81ffc16ea0293d81/>
- Revdep check: 1 reverse dependency (`seminrExtras 0.9.0`) — **0 new problems**, 0 failures. Existing error in CRAN version (not caused by our changes).
