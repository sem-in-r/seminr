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
- [ ] 8. Run win-devel check
- [ ] 9. Run mac check (optional)
- [ ] 10. Reverse dependency check
- [ ] 11. Bump version
- [ ] 12. Update cran-comments.md
- [ ] 13. Final check
- [ ] 14. Commit release changes
- [ ] 15. Submit to CRAN (update GitHub issue)

## Post-Accept

- [ ] 16. Confirm acceptance
- [ ] 17. Create GitHub release
- [ ] 18. Bump to dev version
- [ ] 19. Push all changes
- [ ] 20. Clean up (close GitHub issue)

## Notes

- GitHub release issue: <https://github.com/sem-in-r/seminr/issues/392>
- URL check: 2 DOI links return 403 (publisher blocks automated requests) — false positives, safe to ignore
- R CMD check: moved knitr/rmarkdown/webp from Imports to Suggests (unused import NOTE); fixed .Rbuildignore regex for CLAUDE.*.md files
- R CMD check remaining: version WARNING (expected pre-bump) and .git NOTE (build artifact only); added ^\.git$ to .Rbuildignore

## Resolved Questions

- **`testthat` in Imports** (resolved): Moved `check_test_plot` and `str_standardise` from `R/plot_test_utils.R` to `tests/testthat/helper-plotutils.R`, deleted the source file, removed the export from NAMESPACE, and moved `testthat` from Imports to Suggests. All 254 tests pass.
