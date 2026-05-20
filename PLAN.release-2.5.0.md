# CRAN Release: seminr 2.5.0

release_type: minor
status: PAUSED — waiting for seminrExtras 1.0.1 to be accepted on CRAN before resuming.

## Resume instructions

When seminrExtras 1.0.1 is on CRAN:

1. `revdepcheck::revdep_reset(); revdepcheck::revdep_check(num_workers = 4)` — should now report 0 new problems.
2. Update `cran-comments.md` to reflect the clean revdep run (remove the seminrExtras explanation paragraph; replace with "We saw 0 new problems / 0 failures to check").
3. Run Step 15 (final `devtools::check(remote = TRUE, manual = TRUE)`).
4. Re-submit win-devel (`devtools::check_win_devel()`) and mac (`devtools::check_mac_release()`) — the prior submissions were made before the new public-API exports were added.
5. Wait for results, then proceed with Steps 16 (commit) and 17 (submit to CRAN).

## Submit
- [x] 1. Create GitHub release issue
- [x] 2. Confirm branch and starting state
- [x] 3. Update dependencies
- [x] 4. Polish NEWS.md
- [x] 5. Check URLs
- [x] 6. Rebuild README
- [x] 7. Bump version
- [x] 8. Run local R CMD check
- [x] 9. Submit win-devel check
- [ ] 10. Verify win-devel results
- [x] 11. Submit mac check
- [x] 12. Verify mac check results
- [x] 13. Reverse dependency check
- [ ] 14. Update cran-comments.md
- [ ] 15. Final check
- [ ] 16. Commit release changes
- [ ] 17. Submit to CRAN (update GitHub issue)

## seminrExtras 1.0.1 fix plan (extract this for a PR)

Repo: <https://github.com/sem-in-r/seminrExtras>
Current CRAN version: 1.0.0
Target: 1.0.1 (patch release)

Root cause: seminr 2.5.0 refactored the internal helper `items_of_construct(construct, model)` (non-exported) into a public S3 generic `construct_items(model, construct_name)` (note container-first argument order). Three test lines in seminrExtras call the old internal helper via `seminr:::`, breaking R CMD check against seminr 2.5.0.

Required changes:

1. **`tests/testthat/test-cipma-comprehensive.R`** — replace the 3 internal calls:
   - Line 123: `items <- seminr:::items_of_construct("Image", pls_model)` → `items <- seminr::construct_items(pls_model, "Image")`
   - Line 561: `img_items <- seminr:::items_of_construct("Image", pls_hoc)` → `img_items <- seminr::construct_items(pls_hoc, "Image")`
   - Line 566: `exp_items <- seminr:::items_of_construct("Expectation", pls_hoc)` → `exp_items <- seminr::construct_items(pls_hoc, "Expectation")`

2. **`DESCRIPTION`** — bump seminr dependency floor:
   - `Imports: seminr` → `Imports: seminr (>= 2.5.0)` (the new `construct_items` is exported only from 2.5.0)
   - Bump `Version: 1.0.0` → `Version: 1.0.1`
   - Update `Date:` to submission date

3. **`NEWS.md`** — add new patch-version entry:

   ```markdown
   # seminrExtras 1.0.1

   ### Changed
   * Migrated test code off `seminr:::items_of_construct()` (non-exported internal,
     removed in seminr 2.5.0) to the new public S3 generic
     `seminr::construct_items(model, construct_name)`. Requires seminr (>= 2.5.0).
   ```

4. **Optional cleanup** (recommended but not required for the patch):
   - Delete the local `items_of_construct()` in `R/helpers.R:77` and replace its callers (`R/helpers.R:116, 126`, `R/feature_cipma.R:37,77,126`, `R/feature_pcm.R:96,153,192`, `R/feature_cta.R:125`) with `seminr::construct_items(model, construct)`. This removes a parallel maintenance surface.

5. **Verification before submission**:
   - `devtools::check()` — should pass cleanly with seminr 2.5.0 installed.
   - `urlchecker::url_check()`
   - Submit to win-builder if there are non-trivial changes; for a 3-line test fix the macOS builder is usually sufficient.

6. **CRAN submission**:
   - `cran-comments.md`: short note explaining this is a compatibility patch for seminr 2.5.0's new public API.
   - `devtools::submit_cran()`.

Once seminrExtras 1.0.1 is accepted on CRAN, return to this seminr 2.5.0 plan's "Resume instructions" section above.

## Post-Accept
- [ ] 18. Confirm acceptance
- [ ] 19. Create GitHub release
- [ ] 20. Bump to dev version
- [ ] 21. Clean up (close GitHub issue)
- [ ] 22. Merge release branch into master and develop

## Notes

- GitHub release issue: #415
- Previous release: 2.4.2
- Starting DESCRIPTION version: 2.4.2.9000
- Target version: 2.5.0
- Headline features: prediction support for all interaction methods (product_indicator, orthogonal, two_stage), quadratic term prediction, parallel k-fold cross-validation
- Step 5: Emerald DOI (10.1108/EJM-02-2019-0189) returned 403 to urlchecker — replaced with non-link `doi:` format in vignettes/SEMinR.Rmd:715. All 23 remaining URLs pass.
- Step 8 (local check, v2.5.0): 0 errors, 0 warnings, 3 NOTEs. Fixed by adding `^PLAN\..*\.md$` and `^\.lintr$` to `.Rbuildignore`. Verified empty tarball matches for `.lintr|PLAN|CLAUDE`. Remaining NOTE was harmless "unable to verify current time" (clock check).
- Step 9: win-devel submitted at ~12:29 AM; results expected at nicholasdanks@hotmail.com around 12:59 AM.
- Step 11: mac builder submitted at ~12:30 AM; results at <https://mac.R-project.org/macbuilder/results/1779294631-f1a73d6d1e214e63/> around 12:40 AM.
- Step 13 (revdep): 1 reverse dep (seminrExtras 1.0.0) has 2 test failures (`Error: object 'items_of_construct' not found`) caused by `seminr:::items_of_construct()` calls in `tests/testthat/test-cipma-comprehensive.R` lines 123, 561, 566. Production code in seminrExtras is unaffected (it has its own local copy). 623 tests pass, 0 new failures from our public-API exports.
- Pre-revdep API addition: exported 8 new public accessors (`construct_items`, `construct_names`, `construct_name`, `construct_mode`, `construct_type`, `all_factors`, `all_composites`, `all_non_interactions`) so that seminrExtras and others can migrate off `seminr:::` internals. NEWS.md updated to document the new public API surface.
- Step 12 (mac results, pre-export code): Status OK, 0/0/0 on R 4.6.0 / macOS Tahoe 26.2 ARM64. Will need a fresh submission after final check since 8 new exports were added.
- IMPORTANT: Win-devel (#9/10) and mac (#11/12) were submitted before the public-API export additions. After step 15 (final check), re-submit both before commit/submission to CRAN.
