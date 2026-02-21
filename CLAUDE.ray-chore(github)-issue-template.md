# Branch Plan: ray/chore(github)-issue-template

**Issue:** #313 — Update issue templates
**Branch:** `ray/chore(github)-issue-template`
**Status:** Complete (pending manual verification after push)

## Issue Summary

GitHub issue #313 requests creating issue templates for the SEMinR repository. Currently there is no `.github/ISSUE_TEMPLATE/` directory. Analysis of ~100 historical issues shows:

- **Bug reports (~60%)**: Often missing reproducible examples, version info, or data descriptions
- **Feature requests (~15%)**: New statistics, export capabilities, new model types
- **Usage questions (~15%)**: Better suited for GitHub Discussions
- **Documentation/environment (~10%)**: Rare enough to fold into bug/feature categories

The biggest quality gaps: no reproducible example, no SEMinR/R version, screenshots instead of code, no data description.

## Files to Create

All new files — no existing files to modify (except CLAUDE.local.md for plan reference):

| File | Purpose |
|------|---------|
| `.github/ISSUE_TEMPLATE/bug_report.yml` | Bug report form (YAML issue form, not markdown) |
| `.github/ISSUE_TEMPLATE/feature_request.yml` | Feature request form |
| `.github/ISSUE_TEMPLATE/config.yml` | Redirects usage questions to Discussions; disables blank issues |

## Test Plan

These are GitHub YAML configuration files, not R code — no testthat tests apply. Verification:

- [x] Validate YAML syntax of all three files (Ruby YAML parser — all pass)
- [ ] Manual: After pushing, visit the repo's "New Issue" page to confirm templates render correctly and fields are required as expected

## Implementation Steps

- [x] Create `.github/ISSUE_TEMPLATE/` directory
- [x] Create `bug_report.yml` with fields: description, expected behavior, reproducible example (required, with SEMinR placeholder code), error message/traceback, version info (required), data description
- [x] Create `feature_request.yml` with fields: problem statement (required), proposed solution, alternatives considered, area dropdown (required)
- [x] Create `config.yml` to disable blank issues and add contact links redirecting usage/book questions to GitHub Discussions
- [x] Validate YAML syntax of all files
- [x] Commit changes

## Design Decisions

- **YAML forms (not markdown templates)**: Fields can't be skipped; dropdowns and labels reduce triage effort
- **Reproducible example required for bugs**: Single biggest quality gap in historical issues; placeholder shows the pattern using built-in `mobi` dataset
- **Version info required**: Many issues turn out to be fixed in newer versions
- **`blank_issues_enabled: false`**: Forces structured submissions; usage questions redirected to Discussions
- **No separate documentation template**: Rare enough to file as bugs or features
- **Contact links**: Redirect "how do I..." and book/course questions to GitHub Discussions

## Open Questions

- None currently — templates were designed based on analysis of historical issue patterns
