# Planning: Fix macOS GitHub Actions pak Installation Failures

## Problem

The macOS-latest (release) job in GitHub Actions is failing intermittently during dependency installation. The error occurs in `pak::lockfile_install()`:

```
Error:
! error in pak subprocess
Caused by error in `file(con, "rb")`:
! cannot open the connection
```

**Observed behavior:**
- macOS-latest (release) failed
- macOS-latest (devel) passed with identical code
- Ubuntu jobs passed

## Root Cause (Confirmed)

This is a **known CRAN infrastructure issue**, not a problem with our code or workflow.

CRAN's macOS binary metadata intermittently becomes out of sync with actual package files. The metadata reports old package versions (e.g., `later 1.4.4`) but the binary files have already been updated (e.g., `later_1.4.5.tgz`). Pak tries to download the old version which no longer exists, causing the "cannot open the connection" error.

### Upstream Issues Tracking This

- [r-lib/actions#1041](https://github.com/r-lib/actions/issues/1041) - Exact same error reported
- [r-lib/actions#1040](https://github.com/r-lib/actions/issues/1040) - CRAN metadata inconsistencies workaround
- [r-lib/pak#840](https://github.com/r-lib/pak/issues/840) - pak workaround in progress
- [yihui/litedown#112](https://github.com/yihui/litedown/issues/112) - Detailed root cause analysis

### Why It Recurs

The CRAN maintainer (@s-u) fixes specific instances when reported, but the issue recurs each time packages are updated and mirrors don't sync properly.

## Changes Made to Workflow

The following changes were made during investigation. Some are worth keeping, others should be reverted once the upstream issue is resolved.

### KEEP: Added `workflow_dispatch` trigger
```yaml
on:
  ...
  workflow_dispatch:
```
**Why keep**: Allows manually triggering CI from GitHub Actions UI - useful for debugging without code changes.

### KEEP: Updated `setup-pandoc` from v1 to v2
```yaml
- uses: r-lib/actions/setup-pandoc@v2  # was @v1
```
**Why keep**: v1 is deprecated; v2 is the current version.

### REVERTED: Attempted fixes that didn't work
These were tried but didn't help and have been removed:
- `cache: "never"` - didn't fix the issue, only slowed CI
- `pak-version: devel` - didn't fix the issue
- `extra-packages: any::psych` - didn't fix the issue

## Upstream Status (checked 2026-02-20)

All three upstream issues remain **open with no fix merged**:
- [r-lib/pak#840](https://github.com/r-lib/pak/issues/840) - Still open. Gaborcsardi says the fix needs to happen at the resolution phase, making it complex. No PR created.
- [r-lib/actions#1040](https://github.com/r-lib/actions/issues/1040) - Still open. As of 2026-02-11, Gaborcsardi is monitoring with a gist but taking a "wait and see" approach.
- [r-lib/actions#1041](https://github.com/r-lib/actions/issues/1041) - Still open. Specific instances self-resolved but no code fix applied.

The pak maintainer considers this a CRAN-side bug that is "probably not going to be fixed" upstream. The issue can affect either macOS release or devel — it depends on which R version's binary repo has a package update with lagging metadata at that moment.

## Workaround Options (trying one at a time)

### Option 1: Use Posit Package Manager (PPM) for macOS — TRY FIRST

PPM serves macOS binaries (x86 and arm64) with its own metadata, independent of CRAN's mirrors. We already use PPM for Ubuntu via the `rspm` matrix setting. The `setup-r` action's `use-public-rspm` input only covers Linux/Windows, but we can manually set the CRAN repo to `https://packagemanager.posit.co/cran/latest` for macOS using the `cran` input of `setup-r` or by setting `options(repos = ...)`.

**Status:** Tried (run 22223705978, 2026-02-20). **Solves the original CRAN metadata issue.** macOS (release) passed. macOS (devel) failed with a *different* error: `data.table` source build failed due to missing `libintl.h` (gettext header). PPM doesn't have devel binaries for `data.table`, so pak fell back to source compilation which requires system headers not present on the runner. This is unrelated to the CRAN metadata sync problem — it's a missing system dependency for source builds on R-devel.

**Resolution:** Removed macOS devel from the CI matrix. PPM lacks devel binaries, and source builds will keep failing on ad hoc missing system libraries. R-devel regressions are still caught by Ubuntu devel. Final CI matrix: macOS release, Ubuntu release, Ubuntu devel.

### Option 2: Fall back to `install.packages()` for macOS
The old `remotes::install_deps()` approach (still commented out in workflow) uses `install.packages()` under the hood. Unlike pak, `install.packages()` falls back to source compilation when a binary isn't found, making it resilient to metadata mismatches. Remotes caused errors on Ubuntu, but we could conditionally use remotes for macOS only while keeping pak for Ubuntu.

**Status:** Not yet tried

### Option 3: Accept intermittent failures (status quo)
Continue with pak + CRAN and re-run failed jobs when they hit the metadata window. The issue is intermittent and self-resolving within hours.

**Status:** Current fallback

## Historical Context

The old `remotes::` based approach (commented out in workflow) was replaced with `r-lib/actions/setup-r-dependencies@v2` because remotes was causing errors on Ubuntu. Don't revert to remotes without understanding why it was changed.

## Files Modified

- `.github/workflows/rcmdcheck.yml`
