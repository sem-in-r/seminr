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

## Recommended Action

**Wait for upstream fix.** This is a CRAN infrastructure issue being actively addressed:
- Monitor [r-lib/pak#840](https://github.com/r-lib/pak/issues/840) for pak-level workaround
- The issue typically resolves within hours/days when CRAN metadata syncs
- Re-run failed CI jobs when this happens

## Historical Context

The old `remotes::` based approach (commented out in workflow) was replaced with `r-lib/actions/setup-r-dependencies@v2` because remotes was causing errors on Ubuntu. Don't revert to remotes without understanding why it was changed.

## Files Modified

- `.github/workflows/rcmdcheck.yml`
