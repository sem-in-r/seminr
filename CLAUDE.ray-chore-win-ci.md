# Plan: Reintroduce Windows into GitHub Actions CI

**Branch:** `ray/chore-win-ci`
**Date:** 2026-02-20

## Issue Summary

Windows CI (`windows-latest`, R release) has been commented out of the GitHub Actions matrix since at least the `Composite predict (#366)` merge in September 2025. The current workflow has custom Windows-specific build/test steps (`BuildWin`/`TestWin`) that use `R CMD INSTALL` + `devtools::test()` instead of the standard `R CMD check --as-cran`. This is a divergence from the standard r-lib/actions workflow and means Windows is not being checked at all.

**Goal:** Reintroduce Windows into CI using the modern r-lib/actions approach, aligned with the standard `check-standard.yaml` reference workflow.

## Relevant Files

- `.github/workflows/rcmdcheck.yml` — the CI workflow (primary file to change)
- `R/library_parallel.R` — parallel cluster setup (already handles Windows PSOCK + library path propagation for issue #318)
- `R/estimate_bootstrap.R` — uses `setup_parallel_cluster()` for parallel bootstrap
- `R/feature_plspredict.R` — uses `setup_parallel_cluster()` for LOOCV
- `DESCRIPTION` — pure R package (no compiled code), so Rtools is not critical

## Key Findings from Research

1. **The standard r-lib/actions `check-standard.yaml`** includes `windows-latest` with no special steps — the same `setup-r@v2`, `setup-r-dependencies@v2`, and `check-r-package@v2` actions work uniformly across all platforms.

2. **`use-public-rspm: true`** in `setup-r@v2` handles binary package repos for all platforms (Windows, macOS, Linux), eliminating the need for manual PPM/RSPM configuration.

3. **`r-lib/actions/check-r-package@v2`** replaces the manual `rcmdcheck::rcmdcheck()` call and custom `BuildWin`/`TestWin` steps, providing uniform `R CMD check` across all OSes.

4. **No known blocking issues** for Windows + r-lib/actions as of Feb 2026. The open issues (#1038, #1033, #999) do not affect a pure-R package on `windows-latest` with R release.

5. **Parallel tests** should work fine: `R CMD check` installs the package into a temp library before running tests, so parallel PSOCK workers calling `library(seminr)` will find it. The `library_parallel.R` helper already propagates `.libPaths()` to workers.

6. **Obsolete env vars and steps** in the current workflow can be cleaned up:
   - `R_REMOTES_NO_ERRORS_FROM_WARNINGS` — no longer needed (pak replaces remotes)
   - `RSPM` matrix variable — replaced by `use-public-rspm: true`
   - Custom macOS PPM step — replaced by `use-public-rspm: true`
   - `BuildWin`/`TestWin` steps — replaced by `check-r-package@v2`
   - Commented-out old manual dependency management block — dead code

## Test Plan

**No new R package tests needed.** This is a CI infrastructure change. Verification is done by:

- Pushing the branch and confirming all 4 matrix jobs pass (macOS, Windows, Ubuntu release, Ubuntu devel)
- Specifically verifying the Windows job runs `R CMD check --as-cran` successfully

**Manual verification checklist:**
- [ ] Windows job completes the check step
- [ ] No test failures (especially `test-plspredict.R` which uses parallel workers)
- [ ] No NOTEs specific to Windows (e.g., path issues, encoding)
- [ ] Other 3 platforms remain green (no regressions from cleanup)

## Implementation Steps

**Note:** Update this planning document at the completion of every phase to reflect progress and any new findings.

### Phase 1: Modernize workflow to standard r-lib/actions pattern -- DONE

- [x] Add `windows-latest` R release to the matrix
- [x] Add `use-public-rspm: true` to the `setup-r@v2` step
- [x] Replace the manual `rcmdcheck::rcmdcheck()` Check step and custom `BuildWin`/`TestWin` steps with `r-lib/actions/check-r-package@v2`
- [x] Remove the custom macOS PPM configuration step (superseded by `use-public-rspm`)
- [x] Remove obsolete `R_REMOTES_NO_ERRORS_FROM_WARNINGS` env var
- [x] Remove the `RSPM` matrix variable and env reference (superseded by `use-public-rspm`)
- [x] Remove the commented-out old manual dependency management block (dead code)
- [x] Remove the manual `upload-artifact` step (`check-r-package@v2` handles artifacts internally)
- [x] Add `permissions: read-all` for security best practice
- [x] Add `R_KEEP_PKG_SOURCE: yes` env var
- [x] Add `http-user-agent: 'release'` for Ubuntu devel config
- [x] Update `CLAUDE.md` CI/CD section to reflect Windows inclusion

### Phase 2: Verify -- DONE

- [x] Push branch and trigger CI
- [x] Confirm all 4 jobs pass (macOS-latest release, windows-latest release, ubuntu-latest release, ubuntu-latest devel)
- [x] Review Windows job logs for any warnings or notes
- PR #397 created; all 4 matrix jobs green

## Proposed Workflow (target state)

```yaml
on:
  push:
    branches:
      - master
  pull_request:
    branches:
      - '**'
      - '!*_noci'
  workflow_dispatch:

name: R-CMD-check

permissions: read-all

jobs:
  R-CMD-check:
    runs-on: ${{ matrix.config.os }}
    name: ${{ matrix.config.os }} (${{ matrix.config.r }})

    strategy:
      fail-fast: false
      matrix:
        config:
          - {os: macOS-latest, r: 'release'}
          - {os: windows-latest, r: 'release'}
          - {os: ubuntu-latest, r: 'release'}
          - {os: ubuntu-latest, r: 'devel', http-user-agent: 'release'}

    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}
      R_KEEP_PKG_SOURCE: yes

    steps:
      - uses: actions/checkout@v4

      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: ${{ matrix.config.r }}
          http-user-agent: ${{ matrix.config.http-user-agent }}
          use-public-rspm: true

      - uses: r-lib/actions/setup-pandoc@v2

      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-packages: any::rcmdcheck
          needs: check

      - uses: r-lib/actions/check-r-package@v2
        with:
          upload-snapshots: true
          build_args: 'c("--no-manual","--compact-vignettes=gs+qpdf")'
```

## Open Questions / Risks

1. **`--compact-vignettes=gs+qpdf`**: The standard workflow passes this build arg. It requires `ghostscript` and `qpdf` to be available. These are pre-installed on GitHub Actions runners, but worth confirming they are available on Windows. If they cause issues, we can remove this arg (it only affects vignette PDF compression). Alternative: use `--no-build-vignettes` if we hit problems.

2. **`upload-snapshots: true`**: The `check-r-package@v2` action can upload vdiffr snapshots on failure. Since seminr uses vdiffr for visual tests, this is actually a nice improvement — but only works if vdiffr test expectations exist for the check context.

3. **The `upload-artifact` step**: The current workflow has a manual artifact upload step for check results. The `check-r-package@v2` action handles artifact upload internally, so the manual step can likely be removed. Need to verify.
