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

This suggests a transient/flaky issue rather than a code problem.

## Root Cause Analysis

1. **No committed lockfile**: The `setup-r-dependencies@v2` action creates lockfiles dynamically, which can lead to inconsistent package resolution across runs.

2. **Parallel installation race conditions**: `pak` installs packages in parallel, which can cause file I/O issues on macOS runners.

3. **Transitive dependencies**: Packages like `psych` (not a direct seminr dependency) are resolved at runtime and may have availability issues for specific R/OS combinations.

## Potential Solutions

### Option A: Simple Re-run (Status Quo)
- **Approach**: Accept occasional transient failures; re-run when they occur
- **Pros**: No code changes needed
- **Cons**: Annoying; blocks PRs until manual intervention

### Option B: Add Explicit Extra Packages
- **Approach**: Add problematic transitive dependencies to `extra-packages` in workflow
- **Code change**:
  ```yaml
  extra-packages: |
    any::rcmdcheck
    any::psych
  ```
- **Pros**: Simple change; helps pak resolve dependencies more reliably
- **Cons**: May need ongoing maintenance as dependencies change

### Option C: Commit a Lockfile
- **Approach**: Generate and commit `.github/pkg.lock` for reproducible builds
- **Steps**:
  1. Run `pak::lockfile_create()` locally
  2. Commit the lockfile
  3. Update workflow if needed
- **Pros**: Fully reproducible builds; eliminates resolution variability
- **Cons**: Lockfile needs periodic updates; may cause issues across R versions

### Option D: Add Retry Logic
- **Approach**: Wrap dependency installation in retry logic
- **Pros**: Handles transient failures automatically
- **Cons**: More complex workflow; masks real issues

## Recommended Approach

**Start with Option B** (add explicit extra packages) as it's low-risk and addresses the immediate issue. If failures persist, escalate to Option C (committed lockfile).

## Implementation Steps

1. [ ] Wait for re-run of current PR to confirm if issue is truly transient
2. [ ] If re-run fails again, implement Option B
3. [ ] Monitor CI stability over next few PRs
4. [ ] Consider Option C if issues persist

## Files to Modify

- `.github/workflows/rcmdcheck.yml` (lines 44-47)

## References

- Failed run: https://github.com/sem-in-r/seminr/actions/runs/21629028452
- r-lib/actions setup-r-dependencies: https://github.com/r-lib/actions/tree/v2/setup-r-dependencies
