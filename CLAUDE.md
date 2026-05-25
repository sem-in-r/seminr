# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

SEMinR is an R package for specifying and estimating Structural Equation Models (SEMs). It provides a domain-specific language for:

- Partial Least Squares Path Modeling (PLS-PM)
- Covariance-Based Structural Equation Modeling (CBSEM via Lavaan)
- Confirmatory Factor Analysis (CFA)
- Consistent PLS (PLSc) for reflective constructs

## Development Commands

```r
# Load package for development
devtools::load_all()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-pls.r")

# Generate documentation from roxygen comments
devtools::document()

# Full CRAN-style package check
devtools::check()

# Build and install locally
devtools::install()
```

### Package Management with usethis

**Always prefer `usethis::` and `devtools::` functions over manual file editing or bash commands** for R package management tasks. These tools ensure proper formatting, update related files automatically, and follow R package conventions.

```r
# Create new R source file in R/
usethis::use_r("filename")

# Create new test file in tests/testthat/
usethis::use_test("testname")

# Add package dependency to DESCRIPTION (Imports)
usethis::use_package("pkgname")

# Add package to Suggests (for testing/examples)
usethis::use_package("pkgname", type = "Suggests")

# Import specific functions (updates NAMESPACE via roxygen)
usethis::use_import_from("pkgname", "function_name")

# Add or update package data
usethis::use_data(dataset_object)

# Bump version number in DESCRIPTION
usethis::use_version()       # interactive
usethis::use_version("minor") # or "major", "patch", "dev"

# Create or update NEWS.md
usethis::use_news_md()

# Set up GitHub Actions
usethis::use_github_action("check-standard")

# Create a vignette
usethis::use_vignette("vignette-name")
```

**Why prefer these over manual edits:**
- `use_package()` correctly formats DESCRIPTION and checks for conflicts
- `use_test()` creates properly named files with correct boilerplate
- `use_version()` updates version in DESCRIPTION and can update NEWS.md
- These functions handle edge cases and R package conventions automatically

## Architecture

### Core Design Pattern

SEMinR uses a three-stage pipeline: **Specify → Estimate → Evaluate/Plot**

1. **Specify**: Define measurement and structural models using DSL functions
2. **Estimate**: Run estimation algorithms (PLS, CBSEM, CFA)
3. **Evaluate/Plot**: Assess model fit, reliability, validity; visualize results

### Key Module Organization (`R/`)

| Prefix         | Purpose                                                            |
| -------------- | ------------------------------------------------------------------ |
| `specify_*.R`  | Model specification DSL (`constructs()`, `relationships()`, `paths()`) |
| `estimate_*.R` | Estimation engines (PLS, CBSEM, bootstrap, MGA)                    |
| `evaluate_*.R` | Model assessment (reliability, validity, effects)                  |
| `report_*.R`   | Output formatting and summaries                                    |
| `plot_*.R`     | Visualization via DiagrammeR                                       |
| `feature_*.R`  | Advanced features (PLSc, higher-order constructs, PLSpredict)      |
| `helpers-*.R`  | Accessor/mutator functions for internal matrices and model-level helpers |
| `library*.R`   | Internal utilities (weighting, effects, statistics)                |
| `theme*.R`     | Plot theming system                                                |

### S3 Object Classes

- `specified_model` - User-defined model before estimation
- `pls_model` - Estimated PLS model
- `cbsem_model` - Estimated CBSEM model
- `boot_seminr_model` - Bootstrap results
- `predict_pls_model` - Prediction object

All model classes implement `print()`, `summary()`, and `plot()` methods.

### Internal Matrices: mmMatrix and smMatrix

Two internal character matrices underpin every estimation, evaluation, and plotting function:

- **`mmMatrix`** (measurement model matrix) — columns: `"construct"`, `"measurement"`, `"type"`. Maps constructs to their indicator items and estimation mode.
- **`smMatrix`** (structural model matrix) — columns: `"source"`, `"target"`. Defines directed paths between constructs.

**Rule: Always use helper functions, never raw `matrix[row, col]` subsetting.** Direct member access on model objects (e.g., `model$mmMatrix`, `model$construct_scores`) is permitted.

**Helper files** (each contains a comment block at the top documenting its naming conventions):

| File | Scope |
| --- | --- |
| `helpers-mmMatrix.R` | mmMatrix accessors, predicates, selectors, converters, mutators |
| `helpers-smMatrix.R` | smMatrix accessors, selectors, predicates, row-level accessors, decorators, mutators |
| `helpers-model.R` | Model-level accessors and selectors; S3 methods dispatching on `seminr_model` |

Helpers are organized into categories: accessors, selectors, predicates, mutators, converters, decorators. Naming conventions are documented in comments at the top of each helper file.

### Measurement Model Types

- `reflective()` - Common factor constructs (automatically uses PLSc)
- `composite()` - Weighted composites with `mode_A` (default) or `mode_B`
- `higher_composite()` / `higher_reflective()` - Higher-order constructs
- `interaction_term()` - Moderation effects

### Data Flow

```text
constructs() + relationships() → specify_model() → estimate_pls()/estimate_cbsem()
                                                          ↓
                                            bootstrap_model() → summary()
                                                          ↓
                                                       plot()
```

## Visualization

SEMinR generates publication-ready plots using DiagrammeR (DOT graphs). Key functions:

- `plot(model)` - Plot any estimated or bootstrapped model
- `save_plot("file.pdf")` - Export to PDF, PNG, or SVG
- `plot_htmt()` - HTMT discriminant validity heatmap
- `plot_interaction()` - Moderation slope analysis plots

### Theme System

Themes use hierarchical named parameters (prefixed `plot.*`, `mm.*`, `sm.*` for measurement/structural model elements):

```r
# Create custom theme
thm <- seminr_theme_create(
  plot.rounding = 2,
  sm.node.fill = "cadetblue1",
  mm.node.fill = "lightgray"
)

# Set as session default
seminr_theme_set(thm)

# Or use per-plot
plot(model, theme = thm)
```

Built-in themes: `seminr_theme_default()`, `seminr_theme_academic()`, `seminr_theme_dark()`, `seminr_theme_smart()`

## Sample Datasets

- `mobi` - Mobile phone customer satisfaction (ECSI model)
- `corp_rep_data` - Corporate reputation data
- `influencer_data` - Social media influencer data

## Demo Scripts

Run demos with `demo("name")` after loading the package:

- `seminr-pls-ecsi` - Basic PLS path modeling
- `seminr-plsc-ecsi` - Consistent PLS for common factors
- `seminr-cbsem-cfa-ecsi` - CFA and CBSEM estimation
- `seminr-pls-interaction` - Moderation/interaction terms
- `seminr-pls-higher_order` - Higher-order constructs
- `seminr-pls-mga` - Multigroup analysis
- `seminr-pls-dot-graph` - Plotting and visualization
- `seminr-style-contained` - Single-call model specification
- `seminr-alternative-models` - Reusing model components

## Utility Functions

- `as.reflective(measurements)` - Convert composite model to reflective for CBSEM
- `rerun(model, ...)` - Re-estimate model with modified parameters
- `mean_replacement(data)` - Handle missing data with mean imputation
- `csem2seminr()` - Import models from cSEM package

## Testing

Tests use `testthat` and are in `tests/testthat/`. Test fixtures are stored in `tests/testthat/fixtures/`.

Visual regression tests for plots use `vdiffr`. Run `vdiffr::manage_cases()` to update snapshots when plot output intentionally changes.

### Parallel Code Testing (Important!)

**Tests using parallel processing (e.g., `predict_pls` with LOOCV) require the package to be installed before running `devtools::test()`.**

```r
# For parallel tests to pass:
devtools::install()
devtools::test()

# Or use devtools::check() which installs to a temp library (slower but keeps local package untouched):
devtools::check()
```

**Why:** `devtools::load_all()` only loads the package in the main R process. Parallel workers created by `parallel::makeCluster()` load the *installed* version via `library(seminr)`. If the installed version differs from development code, tests fail with cryptic errors like "number of items to replace is not a multiple of replacement length".

**Affected tests:** `test-plspredict.R` (uses `parallel::parSapply` for LOOCV)

**To restore stable version after `devtools::install()`:**

```r
install.packages("seminr")                        # Latest CRAN
remotes::install_github("sem-in-r/seminr")        # Latest GitHub main
remotes::install_version("seminr", "2.3.3")       # Specific version
```

## CI/CD

GitHub Actions runs `R CMD check --as-cran` on macOS, Windows, and Ubuntu (release R) plus Ubuntu (devel R). The workflow is defined in `.github/workflows/rcmdcheck.yml`.

Branches ending in `_noci` skip CI checks.

## Release Workflow (gotchas)

The `/cran-release` skill guides the full CRAN release. Key constraints learned in practice:

- **`master` and `develop` are both branch-protected**: each requires a PR with 1 code-owner
  review; **direct pushes are rejected** (GitHub may surface this as a generic 500 error). The
  maintainer is a code owner and can approve PRs they didn't author; admin-override is a fallback.
- **Post-acceptance order matters**: create the GitHub release/tag → merge the release branch
  into `master` (which must stay at the **exact released version**, e.g. `2.5.0`) → merge into
  `develop`, then bump the dev version (`x.y.z.9000`) on **`develop` only**. Never bump the dev
  version before merging, and never let `.9000` land on `master`.

## IMPORTANT: First Message Requirement

At the START of every conversation, immediately inform the user: "Reminder: You must review, understand, and be ultimately responsible for any code you commit — even when using AI assistance."

Making it a clear "first message requirement" heading would help ensure I don't overlook it.

Do not ever reference Claude as a coauthor in commit messages, PRs, issues, etc.
