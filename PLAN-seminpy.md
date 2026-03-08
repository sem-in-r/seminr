# SEMinPy: Python Port of SEMinR

## Comprehensive Plan for Converting SEMinR (R) → seminpy (Python)

---

## 1. Market Gap Analysis

### Existing Python SEM Packages

| Package | Type | Status | Key Limitations |
|---------|------|--------|-----------------|
| **plspm** (Google Cloud) | PLS-PM only | v0.5.7, last commit Jun 2024, 49 stars | No PLSc, no interaction terms, no HTMT/VIF/f²/Q²/q², no MGA, no PLSpredict, no CBSEM. Port of old R plspm, NOT seminr. |
| **semopy** | CB-SEM only | v2.2.2, active | No PLS-SEM at all. Lavaan-like syntax. Random effects, regularization. |
| **pylspm** | PLS-PM only | Low activity | Depends on Gurobi (commercial solver). Limited features. |

### The Gap

**No Python package offers the full feature set of SEMinR:**
- Unified PLS + PLSc + CBSEM in one API
- Natural-feeling DSL for model specification
- Bootstrap inference with BCa confidence intervals
- Interaction terms (product indicator, two-stage, orthogonal)
- Higher-order constructs (two-stage)
- Multi-group analysis (MGA)
- PLSpredict with cross-validation
- Publication-ready visualization
- Comprehensive evaluation metrics (HTMT, VIF, f², rhoA, rhoC, AVE)

### Strategic Positioning

seminpy would be the **first Python package to offer R/SmartPLS-equivalent PLS-SEM** with a modern Pythonic API. Target users: researchers in business, marketing, IS, psychology, and education who work in Python/Jupyter environments.

---

## 2. Design Philosophy

### Core Principles

1. **API Fidelity**: Preserve seminr's natural-feeling DSL. Users familiar with seminr should recognize the API immediately.
2. **Pythonic Idioms**: Use Python conventions (snake_case, classes, type hints, context managers) while keeping the domain language identical.
3. **NumPy/Pandas Native**: All matrices as numpy arrays with labeled axes; all data as pandas DataFrames.
4. **Type Safety**: Full type hints throughout. Support for mypy/pyright static analysis.
5. **Jupyter First**: Rich `_repr_html_()` for model objects, inline plotting, progress bars.
6. **Zero Surprises**: Numerical results must match seminr to machine precision. Validated against seminr, SmartPLS, and ADANCO.

### API Design: R → Python Mapping

```python
# R (seminr)                          # Python (seminpy)
# ─────────────────────────           # ─────────────────────────
# library(seminr)                     import seminpy as sp

# Measurement Model
# constructs(                         mm = sp.constructs(
#   reflective("Image",                 sp.reflective("Image",
#     multi_items("IMAG", 1:5)),           sp.multi_items("IMAG", 1, 5)),
#   composite("Value",                  sp.composite("Value",
#     multi_items("PERV", 1:2),             sp.multi_items("PERV", 1, 2),
#     weights = mode_B)                     weights=sp.mode_B),
# )                                   )

# Structural Model
# relationships(                      sm = sp.relationships(
#   paths(from="Image",                 sp.paths(from_="Image",
#     to=c("Value","Satisfaction"))        to=["Value","Satisfaction"]),
# )                                   )

# Estimation
# estimate_pls(data=mobi,             model = sp.estimate_pls(
#   measurement_model=mm,               data=mobi,
#   structural_model=sm)                measurement_model=mm,
#                                       structural_model=sm)

# Bootstrap
# bootstrap_model(model,              boot = sp.bootstrap_model(
#   nboot=1000, cores=4)                model, nboot=1000, cores=4)

# Summary
# summary(boot)                       boot.summary()
#                                     # or: sp.summary(boot)

# Plot
# plot(model)                         model.plot()
#                                     # or: sp.plot(model)
```

### Key API Differences from R

| R (seminr) | Python (seminpy) | Reason |
|------------|-------------------|--------|
| `c("a", "b")` | `["a", "b"]` | Python lists |
| `1:5` | `range(1, 6)` or `(1, 5)` as args to `multi_items` | No R-style range syntax |
| `from = "X"` | `from_="X"` | `from` is a Python keyword |
| `S3 dispatch: summary(model)` | `model.summary()` or `sp.summary(model)` | Both work |
| `plot(model)` | `model.plot()` or `sp.plot(model)` | Both work |
| `model$path_coef` | `model.path_coef` | Attribute access |
| `paths(from=..., to=...)` | `sp.paths(from_=..., to=...)` | Keyword conflict |
| `NA` | `np.nan` / `None` | Python missing values |
| `mode_A` (function) | `sp.mode_A` (enum/sentinel) | No first-class functions as defaults |

---

## 3. Package Architecture

### Directory Structure

```
seminpy/
├── pyproject.toml              # Package metadata, dependencies
├── README.md
├── LICENSE                     # GPL-3 (match seminr)
├── seminpy/
│   ├── __init__.py             # Public API re-exports
│   ├── _types.py               # Type definitions, enums, protocols
│   │
│   ├── # ── Specification DSL ──
│   ├── specify.py              # constructs(), relationships(), paths()
│   ├── constructs.py           # reflective(), composite(), single_item(), multi_items()
│   ├── interactions.py         # interaction_term(), product_indicator, two_stage, orthogonal
│   ├── higher_order.py         # higher_composite(), higher_reflective()
│   ├── model.py                # specify_model(), SpecifiedModel class
│   │
│   ├── # ── Internal Data Structures ──
│   ├── mm_matrix.py            # MeasurementMatrix class (replaces mmMatrix)
│   ├── sm_matrix.py            # StructuralMatrix class (replaces smMatrix)
│   │
│   ├── # ── Estimation Engines ──
│   ├── estimate_pls.py         # estimate_pls(), simplePLS algorithm
│   ├── estimate_cbsem.py       # estimate_cbsem(), estimate_cfa() via semopy
│   ├── estimate_bootstrap.py   # bootstrap_model(), parallel resampling
│   ├── estimate_mga.py         # estimate_pls_mga()
│   ├── consistent.py           # PLSc correction (Dijkstra-Henseler)
│   │
│   ├── # ── Evaluation & Metrics ──
│   ├── evaluate.py             # All assessment metrics
│   ├── reliability.py          # cronbachs_alpha, rhoA, rhoC, AVE
│   ├── validity.py             # HTMT, cross-loadings, fornell_larcker
│   ├── effects.py              # f², total effects, indirect effects, VIF
│   ├── predict.py              # PLSpredict (predict_pls)
│   │
│   ├── # ── Reporting ──
│   ├── summary.py              # Summary classes with __repr__ and _repr_html_
│   ├── report.py               # Formatted output tables
│   │
│   ├── # ── Visualization ──
│   ├── plot.py                 # Plotly-based interactive path diagrams
│   ├── plot_htmt.py            # HTMT heatmap
│   ├── plot_interaction.py     # Slope analysis plots
│   ├── themes.py               # Theme system (port of seminr themes)
│   │
│   ├── # ── Utilities ──
│   ├── utils.py                # Standardization, helper math
│   ├── missing.py              # Missing data handling
│   ├── weighting.py            # path_weighting, path_factorial, mode functions
│   ├── datasets.py             # Built-in datasets (mobi, corp_rep_data)
│   │
│   └── # ── Interop ──
│       └── from_seminr.py      # Import seminr models via rpy2 (optional)
│
├── tests/
│   ├── conftest.py             # Shared fixtures (mobi data, estimated models)
│   ├── test_specify.py
│   ├── test_estimate_pls.py
│   ├── test_estimate_cbsem.py
│   ├── test_bootstrap.py
│   ├── test_consistent.py
│   ├── test_higher_order.py
│   ├── test_interactions.py
│   ├── test_evaluate.py
│   ├── test_predict.py
│   ├── test_mga.py
│   ├── test_plot.py
│   └── test_summary.py
│
├── docs/
│   ├── conf.py                 # Sphinx configuration
│   ├── index.rst
│   ├── quickstart.rst
│   ├── api.rst
│   └── tutorials/
│       ├── pls_ecsi.ipynb      # Port of seminr-pls-ecsi demo
│       ├── cbsem_cfa.ipynb     # Port of seminr-cbsem-cfa-ecsi demo
│       └── advanced.ipynb      # Interactions, HOC, MGA
│
└── benchmarks/
    └── validate_against_seminr.py  # Cross-validation script
```

### Module Count & Estimated Lines

| Module Group | Files | Est. Python Lines | R Source Lines |
|---|---|---|---|
| Specification DSL | 5 | ~600 | ~900 |
| Internal Data Structures | 2 | ~500 | ~600 |
| Estimation Engines | 5 | ~1,200 | ~1,250 |
| Evaluation & Metrics | 5 | ~700 | ~850 |
| Reporting | 2 | ~500 | ~630 |
| Visualization | 4 | ~800 | ~2,100 |
| Utilities | 4 | ~300 | ~500 |
| **Total** | **27** | **~4,600** | **~6,830** |

Python is typically more concise due to: no roxygen blocks, less boilerplate, comprehensions, and the class system handling dispatch that R does explicitly.

---

## 4. Dependencies

### Core (Required)

| Package | Purpose | R Equivalent |
|---------|---------|-------------|
| `numpy` >= 1.24 | Matrix operations, linear algebra | Base R matrices |
| `pandas` >= 2.0 | DataFrames, data loading | Base R data.frames |
| `scipy` >= 1.11 | Statistics (t-dist, correlations, regression) | `stats` package |

### Estimation (Required for respective features)

| Package | Purpose | R Equivalent |
|---------|---------|-------------|
| `semopy` >= 2.2 | CBSEM/CFA estimation engine | `lavaan` |
| `joblib` >= 1.3 | Parallel bootstrap processing | `parallel` package |

### Visualization (Optional, in extras)

| Package | Purpose | R Equivalent |
|---------|---------|-------------|
| `plotly` >= 5.18 | Interactive path diagrams | `DiagrammeR` |
| `matplotlib` >= 3.8 | Static plots, HTMT heatmap | Base R plotting |
| `graphviz` (python-graphviz) | DOT-based static diagrams | `DiagrammeR` (DOT) |

### Development

| Package | Purpose |
|---------|---------|
| `pytest` >= 7.0 | Testing |
| `pytest-cov` | Coverage |
| `rpy2` (optional) | Cross-validation against R seminr |
| `sphinx` | Documentation |
| `nbsphinx` | Jupyter notebook docs |

### pyproject.toml Dependency Groups

```toml
[project]
dependencies = [
    "numpy>=1.24",
    "pandas>=2.0",
    "scipy>=1.11",
    "joblib>=1.3",
]

[project.optional-dependencies]
cbsem = ["semopy>=2.2"]
plot = ["plotly>=5.18", "matplotlib>=3.8"]
full = ["semopy>=2.2", "plotly>=5.18", "matplotlib>=3.8"]
dev = ["pytest>=7.0", "pytest-cov", "ruff", "mypy"]
docs = ["sphinx", "nbsphinx", "sphinx-rtd-theme"]
```

---

## 5. Data Structures: R → Python Mapping

### mmMatrix → MeasurementMatrix

R's `mmMatrix` is a character matrix with columns `["construct", "measurement", "type"]`.

```python
# Python: Thin wrapper around a pandas DataFrame
class MeasurementMatrix:
    """Internal measurement model specification."""

    def __init__(self, data: pd.DataFrame):
        # data has columns: construct, measurement, type
        self._df = data

    # Accessors (port of helpers-mmMatrix.R)
    def all_constructs(self) -> list[str]: ...
    def construct_items(self, construct: str) -> list[str]: ...
    def construct_mode(self, construct: str) -> str: ...
    def is_reflective(self, construct: str) -> bool: ...
    def is_mode_A(self, construct: str) -> bool: ...
    def is_mode_B(self, construct: str) -> bool: ...
    def is_HOC(self, construct: str) -> bool: ...
    def all_items(self) -> list[str]: ...

    # Selectors
    def reflective_constructs(self) -> list[str]: ...
    def composite_constructs(self) -> list[str]: ...
    def HOC_constructs(self) -> list[str]: ...

    # Mutators
    def add_construct(self, construct: str, items: list[str], mode: str): ...
    def remove_construct(self, construct: str): ...
```

### smMatrix → StructuralMatrix

R's `smMatrix` is a character matrix with columns `["source", "target"]`.

```python
class StructuralMatrix:
    """Internal structural model specification."""

    def __init__(self, data: pd.DataFrame):
        # data has columns: source, target
        self._df = data

    # Accessors (port of helpers-smMatrix.R)
    def all_endogenous(self) -> list[str]: ...
    def all_exogenous(self) -> list[str]: ...
    def construct_antecedents(self, construct: str) -> list[str]: ...
    def construct_targets(self, construct: str) -> list[str]: ...
    def is_only_endogenous(self, construct: str) -> bool: ...
    def is_interaction(self, construct: str) -> bool: ...
```

### Model Objects: S3 Classes → Python Classes

```python
# Base class for all estimated models
class SEMinRModel:
    """Base class for all estimated SEMinR models."""
    mean_data: np.ndarray
    sd_data: np.ndarray
    mm_matrix: MeasurementMatrix
    sm_matrix: StructuralMatrix
    constructs: list[str]
    mm_variables: list[str]
    outer_loadings: pd.DataFrame     # items × constructs
    outer_weights: pd.DataFrame      # items × constructs
    path_coef: pd.DataFrame          # constructs × constructs
    construct_scores: pd.DataFrame   # observations × constructs
    r_squared: pd.DataFrame
    data: pd.DataFrame
    raw_data: pd.DataFrame
    measurement_model: MeasurementModel  # original specification
    structural_model: StructuralMatrix

    def summary(self) -> 'PLSSummary': ...
    def plot(self, **kwargs) -> 'Figure': ...
    def _repr_html_(self) -> str: ...

class PLSModel(SEMinRModel):
    """Estimated PLS-SEM model."""
    iterations: int
    weight_diff: float
    inner_weights: str  # "path" or "factorial"
    settings: dict

    def rerun(self, **kwargs) -> 'PLSModel': ...

class BootstrapModel(PLSModel):
    """Bootstrapped PLS-SEM model."""
    nboot: int
    boot_paths: np.ndarray       # nboot × paths
    boot_loadings: np.ndarray    # nboot × loadings
    boot_weights: np.ndarray     # nboot × weights
    boot_HTMT: np.ndarray        # nboot × HTMT pairs

    def summary(self) -> 'BootstrapSummary': ...

class CBSEMModel(SEMinRModel):
    """Estimated CB-SEM model (via semopy)."""
    fit_indices: dict   # CFI, TLI, RMSEA, SRMR, chi2, df, p
    lavaan_model: object  # underlying semopy model

class CFAModel(SEMinRModel):
    """Confirmatory Factor Analysis model (via semopy)."""
    fit_indices: dict

class PredictModel:
    """PLSpredict cross-validation results."""
    test_predictions: pd.DataFrame
    benchmark_predictions: pd.DataFrame  # LM and/or mean
    metrics: pd.DataFrame  # RMSE, MAE, Q²
```

---

## 6. Module-by-Module Porting Strategy

### Phase 1: Core Specification DSL

**R Sources**: `specify_constructs.R` (259), `specify_relationships.R` (82), `specify_associations.R` (38), `specify_models.R` (43), `specify_interactions.R` (389)

**Python Targets**: `specify.py`, `constructs.py`, `interactions.py`, `higher_order.py`, `model.py`

#### Key Functions to Port

```python
# constructs.py
def reflective(construct_name: str, *items: str | list[str]) -> ConstructSpec: ...
def composite(construct_name: str, *items: str | list[str],
              weights: WeightMode = mode_A) -> ConstructSpec: ...
def single_item(item: str) -> list[str]: ...
def multi_items(prefix: str, start: int, end: int) -> list[str]: ...
def higher_composite(construct_name: str, *dimensions: str,
                     weights: WeightMode = mode_A) -> ConstructSpec: ...
def higher_reflective(construct_name: str, *dimensions: str) -> ConstructSpec: ...

# specify.py
def constructs(*specs: ConstructSpec) -> MeasurementModel: ...
def relationships(*path_specs: PathSpec) -> StructuralMatrix: ...
def paths(from_: str, to: str | list[str]) -> PathSpec: ...

# interactions.py
def interaction_term(iv: str, moderator: str,
                     method: InteractionMethod = product_indicator) -> ConstructSpec: ...
```

#### R-to-Python Translation Notes

- R's `constructs()` uses `...` (variadic) and builds a character matrix. Python uses `*specs` and builds a MeasurementMatrix object.
- R's `multi_items("IMAG", 1:5)` generates `c("IMAG1", "IMAG2", ..., "IMAG5")`. Python: `multi_items("IMAG", 1, 5)` → `["IMAG1", "IMAG2", ..., "IMAG5"]`.
- R's measurement modes (`mode_A`, `mode_B`) are functions. Python uses enum sentinels.
- Interaction terms encode in R as type codes ("HOCA", "HOCB", etc.) in mmMatrix. Python uses enum fields on ConstructSpec.

---

### Phase 2: PLS Estimation Engine

**R Sources**: `estimate_pls.R` (274), `estimate_simplePLS.R` (199), `library.R` (318)

**Python Target**: `estimate_pls.py`, `weighting.py`, `utils.py`

#### The simplePLS Algorithm (Critical Path)

This is the heart of PLS-SEM. The algorithm must be ported with exact numerical fidelity.

```
Input: X (standardized data), W₀ (initial weights), D (adjacency matrix)
Repeat until convergence:
  1. Y = X · W          (outer estimation: construct scores)
  2. Y = standardize(Y)
  3. E = inner_weights(Y, D)  (inner estimation)
  4. Y = Y · E          (inner-weighted construct scores)
  5. Y = standardize(Y)
  6. W_new = update_outer_weights(X, Y, mode_per_construct)
  7. W_new = standardize_weights(X, W_new)
  8. If |W_new - W| < 10^(-stop_criterion): break
  9. W = W_new
Output: Y (final scores), W (final weights)
```

#### Outer Weight Update Modes

```python
# weighting.py

def mode_A_weights(data: np.ndarray, scores: np.ndarray,
                   item_indices: np.ndarray) -> np.ndarray:
    """Correlation weights (reflective/Mode A)."""
    # w_j = cor(x_j, Y_construct) for each indicator j
    return data[:, item_indices].T @ scores / (len(scores) - 1)

def mode_B_weights(data: np.ndarray, scores: np.ndarray,
                   item_indices: np.ndarray) -> np.ndarray:
    """Regression weights (formative/Mode B)."""
    # w = (X'X)^{-1} X'Y  (OLS regression)
    X = data[:, item_indices]
    return np.linalg.lstsq(X, scores, rcond=None)[0]

def unit_weights(data: np.ndarray, scores: np.ndarray,
                 item_indices: np.ndarray) -> np.ndarray:
    """Equal weights (single items, interactions)."""
    return np.ones(len(item_indices))
```

#### Inner Weighting Schemes

```python
def path_weighting(sm_matrix: StructuralMatrix, scores: np.ndarray,
                   endogenous: list[str]) -> np.ndarray:
    """Path weighting scheme.
    - Predecessors: regression coefficients
    - Successors: correlations
    """
    ...

def path_factorial(sm_matrix: StructuralMatrix, scores: np.ndarray,
                   endogenous: list[str]) -> np.ndarray:
    """Factorial weighting scheme.
    - All adjacent constructs: correlations
    """
    ...
```

#### Post-Estimation

After convergence:
1. **Outer loadings** = cor(X, Y) for each indicator-construct pair
2. **Path coefficients** = OLS regression of endogenous on antecedents
3. **R²** = variance explained in each endogenous construct
4. **Interaction adjustment** for product indicator terms

#### Critical Implementation Detail

R uses `scale()` which standardizes to sample SD (n-1 denominator). NumPy's default `std()` uses population SD (n denominator). **Must use `ddof=1` everywhere** or write a custom `standardize()` that matches R's `scale()`.

```python
def standardize(X: np.ndarray) -> np.ndarray:
    """Match R's scale() function exactly."""
    mean = X.mean(axis=0)
    std = X.std(axis=0, ddof=1)  # Sample SD, not population SD
    std[std == 0] = 1.0  # Avoid division by zero
    return (X - mean) / std
```

---

### Phase 3: PLSc (Consistent PLS)

**R Source**: `feature_consistent.R` (121 lines)

**Python Target**: `consistent.py`

PLSc corrects composite-based PLS estimates to approximate common factor results for reflective constructs. The Dijkstra-Henseler correction:

```
For each reflective construct:
  1. Compute rhoA = reliability coefficient
  2. Disattenuate loadings: λ_c = λ_pls / sqrt(rhoA)
  3. Disattenuate path coefficients between reflective constructs:
     β_c = β_pls * sqrt(rhoA_source) / sqrt(rhoA_target)
```

Key: Only applied to constructs with mode "C" (reflective). Composite constructs (mode "A", "B") are left unchanged. This is a post-estimation correction applied in `estimate_pls()` after `simplePLS()` converges.

---

### Phase 4: Bootstrap Inference

**R Source**: `estimate_bootstrap.R` (346 lines), `boot_utils.R` (30 lines)

**Python Target**: `estimate_bootstrap.py`

#### Algorithm

```
Input: estimated model, nboot, seed, cores
For b in 1..nboot:
  1. Resample data with replacement (same N)
  2. Re-estimate PLS model on resampled data
  3. Store: path coefficients, loadings, weights, HTMT
Output: Bootstrap distributions, means, SDs, confidence intervals
```

#### Parallel Strategy

R uses `parallel::parLapply()` with forked/PSOCK clusters. Python equivalent:

```python
from joblib import Parallel, delayed

def bootstrap_model(model: PLSModel, nboot: int = 1000,
                    cores: int = 1, seed: int | None = None,
                    ci_type: str = "perc") -> BootstrapModel:
    """Bootstrap a PLS model for inference."""

    rng = np.random.default_rng(seed)
    seeds = rng.integers(0, 2**31, size=nboot)

    def _single_boot(boot_seed):
        boot_rng = np.random.default_rng(boot_seed)
        idx = boot_rng.choice(n, size=n, replace=True)
        boot_data = model.data.iloc[idx]
        try:
            boot_model = estimate_pls(
                data=boot_data,
                measurement_model=model.measurement_model,
                structural_model=model.structural_model,
                inner_weights=model.settings["inner_weights"],
            )
            return _extract_boot_params(boot_model)
        except Exception:
            return None  # Failed subsample

    results = Parallel(n_jobs=cores)(
        delayed(_single_boot)(s) for s in seeds
    )
    ...
```

#### Confidence Interval Types

- **Percentile** (`perc`): Direct quantiles of bootstrap distribution
- **BCa** (bias-corrected and accelerated): Adjusts for bias and skewness
- **Standard** (`std`): Mean ± z * SE

---

### Phase 5: Evaluation Metrics

**R Sources**: `evaluate_measurement_model.R` (271), `evaluate_reliability.R` (213), `evaluate_model.R` (97), `evaluate_effects.R` (43), `compute_metrics.R` (152)

**Python Targets**: `evaluate.py`, `reliability.py`, `validity.py`, `effects.py`

#### Reliability Metrics

```python
def cronbachs_alpha(data: pd.DataFrame, items: list[str]) -> float: ...
def rho_A(model: PLSModel, construct: str) -> float: ...
def rho_C(model: PLSModel, construct: str) -> float: ...  # composite reliability
def AVE(model: PLSModel, construct: str) -> float: ...     # average variance extracted
```

#### Validity Metrics

```python
def HTMT(model: PLSModel) -> pd.DataFrame: ...        # heterotrait-monotrait ratio
def cross_loadings(model: PLSModel) -> pd.DataFrame: ...
def fornell_larcker(model: PLSModel) -> pd.DataFrame: ...
def VIF(model: PLSModel) -> pd.DataFrame: ...          # variance inflation factors
```

#### Effect Sizes & Model Fit

```python
def f_squared(model: PLSModel) -> pd.DataFrame: ...    # Cohen's f²
def total_effects(model: PLSModel) -> pd.DataFrame: ...
def indirect_effects(model: PLSModel, from_: str, to: str,
                     through: list[str]) -> float: ...
def r_squared(model: PLSModel) -> pd.DataFrame: ...
def adjusted_r_squared(model: PLSModel) -> pd.DataFrame: ...
```

---

### Phase 6: Higher-Order Constructs

**R Source**: `feature_higher_order.R` (179 lines)

**Python Target**: `higher_order.py`

Two-stage approach:
1. **Stage 1**: Estimate model with LOCs (lower-order constructs) only
2. **Stage 2**: Use LOC scores as indicators for HOC (higher-order construct), re-estimate

The `estimate_pls()` function detects HOCs via `all_HOCs()` and automatically applies two-stage estimation. Same pattern in Python.

---

### Phase 7: Interaction Terms

**R Source**: `specify_interactions.R` (389 lines — largest specification file)

**Python Target**: `interactions.py`

Three methods:
1. **Product Indicator** (default): Creates cross-products of standardized indicators
2. **Two-Stage**: Uses construct scores from Stage 1 as single indicator
3. **Orthogonal**: Residualizes product indicators against main effects

```python
class InteractionMethod(Enum):
    PRODUCT_INDICATOR = "product_indicator"
    TWO_STAGE = "two_stage"
    ORTHOGONAL = "orthogonal"

def interaction_term(iv: str, moderator: str,
                     method: InteractionMethod = InteractionMethod.PRODUCT_INDICATOR
                     ) -> ConstructSpec:
    """Create an interaction term between two constructs."""
    ...
```

The product indicator method is the most complex: it generates `n_iv × n_mod` new columns in the data matrix, each being the product of a pair of standardized indicators.

---

### Phase 8: CBSEM & CFA

**R Source**: `estimate_cbsem.R` (306 lines)

**Python Target**: `estimate_cbsem.py`

R uses `lavaan`. Python will use `semopy` (the best-maintained Python CB-SEM package).

#### Translation Strategy

seminr converts its DSL into lavaan syntax (`generate_lavaan_syntax()`). We'll convert to semopy syntax instead:

```python
def _to_semopy_syntax(mm: MeasurementModel, sm: StructuralMatrix) -> str:
    """Convert seminpy model spec to semopy model string."""
    lines = []

    # Measurement model: latent =~ indicators
    for construct in mm.all_constructs():
        items = mm.construct_items(construct)
        lines.append(f"{construct} =~ {' + '.join(items)}")

    # Structural model: DV ~ IVs
    for target in sm.all_endogenous():
        antecedents = sm.construct_antecedents(target)
        lines.append(f"{target} ~ {' + '.join(antecedents)}")

    return "\n".join(lines)
```

semopy uses the same `=~` and `~` operators as lavaan, so the translation is straightforward.

#### Fit Indices

semopy provides: chi², df, p-value, CFI, TLI, RMSEA, SRMR, AIC, BIC. These map directly to what seminr extracts from lavaan.

---

### Phase 9: PLSpredict

**R Source**: `feature_plspredict.R` (718 lines — second largest file)

**Python Target**: `predict.py`

#### Algorithm

```
For each endogenous construct's indicators:
  1. k-fold or LOOCV cross-validation
  2. In each fold:
     a. Estimate PLS on training set
     b. Predict indicator values for test set
     c. Also compute LM (linear model) benchmark predictions
  3. Compute RMSE, MAE for both PLS and LM
  4. Compute Q² = 1 - (SS_residual / SS_total)
  5. Compare PLS vs LM: if PLS RMSE < LM RMSE for all indicators → predictive
```

#### Key Challenge

R's implementation uses `parallel::parSapply` for LOOCV. In Python, use `joblib.Parallel`. The prediction function reconstructs scores from raw data using the estimated weights, then applies path coefficients to predict endogenous scores, then converts back to indicator space via loadings.

---

### Phase 10: Multi-Group Analysis (MGA)

**R Source**: `estimate_pls_mga.R` (120 lines)

**Python Target**: `estimate_mga.py`

```python
def estimate_pls_mga(model: PLSModel, condition: pd.Series,
                     nboot: int = 1000, cores: int = 1
                     ) -> MGAResult:
    """Parametric multi-group analysis.

    Splits data by condition (binary grouping variable),
    bootstraps each group, tests for significant differences
    in path coefficients between groups.
    """
    ...
```

---

### Phase 11: Summary & Reporting

**R Sources**: `report_summary.R` (232), `report_paths_and_intervals.R` (392), `report_cbsem.R` (67), `report_cfa.R` (65), `report_lavaan.R` (137)

**Python Target**: `summary.py`, `report.py`

```python
class PLSSummary:
    """Rich summary of a PLS model."""
    paths: pd.DataFrame
    loadings: pd.DataFrame
    weights: pd.DataFrame
    reliability: pd.DataFrame     # alpha, rhoA, rhoC, AVE
    htmt: pd.DataFrame
    r_squared: pd.DataFrame
    f_squared: pd.DataFrame
    vif: pd.DataFrame

    def __repr__(self) -> str: ...      # Plain text table
    def _repr_html_(self) -> str: ...   # Jupyter HTML rendering

class BootstrapSummary(PLSSummary):
    """Summary with bootstrap confidence intervals."""
    boot_paths: pd.DataFrame      # coef, mean, SD, t, p, CI_lo, CI_hi, sig
    boot_loadings: pd.DataFrame
    boot_weights: pd.DataFrame
    boot_htmt: pd.DataFrame
    total_effects: pd.DataFrame
    specific_indirect: pd.DataFrame
```

#### Jupyter Integration

Every model and summary object implements `_repr_html_()` for rich display in Jupyter notebooks — formatted HTML tables with significance stars, color-coded cells, etc.

---

### Phase 12: Visualization

**R Sources**: `plot_dot.R` (1,636), `plot_htmt.R` (255), `plot_results.R` (209), `plot_utils.R` (106), `theme.R` (312), `theme_defaults.R` (160), `theme_current.R` (48)

**Python Targets**: `plot.py`, `plot_htmt.py`, `plot_interaction.py`, `themes.py`

#### Strategy: Plotly for Interactive, Matplotlib for Static

```python
def plot_model(model: SEMinRModel, theme: Theme | None = None,
               backend: str = "plotly", **kwargs) -> Figure:
    """Plot a path diagram of the estimated model.

    Args:
        backend: "plotly" (interactive, default) or "matplotlib" (static)
    """
    ...
```

**Plotly** provides:
- Interactive hover tooltips (loadings, path coefficients, p-values)
- Click-to-zoom on constructs
- Export to HTML, PNG, SVG, PDF
- Native Jupyter notebook rendering

**Layout**: Use `networkx` for graph layout computation (hierarchical left-to-right), render with Plotly scatter/annotations.

#### Theme System

Port the hierarchical theme system directly:

```python
@dataclass
class Theme:
    plot_rounding: int = 2
    sm_node_fill: str = "cadetblue1"
    sm_node_color: str = "black"
    sm_node_label_fontsize: int = 12
    mm_node_fill: str = "white"
    mm_edge_color: str = "grey"
    # ... all theme parameters

# Built-in themes
def theme_default() -> Theme: ...
def theme_academic() -> Theme: ...
def theme_dark() -> Theme: ...
def theme_smart() -> Theme: ...
```

---

## 7. Datasets

Bundle the same datasets as seminr:

```python
# datasets.py
import importlib.resources

def load_mobi() -> pd.DataFrame:
    """Mobile phone customer satisfaction data (ECSI model).
    250 observations, 33 indicators, 7 constructs.
    """
    ...

def load_corp_rep() -> pd.DataFrame:
    """Corporate reputation data.
    344 observations, 34 indicators.
    """
    ...

def load_influencer() -> pd.DataFrame:
    """Social media influencer data."""
    ...
```

Store as CSV files in `seminpy/data/` and load via `importlib.resources`.

---

## 8. Testing Strategy

### Numerical Validation (Critical)

Every estimation result must match seminr to at least 6 decimal places. Strategy:

1. **Generate reference values from R**: Run seminr on each test case, export all matrices to JSON/CSV
2. **Compare in Python**: Load reference values, run seminpy, assert `np.allclose(result, reference, atol=1e-6)`
3. **Test cases from seminr's own test suite** (4,204 lines of R tests)

```python
# tests/conftest.py
import json
import pytest

@pytest.fixture
def mobi_reference():
    """Load reference results from seminr for the MOBI model."""
    with open("tests/fixtures/mobi_pls_reference.json") as f:
        return json.load(f)

# tests/test_estimate_pls.py
def test_path_coefficients_match_seminr(mobi_reference):
    model = sp.estimate_pls(data=mobi, measurement_model=mm, structural_model=sm)
    expected = np.array(mobi_reference["path_coef"])
    np.testing.assert_allclose(model.path_coef.values, expected, atol=1e-6)
```

### Test Categories

| Category | Tests | Priority |
|---|---|---|
| Specification DSL | ~30 | Phase 1 |
| PLS estimation (basic) | ~40 | Phase 2 |
| PLSc correction | ~15 | Phase 3 |
| Bootstrap inference | ~25 | Phase 4 |
| Evaluation metrics | ~30 | Phase 5 |
| Higher-order constructs | ~15 | Phase 6 |
| Interaction terms | ~20 | Phase 7 |
| CBSEM/CFA | ~20 | Phase 8 |
| PLSpredict | ~15 | Phase 9 |
| MGA | ~10 | Phase 10 |
| Summary/reporting | ~15 | Phase 11 |
| Plotting | ~15 | Phase 12 |
| **Total** | **~250** | |

### Cross-Validation Script

```python
# benchmarks/validate_against_seminr.py
"""
Runs identical models in both seminr (via rpy2) and seminpy,
compares all output matrices. Requires R + seminr installed.
"""
```

---

## 9. Implementation Phases & Timeline

### Phase 1: Foundation (Weeks 1-3)
- [ ] Project scaffolding (pyproject.toml, CI, linting)
- [ ] `_types.py` — enums, protocols, type aliases
- [ ] `mm_matrix.py` — MeasurementMatrix class with all accessors
- [ ] `sm_matrix.py` — StructuralMatrix class with all accessors
- [ ] `constructs.py` — reflective(), composite(), single_item(), multi_items()
- [ ] `specify.py` — constructs(), relationships(), paths()
- [ ] `model.py` — specify_model(), SpecifiedModel
- [ ] `datasets.py` — mobi, corp_rep_data
- [ ] Tests: specification DSL, data structures

### Phase 2: Core PLS Engine (Weeks 4-6)
- [ ] `utils.py` — standardize(), standardize_weights()
- [ ] `weighting.py` — mode_A, mode_B, unit, path_weighting, path_factorial
- [ ] `estimate_pls.py` — simplePLS algorithm, estimate_pls()
- [ ] `missing.py` — mean_replacement, na_omit
- [ ] Tests: numerical validation against seminr reference values

### Phase 3: PLSc + Evaluation (Weeks 7-8)
- [ ] `consistent.py` — PLSc correction
- [ ] `reliability.py` — alpha, rhoA, rhoC, AVE
- [ ] `validity.py` — HTMT, cross-loadings, Fornell-Larcker
- [ ] `effects.py` — f², total effects, VIF
- [ ] `evaluate.py` — top-level evaluation orchestrator
- [ ] Tests: all metrics match seminr

### Phase 4: Bootstrap + Summary (Weeks 9-11)
- [ ] `estimate_bootstrap.py` — parallel bootstrap with joblib
- [ ] `summary.py` — PLSSummary, BootstrapSummary with rich display
- [ ] `report.py` — formatted output tables
- [ ] Tests: bootstrap distributions, CI computation

### Phase 5: Advanced Features (Weeks 12-15)
- [ ] `interactions.py` — product_indicator, two_stage, orthogonal
- [ ] `higher_order.py` — two-stage HOC estimation
- [ ] `estimate_mga.py` — multi-group analysis
- [ ] `predict.py` — PLSpredict with k-fold/LOOCV
- [ ] Tests: all advanced features match seminr

### Phase 6: CBSEM Integration (Weeks 16-17)
- [ ] `estimate_cbsem.py` — semopy integration, estimate_cbsem(), estimate_cfa()
- [ ] Tests: fit indices, factor loadings match seminr/lavaan

### Phase 7: Visualization + Polish (Weeks 18-20)
- [ ] `themes.py` — theme system
- [ ] `plot.py` — Plotly interactive path diagrams
- [ ] `plot_htmt.py` — HTMT heatmap
- [ ] `plot_interaction.py` — slope analysis
- [ ] Documentation (Sphinx + Jupyter tutorials)
- [ ] PyPI release preparation

---

## 10. R-Specific Patterns Requiring Python Adaptation

### S3 Method Dispatch → Python Methods

R uses `UseMethod()` for S3 dispatch on the first argument's class:

```r
# R
summary.pls_model <- function(object, ...) { ... }
summary.boot_seminr_model <- function(object, ...) { ... }
```

Python uses class methods:

```python
# Python
class PLSModel:
    def summary(self) -> PLSSummary: ...

class BootstrapModel(PLSModel):
    def summary(self) -> BootstrapSummary: ...
```

For users who prefer functional style, also provide top-level functions:

```python
def summary(model: SEMinRModel) -> Summary:
    """Dispatch to the appropriate summary method."""
    return model.summary()
```

### Named Matrices → Labeled DataFrames

R matrices have `rownames()` and `colnames()`. Python equivalent is `pd.DataFrame` with named index/columns:

```python
# R: path_coef is a matrix with rownames = colnames = construct names
# Python: path_coef is a DataFrame with index = columns = construct names
model.path_coef  # pd.DataFrame, index=constructs, columns=constructs
```

### Variadic Functions → *args

R's `...` (dots) maps to Python's `*args`:

```r
# R
constructs <- function(...) { ... }
```

```python
# Python
def constructs(*specs: ConstructSpec) -> MeasurementModel: ...
```

### Function-as-Parameter → Enum/Callable

R passes functions directly as parameters (e.g., `inner_weights = path_weighting`). Python can do the same, but enums + dispatch are more discoverable:

```python
# Option A: Match R (function references)
model = estimate_pls(data, mm, sm, inner_weights=path_weighting)

# Option B: String/enum dispatch (more Pythonic)
model = estimate_pls(data, mm, sm, inner_weights="path")

# We'll support BOTH via Union type
inner_weights: Callable | str = "path"
```

### R's `scale()` vs NumPy

R's `scale(x)` centers and scales to sample SD (ddof=1). **Must replicate exactly**:

```python
def standardize(x: np.ndarray) -> np.ndarray:
    """R's scale() equivalent: center and divide by sample SD."""
    mu = np.mean(x, axis=0)
    sd = np.std(x, axis=0, ddof=1)
    sd[sd == 0] = 1.0
    return (x - mu) / sd
```

### R's Matrix Operations

| R | NumPy |
|---|-------|
| `A %*% B` | `A @ B` |
| `t(A)` | `A.T` |
| `solve(A)` | `np.linalg.solve(A, ...)` or `np.linalg.inv(A)` |
| `cor(X)` | `np.corrcoef(X, rowvar=False)` |
| `crossprod(A)` | `A.T @ A` |
| `diag(A)` | `np.diag(A)` |
| `colMeans(X)` | `X.mean(axis=0)` |
| `apply(X, 2, fn)` | `np.apply_along_axis(fn, 0, X)` |

---

## 11. Competitive Advantages Over Existing Python Packages

| Feature | seminpy | plspm (Google) | semopy | pylspm |
|---------|---------|----------------|--------|--------|
| PLS-SEM | Yes | Yes | No | Yes |
| CB-SEM / CFA | Yes | No | Yes | No |
| PLSc | Yes | Planned | N/A | No |
| Bootstrap | Yes (parallel) | Yes | N/A | No |
| Interaction terms | Yes (3 methods) | No | No | No |
| Higher-order constructs | Yes | Yes | No | No |
| MGA | Yes | No | No | No |
| PLSpredict | Yes | No | No | No |
| HTMT | Yes | Planned | No | No |
| f², VIF, Q² | Yes | Planned | Partial | No |
| Natural DSL | Yes | Yes | Lavaan-style | No |
| Interactive plots | Yes (Plotly) | No | No | No |
| Jupyter integration | Yes (_repr_html_) | No | Partial | No |
| Type hints | Full | Partial | No | No |
| Validated against SmartPLS | Yes | Yes | N/A | No |

---

## 12. Risk Assessment & Mitigations

### High Risk

| Risk | Impact | Mitigation |
|------|--------|------------|
| Numerical precision mismatch | Results differ from seminr/SmartPLS | Exhaustive cross-validation suite; use identical algorithms, same ddof, same convergence criteria |
| semopy API changes | CBSEM integration breaks | Pin semopy version; wrap in adapter layer; have fallback to statsmodels for CFA |
| Bootstrap performance | Slow for large models | joblib parallelization; optional numba JIT for inner loop |

### Medium Risk

| Risk | Impact | Mitigation |
|------|--------|------------|
| PLSc edge cases (negative rhoA) | Crash or wrong results | Port seminr's exact handling: skip correction when rhoA < 0 or > 1 |
| Interaction term complexity | Hardest module to port (389 lines) | Port incrementally: product_indicator first, then two_stage, then orthogonal |
| User adoption | Low uptake | Strong documentation, Jupyter tutorials, seminr-familiar API |

### Low Risk

| Risk | Impact | Mitigation |
|------|--------|------------|
| Python version compatibility | Breaks on older Python | Target 3.10+ (type unions, match statements) |
| Dependency conflicts | Version clashes | Minimal core deps (numpy, pandas, scipy only) |

---

## 13. CI/CD & Release Strategy

### GitHub Actions

```yaml
# .github/workflows/test.yml
- Python 3.10, 3.11, 3.12, 3.13 on Ubuntu, macOS, Windows
- Run pytest with coverage
- Type checking with mypy
- Linting with ruff

# .github/workflows/validate.yml (weekly)
- Cross-validate against R/seminr via rpy2
- Ensures numerical results remain in sync
```

### Release Process

1. **0.1.0** — Core PLS + specification DSL + basic evaluation
2. **0.2.0** — PLSc + bootstrap + full evaluation suite
3. **0.3.0** — Interactions + HOC + MGA + PLSpredict
4. **0.4.0** — CBSEM/CFA integration
5. **0.5.0** — Visualization + themes
6. **1.0.0** — Full feature parity with seminr, stable API

### PyPI Publication

```bash
pip install seminpy              # Core PLS-SEM
pip install seminpy[cbsem]       # + CBSEM via semopy
pip install seminpy[plot]        # + Plotly visualization
pip install seminpy[full]        # Everything
```

---

## 14. Documentation Plan

### API Reference (auto-generated via Sphinx + autodoc)
- Every public function/class fully documented with type hints
- Examples in docstrings that render as runnable code

### Tutorials (Jupyter Notebooks)
1. **Quickstart**: ECSI model with mobi data (mirrors seminr-pls-ecsi demo)
2. **Consistent PLS**: Reflective constructs with PLSc (mirrors seminr-plsc-ecsi)
3. **CBSEM & CFA**: Covariance-based estimation (mirrors seminr-cbsem-cfa-ecsi)
4. **Interactions**: Moderation analysis (mirrors seminr-pls-interaction)
5. **Higher-Order Constructs**: Two-stage HOC (mirrors seminr-pls-higher_order)
6. **Multi-Group Analysis**: Group comparisons (mirrors seminr-pls-mga)
7. **Prediction**: PLSpredict for predictive validation
8. **Migration Guide**: "Coming from seminr? Here's what's different"

### Hosted on Read the Docs

---

## 15. Summary: File-by-File Porting Map

| R Source File | Lines | Python Target | Priority |
|---|---|---|---|
| `specify_constructs.R` | 259 | `constructs.py` | P1 |
| `specify_relationships.R` | 82 | `specify.py` | P1 |
| `specify_associations.R` | 38 | `specify.py` | P1 |
| `specify_models.R` | 43 | `model.py` | P1 |
| `specify_interactions.R` | 389 | `interactions.py` | P5 |
| `helpers-mmMatrix.R` | 349 | `mm_matrix.py` | P1 |
| `helpers-smMatrix.R` | 257 | `sm_matrix.py` | P1 |
| `helpers-model.R` | 110 | Model classes | P1 |
| `estimate_simplePLS.R` | 199 | `estimate_pls.py` | P2 |
| `estimate_pls.R` | 274 | `estimate_pls.py` | P2 |
| `library.R` | 318 | `weighting.py`, `utils.py` | P2 |
| `feature_consistent.R` | 121 | `consistent.py` | P3 |
| `estimate_bootstrap.R` | 346 | `estimate_bootstrap.py` | P4 |
| `boot_utils.R` | 30 | `estimate_bootstrap.py` | P4 |
| `evaluate_measurement_model.R` | 271 | `evaluate.py` | P3 |
| `evaluate_reliability.R` | 213 | `reliability.py` | P3 |
| `evaluate_model.R` | 97 | `evaluate.py` | P3 |
| `evaluate_effects.R` | 43 | `effects.py` | P5 |
| `compute_metrics.R` | 152 | `evaluate.py` | P3 |
| `feature_higher_order.R` | 179 | `higher_order.py` | P5 |
| `feature_plspredict.R` | 718 | `predict.py` | P5 |
| `estimate_pls_mga.R` | 120 | `estimate_mga.py` | P5 |
| `estimate_cbsem.R` | 306 | `estimate_cbsem.py` | P6 |
| `report_summary.R` | 232 | `summary.py` | P4 |
| `report_paths_and_intervals.R` | 392 | `report.py` | P4 |
| `report_cbsem.R` | 67 | `summary.py` | P6 |
| `report_cfa.R` | 65 | `summary.py` | P6 |
| `report_lavaan.R` | 137 | `estimate_cbsem.py` | P6 |
| `plot_dot.R` | 1,636 | `plot.py` | P7 |
| `plot_htmt.R` | 255 | `plot_htmt.py` | P7 |
| `plot_results.R` | 209 | `plot.py` | P7 |
| `plot_utils.R` | 106 | `plot.py` | P7 |
| `theme.R` | 312 | `themes.py` | P7 |
| `theme_defaults.R` | 160 | `themes.py` | P7 |
| `theme_current.R` | 48 | `themes.py` | P7 |
| `data.R` | 192 | `datasets.py` | P1 |
| `clean_data.R` | 30 | `missing.py` | P2 |
| `import_lavaan_syntax.R` | 92 | `estimate_cbsem.py` | P6 |
| `estimate_factor_scores.R` | 56 | `estimate_cbsem.py` | P6 |
| `documentation_utils.R` | 36 | N/A (R-specific) | — |
| `zzz.R` | 8 | N/A (R-specific) | — |

---

## 16. Quick-Start Example (What the Final Product Looks Like)

```python
import seminpy as sp

# Load built-in dataset
mobi = sp.datasets.load_mobi()

# Define measurement model
mm = sp.constructs(
    sp.reflective("Image",        sp.multi_items("IMAG", 1, 5)),
    sp.reflective("Expectation",  sp.multi_items("CUEX", 1, 3)),
    sp.reflective("Quality",      sp.multi_items("PERQ", 1, 7)),
    sp.reflective("Value",        sp.multi_items("PERV", 1, 2)),
    sp.reflective("Satisfaction", sp.multi_items("CUSA", 1, 3)),
    sp.reflective("Complaints",   sp.single_item("CUSCO")),
    sp.reflective("Loyalty",      sp.multi_items("CUSL", 1, 3)),
)

# Define structural model
sm = sp.relationships(
    sp.paths(from_="Image",        to=["Expectation", "Satisfaction", "Loyalty"]),
    sp.paths(from_="Expectation",  to=["Quality", "Value", "Satisfaction"]),
    sp.paths(from_="Quality",      to=["Value", "Satisfaction"]),
    sp.paths(from_="Value",        to=["Satisfaction"]),
    sp.paths(from_="Satisfaction", to=["Complaints", "Loyalty"]),
    sp.paths(from_="Complaints",   to="Loyalty"),
)

# Estimate PLS model
model = sp.estimate_pls(data=mobi, measurement_model=mm, structural_model=sm)

# Bootstrap for inference
boot = sp.bootstrap_model(model, nboot=1000, cores=4, seed=42)

# Rich summary (with significance stars in Jupyter)
boot.summary()

# Interactive path diagram
model.plot()

# Specific results
print(boot.summary().paths)          # Path coefficients with CIs
print(boot.summary().reliability)    # Alpha, rhoA, rhoC, AVE
print(boot.summary().htmt)           # Discriminant validity

# PLSpredict
pred = sp.predict_pls(model, technique="LOOCV")
pred.summary()

# Export
sp.save_plot(model, "model.png")
sp.save_plot(model, "model.html")    # Interactive HTML
```

---

## 17. Verification Criteria

The port is considered complete when:

1. **Numerical equivalence**: All estimation outputs match seminr to 6+ decimal places on mobi, corp_rep_data, and influencer_data models
2. **Feature parity**: Every exported function in seminr has a Python equivalent
3. **Test coverage**: ≥ 90% line coverage, ≥ 250 tests passing
4. **Documentation**: All public API documented, 7+ tutorial notebooks, hosted on ReadTheDocs
5. **Cross-validation**: `validate_against_seminr.py` passes on CI (weekly)
6. **Performance**: Bootstrap 1000 reps within 2x of R's time on same hardware
7. **PyPI**: Successfully published and installable via `pip install seminpy`
8. **Compatibility**: Works on Python 3.10-3.13, Linux/macOS/Windows
