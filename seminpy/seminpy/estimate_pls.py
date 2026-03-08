"""PLS-SEM estimation engine.

Ports R's estimate_pls.R and estimate_simplePLS.R.
Core iterative algorithm for Partial Least Squares Path Modeling.
"""
from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any

import numpy as np
import pandas as pd

from .mm_matrix import MeasurementMatrix
from .sm_matrix import StructuralMatrix
from .utils import standardize, standardize_weights, cov_with
from .weighting import (
    outer_mode_A,
    outer_mode_B,
    outer_unit,
    path_factorial,
    path_weighting,
)


@dataclass
class PLSModel:
    """Estimated PLS-SEM model.

    Attributes match R's seminr pls_model list for API fidelity.
    All matrices are stored as pandas DataFrames with labeled axes.
    """

    # Estimation results
    path_coef: pd.DataFrame
    outer_loadings: pd.DataFrame
    outer_weights: pd.DataFrame
    construct_scores: pd.DataFrame
    r_squared: pd.DataFrame

    # Data
    data: pd.DataFrame
    raw_data: pd.DataFrame
    mean_data: pd.Series
    sd_data: pd.Series

    # Model specification (for rerun / bootstrap)
    mm_matrix: MeasurementMatrix
    sm_matrix: StructuralMatrix
    measurement_model: MeasurementMatrix
    construct_names_list: list[str]
    mm_variables: list[str]

    # Convergence info
    iterations: int
    weight_diff: float

    # Settings for rerun
    settings: dict[str, Any] = field(default_factory=dict)

    def summary(self) -> dict[str, Any]:
        """Basic summary of the estimated model."""
        return {
            "path_coef": self.path_coef,
            "outer_loadings": self.outer_loadings,
            "outer_weights": self.outer_weights,
            "r_squared": self.r_squared,
            "iterations": self.iterations,
            "weight_diff": self.weight_diff,
        }

    def __repr__(self) -> str:
        n_obs = len(self.data)
        n_constructs = len(self.construct_names_list)
        n_items = len(self.mm_variables)
        return (
            f"PLSModel(observations={n_obs}, constructs={n_constructs}, "
            f"indicators={n_items}, iterations={self.iterations})"
        )


def estimate_pls(
    data: pd.DataFrame,
    measurement_model: MeasurementMatrix,
    structural_model: StructuralMatrix,
    inner_weights: str = "path",
    missing: str = "mean",
    missing_value: float | None = None,
    max_it: int = 300,
    stop_criterion: int = 7,
) -> PLSModel:
    """Estimate a PLS-SEM model.

    Args:
        data: DataFrame with indicator columns.
        measurement_model: MeasurementMatrix from constructs() or builder.
        structural_model: StructuralMatrix from relationships() or builder.
        inner_weights: "path" (default) or "factorial".
        missing: "mean" (mean replacement) or "drop" (listwise deletion).
        missing_value: Value to treat as missing (default: NaN).
        max_it: Maximum iterations (default: 300).
        stop_criterion: Convergence exponent (default: 7, i.e., 10^-7).

    Returns:
        Estimated PLSModel.
    """
    raw_data = data.copy()

    # Handle missing values
    if missing_value is not None:
        data = data.replace(missing_value, np.nan)

    # Select only columns used in the measurement model
    mm_items = measurement_model.all_items()
    data = data[mm_items].copy()

    if missing == "mean":
        data = data.fillna(data.mean())
    elif missing == "drop":
        data = data.dropna()

    # Run core PLS algorithm
    model = _simple_pls(
        obs_data=data,
        mm=measurement_model,
        sm=structural_model,
        inner_weights_scheme=inner_weights,
        max_it=max_it,
        stop_criterion=stop_criterion,
    )

    # Store settings for rerun
    model.raw_data = raw_data
    model.measurement_model = measurement_model
    model.settings = {
        "inner_weights": inner_weights,
        "missing": missing,
        "missing_value": missing_value,
        "max_it": max_it,
        "stop_criterion": stop_criterion,
    }

    return model


def _simple_pls(
    obs_data: pd.DataFrame,
    mm: MeasurementMatrix,
    sm: StructuralMatrix,
    inner_weights_scheme: str,
    max_it: int,
    stop_criterion: int,
) -> PLSModel:
    """Core PLS algorithm. Ports R's simplePLS().

    This implements the iterative PLS-PM algorithm:
    1. Initialize outer weights
    2. Repeat until convergence:
       a. Estimate construct scores from outer weights
       b. Estimate inner weights
       c. Update construct scores from inner weights
       d. Update outer weights based on measurement mode
       e. Standardize outer weights
    3. Compute final loadings, path coefficients, R²
    """
    # Get ordered construct names from structural model
    sm_construct_names = sm.construct_names()
    mm_ordered_constructs = [
        c for c in mm.all_constructs() if c in sm_construct_names
    ]
    # Build ordered item list
    mm_variables: list[str] = []
    for c in mm_ordered_constructs:
        for item in mm.construct_items(c):
            if item not in mm_variables:
                mm_variables.append(item)

    all_constructs = sm.construct_names()
    n_constructs = len(all_constructs)
    n_items = len(mm_variables)

    # Precompute construct-to-item-index mapping
    construct_item_map: dict[str, list[int]] = {}
    for c in all_constructs:
        items = mm.construct_items(c)
        construct_item_map[c] = [mm_variables.index(item) for item in items]

    # Determine measurement mode function for each construct
    mode_fns: dict[str, str] = {}
    for c in all_constructs:
        mode = mm.construct_mode(c)
        if mode in ("A", "C", "HOCA"):
            mode_fns[c] = "A"
        elif mode in ("B", "HOCB"):
            mode_fns[c] = "B"
        elif mode == "UNIT":
            mode_fns[c] = "UNIT"
        else:
            mode_fns[c] = "A"  # fallback

    # Standardize data (matches R's scale())
    data_arr = obs_data[mm_variables].values.astype(np.float64)
    mean_data = data_arr.mean(axis=0)
    sd_data = data_arr.std(axis=0, ddof=1)
    sd_data[sd_data == 0] = 1.0
    norm_data = (data_arr - mean_data) / sd_data

    n_obs = norm_data.shape[0]

    # Identify endogenous constructs
    endogenous = sm.all_endogenous()

    # Initialize outer weights: 1 where construct-item relationship exists
    outer_weights = np.zeros((n_items, n_constructs))
    for i, c in enumerate(all_constructs):
        for idx in construct_item_map[c]:
            outer_weights[idx, i] = 1.0

    # Weights mask (1 where relationships exist)
    weights_mask = outer_weights.copy()

    # Paths adjacency matrix: paths_matrix[source, target] = 1
    paths_matrix = np.zeros((n_constructs, n_constructs))
    name_to_idx = {name: i for i, name in enumerate(all_constructs)}
    sm_sources = sm.path_sources()
    sm_targets = sm.path_targets()
    for s, t in zip(sm_sources, sm_targets):
        paths_matrix[name_to_idx[s], name_to_idx[t]] = 1.0

    # Select inner weighting function
    if inner_weights_scheme == "path":
        inner_weight_fn = path_weighting
    elif inner_weights_scheme == "factorial":
        inner_weight_fn = path_factorial
    else:
        raise ValueError(f"inner_weights must be 'path' or 'factorial', got '{inner_weights_scheme}'")

    # -- Iterative PLS Algorithm --
    iterations = 0
    weight_diff = float("inf")

    for iteration in range(max_it + 1):
        iterations = iteration

        # 1. Construct scores from outer weights
        construct_scores = norm_data @ outer_weights
        construct_scores = standardize(construct_scores)

        # 2. Inner weighting
        inner_paths = inner_weight_fn(
            sm_sources=sm_sources,
            sm_targets=sm_targets,
            construct_scores=construct_scores,
            construct_names=all_constructs,
            endogenous=endogenous,
            paths_matrix=paths_matrix,
        )

        # 3. Update construct scores from inner weights
        construct_scores = construct_scores @ inner_paths
        construct_scores = standardize(construct_scores)

        # 4. Save and update outer weights
        last_outer_weights = outer_weights.copy()

        for i, c in enumerate(all_constructs):
            item_idx = construct_item_map[c]
            mode = mode_fns[c]
            if mode == "A":
                w = outer_mode_A(norm_data, construct_scores[:, i], item_idx)
            elif mode == "B":
                w = outer_mode_B(norm_data, construct_scores[:, i], item_idx)
            elif mode == "UNIT":
                w = outer_unit(norm_data, construct_scores[:, i], item_idx)
            else:
                w = outer_mode_A(norm_data, construct_scores[:, i], item_idx)

            outer_weights[item_idx, i] = w.ravel()

        # 5. Standardize outer weights
        outer_weights = standardize_weights(norm_data, outer_weights)

        # 6. Check convergence
        weight_diff = np.sum(np.abs(outer_weights - last_outer_weights))
        if weight_diff < 10 ** (-stop_criterion):
            break

    # -- Post-convergence computations --

    # Final construct scores
    construct_scores = norm_data @ outer_weights

    # Outer loadings: cov(data, scores) masked by weights
    outer_loadings = cov_with(norm_data, construct_scores) * weights_mask

    # Path coefficients: OLS regression for each endogenous construct
    path_coef = np.zeros((n_constructs, n_constructs))
    for dep in endogenous:
        dep_idx = name_to_idx[dep]
        ant_names = sm.construct_antecedents(dep)
        if not ant_names:
            continue
        ant_idx = [name_to_idx[a] for a in ant_names]
        X = construct_scores[:, ant_idx]
        y = construct_scores[:, dep_idx]
        beta = np.linalg.solve(X.T @ X, X.T @ y)
        for j, ai in enumerate(ant_idx):
            path_coef[ai, dep_idx] = beta[j]

    # R² for endogenous constructs
    cor_scores = np.corrcoef(construct_scores, rowvar=False)
    cor_scores = np.nan_to_num(cor_scores, nan=0.0)
    r_squared_vals = np.zeros(n_constructs)
    adjusted_r_squared_vals = np.zeros(n_constructs)
    for dep in endogenous:
        dep_idx = name_to_idx[dep]
        ant_names = sm.construct_antecedents(dep)
        ant_idx = [name_to_idx[a] for a in ant_names]
        if ant_idx:
            predicted = construct_scores[:, ant_idx] @ path_coef[ant_idx, dep_idx]
            ss_total = np.sum((construct_scores[:, dep_idx] - construct_scores[:, dep_idx].mean()) ** 2)
            ss_res = np.sum((construct_scores[:, dep_idx] - predicted) ** 2)
            if ss_total > 0:
                r2 = 1.0 - ss_res / ss_total
            else:
                r2 = 0.0
            r_squared_vals[dep_idx] = r2
            # Adjusted R²
            p = len(ant_idx)
            if n_obs - p - 1 > 0:
                adjusted_r_squared_vals[dep_idx] = 1.0 - (1.0 - r2) * (n_obs - 1) / (n_obs - p - 1)

    # -- Package results as DataFrames --
    path_coef_df = pd.DataFrame(
        path_coef, index=all_constructs, columns=all_constructs
    )
    outer_loadings_df = pd.DataFrame(
        outer_loadings, index=mm_variables, columns=all_constructs
    )
    outer_weights_df = pd.DataFrame(
        outer_weights, index=mm_variables, columns=all_constructs
    )
    construct_scores_df = pd.DataFrame(
        construct_scores, index=obs_data.index, columns=all_constructs
    )
    r_squared_df = pd.DataFrame(
        {"R2": r_squared_vals, "Adj_R2": adjusted_r_squared_vals},
        index=all_constructs,
    )

    return PLSModel(
        path_coef=path_coef_df,
        outer_loadings=outer_loadings_df,
        outer_weights=outer_weights_df,
        construct_scores=construct_scores_df,
        r_squared=r_squared_df,
        data=obs_data,
        raw_data=obs_data,
        mean_data=pd.Series(mean_data, index=mm_variables),
        sd_data=pd.Series(sd_data, index=mm_variables),
        mm_matrix=mm,
        sm_matrix=sm,
        measurement_model=mm,
        construct_names_list=all_constructs,
        mm_variables=mm_variables,
        iterations=iterations,
        weight_diff=weight_diff,
    )
