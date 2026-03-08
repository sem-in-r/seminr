"""Inner and outer weighting functions for the PLS algorithm.

Ports R's library.R: path_weighting, path_factorial, mode_A, mode_B, unit_weights.
"""
from __future__ import annotations

import numpy as np

from .utils import safe_corrcoef


def path_weighting(
    sm_sources: list[str],
    sm_targets: list[str],
    construct_scores: np.ndarray,
    construct_names: list[str],
    endogenous: list[str],
    paths_matrix: np.ndarray,
) -> np.ndarray:
    """Path weighting scheme.

    - Outgoing paths (successors): correlation weights
    - Incoming paths (predecessors): regression (OLS) weights

    Matches R's path_weighting() in library.R.

    Args:
        sm_sources: Source column of smMatrix (one per row).
        sm_targets: Target column of smMatrix (one per row).
        construct_scores: Current construct scores (n × k).
        construct_names: Ordered construct names matching columns.
        endogenous: Names of endogenous constructs.
        paths_matrix: Adjacency matrix (k × k), 1 where path exists.

    Returns:
        Inner weights matrix (k × k).
    """
    cor_matrix = safe_corrcoef(construct_scores)
    # Outgoing: correlations for successor paths
    inner_paths = cor_matrix * paths_matrix.T

    name_to_idx = {name: i for i, name in enumerate(construct_names)}

    # Incoming: regression for predecessor paths
    for dep in endogenous:
        dep_idx = name_to_idx[dep]
        # Find antecedents
        ant_names = [
            sm_sources[i]
            for i in range(len(sm_sources))
            if sm_targets[i] == dep
        ]
        if not ant_names:
            continue
        ant_idx = [name_to_idx[a] for a in ant_names]
        X = construct_scores[:, ant_idx]
        y = construct_scores[:, dep_idx]
        # OLS: beta = (X'X)^{-1} X'y
        beta = np.linalg.solve(X.T @ X, X.T @ y)
        for j, ai in enumerate(ant_idx):
            inner_paths[ai, dep_idx] = beta[j]

    return inner_paths


def path_factorial(
    sm_sources: list[str],
    sm_targets: list[str],
    construct_scores: np.ndarray,
    construct_names: list[str],
    endogenous: list[str],
    paths_matrix: np.ndarray,
) -> np.ndarray:
    """Factorial weighting scheme.

    All adjacent constructs weighted by correlation.

    Matches R's path_factorial() in library.R.
    """
    cor_matrix = safe_corrcoef(construct_scores)
    inner_paths = cor_matrix * (paths_matrix + paths_matrix.T)
    return inner_paths


def outer_mode_A(
    data: np.ndarray, scores: np.ndarray, item_indices: list[int]
) -> np.ndarray:
    """Mode A (correlation) outer weights.

    w_j = cov(x_j, Y) for each indicator j in the construct.
    Matches R's mode_A() in library.R.
    """
    n = data.shape[0]
    X = data[:, item_indices]
    X_centered = X - X.mean(axis=0)
    y_centered = scores - scores.mean()
    return (X_centered.T @ y_centered) / (n - 1)


def outer_mode_B(
    data: np.ndarray, scores: np.ndarray, item_indices: list[int]
) -> np.ndarray:
    """Mode B (regression) outer weights.

    w = cor(X)^{-1} @ cor(X, Y)
    Matches R's mode_B() in library.R.
    """
    X = data[:, item_indices]
    cor_XX = safe_corrcoef(X)
    n = X.shape[0]
    X_centered = X - X.mean(axis=0)
    y_centered = scores - scores.mean()
    sd_X = np.std(X, axis=0, ddof=1)
    sd_y = np.std(scores, ddof=1)
    sd_X[sd_X == 0] = 1.0
    if sd_y == 0:
        sd_y = 1.0
    cor_Xy = ((X_centered.T @ y_centered) / (n - 1)) / (sd_X * sd_y)
    return np.linalg.solve(cor_XX, cor_Xy)


def outer_unit(
    data: np.ndarray, scores: np.ndarray, item_indices: list[int]
) -> np.ndarray:
    """Unit (equal) outer weights."""
    return np.ones(len(item_indices))
