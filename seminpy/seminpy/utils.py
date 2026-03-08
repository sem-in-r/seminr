"""Utility functions for seminpy.

Standardization, safe division, and matrix helpers that match R's behavior exactly.
"""
from __future__ import annotations

import numpy as np


def standardize(x: np.ndarray) -> np.ndarray:
    """Standardize columns to zero mean and unit sample SD.

    Matches R's scale(x, center=TRUE, scale=TRUE) exactly:
    - Centers by column mean
    - Divides by sample standard deviation (ddof=1, i.e. n-1 denominator)
    - Columns with zero variance are left as zero (avoids division by zero)

    Args:
        x: 1-D or 2-D numpy array.

    Returns:
        Standardized array (same shape as input).
    """
    if x.ndim == 1:
        x = x.reshape(-1, 1)
        squeeze = True
    else:
        squeeze = False

    mu = np.mean(x, axis=0)
    sd = np.std(x, axis=0, ddof=1)
    sd[sd == 0] = 1.0
    result = (x - mu) / sd

    if squeeze:
        result = result.ravel()
    return result


def standardize_weights(data: np.ndarray, outer_weights: np.ndarray) -> np.ndarray:
    """Standardize outer weights so that construct scores have unit SD.

    Matches R's standardize_outer_weights():
        scores = data @ weights
        sd_scores = std(scores, ddof=1)  per construct
        weights_new = weights / sd_scores

    Args:
        data: Standardized indicator data (n × p).
        outer_weights: Weight matrix (p × k).

    Returns:
        Standardized weight matrix (p × k).
    """
    scores = data @ outer_weights
    sd_scores = np.std(scores, axis=0, ddof=1)
    sd_scores[sd_scores == 0] = 1.0
    return outer_weights / sd_scores


def safe_corrcoef(x: np.ndarray) -> np.ndarray:
    """Correlation matrix matching R's cor().

    Uses numpy corrcoef with rowvar=False, handles the case where
    input may have constant columns.

    Args:
        x: Data matrix (n × k).

    Returns:
        Correlation matrix (k × k).
    """
    # np.corrcoef with rowvar=False expects variables in columns
    c = np.corrcoef(x, rowvar=False)
    # Handle NaN from constant columns (set to 0)
    c = np.nan_to_num(c, nan=0.0)
    return c


def cov_with(x: np.ndarray, y: np.ndarray) -> np.ndarray:
    """Covariance between columns of x and columns of y.

    Matches R's cov(x, y) which returns a matrix where
    cov[i,j] = cov(x[:,i], y[:,j]) using n-1 denominator.

    Args:
        x: Data matrix (n × p).
        y: Data matrix (n × q).

    Returns:
        Covariance matrix (p × q).
    """
    n = x.shape[0]
    x_centered = x - x.mean(axis=0)
    y_centered = y - y.mean(axis=0)
    return (x_centered.T @ y_centered) / (n - 1)


def cor_with(x: np.ndarray, y: np.ndarray) -> np.ndarray:
    """Correlation between columns of x and columns of y.

    Matches R's cor(x, y).

    Args:
        x: Data matrix (n × p).
        y: Data matrix (n × q).

    Returns:
        Correlation matrix (p × q).
    """
    cov = cov_with(x, y)
    sd_x = np.std(x, axis=0, ddof=1)
    sd_y = np.std(y, axis=0, ddof=1)
    sd_x[sd_x == 0] = 1.0
    sd_y[sd_y == 0] = 1.0
    return cov / np.outer(sd_x, sd_y)
