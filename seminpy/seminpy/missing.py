"""Missing data handling for seminpy.

Ports R's clean_data.R: mean_replacement.
"""
from __future__ import annotations

import pandas as pd


def mean_replacement(data: pd.DataFrame) -> pd.DataFrame:
    """Replace NaN values with column means.

    Matches R's mean_replacement() function in seminr.

    Args:
        data: DataFrame that may contain NaN values.

    Returns:
        DataFrame with NaN replaced by column means.
    """
    return data.fillna(data.mean())
