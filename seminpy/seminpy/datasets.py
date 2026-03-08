"""Built-in datasets for seminpy.

Provides the same sample datasets as seminr for tutorials and testing.
"""
from __future__ import annotations

from importlib import resources

import pandas as pd


def load_mobi() -> pd.DataFrame:
    """Load the MOBI (European Customer Satisfaction Index) dataset.

    250 observations, 24 indicator variables across 7 constructs:
    - Image (IMAG1-5)
    - Expectation (CUEX1-3)
    - Quality (PERQ1-7)
    - Value (PERV1-2)
    - Satisfaction (CUSA1-3)
    - Complaints (CUSCO)
    - Loyalty (CUSL1-3)

    Returns:
        DataFrame with 250 rows and 24 columns.
    """
    data_path = resources.files("seminpy") / "data" / "mobi.csv"
    return pd.read_csv(data_path)
