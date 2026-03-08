"""Shared test fixtures for seminpy."""
from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd
import pytest

import seminpy as sp


FIXTURES_DIR = Path(__file__).parent / "fixtures"


@pytest.fixture
def mobi_data():
    """Load the MOBI dataset."""
    return sp.datasets.load_mobi()


@pytest.fixture
def mobi_mm():
    """MOBI measurement model (all reflective, seminr-style)."""
    return sp.constructs(
        sp.reflective("Image",        sp.multi_items("IMAG", 1, 5)),
        sp.reflective("Expectation",  sp.multi_items("CUEX", 1, 3)),
        sp.reflective("Quality",      sp.multi_items("PERQ", 1, 7)),
        sp.reflective("Value",        sp.multi_items("PERV", 1, 2)),
        sp.reflective("Satisfaction", sp.multi_items("CUSA", 1, 3)),
        sp.reflective("Complaints",   sp.single_item("CUSCO")),
        sp.reflective("Loyalty",      sp.multi_items("CUSL", 1, 3)),
    )


@pytest.fixture
def mobi_sm():
    """MOBI structural model."""
    return sp.relationships(
        sp.paths(from_="Image",        to=["Expectation", "Satisfaction", "Loyalty"]),
        sp.paths(from_="Expectation",  to=["Quality", "Value", "Satisfaction"]),
        sp.paths(from_="Quality",      to=["Value", "Satisfaction"]),
        sp.paths(from_="Value",        to=["Satisfaction"]),
        sp.paths(from_="Satisfaction", to=["Complaints", "Loyalty"]),
        sp.paths(from_="Complaints",   to="Loyalty"),
    )


@pytest.fixture
def mobi_pls(mobi_data, mobi_mm, mobi_sm):
    """Estimated MOBI PLS model."""
    return sp.estimate_pls(
        data=mobi_data,
        measurement_model=mobi_mm,
        structural_model=mobi_sm,
    )


# -- Reference values from R/seminr --

@pytest.fixture
def ref_path_coef():
    """Reference path coefficients from R seminr (composite, no PLSc)."""
    return pd.read_csv(FIXTURES_DIR / "mobi_composite_path_coef.csv", index_col=0)


@pytest.fixture
def ref_outer_loadings():
    """Reference outer loadings from R seminr (composite, no PLSc)."""
    return pd.read_csv(FIXTURES_DIR / "mobi_composite_outer_loadings.csv", index_col=0)


@pytest.fixture
def ref_outer_weights():
    """Reference outer weights from R seminr (composite, no PLSc)."""
    return pd.read_csv(FIXTURES_DIR / "mobi_composite_outer_weights.csv", index_col=0)


@pytest.fixture
def ref_plsc_path_coef():
    """Reference path coefficients from R seminr (reflective, WITH PLSc)."""
    return pd.read_csv(FIXTURES_DIR / "mobi_path_coef.csv", index_col=0)
