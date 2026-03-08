"""Tests for PLS-SEM estimation.

Validates numerical results against R/seminr reference values.
"""
from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

import seminpy as sp


class TestEstimatePLS:
    """Basic PLS estimation tests."""

    def test_returns_pls_model(self, mobi_pls):
        assert isinstance(mobi_pls, sp.PLSModel)

    def test_convergence(self, mobi_pls):
        # R seminr converges in 6 iterations
        assert mobi_pls.iterations <= 10
        assert mobi_pls.weight_diff < 1e-7

    def test_construct_count(self, mobi_pls):
        assert len(mobi_pls.construct_names_list) == 7

    def test_indicator_count(self, mobi_pls):
        assert len(mobi_pls.mm_variables) == 24

    def test_scores_shape(self, mobi_pls):
        assert mobi_pls.construct_scores.shape == (250, 7)

    def test_path_coef_shape(self, mobi_pls):
        assert mobi_pls.path_coef.shape == (7, 7)

    def test_loadings_shape(self, mobi_pls):
        assert mobi_pls.outer_loadings.shape == (24, 7)

    def test_weights_shape(self, mobi_pls):
        assert mobi_pls.outer_weights.shape == (24, 7)


class TestNumericalAccuracy:
    """Validate against R/seminr composite reference values (no PLSc).

    The MOBI model with all reflective constructs uses PLSc in R, which
    corrects loadings and path coefficients. Since PLSc is not yet
    implemented, we validate against composite (no PLSc) reference values.
    The core PLS algorithm (weights, convergence) is identical regardless.
    """

    @pytest.fixture
    def composite_model(self, mobi_data, mobi_sm):
        """MOBI model with all composites (no PLSc correction)."""
        mm = sp.constructs(
            sp.composite("Image",        sp.multi_items("IMAG", 1, 5)),
            sp.composite("Expectation",  sp.multi_items("CUEX", 1, 3)),
            sp.composite("Quality",      sp.multi_items("PERQ", 1, 7)),
            sp.composite("Value",        sp.multi_items("PERV", 1, 2)),
            sp.composite("Satisfaction", sp.multi_items("CUSA", 1, 3)),
            sp.composite("Complaints",   sp.single_item("CUSCO")),
            sp.composite("Loyalty",      sp.multi_items("CUSL", 1, 3)),
        )
        return sp.estimate_pls(
            data=mobi_data, measurement_model=mm, structural_model=mobi_sm
        )

    def test_path_coefficients(self, composite_model, ref_path_coef):
        for source in ref_path_coef.index:
            for target in ref_path_coef.columns:
                r_val = ref_path_coef.loc[source, target]
                if abs(r_val) > 1e-10:
                    py_val = composite_model.path_coef.loc[source, target]
                    np.testing.assert_allclose(
                        py_val, r_val, atol=1e-4,
                        err_msg=f"Path {source} -> {target}: Python={py_val:.6f}, R={r_val:.6f}"
                    )

    def test_outer_loadings(self, composite_model, ref_outer_loadings):
        for item in ref_outer_loadings.index:
            for construct in ref_outer_loadings.columns:
                r_val = ref_outer_loadings.loc[item, construct]
                if abs(r_val) > 1e-10:
                    py_val = composite_model.outer_loadings.loc[item, construct]
                    np.testing.assert_allclose(
                        py_val, r_val, atol=1e-4,
                        err_msg=f"Loading {item} <- {construct}: Python={py_val:.6f}, R={r_val:.6f}"
                    )

    def test_outer_weights(self, composite_model, ref_outer_weights):
        for item in ref_outer_weights.index:
            for construct in ref_outer_weights.columns:
                r_val = ref_outer_weights.loc[item, construct]
                if abs(r_val) > 1e-10:
                    py_val = composite_model.outer_weights.loc[item, construct]
                    np.testing.assert_allclose(
                        py_val, r_val, atol=1e-4,
                        err_msg=f"Weight {item} <- {construct}: Python={py_val:.6f}, R={r_val:.6f}"
                    )


class TestR2:
    """R-squared values for endogenous constructs."""

    def test_r2_positive_for_endogenous(self, mobi_pls):
        endogenous = mobi_pls.sm_matrix.all_endogenous()
        for construct in endogenous:
            r2 = mobi_pls.r_squared.loc[construct, "R2"]
            assert r2 > 0, f"R² for {construct} should be positive, got {r2}"

    def test_r2_zero_for_exogenous(self, mobi_pls):
        exogenous = mobi_pls.sm_matrix.only_exogenous()
        for construct in exogenous:
            r2 = mobi_pls.r_squared.loc[construct, "R2"]
            assert r2 == 0.0, f"R² for exogenous {construct} should be 0"


class TestEdgeCases:
    """Edge cases and error handling."""

    def test_simple_two_construct_model(self, mobi_data):
        mm = sp.constructs(
            sp.composite("Image", sp.multi_items("IMAG", 1, 5)),
            sp.composite("Loyalty", sp.multi_items("CUSL", 1, 3)),
        )
        sm = sp.relationships(sp.paths(from_="Image", to="Loyalty"))
        model = sp.estimate_pls(data=mobi_data, measurement_model=mm, structural_model=sm)
        assert model.iterations < 300
        assert model.path_coef.loc["Image", "Loyalty"] != 0

    def test_mode_b_construct(self, mobi_data):
        mm = sp.constructs(
            sp.composite("Image", sp.multi_items("IMAG", 1, 5), weights=sp.mode_B),
            sp.composite("Loyalty", sp.multi_items("CUSL", 1, 3)),
        )
        sm = sp.relationships(sp.paths(from_="Image", to="Loyalty"))
        model = sp.estimate_pls(data=mobi_data, measurement_model=mm, structural_model=sm)
        assert model.iterations < 300

    def test_repr(self, mobi_pls):
        r = repr(mobi_pls)
        assert "PLSModel" in r
        assert "250" in r
        assert "7" in r


class TestBuilderStyleEstimation:
    """Test that Style A (builder) works with estimate_pls."""

    def test_builder_produces_same_results(self, mobi_data, mobi_pls):
        mm = (sp.MeasurementModel()
            .add_reflective("Image",        sp.multi_items("IMAG", 1, 5))
            .add_reflective("Expectation",  sp.multi_items("CUEX", 1, 3))
            .add_reflective("Quality",      sp.multi_items("PERQ", 1, 7))
            .add_reflective("Value",        sp.multi_items("PERV", 1, 2))
            .add_reflective("Satisfaction", sp.multi_items("CUSA", 1, 3))
            .add_reflective("Complaints",   ["CUSCO"])
            .add_reflective("Loyalty",      sp.multi_items("CUSL", 1, 3))
        )
        sm = (sp.StructuralModel()
            .add_paths(from_="Image",        to=["Expectation", "Satisfaction", "Loyalty"])
            .add_paths(from_="Expectation",  to=["Quality", "Value", "Satisfaction"])
            .add_paths(from_="Quality",      to=["Value", "Satisfaction"])
            .add_paths(from_="Value",        to=["Satisfaction"])
            .add_paths(from_="Satisfaction", to=["Complaints", "Loyalty"])
            .add_paths(from_="Complaints",   to="Loyalty")
        )
        model = sp.estimate_pls(data=mobi_data, measurement_model=mm, structural_model=sm)

        # Should produce identical results
        np.testing.assert_allclose(
            model.path_coef.values, mobi_pls.path_coef.values, atol=1e-10,
            err_msg="Builder and DSL APIs should produce identical results"
        )
