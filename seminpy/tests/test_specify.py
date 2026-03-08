"""Tests for the specification DSL and data structures."""
from __future__ import annotations

import seminpy as sp
from seminpy.mm_matrix import MeasurementMatrix
from seminpy.sm_matrix import StructuralMatrix


# ── multi_items / single_item ────────────────────────────────────────────────

class TestMultiItems:
    def test_basic(self):
        items = sp.multi_items("IMAG", 1, 5)
        assert items == ["IMAG1", "IMAG2", "IMAG3", "IMAG4", "IMAG5"]

    def test_single_range(self):
        items = sp.multi_items("X", 3, 3)
        assert items == ["X3"]

    def test_prefix_style(self):
        items = sp.multi_items("CUEX", 1, 3)
        assert items == ["CUEX1", "CUEX2", "CUEX3"]


class TestSingleItem:
    def test_passthrough(self):
        assert sp.single_item("CUSCO") == "CUSCO"


# ── reflective / composite ──────────────────────────────────────────────────

class TestReflective:
    def test_creates_spec(self):
        spec = sp.reflective("Image", sp.multi_items("IMAG", 1, 5))
        assert spec.name == "Image"
        assert spec.items == ["IMAG1", "IMAG2", "IMAG3", "IMAG4", "IMAG5"]
        assert spec.mode == sp.WeightMode.MODE_C
        assert spec.construct_type == "reflective"

    def test_single_item_string(self):
        spec = sp.reflective("Complaints", "CUSCO")
        assert spec.items == ["CUSCO"]


class TestComposite:
    def test_default_mode_a(self):
        spec = sp.composite("Value", sp.multi_items("PERV", 1, 2))
        assert spec.mode == sp.WeightMode.MODE_A
        assert spec.construct_type == "composite"

    def test_mode_b(self):
        spec = sp.composite("Value", sp.multi_items("PERV", 1, 2), weights=sp.mode_B)
        assert spec.mode == sp.WeightMode.MODE_B

    def test_mode_aliases(self):
        assert sp.mode_A == sp.correlation_weights
        assert sp.mode_B == sp.regression_weights


# ── constructs() ─────────────────────────────────────────────────────────────

class TestConstructs:
    def test_builds_measurement_matrix(self):
        mm = sp.constructs(
            sp.reflective("Image", sp.multi_items("IMAG", 1, 3)),
            sp.composite("Value", sp.multi_items("PERV", 1, 2)),
        )
        assert isinstance(mm, MeasurementMatrix)
        assert mm.all_constructs() == ["Image", "Value"]
        assert mm.construct_items("Image") == ["IMAG1", "IMAG2", "IMAG3"]
        assert mm.construct_items("Value") == ["PERV1", "PERV2"]

    def test_construct_modes(self):
        mm = sp.constructs(
            sp.reflective("A", ["x1", "x2"]),
            sp.composite("B", ["y1", "y2"], weights=sp.mode_B),
        )
        assert mm.is_reflective("A")
        assert mm.is_mode_B("B")
        assert not mm.is_reflective("B")

    def test_full_mobi(self, mobi_mm):
        assert len(mobi_mm.all_constructs()) == 7
        assert mobi_mm.construct_items("Quality") == [
            "PERQ1", "PERQ2", "PERQ3", "PERQ4", "PERQ5", "PERQ6", "PERQ7"
        ]
        assert mobi_mm.is_single_item("Complaints")
        assert not mobi_mm.is_single_item("Image")


# ── Builder API (Style A) ───────────────────────────────────────────────────

class TestBuilderAPI:
    def test_measurement_model_builder(self):
        mm = (MeasurementMatrix()
            .add_reflective("Image", ["IMAG1", "IMAG2", "IMAG3"])
            .add_composite("Value", ["PERV1", "PERV2"], mode="B")
        )
        assert mm.all_constructs() == ["Image", "Value"]
        assert mm.is_reflective("Image")
        assert mm.is_mode_B("Value")

    def test_structural_model_builder(self):
        sm = (StructuralMatrix()
            .add_paths(from_="Image", to=["Expectation", "Satisfaction"])
            .add_paths(from_="Satisfaction", to="Loyalty")
        )
        assert len(sm) == 3
        assert sm.construct_antecedents("Satisfaction") == ["Image"]
        assert sm.construct_targets("Image") == ["Expectation", "Satisfaction"]


# ── relationships() / paths() ───────────────────────────────────────────────

class TestRelationships:
    def test_single_path(self):
        sm = sp.relationships(sp.paths(from_="A", to="B"))
        assert sm.construct_names() == ["A", "B"]
        assert sm.all_endogenous() == ["B"]
        assert sm.all_exogenous() == ["A"]

    def test_multiple_targets(self):
        sm = sp.relationships(sp.paths(from_="A", to=["B", "C", "D"]))
        assert len(sm) == 3
        assert sm.construct_targets("A") == ["B", "C", "D"]

    def test_full_mobi(self, mobi_sm):
        assert len(mobi_sm) == 12  # 12 paths in the ECSI model
        assert set(mobi_sm.only_exogenous()) == {"Image"}
        assert mobi_sm.construct_antecedents("Satisfaction") == [
            "Image", "Expectation", "Quality", "Value"
        ]


# ── MeasurementMatrix accessors ─────────────────────────────────────────────

class TestMeasurementMatrixAccessors:
    def test_all_items(self, mobi_mm):
        items = mobi_mm.all_items()
        assert len(items) == 24
        assert "IMAG1" in items
        assert "CUSCO" in items

    def test_construct_of_item(self, mobi_mm):
        assert mobi_mm.construct_of_item("IMAG3") == "Image"
        assert mobi_mm.construct_of_item("CUSCO") == "Complaints"

    def test_construct_mode(self, mobi_mm):
        assert mobi_mm.construct_mode("Image") == "C"  # reflective


# ── StructuralMatrix accessors ──────────────────────────────────────────────

class TestStructuralMatrixAccessors:
    def test_endogenous_exogenous(self, mobi_sm):
        assert "Image" not in mobi_sm.all_endogenous()
        assert "Image" in mobi_sm.all_exogenous()

    def test_interaction_detection(self):
        assert StructuralMatrix.is_interaction("A*B")
        assert not StructuralMatrix.is_interaction("AB")

    def test_path_labels(self):
        sm = sp.relationships(sp.paths(from_="A", to="B"))
        assert sm.path_labels() == ["A -> B"]

    def test_mutators(self):
        sm = sp.relationships(
            sp.paths(from_="A", to=["B", "C"]),
            sp.paths(from_="B", to="C"),
        )
        sm2 = sm.remove_paths_to("C")
        assert len(sm2) == 1
        assert sm2.path_labels() == ["A -> B"]
