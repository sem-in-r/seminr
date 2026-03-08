"""seminpy: Building and Estimating Structural Equation Models in Python.

A Python port of the R package seminr, providing a natural-feeling DSL
for specifying and estimating PLS-SEM models.

Two API styles are supported:

    # Style A: Builder pattern (Pythonic)
    mm = (sp.MeasurementModel()
        .add_reflective("Image", ["IMAG1", "IMAG2", "IMAG3"])
        .add_composite("Value", ["PERV1", "PERV2"], mode="B")
    )
    sm = (sp.StructuralModel()
        .add_paths(from_="Image", to=["Value", "Satisfaction"])
    )

    # Style C: seminr-style DSL
    mm = sp.constructs(
        sp.reflective("Image", sp.multi_items("IMAG", 1, 3)),
        sp.composite("Value", sp.multi_items("PERV", 1, 2), weights=sp.mode_B),
    )
    sm = sp.relationships(
        sp.paths(from_="Image", to=["Value", "Satisfaction"]),
    )
"""
from __future__ import annotations

# Types & sentinels
from ._types import (
    WeightMode,
    InnerWeighting,
    mode_A,
    mode_B,
    mode_plsc,
    correlation_weights,
    regression_weights,
)

# Data structures (Builder API - Style A)
from .mm_matrix import MeasurementMatrix
from .sm_matrix import StructuralMatrix

# Specification DSL (seminr-style - Style C)
from .constructs import (
    reflective,
    composite,
    higher_composite,
    higher_reflective,
    multi_items,
    single_item,
)
from .specify import constructs, relationships, paths

# Estimation
from .estimate_pls import estimate_pls, PLSModel

# Datasets
from . import datasets

# Missing data
from .missing import mean_replacement

__version__ = "0.1.0.dev0"

__all__ = [
    # Types
    "WeightMode",
    "InnerWeighting",
    "mode_A",
    "mode_B",
    "mode_plsc",
    "correlation_weights",
    "regression_weights",
    # Data structures (Style A)
    "MeasurementModel",
    "StructuralModel",
    # Specification (Style C)
    "reflective",
    "composite",
    "higher_composite",
    "higher_reflective",
    "multi_items",
    "single_item",
    "constructs",
    "relationships",
    "paths",
    # Estimation
    "estimate_pls",
    "PLSModel",
    # Datasets
    "datasets",
    # Missing
    "mean_replacement",
]

# Aliases for builder-style imports
MeasurementModel = MeasurementMatrix
StructuralModel = StructuralMatrix
