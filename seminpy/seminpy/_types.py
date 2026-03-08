"""Type definitions, enums, and protocols for seminpy."""
from __future__ import annotations

from enum import Enum
from typing import NamedTuple


class WeightMode(Enum):
    """Outer weighting mode for composite constructs."""
    MODE_A = "A"           # Correlation weights
    MODE_B = "B"           # Regression weights
    MODE_C = "C"           # Reflective (PLSc)
    UNIT = "UNIT"          # Equal weights (single items, interactions)
    HOCA = "HOCA"          # Higher-order composite Mode A
    HOCB = "HOCB"          # Higher-order composite Mode B


class InnerWeighting(Enum):
    """Inner weighting scheme for PLS algorithm."""
    PATH = "path"
    FACTORIAL = "factorial"


class ConstructSpec(NamedTuple):
    """Specification of a single construct."""
    name: str
    items: list[str]
    mode: WeightMode
    construct_type: str  # "reflective", "composite", "higher_order_composite", "higher_order_reflective"


class PathSpec(NamedTuple):
    """A single structural path specification (from one source to one target)."""
    source: str
    target: str


# Convenience sentinels matching R's API
mode_A = WeightMode.MODE_A
mode_B = WeightMode.MODE_B
mode_plsc = WeightMode.MODE_C
correlation_weights = WeightMode.MODE_A
regression_weights = WeightMode.MODE_B
