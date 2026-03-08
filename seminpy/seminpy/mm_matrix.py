"""MeasurementMatrix: internal measurement model specification.

Wraps a pandas DataFrame with columns [construct, measurement, type].
Ports R's mmMatrix character matrix and all helpers-mmMatrix.R accessors.
"""
from __future__ import annotations

import pandas as pd

from ._types import ConstructSpec, WeightMode


class MeasurementMatrix:
    """Internal measurement model matrix.

    Each row maps one indicator item to its parent construct and estimation mode.
    Columns: construct, measurement, type.
    """

    def __init__(self, df: pd.DataFrame | None = None):
        if df is not None:
            self._df = df.copy()
        else:
            self._df = pd.DataFrame(columns=["construct", "measurement", "type"])

    # -- Builder API (Alternative A) -------------------------------------------

    def add_reflective(
        self, construct_name: str, items: list[str]
    ) -> "MeasurementMatrix":
        """Add a reflective construct. Returns self for chaining."""
        self._add_construct(construct_name, items, "C")
        return self

    def add_composite(
        self, construct_name: str, items: list[str], mode: str = "A"
    ) -> "MeasurementMatrix":
        """Add a composite construct (mode 'A' or 'B'). Returns self for chaining."""
        if mode not in ("A", "B"):
            raise ValueError(f"Composite mode must be 'A' or 'B', got '{mode}'")
        self._add_construct(construct_name, items, mode)
        return self

    def add_higher_composite(
        self, construct_name: str, dimensions: list[str], mode: str = "A"
    ) -> "MeasurementMatrix":
        """Add a higher-order composite construct. Returns self for chaining."""
        type_code = "HOCA" if mode == "A" else "HOCB"
        self._add_construct(construct_name, dimensions, type_code)
        return self

    def add_higher_reflective(
        self, construct_name: str, dimensions: list[str]
    ) -> "MeasurementMatrix":
        """Add a higher-order reflective construct. Returns self for chaining."""
        self._add_construct(construct_name, dimensions, "C")
        return self

    def _add_construct(self, name: str, items: list[str], type_code: str) -> None:
        rows = pd.DataFrame({
            "construct": [name] * len(items),
            "measurement": items,
            "type": [type_code] * len(items),
        })
        self._df = pd.concat([self._df, rows], ignore_index=True)

    # -- Construct from ConstructSpec list (Alternative C support) -------------

    @classmethod
    def from_specs(cls, specs: list[ConstructSpec]) -> "MeasurementMatrix":
        """Build from a list of ConstructSpec tuples (used by constructs())."""
        rows: list[dict[str, str]] = []
        for spec in specs:
            for item in spec.items:
                rows.append({
                    "construct": spec.name,
                    "measurement": item,
                    "type": spec.mode.value,
                })
        df = pd.DataFrame(rows, columns=["construct", "measurement", "type"])
        return cls(df)

    # -- Accessors (port of helpers-mmMatrix.R) --------------------------------

    def all_constructs(self) -> list[str]:
        """All unique construct names, preserving order."""
        return list(dict.fromkeys(self._df["construct"]))

    def construct_items(self, construct_name: str) -> list[str]:
        """Item names for a given construct."""
        mask = self._df["construct"] == construct_name
        return list(self._df.loc[mask, "measurement"])

    def construct_mode(self, construct_name: str) -> str:
        """Raw mode code for a construct ('C', 'A', 'B', 'HOCA', 'HOCB', 'UNIT')."""
        mask = self._df["construct"] == construct_name
        vals = self._df.loc[mask, "type"]
        if vals.empty:
            raise KeyError(f"Construct '{construct_name}' not found")
        return str(vals.iloc[0])

    def construct_mode_enum(self, construct_name: str) -> WeightMode:
        """WeightMode enum for a construct."""
        return WeightMode(self.construct_mode(construct_name))

    def all_items(self) -> list[str]:
        """All unique item names."""
        return list(dict.fromkeys(self._df["measurement"]))

    def construct_of_item(self, item: str) -> str:
        """Reverse lookup: which construct does this item belong to?"""
        mask = self._df["measurement"] == item
        vals = self._df.loc[mask, "construct"]
        if vals.empty:
            raise KeyError(f"Item '{item}' not found")
        return str(vals.iloc[0])

    # -- Predicates ------------------------------------------------------------

    def is_reflective(self, construct: str) -> bool:
        return self.construct_mode(construct) == "C"

    def is_mode_A(self, construct: str) -> bool:
        return self.construct_mode(construct) in ("A", "HOCA")

    def is_mode_B(self, construct: str) -> bool:
        return self.construct_mode(construct) in ("B", "HOCB")

    def is_HOC(self, construct: str) -> bool:
        return self.construct_mode(construct) in ("HOCA", "HOCB")

    def is_unit_weighted(self, construct: str) -> bool:
        return self.construct_mode(construct) == "UNIT"

    def is_single_item(self, construct: str) -> bool:
        return len(self.construct_items(construct)) == 1

    # -- Selectors -------------------------------------------------------------

    def all_reflective(self) -> list[str]:
        """All constructs with mode 'C'."""
        return self._constructs_of_mode("C")

    def all_HOC(self) -> list[str]:
        """All higher-order constructs."""
        return self._constructs_of_mode("HOCA") + self._constructs_of_mode("HOCB")

    def all_LOC(self) -> list[str]:
        """All lower-order (non-HOC) constructs."""
        hocs = set(self.all_HOC())
        return [c for c in self.all_constructs() if c not in hocs]

    def _constructs_of_mode(self, mode: str) -> list[str]:
        mask = self._df["type"] == mode
        return list(dict.fromkeys(self._df.loc[mask, "construct"]))

    # -- Mutators --------------------------------------------------------------

    def append_rows(self, other: "MeasurementMatrix") -> "MeasurementMatrix":
        """Return a new MeasurementMatrix with rows from both."""
        combined = pd.concat([self._df, other._df], ignore_index=True)
        return MeasurementMatrix(combined)

    # -- Utilities -------------------------------------------------------------

    @property
    def df(self) -> pd.DataFrame:
        """Access the underlying DataFrame (read-only copy)."""
        return self._df.copy()

    def __len__(self) -> int:
        return len(self._df)

    def __repr__(self) -> str:
        constructs = self.all_constructs()
        parts = []
        for c in constructs:
            mode = self.construct_mode(c)
            items = self.construct_items(c)
            parts.append(f"  {c} ({mode}): {items}")
        return "MeasurementMatrix(\n" + "\n".join(parts) + "\n)"
