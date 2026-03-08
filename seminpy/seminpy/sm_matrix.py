"""StructuralMatrix: internal structural model specification.

Wraps a pandas DataFrame with columns [source, target].
Ports R's smMatrix character matrix and all helpers-smMatrix.R accessors.
"""
from __future__ import annotations

import re

import pandas as pd

from ._types import PathSpec


class StructuralMatrix:
    """Internal structural model matrix.

    Each row represents a directed path from source construct to target construct.
    Columns: source, target.
    """

    def __init__(self, df: pd.DataFrame | None = None):
        if df is not None:
            self._df = df.copy()
        else:
            self._df = pd.DataFrame(columns=["source", "target"])

    # -- Builder API (Alternative A) -------------------------------------------

    def add_paths(
        self, from_: str, to: str | list[str]
    ) -> "StructuralMatrix":
        """Add path(s) from one source to one or more targets. Returns self for chaining."""
        if isinstance(to, str):
            to = [to]
        rows = pd.DataFrame({"source": [from_] * len(to), "target": to})
        self._df = pd.concat([self._df, rows], ignore_index=True)
        return self

    # -- Construct from PathSpec list (Alternative C support) -------------------

    @classmethod
    def from_specs(cls, specs: list[PathSpec]) -> "StructuralMatrix":
        """Build from a list of PathSpec tuples (used by relationships())."""
        df = pd.DataFrame(
            [(s.source, s.target) for s in specs],
            columns=["source", "target"],
        )
        return cls(df)

    # -- Selectors -------------------------------------------------------------

    def construct_names(self) -> list[str]:
        """All unique construct names (sources + targets), preserving order."""
        seen: dict[str, None] = {}
        for s in self._df["source"]:
            seen.setdefault(s, None)
        for t in self._df["target"]:
            seen.setdefault(t, None)
        return list(seen)

    def all_endogenous(self) -> list[str]:
        """All constructs that appear as targets."""
        return list(dict.fromkeys(self._df["target"]))

    def all_exogenous(self) -> list[str]:
        """All constructs that appear as sources."""
        return list(dict.fromkeys(self._df["source"]))

    def only_exogenous(self) -> list[str]:
        """Constructs that are sources but never targets (purely exogenous)."""
        targets = set(self._df["target"])
        return [s for s in self.all_exogenous() if s not in targets]

    def only_endogenous(self) -> list[str]:
        """Constructs that are targets but never sources (purely endogenous)."""
        sources = set(self._df["source"])
        return [t for t in self.all_endogenous() if t not in sources]

    # -- Accessors -------------------------------------------------------------

    def construct_antecedents(self, outcome: str) -> list[str]:
        """Source constructs that predict a given target."""
        mask = self._df["target"] == outcome
        return list(self._df.loc[mask, "source"])

    def construct_targets(self, source: str) -> list[str]:
        """Target constructs predicted by a given source."""
        mask = self._df["source"] == source
        return list(self._df.loc[mask, "target"])

    # -- Predicates ------------------------------------------------------------

    @staticmethod
    def is_interaction(construct_name: str) -> bool:
        """Test if a construct name is an interaction term (contains '*')."""
        return "*" in construct_name

    def has_interactions(self, outcome: str | None = None) -> bool:
        """Test if the model (or a specific DV's antecedents) includes interaction terms."""
        if outcome is None:
            return any(self.is_interaction(n) for n in self.construct_names())
        return any(self.is_interaction(a) for a in self.construct_antecedents(outcome))

    def has_paths_to(self, target: str) -> bool:
        """Test if any paths target a given construct."""
        return target in set(self._df["target"])

    # -- Row-level accessors ---------------------------------------------------

    def path_sources(self) -> list[str]:
        """All source values, one per row (not unique)."""
        return list(self._df["source"])

    def path_targets(self) -> list[str]:
        """All target values, one per row (not unique)."""
        return list(self._df["target"])

    def path_labels(self) -> list[str]:
        """Formatted 'source -> target' labels."""
        return [
            f"{s} -> {t}"
            for s, t in zip(self._df["source"], self._df["target"])
        ]

    # -- Mutators --------------------------------------------------------------

    def remove_paths_to(self, target: str | list[str]) -> "StructuralMatrix":
        """Return new StructuralMatrix without paths to the given target(s)."""
        if isinstance(target, str):
            target = [target]
        mask = ~self._df["target"].isin(target)
        return StructuralMatrix(self._df[mask])

    def remove_paths_from(self, source: str | list[str]) -> "StructuralMatrix":
        """Return new StructuralMatrix without paths from the given source(s)."""
        if isinstance(source, str):
            source = [source]
        mask = ~self._df["source"].isin(source)
        return StructuralMatrix(self._df[mask])

    # -- Utilities -------------------------------------------------------------

    @property
    def df(self) -> pd.DataFrame:
        """Access the underlying DataFrame (read-only copy)."""
        return self._df.copy()

    def __len__(self) -> int:
        return len(self._df)

    def __repr__(self) -> str:
        paths = self.path_labels()
        return "StructuralMatrix(\n" + "\n".join(f"  {p}" for p in paths) + "\n)"
