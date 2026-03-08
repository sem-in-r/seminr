"""Construct specification functions.

Ports R's specify_constructs.R: reflective(), composite(), multi_items(),
single_item(), higher_composite(), higher_reflective().
"""
from __future__ import annotations

from ._types import ConstructSpec, WeightMode


def reflective(construct_name: str, item_names: str | list[str]) -> ConstructSpec:
    """Define a reflective (common factor) construct.

    Automatically estimated with PLSc correction.

    Args:
        construct_name: Name of the construct.
        item_names: Indicator item name(s). Use multi_items() or single_item().

    Returns:
        A ConstructSpec for use in constructs().
    """
    if isinstance(item_names, str):
        item_names = [item_names]
    return ConstructSpec(
        name=construct_name,
        items=list(item_names),
        mode=WeightMode.MODE_C,
        construct_type="reflective",
    )


def composite(
    construct_name: str,
    item_names: str | list[str],
    weights: WeightMode = WeightMode.MODE_A,
) -> ConstructSpec:
    """Define a composite construct.

    Args:
        construct_name: Name of the construct.
        item_names: Indicator item name(s). Use multi_items() or single_item().
        weights: WeightMode.MODE_A (correlation, default) or WeightMode.MODE_B (regression).

    Returns:
        A ConstructSpec for use in constructs().
    """
    if isinstance(item_names, str):
        item_names = [item_names]
    if weights not in (WeightMode.MODE_A, WeightMode.MODE_B, WeightMode.UNIT, WeightMode.MODE_C):
        raise ValueError(
            f"Composite weights must be mode_A, mode_B, or unit, got {weights}"
        )
    return ConstructSpec(
        name=construct_name,
        items=list(item_names),
        mode=weights,
        construct_type="composite",
    )


def higher_composite(
    construct_name: str,
    dimensions: str | list[str],
    weights: WeightMode = WeightMode.MODE_A,
) -> ConstructSpec:
    """Define a higher-order composite construct (two-stage estimation).

    Args:
        construct_name: Name of the second-order construct.
        dimensions: First-order construct names to aggregate.
        weights: WeightMode.MODE_A (default) or WeightMode.MODE_B.

    Returns:
        A ConstructSpec for use in constructs().
    """
    if isinstance(dimensions, str):
        dimensions = [dimensions]
    type_code = WeightMode.HOCA if weights == WeightMode.MODE_A else WeightMode.HOCB
    return ConstructSpec(
        name=construct_name,
        items=list(dimensions),
        mode=type_code,
        construct_type="higher_order_composite",
    )


def higher_reflective(
    construct_name: str,
    dimensions: str | list[str],
) -> ConstructSpec:
    """Define a higher-order reflective construct.

    Args:
        construct_name: Name of the second-order construct.
        dimensions: First-order construct names.

    Returns:
        A ConstructSpec for use in constructs().
    """
    if isinstance(dimensions, str):
        dimensions = [dimensions]
    return ConstructSpec(
        name=construct_name,
        items=list(dimensions),
        mode=WeightMode.MODE_C,
        construct_type="higher_order_reflective",
    )


def multi_items(item_name: str, start: int, end: int) -> list[str]:
    """Generate numbered indicator names.

    Args:
        item_name: Prefix (e.g. "IMAG").
        start: First number (inclusive).
        end: Last number (inclusive).

    Returns:
        List of names like ["IMAG1", "IMAG2", ..., "IMAG5"].
    """
    return [f"{item_name}{i}" for i in range(start, end + 1)]


def single_item(item: str) -> str:
    """Specify a single indicator item.

    Args:
        item: The indicator column name.

    Returns:
        The item name (passthrough for API consistency with R).
    """
    return item
