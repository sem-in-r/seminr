"""Model specification functions.

Ports R's constructs(), relationships(), paths().
Provides the seminr-style DSL (Alternative C API).
"""
from __future__ import annotations

from ._types import ConstructSpec, PathSpec
from .mm_matrix import MeasurementMatrix
from .sm_matrix import StructuralMatrix


def constructs(*specs: ConstructSpec) -> MeasurementMatrix:
    """Build a measurement model from construct specifications.

    This is the seminr-style API (Alternative C). For the builder-style API,
    use MeasurementMatrix() directly with .add_reflective() / .add_composite().

    Args:
        *specs: ConstructSpec objects from reflective(), composite(), etc.

    Returns:
        A MeasurementMatrix object.

    Example:
        mm = constructs(
            reflective("Image", multi_items("IMAG", 1, 5)),
            composite("Value", multi_items("PERV", 1, 2), weights=mode_B),
        )
    """
    return MeasurementMatrix.from_specs(list(specs))


def relationships(*path_specs: PathSpec | list[PathSpec]) -> StructuralMatrix:
    """Build a structural model from path specifications.

    This is the seminr-style API (Alternative C). For the builder-style API,
    use StructuralMatrix() directly with .add_paths().

    Args:
        *path_specs: PathSpec objects from paths(), or lists of PathSpecs.

    Returns:
        A StructuralMatrix object.

    Example:
        sm = relationships(
            paths(from_="Image", to=["Expectation", "Satisfaction"]),
            paths(from_="Satisfaction", to="Loyalty"),
        )
    """
    flat: list[PathSpec] = []
    for spec in path_specs:
        if isinstance(spec, list):
            flat.extend(spec)
        else:
            flat.append(spec)
    return StructuralMatrix.from_specs(flat)


def paths(from_: str, to: str | list[str]) -> list[PathSpec]:
    """Define structural paths from one source to one or more targets.

    Args:
        from_: Source construct name. (Underscore suffix avoids Python keyword.)
        to: Target construct name(s).

    Returns:
        List of PathSpec tuples for use in relationships().

    Example:
        paths(from_="Image", to=["Expectation", "Satisfaction", "Loyalty"])
    """
    if isinstance(to, str):
        to = [to]
    return [PathSpec(source=from_, target=t) for t in to]
