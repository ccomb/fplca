"""Contract tests for ClassificationFilter / MatchMode normalisation."""

from __future__ import annotations

import dataclasses
import json

import pytest

from volca import ClassificationFilter, MatchMode


def test_string_form_normalises_to_enum() -> None:
    f = ClassificationFilter("Category", "Food", "exact")
    assert f.mode is MatchMode.EXACT


def test_enum_form_preserved() -> None:
    f = ClassificationFilter("Category", "Food", MatchMode.EXACT)
    assert f.mode is MatchMode.EXACT


def test_default_is_contains() -> None:
    f = ClassificationFilter("Category", "Food")
    assert f.mode is MatchMode.CONTAINS


@pytest.mark.parametrize("bad", ["exct", "Exact", "EXACT", "", "near"])
def test_invalid_string_raises(bad: str) -> None:
    with pytest.raises(ValueError, match="mode must be one of"):
        ClassificationFilter("Category", "Food", bad)  # type: ignore[arg-type]


def test_is_frozen() -> None:
    f = ClassificationFilter("Category", "Food", "exact")
    with pytest.raises(dataclasses.FrozenInstanceError):
        f.mode = MatchMode.CONTAINS  # type: ignore[misc]


def test_equality_across_construction_forms() -> None:
    assert ClassificationFilter("s", "v", "exact") == ClassificationFilter(
        "s", "v", MatchMode.EXACT
    )


def test_mode_json_serialises_as_string() -> None:
    """MatchMode inherits from str, so it must round-trip through json as the
    bare wire value, not as ``"MatchMode.EXACT"`` or the enum repr."""
    assert json.dumps(MatchMode.EXACT) == '"exact"'
    payload = dataclasses.asdict(ClassificationFilter("s", "v", "exact"))
    assert json.dumps(payload) == '{"system": "s", "value": "v", "mode": "exact"}'
