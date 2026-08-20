"""WasteExchange parsing: third top-level Exchange variant.

The engine emits waste flows as their own kind (PR #83): they share the
technosphere matrix with product flows but are tagged separately so callers
can distinguish "waste sent to landfill" from a product input. Orphan waste
(``activityLinkId == nil``, ``targetActivity == null``) is the typical case
for SimaPro "Final waste flows".

This module covers the wire shape for both envelopes:

* ``ExchangeWithUnit``: inner ``{tag: "WasteExchange", isInput, ...}`` plus
  target fields at the envelope level when the cross-DB linker resolved the
  waste output to a treatment activity.
* ``ExchangeDetail``: same inner shape, but the flow is carried as a
  ``{kind: "waste", flow: <wasteFlow>}`` tagged sum.
"""

from __future__ import annotations

import pytest

from volca.types import (
    BiosphereExchange,
    TechnosphereExchange,
    WasteExchange,
    parse_exchange,
    parse_exchange_detail,
)


def _waste_ewu(*, is_input: bool, target: dict | None = None) -> dict:
    out: dict = {
        "exchange": {
            "tag": "WasteExchange",
            "amount": 2.5,
            "isInput": is_input,
        },
        "flowName": "Organic carbon, placed in landfill",
        "unitName": "kg",
        "targetActivityName": (target or {}).get("activityName"),
        "targetLocation": (target or {}).get("location"),
        "targetProcessId": (target or {}).get("processId"),
    }
    return out


def _waste_ed(*, is_input: bool, target: dict | None = None) -> dict:
    out: dict = {
        "exchange": {
            "tag": "WasteExchange",
            "amount": 2.5,
            "isInput": is_input,
        },
        "exchangeUnitName": "kg",
        "flow": {
            "kind": "waste",
            "flow": {"name": "Organic carbon, placed in landfill"},
        },
    }
    if target is not None:
        out["targetActivity"] = target
    return out


class TestParseExchangeWaste:
    def test_orphan_waste_output_maps_to_waste_variant(self):
        ex = parse_exchange(_waste_ewu(is_input=False))
        assert isinstance(ex, WasteExchange)
        assert ex.flow_name == "Organic carbon, placed in landfill"
        assert ex.amount == 2.5
        assert ex.unit == "kg"
        assert ex.is_input is False
        assert ex.target_activity_name is None
        assert ex.target_location is None
        assert ex.target_process_id is None

    def test_linked_waste_input_carries_treatment_target(self):
        target = {
            "activityName": "Landfill of organic waste, FR",
            "location": "FR",
            "processId": "tttt0000-aaaa-bbbb-cccc-111122223333_pppp1111-eeee-ffff-aaaa-444455556666",
        }
        ex = parse_exchange(_waste_ewu(is_input=True, target=target))
        assert isinstance(ex, WasteExchange)
        assert ex.is_input is True
        assert ex.target_activity_name == "Landfill of organic waste, FR"
        assert ex.target_location == "FR"
        assert ex.target_process_id == target["processId"]

    def test_discriminator_flags_set_correctly(self):
        """Duck-typing flags must place waste alongside neither tech nor bio."""
        ex = parse_exchange(_waste_ewu(is_input=False))
        assert ex.is_waste is True
        assert ex.is_biosphere is False
        # is_reference is always False on waste: there's no reference-waste concept.
        assert ex.is_reference is False

    def test_other_variants_are_not_waste(self):
        tech = parse_exchange({
            "exchange": {"tag": "TechnosphereExchange", "amount": 1.0, "role": "Input"},
            "flowName": "wheat", "unitName": "kg",
            "targetActivityName": None, "targetLocation": None, "targetProcessId": None,
        })
        bio = parse_exchange({
            "exchange": {"tag": "BiosphereExchange", "amount": 1.0, "direction": "Emission", "flowId": "11111111-2222-3333-4444-555555555555"},
            "flowName": "CO2", "unitName": "kg",
            "compartment": {"name": "air", "sub": None},
        })
        assert isinstance(tech, TechnosphereExchange) and tech.is_waste is False
        assert isinstance(bio, BiosphereExchange) and bio.is_waste is False

    def test_is_reference_is_defined_on_every_variant(self):
        """All three variants expose `is_reference` so duck-typing callers
        never trip on AttributeError when iterating mixed exchanges."""
        tech = parse_exchange({
            "exchange": {"tag": "TechnosphereExchange", "amount": 1.0, "role": "Input"},
            "flowName": "wheat", "unitName": "kg",
            "targetActivityName": None, "targetLocation": None, "targetProcessId": None,
        })
        bio = parse_exchange({
            "exchange": {"tag": "BiosphereExchange", "amount": 1.0, "direction": "Emission", "flowId": "11111111-2222-3333-4444-555555555555"},
            "flowName": "CO2", "unitName": "kg",
            "compartment": {"name": "air", "sub": None},
        })
        waste = parse_exchange(_waste_ewu(is_input=False))
        assert tech.is_reference is False
        assert bio.is_reference is False
        assert waste.is_reference is False


class TestParseExchangeDetailWaste:
    def test_waste_kind_flow_envelope_parses(self):
        ex = parse_exchange_detail(_waste_ed(is_input=False))
        assert isinstance(ex, WasteExchange)
        assert ex.flow_name == "Organic carbon, placed in landfill"
        assert ex.is_input is False
        assert ex.target_activity_name is None

    def test_waste_with_linked_treatment_target(self):
        target = {
            "activityName": "Treatment, municipal solid waste, sanitary landfill",
            "location": "CH",
            "processId": "wwww1111-aaaa-bbbb-cccc-111122223333_qqqq2222-eeee-ffff-aaaa-444455556666",
        }
        ex = parse_exchange_detail(_waste_ed(is_input=True, target=target))
        assert isinstance(ex, WasteExchange)
        assert ex.is_input is True
        assert ex.target_activity_name == target["activityName"]
        assert ex.target_location == "CH"

    def test_mismatched_flow_kind_rejected(self):
        """If the engine ever ships a WasteExchange paired with a non-waste
        flow envelope, surface it loudly instead of silently dropping the
        type information."""
        bad = _waste_ed(is_input=False)
        bad["flow"]["kind"] = "biosphere"
        with pytest.raises(ValueError, match="WasteExchange carried flow kind 'biosphere'"):
            parse_exchange_detail(bad)

    def test_waste_flow_envelope_rejected_on_technosphere_tag(self):
        """A waste-kind flow paired with a TechnosphereExchange tag is a
        wire-format bug: refuse it rather than parsing as a product input."""
        bad = {
            "exchange": {"tag": "TechnosphereExchange", "amount": 1.0, "role": "Input"},
            "exchangeUnitName": "kg",
            "flow": {"kind": "waste", "flow": {"name": "wheat"}},
        }
        with pytest.raises(ValueError, match="TechnosphereExchange carried flow kind 'waste'"):
            parse_exchange_detail(bad)

    def test_waste_flow_envelope_rejected_on_biosphere_tag(self):
        bad = {
            "exchange": {"tag": "BiosphereExchange", "amount": 1.0, "direction": "Emission", "flowId": "11111111-2222-3333-4444-555555555555"},
            "exchangeUnitName": "kg",
            "flow": {"kind": "waste", "flow": {"name": "CO2"}},
        }
        with pytest.raises(ValueError, match="BiosphereExchange carried flow kind 'waste'"):
            parse_exchange_detail(bad)
