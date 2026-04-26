"""Per-exchange free-text comment parsing.

The engine surfaces the source-format ``generalComment`` / ``<comment>`` /
SimaPro free-text on every exchange. It appears in two wire shapes:

* ``ExchangeWithUnit`` — flat ``exComment`` next to the inner ``exchange``.
* ``ExchangeDetail``   — nested ``exchange.comment`` only.

pyvolca surfaces both as ``Exchange.comment`` so callers don't have to know
which envelope they got.
"""

from __future__ import annotations

from volca.types import (
    BiosphereExchange,
    TechnosphereExchange,
    parse_exchange,
    parse_exchange_detail,
)


def _ewu(tag: str, *, ex_comment: str | None, inner_comment: str | None) -> dict:
    """Build a minimal ExchangeWithUnit payload."""
    inner: dict = {
        "tag": tag,
        "amount": 1.0,
        "isInput": True,
    }
    if tag == "TechnosphereExchange":
        inner["isReference"] = False
    if inner_comment is not None:
        inner["comment"] = inner_comment
    out: dict = {
        "exchange": inner,
        "flowName": "wheat",
        "flowCategory": "technosphere" if tag == "TechnosphereExchange" else "air",
        "unitName": "kg",
        "targetActivity": None,
        "targetLocation": None,
        "targetProcessId": None,
    }
    if ex_comment is not None:
        out["exComment"] = ex_comment
    return out


def _ed(tag: str, *, inner_comment: str | None, ex_comment: str | None = None) -> dict:
    """Build a minimal ExchangeDetail payload."""
    inner: dict = {
        "tag": tag,
        "amount": 1.0,
        "isInput": True,
    }
    if tag == "TechnosphereExchange":
        inner["isReference"] = False
    if inner_comment is not None:
        inner["comment"] = inner_comment
    out: dict = {
        "exchange": inner,
        "flow": {"name": "wheat", "category": "technosphere"},
        "exchangeUnitName": "kg",
    }
    if ex_comment is not None:
        out["exComment"] = ex_comment
    return out


class TestExchangeWithUnitComment:
    def test_flat_excomment_is_picked_up_on_techno(self):
        ex = parse_exchange(_ewu("TechnosphereExchange", ex_comment="from EI", inner_comment=None))
        assert isinstance(ex, TechnosphereExchange)
        assert ex.comment == "from EI"

    def test_flat_excomment_is_picked_up_on_bio(self):
        ex = parse_exchange(_ewu("BiosphereExchange", ex_comment="measured", inner_comment=None))
        assert isinstance(ex, BiosphereExchange)
        assert ex.comment == "measured"

    def test_falls_back_to_inner_comment_when_flat_absent(self):
        ex = parse_exchange(_ewu("TechnosphereExchange", ex_comment=None, inner_comment="legacy"))
        assert ex.comment == "legacy"

    def test_flat_wins_when_both_present(self):
        ex = parse_exchange(_ewu("TechnosphereExchange", ex_comment="canonical", inner_comment="other"))
        assert ex.comment == "canonical"

    def test_none_when_neither_present(self):
        ex = parse_exchange(_ewu("TechnosphereExchange", ex_comment=None, inner_comment=None))
        assert ex.comment is None


class TestExchangeDetailComment:
    def test_inner_comment_is_picked_up_on_techno(self):
        ex = parse_exchange_detail(_ed("TechnosphereExchange", inner_comment="from EI"))
        assert isinstance(ex, TechnosphereExchange)
        assert ex.comment == "from EI"

    def test_inner_comment_is_picked_up_on_bio(self):
        ex = parse_exchange_detail(_ed("BiosphereExchange", inner_comment="measured"))
        assert isinstance(ex, BiosphereExchange)
        assert ex.comment == "measured"

    def test_none_when_inner_absent(self):
        ex = parse_exchange_detail(_ed("TechnosphereExchange", inner_comment=None))
        assert ex.comment is None

    def test_flat_excomment_wins_if_engine_ever_adds_it(self):
        """ExchangeDetail doesn't expose exComment today, but if the backend
        gains it for symmetry, pyvolca already prefers it over the nested
        copy — same precedence as ExchangeWithUnit."""
        payload = _ed("TechnosphereExchange", inner_comment="other", ex_comment="canonical")
        ex = parse_exchange_detail(payload)
        assert ex.comment == "canonical"
