"""Offline tests for sensitivity analysis and batch scoring.

Both go through the OpenAPI dispatcher (``_call``) with a POST body, so they
rely on the fixture_spec entries for ``compute_sensitivity`` and
``score_activities``. The tests assert the dispatched URL/method/params/body
and the typed parsing of the responses.
"""

from __future__ import annotations

import pytest

from volca.types import BatchScores, SensitivityResult

# A method id travels the URL as a UUID; anything else is a name the client
# resolves against the engine first (see TestMethodResolution).
_METHOD_ID = "00000000-0000-0000-0000-000000000001"


def _lcia(score: float) -> dict:
    return {
        "methodId": "00000000-0000-0000-0000-000000000001",
        "methodName": "Climate change",
        "category": "Climate change",
        "damageCategory": "Climate change",
        "score": score,
        "unit": "kg CO2 eq",
        "mappedFlows": 3,
        "functionalUnit": "1.0 kg",
        "topContributors": [],
    }


def _batch(score: float) -> dict:
    return {"results": [_lcia(score)]}


class TestComputeSensitivity:
    def test_dispatches_post_with_body(self, mocked_client, make_response):
        client, session = mocked_client
        session.post.return_value = make_response(
            {
                "baseline": _lcia(1.0),
                "perturbed": [
                    {"perturbation": {"consumer": "c", "supplier": "s", "delta": -0.05, "label": "cut"}, "impact": _lcia(0.95), "deltaImpact": -0.05},
                    {"perturbation": {"consumer": "c", "supplier": "z", "delta": 0.1}, "error": "supplier not found"},
                ],
            }
        )
        result = client.compute_sensitivity(
            "proc_a",
            _METHOD_ID,
            [{"consumer": "c", "supplier": "s", "delta": -0.05, "label": "cut"}],
            collection="methods",
        )
        session.post.assert_called_once()
        session.get.assert_not_called()
        url = session.post.call_args[0][0]
        assert url == f"http://test.local/api/v1/db/testdb/activity/proc_a/sensitivity/methods/{_METHOD_ID}"
        assert session.post.call_args[1]["json"] == {
            "perturbations": [{"consumer": "c", "supplier": "s", "delta": -0.05, "label": "cut"}]
        }
        assert isinstance(result, SensitivityResult)
        assert result.baseline.score == 1.0
        ok, bad = result.perturbed
        assert ok.impact is not None and ok.delta_impact == -0.05 and ok.error is None
        assert bad.impact is None and bad.error == "supplier not found"


class TestScoreActivities:
    def test_dispatches_post_with_ids_and_query(self, mocked_client, make_response):
        client, session = mocked_client
        session.post.return_value = make_response(
            {
                "results": [
                    {"processId": "p1", "activityName": "Wheat", "impacts": _batch(1.0)},
                    {"processId": "p2", "activityName": "Maize", "impacts": _batch(2.0)},
                ],
                "notFound": ["p3"],
                "invalid": ["bogus"],
            }
        )
        result = client.score_activities(
            ["p1", "p2", "p3"], collection="methods", top_flows=5, exclude_long_term=True
        )
        session.post.assert_called_once()
        session.get.assert_not_called()
        assert session.post.call_args[0][0] == "http://test.local/api/v1/db/testdb/impacts/methods"
        params = dict(session.post.call_args[1]["params"])
        assert params["top-flows"] == "5"
        assert params["exclude-long-term"] == "true"
        assert session.post.call_args[1]["json"] == {"processIds": ["p1", "p2", "p3"]}
        assert isinstance(result, BatchScores)
        assert [s.process_id for s in result.results] == ["p1", "p2"]
        assert result.results[1].impacts.results[0].score == 2.0
        assert result.not_found == ["p3"]
        assert result.invalid == ["bogus"]

    def test_optional_query_params_omitted_by_default(self, mocked_client, make_response):
        client, session = mocked_client
        session.post.return_value = make_response({"results": [], "notFound": [], "invalid": []})
        client.score_activities(["p1"], collection="methods")
        params = dict(session.post.call_args[1]["params"])
        assert "top-flows" not in params
        assert "exclude-long-term" not in params
