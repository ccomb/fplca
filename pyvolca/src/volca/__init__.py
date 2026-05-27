"""VoLCA Python client — Life Cycle Assessment engine.

See https://volca.run/docs/guides/pyvolca/ for the full guide.
"""

from ._download import DownloadError, Installed, download
from .client import Client, VoLCAError
from .compare import ActivityDiff, ActivityDiffRow, compare_activities
from .server import Server
from .types import (
    Activity,
    ActivityDetail,
    AggregateGroup,
    AggregateResult,
    BiosphereExchange,
    ClassificationFilter,
    Compartment,
    ConsumerResult,
    ConsumersResponse,
    DatabaseInfo,
    Exchange,
    FlowContribution,
    LCIABatchResult,
    LCIAResult,
    MatchMode,
    MatchModeLike,
    PathResult,
    PathStep,
    ScoringIndicator,
    SupplyChain,
    SupplyChainEdge,
    SupplyChainEntry,
    TechRole,
    TechnosphereExchange,
    WasteExchange,
)

__all__ = [
    "Activity",
    "ActivityDetail",
    "ActivityDiff",
    "ActivityDiffRow",
    "AggregateGroup",
    "AggregateResult",
    "BiosphereExchange",
    "Client",
    "ClassificationFilter",
    "Compartment",
    "ConsumerResult",
    "ConsumersResponse",
    "DatabaseInfo",
    "DownloadError",
    "Exchange",
    "FlowContribution",
    "Installed",
    "LCIABatchResult",
    "LCIAResult",
    "MatchMode",
    "MatchModeLike",
    "PathResult",
    "PathStep",
    "ScoringIndicator",
    "Server",
    "SupplyChain",
    "SupplyChainEdge",
    "SupplyChainEntry",
    "TechRole",
    "TechnosphereExchange",
    "VoLCAError",
    "WasteExchange",
    "compare_activities",
    "download",
]
