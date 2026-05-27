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
    Flow,
    FlowContribution,
    LCIABatchResult,
    LCIAResult,
    PathResult,
    PathStep,
    ScoringIndicator,
    SearchResults,
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
    "Flow",
    "FlowContribution",
    "Installed",
    "LCIABatchResult",
    "LCIAResult",
    "PathResult",
    "PathStep",
    "ScoringIndicator",
    "SearchResults",
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
