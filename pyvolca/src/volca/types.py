"""Data types for VoLCA API responses."""

from dataclasses import dataclass, field


@dataclass
class Activity:
    process_id: str
    name: str
    location: str
    product: str
    product_amount: float
    product_unit: str

    @classmethod
    def from_json(cls, d: dict) -> "Activity":
        return cls(
            process_id=d["prsId"],
            name=d["prsName"],
            location=d["prsLocation"],
            product=d["prsProduct"],
            product_amount=d["prsProductAmount"],
            product_unit=d["prsProductUnit"],
        )


@dataclass
class ConsumerResult:
    """Activity that consumes a given supplier, with BFS depth."""
    process_id: str
    name: str
    location: str
    product: str
    product_amount: float
    product_unit: str
    depth: int  # hops from the queried supplier (1 = direct consumer)

    @classmethod
    def from_json(cls, d: dict) -> "ConsumerResult":
        return cls(
            process_id=d["crId"],
            name=d["crName"],
            location=d["crLocation"],
            product=d["crProduct"],
            product_amount=d["crProductAmount"],
            product_unit=d["crProductUnit"],
            depth=d["crDepth"],
        )


@dataclass
class SupplyChainEntry:
    process_id: str
    name: str
    location: str
    quantity: float
    unit: str
    scaling_factor: float
    classifications: dict[str, str] = field(default_factory=dict)

    @classmethod
    def from_json(cls, d: dict) -> "SupplyChainEntry":
        return cls(
            process_id=d["sceProcessId"],
            name=d["sceName"],
            location=d["sceLocation"],
            quantity=d["sceQuantity"],
            unit=d["sceUnit"],
            scaling_factor=d["sceScalingFactor"],
            classifications=d.get("sceClassifications", {}),
        )


@dataclass
class PathStep:
    """One step in the supply chain path returned by get_path_to."""
    process_id: str
    name: str
    location: str
    unit: str
    cumulative_quantity: float
    scaling_factor: float
    local_step_ratio: float | None = None  # absent on root step

    @classmethod
    def from_json(cls, d: dict) -> "PathStep":
        return cls(
            process_id=d["process_id"],
            name=d["name"],
            location=d["location"],
            unit=d["unit"],
            cumulative_quantity=d["cumulative_quantity"],
            scaling_factor=d["scaling_factor"],
            local_step_ratio=d.get("local_step_ratio"),
        )


@dataclass
class PathResult:
    """Shortest upstream path from a root process to a matching activity."""
    path: list[PathStep]
    path_length: int
    total_ratio: float

    @classmethod
    def from_json(cls, d: dict) -> "PathResult":
        return cls(
            path=[PathStep.from_json(s) for s in d["path"]],
            path_length=d["path_length"],
            total_ratio=d["total_ratio"],
        )


@dataclass
class SupplyChainEdge:
    from_id: str
    to_id: str
    amount: float

    @classmethod
    def from_json(cls, d: dict) -> "SupplyChainEdge":
        return cls(
            from_id=d["sceEdgeFrom"],
            to_id=d["sceEdgeTo"],
            amount=d["sceEdgeAmount"],
        )


@dataclass
class SupplyChain:
    root: Activity
    total_activities: int
    filtered_activities: int
    entries: list[SupplyChainEntry] = field(default_factory=list)
    edges: list[SupplyChainEdge] = field(default_factory=list)

    @classmethod
    def from_json(cls, d: dict) -> "SupplyChain":
        return cls(
            root=Activity.from_json(d["scrRoot"]),
            total_activities=d["scrTotalActivities"],
            filtered_activities=d["scrFilteredActivities"],
            entries=[SupplyChainEntry.from_json(e) for e in d["scrSupplyChain"]],
            edges=[SupplyChainEdge.from_json(e) for e in d.get("scrEdges", [])],
        )


