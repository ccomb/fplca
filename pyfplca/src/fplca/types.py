"""Data types for fpLCA API responses."""

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
class SupplyChainPathNode:
    process_id: str
    name: str

    @classmethod
    def from_json(cls, d: dict) -> "SupplyChainPathNode":
        return cls(process_id=d["scpProcessId"], name=d["scpName"])


@dataclass
class SupplyChainEntry:
    process_id: str
    name: str
    location: str
    quantity: float
    unit: str
    scaling_factor: float
    path: list[SupplyChainPathNode] = field(default_factory=list)

    @classmethod
    def from_json(cls, d: dict) -> "SupplyChainEntry":
        return cls(
            process_id=d["sceProcessId"],
            name=d["sceName"],
            location=d["sceLocation"],
            quantity=d["sceQuantity"],
            unit=d["sceUnit"],
            scaling_factor=d["sceScalingFactor"],
            path=[SupplyChainPathNode.from_json(p) for p in d.get("scePath", [])],
        )


@dataclass
class SupplyChain:
    root: Activity
    total_activities: int
    filtered_activities: int
    entries: list[SupplyChainEntry] = field(default_factory=list)

    @classmethod
    def from_json(cls, d: dict) -> "SupplyChain":
        return cls(
            root=Activity.from_json(d["scrRoot"]),
            total_activities=d["scrTotalActivities"],
            filtered_activities=d["scrFilteredActivities"],
            entries=[SupplyChainEntry.from_json(e) for e in d["scrSupplyChain"]],
        )
