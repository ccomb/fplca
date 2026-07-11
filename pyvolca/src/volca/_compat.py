"""Engine wire-compatibility policy — the single source of truth.

pyvolca speaks one revision of the JSON wire format; the engine advertises its
own revision as ``wireVersion`` on ``/api/v1/version``. This module owns the
comparison: the wire this client requires, the engine hint shown when the check
fails, and the check itself.

It is deliberately import-cheap — only the stdlib at runtime — so the release
preflight can read :data:`MIN_ENGINE_HINT` without pulling in ``requests`` or
the client, and so importing it can never cycle back through the client.
"""

from __future__ import annotations

import os
import warnings
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from .types import ServerVersion

REQUIRED_WIRE = 2
"""The JSON wire-format revision this pyvolca speaks."""

MIN_ENGINE_HINT = "0.9.1"
"""First engine release that advertises :data:`REQUIRED_WIRE`. Used only for the
error message and the release preflight — it is not, by itself, a runtime gate
(the engine's ``wireVersion`` is)."""


def check(sv: ServerVersion) -> None:
    """Raise :class:`VoLCAError` if the engine's wire is too old; warn if newer.

    The opt-out is read here, not in the caller, so every entry point — the
    client's first operation and :meth:`volca.server.Server.start` alike —
    honours it. It is deliberately noisy: silencing the check is a foot-gun.
    """
    if os.environ.get("VOLCA_SKIP_COMPAT_CHECK"):
        warnings.warn(
            "VOLCA_SKIP_COMPAT_CHECK set — skipping the engine "
            "wire-compatibility check."
        )
        return

    w = sv.wire_version
    if w is None or w < REQUIRED_WIRE:
        from .client import VoLCAError  # deferred: keep this module client-free

        spoken = "pre-1" if w is None else str(w)
        raise VoLCAError(
            f"This engine (v{sv.version}) speaks an older wire than pyvolca "
            f"needs (wire {spoken} < {REQUIRED_WIRE}). Upgrade the engine to "
            f">= v{MIN_ENGINE_HINT}, or `pip install 'pyvolca<0.7.2'`."
        )
    if w > REQUIRED_WIRE:
        warnings.warn(
            f"Engine v{sv.version} speaks wire {w}; this pyvolca knows up to "
            f"wire {REQUIRED_WIRE}. Some responses may not decode — upgrade "
            "pyvolca."
        )
