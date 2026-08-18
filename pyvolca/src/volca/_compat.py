"""Engine wire-compatibility policy: the single source of truth.

pyvolca speaks a range of revisions of the JSON wire format; the engine
advertises its own revision as ``wireVersion`` on ``/api/v1/version``. This
module owns the comparison: the oldest wire this client accepts, the newest it
understands, the engine hint shown when the check fails, and the check itself.

It is deliberately import-cheap (only the stdlib at runtime) so the release
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
"""The oldest JSON wire-format revision this pyvolca accepts. Everything in
the client works against it except the revision-gated capabilities (see
``Client._require_wire``), which check the engine's advertised revision
before sending anything."""

KNOWN_WIRE = 8
"""The newest wire revision this pyvolca understands (revision 8 adds the two
quality reports as downloadable CSV; revision 7 added editing an activity's
exchanges; revision 6 added explain_cf and the match_kind field on flow
contributions; revision 5 added writing activities; revision 4 added the
quality-report routes). An engine advertising more is newer than this client
and may answer shapes it cannot decode."""

MIN_ENGINE_HINT = "0.9.1"
"""First engine release that advertises :data:`REQUIRED_WIRE`. Used only for the
error message and the release preflight; it is not, by itself, a runtime gate
(the engine's ``wireVersion`` is)."""


def check(sv: ServerVersion) -> None:
    """Raise :class:`VoLCAError` if the engine's wire is too old; warn if newer.

    The opt-out is read here, not in the caller, so every entry point,
    the client's first operation and :meth:`volca.server.Server.start` alike,
    honours it. It is deliberately noisy: silencing the check is a foot-gun.
    """
    if os.environ.get("VOLCA_SKIP_COMPAT_CHECK"):
        warnings.warn(
            "VOLCA_SKIP_COMPAT_CHECK set, skipping the engine "
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
    if w > KNOWN_WIRE:
        warnings.warn(
            f"Engine v{sv.version} speaks wire {w}; this pyvolca knows up to "
            f"wire {KNOWN_WIRE}. Some responses may not decode; upgrade "
            "pyvolca."
        )
