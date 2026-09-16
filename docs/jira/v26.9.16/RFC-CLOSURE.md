# unrdf v26.9.16 — RFC Closure Contract

Status: DRAFT IMPLEMENTATION PR.

## Canonical Jira tickets

- A2A-2607 — Blue River Dam cross-repo closure
- A2A-2610 — AtomVM enterprise idle-estate controller
- A2A-2612 — machine-experience compile-back (edge candidate/evidence side)

## RFC ownership

This repo owns the edge/distributed execution substrate already containing `@unrdf/atomvm`, Knowledge Hooks, RDF/streaming integration and WebAssembly-hosted BEAM machinery.

## Required closure

1. Turn the existing AtomVM runtime into an admitted ephemeral compute-cell profile.
2. Define a node envelope covering CPU, memory, storage, availability window, thermal/power policy, network policy and consequence ceiling.
3. Implement lifecycle `PRIMARY_ROLE -> AVAILABLE -> COMPUTE_CELL -> DRAINING -> PRIMARY_ROLE`.
4. Accept only bounded jobs with exact semantic subject/runtime/resource identities.
5. Return `CandidateResult + Receipt + Evidence`; a cell must never assert canonical truth or acquire implicit DO authority.
6. Add deferred/overnight scheduling hooks while keeping actual time windows policy-defined rather than hard-coded.
7. Make interruption, timeout, worker disappearance and morning drain leave canonical semantic state unchanged unless a separately receipted consequence exists.
8. Expose candidate/receipt output suitable for SA2A admission and machine-experience compile-back.

## Existing machinery to reuse

- `@unrdf/atomvm`
- AtomVM/WASM runtime state machine and execution timeout
- Knowledge Hooks AtomVM/Erlang bridge
- Oxigraph/streaming packages
- existing observability/receipt hooks

## Chicago falsifiers

- an unadmitted device becomes a compute cell;
- a job exceeds its declared resource/time envelope;
- morning drain leaves orphan work capable of later consequence;
- worker output writes canonical state directly;
- worker loss corrupts canonical state;
- retry performs an externally consequential action twice;
- an idle device is assumed available merely because CPU utilization is low.

## Definition of done

A repository-native exact-head court demonstrates at least one real AtomVM cell entering/exiting the compute role, bounded job execution, forced timeout/loss, drain/restore, candidate+receipt return, and fail-closed behavior. Cross-repo qualification then consumes this exact subject from SA2A rather than reimplementing the cell runtime elsewhere.
