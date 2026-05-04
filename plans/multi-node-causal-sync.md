# Phase 3 Plan: Multi-node Causal Sync & Transport

## 1. Integration of Hypercore/Hyperswarm

To achieve decentralized, peer-to-peer synchronization of the Knowledge Graph (KGC) state, we will integrate the Hypercore protocol suite into `packages/kgc-swarm` and the `sidecar`.

### Components
- **Hyperswarm**: Used for peer discovery and NAT-traversing connections. Nodes will join a swarm topic derived from the KGC's unique namespace (e.g., `sha256(kgc-namespace)`).
- **Hypercore**: Each node will maintain its own append-only log of `KGCStore` events.
- **Corestore**: Manages multiple Hypercores (the local node's core + remote cores from peers).
- **Autobase (Optional/Future)**: For multi-writer causal ordering and linearization of multiple cores into a single view.

### Implementation Steps
1.  **Transport Layer**: Implement a `HyperTransport` class in `packages/kgc-swarm/src/transport/hyper.mjs` that wraps `Hyperswarm` and `Corestore`.
2.  **Sync Protocol**: Define a gossip-based protocol over Hypercore extension messages to exchange "Heads" (latest event IDs and VectorClock states).
3.  **KGCStore Integration**: Update `KGCStore.appendEvent()` to automatically push the generated event to the local Hypercore.

## 2. Semantic Partitioning ($O^*$) Logic

Semantic Partitioning allows nodes to subscribe to a subset of the global Knowledge Graph while maintaining "Semantic Closure" ($O^*$) within that subset.

### Definition of $O^*$ Partition
A partition is defined by a **Semantic Filter** ($F$):
- $F := \{ P_1, P_2, ... P_n \}$ where each $P$ is a SPARQL triple pattern or a SHACL shape.
- An event $E$ is relevant to partition $O^*_F$ if $E.deltas \cap F \neq \emptyset$.

### Filtering Logic
1.  **Subscription**: Each node broadcasts its $F$ to peers via Hyperswarm.
2.  **Gossip Filtering**: When a node receives an update notification, it only requests the full event data if the update matches its local filter $F$ or if it is a causal dependency of a matched update.
3.  **Causal Closure**: To maintain semantic closure, the filtering logic must recursively include triples that are "connected" to the matched triples (e.g., using Concise Bounded Descriptions).

## 3. Sidecar Refactoring

The sidecar will be updated to use the formal `KGCStore` as its primary state engine.

### Changes
1.  **Storage Engine**: Replace the ad-hoc `transactionManager` in `sidecar/server/utils/managers.mjs` with an instance of `KGCStore`.
2.  **Receipt Chain**: Leverage `KGCStore`'s built-in cryptographic receipt chain for the transaction log dashboard.
3.  **P2P Sync**: Add a "Sync Status" view to the sidecar dashboard, showing connected peers, Hypercore replication progress, and partition health.

---

## Adversarial Review

### Critique 1: Network Partitions in SWIM-based Gossip
**Question**: "How do we handle network partitions in a SWIM-based gossip protocol?"
**Answer**: While SWIM detects failures, it doesn't solve the state sync problem during long-term partitions. By integrating Hypercore, we move from "fire-and-forget" gossip to "durable log" replication. When a partition heals, Hypercore's synchronization mechanism automatically fetches missing blocks from the logs of reconnected peers. The `VectorClock` in `KGCStore` ensures that causal ordering is maintained even if updates are received out of order after a partition heals.

### Critique 2: Semantic Partitioning and Semantic Closure
**Question**: "Does semantic partitioning compromise the 'Semantic Closure' if nodes miss relevant events?"
**Answer**: Yes, if the filter $F$ is too narrow. A naive filter might capture a triple but miss its class definition or related entities. 
**Mitigation**: We implement **Closure Expansion**. The filter $F$ is not just a static pattern; it is an expansion rule. When an event is matched, the node also requests the "Semantic Context" of the change—ensuring that all referenced URIs that are not already in the local $O^*$ are also fetched. If a node detects a missing link in its receipt chain (via the `event_count` or `VectorClock`), it triggers a "Sync Repair" operation to fetch the gap.

## Deliverables
- `packages/kgc-swarm/src/transport/hyper.mjs`
- `sidecar/server/plugins/kgc-store.mjs` (Initializes `KGCStore`)
- Updated `sidecar/server/api/transaction/apply.post.mjs` using `KGCStore.appendEvent()`
- `plans/multi-node-causal-sync.md` (This document)
