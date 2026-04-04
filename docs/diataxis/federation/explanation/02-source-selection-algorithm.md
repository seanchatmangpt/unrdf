# Explanation: Source Selection Algorithm

This document explains how `@unrdf/federation` decides which stores receive any given query.
There are two separate source-selection layers: the simple coordinator's routing strategies
and the v6 `FederationCoordinator`'s load balancing. They serve different use cases and are
not interchangeable.

---

## Layer 1: Coordinator routing strategies

The simple coordinator (`createCoordinator`) uses `routeQuery()` to map a list of healthy
peers to a list of target peers. The mapping is determined by the `strategy` field.

### Why three strategies?

The strategies represent three different operational assumptions:

**`broadcast` (default)** — You do not know which store holds the relevant data, or you want
a union over all stores. Every healthy peer receives the query. This is the safe default for
general-purpose federation where stores hold complementary, potentially overlapping data.

**`first-available`** — Stores are homogeneous replicas. You only need one answer and you want
the fastest possible response. The query goes to whichever peer is first in insertion order;
the rest receive nothing. This strategy is unsuitable for heterogeneous stores where different
peers hold different data.

**`selective`** — Reserved for future use. Currently identical to `broadcast`. The intent is
that metadata attached to peers at registration time (via the `metadata` argument of
`addPeer`) would be used to filter peers based on query content. For example, a peer tagged
`{ dataset: 'persons' }` would only receive queries that reference person-related predicates.
This filtering is not implemented yet; if you need it, implement it yourself by calling
`routeQuery()` with a custom pre-filtered peer list or use `executeDistributedQuery()`
directly.

### The health filter precedes routing

`coordinator.query()` filters the peer list to `{ status: 'healthy' }` peers before passing
it to `routeQuery()`. Routing strategies operate on the already-filtered list. If all peers
are `'degraded'` or `'unreachable'`, routing receives an empty list and the query returns
`{ success: false, error: 'No healthy peers available' }`.

---

## Layer 2: FederationCoordinator load balancing (v6)

The v6 `FederationCoordinator` addresses a different problem. Where the simple coordinator
treats all healthy peers equally (modulo insertion order for `first-available`), the v6
coordinator assigns weights to stores and uses load balancing to distribute individual
queries across them.

### Load balancing strategies

**`weighted` (default)** — Each store is assigned a `weight` between 0 and 1 at registration
time. The coordinator performs weighted random selection: a random value in `[0, totalWeight]`
is drawn, and stores are iterated until the cumulative weight exceeds the random value. Stores
with higher weight are selected more often. This is useful when stores have different hardware
capacities.

**`round-robin`** — A counter increments on every `selectStore()` call, and the store at
`counter % stores.length` is returned. This distributes queries evenly regardless of store
capacity.

**`least-loaded`** — Currently delegates to `selectWeighted`. In a production implementation
this would track active query counts or recent response latencies per store and favour stores
with available capacity.

### Store metadata vs peer metadata

The v6 `StoreMetadataSchema` is richer than the simple coordinator's peer object:

| Field          | Simple coordinator `peers[]`        | v6 `StoreMetadataSchema`            |
| -------------- | ----------------------------------- | ----------------------------------- |
| Identifier     | `id: string`                        | `storeId: string`                   |
| URL            | `endpoint: string (URL)`            | `endpoint: string (URL)`            |
| Arbitrary tags | `metadata: Record<string, unknown>` | `metadata: Record<string, unknown>` |
| Display name   | —                                   | `name: string (optional)`           |
| Capabilities   | —                                   | `capabilities: string[]`            |
| Priority       | —                                   | `priority: 0–100`                   |
| Weight         | —                                   | `weight: 0–1`                       |

`capabilities` is a free-form string array (e.g. `['sparql-1.1', 'update', 'geosparql']`).
The coordinator does not interpret it; it is available for your own routing logic.

---

## RAFT and source membership

The v6 `FederationCoordinator` integrates `ConsensusManager` (a RAFT implementation) so that
store registration and deregistration decisions are replicated across all coordinator nodes.
This prevents split-brain scenarios where two coordinator nodes disagree about which stores
are members of the federation.

The RAFT log contains `REGISTER_STORE` and `DEREGISTER_STORE` commands. When a command is
committed (acknowledged by a majority of RAFT peers), it is applied to each node's local
store map via `handleConsensusCommand()`. This means:

- Store membership is consistent across coordinator nodes.
- A store registration is not effective until it has been committed by a majority.
- If the RAFT leader fails, a new leader is elected before store registration can proceed.

This is relevant to source selection because the set of stores that `getHealthyStores()` can
return is ultimately determined by the RAFT-committed state, not just the local in-memory map.

---

## Health tracking in v6

The v6 coordinator runs a `setInterval` health check at `healthCheckInterval` (default 5 000
ms, compared to 60 000 ms in the simple coordinator). The current implementation simulates a
90 % healthy rate using `Math.random()`. A real implementation would make HTTP requests to a
health endpoint and observe the response.

The health state is stored per store in `storeHealth: Map<storeId, StoreHealth>`. The enum
values are `'healthy'`, `'degraded'`, `'unhealthy'`, and `'unknown'`. Note that the v6
coordinator uses `'unhealthy'` where the simple coordinator uses `'unreachable'`.

`storeHealthChanged` events are emitted when a store's health status transitions, allowing
external systems (dashboards, alerting) to react to changes without polling.

---

## Choosing between the two layers

| Concern                                      | Use simple coordinator | Use v6 FederationCoordinator              |
| -------------------------------------------- | ---------------------- | ----------------------------------------- |
| Fan out SPARQL to HTTP endpoints             | Yes                    | Not designed for this                     |
| Stores are transient / registered at runtime | Yes                    | Yes                                       |
| Stores are long-lived services               | Yes                    | Yes (with richer metadata)                |
| Weighted load distribution                   | No                     | Yes (`loadBalancingStrategy: 'weighted'`) |
| Store membership consistency across nodes    | No                     | Yes (via RAFT consensus)                  |
| Data replication across stores               | No                     | Yes (via `DataReplicationManager`)        |
| Simple setup, minimal config                 | Yes                    | More complex                              |

In most applications `createCoordinator()` is sufficient. The v6 layer is appropriate when
you are building a multi-node UNRDF deployment where coordinator instances themselves need to
agree on federation membership.
