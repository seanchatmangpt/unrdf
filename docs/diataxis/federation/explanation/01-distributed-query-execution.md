# Explanation: Distributed Query Execution

This document explains what happens inside `coordinator.query()` from the moment you call it
to the moment you receive aggregated results. Understanding this flow helps you reason about
result completeness, deduplication behaviour, and partial failure handling.

---

## The execution pipeline

When you call `coordinator.query(sparql)`, the coordinator runs these steps in order:

```
1. Health filter   — exclude peers whose status is not 'healthy'
2. Route query     — select a subset of healthy peers based on strategy
3. Fan out         — issue HTTP POST to each selected peer (parallel or sequential)
4. Aggregate       — combine results from all peers that responded
5. Deduplicate     — remove duplicate bindings
6. Return envelope — success flag, combined results, per-peer results, timing
```

Steps 3–5 are handled by `executeDistributedQuery()` and `aggregateResults()`, which are also
exported and can be called directly if you need to bypass the coordinator.

---

## Step 1: Health filter

The coordinator maintains a status for each peer: `'healthy'`, `'degraded'`, or
`'unreachable'`. Only `'healthy'` peers are included in step 2.

Status transitions happen through two mechanisms:

- **Proactive pings** — `healthCheck()` issues a HEAD request to each peer endpoint. A 2xx
  response sets the peer to `'healthy'`; a non-2xx sets it to `'degraded'`; a network error
  sets it to `'unreachable'`.
- **Reactive updates** — `queryPeer()` observes the HTTP response from each query. If a peer
  returns a non-2xx response it is set to `'degraded'`. If the request fails with a connection
  or timeout error it is set to `'unreachable'`.

A peer that starts as `'unreachable'` will never receive queries until a successful health
check restores it to `'healthy'`. This is intentional: the coordinator should not waste query
budget on peers that are known to be down.

---

## Step 2: Route query

`routeQuery()` maps the filtered healthy peer list through one of three strategies:

- **`broadcast`** — returns all healthy peers. This is the default and ensures union coverage:
  every peer that holds relevant data will be queried.
- **`first-available`** — returns only the first healthy peer (insertion order). Use this when
  stores are replicas and you only need one answer.
- **`selective`** — currently identical to `broadcast`. It is reserved for future metadata-based
  filtering (e.g. routing to stores tagged with a specific dataset).

The strategy operates on the already-filtered healthy peer list. You cannot use it to include
unhealthy peers.

---

## Step 3: Fan out

`executeDistributedQuery()` sends the SPARQL query to each selected peer via HTTP POST with
`Content-Type: application/sparql-query`. The default execution mode is parallel:

```javascript
peerResults = await Promise.all(
  peers.map(peer => executeFederatedQuery(peer.id, peer.endpoint, sparqlQuery, options))
);
```

Each call to `executeFederatedQuery()` uses `AbortController` and `setTimeout` to enforce the
timeout. If a peer does not respond within the timeout budget, `AbortController.abort()` fires
and the peer result is marked as failed with an `'AbortError'` message.

The `executionStrategy: 'sequential'` option is available for cases where you cannot issue
parallel requests (e.g. rate-limited endpoints or ordered pipeline requirements).

---

## Step 4: Aggregate

`aggregateResults()` receives the array of per-peer `QueryResult` objects. It processes only
the ones where `success === true` and extracts bindings:

- If `result.data.results.bindings` exists (SPARQL 1.1 JSON format), it iterates the bindings.
- If `result.data` is an array, it iterates the items.
- Otherwise it treats `result.data` as a single item.

All extracted items are pushed into a combined array.

---

## Step 5: Deduplicate

Deduplication is by `JSON.stringify`. Each item is serialised; if the serialisation has been
seen before, the item is discarded.

This approach is:

- **Simple and allocation-efficient** — no deep-equal traversal, no secondary index.
- **Order-sensitive** — `{ a: 1, b: 2 }` and `{ b: 2, a: 1 }` are treated as different
  bindings even though they are semantically equivalent. This is a known limitation.
- **Reference-transparent** — the same binding returned by two different peers is correctly
  deduplicated as long as the JSON serialisation is identical.

For most SPARQL JSON responses from a single engine (e.g. Fuseki, Oxigraph) the key ordering
is deterministic, so deduplication works as expected in practice.

---

## Step 6: Return envelope

The return value of `coordinator.query()` always has this structure:

```javascript
{
  success:       boolean,    // true if at least one peer succeeded
  results:       any[],      // deduplicated bindings
  peerResults:   QueryResult[],
  totalDuration: number,     // wall-clock ms (parallel: slowest peer; sequential: sum)
  successCount:  number,
  failureCount:  number,
  error?:        string,     // only when success === false
}
```

`success` is `true` if even one peer returned a result. This is a union-over-partial-failure
model: the federation provides best-effort coverage, and the caller decides what to do with
partial results.

---

## The advanced Comunica path

`createAdvancedFederationEngine()` uses a different pipeline. Instead of manually fanning out
and aggregating, it passes the list of sources to Comunica's `queryBindings()`, which handles
federation internally using the SPARQL 1.1 `SERVICE` mechanism. This means:

- Joins across stores are evaluated inside the query engine with proper variable binding
  propagation, not as post-hoc result merging.
- Filters can be pushed down to individual sources (when `optimization: 'aggressive'` is set,
  Comunica runs in lenient mode).
- The result stream is an async iterator over binding objects rather than a resolved array.

The trade-off is that Comunica's federation adds startup overhead (source discovery) and
produces a different result shape (`{ bindings, metadata }` vs the coordinator envelope).
Use Comunica federation when cross-source correctness matters; use the coordinator when you
need operational simplicity and transparent partial-failure handling.

---

## V6: Distributed query planning

The `DistributedQueryEngine` class (v6) introduces a query planning layer between the
coordinator and execution:

1. `analyzeSPARQLQuery()` (from `@unrdf/core`) classifies the query type, variable count,
   and presence of `GROUP BY`.
2. `selectExecutionStrategy()` chooses `PARALLEL`, `SEQUENTIAL`, or `ADAPTIVE` based on that
   analysis (simple queries with <= 5 variables → parallel; complex with > 10 variables or
   GROUP BY → sequential).
3. `createParallelPlan()` / `createSequentialPlan()` produce a tree of `PlanNode` objects
   (`MERGE`, `UNION`, `SCAN`).
4. `executePlan()` walks the plan tree recursively, executing `SCAN` nodes against individual
   stores and merging results at `MERGE`/`UNION` nodes.

The current `applyPushdown()` implementation is a stub that returns the query unchanged. In a
full production implementation it would rewrite the SPARQL to push filters and projections
down to each store, reducing data transfer and remote evaluation cost.

Cost estimation (`estimateCost()`) currently uses a simple heuristic: base cost of 100 plus
query length divided by store weight. In production this would use store statistics (triple
count, index availability) and cardinality estimates.
