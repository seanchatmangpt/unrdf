# Reference: Federation API

All functions and classes are exported from `@unrdf/federation`.

---

## `createCoordinator(config?)`

Create a federation coordinator. This is the **primary API** for most use cases.

**Parameters** — `CoordinatorConfig` (optional, validated by Zod):

| Field                 | Type                                              | Default       | Description                         |
| --------------------- | ------------------------------------------------- | ------------- | ----------------------------------- |
| `peers`               | `Array<{ id, endpoint, metadata? }>`              | `[]`          | Initial peers to register           |
| `strategy`            | `'broadcast' \| 'selective' \| 'first-available'` | `'broadcast'` | Default query routing strategy      |
| `timeout`             | `number` (ms, positive)                           | `30000`       | Default per-peer query timeout      |
| `healthCheckInterval` | `number` (ms, positive)                           | `60000`       | Interval for periodic health checks |

**Returns** `FederationCoordinator` — see coordinator methods below.

**Throws** `ZodError` if config fails validation.

---

## FederationCoordinator methods

### `coordinator.addPeer(id, endpoint, metadata?)`

Register a new peer or update an existing one, then ping it.

| Param      | Type                                 | Description                  |
| ---------- | ------------------------------------ | ---------------------------- |
| `id`       | `string`                             | Unique peer identifier       |
| `endpoint` | `string` (URL)                       | HTTP SPARQL endpoint URL     |
| `metadata` | `Record<string, unknown>` (optional) | Arbitrary key-value metadata |

**Returns** `Promise<PeerInfo>`

**Throws** `ZodError` if `endpoint` is not a valid URL.

**Side effects** Logs a warning if the peer is not reachable during the initial ping.

---

### `coordinator.removePeer(id)`

Remove a peer from the federation.

| Param | Type     | Description     |
| ----- | -------- | --------------- |
| `id`  | `string` | Peer identifier |

**Returns** `boolean` — `true` if the peer existed and was removed, `false` otherwise.

---

### `coordinator.getPeer(id)`

Get current information about one peer.

| Param | Type     | Description     |
| ----- | -------- | --------------- |
| `id`  | `string` | Peer identifier |

**Returns** `PeerInfo | null`

---

### `coordinator.listPeers(options?)`

List registered peers, optionally filtered by status.

| Option   | Type                                                  | Description                      |
| -------- | ----------------------------------------------------- | -------------------------------- |
| `status` | `'healthy' \| 'degraded' \| 'unreachable'` (optional) | Filter to peers with this status |

**Returns** `PeerInfo[]`

---

### `coordinator.query(sparqlQuery, options?)`

Execute a SPARQL query across the federation.

| Param         | Type     | Description         |
| ------------- | -------- | ------------------- |
| `sparqlQuery` | `string` | SPARQL query string |

Options:

| Option              | Type                                              | Description                                  |
| ------------------- | ------------------------------------------------- | -------------------------------------------- |
| `strategy`          | `'broadcast' \| 'selective' \| 'first-available'` | Override default routing strategy            |
| `timeout`           | `number` (ms)                                     | Override default timeout                     |
| `format`            | `'json' \| 'xml' \| 'turtle' \| 'ntriples'`       | Response format (default: `'json'`)          |
| `executionStrategy` | `'parallel' \| 'sequential'`                      | Peer execution order (default: `'parallel'`) |

**Returns** `Promise<AggregatedResult>`

```typescript
{
  success:       boolean;        // true if at least one peer succeeded
  results:       any[];          // deduplicated bindings from all successful peers
  peerResults:   QueryResult[];  // individual per-peer results
  totalDuration: number;         // wall-clock ms
  successCount:  number;
  failureCount:  number;
  error?:        string;         // set when success === false
}
```

**Never throws.** Returns `{ success: false, error: 'No healthy peers available', … }` when
no healthy peers exist.

---

### `coordinator.queryPeer(peerId, sparqlQuery, options?)`

Execute a query on one specific peer by ID.

| Param         | Type     | Description     |
| ------------- | -------- | --------------- |
| `peerId`      | `string` | Peer identifier |
| `sparqlQuery` | `string` | SPARQL query    |

Options: `timeout`, `format` (same as `query()`).

**Returns** `Promise<QueryResult>`

```typescript
{
  success:  boolean;
  data:     any | null;   // parsed HTTP response body
  error?:   string;
  duration: number;       // ms
  peerId:   string;
}
```

**Side effects** Updates peer status to `'degraded'` or `'unreachable'` on failure.

---

### `coordinator.healthCheck()`

Ping all registered peers and return a health summary.

**Returns** `Promise<HealthCheckResult>`

```typescript
{
  totalPeers: number;
  healthyPeers: number;
  degradedPeers: number;
  unreachablePeers: number;
  results: Array<{
    id: string;
    healthy: boolean;
    status: 'healthy' | 'degraded' | 'unreachable';
  }>;
}
```

---

### `coordinator.startHealthChecks()`

Start a `setInterval` that calls `healthCheck()` on every `healthCheckInterval` ms. A
second call while a timer is already running is a no-op.

**Returns** `void`

---

### `coordinator.stopHealthChecks()`

Clear the health-check timer. Safe to call when no timer is running.

**Returns** `void`

---

### `coordinator.destroy()`

Stop health checks and release the timer. Call before discarding the coordinator or before
your process exits to prevent the event loop from staying alive.

**Returns** `void`

---

### `coordinator.getStats()`

Get current federation statistics.

**Returns** `FederationStats`

```typescript
{
  totalPeers: number;
  healthyPeers: number;
  degradedPeers: number;
  unreachablePeers: number;
  totalQueries: number;
  totalErrors: number;
  errorRate: number; // totalErrors / totalQueries, or 0
}
```

---

## `createPeerManager()`

Create a standalone peer manager. The coordinator creates one internally; use this directly
only when you need peer tracking without the query routing layer.

**Returns** `PeerManager` with methods: `registerPeer`, `unregisterPeer`, `getPeer`,
`listPeers`, `ping`, `updateStatus`, `clear`, `size`.

See source for full signatures: `packages/federation/src/federation/peer-manager.mjs`.

---

## `executeFederatedQuery(peerId, endpoint, sparqlQuery, options?)`

Execute a SPARQL query against a single peer via HTTP POST.

| Param         | Type     | Description                          |
| ------------- | -------- | ------------------------------------ |
| `peerId`      | `string` | Peer identifier (for result tagging) |
| `endpoint`    | `string` | HTTP endpoint URL                    |
| `sparqlQuery` | `string` | SPARQL query string                  |

Options: `timeout` (number, ms), `format` (`'json' | 'xml' | 'turtle' | 'ntriples'`).

**Returns** `Promise<QueryResult>`

Uses `Content-Type: application/sparql-query` and the format-appropriate `Accept` header.
Returns `{ success: false, error, duration, peerId }` on any failure rather than throwing.

---

## `executeDistributedQuery(peers, sparqlQuery, options?)`

Execute a SPARQL query across an array of peers.

| Param         | Type                                      | Description         |
| ------------- | ----------------------------------------- | ------------------- |
| `peers`       | `Array<{ id: string, endpoint: string }>` | Peers to query      |
| `sparqlQuery` | `string`                                  | SPARQL query string |

Options:

| Option     | Type                         | Default      | Description                                       |
| ---------- | ---------------------------- | ------------ | ------------------------------------------------- |
| `timeout`  | `number` (ms)                | `30000`      | Per-peer timeout                                  |
| `format`   | `string`                     | `'json'`     | Response format                                   |
| `strategy` | `'parallel' \| 'sequential'` | `'parallel'` | Whether to query peers in parallel or in sequence |

**Returns** `Promise<AggregatedResult>` — same shape as `coordinator.query()` return value.

---

## `aggregateResults(results)`

Combine and deduplicate results from multiple peer query results.

| Param     | Type            | Description               |
| --------- | --------------- | ------------------------- |
| `results` | `QueryResult[]` | Array of per-peer results |

Handles SPARQL JSON results format (`data.results.bindings`), plain arrays, and single
objects. Deduplication is by `JSON.stringify` key.

**Returns** `any[]`

---

## `routeQuery(allPeers, sparqlQuery, strategy?)`

Select peers for a query based on strategy.

| Param         | Type                                              | Description                                      |
| ------------- | ------------------------------------------------- | ------------------------------------------------ |
| `allPeers`    | `Array<{ id, endpoint, metadata? }>`              | Available (pre-filtered healthy) peers           |
| `sparqlQuery` | `string`                                          | SPARQL query (used for future selective routing) |
| `strategy`    | `'broadcast' \| 'selective' \| 'first-available'` | Default `'broadcast'`                            |

**Returns** `Array<{ id: string, endpoint: string }>` — selected peers.

| Strategy          | Behaviour                                       |
| ----------------- | ----------------------------------------------- |
| `broadcast`       | Returns all peers                               |
| `selective`       | Returns all peers (currently same as broadcast) |
| `first-available` | Returns `[allPeers[0]]`                         |

---

## `createAdvancedFederationEngine(config)`

Create a Comunica-backed federation engine for standards-compliant cross-source SPARQL.

**Parameters** — validated by `AdvancedFederationConfigSchema`:

| Field          | Type                                     | Default   | Description                               |
| -------------- | ---------------------------------------- | --------- | ----------------------------------------- |
| `sources`      | `Array<{ url, type?, auth?, headers? }>` | required  | SPARQL sources (minimum 1)                |
| `streaming`    | `boolean`                                | `true`    | Enable per-binding streaming callback     |
| `timeout`      | `number` (ms)                            | `30000`   | Query timeout                             |
| `cache`        | `boolean`                                | `true`    | Enable result caching                     |
| `optimization` | `'none' \| 'basic' \| 'aggressive'`      | `'basic'` | Comunica lenient mode when `'aggressive'` |
| `trace`        | `boolean`                                | `true`    | Enable OTEL tracing                       |

**Returns** `Promise<AdvancedFederationEngine>` with methods:

- `engine.query(sparql, options?)` — execute query, returns `QueryExecutionResult`
- `engine.distributedQuery(sparql)` — use UNRDF distributed mechanism instead of Comunica
- `engine.getSourcesMetadata()` — query triple count from each source
- `engine.close()` — release resources
- `engine.comunica` — underlying `QueryEngine` instance
- `engine.coordinator` — underlying UNRDF coordinator

**Throws** `ZodError` if config is invalid.

---

## `federatedQuery(endpoints, query)`

One-shot convenience wrapper: create engine, run query, close engine.

| Param       | Type       | Description                   |
| ----------- | ---------- | ----------------------------- |
| `endpoints` | `string[]` | Array of SPARQL endpoint URLs |
| `query`     | `string`   | SPARQL query string           |

**Returns** `Promise<QueryExecutionResult>`

---

## `streamFederatedQuery(endpoints, query, onBinding)`

One-shot streaming query. Calls `onBinding` for each result binding as it arrives.

| Param       | Type                        | Description                    |
| ----------- | --------------------------- | ------------------------------ |
| `endpoints` | `string[]`                  | Array of SPARQL endpoint URLs  |
| `query`     | `string`                    | SPARQL query string            |
| `onBinding` | `(binding: object) => void` | Called for each result binding |

**Returns** `Promise<void>`

---

## `createFederationCoordinator(config?)` (v6)

Create a store-level federation coordinator with RAFT consensus and load balancing. Use this
when stores are long-lived services identified by `storeId` rather than transient HTTP peers.

| Field                   | Type                                            | Default        | Description                       |
| ----------------------- | ----------------------------------------------- | -------------- | --------------------------------- |
| `federationId`          | `string`                                        | `randomUUID()` | Federation identifier             |
| `enableConsensus`       | `boolean`                                       | `true`         | Enable RAFT consensus manager     |
| `healthCheckInterval`   | `number` (ms)                                   | `5000`         | Store health check interval       |
| `healthCheckTimeout`    | `number` (ms)                                   | `2000`         | Health check request timeout      |
| `maxRetries`            | `number`                                        | `3`            | Max retries for failed operations |
| `loadBalancingStrategy` | `'round-robin' \| 'weighted' \| 'least-loaded'` | `'weighted'`   | Store selection strategy          |

**Returns** `FederationCoordinator` (class instance, extends `EventEmitter`)

Key methods: `initialize()`, `registerStore(metadata)`, `deregisterStore(storeId)`,
`getStores()`, `getHealthyStores()`, `selectStore(options?)`, `checkStoreHealth(storeId)`,
`getStats()`, `shutdown()`.

Key events: `initialized`, `storeRegistered`, `storeDeregistered`, `storeHealthChanged`, `shutdown`.

---

## `createConsensusManager(config)` (v6)

Create a RAFT consensus manager.

| Field                | Type     | Default  | Description                         |
| -------------------- | -------- | -------- | ----------------------------------- |
| `nodeId`             | `string` | required | Unique node identifier              |
| `electionTimeoutMin` | `number` | `150`    | Minimum election timeout ms         |
| `electionTimeoutMax` | `number` | `300`    | Maximum election timeout ms         |
| `heartbeatInterval`  | `number` | `50`     | Heartbeat interval ms (leader only) |
| `maxLogEntries`      | `number` | `10000`  | Maximum log entries before snapshot |
| `snapshotThreshold`  | `number` | `1000`   | Log entries before compaction       |

**Returns** `ConsensusManager` (class instance, extends `EventEmitter`)

Key methods: `initialize()`, `addPeer(nodeId, endpoint)`, `removePeer(nodeId)`,
`replicate(command)`, `handleAppendEntries(request)`, `getState()`, `destroy()`, `shutdown()`.

Key events: `initialized`, `stateChange` (emits `NodeState`), `commandApplied`, `shutdown`.

`NodeState` values: `'follower'`, `'candidate'`, `'leader'`

---

## `createDataReplicationManager(coordinator, config?)` (v6)

Create a multi-master data replication manager.

Key config fields (`ReplicationConfigSchema`):

| Field                | Type                  | Default             | Description                                         |
| -------------------- | --------------------- | ------------------- | --------------------------------------------------- |
| `topology`           | `ReplicationTopology` | `'full-mesh'`       | Replication topology                                |
| `mode`               | `ReplicationMode`     | `'bidirectional'`   | Push / pull / bidirectional                         |
| `conflictResolution` | `ConflictResolution`  | `'last-write-wins'` | Conflict resolution strategy                        |
| `batchSize`          | `number`              | `100`               | Operations per batch                                |
| `batchInterval`      | `number` (ms)         | `1000`              | Batch flush interval                                |
| `enableStreaming`    | `boolean`             | `false`             | Process immediately instead of batching             |
| `maxRetries`         | `number`              | `3`                 | Retry count for failed replications                 |
| `maxQueueSize`       | `number`              | `10000`             | Maximum pending operations before overflow handling |

**Returns** `DataReplicationManager` (class instance, extends `EventEmitter`)

Key methods: `initialize()`, `replicate(change)`, `getStats()`, `getHLCTimestamp()`, `shutdown()`.

Key events: `initialized`, `changeReplicated`, `conflict`, `replicationError`,
`partialReplicationFailure`, `queueOverflow`, `queueEntriesDropped`, `clockDriftDetected`,
`suspiciousVersion`, `shutdown`.

`ReplicationTopology` values: `'full-mesh'`, `'star'`, `'ring'`, `'tree'`

`ConflictResolution` values: `'last-write-wins'`, `'first-write-wins'`, `'manual'`, `'merge'`, `'custom'`

`ReplicationMode` values: `'push'`, `'pull'`, `'bidirectional'`

---

## Metrics functions

These are called internally by the coordinator. Expose them directly only for testing or
custom instrumentation.

| Function                                  | Description                                            |
| ----------------------------------------- | ------------------------------------------------------ |
| `recordQuery(peerId, duration, strategy)` | Increment query counter and record duration histogram  |
| `recordError(peerId, errorType)`          | Increment error counter                                |
| `updatePeerMetrics(stats)`                | Update peer health gauges                              |
| `trackConcurrentQuery()`                  | Increment concurrent counter; returns cleanup function |
| `getMetricsState()`                       | Return `{ concurrentQueries, initialized }`            |
| `resetMetrics()`                          | Reset all metric state (primarily for tests)           |

OTEL instrument names:

| Instrument                      | Type          | Description                                           |
| ------------------------------- | ------------- | ----------------------------------------------------- |
| `federation.queries.total`      | Counter       | Total queries, tagged `peer_id`, `strategy`, `status` |
| `federation.errors.total`       | Counter       | Total errors, tagged `peer_id`, `error_type`          |
| `federation.query.duration`     | Histogram     | Query duration ms, tagged `peer_id`, `strategy`       |
| `federation.peer.metrics`       | UpDownCounter | Peer health gauges, tagged `metric`                   |
| `federation.queries.concurrent` | UpDownCounter | In-flight query count                                 |
