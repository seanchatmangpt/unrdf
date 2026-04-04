# Reference: Endpoint and Configuration Schemas

All schemas are Zod objects. They are used internally for validation and are exported for
use in your own code when you need to validate configuration at the boundary of your
application.

---

## `CoordinatorConfigSchema`

Validates options passed to `createCoordinator()`.

```javascript
import { CoordinatorConfigSchema } from '@unrdf/federation';
```

| Field                 | Zod type                                                           | Default       |
| --------------------- | ------------------------------------------------------------------ | ------------- |
| `peers`               | `z.array(z.object({ id, endpoint, metadata? })).optional()`        | `[]`          |
| `peers[].id`          | `z.string()`                                                       | required      |
| `peers[].endpoint`    | `z.string().url()`                                                 | required      |
| `peers[].metadata`    | `z.record(z.string(), z.unknown()).optional()`                     | —             |
| `strategy`            | `z.enum(['broadcast', 'selective', 'first-available']).optional()` | `'broadcast'` |
| `timeout`             | `z.number().positive().optional()`                                 | `30000`       |
| `healthCheckInterval` | `z.number().positive().optional()`                                 | `60000`       |

---

## `PeerConfigSchema`

Validates arguments to `peerManager.registerPeer()` and `coordinator.addPeer()`.

```javascript
import { PeerConfigSchema } from '@unrdf/federation';
```

| Field      | Zod type                                       | Constraint          |
| ---------- | ---------------------------------------------- | ------------------- |
| `id`       | `z.string().min(1)`                            | Must not be empty   |
| `endpoint` | `z.string().url()`                             | Must be a valid URL |
| `metadata` | `z.record(z.string(), z.unknown()).optional()` | —                   |

---

## `PeerInfoSchema`

Shape returned by `coordinator.getPeer()`, `coordinator.listPeers()`, and `coordinator.addPeer()`.
Extends `PeerConfigSchema` with runtime fields.

```javascript
import { PeerInfoSchema } from '@unrdf/federation';
```

| Field          | Zod type                                         | Description                                 |
| -------------- | ------------------------------------------------ | ------------------------------------------- |
| `id`           | `z.string().min(1)`                              | Peer identifier                             |
| `endpoint`     | `z.string().url()`                               | HTTP endpoint URL                           |
| `metadata`     | `z.record(z.string(), z.unknown()).optional()`   | Arbitrary metadata                          |
| `registeredAt` | `z.number().positive()`                          | Unix timestamp (ms) of registration         |
| `lastSeen`     | `z.number().positive()`                          | Unix timestamp (ms) of last successful ping |
| `status`       | `z.enum(['healthy', 'degraded', 'unreachable'])` | Current health status                       |

Status transitions:

| From          | To            | Trigger                                            |
| ------------- | ------------- | -------------------------------------------------- |
| `healthy`     | `degraded`    | `ping()` returns non-OK HTTP response              |
| `healthy`     | `unreachable` | `ping()` throws (network error, timeout)           |
| `degraded`    | `unreachable` | Query fails with connection/timeout error          |
| `unreachable` | `healthy`     | Successful `ping()` or successful query response   |
| any           | `healthy`     | `peerManager.updateStatus(id, 'healthy')` (manual) |

---

## `QueryConfigSchema`

Validates options passed to `executeFederatedQuery()` and `executeDistributedQuery()`.

| Field     | Zod type                                                   | Default  |
| --------- | ---------------------------------------------------------- | -------- |
| `sparql`  | `z.string().min(1)`                                        | required |
| `timeout` | `z.number().positive().optional()`                         | `30000`  |
| `format`  | `z.enum(['json', 'xml', 'turtle', 'ntriples']).optional()` | `'json'` |

The `format` field controls both the HTTP `Content-Type` sent and the `Accept` header:

| Format     | Accept header                     |
| ---------- | --------------------------------- |
| `json`     | `application/sparql-results+json` |
| `xml`      | `application/sparql-results+xml`  |
| `turtle`   | `text/turtle`                     |
| `ntriples` | `application/n-triples`           |

---

## `QueryResultSchema`

Shape of a single-peer result object (returned by `executeFederatedQuery()` and individual
entries in `peerResults`).

| Field      | Zod type                | Description                            |
| ---------- | ----------------------- | -------------------------------------- |
| `success`  | `z.boolean()`           | `true` if the peer responded with 2xx  |
| `data`     | `z.any()`               | Parsed response body (JSON or text)    |
| `error`    | `z.string().optional()` | Error message when `success === false` |
| `duration` | `z.number()`            | Request duration in ms                 |
| `peerId`   | `z.string()`            | The peer that was queried              |

---

## `AdvancedFederationConfigSchema`

Validates options for `createAdvancedFederationEngine()`.

| Field          | Zod type                                  | Default   |
| -------------- | ----------------------------------------- | --------- |
| `sources`      | `z.array(FederatedSourceSchema).min(1)`   | required  |
| `streaming`    | `z.boolean()`                             | `true`    |
| `timeout`      | `z.number().int().positive()`             | `30000`   |
| `cache`        | `z.boolean()`                             | `true`    |
| `optimization` | `z.enum(['none', 'basic', 'aggressive'])` | `'basic'` |
| `trace`        | `z.boolean()`                             | `true`    |

`FederatedSourceSchema`:

| Field     | Zod type                                      | Default    | Description                     |
| --------- | --------------------------------------------- | ---------- | ------------------------------- |
| `url`     | `z.string().url()`                            | required   | SPARQL endpoint or file URL     |
| `type`    | `z.enum(['sparql', 'file', 'other'])`         | `'sparql'` | Source type for Comunica        |
| `auth`    | `z.object({ username, password }).optional()` | —          | HTTP basic authentication       |
| `headers` | `z.record(z.string(), z.string()).optional()` | —          | Custom HTTP headers per request |

---

## `QueryExecutionResultSchema`

Shape returned by `createAdvancedFederationEngine().query()` and `federatedQuery()`.

| Field                     | Zod type          | Description                        |
| ------------------------- | ----------------- | ---------------------------------- |
| `bindings`                | `BindingSchema[]` | Array of variable bindings         |
| `metadata.executionTime`  | `number`          | Total execution time in ms         |
| `metadata.sourcesQueried` | `number`          | Number of sources queried          |
| `metadata.resultCount`    | `number`          | Number of result bindings returned |
| `metadata.cached`         | `boolean`         | Whether results came from cache    |

`BindingSchema` — each key is a variable name, value is:

| Field      | Zod type                              | Description                            |
| ---------- | ------------------------------------- | -------------------------------------- |
| `value`    | `z.string()`                          | RDF term value (IRI or literal string) |
| `type`     | `z.enum(['uri', 'literal', 'bnode'])` | RDF term type                          |
| `datatype` | `z.string().optional()`               | XSD datatype IRI for literals          |
| `xml:lang` | `z.string().optional()`               | Language tag for language literals     |

---

## `FederationConfigSchema` (v6)

Validates options for `createFederationCoordinator()`.

| Field                   | Zod type                                              | Default        |
| ----------------------- | ----------------------------------------------------- | -------------- |
| `federationId`          | `z.string()`                                          | `randomUUID()` |
| `enableConsensus`       | `z.boolean()`                                         | `true`         |
| `healthCheckInterval`   | `z.number().positive()`                               | `5000`         |
| `healthCheckTimeout`    | `z.number().positive()`                               | `2000`         |
| `maxRetries`            | `z.number().int().nonnegative()`                      | `3`            |
| `loadBalancingStrategy` | `z.enum(['round-robin', 'weighted', 'least-loaded'])` | `'weighted'`   |

---

## `ReplicationConfigSchema` (v6)

Validates options for `createDataReplicationManager()`.

| Field                   | Zod type                            | Default             |
| ----------------------- | ----------------------------------- | ------------------- |
| `topology`              | `z.nativeEnum(ReplicationTopology)` | `'full-mesh'`       |
| `mode`                  | `z.nativeEnum(ReplicationMode)`     | `'bidirectional'`   |
| `conflictResolution`    | `z.nativeEnum(ConflictResolution)`  | `'last-write-wins'` |
| `batchSize`             | `z.number().int().positive()`       | `100`               |
| `batchInterval`         | `z.number().positive()`             | `1000`              |
| `enableStreaming`       | `z.boolean()`                       | `false`             |
| `maxRetries`            | `z.number().int().nonnegative()`    | `3`                 |
| `retryDelay`            | `z.number().positive()`             | `1000`              |
| `maxQueueSize`          | `z.number().int().positive()`       | `10000`             |
| `clockDriftThresholdMs` | `z.number().int().positive()`       | `60000`             |

---

## `RaftConfigSchema` (v6)

Validates options for `createConsensusManager()`.

| Field                | Zod type                | Default  |
| -------------------- | ----------------------- | -------- |
| `nodeId`             | `z.string()`            | required |
| `electionTimeoutMin` | `z.number().positive()` | `150`    |
| `electionTimeoutMax` | `z.number().positive()` | `300`    |
| `heartbeatInterval`  | `z.number().positive()` | `50`     |
| `maxLogEntries`      | `z.number().positive()` | `10000`  |
| `snapshotThreshold`  | `z.number().positive()` | `1000`   |
