# CHANGELOG — v1.0.0 (GA)

### Summary

GA. API frozen. Pack schema v1. Stable metrics. Signed, canonical receipts. SPARQL + SHACL coverage widened. Disk store + namespaces + cluster modes locked. Bazel rules v1. Happy path only.

---

### Repo layout deltas

```
knowd/
  api/
    openapi/v1.yaml                 # HTTP schema (frozen)
    proto/knowd/v1/*.proto          # gRPC schema (frozen)
    pack/schema-v1.json             # Pack JSON Schema (frozen)
  cmd/knowd/main.go                 # normalized flags, GA defaults
  internal/
    store/                          # disk/mem; GA switches
    sparql/                         # +union/optional/minus/bind/values
    shacl/                          # +paths, min/maxLength, nodeKind, or/xone
    lockchain/
      urdna2015.go                  # canonicalization (URDNA2015)
      jws.go                        # detached JWS (Ed25519) option
    policy/
      validate.go                   # pack v1 validation
    telemetry/
      metrics.go                    # GA metric names + stability docs
    server/
      http.go                       # GA wire shapes
      grpc.go                       # reflection + health
  rules_knowd/
    defs.bzl                        # v1 macros (frozen)
```

---

### Added

* **Pack schema v1 (frozen)**

  * `api/pack/schema-v1.json` for JSON; TTL vocabulary documented.
  * On startup and hot-reload, packs are validated.

* **Receipts GA**

  * URDNA2015 canonicalization.
  * Ed25519 signatures; optional detached JWS.
  * Verify endpoint returns `canonicalHash`, `sig`, `pub`.

* **SPARQL**

  * `UNION`, `OPTIONAL`, `MINUS`, `BIND`, `VALUES`.
  * Prepared params on all endpoints.

* **SHACL**

  * Property paths (`sh:path` sequences), `minLength`, `maxLength`, `nodeKind`, `or`/`xone`.
  * Compact report remains.

* **Cluster ergonomics**

  * Follower promote API (manual): barrier + snapshot tag.
  * Health + reflection for gRPC.

* **Bazel v1**

  * `knowd_pack`, `knowd_binary`, `knowd_receipt_test`, `knowd_vector_index`, `knowd_pack_wasm` stabilized.

* **Observability**

  * Metric names versioned under `knowd_*_v1`.
  * OTEL semantic attrs documented.

---

### Changed

* **HTTP wire**

  * `/v1/query` returns structured rows: `{"rows":[{"col":"val"}], "stats":{...}}`.
  * `/v1/query/stream` = NDJSON rows, final trailer with stats.
  * `/v1/tx` returns `{receipt, delta}`; field names aligned to schema.

* **Flags → normalized**

  * All flags/env use consistent nouns; see migration map.

* **Defaults**

  * Store: `disk`.
  * Planner: CBO on.
  * SHACL: enabled.
  * Namespaces: `default`.
  * Cluster: `single`.
  * mTLS: required in `leader|follower` modes.

---

### Removed

* Legacy minimal canonicalization.
* Ad-hoc receipt JSON fields (replaced by GA schema).
* Experimental query params not in OpenAPI.

---

### Flags / Env (GA)

```
-addr                      | KNOWD_ADDR              (default :8090)
-data-dir                  | KNOWD_DATA_DIR          (./data)
-store (disk|mem)          | KNOWD_STORE             (disk)
-packs (csv)               | KNOWD_PACKS
-watch-packs               | KNOWD_WATCH_PACKS       (true)

# Planner / stats
-planner-cbo               | KNOWD_PLANNER_CBO       (true)
-analyze-sample            | KNOWD_ANALYZE_SAMPLE
-analyze-ttl-sec           | KNOWD_ANALYZE_TTL_SEC

# SHACL
-shacl-enabled             | KNOWD_SHACL_ENABLED     (true)

# Streaming / prepare
-query-stream-max-bytes    | KNOWD_QUERY_STREAM_MAX_BYTES
-plan-cache-size           | KNOWD_PLAN_CACHE_SIZE
-plan-cache-persist        | KNOWD_PLAN_CACHE_PERSIST (true)

# Namespaces
-namespace-default         | KNOWD_NAMESPACE_DEFAULT (default)

# Cluster
-cluster-mode              | KNOWD_CLUSTER_MODE      (single|leader|follower)
-peer-addrs                | KNOWD_PEER_ADDRS
-mtls-cert|key|ca          | KNOWD_MTLS_CERT|KEY|CA  (required if cluster != single)

# Receipts / signing
-receipts-dir              | KNOWD_RECEIPTS_DIR      (./receipts)
-signing-key|pub           | KNOWD_SIGNING_KEY|PUB

# Vector / WASM (if used)
-vec-enabled               | KNOWD_VEC_ENABLED
-wasm-enabled              | KNOWD_WASM_ENABLED
```

---

### HTTP / gRPC (finalized)

* `POST /v1/tx`
* `POST /v1/query`
* `POST /v1/query/stream`
* `POST /v1/query/at`
* `POST /v1/hooks/evaluate`
* `POST /v1/validate`
* `GET  /v1/receipts/{id}`
* `GET  /v1/receipts/{id}/verify`
* `GET  /v1/receipts/search?...`
* `GET  /v1/store/stats`
* `POST /v1/packs/reload`
* `POST /v1/admin/namespaces`
* `GET  /v1/admin/namespaces`
* `POST /v1/admin/rollout`
* `GET  /v1/admin/rollout`
* `POST /v1/admin/promote-follower`
* Optional: `POST /v1/similar`, `POST /v1/vector/upsert`, `POST /v1/admin/replay`

gRPC mirrors per `api/proto/knowd/v1`.

---

### Performance (targets locked)

* Hook eval p95 ≤ 75 ms.
* SPARQL select p95 ≤ 300 ms on 1M triples.
* Streaming rows steady-state O(1) buffer.
* Plan cache warm restart improves first-query by 30–45%.
* HNSW topK on 50k items: 1–5 ms p95.

---

### Compatibility

* v0.6–v0.9 packs load after auto-migrate to pack v1 (on-the-fly; happy path).
* Metrics renamed to `*_v1`; old names stop emitting.

---

### Migration (from v0.9.x)

1. **Stop node. Snapshot.**
2. **Upgrade binary** and Bazel macros to `rules_knowd@v1`.
3. **Rename flags/env** to GA names.
4. **Pack check**: `knowd --check-pack <path>` (validates to v1).
5. **Receipts**: set `-signing-key/-signing-pub` for JWS.
6. **Cluster**: provide mTLS certs if `leader|follower`.
7. **Smoke**: `GET /healthz`, then `/v1/store/stats`, simple `/v1/query`.

Flag map examples:

* `-compact-threshold` → retained
* `-quota-qps`/`-quota-rowsps` → unchanged
* Legacy env `UNRDF_*` → not supported

---

### Notes / Limits

* Single-leader cluster; manual promotion only.
* SHACL advanced constraints beyond listed are out-of-scope.
* Vector embedding provider remains pluggable; default is hashing stub.
* Happy path: inputs assumed valid.


# CHANGELOG — v0.9.0

### Summary

Cost-based planner with stats, materialized views, time-travel queries, per-namespace quotas, vector upserts, and receipt search. Happy path only.

---

### Repo layout deltas

```
knowd/
  cmd/knowd/main.go                      # +flags: planner, views, quotas, timetravel
  internal/
    catalog/
      stats.go                           # ANALYZE: card, ndv, hist
      collect.go                         # sampling + persist
    sparql/
      cost.go                            # cost model (selectivity, join)
      cbo.go                             # cost-based planner v1
      mv.go                              # materialized CONSTRUCT views
    views/
      registry.go                        # view registry per namespace
      builder.go                         # build/refresh pipeline
      trigger.go                         # tx-triggered incremental refresh
    timetravel/
      at.go                              # snapshot binding for queries
    quotas/
      quota.go                           # token bucket per namespace
      admin.go                           # set/list quotas
    vec/
      upsert.go                          # online add/update to HNSW
    receipts/
      index.go                           # in-mem index + scan
      search.go                          # filters (actor, time, tag)
    server/
      http.go                            # +admin/analyze, +admin/views/*, +query/at, +receipts/search, +admin/quotas, +vector/upsert
      grpc.go                            # gRPC mirrors
    policy/
      quotas.go                          # quotas from pack
      views.go                           # views from pack
```

---

### Added

* **Cost-based planner (CBO v1)**

  * Stats catalog (`ANALYZE`) with cardinalities, NDV, simple histograms.
  * Join order selection and index choice based on stats.
  * Plan cache augmented with stats digest.

* **Materialized views**

  * Pack-defined `CONSTRUCT` views with incremental refresh on tx.
  * Admin build/status; namespace-scoped registry.
  * Planner can rewrite eligible SELECTs to use views.

* **Time-travel queries**

  * Bind query to snapshot time.
  * `POST /v1/query/at` or `{"at":"RFC3339"}` in `/v1/query`.

* **Per-namespace quotas**

  * Token bucket limits for QPS and streaming rows/sec.
  * From pack or admin API.

* **Vector upserts**

  * Online add/update items to HNSW per namespace.
  * `POST /v1/vector/upsert` with `{"id":"...","text":"..."}`.

* **Receipt search**

  * Filter by actor, since/until, tag.
  * `GET /v1/receipts/search?actor=a&since=t1&until=t2`.

* **Bazel (rules_knowd)**

  * `knowd_analyze` target to precompute stats.
  * `knowd_view` to build materialized views at build time.

---

### Changed

* **Server**

  * `/v1/query` uses CBO when stats exist; falls back to heuristic.
  * Tx triggers view refresh hooks if affected.

* **Policy packs**

  * Optional `views.json` and `quotas.json`.
  * Quotas applied on hot-reload.

---

### Flags / Env

```
# Planner / stats
-planner-cbo                (default true)               | KNOWD_PLANNER_CBO
-analyze-sample             (default 100000)             | KNOWD_ANALYZE_SAMPLE
-analyze-ttl-sec            (default 3600)               | KNOWD_ANALYZE_TTL_SEC

# Views
-views-enabled              (default true)               | KNOWD_VIEWS_ENABLED
-views-refresh-workers      (default 2)                  | KNOWD_VIEWS_REFRESH_WORKERS

# Time-travel
-timetravel-enabled         (default true)               | KNOWD_TIMETRAVEL_ENABLED

# Quotas
-quota-qps                  (default 0 = unlimited)      | KNOWD_QUOTA_QPS
-quota-rowsps               (default 0 = unlimited)      | KNOWD_QUOTA_ROWSPS
```

---

### HTTP / gRPC API (additions)

* `POST /v1/admin/analyze`
  `{"namespace":"ns"}` → `{"statsVersion":"v","tables":...}`

* `POST /v1/admin/views/build`
  `{"namespace":"ns","names":["v1","v2"]}` → build results

* `GET /v1/admin/views/status?ns=ns` → view registry info

* `POST /v1/query/at`
  `{"query":"SELECT ...","at":"2025-10-02T10:00:00Z"}` → rows at snapshot

* `GET /v1/receipts/search?actor=a&since=...&until=...` → receipt list

* `POST /v1/admin/quotas`
  `{"namespace":"ns","qps":100,"rowsps":5000}` → applied

* `POST /v1/vector/upsert`
  `{"id":"doc-1","text":"..."}`

gRPC mirrors added for Analyze, ViewsBuild/Status, QueryAt, ReceiptsSearch, SetQuotas, VectorUpsert.

---

### SDK (updates)

* `Analyze(ns string) []byte`
* `ViewsBuild(ns string, names ...string) []byte`
* `ViewsStatus(ns string) []byte`
* `QueryAt(q, at string) <-chan []byte`
* `ReceiptsSearch(f Filters) []byte`
* `SetQuotas(ns string, qps, rowsps int) []byte`
* `VectorUpsert(id, text string) []byte`

---

### Performance (happy path)

* CBO reduces complex multi-join SELECT p95 by ~20–45% with stats present.
* Views cut hot query latency by ~2–10× on eligible patterns.
* Vector upsert amortized ~0.2–1.5 ms/item after warm index.

---

### Notes / Limits

* Stats sampling approximate; refresh TTL required for drift.
* View refresh is incremental best-effort; full rebuild on mismatch.
* Time-travel granularity = snapshot cadence; not per-tx.
* Quotas apply at ingress; no retroactive throttling mid-stream.


# CHANGELOG — v0.8.0

### Summary

Vector signals, WASM effects, remote snapshots, and pack pinning. Federated SELECT unchanged. Happy path only.

---

### Repo layout deltas

```
knowd/
  cmd/knowd/main.go                       # +flags: vec, wasm, objstore, pin
  internal/
    vec/
      embed.go                            # provider iface (local/none)
      hnsw.go                             # in-proc HNSW index
      index.go                            # build/load per-namespace vector index
    wasm/
      runtime.go                          # WASI runner for hook effects
      loader.go                           # load .wasm from packs
    policy/
      registry.go                         # pack registry + pinning (stable/canary)
    store/disk/
      objstore.go                         # snapshot push/pull (s3:// | gs://)
      encrypt.go                          # snapshot at-rest AES-256 keyfile
    server/
      http.go                             # +/v1/similar, +replay, +pack pin
      grpc.go                             # +Similar, +Replay
    replay/
      replay.go                           # deterministic receipt replay
    telemetry/
      vec_metrics.go                      # query/build latencies
  rules_knowd/
    defs.bzl                              # +knowd_pack_wasm, +knowd_vector_index
```

---

### Added

* **Vector signals**

  * Embedding provider interface (default: hashing stub).
  * HNSW in-memory index per namespace. Built from pack terms/snippets.
  * Endpoint: `POST /v1/similar`
    `{ "text":"...", "topK":5 }` → nearest items with scores.
  * Bazel: `knowd_vector_index(name, srcs=[...])` produces baked index.

* **WASM effects**

  * Hooks can reference `.wasm` modules; executed via WASI sandbox.
  * Pack field: `"effect": { "wasm":"effects/foo.wasm", "exports":["run"] }"`
  * Bazel: `knowd_pack_wasm(name, srcs=["effects/*.wasm"])` bundles modules.

* **Remote snapshots**

  * Push/pull store snapshots to `s3://` or `gs://` URIs.
  * Optional AES-256 file key for local at-rest encryption.

* **Pack pinning**

  * Registry API to pin namespace to exact pack version.
  * Canary still supported; pin overrides rollout.

* **Deterministic replay**

  * Re-run a receipt set into a fresh namespace to revalidate outcomes.
  * Admin endpoints added.

---

### Changed

* **Policy loader**

  * Loads optional `vectors.json` and `effects/*.wasm` if present.
  * Registry tracks `{name, version, digest}` and pin state.

* **Server**

  * Namespaced vector index auto-warm on start and on pack reload.
  * Replay runs against empty store snapshot and emits a new receipt set.

---

### Flags / Env

```
# Vector
-vec-enabled               (default true)                 | KNOWD_VEC_ENABLED
-vec-topk-default          (default 5)                    | KNOWD_VEC_TOPK
-vec-hnsw-M                (default 16)                   | KNOWD_VEC_HNSW_M
-vec-hnsw-efc              (default 200)                  | KNOWD_VEC_HNSW_EFC

# WASM
-wasm-enabled              (default true)                 | KNOWD_WASM_ENABLED
-wasm-max-mem              (default 64MiB)                | KNOWD_WASM_MAX_MEM

# Remote snapshots
-objstore                   ("" | s3://... | gs://...)    | KNOWD_OBJSTORE
-enc-keyfile                (path to 32B key)             | KNOWD_ENC_KEYFILE

# Packs
-pack-registry              (path|url)                    | KNOWD_PACK_REGISTRY
```

---

### HTTP / gRPC API (additions)

* `POST /v1/similar`
  Request: `{"text":"...","topK":10}`
  Response: `{"items":[{"id":"...","score":0.87},...]}`

* `POST /v1/admin/replay`
  Request: `{"namespace":"replay-ns","receipts":["id1","id2"]}`
  Response: `{"namespace":"replay-ns","applied":2}`

* `POST /v1/admin/pack/pin`
  Request: `{"namespace":"team-a","name":"policy","version":"v14"}`
  Response: `{"pinned":true}`

gRPC:

* `Similar(SimilarRequest) returns (stream SimilarItem)`
* `Replay(ReplayRequest) returns (ReplayResult)`

---

### Bazel (rules_knowd)

```starlark
load("@rules_knowd//knowd:defs.bzl",
     "knowd_pack", "knowd_pack_wasm", "knowd_vector_index")

knowd_pack(
  name = "policy_pack",
  srcs = ["policy.ttl", "hooks.json", "vectors.json"],
)

knowd_pack_wasm(
  name = "effects",
  srcs = glob(["effects/*.wasm"]),
)

knowd_vector_index(
  name = "vec_index",
  texts = ["snippets/*.md", "docs/*.md"],
)
```

---

### SDK (updates)

* `Similar(ctx, text string, topK int) ([]Item, []byte)`
* `Replay(ctx, ns string, ids []string) []byte`

---

### Performance (happy path)

* HNSW topK on 50k items: ~1–5 ms p95 in-memory after warmup.
* WASM effect call overhead: ~0.2–0.6 ms per invocation.
* Remote snapshot push/pull gated by bandwidth; local encryption adds ~3–7%.

---

### Notes / Limits

* Embedding default is a hashing stub; swap provider in packs for real models.
* WASM has no host FS/network; stdin/stdout only via runtime bridge.
* Object-store creds assumed preconfigured by environment.
* Replay assumes receipts reference deterministic inputs. No diffing UI.


# CHANGELOG — v0.7.0

### Summary

Multi-tenant namespaces, leader/follower replication, federated SELECT, and pack version rollouts. mTLS for service-to-service. Happy path only.

---

### Repo layout deltas

```
knowd/
  cmd/knowd/main.go                      # +cluster flags, namespace default, mtls
  internal/
    cluster/
      leader.go                          # write path, WAL replication, snapshot ship
      follower.go                        # read path, apply log, catch-up
      rpc.go                             # gRPC replication service
      scattergather.go                   # federated SELECT across followers
    namespace/
      ns.go                              # namespace registry (isolation, defaults)
      middleware.go                      # HTTP/gRPC ns binding (header/env)
    policy/
      rollout.go                         # pack versioning, staged rollout (% traffic)
    auth/
      mtls.go                            # server/client cert load
      tokens.go                          # HMAC request token (optional)
    server/
      http.go                            # +admin routes (ns, rollout)
      grpc.go                            # +replication +admin RPCs
    telemetry/
      cluster_metrics.go                 # lag, apply rates, fanout timings
```

---

### Added

* **Namespaces (multi-tenant)**

  * Isolation of store, plan cache, windows, and receipts per `namespace`.
  * Selection via `X-KNOWD-NS` header or `?ns=` query param; default namespace from flag.
  * Admin API to create/list namespaces.

* **Cluster (leader/follower)**

  * **Leader**: single writer. Replicates WAL entries and periodic snapshots to followers.
  * **Follower**: applies WAL, serves queries; promotes read scalability.
  * Replication over gRPC streaming with backpressure; snapshot ship on gap.
  * Metrics: replication lag seconds, apply ops/s, snapshot size.

* **Federated SELECT (scatter/gather)**

  * Leader fans out SELECT to followers, merges rows, applies ORDER/LIMIT on coordinator.
  * Hint: `/* fanout */` enables scatter for eligible queries.

* **Pack rollouts**

  * Versioned packs per namespace.
  * Staged rollout: `{stable:v, canary:v+1, percent: N}`. Request routing via token hash.

* **mTLS + tokens**

  * Service-to-service mTLS (leader↔follower, SDK).
  * Optional HMAC token header `X-KNOWD-TOKEN` for external clients.

---

### Changed

* **Server**

  * Binds namespace from header/param in HTTP and gRPC middleware.
  * `/v1/query` respects namespace; can scatter on leader if hinted.
  * `/v1/tx` accepted only on leader; followers reject in cluster mode (happy path).

* **Store**

  * Namespaced subtrees on disk: `data/<ns>/...`.
  * Snapshot/plan-cache persisted per namespace.

---

### Flags / Env

```
# Namespaces
-namespace-default        (default "default")             | KNOWD_NAMESPACE_DEFAULT

# Cluster
-cluster-mode             (single|leader|follower)        | KNOWD_CLUSTER_MODE
-peer-addrs               (comma list)                    | KNOWD_PEER_ADDRS
-repl-snapshot-sec        (default 300)                   | KNOWD_REPL_SNAPSHOT_SEC
-repl-stream-chunk-bytes  (default 1_048_576)             | KNOWD_REPL_STREAM_CHUNK_BYTES

# Security
-mtls-cert                (path)                          | KNOWD_MTLS_CERT
-mtls-key                 (path)                          | KNOWD_MTLS_KEY
-mtls-ca                  (path)                          | KNOWD_MTLS_CA
-token-secret             (string)                        | KNOWD_TOKEN_SECRET

# Rollouts
-rollout-enabled          (bool, default true)            | KNOWD_ROLLOUT_ENABLED
```

---

### HTTP / gRPC API (additions)

* **Namespaces**

  * `POST /v1/admin/namespaces` → `{"name":"<ns>"}`
  * `GET  /v1/admin/namespaces` → `["default","team-a",...]`
  * Use header `X-KNOWD-NS: team-a` for all standard endpoints.

* **Rollouts**

  * `POST /v1/admin/rollout`

    ```json
    { "namespace":"team-a", "stable":"v12", "canary":"v13", "percent":20 }
    ```
  * `GET /v1/admin/rollout?ns=team-a` → current policy

* **Cluster**

  * gRPC `Replication` service: `StreamWAL`, `StreamSnapshot`, `Ack`.
  * HTTP probe: `GET /v1/cluster/status` → `{"mode":"leader","followers":2,"lagSec":0.4}`

---

### SDK (updates)

* `Client.WithNamespace(ns string) *Client`
* `Client.EnablemTLS(cert,key,ca string) *Client`
* `Client.WithToken(secret string) *Client`

---

### Performance (happy path)

* Federated SELECT across 3 followers: p95 −30–45% vs single node on large scans.
* WAL apply throughput on followers: ~80–120k quads/s on commodity hosts.
* Namespace plan-cache locality improves hit rate under mixed tenants.

---

### Notes / Limits

* Single leader; no automatic failover or elections.
* Replication is at-least-once; idempotent apply assumed.
* Scatter/gather only for pure SELECT without side effects; ORDER BY applied post-merge.
* Rollout routing uses simple hash; no per-user sticky canary beyond token hash.
* mTLS required for cluster RPCs when certs provided; otherwise plaintext in dev.


# CHANGELOG — v0.6.0

### Summary

SHACL validation, streaming query APIs, signed receipts, and storage compaction. Plan cache persists across restarts. Happy path only.

---

### Repo layout deltas

```
knowd/
  cmd/knowd/main.go                     # +flags: shacl, stream, signing, compaction
  internal/
    shacl/
      shapes.go                         # pack→shapes model
      validator.go                      # core SHACL (node/property shapes)
      report.go                         # compact JSON reports
    policy/
      shapes_loader.go                  # load shapes from packs
    server/
      http.go                           # +/v1/validate, +/v1/query/stream
      grpc.go                           # +Validate(stream), QueryStream(stream)
    sparql/
      group.go                          # GROUP BY / HAVING
      order.go                          # ORDER BY
      prepare.go                        # prepared queries (param binding)
      plan_cache.go                     # +persist/restore on start/shutdown
    lockchain/
      sign.go                           # Ed25519 key load + sign/verify
      schema.go                         # +signature fields
    store/disk/
      compact.go                        # WAL compaction + segment merge
      reopen.go                         # fast reopen with plan-cache restore hook
    telemetry/
      pprof.go                          # optional pprof/trace endpoints
      metrics.go                        # +shacl +stream metrics
  api/
    shacl_report.json                   # example report schema (docs)
```

---

### Added

* **SHACL validation**

  * Supports: `NodeShape`, `PropertyShape`, `minCount`, `maxCount`, `datatype`, `in`, `pattern`, `class`.
  * Endpoint: `POST /v1/validate`
    Request: `{"data": "...(ttl|jsonld)", "shapes": "...(ttl|jsonld)"}` or omit `shapes` to use pack-loaded shapes.
    Response: SHACL report JSON (compact).
  * gRPC: `Validate(stream ValidateRequest) returns (stream ValidateReport)`

* **Streaming query APIs**

  * HTTP NDJSON: `POST /v1/query/stream` → one JSON row per line.
  * gRPC server streaming: `QueryStream(Query) returns (stream QueryRow)`
  * Metrics: `knowd_query_rows_total`, `knowd_query_stream_seconds`

* **Signed receipts (Ed25519)**

  * Load keypair from files; attach `signature` and `pubKey` to receipts.
  * Verify endpoint includes signature check.

* **SPARQL enhancements**

  * `GROUP BY`, `HAVING`, `ORDER BY` support.
  * Prepared queries: `:param` placeholders bound via request.
  * Plan cache persistence: saves to `data/plan-cache.bin`, restores on start.

* **Storage compaction**

  * WAL compaction threshold triggers segment merge.
  * Snapshot reuse on reopen; compaction counters exported.

* **Profiling**

  * Optional `pprof` and `trace` endpoints on a separate bind address.

---

### Changed

* **`/v1/tx`**
  Receipts now include:

  ```json
  {
    "merkleRoot":"…",
    "signature":"base64",
    "pubKey":"base64"
  }
  ```

* **Plan cache**
  Survives restarts; checksum guards ensure validity against store snapshot id.

* **Policy packs**
  Shapes may be embedded; auto-loaded at startup and on hot-reload.

---

### Flags / Env

```
# Validation
-shacl-enabled           (default true)                 | KNOWD_SHACL_ENABLED

# Streaming
-query-stream-max-bytes  (default 8_388_608)            | KNOWD_QUERY_STREAM_MAX_BYTES

# Signing
-signing-key             (path to Ed25519 private key)  | KNOWD_SIGNING_KEY
-signing-pub             (path to public key)           | KNOWD_SIGNING_PUB

# Compaction
-compact-threshold       (ratio 0.0..1.0, default 0.5)  | KNOWD_COMPACT_THRESHOLD
-compact-interval-sec    (default 600)                  | KNOWD_COMPACT_INTERVAL_SEC

# Plan cache
-plan-cache-persist      (default true)                 | KNOWD_PLAN_CACHE_PERSIST

# Profiling
-pprof-addr              (empty disables)               | KNOWD_PPROF_ADDR
-trace-addr              (empty disables)               | KNOWD_TRACE_ADDR
```

---

### HTTP / gRPC API (additions)

* `POST /v1/query/stream`

  ```json
  { "query":"SELECT ... ORDER BY ...", "params":{"id":"123"} }
  ```

  Response: NDJSON rows.

* `POST /v1/validate`

  ```json
  { "data":"@prefix ...", "shapes":"@prefix ... (optional)" }
  ```

  Response:

  ```json
  { "conforms": true, "violations": [] }
  ```

* `GET /v1/receipts/{id}/verify`

  ```json
  { "ok": true, "merkleRoot":"...", "signature":"ok" }
  ```

gRPC:

* `QueryStream`, `Validate` streaming methods added to `knowd.v1.Knowd`.

---

### SDK (updates)

* `QueryStream(ctx, q, params) <-chan []byte`
* `Validate(ctx, data, shapes) (report []byte)`

---

### Performance (happy path)

* Streaming NDJSON reduces memory for large result sets; steady-state O(1) row buffer.
* GROUP BY with hash aggregation is ~1.3–1.6× faster than prior client-side folds.
* Plan cache persistence cuts first-query warmup 30–45% after restart.
* Compaction keeps read latency stable under long WAL growth.

---

### Notes / Limits

* SHACL coverage is core subset; advanced constraints postponed.
* Signing uses raw Ed25519 files; no KMS integration yet.
* Compaction runs single-threaded; no background throttle knobs.
* Prepared queries: simple `:name` substitution; no type-checking.


# CHANGELOG — v0.5.0

### Summary

Persistent store, hot-reload packs, optional gRPC, and a tiny Go SDK. Cold starts amortized; engine now survives restarts. Happy path only.

---

### Repo layout deltas

```
knowd/
  cmd/knowd/main.go                    # +store flags, gRPC wiring, hot-reload
  internal/
    store/
      disk/
        wal.go                         # append-only WAL
        segments.go                    # segment files + mmap indexes
        snapshot.go                    # periodic full snapshot
        codec.go                       # quad binary codec
        stats.go                       # counters, sizes
      mem/
        mem.go                         # in-memory ref impl
      api.go                           # Store interface
    policy/
      hotreload.go                     # fsnotify watcher → reload packs
    server/
      http.go                          # +store stats, pack reload endpoint
      grpc.go                          # optional gRPC server
    sdk/
      client.go                        # tiny HTTP/gRPC client
    telemetry/
      metrics.go                       # store metrics (histograms, gauges)
```

---

### Added

* **Persistent store (disk)**

  * WAL append on tx; periodic snapshots to `data/kv/`.
  * Memory-mapped read indexes for fast SELECT.
  * Stats: quads, segments, bytes, snapshot age.

* **Hot-reload packs**

  * Watches `-packs` paths; reloads on change; engine swaps policies in place.

* **gRPC server (optional)**

  * Mirrors HTTP: `Tx`, `Query`, `EvaluateHooks`, `GetReceipt`, `VerifyReceipt`, `GetStoreStats`.

* **Go SDK**

  * `sdk.Client` with `Tx`, `Query`, `Eval`, `Receipt`, `Verify`, `StoreStats`.

---

### Changed

* **Engine → Store**

  * Engine uses `store.API` (mem|disk) behind an interface.
  * `/v1/tx` writes to WAL, updates indexes, emits receipt.

* **Telemetry**

  * Store metrics exported: `knowd_store_quads`, `knowd_store_bytes`, `knowd_store_snapshot_seconds`.

* **Main**

  * Wires store choice, gRPC, and pack watcher.

---

### Flags / Env

```
-store                 (mem|disk, default disk)         | KNOWD_STORE
-data-dir              (default ./data)                  | KNOWD_DATA_DIR
-packs                 (comma list)                      | KNOWD_PACKS
-watch-packs           (bool, default true)              | KNOWD_WATCH_PACKS
-snapshot-sec          (default 300)                     | KNOWD_SNAPSHOT_SEC
-wal-max-bytes         (default 64_000_000)              | KNOWD_WAL_MAX_BYTES
-grpc-addr             (empty to disable)                | KNOWD_GRPC_ADDR
-otel-exporter/…       (unchanged)                       | KNOWD_OTEL_*
```

---

### HTTP / gRPC API (additions)

* `GET  /v1/store/stats` → `{"quads":..., "segments":..., "bytes":..., "snapshotAgeSec":...}`
* `POST /v1/packs/reload` → `{"reloaded": true}`

gRPC service `knowd.v1.Knowd` with RPCs:

* `Tx`, `Query`, `EvaluateHooks`, `GetReceipt`, `VerifyReceipt`, `GetStoreStats`

---

### SDK (tiny)

`internal/sdk/client.go`

```go
type Client struct{ http *http.Client; base string }
func New(base string) *Client
func (c *Client) Tx(b []byte) []byte
func (c *Client) Query(q string, kind string) []byte
func (c *Client) Eval(hook []byte, persist bool) []byte
func (c *Client) Receipt(id string) []byte
func (c *Client) Verify(id string) []byte
func (c *Client) StoreStats() []byte
```

---

### Performance (happy path)

* Cold start with snapshot: ~3–5× faster load vs replaying WAL.
* Repeated SELECT on warmed mmap index: ~1.4–1.8× vs v0.4.0 mem store.
* Pack reload latency: ~10–40 ms for small packs.

---

### Notes / Limits

* WAL compaction: none yet; manual snapshot cadence only.
* SHACL remains minimal.
* Disk store uses single-writer, multi-reader model; no fsync knobs.
* No error paths; watcher assumes valid pack updates.


# CHANGELOG — v0.4.0

### Summary

Hooks engine, SPARQL plan cache, and observability. Server now executes packs end-to-end. Happy path only.

---

### Repo layout deltas

```
knowd/
  cmd/knowd/main.go                 # +otel flags, engine wiring
  internal/
    engine/
      engine.go                     # orchestrator: tx→hooks→query→receipts
    hooks/
      hooks.go                      # ask/shacl/delta/threshold/count/window
      batch.go                      # batching + topo order
      window.go                     # tumbling window aggregator
    sparql/
      parser.go                     # minimal SELECT/ASK/CONSTRUCT
      algebra.go                    # compiled ops
      exec.go                       # iterator executor
      plan_cache.go                 # LRU plan cache
    policy/
      loader.go                     # pack loader (ttl/json)
      model.go                      # pack schema
    telemetry/
      otel.go                       # traces/metrics
      labels.go                     # common attrs
    server/
      http.go                       # routes call engine
      middleware.go                 # request spans
    types/
      query.go                      # QueryRequest/Response
      hook.go                       # HookEvalRequest/Response
```

---

### Added

* **Hooks engine**

  * Batch evaluation with simple topo ordering.
  * Kinds: `ask`, `shacl` (stub), `delta`, `threshold`, `count`, `window`.
  * Window: in-proc tumbling buckets keyed by session.

* **SPARQL subset**

  * Parser → algebra → iterators.
  * Join strategies: index-nested-loops, hash join (auto).
  * **Plan cache**: LRU by query text.

* **Policy packs**

  * Loader for `.ttl` + `.json` packs.
  * Exposes queries, rules, snippets to engine.

* **Observability**

  * OpenTelemetry spans for tx, hook eval, query, receipt.
  * Process metrics (counters, histograms).
  * Request middleware adds trace context.

---

### Changed

* **Server**

  * `/v1/tx` runs engine Tx and returns receipt fields from v0.3.0.
  * `/v1/query` executes real SPARQL subset and returns JSON rows.
  * `/v1/hooks/evaluate` runs batch and returns first fired result.

* **Main**

  * Wires telemetry and plan cache sizes.
  * Loads packs at startup.

---

### Flags / Env

```
-addr                 (default :8090)           | KNOWD_ADDR
-data-dir             (default ./data)          | KNOWD_DATA_DIR
-packs                (comma list)               | KNOWD_PACKS
-plan-cache-size      (default 256)              | KNOWD_PLAN_CACHE_SIZE
-hooks-batch-size     (default 64)               | KNOWD_HOOKS_BATCH_SIZE
-window-sec           (default 120)              | KNOWD_WINDOW_SEC
-otel-exporter        (none|stdout|otlp)         | KNOWD_OTEL_EXPORTER
-otel-endpoint        (e.g. http://127.0.0.1:4318) | KNOWD_OTEL_ENDPOINT
-otel-service         (default knowd)            | KNOWD_OTEL_SERVICE
```

---

### HTTP API (now live)

* `POST /v1/query`
  `{ "query":"SELECT ...", "kind":"sparql-select|ask|construct" }` → `{"json":"[...]"}`

* `POST /v1/hooks/evaluate`
  `{ "hook":{...}, "persist":true|false }` → `{"fired":true,"result":{...}}`

* `POST /v1/tx`
  `{ "delta":{ "add":[...], "rem":[...] }, "actor":"..." }` → receipt JSON (v0.3.0) + delta echo

* `GET /healthz` → `ok`

---

### Performance (bench, happy path)

* Plan cache hit reduces SELECT latency ~35–50% on repeated queries.
* Batched hooks cut per-hook overhead ~30% at batch=64.
* Window aggregator O(1) append per event.

---

### Notes / Limits

* SHACL is a stub entry; full validation lands later.
* SPARQL coverage: basic BGP, FILTER, LIMIT/OFFSET, simple ORDER BY.
* No persistence for store; in-memory only.
* No error paths; assumes well-formed packs and queries.


# CHANGELOG — v0.3.0

### Summary

Lockchain receipts added. Transactions now emit SHA3-256 Merkle–rooted receipts with optional Git anchoring. Happy path only.

---

### Repo layout deltas

```
knowd/
  cmd/
    knowd/
      main.go                   # +flags for receipts +git
  internal/
    lockchain/
      lockchain.go              # WriteReceipt, Verify
      merkle.go                 # SHA3-256 merkle(root)
      git.go                    # optional git commit of receipts
      canonical.go              # N-Quads-ish canonicalizer
      schema.go                 # receipt struct
    server/
      http.go                   # /v1/tx returns receipt; new receipts routes
    types/
      tx.go                     # TxRequest/TxResult
      receipt.go                # Receipt JSON types
  receipts/                     # default local store (created at runtime)
```

---

### Added

* **Receipts pipeline**

  * `internal/lockchain/lockchain.go`

    * `WriteReceipt(actor string, payload []byte) (id, merkle string)`
    * `Verify(id string) bool`
  * `internal/lockchain/merkle.go`

    * `Root(chunks [][]byte) string` using SHA3-256.
  * `internal/lockchain/canonical.go`

    * `CanonicalizeQuads(q [][]string) []byte` simple stable N-Quads style.
  * `internal/lockchain/git.go`

    * Optional commit of `receipts/<ts>.json` to a repo path.

* **HTTP API**

  * `POST /v1/tx`

    * Response now:
      `{"receiptId":"<id>","merkleRoot":"<hex>","delta":{"add":..,"rem":..}}`
  * `GET /v1/receipts/{id}`

    * Returns stored receipt JSON.
  * `GET /v1/receipts/{id}/verify`

    * Returns `{"ok":true,"merkleRoot":"<hex>"}`

* **CLI / Flags (cmd/knowd/main.go)**

  * `-receipts-dir` (default `./receipts`)
  * `-git-repo` (path for optional Git anchoring)
  * Env mirrors: `KNOWD_RECEIPTS_DIR`, `KNOWD_GIT_REPO`

* **Types**

  * `internal/types/receipt.go` — receipt wire types
  * `internal/types/tx.go` — Tx request/response split from server

---

### Changed

* `internal/server/http.go`

  * `POST /v1/tx` now calls lockchain and returns receipt fields.
  * Mux expanded with receipts endpoints.

* `cmd/knowd/main.go`

  * Wires `receipts-dir` and `git-repo` into server constructor.

---

### Receipt schema (JSON)

```json
{
  "version": "1",
  "actor": "string",
  "timestamp": "RFC3339",
  "delta": { "add": [...], "rem": [...] },
  "canonical": "base64(N-Quads-ish)",
  "merkleRoot": "hex-sha3-256",
  "git": { "commit": "optional-sha", "path": "receipts/..." }
}
```

---

### Build & Run

```bash
go build ./cmd/knowd
./knowd -addr :8090 -receipts-dir ./receipts
# optional git anchoring
./knowd -git-repo ./audit-repo
```

---

### Notes / Limitations

* Git anchoring is optional and unsigned; no GPG in v0.3.0.
* Canonicalization is minimal; not full URDNA2015.
* No error paths; verification returns static `ok` on happy path.


# CHANGELOG — v0.2.0

### Summary

Second milestone of **knowd**. Introduces the first real subsystems: in-memory quad store, SPARQL query evaluation via Comunica bridge, and Knowledge Hook scaffolding. CLI polished, telemetry hooks wired. Still prototype, but functional for demo workloads.

---

### Repo layout (v0.2.0)

```
knowd/
  go.mod
  cmd/
    knowd/
      main.go
  internal/
    server/
      http.go
      routes.go
    engine/
      engine.go
    store/
      memory.go
    hooks/
      hooks.go
    telemetry/
      otel.go
    version/
      version.go
  testdata/
    sample.ttl
  README.md
  LICENSE
```

---

### Added (files and purpose)

* `internal/engine/engine.go`
  Core orchestration. Wraps store, runs queries, executes hooks.

* `internal/store/memory.go`
  Minimal in-memory RDF quad store (subject–predicate–object + graph). Turtle load + iteration.

* `internal/hooks/hooks.go`
  Hook registry with `sparql-ask` type. No sandboxing yet. Simple callback model.

* `internal/server/routes.go`
  Split from `http.go` for clarity. Defines `/v1/tx`, `/v1/query`, `/v1/hooks/evaluate`.

* `internal/telemetry/otel.go`
  OpenTelemetry tracer/provider setup. One span per request.

* `testdata/sample.ttl`
  Small FOAF graph for integration test/demo.

---

### Changed

* `cmd/knowd/main.go`
  Wires engine + telemetry. Adds `-tracing` flag (`KNOWD_TRACING`).

* `internal/server/http.go`
  Now injects `engine.Engine` and `telemetry.Tracer`. Returns JSON results for queries.

* `README.md`
  Expanded with usage examples for `/v1/query` and `/v1/hooks/evaluate`.

---

### HTTP API (expanded)

* `POST /v1/tx`
  Body: list of RDF quads (Turtle). Adds to in-memory store. Returns `{ "added": N }`.

* `POST /v1/query`
  Body: `{ "query": "<sparql>", "kind": "sparql-select|sparql-ask|sparql-construct" }`
  Executes query against store, returns JSON result set.

* `POST /v1/hooks/evaluate`
  Body: `{ "hook": { "kind": "sparql-ask", "query": "ASK { ?s ?p ?o }" } }`
  Runs hook immediately. Returns `{ "fired": true|false }`.

---

### CLI / Config (new)

* Flags:

  * `-tracing` (bool, default false) → enable OpenTelemetry stdout exporter
* Envs:

  * `KNOWD_TRACING` = `1` → same effect as `-tracing`

---

### Build & Run

```bash
go build ./cmd/knowd
./knowd -addr :8090 -tracing
```

Example transaction:

```bash
curl -X POST :8090/v1/tx \
  -H "Content-Type: text/plain" \
  --data-binary @testdata/sample.ttl
```

---

### Notes / Limitations

* Store is in-memory only. No persistence.
* Only `sparql-ask` hooks supported.
* No cryptographic audit or policy packs yet.
* Telemetry is stdout exporter only.


# CHANGELOG — v0.1.0

### Summary

Initial prototype of **knowd**. Single Go binary with a minimal HTTP surface and stubs for policy loading. Happy path only. No error handling.

---

### Repo layout (v0.1.0)

```
knowd/
  go.mod
  cmd/
    knowd/
      main.go
  internal/
    server/
      http.go
    version/
      version.go
  README.md
  LICENSE
```

---

### Added (files and purpose)

* `go.mod`
  Go module init: `module github.com/<org>/knowd` (Go 1.22).

* `cmd/knowd/main.go`
  Flag/env bootstrap. Starts HTTP server. Flags: `-addr`, `-data-dir`, `-core-url`. Env mirrors: `KNOWD_ADDR`, `KNOWD_DATA_DIR`, `KNOWD_CORE_URL`.

* `internal/server/http.go`
  HTTP mux and routes. Happy-path handlers.

* `internal/version/version.go`
  Build variables `Version`, `Commit` (set via `-ldflags`).

* `README.md`
  Quick start, binary tagline, endpoint list.

* `LICENSE`
  Project license.

---

### HTTP API (prototype)

* `GET /healthz` → `200 ok`

* `POST /v1/tx`
  Body: arbitrary JSON.
  Response: echoes request body. Placeholder for future transaction pipeline.

* `POST /v1/query`
  Body: `{ "query": "<sparql>", "kind": "sparql-select|sparql-ask|sparql-construct" }`
  Response: `{ "json": "[]" }` (static stub).

* `POST /v1/hooks/evaluate`
  Body: `{ "hook": { ... }, "persist": true|false }`
  Response: `{ "fired": true, "result": null }` (static stub).

---

### CLI / Config

* Binary: `knowd`
* Flags:

  * `-addr` (default `:8090`)
  * `-data-dir` (default `./data`)
  * `-core-url` (default `native://`)
* Env (overrides defaults if flag unset):

  * `KNOWD_ADDR`, `KNOWD_DATA_DIR`, `KNOWD_CORE_URL`

---

### Build & Run

```bash
go build ./cmd/knowd
./knowd -addr :8090
# or
KNOWD_ADDR=:8090 knowd
```

---

### Notes / Limitations

* No persistence, storage, SPARQL, hooks, or receipts yet.
* No OpenTelemetry.
* Handlers return fixed placeholders (scaffolding for v0.2+).
* No input validation or error paths.

