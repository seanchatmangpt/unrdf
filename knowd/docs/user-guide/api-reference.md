# API Reference

Knowd provides both HTTP REST APIs and gRPC APIs for interacting with the knowledge graph. This reference documents all available endpoints, their request/response formats, and usage examples.

## Base URLs

- **HTTP API**: `http://localhost:8090`
- **gRPC API**: `localhost:9090` (when enabled)

## Authentication

### HTTP Headers

For endpoints that support authentication, include the following headers:

```
X-KNOWD-NS: <namespace>          # Optional: Specify namespace
X-KNOWD-TOKEN: <token>          # Optional: HMAC token for authentication
```

### mTLS (for cluster mode)

In cluster deployments, mTLS is required for secure communication between nodes.

## HTTP API Endpoints

### Health and Status

#### GET /healthz

**Health check endpoint.**

**Response:**
```json
"ok"
```

**Status Codes:**
- `200` - Server is healthy

---

### Version Information

#### GET /version

**Get server version and build information.**

**Response:**
```json
{
  "version": "v1.0.0",
  "commit": "abc123def456...",
  "buildDate": "2025-01-01T00:00:00Z"
}
```

**Status Codes:**
- `200` - Success

---

### Core Operations

#### POST /v1/tx

**Submit a transaction to add or remove RDF data.**

**Request Body:**
```json
{
  "delta": {
    "add": [
      {
        "subject": "<http://example.org/s1>",
        "predicate": "<http://example.org/p1>",
        "object": "<http://example.org/o1>",
        "graph": "default"
      }
    ],
    "rem": [
      {
        "subject": "<http://example.org/s2>",
        "predicate": "<http://example.org/p2>",
        "object": "<http://example.org/o2>",
        "graph": "default"
      }
    ]
  },
  "actor": "user@example.com"
}
```

**Response:**
```json
{
  "receiptId": "tx-receipt-uuid",
  "added": 1,
  "removed": 1
}
```

**Status Codes:**
- `200` - Transaction processed successfully
- `400` - Invalid request format
- `500` - Internal server error

---

#### POST /v1/query

**Execute a SPARQL query.**

**Request Body:**
```json
{
  "query": "SELECT ?name WHERE { ?person foaf:name ?name }",
  "kind": "sparql-select",
  "params": {
    "limit": 10
  }
}
```

**Response:**
```json
{
  "rows": [
    {"name": "Alice"},
    {"name": "Bob"}
  ],
  "kind": "sparql-select",
  "stats": {
    "executionTimeMs": 15,
    "rowsReturned": 2,
    "bytesProcessed": 1024
  }
}
```

**Query Kinds:**
- `sparql-select` - SELECT queries
- `sparql-ask` - ASK queries (returns boolean)
- `sparql-construct` - CONSTRUCT queries

**Status Codes:**
- `200` - Query executed successfully
- `400` - Invalid query syntax or parameters
- `500` - Query execution error

---

#### POST /v1/query/stream

**Execute a SPARQL query with streaming results (NDJSON format).**

**Request Body:** Same as `/v1/query`

**Response:** NDJSON stream
```json
{"name": "Alice"}
{"name": "Bob"}
```

**Final trailer:** Contains execution statistics
```json
{
  "stats": {
    "executionTimeMs": 15,
    "rowsReturned": 2,
    "bytesProcessed": 1024
  }
}
```

**Status Codes:**
- `200` - Query executed successfully
- `400` - Invalid query syntax or parameters
- `500` - Query execution error

---

#### POST /v1/query/at

**Execute a SPARQL query at a specific point in time.**

**Request Body:**
```json
{
  "query": "SELECT ?name WHERE { ?person foaf:name ?name }",
  "kind": "sparql-select",
  "at": "2025-01-01T12:00:00Z"
}
```

**Response:** Same as `/v1/query` but for historical data

**Status Codes:**
- `200` - Query executed successfully
- `400` - Invalid timestamp or query
- `404` - No data available at specified time

---

### SHACL Validation

#### POST /v1/validate

**Validate RDF data against SHACL shapes.**

**Request Body:**
```json
{
  "data": "@prefix ex: <http://example.org/> . ex:person a ex:Person .",
  "shapes": "@prefix sh: <http://www.w3.org/ns/shacl#> . ex:PersonShape a sh:NodeShape .",
  "format": "turtle"
}
```

**Response:**
```json
{
  "conforms": true,
  "violations": []
}
```

**Or when violations exist:**
```json
{
  "conforms": false,
  "violations": [
    {
      "focusNode": "<http://example.org/person>",
      "property": "<http://example.org/name>",
      "constraint": "sh:minCount",
      "message": "Missing required property"
    }
  ]
}
```

**Status Codes:**
- `200` - Validation completed
- `400` - Invalid data or shapes format

---

### Hook Management

#### POST /v1/hooks/evaluate

**Evaluate registered hooks against current data.**

**Request Body:**
```json
{
  "hook": {
    "id": "alert-hook",
    "type": "sparql-ask",
    "query": "ASK WHERE { ?person ex:riskLevel \"high\" }"
  },
  "persist": true
}
```

**Response:**
```json
{
  "fired": true,
  "result": {
    "hookId": "alert-hook",
    "data": {
      "alertTriggered": true,
      "riskyPersons": 3
    }
  }
}
```

**Status Codes:**
- `200` - Hook evaluation completed
- `400` - Invalid hook definition

---

### Receipt Management

#### GET /v1/receipts/{id}

**Retrieve a specific receipt by ID.**

**Response:**
```json
{
  "id": "tx-receipt-uuid",
  "actor": "user@example.com",
  "timestamp": "2025-01-01T12:00:00Z",
  "merkleRoot": "abc123...",
  "signature": "base64-encoded-signature",
  "delta": {
    "add": [...],
    "rem": [...]
  }
}
```

**Status Codes:**
- `200` - Receipt found
- `404` - Receipt not found

---

#### GET /v1/receipts/{id}/verify

**Verify the integrity of a receipt.**

**Response:**
```json
{
  "ok": true,
  "merkleRoot": "abc123...",
  "signatureVerified": true,
  "canonicalForm": "N-Quads canonical form..."
}
```

**Status Codes:**
- `200` - Receipt verified successfully
- `400` - Invalid receipt format
- `404` - Receipt not found

---

#### GET /v1/receipts/search

**Search for receipts using filters.**

**Query Parameters:**
- `actor` - Filter by actor
- `since` - Start timestamp (RFC3339)
- `until` - End timestamp (RFC3339)
- `limit` - Maximum results (default: 100)
- `offset` - Pagination offset

**Example:**
```
GET /v1/receipts/search?actor=user@example.com&since=2025-01-01T00:00:00Z&limit=50
```

**Response:**
```json
{
  "receipts": [
    {
      "id": "tx-receipt-uuid",
      "actor": "user@example.com",
      "timestamp": "2025-01-01T12:00:00Z",
      "deltaSummary": {
        "added": 5,
        "removed": 2
      }
    }
  ],
  "total": 150,
  "limit": 50,
  "offset": 0
}
```

**Status Codes:**
- `200` - Search completed successfully
- `400` - Invalid query parameters

---

### Store Management

#### GET /v1/store/stats

**Get storage statistics and health metrics.**

**Response:**
```json
{
  "quads": 15420,
  "segments": 8,
  "bytes": 1048576,
  "snapshotAgeSec": 300,
  "walSize": 67108864,
  "compactionRatio": 0.75
}
```

**Status Codes:**
- `200` - Statistics retrieved successfully

---

### Policy Management

#### POST /v1/packs/reload

**Reload policy packs from disk.**

**Request Body:**
```json
{
  "paths": ["/etc/knowd/policies/", "/opt/knowd/custom-policies/"]
}
```

**Response:**
```json
{
  "reloaded": true,
  "packsLoaded": 5,
  "errors": []
}
```

**Status Codes:**
- `200` - Packs reloaded successfully
- `400` - Invalid request format
- `500` - Failed to reload packs

---

### Namespace Management

#### POST /v1/admin/namespaces

**Create a new namespace.**

**Request Body:**
```json
{
  "name": "team-alpha",
  "description": "Alpha team knowledge graph",
  "config": {
    "quotaQps": 100,
    "quotaRowsps": 5000
  }
}
```

**Response:**
```json
{
  "name": "team-alpha",
  "description": "Alpha team knowledge graph",
  "created": "2025-01-01T12:00:00Z",
  "config": {
    "quotaQps": 100,
    "quotaRowsps": 5000
  }
}
```

**Status Codes:**
- `201` - Namespace created successfully
- `400` - Invalid namespace configuration
- `409` - Namespace already exists

---

#### GET /v1/admin/namespaces

**List all namespaces.**

**Response:**
```json
{
  "namespaces": [
    {
      "name": "default",
      "description": "Default namespace",
      "created": "2025-01-01T00:00:00Z",
      "quads": 15420
    },
    {
      "name": "team-alpha",
      "description": "Alpha team knowledge graph",
      "created": "2025-01-01T12:00:00Z",
      "quads": 2340
    }
  ]
}
```

**Status Codes:**
- `200` - Namespaces retrieved successfully

---

### Policy Rollouts

#### POST /v1/admin/rollout

**Configure policy rollout for a namespace.**

**Request Body:**
```json
{
  "namespace": "production",
  "stable": "v1.2.0",
  "canary": "v1.3.0",
  "percent": 10
}
```

**Response:**
```json
{
  "namespace": "production",
  "stable": "v1.2.0",
  "canary": "v1.3.0",
  "percent": 10,
  "updated": "2025-01-01T12:00:00Z"
}
```

**Status Codes:**
- `200` - Rollout configured successfully
- `400` - Invalid rollout configuration

---

#### GET /v1/admin/rollout

**Get current rollout configuration for a namespace.**

**Query Parameters:**
- `ns` - Namespace name

**Response:** Same as POST response format

**Status Codes:**
- `200` - Rollout configuration retrieved
- `404` - No rollout configured for namespace

---

### Clustering Operations

#### GET /v1/cluster/status

**Get cluster status and health information.**

**Response:**
```json
{
  "mode": "leader",
  "followers": 2,
  "lagSec": 0.5,
  "lastSnapshot": "2025-01-01T12:00:00Z",
  "replicationRate": 1500
}
```

**Status Codes:**
- `200` - Cluster status retrieved

---

#### POST /v1/admin/promote-follower

**Promote a follower node to leader (manual failover).**

**Request Body:**
```json
{
  "followerAddr": "follower2:8090",
  "force": false
}
```

**Response:**
```json
{
  "promoted": true,
  "newLeader": "follower2:8090",
  "previousLeader": "leader1:8090"
}
```

**Status Codes:**
- `200` - Promotion initiated
- `400` - Invalid follower address
- `409` - Promotion already in progress

---

### Vector Operations

#### POST /v1/similar

**Find similar items using vector search.**

**Request Body:**
```json
{
  "text": "machine learning algorithms",
  "topK": 5,
  "namespace": "documents"
}
```

**Response:**
```json
{
  "items": [
    {
      "id": "doc-123",
      "score": 0.89,
      "metadata": {
        "title": "ML Algorithms Overview",
        "category": "technical"
      }
    },
    {
      "id": "doc-456",
      "score": 0.76,
      "metadata": {
        "title": "Deep Learning Guide",
        "category": "tutorial"
      }
    }
  ]
}
```

**Status Codes:**
- `200` - Similar items found
- `400` - Invalid request parameters

---

#### POST /v1/vector/upsert

**Add or update vector embeddings for documents.**

**Request Body:**
```json
{
  "id": "doc-123",
  "text": "This is a document about machine learning",
  "metadata": {
    "title": "ML Overview",
    "category": "technical"
  }
}
```

**Response:**
```json
{
  "id": "doc-123",
  "indexed": true,
  "vectorDimensions": 768
}
```

**Status Codes:**
- `200` - Vector indexed successfully
- `400` - Invalid document format

---

### Analytics and Monitoring

#### POST /v1/admin/analyze

**Analyze query patterns and generate statistics.**

**Request Body:**
```json
{
  "namespace": "production",
  "sampleSize": 10000,
  "includeHistograms": true
}
```

**Response:**
```json
{
  "statsVersion": "v1.2.3",
  "tables": [
    {
      "name": "main_graph",
      "rows": 15420,
      "columns": ["subject", "predicate", "object"],
      "cardinality": {
        "subject": 2340,
        "predicate": 45,
        "object": 8765
      }
    }
  ],
  "histograms": {
    "predicate_frequency": [...]
  }
}
```

**Status Codes:**
- `200` - Analysis completed
- `400` - Invalid analysis parameters

---

#### GET /v1/admin/views/status

**Get status of materialized views.**

**Query Parameters:**
- `ns` - Namespace name

**Response:**
```json
{
  "namespace": "production",
  "views": [
    {
      "name": "person_summary",
      "definition": "CONSTRUCT { ?person ex:summary ?summary } WHERE { ... }",
      "lastRefresh": "2025-01-01T11:30:00Z",
      "refreshDurationMs": 2500,
      "status": "ready"
    }
  ]
}
```

**Status Codes:**
- `200` - View status retrieved

---

#### POST /v1/admin/views/build

**Build or refresh materialized views.**

**Request Body:**
```json
{
  "namespace": "production",
  "names": ["person_summary", "org_chart"],
  "force": false
}
```

**Response:**
```json
{
  "namespace": "production",
  "built": ["person_summary", "org_chart"],
  "skipped": [],
  "errors": []
}
```

**Status Codes:**
- `200` - Views built successfully
- `400` - Invalid view names or parameters

---

### Quota Management

#### POST /v1/admin/quotas

**Set quotas for a namespace.**

**Request Body:**
```json
{
  "namespace": "team-alpha",
  "qps": 100,
  "rowsps": 5000
}
```

**Response:**
```json
{
  "namespace": "team-alpha",
  "qps": 100,
  "rowsps": 5000,
  "applied": "2025-01-01T12:00:00Z"
}
```

**Status Codes:**
- `200` - Quotas applied successfully
- `400` - Invalid quota values

---

## gRPC API

Knowd also provides a gRPC API that mirrors the HTTP endpoints. The service definition is available in `api/proto/knowd/v1/knowd.proto`.

### Service: `knowd.v1.Knowd`

#### RPC Methods

- `Tx(TxRequest) returns (TxResponse)` - Submit transactions
- `Query(QueryRequest) returns (QueryResponse)` - Execute SPARQL queries
- `QueryStream(QueryRequest) returns (stream QueryRow)` - Stream query results
- `QueryAt(QueryAtRequest) returns (QueryResponse)` - Time-travel queries
- `Validate(ValidateRequest) returns (ValidateResponse)` - SHACL validation
- `ValidateStream(ValidateRequest) returns (stream ValidateResult)` - Stream validation results
- `EvaluateHooks(HookEvalRequest) returns (HookEvalResponse)` - Evaluate hooks
- `GetReceipt(GetReceiptRequest) returns (Receipt)` - Retrieve receipts
- `VerifyReceipt(VerifyReceiptRequest) returns (VerifyReceiptResponse)` - Verify receipts
- `SearchReceipts(SearchReceiptsRequest) returns (SearchReceiptsResponse)` - Search receipts
- `GetStoreStats(GetStoreStatsRequest) returns (StoreStats)` - Get storage statistics

### Protocol Buffers

The complete protocol buffer definitions are available in the `api/proto/` directory.

## Error Responses

All endpoints return structured error responses when applicable:

```json
{
  "error": "ErrorType",
  "message": "Human-readable error description",
  "code": 400,
  "details": {
    "field": "query",
    "issue": "Invalid SPARQL syntax"
  }
}
```

## Common Response Headers

- `Content-Type: application/json` - JSON responses
- `Content-Type: application/x-ndjson` - Streaming responses
- `X-KNOWD-Version: v1.0.0` - Server version
- `X-Request-ID: uuid` - Request correlation ID

## Rate Limiting

Knowd supports configurable rate limiting:

- **Queries per second** (`quota-qps`)
- **Rows per second** (`quota-rowsps`)

When limits are exceeded, responses include:
```json
{
  "error": "RateLimited",
  "message": "Rate limit exceeded",
  "retryAfter": 60
}
```

## Examples

### Complete Workflow Example

```bash
# 1. Create namespace
curl -X POST http://localhost:8090/v1/admin/namespaces \
  -H "Content-Type: application/json" \
  -d '{"name": "demo", "description": "Demo namespace"}'

# 2. Add data
curl -X POST http://localhost:8090/v1/tx \
  -H "Content-Type: application/json" \
  -H "X-KNOWD-NS: demo" \
  -d '{
    "delta": {
      "add": [
        {
          "subject": "<http://example.org/alice>",
          "predicate": "<http://example.org/knows>",
          "object": "<http://example.org/bob>",
          "graph": "default"
        }
      ]
    },
    "actor": "admin@example.com"
  }'

# 3. Query data
curl -X POST http://localhost:8090/v1/query \
  -H "Content-Type: application/json" \
  -H "X-KNOWD-NS: demo" \
  -d '{
    "query": "SELECT ?p ?o WHERE { <http://example.org/alice> ?p ?o }",
    "kind": "sparql-select"
  }'

# 4. Get receipt
RECEIPT_ID=$(curl -s http://localhost:8090/v1/tx \
  -H "X-KNOWD-NS: demo" \
  -d '{"delta": {"add": []}, "actor": "admin"}' | jq -r '.receiptId')

curl http://localhost:8090/v1/receipts/$RECEIPT_ID/verify
```

This demonstrates the complete workflow from namespace creation to transaction verification.
