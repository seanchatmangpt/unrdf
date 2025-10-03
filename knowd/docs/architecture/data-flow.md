# Data Flow Architecture

This document describes how data flows through the Knowd system, from client requests to storage and back.

## High-Level Data Flow

```
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   Client/API    │───▶│  HTTP/gRPC       │───▶│   Namespace     │
│   Layer         │    │  Server          │    │   Middleware    │
└─────────────────┘    └──────────────────┘    └─────────────────┘
                                                           │
                                                           ▼
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│ Authentication  │───▶│  Request         │───▶│   Lockchain     │
│ & Authorization │    │  Validation      │    │   (Receipts)    │
└─────────────────┘    └──────────────────┘    └─────────────────┘
                                                           │
                                                           ▼
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   Hooks Engine  │◀───┤  Transaction     │───▶│   RDF Store     │
│   (Validation)  │    │  Processing      │    │   (Persistent)  │
└─────────────────┘    └──────────────────┘    └─────────────────┘
                                                           │
                                                           ▼
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   SPARQL        │◀───┤  Query           │───▶│   Plan Cache    │
│   Engine        │    │  Execution       │    │   (LRU)         │
└─────────────────┘    └──────────────────┘    └─────────────────┘
                                                           │
                                                           ▼
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   Vector        │◀───┤  Similarity      │───▶│   HNSW Index    │
│   Search        │    │  Search          │    │   (In-Memory)   │
└─────────────────┘    └──────────────────┘    └─────────────────┘
```

## Detailed Component Flows

### 1. Transaction Processing Flow

**Transaction Request → Response**

```
HTTP POST /v1/tx
    ↓
Request Validation (JSON Schema)
    ↓
Namespace Resolution (X-KNOWD-NS header)
    ↓
Authentication Check (mTLS/JWT)
    ↓
Transaction Processing:
    ├── Delta Application → RDF Store
    ├── Hook Evaluation → Policy Engine
    ├── SHACL Validation → Constraint Engine
    └── Receipt Generation → Lockchain
    ↓
Response Generation (Receipt + Delta)
```

**Key Components Involved:**
- `internal/server/http.go` - HTTP endpoint handling
- `internal/namespace/middleware.go` - Namespace resolution
- `internal/auth/mtls.go` - Authentication
- `internal/engine/engine.go` - Transaction orchestration
- `internal/store/` - Data persistence
- `internal/hooks/` - Policy evaluation
- `internal/shacl/` - Validation
- `internal/lockchain/` - Receipt generation

### 2. Query Execution Flow

**SPARQL Query → Results**

```
HTTP POST /v1/query
    ↓
Request Parsing (JSON → Query struct)
    ↓
Namespace Resolution
    ↓
Query Processing:
    ├── Query Parsing → SPARQL Parser
    ├── Plan Generation → Algebra Compiler
    ├── Optimization → Cost-Based Optimizer
    ├── Plan Caching → LRU Cache
    └── Execution → Iterator Engine
    ↓
Results Streaming (NDJSON for large datasets)
```

**Key Components Involved:**
- `internal/server/http.go` - Query endpoint
- `internal/sparql/parser.go` - Query parsing
- `internal/sparql/algebra.go` - Plan compilation
- `internal/sparql/cbo.go` - Query optimization
- `internal/sparql/plan_cache.go` - Caching
- `internal/sparql/exec.go` - Query execution

### 3. Hook Evaluation Flow

**Transaction → Hook Execution**

```
Transaction Commit
    ↓
Hook Trigger Detection
    ↓
Hook Batch Assembly (Parallel Independent)
    ↓
Hook Execution:
    ├── SPARQL-ASK → Boolean evaluation
    ├── SHACL → Validation constraints
    ├── Delta → Change detection
    ├── Threshold → Numeric monitoring
    ├── Count → Cardinality checks
    └── Window → Time-based aggregation
    ↓
Hook Results Processing
```

**Key Components Involved:**
- `internal/engine/engine.go` - Hook triggering
- `internal/hooks/batch.go` - Parallel execution
- `internal/hooks/hooks.go` - Hook definitions
- `internal/sparql/` - SPARQL-based hooks
- `internal/shacl/` - Validation hooks

### 4. Cluster Replication Flow

**Leader → Followers**

```
Leader Node:
    ├── Transaction Processing
    ├── WAL Append → Write-Ahead Log
    └── Snapshot Generation → Periodic Full State
        ↓
Follower Nodes:
    ├── WAL Replication → gRPC Streaming
    ├── State Application → Consistent Updates
    └── Query Serving → Read-Only Operations
        ↓
Federated Queries → Scatter/Gather Pattern
```

**Key Components Involved:**
- `internal/cluster/leader.go` - Write coordination
- `internal/cluster/follower.go` - Read scaling
- `internal/cluster/rpc.go` - gRPC communication
- `internal/cluster/scattergather.go` - Query federation
- `internal/store/disk/wal.go` - WAL management

### 5. Vector Search Flow

**Similarity Query → Results**

```
Similarity Search Request
    ↓
Namespace Resolution
    ↓
Vector Index Lookup (HNSW)
    ↓
Embedding Generation (if needed)
    ↓
K-Nearest Neighbor Search
    ↓
Distance Calculation & Ranking
    ↓
Results Assembly & Filtering
    ↓
Response Generation
```

**Key Components Involved:**
- `internal/vec/index.go` - Index management
- `internal/vec/hnsw.go` - Search algorithm
- `internal/vec/embed.go` - Embedding generation
- `internal/namespace/` - Namespace isolation

## Performance Characteristics

### Latency Breakdown

**Transaction Commit (p95 < 500ms):**
- RDF Store Write: 100ms
- Hook Evaluation: 200ms
- SHACL Validation: 150ms
- Receipt Generation: 50ms

**SPARQL Query (p95 < 300ms):**
- Query Parsing: 10ms
- Plan Optimization: 50ms
- Cache Lookup: 5ms
- Execution: 200ms
- Serialization: 35ms

**Vector Search (p95 < 5ms):**
- Index Lookup: 1ms
- Distance Calculation: 3ms
- Result Assembly: 1ms

### Throughput Metrics

**Single Node:**
- Transactions/sec: 100+
- Queries/sec: 500+
- Vector Searches/sec: 1000+

**Clustered (3 nodes):**
- Read Queries/sec: 1500+
- Vector Searches/sec: 3000+
- Network Overhead: <5%

## Error Handling Flow

```
Request → Validation → Processing → Error Detection
    ↓              ↓         ↓           ↓
Response ←─ HTTP 4xx ←─ HTTP 5xx ←─ Error Logging
```

**Error Types:**
- **Validation Errors** (400) - Invalid request format
- **Authentication Errors** (401/403) - Access denied
- **Processing Errors** (500) - Internal failures
- **Resource Errors** (503) - Temporary unavailability

**Error Recovery:**
- Automatic retry for transient failures
- Circuit breaker pattern for external services
- Graceful degradation for partial failures
- Comprehensive error logging and monitoring

## Monitoring and Observability

**Tracing:**
- Distributed tracing with OpenTelemetry
- Span context propagation across components
- Performance profiling with pprof endpoints

**Metrics:**
- Request latency histograms
- Error rate counters
- Resource utilization gauges
- Cache hit/miss ratios

**Logging:**
- Structured logging with contextual information
- Request correlation IDs
- Performance and security event logging

This data flow architecture ensures efficient, reliable, and observable operation of the Knowd knowledge graph database while maintaining security, performance, and scalability requirements.
