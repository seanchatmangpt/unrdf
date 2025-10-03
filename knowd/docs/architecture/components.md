# Architecture Components

This document provides detailed descriptions of the major components that make up the Knowd knowledge graph database system.

## Core Components

### RDF Store (`internal/store/`)

The RDF store is the heart of Knowd, responsible for storing and retrieving RDF quads. It supports both in-memory and disk-based storage backends.

**Key Components:**
- **Memory Store** (`memory.go`) - In-memory RDF quad storage for development and testing
- **Disk Store** (`disk/`) - Persistent storage with WAL (Write-Ahead Log) and snapshot capabilities
- **WAL** (`wal.go`) - Append-only write-ahead log for durability
- **Segments** (`segments.go`) - Memory-mapped read-only segments for efficient querying
- **Snapshots** (`snapshot.go`) - Periodic full store snapshots for recovery
- **Compaction** (`compact.go`) - WAL compaction and segment merging

**Features:**
- Concurrent read/write operations
- ACID transaction guarantees
- Namespace isolation for multi-tenancy
- Compression and encryption support
- Remote storage integration (S3, GCS)

### SPARQL Engine (`internal/sparql/`)

The SPARQL engine handles query parsing, optimization, and execution.

**Key Components:**
- **Parser** (`parser.go`) - SPARQL query parsing with support for SELECT, ASK, CONSTRUCT
- **Algebra** (`algebra.go`) - Query algebra representation and optimization
- **Executor** (`exec.go`) - Query execution engine with iterator-based evaluation
- **Plan Cache** (`plan_cache.go`) - LRU cache for compiled query plans
- **Cost-Based Optimizer** (`cbo.go`) - Query optimization using cardinality estimates

**Features:**
- Full SPARQL 1.1 support (SELECT, ASK, CONSTRUCT, DESCRIBE)
- Advanced constructs: UNION, OPTIONAL, MINUS, BIND, VALUES
- Query optimization with statistics-based cost estimation
- Prepared statements and parameter binding
- Streaming query results for large datasets

### SHACL Validator (`internal/shacl/`)

The SHACL validation engine enforces data quality constraints using SHACL (Shapes Constraint Language).

**Key Components:**
- **Validator** (`validator.go`) - Core validation engine for node and property shapes
- **Shapes** (`shapes.go`) - SHACL shape definitions and constraint parsing
- **Report** (`report.go`) - Validation report generation in SHACL compact format

**Features:**
- NodeShape and PropertyShape validation
- Constraint support: minCount, maxCount, datatype, class, nodeKind, pattern
- Closed shapes and ignored properties
- Custom constraint functions
- Integration with policy packs

### Knowledge Hooks (`internal/hooks/`)

The hooks system provides event-driven processing capabilities for policy automation.

**Key Components:**
- **Hooks** (`hooks.go`) - Hook registration and lifecycle management
- **Batch** (`batch.go`) - Parallel hook execution with dependency resolution
- **Window** (`window.go`) - Time-based windowing for event aggregation

**Hook Types:**
- **sparql-ask** - Boolean queries for conditional logic
- **shacl** - Data validation triggers
- **delta** - Change detection and response
- **threshold** - Numeric threshold monitoring
- **count** - Cardinality-based triggers
- **window** - Time-windowed aggregations

**Features:**
- Event-driven architecture
- Policy pack integration
- Sandboxed execution environment
- Batch processing for efficiency
- Integration with transaction pipeline

### Lockchain (`internal/lockchain/`)

The cryptographic provenance system provides tamper-evident audit trails.

**Key Components:**
- **Lockchain** (`lockchain.go`) - Receipt generation and verification
- **Merkle** (`merkle.go`) - SHA3-256 Merkle tree construction
- **Canonical** (`canonical.go`) - URDNA2015 RDF canonicalization
- **Sign** (`sign.go`) - Ed25519 digital signatures
- **JWS** (`jws.go`) - JSON Web Signature support

**Features:**
- Cryptographic receipt generation
- Merkle tree-based integrity verification
- Ed25519 digital signatures
- JWS detached signature support
- Git integration for audit trails

## Server Components

### HTTP Server (`internal/server/`)

The HTTP server provides REST API endpoints for all Knowd functionality.

**Key Components:**
- **HTTP** (`http.go`) - Main HTTP server with routing and middleware
- **Routes** (`routes.go`) - API endpoint definitions
- **Middleware** (`middleware.go`) - Authentication, logging, and namespace binding
- **gRPC** (`grpc.go`) - gRPC server for high-performance APIs

**Features:**
- RESTful API design
- JSON request/response handling
- Namespace-based request routing
- Authentication and authorization
- Request validation and error handling

### Cluster (`internal/cluster/`)

The clustering system enables horizontal scaling and high availability.

**Key Components:**
- **Leader** (`leader.go`) - Leader node with write coordination
- **Follower** (`follower.go`) - Read-only follower nodes
- **RPC** (`rpc.go`) - gRPC-based cluster communication
- **Scatter/Gather** (`scattergather.go`) - Federated query execution

**Features:**
- Leader/follower architecture
- WAL replication for consistency
- Federated SELECT queries
- Automatic snapshot shipping
- Follower promotion capabilities

## Supporting Components

### Namespace (`internal/namespace/`)

Multi-tenancy support with complete isolation between namespaces.

**Key Components:**
- **Namespace** (`ns.go`) - Namespace registry and management
- **Middleware** (`middleware.go`) - HTTP/gRPC namespace binding

**Features:**
- Complete data isolation
- Per-namespace configuration
- Namespace-scoped caching
- Admin APIs for management

### Vector Search (`internal/vec/`)

Similarity search capabilities using HNSW (Hierarchical Navigable Small World) graphs.

**Key Components:**
- **HNSW** (`hnsw.go`) - In-memory vector index implementation
- **Index** (`index.go`) - Vector index management and search
- **Embed** (`embed.go`) - Text embedding generation

**Features:**
- High-performance similarity search
- Namespace-based vector indexes
- Automatic index rebuilding
- Configurable distance metrics

### WASM Runtime (`internal/wasm/`)

WebAssembly execution environment for hook effects.

**Key Components:**
- **Runtime** (`runtime.go`) - WASI-compliant WASM execution
- **Loader** (`loader.go`) - WASM module loading and management

**Features:**
- Secure sandboxed execution
- WASI compliance
- Resource limits and isolation
- Integration with hook system

## Data Flow

```
Client Request → HTTP/gRPC Server → Namespace Middleware → Authentication
                                                           ↓
Transaction → Lockchain (Receipt) → Hooks (Validation) → RDF Store
                                                           ↓
SPARQL Query → Parser → Optimizer → Executor → Results → Client
                                                           ↓
Vector Search → HNSW Index → Similarity Results → Client
```

## Configuration

All components are configured through a unified configuration system with environment variable support, structured configuration files, and runtime hot-reloading capabilities.