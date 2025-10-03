# Code Organization

This document explains the organization and structure of the Knowd codebase, helping developers understand where to find and place code.

## High-Level Structure

```
knowd/
├── cmd/                    # Application entrypoints
│   └── knowd/             # Main server binary
├── internal/              # Private application code (not exported)
│   ├── auth/              # Authentication & authorization
│   ├── cluster/           # Clustering and replication
│   ├── engine/            # Core transaction engine
│   ├── hooks/             # Knowledge hooks system
│   ├── lockchain/         # Cryptographic provenance
│   ├── namespace/         # Multi-tenancy support
│   ├── policy/            # Policy pack management
│   ├── server/            # HTTP/gRPC servers
│   ├── shacl/             # SHACL validation engine
│   ├── sparql/            # SPARQL query engine
│   ├── store/             # RDF storage backends
│   ├── telemetry/         # Observability and metrics
│   ├── vec/              # Vector similarity search
│   └── wasm/             # WASM execution runtime
├── api/                   # Public API definitions
│   ├── openapi/          # OpenAPI/Swagger specifications
│   ├── proto/            # Protocol buffer definitions
│   └── pack/             # Policy pack schemas
├── docs/                  # Documentation
├── testdata/              # Test data and fixtures
└── rules_knowd/          # Bazel build rules
```

## Package Organization Principles

### Internal Packages (`internal/`)

The `internal/` directory contains all private application code that is not intended for external use. This follows Go's convention for internal packages.

**Key Principles:**
- **No external imports** - Code in `internal/` should not be imported by external packages
- **Clear separation** - Each package has a single, well-defined responsibility
- **Dependency management** - Minimize cross-package dependencies where possible

### Feature-Based Organization

Packages are organized by feature rather than by layer:

```
internal/
├── store/         # Data storage (not "models" or "database")
├── sparql/        # Query processing (not "controllers" or "handlers")
├── hooks/         # Event processing (not "services" or "business logic")
└── server/        # HTTP/gRPC serving (not "presentation" or "web")
```

This approach makes it easier to:
- **Locate functionality** - Features are grouped together
- **Test components** - Each feature can be tested independently
- **Scale development** - Multiple developers can work on different features
- **Understand dependencies** - Clear feature boundaries

## Major Component Breakdown

### Core Engine (`internal/engine/`)

**Purpose:** Orchestrates all major operations and coordinates between components.

**Key Files:**
- `engine.go` - Main transaction processing engine
- `engine_test.go` - Integration tests for the engine

**Responsibilities:**
- Transaction lifecycle management
- Hook execution coordination
- SHACL validation triggering
- Receipt generation

### Storage Layer (`internal/store/`)

**Purpose:** Persistent storage of RDF data with multiple backend options.

**Subpackages:**
- `memory/` - In-memory storage for development/testing
- `disk/` - Persistent disk-based storage with WAL and snapshots

**Key Interfaces:**
- `Interface` - Common storage API
- `Config` - Storage configuration options

### Query Engine (`internal/sparql/`)

**Purpose:** SPARQL query parsing, optimization, and execution.

**Key Components:**
- **Parser** - SPARQL syntax parsing
- **Algebra** - Query plan representation
- **Executor** - Query execution engine
- **Optimizer** - Cost-based query optimization

**Advanced Features:**
- Query plan caching
- Prepared statements
- Streaming results
- Advanced SPARQL constructs (UNION, OPTIONAL, etc.)

### Validation Engine (`internal/shacl/`)

**Purpose:** SHACL constraint validation for data quality.

**Key Components:**
- **Validator** - Core validation logic
- **Shapes** - SHACL shape definitions
- **Report** - Validation result formatting

**Features:**
- NodeShape and PropertyShape validation
- Custom constraint functions
- Integration with policy packs

### Hooks System (`internal/hooks/`)

**Purpose:** Event-driven processing and policy automation.

**Key Components:**
- **Hooks** - Hook registration and lifecycle
- **Batch** - Parallel hook execution
- **Window** - Time-based aggregations

**Hook Types:**
- SPARQL-ASK, SHACL, Delta, Threshold, Count, Window

### Security & Auth (`internal/auth/`)

**Purpose:** Authentication, authorization, and cryptographic operations.

**Key Components:**
- **mTLS** - Mutual TLS certificate management
- **Tokens** - HMAC and JWT token handling

**Features:**
- Certificate-based authentication
- Token-based authorization
- Secure key management

### Clustering (`internal/cluster/`)

**Purpose:** Horizontal scaling and high availability.

**Key Components:**
- **Leader** - Write coordination and replication
- **Follower** - Read scaling and query federation
- **Scatter/Gather** - Distributed query execution

**Features:**
- WAL-based replication
- Federated queries
- Automatic failover

## API Layer (`api/`)

**Purpose:** Public API definitions and schemas.

**OpenAPI (`openapi/`):**
- REST API specifications
- Request/response schemas
- Endpoint documentation

**Protocol Buffers (`proto/`):**
- gRPC service definitions
- Message type definitions
- Service documentation

**Policy Packs (`pack/`):**
- Schema definitions for policy configuration
- Validation rules for pack contents

## Documentation (`docs/`)

**Purpose:** Comprehensive project documentation.

**Structure:**
- `getting-started/` - Installation and quick start
- `user-guide/` - API usage and examples
- `architecture/` - System design and components
- `developer-guide/` - Development workflows and guidelines
- `deployment/` - Production deployment guides
- `troubleshooting/` - Common issues and solutions

## Test Organization

### Unit Tests
- Located alongside implementation files (`*_test.go`)
- Test individual functions and methods
- Use mocks for external dependencies

### Integration Tests
- `integration_test.go` - End-to-end workflow tests
- Test component interactions
- Use real dependencies where possible

### Benchmark Tests
- `*bench_test.go` files for performance testing
- Focus on critical code paths
- Establish performance baselines

## Dependency Management

### Import Organization

**Internal Imports:**
```go
// Preferred: relative imports for internal packages
"github.com/unrdf/knowd/internal/sparql"
"github.com/unrdf/knowd/internal/store"
```

**External Imports:**
```go
// Grouped by domain, then alphabetically
"context"
"fmt"
"log"

"github.com/external/lib1"
"github.com/external/lib2"
```

### Avoiding Circular Dependencies

**Design Patterns:**
- **Dependency Injection** - Pass dependencies as parameters
- **Interface Segregation** - Use small, focused interfaces
- **Event-Driven Architecture** - Use channels/events for loose coupling

**Common Patterns:**
```go
// Good: Dependency injection
func NewEngine(store store.Interface, hooks hooks.Interface) *Engine

// Avoid: Direct package coupling
// func NewEngine() *Engine { store := store.NewMemory() }
```

## Adding New Components

### 1. Choose Package Location

**Question:** "What feature does this implement?"
- **Storage-related** → `internal/store/`
- **Query-related** → `internal/sparql/`
- **Validation-related** → `internal/shacl/`
- **Policy-related** → `internal/hooks/` or `internal/policy/`

### 2. Define Interfaces First

```go
// Define interfaces before implementations
type Interface interface {
    Method1(ctx context.Context) error
    Method2(param string) (result Type, error)
}

// Then implement concrete types
type Implementation struct {
    // fields
}

func (i *Implementation) Method1(ctx context.Context) error {
    // implementation
}
```

### 3. Add Comprehensive Tests

```go
func TestImplementation_Method1(t *testing.T) {
    tests := []struct {
        name    string
        input   InputType
        want    WantType
        wantErr bool
    }{
        {"success case", validInput, expectedOutput, false},
        {"error case", invalidInput, nil, true},
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            // test implementation
        })
    }
}
```

### 4. Update Documentation

- Add to appropriate `docs/` section
- Update API documentation if public interfaces change
- Add examples for new functionality

## Common Patterns

### Error Handling

**Consistent Error Types:**
```go
// Define specific error types
var ErrNotFound = errors.New("resource not found")

// Use error wrapping for context
return fmt.Errorf("failed to process %s: %w", name, ErrNotFound)
```

### Context Usage

**Always pass context:**
```go
func (s *Service) Process(ctx context.Context, data Data) error {
    // Use ctx for timeouts, cancellation, logging
    ctx, cancel := context.WithTimeout(ctx, 30*time.Second)
    defer cancel()
}
```

### Interface Design

**Small, focused interfaces:**
```go
// Good: Single responsibility
type Reader interface {
    Read(ctx context.Context, id string) (Data, error)
}

type Writer interface {
    Write(ctx context.Context, data Data) error
}

// Avoid: God interface
type Storage interface {
    Read(ctx context.Context, id string) (Data, error)
    Write(ctx context.Context, data Data) error)
    Delete(ctx context.Context, id string) error
    List(ctx context.Context) ([]Data, error)
    // ... many more methods
}
```

This code organization ensures maintainability, testability, and scalability as the project grows, while following Go idioms and best practices.
