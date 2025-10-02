# UNRDF System Architecture Overview

High-level architecture of the UNRDF v3 autonomic RDF framework.

## System Components

```
┌────────────────────────────────────────────────────────────────┐
│                     UNRDF v3 Architecture                       │
├────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │                     CLI Layer (citty)                    │  │
│  │  - kubectl/docker-style noun-verb commands              │  │
│  │  - Context management (multi-environment)                │  │
│  │  - Output formatting (JSON, YAML, table, tree)          │  │
│  │  - Shell completion (Bash, Zsh, Fish)                   │  │
│  └──────────────┬──────────────────────────────────────────┘  │
│                 │                                               │
│                 ▼                                               │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │             Composables Layer (Vue-inspired)             │  │
│  │  useGraph() | useTurtle() | useDelta() | useCanon()     │  │
│  │  useTerms() | useReasoner() | useZod() | useValidator() │  │
│  └──────────────┬──────────────────────────────────────────┘  │
│                 │                                               │
│                 ▼                                               │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │              Knowledge Engine (Core Logic)               │  │
│  │  ┌─────────────────┐  ┌─────────────────────────────┐  │  │
│  │  │ Hook Manager    │  │ Transaction Manager         │  │  │
│  │  │ - ASK/SHACL     │  │ - ACID transactions         │  │  │
│  │  │ - DELTA/COUNT   │  │ - Pre/post hooks            │  │  │
│  │  │ - THRESHOLD     │  │ - Conflict resolution       │  │  │
│  │  │ - WINDOW        │  │ - Multi-agent coordination  │  │  │
│  │  └─────────────────┘  └─────────────────────────────┘  │  │
│  │                                                           │  │
│  │  ┌─────────────────┐  ┌─────────────────────────────┐  │  │
│  │  │ Policy Packs    │  │ Lockchain Writer            │  │  │
│  │  │ - Versioning    │  │ - SHA256/SHA3/BLAKE3        │  │  │
│  │  │ - Dependencies  │  │ - Git-notes anchoring       │  │  │
│  │  │ - Activation    │  │ - Merkle trees              │  │  │
│  │  └─────────────────┘  └─────────────────────────────┘  │  │
│  │                                                           │  │
│  │  ┌─────────────────┐  ┌─────────────────────────────┐  │  │
│  │  │ Effect Sandbox  │  │ Resolution Layer            │  │  │
│  │  │ - VM2/Workers   │  │ - Voting                    │  │  │
│  │  │ - CPU limits    │  │ - Merging                   │  │  │
│  │  │ - I/O controls  │  │ - CRDT                      │  │  │
│  │  └─────────────────┘  └─────────────────────────────┘  │  │
│  └──────────────┬──────────────────────────────────────────┘  │
│                 │                                               │
│                 ▼                                               │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │                  RDF Engine (Storage)                    │  │
│  │  - N3.Store (W3C compliant)                              │  │
│  │  - Comunica SPARQL 1.1                                   │  │
│  │  - SHACL validation (rdf-validate-shacl)                 │  │
│  │  - Canonicalization (URDNA2015)                          │  │
│  └─────────────────────────────────────────────────────────┘  │
│                                                                 │
└────────────────────────────────────────────────────────────────┘
```

## Deployment Patterns

### Pattern 1: Embedded Mode (Development)

```
┌──────────────────────────┐
│   Application Process    │
├──────────────────────────┤
│  CLI / Composables       │
│          ↓               │
│  Knowledge Engine        │
│          ↓               │
│  RDF Engine (N3)         │
└──────────────────────────┘
```

**Use Cases:**
- Local development
- Unit testing
- Offline operations
- Quick prototyping

**Pros:**
- No external dependencies
- Fast startup
- Simple deployment

**Cons:**
- Single process
- Limited scalability
- Higher memory usage per instance

### Pattern 2: Sidecar Mode (Production)

```
┌──────────────────────────┐  ┌──────────────────────────┐
│   Application (Any Lang) │  │   KGC Sidecar (Node.js)  │
├──────────────────────────┤  ├──────────────────────────┤
│  gRPC Client             │  │  gRPC Server             │
│          ↓               │  │          ↓               │
│  Business Logic          │  │  Knowledge Engine        │
│                          │  │          ↓               │
│                          │  │  RDF Engine (N3)         │
└──────────┬───────────────┘  └──────────────────────────┘
           │                             ▲
           │  gRPC (localhost:50051)     │
           └─────────────────────────────┘
```

**Use Cases:**
- Production deployments
- Multi-language support
- Microservices architecture
- Kubernetes environments

**Pros:**
- Language-agnostic
- Centralized knowledge operations
- Better resource isolation
- Horizontal scalability

**Cons:**
- Extra network hop (localhost)
- Additional deployment complexity

### Pattern 3: Hybrid Mode (Flexible)

```
┌──────────────────────────────────┐
│   CLI / Application              │
├──────────────────────────────────┤
│  Auto-detection logic:           │
│  if (KGC_SIDECAR_ADDRESS) {      │
│    use gRPC client ───────────┐  │
│  } else {                      │  │
│    use embedded engine         │  │
│  }                             │  │
└────────────────────────────────┼──┘
                                 │
                    ┌────────────▼────────────┐
                    │   KGC Sidecar           │
                    │   (if available)        │
                    └─────────────────────────┘
```

**Use Cases:**
- Gradual migration
- Development + production parity
- Multi-tenant systems

**Pros:**
- Best of both worlds
- Graceful degradation
- Easy migration path

## Data Flow

### Transaction Lifecycle

```
1. Client Request
   ↓
2. Pre-Hook Evaluation
   ├─ ASK Predicate (SPARQL)
   ├─ SHACL Validation
   └─ Custom Logic
   ↓
3. Hook Decision
   ├─ VETO → Abort ❌
   └─ PASS → Continue ✓
   ↓
4. Transaction Apply
   ├─ Add Quads
   ├─ Remove Quads
   └─ Update Store
   ↓
5. Post-Hook Execution
   ├─ Audit Logging
   ├─ Receipts
   └─ Notifications
   ↓
6. Receipt Generation
   ├─ SHA256 Hash
   ├─ Timestamp
   └─ Git-notes Anchor (optional)
   ↓
7. Client Response
```

### Knowledge Hook Evaluation

```
1. Event Trigger
   (Transaction, Timer, External)
   ↓
2. Condition Evaluation
   ├─ Load SPARQL/SHACL from file
   ├─ Verify SHA256 integrity
   └─ Execute condition
   ↓
3. Condition Result
   ├─ FALSE → Skip Hook ⏭
   └─ TRUE → Execute Hook ✓
   ↓
4. before() Lifecycle
   ├─ Validate payload
   ├─ Normalize data
   └─ Check cancellation
   ↓
5. run() Lifecycle
   ├─ Core hook logic
   ├─ Generate results
   └─ Create assertions (optional)
   ↓
6. after() Lifecycle
   ├─ Audit logging
   ├─ Cleanup
   └─ Finalize result
   ↓
7. Result Storage
```

## Security Architecture

### Defense in Depth

```
Layer 1: Input Validation
  ├─ Zod runtime validation
  ├─ SPARQL syntax checking
  └─ File path sanitization

Layer 2: Content Addressing
  ├─ SHA256 verification
  ├─ File integrity checks
  └─ Immutable references

Layer 3: Execution Sandbox
  ├─ VM2 isolation
  ├─ CPU/memory limits
  ├─ I/O restrictions
  └─ Timeout enforcement

Layer 4: Network Security
  ├─ TLS for sidecar (optional)
  ├─ mTLS in service mesh
  └─ Network policies (K8s)

Layer 5: Audit Trail
  ├─ Lockchain receipts
  ├─ Git-notes anchoring
  └─ Cryptographic provenance
```

### SPARQL Injection Prevention

**Vulnerable Code:**

```javascript
// ❌ BAD: Inline SPARQL with user input
const query = `ASK { ?s ex:name "${userInput}" }`;
```

**Secure Pattern:**

```javascript
// ✅ GOOD: Content-addressed file reference
defineHook({
  when: {
    kind: 'sparql-ask',
    ref: {
      uri: 'file://hooks/check.ask.rq',
      sha256: 'verified-hash',  // Prevents tampering
      mediaType: 'application/sparql-query'
    }
  }
});
```

## Observability

### Metrics (OpenTelemetry)

```
Application Metrics:
  - Hook execution time (p50, p95, p99)
  - Transaction commit rate
  - SPARQL query performance
  - Validation failures
  - Cache hit rate

Infrastructure Metrics:
  - CPU usage
  - Memory consumption
  - Network I/O
  - Storage size

Business Metrics:
  - Active hooks
  - Policy pack compliance
  - Audit trail coverage
```

### Tracing (Jaeger)

```
Trace: Transaction Apply
  ├─ Span: Pre-hook evaluation (2.3ms)
  │   ├─ Span: SPARQL ASK query (1.1ms)
  │   └─ Span: SHACL validation (1.2ms)
  ├─ Span: Transaction commit (3.5ms)
  └─ Span: Post-hook execution (1.8ms)
      ├─ Span: Audit logging (0.9ms)
      └─ Span: Receipt generation (0.9ms)

Total: 7.6ms
```

## Performance Characteristics

### Throughput

| Component | Target | Achieved |
|-----------|--------|----------|
| Hook evaluation (p99) | < 2ms | 1.8ms ✅ |
| Transaction commit | < 5ms | 4.5ms ✅ |
| SPARQL SELECT (10k triples) | < 500ms | 342ms ✅ |
| SHACL validation | < 200ms | 187ms ✅ |
| Sidecar RPS | > 1000 | 1200+ ✅ |

### Scalability

**Horizontal:**
- Sidecar instances: 10+ per cluster
- Concurrent connections: 100+ per sidecar
- Request rate: 1000+ RPS per sidecar

**Vertical:**
- Graph size: 1M+ triples
- Hook count: 100+ active hooks
- Query complexity: SPARQL 1.1 full support

## Technology Stack

### Core Dependencies

```
Runtime:
  - Node.js >=18.0.0
  - pnpm (required)

RDF & SPARQL:
  - N3.js (RDF store)
  - Comunica (SPARQL engine)
  - rdf-validate-shacl (SHACL validation)
  - rdf-canonize (URDNA2015)

Framework:
  - citty (CLI framework)
  - unctx (context management)
  - zod (runtime validation)

Infrastructure:
  - @grpc/grpc-js (gRPC)
  - @opentelemetry/* (observability)
  - vm2 (sandboxing)
  - testcontainers (E2E testing)
```

## Deployment Targets

### Local Development

```bash
# Embedded mode
npx unrdf hook eval health-check.mjs

# With local sidecar
docker run -p 50051:50051 unrdf/sidecar:latest
export KGC_SIDECAR_ADDRESS=localhost:50051
npx unrdf hook eval health-check.mjs
```

### Kubernetes

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: my-app
spec:
  template:
    spec:
      containers:
      - name: app
        image: my-app:latest
        env:
        - name: KGC_SIDECAR_ADDRESS
          value: "localhost:50051"
      - name: kgc-sidecar
        image: unrdf/sidecar:latest
        ports:
        - containerPort: 50051
        resources:
          requests:
            memory: "256Mi"
            cpu: "100m"
          limits:
            memory: "512Mi"
            cpu: "500m"
```

### AWS

```bash
# ECS Task Definition
{
  "family": "my-app",
  "containerDefinitions": [
    {
      "name": "app",
      "image": "my-app:latest",
      "environment": [
        { "name": "KGC_SIDECAR_ADDRESS", "value": "localhost:50051" }
      ]
    },
    {
      "name": "kgc-sidecar",
      "image": "unrdf/sidecar:latest",
      "portMappings": [
        { "containerPort": 50051 }
      ]
    }
  ]
}
```

## Future Enhancements

### Planned Features

- **Distributed Sidecar Cluster**: Multi-instance coordination
- **GraphQL Interface**: Alternative query API
- **Real-time Subscriptions**: WebSocket-based updates
- **Federated Hooks**: Cross-cluster hook execution
- **Enhanced Reasoning**: OWL 2 RL support
- **Performance Optimizations**: Query caching, index optimization

### Research Directions

- **Quantum-resistant Signatures**: Post-quantum cryptography
- **Zero-knowledge Proofs**: Privacy-preserving validation
- **Blockchain Integration**: Immutable audit trails
- **AI-driven Policy**: Machine learning for hook optimization

## See Also

- [CLI Architecture](/docs/architecture/cli-v2-architecture.md)
- [Sidecar Architecture](/docs/architecture/kgc-sidecar-architecture.md)
- [Knowledge Hooks](/docs/architecture/knowledge-hooks.md)
- [Deployment Guide](/docs/deployment/)
