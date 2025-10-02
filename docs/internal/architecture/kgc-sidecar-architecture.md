# KGC JavaScript Sidecar Architecture

## Overview

The KGC JavaScript Sidecar is a Node.js reference implementation that provides **transactional knowledge-graph mutation**, **policy-pack–driven knowledge hooks**, **content-addressed audit (lockchain)**, and **sandboxed effects** for any host application (Erlang/C/C++/Go/etc.).

## Architecture Components

### 1. Transaction Manager (`transaction.mjs`)

**Purpose**: Orchestrates atomic transactions with pre/post hooks and comprehensive receipts.

**Key Features**:
- Dual hash support (SHA3/BLAKE3) for cryptographic integrity
- Git-notes anchoring for immutable audit trails
- Performance optimization with `afterHashOnly` fast path
- OpenTelemetry observability integration
- Error isolation and backpressure handling

**Performance Targets**:
- p50 pre-hook pipeline ≤ 200 µs
- p99 ≤ 2 ms (10k triples store, afterHashOnly=true)
- Receipt write ≤ 5 ms median (no canonicalization)

### 2. Knowledge Hook Manager (`knowledge-hook-manager.mjs`)

**Purpose**: Central orchestration of knowledge hooks with file-based execution.

**Key Features**:
- Content-addressed file references (URI + SHA256)
- Support for ASK/SHACL/DELTA/THRESHOLD/COUNT/WINDOW predicates
- Policy pack integration with veto semantics
- Secure effect sandboxing
- Multi-agent coordination support

**Predicate Types**:
- **ASK**: Boolean SPARQL queries for feature flags and permission checks
- **SHACL**: Shape conformance validation for data quality gates
- **DELTA**: Row digest changes for configuration drift detection
- **THRESHOLD**: Numeric comparisons for KPI monitoring
- **COUNT**: Result set cardinality for inventory checks
- **WINDOW**: Time-based aggregations for trend analysis

### 3. Effect Sandbox (`effect-sandbox.mjs`)

**Purpose**: Secure execution of untrusted hook effects with comprehensive isolation.

**Key Features**:
- VM2/worker thread isolation
- CPU timeouts and memory limits
- Blocked I/O by default with allowlist
- Comprehensive logging and metrics
- Error isolation (100% target)

**Security Model**:
- Deny network/file access by default
- Allowlist per effect function
- Per-execution CPU/memory caps
- Wall-clock timeout enforcement

### 4. Policy Pack Manager (`policy-pack.mjs`)

**Purpose**: Versioned governance units with dependency management.

**Key Features**:
- Signed policy packs with signature verification
- Version pinning and rollback capabilities
- Dependency management
- Activation/deactivation controls
- Provenance metadata

**Governance Features**:
- Content-addressed references
- Signature verification for packs
- Immutable receipts with algorithm versions
- Clock source and hash verification

### 5. Lockchain Writer (`lockchain-writer.mjs`)

**Purpose**: Cryptographic audit trail with Git anchoring.

**Key Features**:
- Git-notes storage for immutable receipts
- Batch mode for performance
- Path verification
- Merkle tree support
- Signature verification

**Audit Trail**:
- Dual hash (SHA3/BLAKE3) support
- Git commit anchoring
- Batch processing
- Integrity verification

### 6. Resolution Layer (`resolution-layer.mjs`)

**Purpose**: Multi-agent coordination and conflict resolution.

**Key Features**:
- Proposal submission and resolution
- Multiple resolution strategies (voting, merging, CRDT, consensus)
- Conflict detection and resolution
- Agent registration and management
- Consensus mechanisms

**Resolution Strategies**:
- **Voting**: Weighted voting based on confidence and priority
- **Merging**: Combine all proposals into single delta
- **CRDT**: Conflict-free replicated data type resolution
- **Consensus**: Distributed consensus algorithms
- **Priority**: Highest priority proposal wins

### 7. Observability Manager (`observability.mjs`)

**Purpose**: Comprehensive monitoring with OpenTelemetry integration.

**Key Features**:
- Distributed tracing
- Performance metrics
- Error tracking
- Memory monitoring
- Cache statistics
- Backpressure monitoring

**Metrics**:
- Transaction latency percentiles
- Hook execution rate
- Error rates
- Memory usage
- Cache hit rates
- Queue depth

### 8. Performance Optimizer (`performance-optimizer.mjs`)

**Purpose**: Performance optimization to meet KGC success metrics.

**Key Features**:
- Fast path optimization
- Batch processing
- Parallel execution
- Query optimization
- Memory management
- Cache optimization

**Optimization Strategies**:
- `afterHashOnly` fast path for simple operations
- Batch processing for multiple transactions
- Parallel execution for independent operations
- Query caching and optimization
- Memory threshold monitoring
- Automatic garbage collection

## Data Flow

### Transaction Processing Flow

1. **Transaction Initiation**
   - Validate delta and options with Zod schemas
   - Start OpenTelemetry span
   - Initialize performance tracking

2. **Pre-Hook Execution**
   - Execute pre-hooks with veto semantics
   - Apply knowledge hooks with file-based conditions
   - Sandboxed effect execution
   - Performance optimization

3. **Transaction Commit**
   - Apply delta to store (mutate in place)
   - Calculate dual hashes (SHA3/BLAKE3)
   - Generate comprehensive receipt

4. **Post-Hook Execution**
   - Execute post-hooks for cleanup/auditing
   - Update metrics and observability
   - Write to lockchain if enabled

5. **Resolution (Optional)**
   - Submit proposals to resolution layer
   - Resolve conflicts using configured strategy
   - Apply resolved delta

### Hook Execution Flow

1. **Condition Evaluation**
   - Load content-addressed file references
   - Execute SPARQL/SHACL queries
   - Evaluate DELTA/THRESHOLD/COUNT/WINDOW predicates
   - Cache results for performance

2. **Effect Execution**
   - Sandboxed execution in VM2/worker
   - Timeout and memory limit enforcement
   - Error isolation and logging
   - Performance metrics collection

3. **Result Processing**
   - Validate hook results
   - Update transaction context
   - Apply veto if necessary
   - Record audit trail

## Configuration

### Manager Options

```javascript
const options = {
  // Performance
  performance: {
    enableProfiling: true,
    maxConcurrency: 10,
    afterHashOnly: false, // Fast path
    timeoutMs: 2000, // p99 ≤ 2ms
    maxHooks: 10000 // 10k exec/min
  },
  
  // Observability
  observability: {
    enableTracing: true,
    enableMetrics: true,
    serviceName: 'unrdf-kgc',
    endpoint: 'http://jaeger:14268/api/traces'
  },
  
  // Lockchain
  lockchainConfig: {
    gitRepo: process.cwd(),
    refName: 'refs/notes/lockchain',
    batchSize: 10
  },
  
  // Resolution
  resolutionConfig: {
    defaultStrategy: 'voting',
    maxProposals: 100,
    timeout: 30000
  }
};
```

### Policy Pack Configuration

```javascript
const policyPack = {
  meta: {
    name: 'compliance-v1',
    version: '1.0.0',
    description: 'Enterprise compliance rules'
  },
  config: {
    enabled: true,
    priority: 50,
    strictMode: false,
    timeout: 30000
  },
  hooks: [
    {
      name: 'data-quality-gate',
      file: 'hooks/data-quality.rq',
      enabled: true,
      priority: 80
    }
  ]
};
```

## Performance Targets

### Success Metrics (v1.0)

- **p50 pre-hook pipeline** ≤ 200 µs
- **p99** ≤ 2 ms (10k triples store, afterHashOnly=true)
- **Receipt write** ≤ 5 ms median (no canonicalization) / ≤ 200 ms with URDNA2015 on 100k triples
- **Hook engine** ≥ 10k exec/min sustained
- **Error isolation** 100%

### Optimization Strategies

1. **Fast Path**: Use `afterHashOnly=true` for simple operations
2. **Batch Processing**: Group multiple transactions
3. **Parallel Execution**: Execute independent operations concurrently
4. **Query Optimization**: Cache and optimize SPARQL queries
5. **Memory Management**: Monitor and optimize memory usage
6. **Cache Optimization**: Implement intelligent caching strategies

## Security Model

### Sandboxing

- **VM2/Worker Isolation**: Execute untrusted code in isolated environment
- **Resource Limits**: CPU timeouts and memory caps
- **I/O Restrictions**: Block network/file access by default
- **Allowlist Model**: Explicit permissions for required operations

### Cryptographic Integrity

- **Dual Hash**: SHA3/BLAKE3 for redundancy
- **Git Anchoring**: Immutable audit trail
- **Signature Verification**: Verify policy pack signatures
- **Content Addressing**: SHA256 verification for file references

### Error Isolation

- **100% Target**: No single failing hook halts the process
- **Graceful Degradation**: Continue operation despite individual failures
- **Comprehensive Logging**: Detailed error tracking and reporting
- **Recovery Mechanisms**: Automatic retry and fallback strategies

## Deployment

### Modes

1. **Library Mode**: In-process integration
2. **Sidecar Mode**: HTTP/IPC communication

### Kubernetes Deployment

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: kgc-sidecar
spec:
  replicas: 3
  selector:
    matchLabels:
      app: kgc-sidecar
  template:
    metadata:
      labels:
        app: kgc-sidecar
    spec:
      containers:
      - name: kgc-sidecar
        image: unrdf/kgc-sidecar:latest
        ports:
        - containerPort: 3000
        env:
        - name: OBSERVABILITY_ENDPOINT
          value: "http://jaeger:14268/api/traces"
        - name: LOCKCHAIN_GIT_REPO
          value: "/data/git"
        volumeMounts:
        - name: git-data
          mountPath: /data/git
        - name: policy-packs
          mountPath: /app/policy-packs
        resources:
          requests:
            memory: "150Mi"
            cpu: "100m"
          limits:
            memory: "250Mi"
            cpu: "500m"
      volumes:
      - name: git-data
        persistentVolumeClaim:
          claimName: git-data-pvc
      - name: policy-packs
        configMap:
          name: policy-packs-config
```

### Configuration Management

- **Environment Variables**: Runtime configuration
- **ConfigMaps**: Policy pack definitions
- **Secrets**: Signing keys and credentials
- **Hot Reload**: Atomic policy pack updates

## Monitoring and Observability

### OpenTelemetry Integration

- **Traces**: Distributed transaction tracing
- **Metrics**: Performance and health metrics
- **Logs**: Structured logging with correlation IDs

### Key Performance Indicators

- Transaction success rate
- Veto rate
- p50/p99 phase latencies
- Receipts per second
- Lockchain lag
- Sandbox failure rate
- Pack activation MTTR

### Alerting

- Hook timeout surge
- Veto spike
- Receipt write failures
- Canonicalization backlog
- Memory threshold breaches
- Error rate increases

## Testing Strategy

### Seven Test Suites

1. **Unit Tests**: Individual component testing
2. **Property Tests**: Property-based testing
3. **Permutation Tests**: Order-dependent behavior
4. **Combination Tests**: Multi-component interactions
5. **Stress Tests**: High-load scenarios
6. **Adversarial Tests**: Security and resilience
7. **Benchmark Tests**: Performance validation

### Acceptance Criteria

- All KPIs green for 24h soak test
- 0 critical CVEs
- 100% schema validation in CI
- Golden receipts for determinism
- Performance gates met on target hardware

## Future Enhancements

### Planned Features

1. **Advanced Resolution Strategies**: Machine learning-based conflict resolution
2. **Distributed State Store**: Multi-node coordination
3. **Enhanced Security**: Zero-knowledge proofs
4. **Performance Optimization**: WebAssembly integration
5. **Monitoring Dashboard**: Real-time performance visualization

### Research Areas

1. **Quantum-Resistant Cryptography**: Post-quantum security
2. **Federated Learning**: Distributed model training
3. **Blockchain Integration**: Public blockchain anchoring
4. **AI-Driven Optimization**: Machine learning performance tuning

## Conclusion

The KGC JavaScript Sidecar provides a production-ready, enterprise-grade knowledge graph control system with comprehensive observability, security, and performance optimization. It serves as the reference implementation for the KGC paper while maintaining hot paths in branchless C for maximum performance.

The architecture is designed for scalability, reliability, and maintainability, with clear separation of concerns and comprehensive testing strategies. The system meets all KGC PRD requirements while providing extensibility for future enhancements.




