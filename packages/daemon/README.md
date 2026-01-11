# @unrdf/daemon

Background daemon for managing scheduled tasks and event-driven operations in UNRDF with enterprise-grade security and comprehensive RDF integration.

## Features

### Core Daemon
- **Scheduled Task Execution** - Execute operations based on cron, intervals, or events
- **Clustering Support** - Coordinate task execution across multiple daemon nodes
- **Event-Driven Architecture** - React to system events with automatic task execution
- **Health Monitoring** - Real-time health checks and performance metrics
- **Retry Policies** - Configurable exponential backoff with jitter
- **Receipt Generation** - Audit trail with cryptographic proofs for operations
- **Concurrency Control** - Limit concurrent operations with queue management

### Security (NEW in v6.0.0)
- **API Key Authentication** - BLAKE3 hashing with constant-time verification
- **Environment-Aware Security** - Development mode graceful degradation, production enforcement
- **Comprehensive Validation** - Injection detection (SQL, SPARQL, command)
- **Secret Detection** - Pattern-based scanning to prevent credential exposure
- **Path Traversal Prevention** - Secure file path validation
- **Error Sanitization** - Removes sensitive data from error messages
- **Audit Logging** - Comprehensive authentication and security event tracking

### Integration Modules (NEW in v6.0.0)

**13 Production-Ready Integration Modules** (38 MJS files):

1. **consensus.mjs** - Raft consensus coordination for distributed daemon clusters
2. **distributed.mjs** - Task distribution across daemon nodes with load balancing
3. **event-store.mjs** - Temporal event sourcing with KGC integration
4. **federation-query.mjs** - Federated SPARQL query execution across distributed stores
5. **hook-scheduler.mjs** - Knowledge Hook scheduling with dependency resolution
6. **hooks-policy.mjs** - Policy-based Knowledge Hook execution
7. **kgc-4d-sourcing.mjs** - KGC 4D temporal event sourcing with time-travel capabilities
8. **knowledge-rules.mjs** - Inference engine integration for rule-based reasoning
9. **observability.mjs** - OpenTelemetry (OTEL) integration with distributed tracing
10. **receipts-merkle.mjs** - Merkle tree receipt generation with cryptographic proofs
11. **streaming.mjs** - Real-time RDF streaming with change feed support
12. **v6-deltagate.mjs** - ΔGate control plane integration for unified receipt management
13. **yawl.mjs** - YAWL workflow orchestration with RDF-native task execution

## Installation

```bash
pnpm add @unrdf/daemon
```

## Quick Start

```javascript
import { Daemon } from '@unrdf/daemon';

// Create daemon
const daemon = new Daemon({
  daemonId: 'my-daemon',
  name: 'My Daemon',
  concurrency: 5,
});

// Start daemon
await daemon.start();

// Schedule operation
daemon.schedule({
  id: 'my-task',
  name: 'My Task',
  handler: async () => {
    console.log('Task executing...');
    return { status: 'completed' };
  },
});

// Execute operation
const result = await daemon.execute('my-task');

// Stop daemon
await daemon.stop();
```

## Core Components

### Daemon Class
Main controller for managing scheduled operations and clustering.

**Key Methods**:
- `start()` - Start the daemon
- `stop()` - Stop the daemon
- `schedule(operation)` - Register an operation
- `unschedule(operationId)` - Unregister an operation
- `execute(operationId)` - Execute an operation immediately
- `listOperations()` - List all scheduled operations
- `getHealth()` - Get daemon health status
- `getMetrics()` - Get performance metrics

### TriggerEvaluator
Evaluates events against trigger patterns and determines which tasks to execute.

**Key Methods**:
- `registerTrigger(eventName, pattern, actions)` - Register trigger pattern
- `evaluate(event)` - Evaluate event and get matched task IDs
- `matches(event, trigger)` - Check event-trigger match

### Schemas
Comprehensive Zod validation schemas for configuration and operations:
- `DaemonConfigSchema` - Daemon configuration
- `ScheduledOperationSchema` - Operation definition
- `OperationReceiptSchema` - Operation execution record
- `DaemonHealthSchema` - Health status
- `DaemonMetricsSchema` - Performance metrics

## Configuration

```javascript
const daemon = new Daemon({
  // Core settings
  daemonId: 'unique-id',
  name: 'Human-readable name',

  // Network/server
  port: 8080,                    // Server port (default: 8080)

  // Performance
  concurrency: 10,               // Max concurrent ops (default: 10)
  healthCheckIntervalMs: 30000,  // Health check frequency
  metricsRetentionMs: 3600000,   // Metrics window size

  // Logging
  logLevel: 'info',              // Log level
  logger: customLogger,          // Optional custom logger

  // Optional: Retry policy
  globalRetryPolicy: {
    maxAttempts: 3,
    backoffMs: 1000,
    backoffMultiplier: 2,
    maxBackoffMs: 30000,
    jitterFactor: 0.1,
  },

  // Optional: Pre-registered operations
  operations: [...],

  // Optional: Environment variables
  environment: { KEY: 'value' },
});
```

## Events

The daemon emits events for monitoring:
- `daemon:started` - Daemon has started
- `daemon:stopped` - Daemon has stopped
- `operation:enqueued` - Operation added to queue
- `operation:started` - Operation execution started
- `operation:success` - Operation completed successfully
- `operation:failure` - Operation failed

```javascript
daemon.on('operation:success', (event) => {
  console.log(`Operation ${event.operationId} took ${event.duration}ms`);
});
```

## Health & Metrics

### Health Status
```javascript
const health = daemon.getHealth();
// {
//   nodeId: 'node-xxx',
//   clusterId: 'cluster-xxx',
//   isRunning: true,
//   isLeader: false,
//   uptime: 12345,
//   activeOperations: 2,
//   queuedOperations: 5,
//   completedOperations: 42,
//   timestamp: Date
// }
```

### Metrics
```javascript
const metrics = daemon.getMetrics();
// {
//   nodeId: 'node-xxx',
//   totalOperations: 42,
//   successfulOperations: 40,
//   failedOperations: 2,
//   averageDuration: 156,
//   totalDuration: 6552,
//   successRate: 95.24,
//   timestamp: Date
// }
```

## Testing

Run tests with:
```bash
pnpm test
pnpm test:watch
pnpm test:coverage
```

## Security

### API Key Authentication

```javascript
import { createAuthenticator } from '@unrdf/daemon';

// Generate API key for production
const { authenticator, key } = await createAuthenticator({
  environment: 'production'
});

console.log('API Key:', key); // Distribute to authorized users

// Authenticate requests
const result = await authenticator.authenticate({
  headers: { 'x-api-key': providedKey }
});

if (result.authenticated) {
  await executeDaemonOperation();
}
```

**Security Features**:
- BLAKE3 cryptographic hashing (256-bit)
- Constant-time verification (timing attack prevention)
- Environment variable support (`UNRDF_API_KEY`)
- Comprehensive audit logging
- Graceful degradation (dev warns, production blocks)

See [AUTHENTICATION.md](AUTHENTICATION.md) for complete authentication documentation.

### Security Validation

All daemon operations include:
- Input validation against injection attacks
- Secret detection in outputs
- Path traversal prevention
- Error message sanitization

See [SECURITY_INTEGRATION_SUMMARY.md](SECURITY_INTEGRATION_SUMMARY.md) for security implementation details.

## Integration Examples

### ΔGate Control Plane

```javascript
import { DaemonDeltaGate } from '@unrdf/daemon';

const gate = new DaemonDeltaGate({
  receipts: true,
  validation: true,
  merkleProofs: true,
});

await gate.initialize();

const result = await gate.processDelta({
  operation: 'insert',
  triples: myTriples,
});

console.log('Receipt:', result.receipt);
console.log('Merkle Proof:', result.merkleProof);
```

### YAWL Workflow Orchestration

```javascript
import { createYawlIntegration } from '@unrdf/daemon';

const yawl = createYawlIntegration({
  daemonId: 'workflow-daemon',
});

await yawl.executeWorkflow({
  workflowId: 'rdf-processing',
  tasks: [
    { id: 'parse', handler: parseRdf },
    { id: 'validate', handler: validateShacl },
    { id: 'store', handler: storeTriples },
  ],
});
```

### Federated Query Execution

```javascript
import { createFederationQuery } from '@unrdf/daemon';

const federation = createFederationQuery({
  nodes: ['http://node1:8080', 'http://node2:8080'],
});

const results = await federation.executeFederatedQuery(sparqlQuery);
```

## Examples

See the `examples/` directory for complete working examples:
- `01-basic.mjs` - Basic daemon setup and operations
- `06-api-key-authentication.mjs` - API key authentication examples (NEW)

## Documentation

### Core Documentation
- [README.md](README.md) - This file (overview and quick start)
- [AUTHENTICATION.md](AUTHENTICATION.md) - Complete authentication guide
- [SECURITY_INTEGRATION_SUMMARY.md](SECURITY_INTEGRATION_SUMMARY.md) - Security implementation
- [SECURITY_IMPLEMENTATION_VERIFIED.md](SECURITY_IMPLEMENTATION_VERIFIED.md) - Security verification
- [ERROR_PATH_VALIDATION_SUMMARY.md](ERROR_PATH_VALIDATION_SUMMARY.md) - Error handling

### Additional Documentation
- `docs/README.md` - Detailed daemon documentation
- `benchmarks/` - Performance benchmarks
- `test/` - Comprehensive test suite (100% pass rate)

### Migration & Deployment
- [../../docs/MIGRATING_TO_V6.md](../../docs/MIGRATING_TO_V6.md) - v6 migration guide
- [../../docs/SECURITY_MIGRATION.md](../../docs/SECURITY_MIGRATION.md) - Security migration
- [../../docs/deployment/](../../docs/deployment/) - Deployment guides

## Package Exports

```javascript
// Core daemon
export { UnrdfDaemon } from '@unrdf/daemon';
export { TriggerEvaluator } from '@unrdf/daemon';

// Authentication
export { ApiKeyAuthenticator, createAuthMiddleware, createAuthenticator } from '@unrdf/daemon';
export { generateSecureApiKey, hashApiKey, verifyApiKey, generateApiKeyPair } from '@unrdf/daemon';

// Integration modules
export { DaemonDeltaGate } from '@unrdf/daemon';
export { integrateRaftNode, distributeWork } from '@unrdf/daemon';
export { DistributedTaskDistributor } from '@unrdf/daemon';

// Schemas (Zod validation)
export {
  TaskConfigSchema,
  ScheduleConfigSchema,
  TriggerEventSchema,
  DaemonConfigSchema,
  TaskResultSchema,
  DaemonStateSchema,
  ApiKeySchema,
  AuthConfigSchema,
} from '@unrdf/daemon';
```

## Performance

| Operation | Latency | Memory |
|-----------|---------|--------|
| Task execution | <10ms | ~5MB |
| Authentication | <5ms | ~256 bytes |
| Receipt generation | <1ms | ~64 bytes |
| Security validation | <1ms | ~128 bytes |
| SPARQL query (integrated) | ~10ms | ~10MB |

See `benchmarks/` directory for detailed performance measurements.

## Quality Metrics

- **Test Pass Rate**: 100% (all tests passing)
- **Security Validation**: 13/13 modules secured
- **Code Quality**: Zero TODOs, zero skipped tests
- **File Size**: All files under 500 lines (compliance: 100%)
- **Documentation**: JSDoc on 100% of exports
- **Vulnerabilities**: Zero CRITICAL/HIGH CVEs

## License

MIT
