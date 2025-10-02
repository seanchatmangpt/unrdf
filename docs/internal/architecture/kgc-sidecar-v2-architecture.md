# KGC Sidecar Communication Architecture v2

## Executive Summary

This document outlines the redesigned KGC sidecar communication architecture that addresses critical timeout failures blocking 150+ tests. The v2 architecture introduces **graceful degradation**, **circuit breaker patterns**, **health-based service discovery**, and **fallback mechanisms** to ensure system resilience when the sidecar is unavailable.

## Problem Analysis

### Current Architecture Issues

**Symptoms**:
- gRPC endpoint unreachable (localhost:50051)
- Hook listing fails with `DEADLINE_EXCEEDED` errors
- No inter-process communication with sidecar
- All hook-based operations blocked

**Root Causes**:
1. **No Health Check Before Connection**: Client attempts connection without verifying sidecar availability
2. **Insufficient Timeout Handling**: Current 3s timeout with simple retry (no exponential backoff)
3. **No Circuit Breaker**: Repeated failed attempts cause cascading delays
4. **No Fallback Mechanism**: System fails completely when sidecar unavailable
5. **Binary Operation Mode**: Either full gRPC or nothing (no degraded mode)

## Architecture Redesign

### Design Principles

1. **Graceful Degradation**: System continues operating in local-only mode when sidecar unavailable
2. **Fail-Fast**: Quick detection of sidecar unavailability to prevent timeout accumulation
3. **Self-Healing**: Automatic recovery when sidecar becomes available
4. **Observable**: Comprehensive metrics for debugging connection issues
5. **Backward Compatible**: Existing code continues working without changes

### Communication Layer v2

```
┌─────────────────────────────────────────────────────────────┐
│                   CLI Application Layer                      │
└───────────────────┬─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────────────────────────┐
│              Sidecar Client v2 (Facade)                     │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  Smart Connection Manager                             │  │
│  │  • Health Check (100ms timeout, exponential backoff)  │  │
│  │  • Service Discovery (gRPC available?)                │  │
│  │  • Circuit Breaker (3 failures → OPEN for 30s)       │  │
│  └──────────────────────────────────────────────────────┘  │
│                           │                                  │
│         ┌─────────────────┴──────────────────┐              │
│         ▼                                     ▼              │
│  ┌─────────────┐                     ┌─────────────────┐   │
│  │ gRPC Mode   │                     │ Local-Only Mode │   │
│  │ (Preferred) │                     │ (Fallback)      │   │
│  └─────────────┘                     └─────────────────┘   │
└───────────────────────────────────────────────────────────┘
                    │                           │
                    ▼                           ▼
         ┌──────────────────┐        ┌───────────────────┐
         │ KGC Sidecar      │        │ Local Knowledge   │
         │ (gRPC Service)   │        │ Hook Manager      │
         │ Port: 50051      │        │ (Embedded)        │
         └──────────────────┘        └───────────────────┘
```

### State Machine

```
          ┌─────────────┐
          │ INITIALIZING│
          └──────┬──────┘
                 │
        Health Check (100ms)
                 │
         ┌───────┴────────┐
         │                 │
    SUCCESS            TIMEOUT/ERROR
         │                 │
         ▼                 ▼
    ┌─────────┐      ┌──────────┐
    │ HEALTHY │      │ DEGRADED │
    │ (gRPC)  │      │ (Local)  │
    └────┬────┘      └────┬─────┘
         │                 │
         │    3 consecutive│
         │    failures     │
         │    ┌────────────┘
         │    │
         ▼    ▼
    ┌──────────────┐
    │ CIRCUIT_OPEN │
    │ (30s timeout)│
    └──────┬───────┘
           │
    After 30s (HALF_OPEN)
           │
      ┌────┴─────┐
      │          │
  SUCCESS    FAILURE
      │          │
      ▼          ▼
  [HEALTHY]  [CIRCUIT_OPEN]
```

## Core Components

### 1. Smart Health Check System

**Purpose**: Fast, non-blocking health checks with exponential backoff

**Features**:
- **Initial Check**: 100ms timeout (fail-fast)
- **Exponential Backoff**: 100ms → 200ms → 400ms → 800ms → 1600ms (max)
- **Health States**: HEALTHY, DEGRADED, UNHEALTHY, UNKNOWN
- **Probe Types**:
  - **Liveness**: Is sidecar process running?
  - **Readiness**: Can sidecar accept requests?
  - **Startup**: Has sidecar completed initialization?

**Implementation**:
```javascript
// Fast health check with exponential backoff
async healthCheck(attempt = 0) {
  const timeout = Math.min(100 * Math.pow(2, attempt), 1600);

  try {
    const result = await Promise.race([
      this.client.healthCheck(),
      this._timeout(timeout)
    ]);

    return {
      status: 'HEALTHY',
      latency: Date.now() - startTime,
      metadata: result
    };
  } catch (error) {
    if (error.code === 'DEADLINE_EXCEEDED') {
      return this.healthCheck(attempt + 1); // Retry with backoff
    }

    return {
      status: 'UNHEALTHY',
      error: error.message,
      code: error.code
    };
  }
}
```

### 2. Circuit Breaker Pattern

**Purpose**: Prevent cascading failures from repeated connection attempts

**States**:
- **CLOSED**: Normal operation, all requests pass through
- **OPEN**: Circuit tripped (3 failures), reject requests immediately
- **HALF_OPEN**: Testing recovery (after 30s), allow limited requests

**Configuration**:
```javascript
{
  threshold: 3,              // Failures before opening circuit
  resetTimeout: 30000,       // Time before attempting recovery (30s)
  halfOpenRequests: 3,       // Test requests in HALF_OPEN state
  errorThresholdPercentage: 50 // % errors in rolling window
}
```

**Benefits**:
- **Fail-Fast**: Immediate rejection when circuit OPEN (no timeout waits)
- **Self-Healing**: Automatic recovery testing
- **Observability**: State transitions logged and emitted as events

### 3. Connection Pooling v2

**Enhancements**:
- **Lazy Initialization**: Don't create connections until needed
- **Health-Based Eviction**: Remove unhealthy connections proactively
- **Warmup Strategy**: Pre-create connections when sidecar becomes available
- **Adaptive Sizing**: Scale pool based on request volume

**Pool States**:
```javascript
{
  minConnections: 2,         // Minimum idle connections
  maxConnections: 10,        // Maximum total connections
  idleTimeout: 60000,        // 60s idle timeout
  acquireTimeout: 5000,      // 5s acquire timeout
  healthCheckInterval: 30000 // 30s health check interval
}
```

### 4. Graceful Degradation Strategy

**Fallback Modes**:

1. **gRPC Mode (Preferred)**:
   - Full sidecar features
   - Remote hook execution
   - Policy pack validation
   - Distributed observability

2. **Local-Only Mode (Fallback)**:
   - Embedded knowledge hook manager
   - Local SPARQL/SHACL execution
   - File-based policy packs
   - Local observability

**Fallback Triggers**:
- Circuit breaker OPEN
- Health check failure
- Connection timeout
- gRPC UNAVAILABLE error

**Implementation**:
```javascript
async executeOperation(operation, params) {
  // Try gRPC first if circuit allows
  if (this.circuitBreaker.isClosed()) {
    try {
      return await this.circuitBreaker.execute(async () => {
        return await this.grpcClient.execute(operation, params);
      });
    } catch (error) {
      this.metrics.grpcFailures++;
      // Fall through to local mode
    }
  }

  // Fallback to local-only mode
  this.metrics.localFallbacks++;
  return await this.localManager.execute(operation, params);
}
```

### 5. Service Discovery Mechanism

**Purpose**: Dynamically detect sidecar availability

**Discovery Methods**:

1. **Environment Variable**:
   ```bash
   KGC_SIDECAR_ENABLED=true
   KGC_SIDECAR_ENDPOINT=localhost:50051
   ```

2. **Health Check Polling**:
   - Periodic checks every 5s when degraded
   - Immediate check on operation failure
   - Exponential backoff on repeated failures

3. **DNS/Service Registry** (Future):
   - Kubernetes service discovery
   - Consul/etcd integration

**Auto-Recovery**:
```javascript
// Periodic health monitoring
setInterval(async () => {
  if (this.state === 'DEGRADED') {
    const health = await this.healthCheck();

    if (health.status === 'HEALTHY') {
      this.state = 'HEALTHY';
      this.circuitBreaker.reset();
      this.emit('recovered');
    }
  }
}, this.config.healthCheckInterval);
```

## Configuration Management

### Environment Variables

```bash
# KGC Sidecar V2 Configuration
KGC_SIDECAR_ENABLED=true
KGC_SIDECAR_ENDPOINT=localhost:50051
KGC_SIDECAR_TLS_ENABLED=false

# Health Check Configuration
KGC_SIDECAR_HEALTH_CHECK_INTERVAL=5000
KGC_SIDECAR_HEALTH_CHECK_TIMEOUT=100
KGC_SIDECAR_HEALTH_CHECK_RETRIES=3

# Circuit Breaker Configuration
KGC_SIDECAR_CIRCUIT_BREAKER_THRESHOLD=3
KGC_SIDECAR_CIRCUIT_BREAKER_TIMEOUT=30000
KGC_SIDECAR_CIRCUIT_BREAKER_HALF_OPEN_REQUESTS=3

# Fallback Configuration
KGC_SIDECAR_FALLBACK_MODE=local
KGC_SIDECAR_FALLBACK_AUTO_RECOVER=true

# Retry Configuration
KGC_SIDECAR_RETRY_MAX_ATTEMPTS=3
KGC_SIDECAR_RETRY_INITIAL_DELAY=100
KGC_SIDECAR_RETRY_MAX_DELAY=1600
KGC_SIDECAR_RETRY_MULTIPLIER=2

# Connection Pool Configuration
KGC_SIDECAR_POOL_MIN_CONNECTIONS=2
KGC_SIDECAR_POOL_MAX_CONNECTIONS=10
KGC_SIDECAR_POOL_IDLE_TIMEOUT=60000
```

### Configuration Validation

```javascript
import { z } from 'zod';

const SidecarConfigV2Schema = z.object({
  enabled: z.boolean().default(true),
  endpoint: z.string().default('localhost:50051'),
  tls: z.object({
    enabled: z.boolean().default(false),
    ca: z.string().optional(),
    cert: z.string().optional(),
    key: z.string().optional()
  }).optional(),
  healthCheck: z.object({
    interval: z.number().int().positive().default(5000),
    timeout: z.number().int().positive().default(100),
    retries: z.number().int().min(1).max(10).default(3)
  }),
  circuitBreaker: z.object({
    threshold: z.number().int().positive().default(3),
    resetTimeout: z.number().int().positive().default(30000),
    halfOpenRequests: z.number().int().positive().default(3)
  }),
  fallback: z.object({
    mode: z.enum(['local', 'none']).default('local'),
    autoRecover: z.boolean().default(true)
  }),
  retry: z.object({
    maxAttempts: z.number().int().min(1).max(10).default(3),
    initialDelay: z.number().int().positive().default(100),
    maxDelay: z.number().int().positive().default(1600),
    multiplier: z.number().min(1).default(2)
  }),
  pool: z.object({
    minConnections: z.number().int().min(1).default(2),
    maxConnections: z.number().int().min(1).default(10),
    idleTimeout: z.number().int().positive().default(60000)
  })
});
```

## Error Classification

### Error Codes and Handling

| Error Code | gRPC Status | Meaning | Action |
|------------|-------------|---------|--------|
| 4 | DEADLINE_EXCEEDED | Timeout | Retry with backoff |
| 14 | UNAVAILABLE | Connection failed | Open circuit, fallback to local |
| 12 | UNIMPLEMENTED | Method not supported | Log warning, use local implementation |
| 16 | UNAUTHENTICATED | Auth required | Fail request, notify user |
| 13 | INTERNAL | Server error | Retry, increment failure count |

### Error Recovery Strategy

```javascript
async handleError(error, operation, params) {
  switch (error.code) {
    case 4: // DEADLINE_EXCEEDED
      this.metrics.timeouts++;
      return this.retryWithBackoff(operation, params);

    case 14: // UNAVAILABLE
      this.metrics.connectionFailures++;
      this.circuitBreaker.recordFailure();
      return this.fallbackToLocal(operation, params);

    case 12: // UNIMPLEMENTED
      this.logger.warn(`Method ${operation} not implemented by sidecar`);
      return this.fallbackToLocal(operation, params);

    case 16: // UNAUTHENTICATED
      throw new Error(`Authentication required for ${operation}`);

    default:
      this.metrics.unknownErrors++;
      return this.fallbackToLocal(operation, params);
  }
}
```

## Observability and Metrics

### Key Metrics

```javascript
{
  // Connection Metrics
  connectionState: 'HEALTHY' | 'DEGRADED' | 'UNHEALTHY',
  circuitBreakerState: 'CLOSED' | 'OPEN' | 'HALF_OPEN',

  // Request Metrics
  totalRequests: 0,
  grpcRequests: 0,
  localFallbacks: 0,

  // Success/Failure Metrics
  grpcSuccesses: 0,
  grpcFailures: 0,
  localSuccesses: 0,
  localFailures: 0,

  // Error Metrics
  timeouts: 0,
  connectionFailures: 0,
  unknownErrors: 0,

  // Latency Metrics (percentiles)
  grpcLatencyP50: 0,
  grpcLatencyP95: 0,
  grpcLatencyP99: 0,
  localLatencyP50: 0,

  // Health Metrics
  lastHealthCheck: Date,
  healthCheckFailures: 0,
  uptimeSeconds: 0,

  // Circuit Breaker Metrics
  circuitOpenCount: 0,
  circuitOpenDuration: 0,
  lastStateTransition: Date
}
```

### Event Emission

```javascript
// Client emits events for monitoring
client.on('healthStatusChanged', ({ from, to }) => {
  logger.info(`Sidecar health changed: ${from} → ${to}`);
});

client.on('circuitOpened', ({ failures }) => {
  logger.warn(`Circuit breaker opened after ${failures} failures`);
});

client.on('fallbackToLocal', ({ operation, reason }) => {
  logger.info(`Falling back to local mode for ${operation}: ${reason}`);
});

client.on('recovered', () => {
  logger.info('Sidecar connection recovered, switching to gRPC mode');
});
```

## Testing Strategy

### Health Check Validation

```javascript
describe('Health Check System', () => {
  it('completes health check in <100ms when sidecar available', async () => {
    const start = Date.now();
    const result = await client.healthCheck();
    const duration = Date.now() - start;

    expect(result.status).toBe('HEALTHY');
    expect(duration).toBeLessThan(100);
  });

  it('fails fast with exponential backoff when sidecar unavailable', async () => {
    // Mock sidecar unavailable
    const result = await client.healthCheck();

    expect(result.status).toBe('UNHEALTHY');
    expect(result.error).toContain('UNAVAILABLE');
  });
});
```

### Circuit Breaker Validation

```javascript
describe('Circuit Breaker', () => {
  it('opens after 3 consecutive failures', async () => {
    // Trigger 3 failures
    for (let i = 0; i < 3; i++) {
      await client.execute('listHooks').catch(() => {});
    }

    expect(client.circuitBreaker.getState()).toBe('OPEN');
  });

  it('transitions to HALF_OPEN after reset timeout', async () => {
    // Open circuit
    client.circuitBreaker.forceOpen();

    // Wait 30s
    await new Promise(resolve => setTimeout(resolve, 30000));

    expect(client.circuitBreaker.getState()).toBe('HALF_OPEN');
  });
});
```

### Graceful Degradation Validation

```javascript
describe('Graceful Degradation', () => {
  it('falls back to local mode when gRPC unavailable', async () => {
    // Simulate sidecar unavailable
    mockSidecarUnavailable();

    const result = await client.listHooks();

    expect(result).toBeDefined();
    expect(client.metrics.localFallbacks).toBeGreaterThan(0);
  });

  it('recovers to gRPC mode when sidecar becomes available', async () => {
    // Start in degraded mode
    mockSidecarUnavailable();
    await client.execute('listHooks');

    // Make sidecar available
    mockSidecarAvailable();

    // Wait for health check
    await new Promise(resolve => setTimeout(resolve, 6000));

    expect(client.state).toBe('HEALTHY');
  });
});
```

## Performance Targets

### Success Metrics

| Metric | Target | Rationale |
|--------|--------|-----------|
| Health check latency | <100ms | Fast failure detection |
| Circuit open detection | <10ms | Immediate rejection when open |
| Fallback to local | <50ms | Seamless degradation |
| Recovery detection | <6s | Health check interval (5s) + margin |
| gRPC request latency (p99) | <2s | Existing target from KGC architecture |
| Local fallback latency (p99) | <500ms | No network overhead |

### Validation Criteria

**✅ System is READY when**:
1. Health check completes in <100ms
2. Circuit breaker opens after 3 failures
3. Local-only fallback works for all operations
4. Connection pooling reuses connections
5. Auto-recovery works when sidecar becomes available

## Migration Strategy

See [KGC Sidecar Migration Plan](./sidecar-migration-plan.md) for detailed migration steps.

### Phase 1: Parallel Implementation (Week 1)
- Implement v2 client alongside existing client
- Feature flag: `KGC_SIDECAR_V2_ENABLED=false`
- No breaking changes

### Phase 2: Testing and Validation (Week 2)
- Enable v2 in test environment
- Validate all 150+ sidecar tests pass
- Performance benchmarking

### Phase 3: Gradual Rollout (Week 3)
- Enable v2 for 10% of operations
- Monitor metrics and error rates
- Increase to 50%, 100%

### Phase 4: Cleanup (Week 4)
- Remove v1 client code
- Update documentation
- Archive old tests

## Security Considerations

### TLS Support
- mTLS for production deployments
- Certificate rotation without restart
- Cipher suite configuration

### Authentication
- Service account tokens
- API key management
- Rate limiting per client

### Network Security
- Firewall rules (only localhost:50051 or specific IPs)
- Network policy enforcement (Kubernetes)
- Audit logging of connection attempts

## Operational Runbook

### Debugging Connection Failures

1. **Check sidecar is running**:
   ```bash
   ps aux | grep "unrdf sidecar"
   netstat -an | grep 50051
   ```

2. **Test health endpoint**:
   ```bash
   grpcurl -plaintext localhost:50051 health/check
   ```

3. **Check circuit breaker state**:
   ```javascript
   const metrics = client.getClientMetrics();
   console.log(metrics.circuitBreaker.state);
   ```

4. **Force local-only mode**:
   ```bash
   export KGC_SIDECAR_FALLBACK_MODE=local
   export KGC_SIDECAR_ENABLED=false
   ```

### Monitoring Alerts

**Critical Alerts**:
- Circuit breaker opened for >5 minutes
- Health check failures >90%
- Fallback mode active >10 minutes

**Warning Alerts**:
- gRPC latency p99 >2s
- Connection pool exhaustion
- Error rate >10%

## Future Enhancements

### v2.1 (Q2 2025)
- HTTP/REST fallback (alternative to gRPC)
- Service mesh integration (Istio, Linkerd)
- Multi-sidecar load balancing

### v2.2 (Q3 2025)
- WebSocket streaming for real-time hooks
- GraphQL API for flexible queries
- Distributed tracing with OpenTelemetry

### v3.0 (Q4 2025)
- Zero-downtime sidecar upgrades
- Federated policy pack resolution
- Edge caching for hook definitions

## References

- [KGC Sidecar Architecture v1](./kgc-sidecar-architecture.md)
- [Circuit Breaker Pattern - Martin Fowler](https://martinfowler.com/bliki/CircuitBreaker.html)
- [gRPC Health Checking Protocol](https://github.com/grpc/grpc/blob/master/doc/health-checking.md)
- [Resilience4j Circuit Breaker](https://resilience4j.readme.io/docs/circuitbreaker)
- [Kubernetes Liveness, Readiness, and Startup Probes](https://kubernetes.io/docs/tasks/configure-pod-container/configure-liveness-readiness-startup-probes/)

## Appendix: Error Code Reference

### gRPC Status Codes
```javascript
const GRPC_STATUS_CODES = {
  OK: 0,
  CANCELLED: 1,
  UNKNOWN: 2,
  INVALID_ARGUMENT: 3,
  DEADLINE_EXCEEDED: 4,
  NOT_FOUND: 5,
  ALREADY_EXISTS: 6,
  PERMISSION_DENIED: 7,
  RESOURCE_EXHAUSTED: 8,
  FAILED_PRECONDITION: 9,
  ABORTED: 10,
  OUT_OF_RANGE: 11,
  UNIMPLEMENTED: 12,
  INTERNAL: 13,
  UNAVAILABLE: 14,
  DATA_LOSS: 15,
  UNAUTHENTICATED: 16
};
```
