# KGC Sidecar Integration Design

## Executive Summary

This document specifies the integration architecture between UNRDF's enterprise CLI and the KGC JavaScript Sidecar, enabling seamless knowledge graph operations with transactional integrity, policy enforcement, and cryptographic audit trails.

## Integration Architecture

### High-Level Overview

```
┌──────────────────────────────────────────────────────────────────┐
│                         UNRDF CLI                                │
│                                                                  │
│  ┌────────────┐  ┌────────────┐  ┌────────────┐               │
│  │ Graph Cmds │  │ Hook Cmds  │  │Policy Cmds │               │
│  └─────┬──────┘  └─────┬──────┘  └─────┬──────┘               │
│        │                │                │                       │
│  ┌─────▼────────────────▼────────────────▼─────┐               │
│  │         Sidecar Client Abstraction           │               │
│  └─────┬────────────────┬────────────────┬──────┘               │
│        │                │                │                       │
│        │     ┌──────────▼────────┐      │                       │
│        │     │  Health Monitor   │      │                       │
│        │     │  & Auto-Retry     │      │                       │
│        │     └──────────┬────────┘      │                       │
└────────┼────────────────┼───────────────┼───────────────────────┘
         │                │               │
    ┌────▼────┐      ┌────▼────┐    ┌────▼────┐
    │  gRPC   │      │  HTTP   │    │  IPC    │
    │ Client  │      │ Client  │    │ Client  │
    └────┬────┘      └────┬────┘    └────┬────┘
         │                │               │
         └────────────────▼───────────────┘
                          │
         ┌────────────────▼────────────────┐
         │      KGC JavaScript Sidecar      │
         │                                  │
         │  ┌──────────┐  ┌──────────────┐│
         │  │Transaction│  │  Knowledge   ││
         │  │  Manager  │  │Hook Manager  ││
         │  └──────────┘  └──────────────┘│
         │  ┌──────────┐  ┌──────────────┐│
         │  │ Policy   │  │  Lockchain   ││
         │  │  Pack    │  │   Writer     ││
         │  └──────────┘  └──────────────┘│
         └─────────────────────────────────┘
```

## Sidecar Client Interface

### 1. Client Abstraction Layer

**Purpose**: Unified interface supporting multiple transport protocols

```javascript
// src/cli/sidecar/client.mjs
import { z } from 'zod';

export const SidecarConfigSchema = z.object({
  protocol: z.enum(['grpc', 'http', 'ipc']).default('grpc'),
  host: z.string().default('localhost'),
  port: z.number().default(50051),
  tls: z.boolean().default(false),
  cert: z.string().optional(),
  timeout: z.number().default(30000),
  retryAttempts: z.number().default(3),
  retryDelay: z.number().default(1000)
});

export class SidecarClient {
  constructor(config) {
    this.config = SidecarConfigSchema.parse(config);
    this.transport = this.createTransport();
    this.healthMonitor = new HealthMonitor(this);
  }

  createTransport() {
    switch (this.config.protocol) {
      case 'grpc':
        return new GRPCTransport(this.config);
      case 'http':
        return new HTTPTransport(this.config);
      case 'ipc':
        return new IPCTransport(this.config);
      default:
        throw new Error(`Unsupported protocol: ${this.config.protocol}`);
    }
  }

  // Transaction operations
  async applyTransaction(delta, options = {}) {
    return await this.transport.call('transaction.apply', {
      delta,
      options
    });
  }

  async getTransactionHistory(filter = {}) {
    return await this.transport.call('transaction.history', filter);
  }

  async rollbackTransaction(transactionId) {
    return await this.transport.call('transaction.rollback', {
      transactionId
    });
  }

  // Hook operations
  async registerHook(hook) {
    return await this.transport.call('hook.register', { hook });
  }

  async evaluateHook(hookId, data) {
    return await this.transport.call('hook.evaluate', {
      hookId,
      data
    });
  }

  async listHooks(filter = {}) {
    return await this.transport.call('hook.list', filter);
  }

  async getHookHistory(hookId, limit = 100) {
    return await this.transport.call('hook.history', {
      hookId,
      limit
    });
  }

  // Policy operations
  async applyPolicyPack(pack) {
    return await this.transport.call('policy.apply', { pack });
  }

  async activatePolicyPack(packId) {
    return await this.transport.call('policy.activate', { packId });
  }

  async deactivatePolicyPack(packId) {
    return await this.transport.call('policy.deactivate', { packId });
  }

  async listPolicyPacks() {
    return await this.transport.call('policy.list');
  }

  // Store operations
  async importData(data, format) {
    return await this.transport.call('store.import', {
      data,
      format
    });
  }

  async exportData(format) {
    return await this.transport.call('store.export', { format });
  }

  async executeQuery(query) {
    return await this.transport.call('store.query', { query });
  }

  async getStats() {
    return await this.transport.call('store.stats');
  }

  // Lockchain operations
  async getLockchainReceipts(filter = {}) {
    return await this.transport.call('lockchain.list', filter);
  }

  async verifyLockchain() {
    return await this.transport.call('lockchain.verify');
  }

  async exportLockchain(format) {
    return await this.transport.call('lockchain.export', { format });
  }

  // Health and status
  async health() {
    return await this.transport.call('health.check');
  }

  async status() {
    return await this.transport.call('status.get');
  }

  async metrics() {
    return await this.transport.call('metrics.get');
  }

  async config(operation, key, value) {
    return await this.transport.call('config.manage', {
      operation,
      key,
      value
    });
  }
}
```

### 2. Transport Layer Implementations

#### gRPC Transport

```javascript
// src/cli/sidecar/transports/grpc.mjs
import * as grpc from '@grpc/grpc-js';
import * as protoLoader from '@grpc/proto-loader';

export class GRPCTransport {
  constructor(config) {
    this.config = config;
    this.client = null;
  }

  async connect() {
    // Load proto definitions
    const packageDefinition = protoLoader.loadSync(
      './proto/kgc-sidecar.proto',
      {
        keepCase: true,
        longs: String,
        enums: String,
        defaults: true,
        oneofs: true
      }
    );

    const proto = grpc.loadPackageDefinition(packageDefinition);

    // Create credentials
    const credentials = this.config.tls
      ? grpc.credentials.createSsl(fs.readFileSync(this.config.cert))
      : grpc.credentials.createInsecure();

    // Create client
    this.client = new proto.KGCSidecar(
      `${this.config.host}:${this.config.port}`,
      credentials
    );
  }

  async call(method, params) {
    if (!this.client) {
      await this.connect();
    }

    return new Promise((resolve, reject) => {
      const deadline = Date.now() + this.config.timeout;

      this.client[method](params, { deadline }, (error, response) => {
        if (error) {
          reject(new ConnectionError(`gRPC call failed: ${error.message}`, method));
        } else {
          resolve(response);
        }
      });
    });
  }

  async close() {
    if (this.client) {
      this.client.close();
      this.client = null;
    }
  }
}
```

#### HTTP Transport

```javascript
// src/cli/sidecar/transports/http.mjs
export class HTTPTransport {
  constructor(config) {
    this.config = config;
    this.baseURL = `${config.tls ? 'https' : 'http'}://${config.host}:${config.port}`;
  }

  async call(method, params) {
    const [resource, action] = method.split('.');
    const url = `${this.baseURL}/api/v1/${resource}/${action}`;

    const response = await fetch(url, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${this.config.token || ''}`
      },
      body: JSON.stringify(params),
      signal: AbortSignal.timeout(this.config.timeout)
    });

    if (!response.ok) {
      throw new ConnectionError(
        `HTTP ${response.status}: ${response.statusText}`,
        method
      );
    }

    return await response.json();
  }
}
```

#### IPC Transport

```javascript
// src/cli/sidecar/transports/ipc.mjs
import { createConnection } from 'node:net';

export class IPCTransport {
  constructor(config) {
    this.config = config;
    this.socket = null;
  }

  async connect() {
    return new Promise((resolve, reject) => {
      this.socket = createConnection(this.config.socketPath);

      this.socket.on('connect', () => resolve());
      this.socket.on('error', (error) => reject(error));
    });
  }

  async call(method, params) {
    if (!this.socket) {
      await this.connect();
    }

    return new Promise((resolve, reject) => {
      const request = JSON.stringify({
        method,
        params,
        id: Date.now()
      });

      this.socket.write(request + '\n');

      const timeout = setTimeout(() => {
        reject(new Error('IPC timeout'));
      }, this.config.timeout);

      this.socket.once('data', (data) => {
        clearTimeout(timeout);
        const response = JSON.parse(data.toString());

        if (response.error) {
          reject(new Error(response.error));
        } else {
          resolve(response.result);
        }
      });
    });
  }

  async close() {
    if (this.socket) {
      this.socket.end();
      this.socket = null;
    }
  }
}
```

### 3. Health Monitoring & Auto-Retry

```javascript
// src/cli/sidecar/health-monitor.mjs
export class HealthMonitor {
  constructor(client, options = {}) {
    this.client = client;
    this.checkInterval = options.checkInterval || 30000;
    this.unhealthyThreshold = options.unhealthyThreshold || 3;
    this.healthyThreshold = options.healthyThreshold || 2;

    this.consecutiveFailures = 0;
    this.consecutiveSuccesses = 0;
    this.isHealthy = true;
    this.intervalId = null;
  }

  start() {
    this.intervalId = setInterval(() => {
      this.check();
    }, this.checkInterval);
  }

  stop() {
    if (this.intervalId) {
      clearInterval(this.intervalId);
      this.intervalId = null;
    }
  }

  async check() {
    try {
      const result = await this.client.health();

      if (result.status === 'healthy') {
        this.consecutiveSuccesses++;
        this.consecutiveFailures = 0;

        if (!this.isHealthy && this.consecutiveSuccesses >= this.healthyThreshold) {
          this.isHealthy = true;
          console.log('✅ Sidecar is healthy');
        }
      } else {
        this.handleFailure();
      }
    } catch (error) {
      this.handleFailure();
    }
  }

  handleFailure() {
    this.consecutiveFailures++;
    this.consecutiveSuccesses = 0;

    if (this.isHealthy && this.consecutiveFailures >= this.unhealthyThreshold) {
      this.isHealthy = false;
      console.error('❌ Sidecar is unhealthy');
    }
  }

  async withRetry(operation, maxAttempts = 3) {
    let lastError;

    for (let attempt = 1; attempt <= maxAttempts; attempt++) {
      try {
        return await operation();
      } catch (error) {
        lastError = error;

        if (attempt < maxAttempts) {
          const delay = Math.min(1000 * Math.pow(2, attempt - 1), 10000);
          console.log(`⏳ Retrying in ${delay}ms... (attempt ${attempt}/${maxAttempts})`);
          await new Promise(resolve => setTimeout(resolve, delay));
        }
      }
    }

    throw new ConnectionError(
      `Operation failed after ${maxAttempts} attempts: ${lastError.message}`,
      'retry'
    );
  }
}
```

### 4. Telemetry & Observability Integration

```javascript
// src/cli/sidecar/telemetry.mjs
import { trace, context, SpanStatusCode } from '@opentelemetry/api';

export class SidecarTelemetry {
  constructor(serviceName = 'unrdf-cli') {
    this.tracer = trace.getTracer(serviceName);
  }

  async traceOperation(name, operation, attributes = {}) {
    const span = this.tracer.startSpan(name, {
      attributes: {
        'sidecar.operation': name,
        ...attributes
      }
    });

    try {
      const result = await context.with(
        trace.setSpan(context.active(), span),
        operation
      );

      span.setStatus({ code: SpanStatusCode.OK });
      return result;

    } catch (error) {
      span.setStatus({
        code: SpanStatusCode.ERROR,
        message: error.message
      });
      span.recordException(error);
      throw error;

    } finally {
      span.end();
    }
  }

  recordMetric(name, value, labels = {}) {
    // Record metric via OpenTelemetry metrics API
    const meter = metrics.getMeter('unrdf-cli');
    const counter = meter.createCounter(name);
    counter.add(value, labels);
  }
}
```

## Failover & Resilience Patterns

### 1. Circuit Breaker

```javascript
// src/cli/sidecar/circuit-breaker.mjs
export class CircuitBreaker {
  constructor(options = {}) {
    this.failureThreshold = options.failureThreshold || 5;
    this.resetTimeout = options.resetTimeout || 60000;
    this.state = 'CLOSED';
    this.failures = 0;
    this.nextAttempt = Date.now();
  }

  async execute(operation) {
    if (this.state === 'OPEN') {
      if (Date.now() < this.nextAttempt) {
        throw new Error('Circuit breaker is OPEN');
      }
      this.state = 'HALF_OPEN';
    }

    try {
      const result = await operation();
      this.onSuccess();
      return result;
    } catch (error) {
      this.onFailure();
      throw error;
    }
  }

  onSuccess() {
    this.failures = 0;
    this.state = 'CLOSED';
  }

  onFailure() {
    this.failures++;
    if (this.failures >= this.failureThreshold) {
      this.state = 'OPEN';
      this.nextAttempt = Date.now() + this.resetTimeout;
      console.error('⚠️ Circuit breaker opened');
    }
  }
}
```

### 2. Graceful Degradation

```javascript
// src/cli/sidecar/degradation.mjs
export class GracefulDegradation {
  constructor(client) {
    this.client = client;
    this.fallbackStore = new Map();
  }

  async withFallback(operation, fallback) {
    try {
      return await operation();
    } catch (error) {
      console.warn('⚠️ Operation failed, using fallback');
      return await fallback();
    }
  }

  async cachedOperation(key, operation, ttl = 300000) {
    // Try cache first
    const cached = this.fallbackStore.get(key);
    if (cached && Date.now() < cached.expires) {
      return cached.value;
    }

    // Execute operation
    try {
      const result = await operation();
      this.fallbackStore.set(key, {
        value: result,
        expires: Date.now() + ttl
      });
      return result;
    } catch (error) {
      // Return stale cache if available
      if (cached) {
        console.warn('⚠️ Using stale cache due to error');
        return cached.value;
      }
      throw error;
    }
  }
}
```

## Command Integration Examples

### Graph Commands with Sidecar

```javascript
// src/cli/commands/graph/import.mjs
export const GraphImportHandler = {
  schema: z.object({
    file: z.string(),
    graph: z.string(),
    format: z.enum(['turtle', 'nquads', 'jsonld']).default('turtle')
  }),

  async execute(ctx, args, flags) {
    const { file, graph, format } = flags;
    const sidecar = ctx.sidecarClient;

    // Read file
    const data = await fs.readFile(file, 'utf-8');

    // Create transaction delta
    const delta = {
      additions: await parseRDF(data, format),
      deletions: [],
      graph: graph || 'default'
    };

    // Apply via sidecar transaction manager
    const result = await sidecar.applyTransaction(delta, {
      actor: ctx.user || 'cli',
      description: `Import ${file} to ${graph}`,
      metadata: {
        source: file,
        format,
        timestamp: new Date().toISOString()
      }
    });

    return {
      success: result.receipt.committed,
      transactionId: result.receipt.transactionId,
      triplesAdded: delta.additions.length,
      hash: result.receipt.hash
    };
  }
};
```

### Hook Commands with Sidecar

```javascript
// src/cli/commands/hook/eval.mjs
export const HookEvalHandler = {
  schema: z.object({
    hook: z.string(),
    data: z.string().optional()
  }),

  async execute(ctx, args, flags) {
    const { hook, data } = flags;
    const sidecar = ctx.sidecarClient;

    // Load hook definition
    const hookDef = await loadHook(hook);

    // Load data if provided
    let store;
    if (data) {
      store = await loadStore(data);
    } else {
      // Use current store
      store = await sidecar.exportData('nquads');
    }

    // Evaluate hook via sidecar
    const result = await sidecar.evaluateHook(hookDef.id, {
      store,
      timestamp: Date.now()
    });

    return {
      hookId: hookDef.id,
      hookName: hookDef.meta.name,
      result: result.fired ? 'FIRED' : 'NOT_FIRED',
      condition: result.condition,
      effects: result.effects,
      duration: result.duration,
      timestamp: result.timestamp
    };
  }
};
```

### Policy Commands with Sidecar

```javascript
// src/cli/commands/policy/apply.mjs
export const PolicyApplyHandler = {
  schema: z.object({
    file: z.string()
  }),

  async execute(ctx, args, flags) {
    const { file } = flags;
    const sidecar = ctx.sidecarClient;

    // Load policy pack
    const pack = JSON.parse(await fs.readFile(file, 'utf-8'));

    // Validate policy pack
    validatePolicyPack(pack);

    // Apply to sidecar
    const result = await sidecar.applyPolicyPack(pack);

    return {
      packId: result.id,
      packName: pack.meta.name,
      version: pack.meta.version,
      hooksRegistered: result.hooksRegistered,
      status: 'applied'
    };
  }
};
```

## Protocol Buffers Definition

```protobuf
// proto/kgc-sidecar.proto
syntax = "proto3";

package kgc;

service KGCSidecar {
  // Transaction operations
  rpc ApplyTransaction(TransactionRequest) returns (TransactionReceipt);
  rpc GetTransactionHistory(HistoryFilter) returns (TransactionList);
  rpc RollbackTransaction(RollbackRequest) returns (RollbackResponse);

  // Hook operations
  rpc RegisterHook(HookDefinition) returns (HookRegistration);
  rpc EvaluateHook(HookEvalRequest) returns (HookEvalResponse);
  rpc ListHooks(HookFilter) returns (HookList);
  rpc GetHookHistory(HookHistoryRequest) returns (HookHistoryResponse);

  // Policy operations
  rpc ApplyPolicyPack(PolicyPack) returns (PolicyPackResponse);
  rpc ActivatePolicyPack(PolicyPackId) returns (PolicyPackStatus);
  rpc DeactivatePolicyPack(PolicyPackId) returns (PolicyPackStatus);
  rpc ListPolicyPacks(Empty) returns (PolicyPackList);

  // Store operations
  rpc ImportData(ImportRequest) returns (ImportResponse);
  rpc ExportData(ExportRequest) returns (ExportResponse);
  rpc ExecuteQuery(QueryRequest) returns (QueryResponse);
  rpc GetStats(Empty) returns (StoreStats);

  // Lockchain operations
  rpc GetLockchainReceipts(LockchainFilter) returns (ReceiptList);
  rpc VerifyLockchain(Empty) returns (VerificationResult);
  rpc ExportLockchain(ExportRequest) returns (ExportResponse);

  // Health and status
  rpc HealthCheck(Empty) returns (HealthStatus);
  rpc GetStatus(Empty) returns (SidecarStatus);
  rpc GetMetrics(Empty) returns (MetricsResponse);
  rpc ManageConfig(ConfigRequest) returns (ConfigResponse);
}

message TransactionRequest {
  Delta delta = 1;
  TransactionOptions options = 2;
}

message Delta {
  repeated Quad additions = 1;
  repeated Quad deletions = 2;
  string graph = 3;
}

message TransactionReceipt {
  string transactionId = 1;
  bool committed = 2;
  string hash = 3;
  int64 timestamp = 4;
  map<string, string> metadata = 5;
}

// ... more message definitions
```

## Deployment Configurations

### Development
```yaml
# ~/.unrdf/config
contexts:
  - name: development
    sidecar:
      protocol: http
      host: localhost
      port: 3000
      tls: false
    defaults:
      verbose: true
      format: json
```

### Production
```yaml
contexts:
  - name: production
    sidecar:
      protocol: grpc
      host: kgc-sidecar.prod.svc.cluster.local
      port: 50051
      tls: true
      cert: /etc/tls/ca.crt
    defaults:
      verbose: false
      format: table
    auth:
      type: token
      credentials:
        token: ${UNRDF_PROD_TOKEN}
```

## Conclusion

This sidecar integration design provides:
- **Protocol flexibility**: gRPC, HTTP, IPC support
- **Resilience**: Health monitoring, auto-retry, circuit breaker
- **Observability**: Full telemetry integration
- **Security**: TLS support, token authentication
- **Performance**: Connection pooling, request caching

The design enables seamless CLI-to-sidecar communication while maintaining enterprise-grade reliability and security.

**Status**: ✅ SIDECAR INTEGRATION COMPLETE - Ready for knowledge engine integration
