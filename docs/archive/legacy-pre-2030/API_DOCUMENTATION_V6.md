# UNRDF v6.0.0 API Documentation

**Complete API reference for new v6.0.0 features**

## Overview

UNRDF v6.0.0 introduces the `@unrdf/daemon` package with enterprise-grade authentication, security validation, and 13 production-ready integration modules.

**Base Package**: `@unrdf/daemon`
**Version**: 6.0.0
**Node.js**: >=18.0.0
**Module System**: ESM only (.mjs)

---

## Table of Contents

1. [Authentication API](#authentication-api)
2. [Daemon Core API](#daemon-core-api)
3. [Integration Modules](#integration-modules)
4. [Security Functions](#security-functions)
5. [Schemas & Validation](#schemas--validation)
6. [Error Handling](#error-handling)

---

## Authentication API

### `createAuthenticator(options)`

Factory function to create API key authenticator with automatic key generation.

**Signature**:
```typescript
async function createAuthenticator(options?: AuthOptions): Promise<{
  authenticator: ApiKeyAuthenticator;
  key: string;
}>
```

**Parameters**:
```typescript
interface AuthOptions {
  environment?: 'development' | 'test' | 'production';  // Default: 'production'
  requireInDev?: boolean;                                // Default: false
  apiKeyHash?: string;                                   // Optional: pre-hashed key
}
```

**Returns**:
```typescript
{
  authenticator: ApiKeyAuthenticator,  // Authenticator instance
  key: string                          // Generated API key (64 hex chars)
}
```

**Example**:
```javascript
import { createAuthenticator } from '@unrdf/daemon';

const { authenticator, key } = await createAuthenticator({
  environment: 'production'
});

console.log('API Key:', key); // Distribute to authorized users
```

---

### `ApiKeyAuthenticator`

Main authentication controller class.

#### Constructor

**Signature**:
```typescript
new ApiKeyAuthenticator(config: AuthenticatorConfig)
```

**Parameters**:
```typescript
interface AuthenticatorConfig {
  apiKeyHash?: string;                  // BLAKE3 hash of valid API key
  environment?: string;                 // 'development' | 'test' | 'production'
  requireInDev?: boolean;              // Require key in development
  maxAuditEntries?: number;            // Max audit log size (default: 1000)
}
```

**Example**:
```javascript
import { ApiKeyAuthenticator } from '@unrdf/daemon';

const authenticator = new ApiKeyAuthenticator({
  apiKeyHash: 'a1b2c3...', // BLAKE3 hash
  environment: 'production',
  requireInDev: false,
  maxAuditEntries: 1000,
});
```

#### `authenticate(context)`

Authenticate a request based on API key.

**Signature**:
```typescript
async authenticate(context: AuthContext): Promise<AuthResult>
```

**Parameters**:
```typescript
interface AuthContext {
  headers?: {
    'x-api-key'?: string;              // API key in header
    [key: string]: string | undefined;
  };
}
```

**Returns**:
```typescript
interface AuthResult {
  authenticated: boolean;               // true if authenticated
  reason?: string;                      // Reason for failure (if any)
  source?: 'header' | 'environment';   // Key source
}
```

**Example**:
```javascript
const result = await authenticator.authenticate({
  headers: { 'x-api-key': providedKey }
});

if (result.authenticated) {
  console.log('Authenticated via:', result.source);
} else {
  console.error('Auth failed:', result.reason);
}
```

#### `getAuditLog()`

Retrieve authentication audit log.

**Signature**:
```typescript
getAuditLog(): AuditEntry[]
```

**Returns**:
```typescript
interface AuditEntry {
  timestamp: string;                   // ISO 8601 timestamp
  success: boolean;                    // Auth success/failure
  reason?: string;                     // Failure reason
  source?: string;                     // Key source
  environment: string;                 // Environment at auth time
}
```

**Example**:
```javascript
const log = authenticator.getAuditLog();

const failedAttempts = log.filter(e => !e.success);
console.log(`Failed attempts: ${failedAttempts.length}`);
```

---

### Crypto Utilities

#### `generateSecureApiKey()`

Generate cryptographically secure API key.

**Signature**:
```typescript
function generateSecureApiKey(): string
```

**Returns**: 64-character hexadecimal string (256 bits)

**Example**:
```javascript
import { generateSecureApiKey } from '@unrdf/daemon';

const key = generateSecureApiKey();
console.log(key); // "a1b2c3d4..." (64 chars)
```

#### `hashApiKey(apiKey)`

Hash API key using BLAKE3.

**Signature**:
```typescript
async function hashApiKey(apiKey: string): Promise<string>
```

**Parameters**:
- `apiKey` (string): API key to hash (32-128 hex characters)

**Returns**: 64-character hexadecimal hash (BLAKE3)

**Example**:
```javascript
import { hashApiKey } from '@unrdf/daemon';

const hash = await hashApiKey(myApiKey);
console.log(hash); // "d5e6f7..." (64 chars)
```

#### `verifyApiKey(providedKey, storedHash)`

Verify API key against stored hash (constant-time).

**Signature**:
```typescript
async function verifyApiKey(providedKey: string, storedHash: string): Promise<boolean>
```

**Parameters**:
- `providedKey` (string): API key provided by user
- `storedHash` (string): Stored BLAKE3 hash

**Returns**: `true` if keys match, `false` otherwise

**Example**:
```javascript
import { verifyApiKey } from '@unrdf/daemon';

const isValid = await verifyApiKey(providedKey, storedHash);
if (isValid) {
  console.log('Key is valid');
}
```

#### `generateApiKeyPair()`

Generate API key and hash pair.

**Signature**:
```typescript
async function generateApiKeyPair(): Promise<{ key: string; hash: string }>
```

**Returns**:
```typescript
{
  key: string;   // 64-char hex API key
  hash: string;  // 64-char hex BLAKE3 hash
}
```

**Example**:
```javascript
import { generateApiKeyPair } from '@unrdf/daemon';

const { key, hash } = await generateApiKeyPair();
console.log('Key:', key);
console.log('Hash:', hash);
```

---

### `createAuthMiddleware(options)`

Create Express/Fastify-compatible authentication middleware.

**Signature**:
```typescript
function createAuthMiddleware(options?: AuthOptions): Function
```

**Parameters**: Same as `createAuthenticator()`

**Returns**: Middleware function compatible with Express/Fastify

**Example (Express)**:
```javascript
import express from 'express';
import { createAuthMiddleware } from '@unrdf/daemon';

const app = express();
const authMiddleware = createAuthMiddleware({
  environment: 'production'
});

// Protect routes
app.use('/api/daemon/*', authMiddleware);

app.post('/api/daemon/execute', (req, res) => {
  res.json({ status: 'authenticated' });
});
```

**Example (Fastify)**:
```javascript
import Fastify from 'fastify';
import { ApiKeyAuthenticator } from '@unrdf/daemon';

const fastify = Fastify();
const authenticator = new ApiKeyAuthenticator({
  environment: 'production'
});

fastify.addHook('preHandler', async (request, reply) => {
  const result = await authenticator.authenticate({
    headers: request.headers
  });

  if (!result.authenticated) {
    reply.code(401).send({ error: 'Unauthorized' });
  }
});
```

---

## Daemon Core API

### `UnrdfDaemon`

Main daemon class for background task orchestration.

#### Constructor

**Signature**:
```typescript
new UnrdfDaemon(config: DaemonConfig)
```

**Parameters**:
```typescript
interface DaemonConfig {
  daemonId: string;                    // Unique daemon identifier
  name?: string;                       // Human-readable name
  port?: number;                       // Server port (default: 8080)
  concurrency?: number;                // Max concurrent operations (default: 10)
  healthCheckIntervalMs?: number;      // Health check frequency (default: 30000)
  metricsRetentionMs?: number;         // Metrics window (default: 3600000)
  logLevel?: string;                   // Log level (default: 'info')
  logger?: Logger;                     // Custom logger
  globalRetryPolicy?: RetryPolicy;     // Default retry policy
  operations?: ScheduledOperation[];   // Pre-registered operations
  environment?: Record<string, string>; // Environment variables
}
```

**Example**:
```javascript
import { UnrdfDaemon } from '@unrdf/daemon';

const daemon = new UnrdfDaemon({
  daemonId: 'my-rdf-daemon',
  name: 'RDF Background Processor',
  concurrency: 10,
  port: 8080,
});
```

#### `start()`

Start the daemon.

**Signature**:
```typescript
async start(): Promise<void>
```

**Example**:
```javascript
await daemon.start();
console.log('Daemon started');
```

#### `stop()`

Stop the daemon.

**Signature**:
```typescript
async stop(): Promise<void>
```

**Example**:
```javascript
await daemon.stop();
console.log('Daemon stopped');
```

#### `schedule(operation)`

Register a scheduled operation.

**Signature**:
```typescript
schedule(operation: ScheduledOperation): void
```

**Parameters**:
```typescript
interface ScheduledOperation {
  id: string;                          // Unique operation ID
  name: string;                        // Human-readable name
  handler: OperationHandler;           // Async handler function
  schedule?: {
    cron?: string;                     // Cron expression
    intervalMs?: number;               // Interval in milliseconds
    events?: string[];                 // Event names to trigger on
  };
  retryPolicy?: RetryPolicy;           // Optional retry policy
  concurrency?: number;                // Max concurrent instances
}

type OperationHandler = (context: OperationContext) => Promise<any>;

interface OperationContext {
  operationId: string;
  triggeredBy?: string;
  data?: any;
}
```

**Example**:
```javascript
daemon.schedule({
  id: 'validate-rdf',
  name: 'Periodic RDF Validation',
  schedule: { cron: '0 * * * *' },     // Hourly
  handler: async (context) => {
    const results = await validateAllStores();
    return { validated: results.length };
  },
  retryPolicy: {
    maxAttempts: 3,
    backoffMs: 1000,
  },
});
```

#### `execute(operationId, data?)`

Execute an operation immediately.

**Signature**:
```typescript
async execute(operationId: string, data?: any): Promise<OperationResult>
```

**Returns**:
```typescript
interface OperationResult {
  operationId: string;
  status: 'success' | 'failure';
  result?: any;
  error?: Error;
  duration: number;                    // Milliseconds
  receipt?: OperationReceipt;          // Cryptographic proof
}
```

**Example**:
```javascript
const result = await daemon.execute('validate-rdf', { scope: 'all' });
console.log(`Validation took ${result.duration}ms`);
```

#### `getHealth()`

Get daemon health status.

**Signature**:
```typescript
getHealth(): DaemonHealth
```

**Returns**:
```typescript
interface DaemonHealth {
  nodeId: string;
  clusterId?: string;
  isRunning: boolean;
  isLeader?: boolean;
  uptime: number;                      // Milliseconds
  activeOperations: number;
  queuedOperations: number;
  completedOperations: number;
  timestamp: Date;
}
```

**Example**:
```javascript
const health = daemon.getHealth();
console.log(`Active operations: ${health.activeOperations}`);
```

#### `getMetrics()`

Get performance metrics.

**Signature**:
```typescript
getMetrics(): DaemonMetrics
```

**Returns**:
```typescript
interface DaemonMetrics {
  nodeId: string;
  totalOperations: number;
  successfulOperations: number;
  failedOperations: number;
  averageDuration: number;             // Milliseconds
  totalDuration: number;               // Milliseconds
  successRate: number;                 // Percentage
  timestamp: Date;
}
```

**Example**:
```javascript
const metrics = daemon.getMetrics();
console.log(`Success rate: ${metrics.successRate}%`);
```

---

## Integration Modules

### ΔGate Control Plane

**Module**: `v6-deltagate.mjs`

#### `DaemonDeltaGate`

ΔGate integration for daemon operations.

**Constructor**:
```typescript
new DaemonDeltaGate(config: DeltaGateConfig)

interface DeltaGateConfig {
  receipts?: boolean;                  // Generate receipts (default: true)
  validation?: boolean;                // Validate deltas (default: true)
  merkleProofs?: boolean;              // Include Merkle proofs (default: false)
}
```

**Methods**:
- `initialize(): Promise<void>` - Initialize gate
- `processDelta(delta: Delta): Promise<DeltaResult>` - Process delta with receipt
- `validateContract(contract: DeltaContract): Promise<boolean>` - Validate delta contract

**Example**:
```javascript
import { DaemonDeltaGate } from '@unrdf/daemon';

const gate = new DaemonDeltaGate({
  receipts: true,
  merkleProofs: true,
});

await gate.initialize();

const result = await gate.processDelta({
  operation: 'insert',
  triples: [/* RDF triples */],
});

console.log('Receipt:', result.receipt);
```

---

### YAWL Workflow Orchestration

**Module**: `yawl.mjs`

#### `createYawlIntegration(config)`

Create YAWL workflow integration.

**Signature**:
```typescript
function createYawlIntegration(config: YawlConfig): YawlIntegration

interface YawlConfig {
  daemonId: string;
  workflowStore?: WorkflowStore;
}
```

**Methods**:
- `executeWorkflow(workflow: Workflow): Promise<WorkflowResult>`
- `getWorkflowStatus(workflowId: string): Promise<WorkflowStatus>`

**Example**:
```javascript
import { createYawlIntegration } from '@unrdf/daemon';

const yawl = createYawlIntegration({
  daemonId: 'workflow-daemon',
});

await yawl.executeWorkflow({
  workflowId: 'rdf-pipeline',
  tasks: [
    { id: 'parse', handler: parseRdf },
    { id: 'validate', handler: validateShacl },
    { id: 'store', handler: storeTriples },
  ],
});
```

---

### Federated Query Execution

**Module**: `federation-query.mjs`

#### `createFederationQuery(config)`

Create federated SPARQL query executor.

**Signature**:
```typescript
function createFederationQuery(config: FederationConfig): FederationQuery

interface FederationConfig {
  nodes: string[];                     // SPARQL endpoint URLs
  timeout?: number;                    // Query timeout (ms)
  retryPolicy?: RetryPolicy;
}
```

**Methods**:
- `executeFederatedQuery(sparql: string): Promise<SPARQLResults>`
- `getNodeHealth(): Promise<NodeHealth[]>`

**Example**:
```javascript
import { createFederationQuery } from '@unrdf/daemon';

const federation = createFederationQuery({
  nodes: [
    'http://node1:8080/sparql',
    'http://node2:8080/sparql',
  ],
  timeout: 30000,
});

const results = await federation.executeFederatedQuery(`
  SELECT ?subject ?predicate ?object WHERE {
    ?subject ?predicate ?object .
  }
`);
```

---

## Security Functions

**Module**: `security-audit.mjs`

### `detectInjection(input, type)`

Detect injection attacks.

**Signature**:
```typescript
function detectInjection(input: string, type: 'sql' | 'command' | 'rdf'): InjectionResult

interface InjectionResult {
  detected: boolean;
  reason?: string;
  patterns?: string[];
}
```

**Example**:
```javascript
import { detectInjection } from '@unrdf/daemon/security-audit';

const result = detectInjection(userInput, 'rdf');
if (result.detected) {
  throw new Error(`Injection detected: ${result.reason}`);
}
```

### `detectSecrets(input)`

Detect secrets in outputs.

**Signature**:
```typescript
function detectSecrets(input: string): SecretResult

interface SecretResult {
  detected: boolean;
  patterns?: string[];                 // Matched patterns (e.g., 'API_KEY')
}
```

**Example**:
```javascript
import { detectSecrets } from '@unrdf/daemon/security-audit';

const secrets = detectSecrets(JSON.stringify(output));
if (secrets.detected) {
  console.error('Secret exposure detected:', secrets.patterns);
}
```

### `sanitizePath(path)`

Sanitize file paths to prevent traversal.

**Signature**:
```typescript
function sanitizePath(path: string): string | null
```

**Returns**: Sanitized path or `null` if invalid

**Example**:
```javascript
import { sanitizePath } from '@unrdf/daemon/security-audit';

const safePath = sanitizePath(userProvidedPath);
if (!safePath) {
  throw new Error('Invalid file path');
}
```

### `sanitizeError(error)`

Remove sensitive data from error messages.

**Signature**:
```typescript
function sanitizeError(error: Error): Error
```

**Returns**: Error with sanitized message

**Example**:
```javascript
import { sanitizeError } from '@unrdf/daemon/security-audit';

try {
  await riskyOperation();
} catch (error) {
  throw sanitizeError(error);
}
```

---

## Schemas & Validation

All schemas use Zod for runtime validation.

### Authentication Schemas

```javascript
import {
  ApiKeySchema,
  ApiKeyHashSchema,
  AuthConfigSchema,
} from '@unrdf/daemon';

// Validate API key format
const key = ApiKeySchema.parse(userProvidedKey);

// Validate hash format
const hash = ApiKeyHashSchema.parse(storedHash);

// Validate auth config
const config = AuthConfigSchema.parse({
  environment: 'production',
  requireInDev: false,
});
```

### Daemon Schemas

```javascript
import {
  DaemonConfigSchema,
  TaskConfigSchema,
  DaemonHealthSchema,
} from '@unrdf/daemon';

// Validate daemon config
const config = DaemonConfigSchema.parse({
  daemonId: 'my-daemon',
  concurrency: 10,
});
```

---

## Error Handling

All daemon operations throw specific error types:

### Error Types

```typescript
class AuthenticationError extends Error {}
class ValidationError extends Error {}
class OperationError extends Error {}
class SecurityError extends Error {}
```

### Error Handling Pattern

```javascript
import { AuthenticationError } from '@unrdf/daemon';

try {
  const result = await authenticator.authenticate(context);

  if (!result.authenticated) {
    throw new AuthenticationError(result.reason);
  }

  // Proceed with operation
} catch (error) {
  if (error instanceof AuthenticationError) {
    console.error('Auth failed:', error.message);
  } else {
    console.error('Unexpected error:', error);
  }
}
```

---

## Environment Variables

```bash
# API key authentication
UNRDF_API_KEY=your_64_char_hex_key_here

# Daemon configuration
DAEMON_PORT=8080
DAEMON_CONCURRENCY=10
DAEMON_LOG_LEVEL=info

# Environment mode
NODE_ENV=production  # or 'development' or 'test'
```

---

## HTTP API Endpoints (Optional HTTP Server)

If using the daemon HTTP server:

### `POST /api/daemon/execute`

Execute a daemon operation.

**Request**:
```json
{
  "operationId": "validate-rdf",
  "data": { "scope": "all" }
}
```

**Headers**:
```
X-API-Key: <your-api-key>
```

**Response**:
```json
{
  "status": "success",
  "operationId": "validate-rdf",
  "result": { "validated": 42 },
  "duration": 123,
  "receipt": { ... }
}
```

### `GET /api/daemon/health`

Get daemon health.

**Response**:
```json
{
  "isRunning": true,
  "uptime": 12345,
  "activeOperations": 2,
  "queuedOperations": 5
}
```

### `GET /api/daemon/metrics`

Get performance metrics.

**Response**:
```json
{
  "totalOperations": 100,
  "successfulOperations": 95,
  "failedOperations": 5,
  "successRate": 95.0,
  "averageDuration": 125.5
}
```

---

## TypeScript Definitions

Full TypeScript definitions are provided via JSDoc:

```javascript
/**
 * @typedef {Object} AuthResult
 * @property {boolean} authenticated - Whether authentication succeeded
 * @property {string} [reason] - Reason for failure
 * @property {'header'|'environment'} [source] - Key source
 */
```

Use with TypeScript projects:

```typescript
import type {
  AuthResult,
  DaemonConfig,
  OperationResult,
} from '@unrdf/daemon';
```

---

## See Also

- [Authentication Guide](../packages/daemon/AUTHENTICATION.md)
- [Security Migration](SECURITY_MIGRATION.md)
- [Migration Guide](MIGRATING_TO_V6.md)
- [Deployment Guides](deployment/)
