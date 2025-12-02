# Schemas Reference

**Version**: v4.1.1
**Package**: `unrdf`
**Module**: `knowledge-engine/schemas`

This reference documents all Zod schemas exported by UNRDF for runtime validation of hook definitions, conditions, events, and configurations.

---

## Table of Contents

- [Hook Schemas](#hook-schemas)
- [Condition Schemas](#condition-schemas)
- [Event & Result Schemas](#event--result-schemas)
- [Configuration Schemas](#configuration-schemas)
- [Transaction Schemas](#transaction-schemas)
- [Validation Functions](#validation-functions)
- [Type-Safe Creators](#type-safe-creators)

---

## Hook Schemas

### HookMetaSchema

**Description**: Schema for hook metadata.

**Schema**:
```typescript
{
  name: string (1-100 chars, alphanumeric + :_-),
  description?: string (1-500 chars),
  version?: string (semver format),
  author?: string (1-100 chars),
  tags?: string[] (max 10, each 1-50 chars),
  ontology?: string[] (max 10, each 1-50 chars),
  createdAt?: Date,
  updatedAt?: Date
}
```

**Example**:
```javascript
import { HookMetaSchema } from 'unrdf';

const meta = HookMetaSchema.parse({
  name: 'compliance:largeTx',
  description: 'Alerts on large financial transactions',
  version: '1.0.0',
  author: 'Compliance Team',
  tags: ['financial', 'compliance'],
  ontology: ['fibo']
});
```

**Since**: v4.1.1

---

### FileRefSchema

**Description**: Schema for content-addressed file references.

**Schema**:
```typescript
{
  uri: string (valid URI or file:// URI),
  sha256?: string (64 hex chars),
  mediaType?: string (1-100 chars),
  size?: number (positive integer),
  lastModified?: Date
}
```

**Example**:
```javascript
import { FileRefSchema } from 'unrdf';

const ref = FileRefSchema.parse({
  uri: 'file://hooks/compliance/largeTx.ask.rq',
  sha256: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
  mediaType: 'application/sparql-query'
});
```

**Since**: v4.1.1

---

### HookChannelSchema

**Description**: Schema for hook observation channel configuration.

**Schema**:
```typescript
{
  graphs?: string[],
  view?: 'before' | 'after' | 'delta'
}
```

**Example**:
```javascript
import { HookChannelSchema } from 'unrdf';

const channel = HookChannelSchema.parse({
  graphs: ['urn:graph:fibo:prod', 'urn:graph:fibo:staging'],
  view: 'delta'
});
```

**Since**: v4.1.1

---

### KnowledgeHookSchema

**Description**: Schema for complete knowledge hook definition.

**Schema**:
```typescript
{
  meta: HookMetaSchema,
  channel?: HookChannelSchema,
  when: ConditionSchema,
  run: function,
  before?: function,
  after?: function,
  determinism?: DeterminismSchema,
  receipt?: ReceiptSchema,
  timeout?: number (1-300000ms),
  retries?: number (0-5),
  priority?: number (0-100, default 50)
}
```

**Example**:
```javascript
import { KnowledgeHookSchema } from 'unrdf';

const hook = KnowledgeHookSchema.parse({
  meta: { name: 'test-hook' },
  when: { kind: 'sparql-ask', query: 'ASK { ?s ?p ?o }' },
  run: async (event) => ({ result: 'executed' })
});
```

**Since**: v4.1.1

---

## Condition Schemas

### SparqlAskConditionSchema

**Description**: Schema for SPARQL ASK condition (boolean query).

**Schema**:
```typescript
{
  kind: 'sparql-ask',
  ref?: FileRefSchema,
  query?: string,
  options?: {
    timeout?: number (1-30000ms),
    strict?: boolean,
    variables?: Record<string, string>
  }
}
```

**Example**:
```javascript
import { SparqlAskConditionSchema } from 'unrdf';

const condition = SparqlAskConditionSchema.parse({
  kind: 'sparql-ask',
  query: 'ASK WHERE { ?s ?p ?o }',
  options: { timeout: 5000 }
});
```

**Since**: v4.1.1

---

### SparqlSelectConditionSchema

**Description**: Schema for SPARQL SELECT condition.

**Schema**:
```typescript
{
  kind: 'sparql-select',
  ref?: FileRefSchema,
  query?: string,
  options?: {
    timeout?: number (1-30000ms),
    limit?: number (1-10000),
    offset?: number (>=0),
    strict?: boolean,
    variables?: Record<string, string>
  }
}
```

**Example**:
```javascript
import { SparqlSelectConditionSchema } from 'unrdf';

const condition = SparqlSelectConditionSchema.parse({
  kind: 'sparql-select',
  query: 'SELECT ?name WHERE { ?person <http://xmlns.com/foaf/0.1/name> ?name }',
  options: { limit: 100 }
});
```

**Since**: v4.1.1

---

### ShaclConditionSchema

**Description**: Schema for SHACL validation condition.

**Schema**:
```typescript
{
  kind: 'shacl',
  ref?: FileRefSchema,
  shapes?: string,
  options?: {
    strict?: boolean,
    includeDetails?: boolean,
    maxViolations?: number (1-1000),
    shapesGraph?: string (valid URL)
  }
}
```

**Example**:
```javascript
import { ShaclConditionSchema } from 'unrdf';

const condition = ShaclConditionSchema.parse({
  kind: 'shacl',
  shapes: `
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    ex:PersonShape a sh:NodeShape .
  `,
  options: { includeDetails: true }
});
```

**Since**: v4.1.1

---

### DeltaConditionSchema

**Description**: Schema for delta-based condition (graph change detection).

**Schema**:
```typescript
{
  kind: 'delta',
  spec: {
    change: 'any' | 'increase' | 'decrease' | 'modify',
    key: string[] (at least 1),
    threshold?: number (0-1),
    baseline?: string
  },
  options?: {
    timeout?: number (1-30000ms),
    strict?: boolean
  }
}
```

**Example**:
```javascript
import { DeltaConditionSchema } from 'unrdf';

const condition = DeltaConditionSchema.parse({
  kind: 'delta',
  spec: {
    change: 'increase',
    key: ['http://example.org/amount'],
    threshold: 0.1
  }
});
```

**Since**: v4.1.1

---

### ThresholdConditionSchema

**Description**: Schema for threshold condition (value comparison).

**Schema**:
```typescript
{
  kind: 'threshold',
  spec: {
    var: string,
    op: '>' | '>=' | '<' | '<=' | '==' | '!=',
    value: number,
    aggregate?: 'sum' | 'avg' | 'min' | 'max' | 'count'
  },
  options?: {
    timeout?: number (1-30000ms),
    strict?: boolean
  }
}
```

**Example**:
```javascript
import { ThresholdConditionSchema } from 'unrdf';

const condition = ThresholdConditionSchema.parse({
  kind: 'threshold',
  spec: {
    var: 'amount',
    op: '>',
    value: 10000,
    aggregate: 'sum'
  }
});
```

**Since**: v4.1.1

---

### CountConditionSchema

**Description**: Schema for count condition (result count comparison).

**Schema**:
```typescript
{
  kind: 'count',
  spec: {
    op: '>' | '>=' | '<' | '<=' | '==' | '!=',
    value: number (non-negative integer),
    query?: string
  },
  options?: {
    timeout?: number (1-30000ms),
    strict?: boolean
  }
}
```

**Example**:
```javascript
import { CountConditionSchema } from 'unrdf';

const condition = CountConditionSchema.parse({
  kind: 'count',
  spec: {
    op: '>=',
    value: 100
  }
});
```

**Since**: v4.1.1

---

### WindowConditionSchema

**Description**: Schema for sliding window condition (time-windowed aggregation).

**Schema**:
```typescript
{
  kind: 'window',
  spec: {
    size: number (positive, milliseconds),
    slide?: number (positive, milliseconds),
    aggregate: 'sum' | 'avg' | 'min' | 'max' | 'count',
    query?: string
  },
  options?: {
    timeout?: number (1-30000ms),
    strict?: boolean
  }
}
```

**Example**:
```javascript
import { WindowConditionSchema } from 'unrdf';

const condition = WindowConditionSchema.parse({
  kind: 'window',
  spec: {
    size: 60000, // 1 minute
    slide: 10000, // Slide every 10 seconds
    aggregate: 'avg'
  }
});
```

**Since**: v4.1.1

---

### ConditionSchema

**Description**: Union schema for all condition types (discriminated union on 'kind').

**Example**:
```javascript
import { ConditionSchema } from 'unrdf';

// Validates any condition type
const condition = ConditionSchema.parse({
  kind: 'sparql-ask',
  query: 'ASK { ?s ?p ?o }'
});
```

**Since**: v4.1.1

---

## Event & Result Schemas

### HookContextSchema

**Description**: Schema for hook execution context.

**Schema**:
```typescript
{
  graph: any,
  env?: Record<string, any>,
  metadata?: Record<string, any>,
  transactionId?: string (valid UUID),
  timestamp?: Date
}
```

**Since**: v4.1.1

---

### HookEventSchema

**Description**: Schema for hook events passed to lifecycle functions.

**Schema**:
```typescript
{
  name: string (1-100 chars),
  payload: any,
  context: HookContextSchema,
  id?: string (valid UUID),
  timestamp?: Date,
  source?: string (1-100 chars),
  correlationId?: string (valid UUID)
}
```

**Since**: v4.1.1

---

### HookResultSchema

**Description**: Schema for hook execution results.

**Schema**:
```typescript
{
  success: boolean,
  result?: any,
  error?: string,
  duration?: number (non-negative),
  phase?: 'before' | 'run' | 'after' | 'completed' | 'failed',
  cancelled: boolean (default false),
  metadata?: Record<string, any>,
  assertions?: Array<{
    subject: string,
    predicate: string,
    object: string,
    graph?: string
  }>
}
```

**Since**: v4.1.1

---

## Configuration Schemas

### DeterminismSchema

**Description**: Schema for determinism configuration in hooks.

**Schema**:
```typescript
{
  seed: number (0-2147483647, default 42),
  algorithm: 'lcg' | 'xorshift' | 'mersenne' (default 'lcg'),
  salt?: string (1-100 chars)
}
```

**Example**:
```javascript
import { DeterminismSchema } from 'unrdf';

const determinism = DeterminismSchema.parse({
  seed: 42,
  algorithm: 'lcg'
});
```

**Since**: v4.1.1

---

### ReceiptSchema

**Description**: Schema for receipt configuration in hooks.

**Schema**:
```typescript
{
  anchor: 'none' | 'blockchain' | 'merkle' | 'timestamp' | 'hash' | 'git-notes' (default 'none'),
  format: 'json' | 'jsonld' | 'rdf' | 'turtle' (default 'json'),
  includeProof: boolean (default false),
  ttl?: number (1-86400 seconds)
}
```

**Example**:
```javascript
import { ReceiptSchema } from 'unrdf';

const receipt = ReceiptSchema.parse({
  anchor: 'git-notes',
  format: 'jsonld',
  includeProof: true
});
```

**Since**: v4.1.1

---

### ManagerConfigSchema

**Description**: Schema for hook manager configuration.

**Schema**:
```typescript
{
  basePath: string (default: cwd),
  strictMode: boolean (default false),
  enableConditionEvaluation: boolean (default true),
  maxHooks: number (1-1000, default 100),
  timeout: number (1-300000ms, default 30000),
  enableCache: boolean (default true),
  cacheMaxAge: number (1-3600000ms, default 300000),
  enableMetrics: boolean (default true),
  logLevel: 'error' | 'warn' | 'info' | 'debug' (default 'info'),
  observability?: ObservabilityConfigSchema,
  performance?: {
    enableProfiling: boolean (default false),
    maxConcurrency: number (default 10),
    afterHashOnly: boolean (default false),
    enableCache: boolean (default true),
    timeoutMs: number (default 2000),
    maxHooks: number (default 10000)
  }
}
```

**Since**: v4.1.1

---

### ObservabilityConfigSchema

**Description**: Schema for OpenTelemetry observability configuration.

**Schema**:
```typescript
{
  enableTracing: boolean (default true),
  enableMetrics: boolean (default true),
  enableLogging: boolean (default true),
  serviceName: string (default 'unrdf-kgc'),
  serviceVersion: string (default '1.0.0'),
  endpoint?: string (valid URL),
  headers?: Record<string, string>,
  resourceAttributes?: Record<string, string>,
  samplingRatio: number (0-1, default 1.0),
  maxQueueSize: number (default 2048),
  maxExportBatchSize: number (default 512),
  exportTimeoutMillis: number (default 30000),
  scheduledDelayMillis: number (default 5000),
  cacheMaxSize?: number,
  minSamples: number (default 20),
  ewmaAlpha: number (0-1, default 0.3)
}
```

**Since**: v4.1.1

---

## Transaction Schemas

### QuadSchema

**Description**: Schema for RDF quads (flexible to allow RDF/JS Quad objects).

**Schema**:
```typescript
{
  subject: any,
  predicate: any,
  object: any,
  graph?: any,
  equals?: function
}
```

**Since**: v4.1.1

---

### DeltaSchema

**Description**: Schema for transaction delta (additions and removals).

**Schema**:
```typescript
{
  additions: QuadSchema[],
  removals: QuadSchema[]
}
```

**Example**:
```javascript
import { DeltaSchema, DataFactory } from 'unrdf';

const { namedNode, literal, quad } = DataFactory;

const delta = DeltaSchema.parse({
  additions: [
    quad(
      namedNode('http://example.org/alice'),
      namedNode('http://example.org/age'),
      literal('31')
    )
  ],
  removals: []
});
```

**Since**: v4.1.1

---

### TransactionDeltaSchema

**Description**: Extended schema for transaction delta with metadata.

**Schema**:
```typescript
{
  additions: any[] (default []),
  removals: any[] (default []),
  metadata?: Record<string, any>,
  id?: string,
  timestamp?: Date
}
```

**Since**: v4.1.1

---

### TransactionReceiptSchema

**Description**: Schema for transaction receipt.

**Schema**:
```typescript
{
  committed: boolean,
  delta: TransactionDeltaSchema,
  hookResults: Array<{
    hookId: string,
    result: boolean,
    error?: string,
    duration?: number
  }> (default []),
  beforeHash?: { sha3: string, blake3: string },
  afterHash?: { sha3: string, blake3: string },
  timestamp: Date,
  duration: number,
  knowledgeHookResults: number (default 0)
}
```

**Since**: v4.1.1

---

## Validation Functions

### validateKnowledgeHook

**Signature**: `validateKnowledgeHook(hook: any): { success: boolean, data: any, errors: any[] }`

**Description**: Validates a knowledge hook definition.

**Parameters**:
- `hook` (any) - Hook definition to validate

**Returns**: Validation result object with:
- `success` (boolean) - Whether validation passed
- `data` (any | null) - Validated data if success, null otherwise
- `errors` (any[]) - Array of validation error objects

**Example**:
```javascript
import { validateKnowledgeHook } from 'unrdf';

const validation = validateKnowledgeHook({
  meta: { name: 'test-hook' },
  when: { kind: 'sparql-ask', query: 'ASK { ?s ?p ?o }' },
  run: async (event) => ({ result: 'executed' })
});

if (!validation.success) {
  console.error('Validation errors:', validation.errors);
}
```

**Since**: v4.1.1

---

### validateHookEvent

**Signature**: `validateHookEvent(event: any): { success: boolean, data: any, errors: any[] }`

**Description**: Validates a hook event.

**Parameters**:
- `event` (any) - Event to validate

**Returns**: Validation result object

**Since**: v4.1.1

---

### validateCondition

**Signature**: `validateCondition(condition: any): { success: boolean, data: any, errors: any[] }`

**Description**: Validates a hook condition.

**Parameters**:
- `condition` (any) - Condition to validate

**Returns**: Validation result object

**Since**: v4.1.1

---

### validateManagerConfig

**Signature**: `validateManagerConfig(config: any): { success: boolean, data: any, errors: any[] }`

**Description**: Validates manager configuration.

**Parameters**:
- `config` (any) - Configuration to validate

**Returns**: Validation result object

**Since**: v4.1.1

---

### validateTransactionDelta

**Signature**: `validateTransactionDelta(delta: any): { success: boolean, data: any, errors: any[] }`

**Description**: Validates a transaction delta.

**Parameters**:
- `delta` (any) - Delta to validate

**Returns**: Validation result object

**Since**: v4.1.1

---

## Type-Safe Creators

### createKnowledgeHook

**Signature**: `createKnowledgeHook(definition: any): KnowledgeHook`

**Description**: Creates a validated and frozen knowledge hook definition. This is the recommended way to create hooks with full validation.

**Parameters**:
- `definition` (any) - Hook definition object

**Returns**: Validated and frozen KnowledgeHook object

**Throws**:
- `TypeError` if validation fails with detailed error messages

**Example**:
```javascript
import { createKnowledgeHook } from 'unrdf';

const hook = createKnowledgeHook({
  meta: { name: 'test-hook', description: 'Test hook' },
  when: { kind: 'sparql-ask', query: 'ASK { ?s ?p ?o }' },
  run: async (event) => ({ result: 'executed' })
});

// Hook is validated and frozen
console.log(Object.isFrozen(hook)); // true
```

**Since**: v4.1.1

---

### createHookEvent

**Signature**: `createHookEvent(event: any): HookEvent`

**Description**: Creates a validated hook event with defaults.

**Parameters**:
- `event` (any) - Event definition

**Returns**: Validated HookEvent object with:
- Auto-generated UUID if not provided
- Auto-generated timestamp if not provided

**Throws**:
- `TypeError` if validation fails

**Example**:
```javascript
import { createHookEvent } from 'unrdf';

const event = createHookEvent({
  name: 'test-hook',
  payload: { amount: 1000 },
  context: { graph: store }
});

console.log('Event ID:', event.id);
console.log('Timestamp:', event.timestamp);
```

**Since**: v4.1.1

---

### createCondition

**Signature**: `createCondition(condition: any): Condition`

**Description**: Creates a validated condition.

**Parameters**:
- `condition` (any) - Condition definition

**Returns**: Validated Condition object

**Throws**:
- `TypeError` if validation fails

**Example**:
```javascript
import { createCondition } from 'unrdf';

const condition = createCondition({
  kind: 'sparql-ask',
  query: 'ASK { ?s ?p ?o }'
});
```

**Since**: v4.1.1

---

## Usage Examples

### Complete Hook Definition with Validation

```javascript
import {
  createKnowledgeHook,
  HookMetaSchema,
  SparqlAskConditionSchema
} from 'unrdf';

// Validate individual components first
const meta = HookMetaSchema.parse({
  name: 'compliance:largeTx',
  description: 'Alerts on large financial transactions',
  version: '1.0.0',
  ontology: ['fibo']
});

const condition = SparqlAskConditionSchema.parse({
  kind: 'sparql-ask',
  query: `
    ASK WHERE {
      ?tx <http://example.org/amount> ?amount .
      FILTER (?amount > 10000)
    }
  `
});

// Create validated hook
const hook = createKnowledgeHook({
  meta,
  when: condition,
  async run({ payload }) {
    return {
      result: { status: 'alert', amount: payload.amount },
      assertions: []
    };
  }
});
```

---

### Transaction with Validated Delta

```javascript
import { TransactionManager, DeltaSchema, DataFactory } from 'unrdf';

const { namedNode, literal, quad } = DataFactory;

// Validate delta
const delta = DeltaSchema.parse({
  additions: [
    quad(
      namedNode('http://example.org/alice'),
      namedNode('http://example.org/age'),
      literal('31')
    )
  ],
  removals: []
});

// Apply transaction
const manager = new TransactionManager();
const receipt = await manager.apply(store, delta);

console.log('Transaction receipt:', receipt);
```

---

### Manager Configuration with Validation

```javascript
import { ManagerConfigSchema, KnowledgeHookManager } from 'unrdf';

// Validate configuration
const config = ManagerConfigSchema.parse({
  basePath: process.cwd(),
  strictMode: false,
  maxHooks: 50,
  timeout: 10000,
  enableCache: true,
  observability: {
    enableTracing: true,
    serviceName: 'my-rdf-app',
    samplingRatio: 0.5
  }
});

// Create manager with validated config
const manager = new KnowledgeHookManager(config);
```

---

## Related Documentation

- [Knowledge Hooks API Reference](./knowledge-hooks-api.md) - Hook system API
- [Core RDF API Reference](./core-rdf-api.md) - RDF operations
- [Composables API Reference](./composables-api.md) - High-level API
- [Utilities API Reference](./utilities-api.md) - Utility functions
