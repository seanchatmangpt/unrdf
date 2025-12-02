# Knowledge Hooks API Reference

**Version**: v4.1.1
**Package**: `unrdf`
**Module**: `knowledge-engine`

This reference documents the Knowledge Hooks system - an autonomic, declarative system for defining and managing reactive RDF operations.

---

## Table of Contents

- [Hook Definition](#hook-definition)
- [Hook Management](#hook-management)
- [Hook Execution](#hook-execution)
- [Transaction Management](#transaction-management)
- [Condition Types](#condition-types)

---

## Hook Definition

### defineHook

**Signature**: `defineHook(spec: KnowledgeHookSpec): KnowledgeHook`

**Description**: Defines a Knowledge Hook with comprehensive Zod validation. This is the primary entry point for creating autonomic, policy-first hooks with lifecycle management.

**Parameters**:
- `spec` (KnowledgeHookSpec) - The hook definition object with the following properties:
  - `meta` (HookMeta) - Essential metadata
    - `name` (string, required) - Unique hook name (e.g., "compliance:largeTx")
    - `description` (string, optional) - Brief explanation of hook's purpose
    - `ontology` (string[], optional) - Related ontology prefixes (e.g., ["fibo", "prov"])
  - `channel` (HookChannel, optional) - Observation channel
    - `graphs` (string[], optional) - Named graph IRIs to scope the query
    - `view` ('before' | 'after' | 'delta', optional) - Graph state to evaluate
  - `when` (HookCondition, required) - Declarative trigger condition
    - `kind` ('sparql-ask' | 'sparql-select' | 'shacl' | 'delta' | 'threshold' | 'count' | 'window') - Condition type
    - `ref` (Ref, optional) - Content-addressed file reference
      - `uri` (string) - File URI (e.g., "file://hooks/compliance/largeTx.ask.rq")
      - `sha256` (string) - SHA-256 hash for integrity
      - `mediaType` (string) - MIME type ('application/sparql-query', 'text/shacl', 'text/turtle')
    - `query` (string, optional) - Inline query (convenience alternative to ref)
    - `shapes` (string, optional) - Inline SHACL shapes (convenience alternative to ref)
  - `before` (function, optional) - Pre-execution lifecycle function
  - `run` (function, required) - Main execution body
  - `after` (function, optional) - Post-execution lifecycle function
  - `determinism` (Object, optional) - Deterministic operation config
    - `seed` (number, optional) - Random seed (default: 42)
  - `receipt` (Object, optional) - Receipt anchoring strategy
    - `anchor` ('git-notes' | 'none', optional) - Anchoring method (default: 'none')

**Returns**: Validated and frozen KnowledgeHook object

**Throws**:
- `TypeError` if validation fails with detailed error messages
- `Error` if security validation fails (in development mode, logs warning only)

**Example**:
```javascript
import { defineHook } from 'unrdf';

const hook = defineHook({
  meta: {
    name: 'compliance:largeTx',
    description: 'Alerts on large financial transactions',
    ontology: ['fibo']
  },
  channel: {
    graphs: ['urn:graph:fibo:prod'],
    view: 'delta'
  },
  when: {
    kind: 'sparql-ask',
    ref: {
      uri: 'file://hooks/compliance/largeTx.ask.rq',
      sha256: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
      mediaType: 'application/sparql-query'
    }
  },
  determinism: { seed: 42 },
  receipt: { anchor: 'git-notes' },

  async before({ payload }) {
    if (!payload || typeof payload.amount !== 'number' || payload.amount <= 0) {
      return {
        cancel: true,
        reason: 'Invalid or non-positive transaction amount.'
      };
    }
    return { ...payload, validatedAt: new Date().toISOString() };
  },

  async run({ payload }) {
    console.log(`Processing large transaction of ${payload.amount}`);
    return {
      result: { status: 'alert-dispatched', amount: payload.amount },
      assertions: [] // Optional RDF quads to add
    };
  },

  async after({ result, cancelled, reason }) {
    if (cancelled) {
      console.log(`Hook cancelled. Reason: ${reason}`);
    } else {
      console.log(`Hook completed with status: ${result?.result?.status}`);
    }
    return { result: { finalStatus: cancelled ? 'cancelled' : 'completed' } };
  }
});
```

**See Also**: [registerHook](#registerhook), [evaluateHook](#evaluatehook)

**Since**: v4.1.1

---

## Hook Management

### registerHook

**Signature**: `registerHook(hook: KnowledgeHook, options?: Object): Promise<void>`

**Description**: Registers a knowledge hook globally in the hook manager.

**Parameters**:
- `hook` (KnowledgeHook) - The knowledge hook to register (from defineHook)
- `options` (Object, optional) - Manager options (if manager needs initialization)

**Returns**: Promise that resolves when hook is registered

**Throws**:
- `Error` if hook is invalid or already exists

**Example**:
```javascript
import { defineHook, registerHook } from 'unrdf';

const hook = defineHook({
  meta: { name: 'test-hook', description: 'Test hook' },
  when: { kind: 'sparql-ask', query: 'ASK { ?s ?p ?o }' },
  run: async (event) => console.log('Hook triggered')
});

await registerHook(hook);
```

**See Also**: [defineHook](#definehook), [deregisterHook](#deregisterhook), [getRegisteredHooks](#getregisteredhooks)

**Since**: v4.1.1

---

### deregisterHook

**Signature**: `deregisterHook(hookName: string): Promise<boolean>`

**Description**: Deregisters (removes) a knowledge hook globally.

**Parameters**:
- `hookName` (string) - The name of the hook to remove

**Returns**: Promise resolving to true if hook was removed, false if not found

**Example**:
```javascript
import { deregisterHook } from 'unrdf';

const removed = await deregisterHook('test-hook');
console.log('Hook removed:', removed);
```

**See Also**: [registerHook](#registerhook), [getRegisteredHooks](#getregisteredhooks)

**Since**: v4.1.1

---

### getRegisteredHooks

**Signature**: `getRegisteredHooks(): string[]`

**Description**: Retrieves names of all registered hooks.

**Returns**: Array of registered hook names

**Example**:
```javascript
import { getRegisteredHooks } from 'unrdf';

const hooks = getRegisteredHooks();
console.log('Registered hooks:', hooks);
```

**See Also**: [registerHook](#registerhook), [deregisterHook](#deregisterhook)

**Since**: v4.1.1

---

### resetGlobalHookManager

**Signature**: `resetGlobalHookManager(): void`

**Description**: Resets the global hook manager, clearing all registered hooks. Useful for testing or cleanup.

**Example**:
```javascript
import { resetGlobalHookManager } from 'unrdf';

// After tests
resetGlobalHookManager();
```

**See Also**: [registerHook](#registerhook)

**Since**: v4.1.1

---

## Hook Execution

### evaluateHook

**Signature**: `evaluateHook(hook: KnowledgeHook, store: Store, context?: Object): Promise<Object>`

**Description**: Manually evaluates a hook against given data without registering it globally.

**Parameters**:
- `hook` (KnowledgeHook) - The hook to evaluate
- `store` (Store) - The RDF store to evaluate against
- `context` (Object, optional) - Additional context for evaluation

**Returns**: Promise resolving to evaluation result object with:
- `satisfied` (boolean) - Whether condition was satisfied
- `result` (any) - Condition evaluation result
- `output` (any, optional) - Hook run output if condition satisfied
- `error` (string, optional) - Error message if execution failed
- `timestamp` (number) - Evaluation timestamp
- `hookName` (string) - Name of evaluated hook

**Example**:
```javascript
import { evaluateHook, defineHook, Store } from 'unrdf';

const hook = defineHook({
  meta: { name: 'test-hook' },
  when: { kind: 'sparql-ask', query: 'ASK { ?s ?p ?o }' },
  run: async (event) => ({ result: 'executed' })
});

const store = new Store();
// ... add quads

const result = await evaluateHook(hook, store, { timestamp: Date.now() });
console.log('Hook evaluation result:', result);
```

**See Also**: [defineHook](#definehook), [registerHook](#registerhook)

**Since**: v4.1.1

---

### createHookExecutor

**Signature**: `createHookExecutor(options?: Object): HookExecutor`

**Description**: Creates a sandboxed hook execution environment.

**Parameters**:
- `options` (Object, optional) - Executor configuration
  - `timeout` (number, optional) - Execution timeout in ms (default: 30000)
  - `maxConcurrent` (number, optional) - Max concurrent executions (default: 10)
  - `retries` (number, optional) - Number of retries on failure (default: 1)
  - `enableMetrics` (boolean, optional) - Enable metrics collection (default: true)
  - `strict` (boolean, optional) - Strict mode (default: false)

**Returns**: HookExecutor instance

**Example**:
```javascript
import { createHookExecutor } from 'unrdf';

const executor = createHookExecutor({
  timeout: 5000,
  maxConcurrent: 5,
  enableMetrics: true
});

const result = await executor.execute(hook, event);
```

**See Also**: [evaluateHook](#evaluatehook)

**Since**: v4.1.1

---

### createConditionEvaluator

**Signature**: `createConditionEvaluator(options?: Object): ConditionEvaluator`

**Description**: Creates a condition evaluation engine for hook triggers.

**Parameters**:
- `options` (Object, optional) - Evaluator configuration
  - `enableCache` (boolean, optional) - Enable result caching (default: true)
  - `timeout` (number, optional) - Evaluation timeout in ms (default: 10000)
  - `maxConcurrent` (number, optional) - Max concurrent evaluations (default: 10)

**Returns**: ConditionEvaluator instance

**Example**:
```javascript
import { createConditionEvaluator } from 'unrdf';

const evaluator = createConditionEvaluator({
  enableCache: true,
  timeout: 5000
});

const satisfied = await evaluator.isSatisfied(condition, store, context);
```

**See Also**: [evaluateHook](#evaluatehook)

**Since**: v4.1.1

---

## Transaction Management

### TransactionManager

**Description**: Hook-driven transaction manager for autonomic RDF graph operations.

**Constructor**: `new TransactionManager(options?: Object)`

**Parameters**:
- `options` (Object, optional) - Manager configuration
  - `basePath` (string, optional) - Base path for file resolution
  - `strictMode` (boolean, optional) - Enable strict mode (default: false)
  - `enableConditionEvaluation` (boolean, optional) - Enable condition evaluation (default: true)
  - `maxHooks` (number, optional) - Maximum hooks (default: 100)
  - `timeout` (number, optional) - Operation timeout in ms (default: 30000)

**Methods**:

#### addHook

**Signature**: `addHook(hook: KnowledgeHook): void`

**Description**: Adds a knowledge hook to the transaction manager.

**Parameters**:
- `hook` (KnowledgeHook) - The hook to add

**Example**:
```javascript
import { TransactionManager, defineHook } from 'unrdf';

const manager = new TransactionManager();
const hook = defineHook({
  meta: { name: 'audit-hook' },
  when: { kind: 'sparql-ask', query: 'ASK { ?s ?p ?o }' },
  run: async (event) => ({ result: 'audited' })
});

manager.addHook(hook);
```

---

#### apply

**Signature**: `apply(store: Store, delta: Delta): Promise<Receipt>`

**Description**: Applies a transaction delta to the store with hook execution.

**Parameters**:
- `store` (Store) - The RDF store to modify
- `delta` (Delta) - Transaction delta with:
  - `additions` (Quad[]) - Quads to add
  - `removals` (Quad[]) - Quads to remove

**Returns**: Promise resolving to TransactionReceipt with:
- `committed` (boolean) - Whether transaction committed
- `delta` (Delta) - Applied delta
- `hookResults` (Array) - Hook execution results
- `beforeHash` (Object) - Hash before transaction
- `afterHash` (Object) - Hash after transaction
- `timestamp` (number) - Transaction timestamp
- `duration` (number) - Execution duration in ms

**Throws**:
- `Error` if transaction fails or hook vetoes

**Example**:
```javascript
import { TransactionManager, Store, DataFactory } from 'unrdf';

const { namedNode, literal, quad } = DataFactory;

const manager = new TransactionManager();
const store = new Store();

// Add hooks
manager.addHook(complianceHook);
manager.addHook(auditHook);

// Apply delta
const delta = {
  additions: [
    quad(
      namedNode('http://example.org/alice'),
      namedNode('http://example.org/age'),
      literal('31')
    )
  ],
  removals: []
};

const receipt = await manager.apply(store, delta);
console.log('Transaction committed:', receipt.committed);
console.log('Hook results:', receipt.hookResults);
```

**See Also**: [addHook](#addhook), [getStats](#getstats)

---

#### getStats

**Signature**: `getStats(): Object`

**Description**: Retrieves transaction statistics.

**Returns**: Statistics object with:
- `totalTransactions` (number) - Total transactions processed
- `committedTransactions` (number) - Successfully committed transactions
- `failedTransactions` (number) - Failed transactions
- `totalHookExecutions` (number) - Total hook executions
- `averageTransactionTime` (number) - Average transaction duration in ms

**Example**:
```javascript
const stats = manager.getStats();
console.log('Transaction stats:', stats);
```

**See Also**: [apply](#apply)

---

## Condition Types

Knowledge Hooks support multiple condition types for declarative triggering.

### SPARQL ASK

**Description**: Boolean query condition - triggers if query returns true.

**Schema**:
```typescript
{
  kind: 'sparql-ask',
  ref?: { uri: string, sha256: string, mediaType: string },
  query?: string, // Inline alternative
  options?: {
    timeout?: number,
    strict?: boolean,
    variables?: Record<string, string>
  }
}
```

**Example**:
```javascript
when: {
  kind: 'sparql-ask',
  query: `
    ASK WHERE {
      ?tx <http://example.org/amount> ?amount .
      FILTER (?amount > 10000)
    }
  `
}
```

---

### SPARQL SELECT

**Description**: Query condition - triggers if SELECT returns non-empty results.

**Schema**:
```typescript
{
  kind: 'sparql-select',
  ref?: { uri: string, sha256: string, mediaType: string },
  query?: string, // Inline alternative
  options?: {
    timeout?: number,
    limit?: number,
    offset?: number,
    strict?: boolean,
    variables?: Record<string, string>
  }
}
```

**Example**:
```javascript
when: {
  kind: 'sparql-select',
  query: `
    SELECT ?person ?age WHERE {
      ?person <http://example.org/age> ?age .
      FILTER (?age < 18)
    }
  `
}
```

---

### SHACL

**Description**: SHACL validation condition - triggers on validation violations.

**Schema**:
```typescript
{
  kind: 'shacl',
  ref?: { uri: string, sha256: string, mediaType: string },
  shapes?: string, // Inline Turtle shapes
  options?: {
    strict?: boolean,
    includeDetails?: boolean,
    maxViolations?: number,
    shapesGraph?: string
  }
}
```

**Example**:
```javascript
when: {
  kind: 'shacl',
  shapes: `
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix ex: <http://example.org/> .

    ex:PersonShape a sh:NodeShape ;
      sh:targetClass ex:Person ;
      sh:property [
        sh:path ex:name ;
        sh:minCount 1
      ] .
  `
}
```

---

### DELTA

**Description**: Delta-based condition - triggers on graph changes matching criteria.

**Schema**:
```typescript
{
  kind: 'delta',
  spec: {
    change: 'any' | 'increase' | 'decrease' | 'modify',
    key: string[], // Predicate URIs to watch
    threshold?: number, // Change threshold (0-1)
    baseline?: string // Baseline graph URI
  },
  options?: {
    timeout?: number,
    strict?: boolean
  }
}
```

**Example**:
```javascript
when: {
  kind: 'delta',
  spec: {
    change: 'increase',
    key: ['http://example.org/amount'],
    threshold: 0.1 // 10% increase
  }
}
```

---

### THRESHOLD

**Description**: Threshold condition - triggers when aggregated value crosses threshold.

**Schema**:
```typescript
{
  kind: 'threshold',
  spec: {
    var: string, // Variable name from query
    op: '>' | '>=' | '<' | '<=' | '==' | '!=',
    value: number,
    aggregate?: 'sum' | 'avg' | 'min' | 'max' | 'count'
  },
  options?: {
    timeout?: number,
    strict?: boolean
  }
}
```

**Example**:
```javascript
when: {
  kind: 'threshold',
  spec: {
    var: 'amount',
    op: '>',
    value: 10000,
    aggregate: 'sum'
  }
}
```

---

### COUNT

**Description**: Count condition - triggers when count matches criteria.

**Schema**:
```typescript
{
  kind: 'count',
  spec: {
    op: '>' | '>=' | '<' | '<=' | '==' | '!=',
    value: number,
    query?: string // SPARQL query for counting
  },
  options?: {
    timeout?: number,
    strict?: boolean
  }
}
```

**Example**:
```javascript
when: {
  kind: 'count',
  spec: {
    op: '>=',
    value: 100,
    query: 'SELECT (COUNT(?s) as ?count) WHERE { ?s ?p ?o }'
  }
}
```

---

### WINDOW

**Description**: Sliding window condition - triggers on windowed aggregates.

**Schema**:
```typescript
{
  kind: 'window',
  spec: {
    size: number, // Window size in milliseconds
    slide?: number, // Slide interval in milliseconds
    aggregate: 'sum' | 'avg' | 'min' | 'max' | 'count',
    query?: string // SPARQL query for windowing
  },
  options?: {
    timeout?: number,
    strict?: boolean
  }
}
```

**Example**:
```javascript
when: {
  kind: 'window',
  spec: {
    size: 60000, // 1 minute window
    slide: 10000, // Slide every 10 seconds
    aggregate: 'avg'
  }
}
```

---

## Hook Lifecycle

Knowledge Hooks have a three-phase lifecycle:

### before(event)

**Description**: Pre-execution gate for payload normalization or cancellation.

**Parameters**:
- `event` (HookEvent) - Event object with:
  - `name` (string) - Hook name
  - `payload` (Object) - Input data
  - `context` (HookContext) - Execution context

**Returns**: Promise or sync value:
- Partial payload (merged with original)
- `{ cancel: true, reason?: string }` to cancel execution

**Example**:
```javascript
async before({ payload }) {
  if (!payload.amount || payload.amount <= 0) {
    return { cancel: true, reason: 'Invalid amount' };
  }
  return { ...payload, validatedAt: Date.now() };
}
```

---

### run(event)

**Description**: Main execution body of the hook.

**Parameters**:
- `event` (HookEvent) - Event object (same as before)

**Returns**: Promise or sync HookResult:
- `result` (any) - Primary output
- `assertions` (Quad[], optional) - RDF quads to add
- `deltas` (Object, optional) - Complex delta to apply
- `cancelled` (boolean, optional) - Cancellation flag
- `reason` (string, optional) - Cancellation reason

**Example**:
```javascript
async run({ payload }) {
  console.log('Processing', payload.amount);
  return {
    result: { status: 'processed', amount: payload.amount },
    assertions: [
      quad(
        namedNode(payload.id),
        namedNode('http://example.org/processed'),
        literal('true')
      )
    ]
  };
}
```

---

### after(result)

**Description**: Post-execution cleanup or auditing.

**Parameters**:
- `event` (HookEvent & HookResult) - Extended event with:
  - All event properties
  - `result` (any) - Result from run
  - `cancelled` (boolean) - Whether execution was cancelled
  - `reason` (string, optional) - Cancellation reason

**Returns**: Promise or sync HookResult

**Example**:
```javascript
async after({ result, cancelled, reason }) {
  if (cancelled) {
    console.log('Cancelled:', reason);
    return { result: { status: 'cancelled' } };
  }
  console.log('Completed:', result.status);
  return { result: { status: 'finalized' } };
}
```

---

## Related Documentation

- [Core RDF API Reference](./core-rdf-api.md) - RDF parsing, querying, validation
- [Schemas Reference](./schemas.md) - Zod schemas for hooks
- [Transaction Tutorial](../tutorials/03-transactions.md) - Hook-driven transactions guide
- [Knowledge Hooks Tutorial](../tutorials/02-knowledge-hooks.md) - Getting started with hooks
