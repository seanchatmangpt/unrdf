# @unrdf/hooks API Reference

Complete API documentation with Zod schemas and usage examples.

## Table of Contents

1. [KnowledgeHookEngine](#knowledgehookengine)
2. [Hook Definition](#hook-definition)
3. [Condition Schemas](#condition-schemas)
4. [Effect Schemas](#effect-schemas)
5. [Receipt Schema](#receipt-schema)
6. [Execution Results](#execution-results)
7. [Error Types](#error-types)
8. [Built-in Hooks](#built-in-hooks)
9. [Utilities](#utilities)

---

## KnowledgeHookEngine

Main class for hook management and execution.

### Constructor

```javascript
import { KnowledgeHookEngine } from '@unrdf/hooks';
import { createStore } from '@unrdf/oxigraph';

const store = createStore();
const engine = new KnowledgeHookEngine(store);
```

### Methods

#### `registerHook(hook: KnowledgeHook): string`

Register a hook and return its ID.

```javascript
const hookId = engine.registerHook({
  name: 'my-hook',
  condition: { kind: 'sparql-ask', query: 'ASK { ?s ?p ?o }' },
  effects: [{ kind: 'sparql-construct', query: '...' }]
});

console.log('Hook ID:', hookId);
```

**Parameters**:
- `hook` - Hook definition (Zod validated)

**Returns**: `string` - UUID of registered hook

**Throws**: `ValidationError` if hook schema invalid

---

#### `async evaluateCondition(condition: Condition): Promise<boolean>`

Evaluate any condition kind.

```javascript
const result = await engine.evaluateCondition({
  kind: 'sparql-ask',
  query: 'ASK { ?s a ex:Person }'
});

console.log('Condition passed:', result);
```

**Parameters**:
- `condition` - Condition object (9 kinds)

**Returns**: `Promise<boolean>` - True if condition passes

**Throws**: 
- `ConditionEvaluationError` - Invalid condition
- `QueryExecutionError` - SPARQL/Datalog execution failed

---

#### `async execute(context: ExecutionContext, hooks: KnowledgeHook[]): Promise<ExecutionResult>`

Execute hooks with receipt chaining.

```javascript
const ctx = createContext({
  nodeId: 'my-app',
  t_ns: BigInt(Date.now() * 1000000)
});

const result = await engine.execute(ctx, [hook1, hook2]);

console.log('Successful:', result.successful);
console.log('Failed:', result.failed);
console.log('Receipt:', result.receipt);
```

**Parameters**:
- `context` - Execution context with nodeId, t_ns, optional previousReceiptHash
- `hooks` - Array of KnowledgeHook definitions

**Returns**: `Promise<ExecutionResult>` - Result with receipt and delta

**Throws**:
- `HookExecutionError` - Hook execution failed
- `ReceiptGenerationError` - Receipt creation failed

---

#### `getReceiptChain(): Receipt[]`

Get entire receipt chain.

```javascript
const chain = engine.getReceiptChain();

chain.forEach((receipt, i) => {
  console.log(`Receipt ${i}: ${receipt.receiptHash}`);
  console.log(`  Previous: ${receipt.previousReceiptHash}`);
  console.log(`  Delta: +${receipt.delta.adds.length} -${receipt.delta.deletes.length}`);
});
```

**Returns**: `Receipt[]` - Array of receipts in execution order

---

#### `clearReceiptChain(): void`

Clear receipt history.

```javascript
engine.clearReceiptChain();
```

---

## Hook Definition

### KnowledgeHook Schema (Zod)

```javascript
import { z } from 'zod';
import { KnowledgeHookSchema } from '@unrdf/hooks';

// Schema structure:
const KnowledgeHook = z.object({
  name: z.string()
    .min(1).max(100)
    .describe('Hook identifier'),
  
  condition: z.union([
    SparqlAskCondition,
    SparqlSelectCondition,
    ShaclCondition,
    DeltaCondition,
    ThresholdCondition,
    CountCondition,
    WindowCondition,
    N3Condition,
    DatalogCondition
  ]).describe('Condition to evaluate (9 kinds)'),
  
  effects: z.array(
    z.union([
      SparqlConstructEffect,
      FunctionEffect
    ])
  ).min(0).max(10)
    .describe('Transformations to apply'),
  
  metadata: z.record(z.any())
    .optional()
    .describe('Custom metadata')
});
```

### Complete Example

```javascript
import { createKnowledgeHook, validateKnowledgeHook } from '@unrdf/hooks';

const hook = createKnowledgeHook({
  name: 'trade-validation',
  condition: {
    kind: 'shacl',
    ref: { uri: 'file:///shapes/trade.ttl' },
    enforcementMode: 'annotate'
  },
  effects: [{
    kind: 'sparql-construct',
    query: `CONSTRUCT { ?s ex:validated true } WHERE { ?s a ex:Trade }`
  }],
  metadata: {
    author: 'risk-team',
    version: '1.0.0',
    tags: ['compliance', 'fibo']
  }
});

// Validate
const validation = validateKnowledgeHook(hook);
if (!validation.success) {
  console.error('Invalid hook:', validation.error);
}
```

---

## Condition Schemas

### 1. SparqlAskCondition

Boolean SPARQL query.

```javascript
const SparqlAskCondition = z.object({
  kind: z.literal('sparql-ask'),
  query: z.string()
    .min(1)
    .describe('SPARQL ASK query')
});

// Usage
{
  kind: 'sparql-ask',
  query: 'ASK { ?s a ex:Person }'
}
```

---

### 2. SparqlSelectCondition

SPARQL SELECT with result binding.

```javascript
const SparqlSelectCondition = z.object({
  kind: z.literal('sparql-select'),
  query: z.string()
    .min(1)
    .describe('SPARQL SELECT query')
});

// Usage
{
  kind: 'sparql-select',
  query: 'SELECT ?s ?name WHERE { ?s a ex:Person ; ex:name ?name }'
}
```

---

### 3. ShaclCondition

SHACL shape validation (3 enforcement modes).

```javascript
const ShaclCondition = z.object({
  kind: z.literal('shacl'),
  
  ref: z.object({
    uri: z.string().min(1).describe('Shape file URI'),
    sha256: z.string()
      .regex(/^[a-f0-9]{64}$/)
      .optional()
      .describe('SHA-256 hash of shape file'),
    mediaType: z.string().optional().describe('MIME type')
  }),
  
  enforcementMode: z.enum(['block', 'annotate', 'repair'])
    .default('block')
    .describe('Governance mode'),
  
  repairConstruct: z.string()
    .optional()
    .describe('SPARQL CONSTRUCT for repair mode')
});

// Usage
{
  kind: 'shacl',
  ref: { uri: 'file:///shapes/person.ttl' },
  enforcementMode: 'block',
  repairConstruct: 'CONSTRUCT { ?s ex:repaired true } WHERE { ... }'
}
```

---

### 4. DeltaCondition

Change detection (adds/deletes).

```javascript
const DeltaCondition = z.object({
  kind: z.literal('delta'),
  
  adds: z.array(
    z.object({
      subject: z.string(),
      predicate: z.string(),
      object: z.string()
    })
  ).optional().describe('Patterns to match in additions'),
  
  deletes: z.array(
    z.object({
      subject: z.string(),
      predicate: z.string(),
      object: z.string()
    })
  ).optional().describe('Patterns to match in deletions')
});

// Usage
{
  kind: 'delta',
  adds: [
    {
      subject: '?trade',
      predicate: 'rdf:type',
      object: 'ex:Trade'
    }
  ],
  deletes: []
}
```

---

### 5. ThresholdCondition

Numeric comparison.

```javascript
const ThresholdCondition = z.object({
  kind: z.literal('threshold'),
  
  query: z.string()
    .min(1)
    .describe('SPARQL SELECT returning numeric value'),
  
  operator: z.enum([
    'greaterThan',
    'lessThan',
    'equal',
    'greaterThanOrEqual',
    'lessThanOrEqual'
  ]).describe('Comparison operator'),
  
  value: z.number()
    .describe('Threshold value')
});

// Usage
{
  kind: 'threshold',
  query: 'SELECT (COUNT(?s) as ?count) WHERE { ?s a ex:Trade }',
  operator: 'greaterThan',
  value: 100
}
```

---

### 6. CountCondition

Pattern cardinality.

```javascript
const CountCondition = z.object({
  kind: z.literal('count'),
  
  pattern: z.object({
    subject: z.string(),
    predicate: z.string(),
    object: z.string()
  }).describe('Pattern to count'),
  
  expected: z.number()
    .int()
    .nonnegative()
    .describe('Expected count')
});

// Usage
{
  kind: 'count',
  pattern: {
    subject: '?s',
    predicate: 'ex:status',
    object: 'ex:Active'
  },
  expected: 50
}
```

---

### 7. WindowCondition

Time range evaluation.

```javascript
const WindowCondition = z.object({
  kind: z.literal('window'),
  
  windowMs: z.number()
    .positive()
    .describe('Time window in milliseconds'),
  
  maxMatches: z.number()
    .positive()
    .describe('Maximum matches allowed in window'),
  
  query: z.string()
    .min(1)
    .describe('SPARQL query to count matches')
});

// Usage
{
  kind: 'window',
  windowMs: 60000,      // 1 minute
  maxMatches: 10,       // Max 10 per minute
  query: 'SELECT ?timestamp WHERE { ?s ex:createdAt ?timestamp }'
}
```

---

### 8. N3Condition

Forward-chaining rules via EYE.

```javascript
const N3Condition = z.object({
  kind: z.literal('n3'),
  
  rules: z.string()
    .min(1)
    .describe('N3 rules (Notation3 format)'),
  
  askQuery: z.string()
    .min(1)
    .describe('SPARQL ASK to verify rule results')
});

// Usage
{
  kind: 'n3',
  rules: `
    { ?x a :RestrictedClass } => { ?x :requiresApproval true } .
    { ?x :riskScore ?score . ?score > 50 } => { ?x :requiresReview true } .
  `,
  askQuery: 'ASK { ?s :requiresApproval true }'
}
```

**Rule Syntax**:
```
{ ANTECEDENT } => { CONSEQUENT } .
```

Example:
```
{ ?x a :HighRisk ; :exposure ?exp } => { ?x :needsReview true } .
```

---

### 9. DatalogCondition

Logic programming via bottom-up evaluation.

```javascript
const DatalogCondition = z.object({
  kind: z.literal('datalog'),
  
  facts: z.array(z.string())
    .min(1)
    .describe('Facts (ground atoms)'),
  
  rules: z.array(z.string())
    .min(0)
    .describe('Deduction rules'),
  
  goal: z.string()
    .min(1)
    .describe('Goal to prove')
});

// Usage
{
  kind: 'datalog',
  facts: [
    'user(alice)',
    'admin(alice)',
    'group(admins)',
    'member(alice, admins)'
  ],
  rules: [
    'allowed(X) :- admin(X)',
    'allowed(X) :- member(X, admins)'
  ],
  goal: 'allowed(alice)'
}
```

**Syntax**:
- Facts: `predicate(arg1, arg2, ...)`
- Rules: `head(X, Y) :- body(X), body2(Y)`
- Goal: `predicate(value, Variable)`

---

## Effect Schemas

### SparqlConstructEffect

RDF-native transformation.

```javascript
const SparqlConstructEffect = z.object({
  kind: z.literal('sparql-construct'),
  
  query: z.string()
    .min(1)
    .describe('SPARQL CONSTRUCT query')
});

// Usage
{
  kind: 'sparql-construct',
  query: `
    CONSTRUCT {
      ?s ex:processed true ;
         ex:processedAt ?now .
    }
    WHERE {
      ?s a ex:Trade .
      BIND (NOW() as ?now)
    }
  `
}
```

**Features**:
- Pure RDF transformation
- No JavaScript execution risk
- Composable with other SPARQL
- Deterministic output

---

### FunctionEffect

Custom JavaScript logic (legacy).

```javascript
const FunctionEffect = z.object({
  kind: z.literal('function'),
  
  inline: z.function().optional()
    .describe('Async function for transformation'),
  
  ref: z.object({
    uri: z.string().min(1),
    sha256: z.string().regex(/^[a-f0-9]{64}$/).optional(),
    mediaType: z.string().optional()
  }).optional()
    .describe('External function reference'),
  
  timeout: z.number()
    .int()
    .positive()
    .max(300000)
    .default(30000)
    .describe('Execution timeout in ms'),
  
  retries: z.number()
    .int()
    .nonnegative()
    .max(5)
    .default(1)
    .describe('Retry attempts'),
  
  sandbox: z.boolean()
    .default(false)
    .describe('Run in sandbox (security)')
});

// Usage
{
  kind: 'function',
  inline: async (store, quad) => {
    console.log('Processing:', quad);
    return { success: true, modified: 1 };
  },
  timeout: 30000,
  retries: 1,
  sandbox: false
}
```

---

## Receipt Schema

Cryptographic proof of execution with BLAKE3 chaining.

```javascript
const Receipt = z.object({
  receiptHash: z.string()
    .describe('BLAKE3(entire receipt)'),
  
  payloadHash: z.string()
    .describe('BLAKE3(hook definitions)'),
  
  input_hash: z.string()
    .describe('BLAKE3(store state before)'),
  
  output_hash: z.string()
    .describe('BLAKE3(store state after)'),
  
  previousReceiptHash: z.string()
    .optional()
    .describe('Links to prior operation'),
  
  timestamp: z.number()
    .describe('Execution timestamp (ms)'),
  
  nodeId: z.string()
    .describe('Node/application identifier'),
  
  delta: z.object({
    adds: z.array(
      z.object({
        subject: z.string(),
        predicate: z.string(),
        object: z.string()
      })
    ),
    deletes: z.array(
      z.object({
        subject: z.string(),
        predicate: z.string(),
        object: z.string()
      })
    )
  }).describe('RDF changes (adds/deletes)'),
  
  hooksExecuted: z.number()
    .nonnegative()
    .describe('Total hooks executed'),
  
  successful: z.number()
    .nonnegative()
    .describe('Successfully executed'),
  
  failed: z.number()
    .nonnegative()
    .describe('Failed hooks')
});

// Example receipt
{
  receiptHash: 'a3f7d9e2c8f4b6a1e9c7d5f3a1b9e8c6',
  payloadHash: 'b9e2c4d1f7a3e8c9b5d3f1a7e9c6b4a2',
  input_hash: 'c4d1e7f3a9b5c1d7e3f9a5b1c7d3e9f5',
  output_hash: 'e7f3a9b5c1d7e3f9a5b1c7d3e9f5a1b7',
  previousReceiptHash: '8f2a4c6e8a0c2e4a6c8e0a2c4e6a8c0e',
  timestamp: 1680000000000,
  nodeId: 'my-app',
  delta: {
    adds: [
      {
        subject: 'ex:alice',
        predicate: 'ex:verified',
        object: 'true'
      }
    ],
    deletes: []
  },
  hooksExecuted: 3,
  successful: 2,
  failed: 0
}
```

---

## Execution Results

### ExecutionResult

```javascript
const ExecutionResult = z.object({
  successful: z.number()
    .nonnegative()
    .describe('Successfully executed hooks'),
  
  failed: z.number()
    .nonnegative()
    .describe('Failed hooks'),
  
  skipped: z.number()
    .nonnegative()
    .describe('Skipped hooks'),
  
  receipt: Receipt
    .describe('Cryptographic proof'),
  
  violations: z.array(
    z.object({
      hookName: z.string(),
      focusNode: z.string().optional(),
      resultMessage: z.string(),
      severity: z.enum(['VIOLATION', 'WARNING', 'INFO'])
    })
  ).optional().describe('SHACL violations (annotate mode)'),
  
  errors: z.array(
    z.object({
      hookName: z.string(),
      message: z.string(),
      code: z.string().optional()
    })
  ).optional().describe('Execution errors'),
  
  deltaMatched: z.boolean()
    .optional()
    .describe('For delta conditions'),
  
  repairs: z.number()
    .nonnegative()
    .optional()
    .describe('Applied repairs (repair mode)')
});
```

---

## Error Types

### HookExecutionError

Hook execution failed.

```javascript
try {
  await engine.execute(ctx, [hook]);
} catch (err) {
  if (err instanceof HookExecutionError) {
    console.error('Hook failed:', err.hookName);
    console.error('Reason:', err.message);
    console.error('Code:', err.code);
  }
}
```

### ConditionEvaluationError

Condition evaluation failed.

```javascript
try {
  await engine.evaluateCondition(condition);
} catch (err) {
  if (err instanceof ConditionEvaluationError) {
    console.error('Condition error:', err.message);
    console.error('Kind:', err.conditionKind);
  }
}
```

### ValidationError

Hook schema validation failed.

```javascript
try {
  createKnowledgeHook(invalidHook);
} catch (err) {
  if (err instanceof ValidationError) {
    console.error('Validation failed:');
    err.issues.forEach(issue => {
      console.error(`  ${issue.path}: ${issue.message}`);
    });
  }
}
```

### ReceiptGenerationError

Receipt creation failed.

```javascript
try {
  await engine.execute(ctx, [hook]);
} catch (err) {
  if (err instanceof ReceiptGenerationError) {
    console.error('Receipt creation failed:', err.message);
  }
}
```

---

## Built-in Hooks

Pre-defined validation hooks.

```javascript
import {
  validateIRIFormat,
  rejectBlankNodes,
  normalizeNamespace,
  validateLanguageTag,
  trimLiterals,
  standardValidation
} from '@unrdf/hooks';

// IRI Format Validation
const iriHook = validateIRIFormat();

// Blank Node Rejection
const blankNodeHook = rejectBlankNodes();

// Namespace Normalization
const normHook = normalizeNamespace();

// Language Tag Validation
const langHook = validateLanguageTag();

// Literal Trimming
const trimHook = trimLiterals();

// Standard Validation Suite
const standardHooks = standardValidation();

// Use in execution
const result = await engine.execute(ctx, [
  iriHook,
  blankNodeHook,
  normHook
]);
```

---

## Utilities

### createKnowledgeHook

Create and validate hook.

```javascript
import { createKnowledgeHook } from '@unrdf/hooks';

const hook = createKnowledgeHook({
  name: 'my-hook',
  condition: { kind: 'sparql-ask', query: 'ASK { ?s ?p ?o }' },
  effects: [{ kind: 'sparql-construct', query: '...' }]
});

// Validated and ready to use
```

### validateKnowledgeHook

Validate hook definition.

```javascript
import { validateKnowledgeHook } from '@unrdf/hooks';

const validation = validateKnowledgeHook(hookDef);

if (validation.success) {
  console.log('Valid hook');
} else {
  console.error('Errors:', validation.error.issues);
}
```

### ExecutionContext

Context for hook execution.

```javascript
import { createContext } from '@unrdf/v6-core/receipt-pattern';

const ctx = createContext({
  nodeId: 'my-app',
  t_ns: BigInt(Date.now() * 1000000),
  previousReceiptHash: 'optional-hash'
});
```

---

## Type Exports

All types available for TypeScript/JSDoc:

```javascript
import {
  KnowledgeHook,
  Condition,
  Effect,
  Receipt,
  ExecutionResult,
  ExecutionContext,
  HookExecutionError,
  ConditionEvaluationError,
  ValidationError,
  ReceiptGenerationError
} from '@unrdf/hooks';
```

---

## Complete API Summary Table

| API | Type | Returns | Throws |
|---|---|---|---|
| `registerHook()` | Method | `string` (hookId) | `ValidationError` |
| `evaluateCondition()` | Async | `Promise<boolean>` | `ConditionEvaluationError` |
| `execute()` | Async | `Promise<ExecutionResult>` | `HookExecutionError` |
| `getReceiptChain()` | Method | `Receipt[]` | — |
| `clearReceiptChain()` | Method | `void` | — |
| `createKnowledgeHook()` | Function | `KnowledgeHook` | `ValidationError` |
| `validateKnowledgeHook()` | Function | `ValidationResult` | — |

---

## Examples

### Complete Hook Definition

```javascript
import { createKnowledgeHook } from '@unrdf/hooks';

const completeHook = createKnowledgeHook({
  name: 'trade-validation-complete',
  condition: {
    kind: 'shacl',
    ref: {
      uri: 'file:///shapes/trade-shape.ttl',
      sha256: 'a3f7d9e2c8f4b6a1e9c7d5f3a1b9e8c6',
      mediaType: 'text/turtle'
    },
    enforcementMode: 'annotate',
    repairConstruct: `
      CONSTRUCT {
        ?trade ex:repaired true .
      }
      WHERE {
        ?violation a sh:ValidationResult ;
                   sh:focusNode ?trade .
      }
    `
  },
  effects: [
    {
      kind: 'sparql-construct',
      query: `
        CONSTRUCT {
          ?trade ex:validated true ;
                 ex:validatedAt ?now .
        }
        WHERE {
          ?trade a ex:Trade .
          BIND (NOW() as ?now)
        }
      `
    },
    {
      kind: 'sparql-construct',
      query: `
        CONSTRUCT {
          ?trade ex:status ex:Ready .
        }
        WHERE {
          ?trade a ex:Trade ;
                 ex:validated true .
        }
      `
    }
  ],
  metadata: {
    version: '1.0.0',
    author: 'compliance-team',
    tags: ['fibo', 'validation', 'required'],
    documentation: 'https://docs.example.com/hooks/trade-validation'
  }
});
```

See [EXAMPLES.md](./EXAMPLES.md) for more working examples.
