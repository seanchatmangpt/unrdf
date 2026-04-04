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
  effects: [{ kind: 'sparql-construct', query: '...' }],
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
  query: 'ASK { ?s a ex:Person }',
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
  t_ns: BigInt(Date.now() * 1000000),
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
  name: z.string().min(1).max(100).describe('Hook identifier'),

  condition: z
    .union([
      SparqlAskCondition,
      SparqlSelectCondition,
      ShaclCondition,
      DeltaCondition,
      ThresholdCondition,
      CountCondition,
      WindowCondition,
      N3Condition,
      DatalogCondition,
    ])
    .describe('Condition to evaluate (9 kinds)'),

  effects: z
    .array(z.union([SparqlConstructEffect, FunctionEffect]))
    .min(0)
    .max(10)
    .describe('Transformations to apply'),

  metadata: z.record(z.any()).optional().describe('Custom metadata'),
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
    enforcementMode: 'annotate',
  },
  effects: [
    {
      kind: 'sparql-construct',
      query: `CONSTRUCT { ?s ex:validated true } WHERE { ?s a ex:Trade }`,
    },
  ],
  metadata: {
    author: 'risk-team',
    version: '1.0.0',
    tags: ['compliance', 'fibo'],
  },
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

SHACL shape validation with 3 enforcement modes (now fully implemented).

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

**Enforcement Modes** (fully implemented):

- **`block`** (default): Fail if validation fails. Returns false on violations.
- **`annotate`**: Allow write with annotation. Adds SHACL validation report triples (sh:ValidationResult) to the store. Returns true to permit operation. Useful for logging violations without blocking.
- **`repair`**: Attempt automatic repair. Executes `repairConstruct` SPARQL query on validation failure, then re-validates. Returns repair success status.

**Example - Block Mode** (strictest):

```javascript
{
  kind: 'shacl',
  ref: { uri: 'file:///shapes/trade.ttl' },
  enforcementMode: 'block'
}
// Fails the hook if trade doesn't conform to shape
```

**Example - Annotate Mode** (logging + audit trail):

```javascript
{
  kind: 'shacl',
  ref: { uri: 'file:///shapes/trade.ttl' },
  enforcementMode: 'annotate'
}
// Adds sh:ValidationResult triples to store documenting violations
```

**Example - Repair Mode** (automatic fixing):

```javascript
{
  kind: 'shacl',
  ref: { uri: 'file:///shapes/trade.ttl' },
  enforcementMode: 'repair',
  repairConstruct: `
    CONSTRUCT {
      ?s ex:riskScore ?defaultRisk .
    }
    WHERE {
      ?s a ex:Trade .
      BIND (50 as ?defaultRisk)
    }
  `
}
// Auto-repairs missing values, then re-validates
```

---

### 4. DeltaCondition

Change detection with direction awareness (now supports increase/decrease detection).

```javascript
const DeltaCondition = z.object({
  kind: z.literal('delta'),

  spec: z.object({
    change: z.enum(['any', 'increase', 'decrease', 'modify'])
      .describe('Direction of change to detect'),

    threshold: z.number()
      .nonnegative()
      .default(0.1)
      .describe('Magnitude threshold for change (0-1)'),

    baseline: z.string()
      .optional()
      .describe('URI to baseline state for comparison')
  }).describe('Delta specification')
});

// Usage - Detect any change
{
  kind: 'delta',
  spec: {
    change: 'any'
  }
}

// Usage - Detect increase (more adds than deletes)
{
  kind: 'delta',
  spec: {
    change: 'increase',
    threshold: 0.1  // 10% growth
  }
}

// Usage - Detect decrease (more deletes than adds)
{
  kind: 'delta',
  spec: {
    change: 'decrease',
    threshold: 0.1  // 10% shrinkage
  }
}

// Usage - Detect any significant change (add or delete)
{
  kind: 'delta',
  spec: {
    change: 'modify',
    threshold: 0.05  // 5% change
  }
}
```

**Change Types** (now fully implemented):

- **`any`**: Trigger if any quad was added or deleted.
- **`increase`**: Trigger if net adds exceed threshold (additions - deletions > threshold \* totalQuads).
- **`decrease`**: Trigger if net deletions exceed threshold (deletions - additions > threshold \* totalQuads).
- **`modify`**: Trigger if absolute change exceeds threshold (|adds - deletes| > threshold \* totalQuads).

**Notes**:

- Threshold is expressed as a fraction of total quads (0.1 = 10% of store).
- Baseline comparison allows delta evaluation against a known good state.
- Perfect for triggering validation rules after large data imports.

---

### 5. ThresholdCondition

Numeric aggregation with comparison operators.

```javascript
const ThresholdCondition = z.object({
  kind: z.literal('threshold'),

  spec: z.object({
    var: z.string()
      .describe('Variable name to aggregate'),

    op: z.enum(['>', '>=', '<', '<=', '==', '!='])
      .describe('Comparison operator'),

    value: z.number()
      .describe('Threshold value'),

    aggregate: z.enum(['sum', 'avg', 'min', 'max', 'count'])
      .default('avg')
      .describe('Aggregation function')
  }).describe('Threshold specification')
});

// Usage - Count exceeds threshold
{
  kind: 'threshold',
  spec: {
    var: 'count',
    aggregate: 'count',
    op: '>',
    value: 100
  }
}

// Usage - Average value exceeds threshold
{
  kind: 'threshold',
  spec: {
    var: 'amount',
    aggregate: 'avg',
    op: '>=',
    value: 50000
  }
}

// Usage - Sum within range
{
  kind: 'threshold',
  spec: {
    var: 'value',
    aggregate: 'sum',
    op: '<',
    value: 1000000
  }
}
```

**Aggregate Functions**:

- **`count`**: Number of matching values
- **`sum`**: Total of numeric values
- **`avg`**: Average of numeric values
- **`min`**: Minimum value
- **`max`**: Maximum value

**Use Cases**:

- Validation: "total transaction amount must be < $1M"
- Anomaly detection: "average response time exceeds SLA"
- Quotas: "user has exceeded monthly upload limit"

---

### 6. CountCondition

Pattern cardinality with comparison operators.

```javascript
const CountCondition = z.object({
  kind: z.literal('count'),

  spec: z.object({
    op: z.enum(['>', '>=', '<', '<=', '==', '!='])
      .describe('Comparison operator'),

    value: z.number()
      .int()
      .nonnegative()
      .describe('Expected count'),

    query: z.string()
      .optional()
      .describe('Custom SPARQL query (if not using pattern)')
  }).describe('Count specification')
});

// Usage - Simple comparison
{
  kind: 'count',
  spec: {
    op: '>',
    value: 100
    // Counts all quads in store
  }
}

// Usage - With custom query
{
  kind: 'count',
  spec: {
    op: '>=',
    value: 50,
    query: 'SELECT ?s WHERE { ?s a ex:Trade ; ex:status ex:Active }'
  }
}

// Usage - Exact count
{
  kind: 'count',
  spec: {
    op: '==',
    value: 42
  }
}
```

**Comparison Operators**:

- **`>`**: Count > value
- **`>=`**: Count >= value
- **`<`**: Count < value
- **`<=`**: Count <= value
- **`==`**: Count equals value exactly
- **`!=`**: Count not equal to value

**Query Parameter**:

- If provided: counts results of the query
- If omitted: counts all quads in the store

**Use Cases**:

- Data integrity: "ensure at least 1000 entities loaded"
- Dataset validation: "verify expected result set size"
- Workflow control: "trigger next stage if count reaches threshold"

---

### 7. WindowCondition

Time-windowed aggregation evaluation (now properly implemented).

```javascript
const WindowCondition = z.object({
  kind: z.literal('window'),

  spec: z.object({
    size: z.number()
      .positive()
      .describe('Window size in milliseconds'),

    slide: z.number()
      .positive()
      .optional()
      .describe('Slide window by this much (defaults to size for tumbling window)'),

    aggregate: z.enum(['sum', 'avg', 'min', 'max', 'count'])
      .describe('Aggregation function'),

    query: z.string()
      .min(1)
      .describe('SPARQL query to extract values within window')
  }).describe('Window specification')
});

// Usage - Count-based window
{
  kind: 'window',
  spec: {
    size: 60000,      // 1 minute window
    aggregate: 'count',
    query: 'SELECT ?s WHERE { ?s a ex:Transaction ; ex:timestamp ?t }'
  }
}

// Usage - Sum aggregation
{
  kind: 'window',
  spec: {
    size: 300000,     // 5 minute window
    aggregate: 'sum',
    query: 'SELECT ?amount WHERE { ?s ex:amount ?amount }'
  }
}

// Usage - Average value
{
  kind: 'window',
  spec: {
    size: 86400000,   // 1 day window
    aggregate: 'avg',
    query: 'SELECT ?temperature WHERE { ?sensor ex:reading ?temperature }'
  }
}
```

**Aggregate Functions**:

- **`count`**: Number of matches in window
- **`sum`**: Sum of numeric values in window
- **`avg`**: Average of numeric values in window
- **`min`**: Minimum value in window
- **`max`**: Maximum value in window

**Use Cases**:

- Rate limiting: "trigger if more than 100 transactions/minute"
- Anomaly detection: "trigger if average temperature exceeds threshold"
- Sliding window validation: "check if sum of amounts exceeds daily limit"

---

### 8. N3Condition

Forward-chaining rules via EYE reasoning engine (semantic inference).

```javascript
const N3Condition = z.object({
  kind: z.literal('n3'),

  rules: z.string()
    .min(1)
    .describe('N3 rules in Notation3 format (forward-chaining)'),

  askQuery: z.string()
    .min(1)
    .describe('SPARQL ASK to verify derived facts')
});

// Usage - Simple forward rule
{
  kind: 'n3',
  rules: `
    @prefix : <http://example.org/> .
    { ?x a :RestrictedClass } => { ?x :requiresApproval true } .
  `,
  askQuery: 'ASK { ?s :requiresApproval true }'
}

// Usage - Multi-step inference chain
{
  kind: 'n3',
  rules: `
    @prefix : <http://example.org/> .
    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

    { ?s rdfs:subClassOf ?c . ?o a ?s } => { ?o a ?c } .
    { ?o a :HighRiskClass } => { ?o :requiresAudit true } .
    { ?o :requiresAudit true } => { ?o :needsReview true } .
  `,
  askQuery: 'ASK { ?s :needsReview true }'
}

// Usage - Business logic inference
{
  kind: 'n3',
  rules: `
    @prefix : <http://example.org/> .

    { ?trade a :Trade ; :amount ?amt . ?amt > 1000000 }
    => { ?trade :requiresCompliance true } .

    { ?trade :requiresCompliance true }
    => { ?trade :requiresRiskReview true } .
  `,
  askQuery: 'ASK { ?trade :requiresRiskReview true }'
}
```

**Rule Syntax** (Notation3):

```
{ ANTECEDENT_PATTERN } => { CONSEQUENT_PATTERN } .
```

**Features**:

- **Full forward-chaining**: Derives new facts from rules until fixpoint
- **Semantic inference**: Supports RDFS, OWL-like reasoning
- **Multi-rule compositions**: Chain rules for complex business logic
- **Built on EYE**: Production-grade reasoner from W3C

**Performance Notes**:

- Rules are evaluated until no new facts derive (fixpoint)
- Large graph + complex rules = longer evaluation
- Consider materializing inference results for repeated queries

**Example - Access Control**:

```javascript
{
  kind: 'n3',
  rules: `
    @prefix : <http://example.org/> .

    { ?u a :Admin } => { ?u :canAccess :SecureArea } .
    { ?u :isMemberOf ?g . ?g :hasPermission :EditContent }
      => { ?u :canAccess :EditContent } .
    { ?u :canAccess :SecureArea }
      => { ?u :requiresAuditLog true } .
  `,
  askQuery: 'ASK { ?u :requiresAuditLog true }'
}
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
  successful: z.number().nonnegative().describe('Successfully executed hooks'),

  failed: z.number().nonnegative().describe('Failed hooks'),

  skipped: z.number().nonnegative().describe('Skipped hooks'),

  receipt: Receipt.describe('Cryptographic proof'),

  violations: z
    .array(
      z.object({
        hookName: z.string(),
        focusNode: z.string().optional(),
        resultMessage: z.string(),
        severity: z.enum(['VIOLATION', 'WARNING', 'INFO']),
      })
    )
    .optional()
    .describe('SHACL violations (annotate mode)'),

  errors: z
    .array(
      z.object({
        hookName: z.string(),
        message: z.string(),
        code: z.string().optional(),
      })
    )
    .optional()
    .describe('Execution errors'),

  deltaMatched: z.boolean().optional().describe('For delta conditions'),

  repairs: z.number().nonnegative().optional().describe('Applied repairs (repair mode)'),
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

10+ pre-defined hooks for IRI validation, literal processing, and normalization.

### IRI Validation

```javascript
import {
  validateSubjectIRI,
  validatePredicateIRI,
  validateIRIFormat,
  executeHook,
} from '@unrdf/hooks';

const quad = {
  subject: { value: 'http://example.org/s' },
  predicate: { value: 'http://example.org/p' },
  object: { value: 'test' },
};

// Validate subject is a named node IRI
const result = await executeHook(validateSubjectIRI, quad);
console.log('Valid:', result.valid); // true

// Validate predicate is a named node IRI
await executeHook(validatePredicateIRI, quad);

// Validate IRI format (RFC3987)
await executeHook(validateIRIFormat, quad);
```

### Literal Validation

```javascript
import { validateObjectLiteral, validateLanguageTag, executeHook } from '@unrdf/hooks';

const literalQuad = {
  subject: { value: 'http://example.org/s' },
  predicate: { value: 'http://example.org/label' },
  object: { value: 'Hello', language: 'en' },
};

// Validate literal value (non-empty)
await executeHook(validateObjectLiteral, literalQuad);

// Validate BCP47 language tags
await executeHook(validateLanguageTag, literalQuad);
```

### Node Type Validation

```javascript
import { rejectBlankNodes, executeHook } from '@unrdf/hooks';

const quad = {
  subject: { value: 'http://example.org/s', termType: 'NamedNode' },
  predicate: { value: 'http://example.org/p', termType: 'NamedNode' },
  object: { value: 'test', termType: 'Literal' },
};

// Reject blank nodes in any position
await executeHook(rejectBlankNodes, quad); // passes
```

### Normalization

```javascript
import {
  normalizeNamespace,
  normalizeLanguageTag,
  trimLiterals,
  normalizeLanguageTagPooled,
  trimLiteralsPooled,
  executeHook,
} from '@unrdf/hooks';

// Normalize namespace URIs
const normQuad = await executeHook(normalizeNamespace, quad);

// Normalize language tags to lowercase
const enQuad = {
  ...quad,
  object: { value: 'Hello', language: 'EN-US' },
};
const result = await executeHook(normalizeLanguageTag, enQuad);
console.log(result.quad.object.language); // 'en-us'

// Trim whitespace from literals
const spacedQuad = {
  ...quad,
  object: { value: '  hello  ', termType: 'Literal' },
};
const trimmed = await executeHook(trimLiterals, spacedQuad);
console.log(trimmed.quad.object.value); // 'hello'

// Pooled versions use zero-allocation object pooling
const pooledResult = await executeHook(normalizeLanguageTagPooled, enQuad);
console.log(pooledResult.quad.object.language); // 'en-us'
```

### Composite Validation

```javascript
import { standardValidation, executeHook, executeHookChain } from '@unrdf/hooks';

const quad = {
  subject: { value: 'http://example.org/s', termType: 'NamedNode' },
  predicate: { value: 'http://example.org/p', termType: 'NamedNode' },
  object: { value: 'test', termType: 'Literal' },
};

// All-in-one validation (IRI format + no blank nodes + valid literals)
const result = await executeHook(standardValidation, quad);
console.log('Passed all checks:', result.valid);

// Chain multiple hooks for custom validation
const hooks = [standardValidation, normalizeLanguageTag, trimLiterals];

const chainResult = await executeHookChain(hooks, quad);
```

### Batch Operations

```javascript
import {
  validateSubjectIRI,
  executeBatch,
  validateBatch,
  transformBatch
} from '@unrdf/hooks';

const quads = [
  { subject: { value: 'http://example.org/s1' }, ... },
  { subject: { value: 'http://example.org/s2' }, ... },
  { subject: { value: 'invalid' }, ... }
];

// Batch validation
const results = await executeBatch([validateSubjectIRI], quads);
results.forEach((r, i) => {
  console.log(`Quad ${i}: ${r.valid ? 'valid' : 'invalid'}`);
});

// Returns array of booleans
const validArray = await validateBatch([validateSubjectIRI], quads);

// Apply transformations to batch
const normalized = await transformBatch([normalizeLanguageTag], quads);
```

### Hook List

| Hook                         | Type       | Purpose                     |
| ---------------------------- | ---------- | --------------------------- |
| `validateSubjectIRI`         | Validator  | Subject must be NamedNode   |
| `validatePredicateIRI`       | Validator  | Predicate must be NamedNode |
| `validateObjectLiteral`      | Validator  | Object literal non-empty    |
| `validateIRIFormat`          | Validator  | RFC3987 IRI structure       |
| `validateLanguageTag`        | Validator  | BCP47 language tags         |
| `rejectBlankNodes`           | Validator  | No blank nodes anywhere     |
| `normalizeNamespace`         | Normalizer | Standardize namespace URIs  |
| `normalizeLanguageTag`       | Normalizer | Lowercase language tags     |
| `trimLiterals`               | Normalizer | Remove whitespace           |
| `normalizeLanguageTagPooled` | Normalizer | Pooled normalization        |
| `trimLiteralsPooled`         | Normalizer | Pooled trimming             |
| `standardValidation`         | Composite  | All-in-one validation       |

### Performance Features

**Object Pooling** - Pooled variants reduce allocation overhead:

```javascript
import { normalizeLanguageTagPooled, isPooledQuad } from '@unrdf/hooks';

const result = await executeHook(normalizeLanguageTagPooled, quad);
console.log('Uses pooled memory:', isPooledQuad(result.quad)); // true
```

**JIT Compilation** - Hook chains are compiled for speed:

```javascript
import { compileHookChain, defineHook, executeHookChain } from '@unrdf/hooks';

const hooks = [
  defineHook({ name: 'h1', trigger: 'before-add', validate: () => true }),
  defineHook({ name: 'h2', trigger: 'before-add', validate: () => true }),
];

// Automatically compiled on first use, then reused
await executeHookChain(hooks, quad);
await executeHookChain(hooks, quad); // Faster - uses compiled version
```

### Testing Built-in Hooks

See `test/builtin-hooks-advanced.test.mjs` for comprehensive test suite covering:

- All 12+ built-in hooks
- Edge cases and boundary conditions
- Hook composition and chaining
- Batch operations at scale
- Performance characteristics

---

## Utilities

### createKnowledgeHook

Create and validate hook.

```javascript
import { createKnowledgeHook } from '@unrdf/hooks';

const hook = createKnowledgeHook({
  name: 'my-hook',
  condition: { kind: 'sparql-ask', query: 'ASK { ?s ?p ?o }' },
  effects: [{ kind: 'sparql-construct', query: '...' }],
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
  previousReceiptHash: 'optional-hash',
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
  ReceiptGenerationError,
} from '@unrdf/hooks';
```

---

## Complete API Summary Table

| API                       | Type     | Returns                    | Throws                     |
| ------------------------- | -------- | -------------------------- | -------------------------- |
| `registerHook()`          | Method   | `string` (hookId)          | `ValidationError`          |
| `evaluateCondition()`     | Async    | `Promise<boolean>`         | `ConditionEvaluationError` |
| `execute()`               | Async    | `Promise<ExecutionResult>` | `HookExecutionError`       |
| `getReceiptChain()`       | Method   | `Receipt[]`                | —                          |
| `clearReceiptChain()`     | Method   | `void`                     | —                          |
| `createKnowledgeHook()`   | Function | `KnowledgeHook`            | `ValidationError`          |
| `validateKnowledgeHook()` | Function | `ValidationResult`         | —                          |

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
      mediaType: 'text/turtle',
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
    `,
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
      `,
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
      `,
    },
  ],
  metadata: {
    version: '1.0.0',
    author: 'compliance-team',
    tags: ['fibo', 'validation', 'required'],
    documentation: 'https://docs.example.com/hooks/trade-validation',
  },
});
```

See examples/ directory for more working examples.

---

## Breaking Changes & Migration

### v26.4.3 - Documentation Updates

**What Changed:**

- All 9 condition kinds now have complete documentation
- SHACL repair mode fully documented with three enforcement modes
- Delta conditions now support directional change detection (increase/decrease/modify)
- Window conditions support time-windowed aggregation
- N3 reasoning conditions fully integrated with EYE reasoner

**Migration Required?**

- No breaking changes to APIs
- Existing code continues to work without modification
- New documentation clarifies usage patterns that were previously unclear

**What's New:**

1. **SHACL Repair Mode**: Automatic data fixing on validation failure
   - Was: Only 'block' mode functional
   - Now: 'repair' and 'annotate' modes fully operational

2. **Delta Decrease Detection**: Can now detect data loss
   - Was: Only crude change detection
   - Now: Precise directional detection (increase/decrease/modify)

3. **Window Aggregation**: Time-windowed metrics
   - Was: Window conditions not implemented
   - Now: Full support for count/sum/avg/min/max over time windows

4. **N3 Forward-Chaining**: Semantic inference
   - Was: Experimental, minimal documentation
   - Now: Full integration with EYE reasoner, complete examples

**No Deprecations**: All existing condition kinds remain supported.
