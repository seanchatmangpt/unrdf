# 🎯 Knowledge Hooks API Reference

**Complete API reference for unrdf's Knowledge Hooks - the primary API for reactive knowledge graphs**

## Overview

The Knowledge Hooks API provides a declarative, type-safe interface for building reactive RDF applications. It consists of three main functions that work together to create, evaluate, and manage knowledge hooks with cryptographic provenance.

## Core Functions

### defineHook

Creates a new Knowledge Hook with the specified configuration.

```javascript
/**
 * Define a new Knowledge Hook
 * @param {Object} spec - Hook specification
 * @param {string} spec.id - Unique hook identifier (IRI)
 * @param {string} [spec.name] - Human-readable hook name
 * @param {string} [spec.description] - Hook description
 * @param {string} spec.select - SPARQL SELECT query to execute
 * @param {Array} spec.predicates - Array of predicate specifications
 * @param {string} spec.combine - Logic to combine predicates ('AND', 'OR', 'NOT')
 * @param {Object} [spec.output] - Output configuration
 * @param {Object} [spec.baseline] - Baseline data for delta comparisons
 * @returns {Hook} Immutable hook object
 */
function defineHook(spec)
```

#### Parameters

**spec.id** (required)
- Type: `string`
- Description: Unique identifier for the hook (must be an IRI)
- Example: `'ex:ServiceHealthMonitor'`

**spec.name** (optional)
- Type: `string`
- Description: Human-readable name for the hook
- Example: `'Critical Service Health Monitor'`

**spec.description** (optional)
- Type: `string`
- Description: Detailed description of what the hook monitors
- Example: `'Monitors service error rates and latency for critical services'`

**spec.select** (required)
- Type: `string`
- Description: SPARQL SELECT query that defines what data to monitor
- Example: `'SELECT ?service ?errorRate WHERE { ?service ex:errorRate ?errorRate }'`

**spec.predicates** (required)
- Type: `Array<PredicateSpec>`
- Description: Array of predicate specifications to evaluate

**spec.combine** (required)
- Type: `string`
- Description: Logic for combining predicate results
- Values: `'AND'`, `'OR'`, `'NOT'`

**spec.output** (optional)
- Type: `OutputSpec`
- Description: Configuration for hook output formatting and delivery

**spec.baseline** (optional)
- Type: `BaselineSpec`
- Description: Baseline data configuration for delta comparisons

#### Predicate Specifications

##### ASK Predicate
```javascript
{
  kind: 'ASK',
  spec: {
    query: 'ASK WHERE { ?service ex:critical true }',
    expected: true  // Optional: invert result (default: false)
  }
}
```

##### THRESHOLD Predicate
```javascript
{
  kind: 'THRESHOLD',
  spec: {
    var: 'errorRate',        // Variable to check
    op: '>',                 // Operator: >, >=, <, <=, ==, !=
    value: 0.02,            // Threshold value
    aggregate: 'avg'        // Optional: avg, sum, count, max, min
  }
}
```

##### DELTA Predicate
```javascript
{
  kind: 'DELTA',
  spec: {
    change: 'any',           // 'increase', 'decrease', 'any'
    key: ['service'],       // Variables that identify the row
    threshold: 0.1          // Optional: minimum change percentage
  }
}
```

##### SHACL Predicate
```javascript
{
  kind: 'SHACL',
  spec: {
    shapes: 'ex:ServiceShape',
    mode: 'violations',      // 'conforms' or 'violations'
    strict: true            // Fail on first violation
  }
}
```

##### COUNT Predicate
```javascript
{
  kind: 'COUNT',
  spec: {
    op: '>',                // Operator: >, >=, <, <=, ==, !=
    value: 10              // Expected count
  }
}
```

##### WINDOW Predicate
```javascript
{
  kind: 'WINDOW',
  spec: {
    var: 'requests',        // Variable to aggregate
    size: '5m',            // Window size: 1m, 5m, 1h, etc.
    op: 'count',           // Operation: count, sum, avg
    cmp: {
      op: '>',             // Comparison operator
      value: 100          // Threshold value
    }
  }
}
```

#### Output Specifications

**spec.output.schema**
- Type: `ZodSchema`
- Description: Zod schema for validating and structuring output data
- Example: `z.object({ service: z.string(), alert: z.string() })`

**spec.output.format**
- Type: `string`
- Description: Output format
- Values: `'json'`, `'turtle'`, `'jsonld'`

**spec.output.destination**
- Type: `string`
- Description: Where to send output
- Values: `'console'`, `'webhook'`, `'file'`, `'database'`, `'custom'`

**spec.output.webhook** (for webhook destination)
```javascript
{
  url: 'https://api.example.com/alerts',
  method: 'POST',
  headers: { 'Authorization': 'Bearer token' }
}
```

**spec.output.handler** (for custom destination)
```javascript
async (data) => {
  // Custom processing logic
  await processEvent(data);
  await notifyStakeholders(data);
}
```

#### Baseline Specifications

**spec.baseline.store**
- Type: `string`
- Description: Path to baseline data file
- Example: `'baseline-configs.ttl'`

**spec.baseline.key**
- Type: `string`
- Description: Variable used as key for baseline comparisons
- Example: `'configHash'`

#### Return Value

Returns an immutable hook object with the following structure:

```javascript
{
  id: 'ex:ServiceHealthMonitor',
  name: 'Critical Service Health Monitor',
  description: 'Monitors service error rates and latency',
  select: 'SELECT ?service ?errorRate WHERE { ?service ex:errorRate ?errorRate }',
  predicates: [...],
  combine: 'OR',
  output: {...},
  hash: 'sha256-hash-of-hook-definition'
}
```

### evaluateHook

Evaluates a hook against the current RDF graph and returns a cryptographically signed receipt.

```javascript
/**
 * Evaluate a Knowledge Hook
 * @param {Hook} hook - Hook to evaluate
 * @param {Object} [options] - Evaluation options
 * @param {boolean} [options.persist=false] - Persist baseline data after evaluation
 * @param {boolean} [options.debug=false] - Enable debug mode
 * @param {number} [options.timeout=5000] - Evaluation timeout in milliseconds
 * @param {boolean} [options.verify=true] - Verify cryptographic signatures
 * @returns {Promise<HookReceipt>} Cryptographically signed evaluation receipt
 */
async function evaluateHook(hook, options = {})
```

#### Parameters

**hook** (required)
- Type: `Hook`
- Description: Hook object returned by `defineHook`

**options.persist** (optional)
- Type: `boolean`
- Default: `false`
- Description: Whether to persist baseline data for future delta comparisons

**options.debug** (optional)
- Type: `boolean`
- Default: `false`
- Description: Enable detailed logging and debugging information

**options.timeout** (optional)
- Type: `number`
- Default: `5000`
- Description: Maximum time to wait for evaluation (milliseconds)

**options.verify** (optional)
- Type: `boolean`
- Default: `true`
- Description: Whether to verify cryptographic signatures in receipts

#### Return Value

Returns a Promise that resolves to a `HookReceipt` object:

```javascript
interface HookReceipt {
  // Hook identification
  id: string;
  fired: boolean;

  // Evaluation results
  predicates: Array<{
    kind: string;
    ok: boolean;
    meta?: Record<string, any>;
    duration: number;
  }>;

  // Performance metrics
  durations: {
    totalMs: number;
    queryMs: number;
    predicateMs: number;
    canonicalizationMs: number;
  };

  // Cryptographic provenance
  provenance: {
    hookHash: string;        // SHA-256 of hook definition
    queryHash: string;       // SHA-256 of SPARQL query
    graphHash: string;       // SHA-256 of evaluated graph
    baselineHash: string;    // SHA-256 of baseline data
    receiptHash: string;     // SHA-256 of this receipt
  };

  // Metadata
  at: string;                // ISO timestamp
  input: {
    bindings: number;        // Number of query results
    variables: string[];     // Query variables
  };

  // Error information (if any)
  error?: string;
  errorDetails?: any;
}
```

### initStore

Initializes the RDF store context for hook evaluation.

```javascript
/**
 * Initialize the RDF store context
 * @param {Array} [quads=[]] - Initial RDF quads to load
 * @param {Object} [options={}] - Context options
 * @param {string} [options.baseIRI] - Base IRI for the context
 * @param {boolean} [options.validation=true] - Enable validation
 * @param {boolean} [options.performance=false] - Enable performance profiling
 * @returns {Function} Context function to run hook operations
 */
function initStore(quads = [], options = {})
```

#### Parameters

**quads** (optional)
- Type: `Array<Quad>`
- Default: `[]`
- Description: Initial RDF quads to load into the store

**options.baseIRI** (optional)
- Type: `string`
- Description: Base IRI for resolving relative IRIs
- Example: `'https://production.example.org/'`

**options.validation** (optional)
- Type: `boolean`
- Default: `true`
- Description: Enable runtime validation of operations

**options.performance** (optional)
- Type: `boolean`
- Default: `false`
- Description: Enable performance profiling and metrics

#### Return Value

Returns a context function that provides the RDF environment for hook operations:

```javascript
const runApp = initStore(quads, { baseIRI: 'https://example.org/' });

// Use the context to run hook operations
runApp(async () => {
  const hook = defineHook({...});
  const receipt = await evaluateHook(hook);
});
```

## Hook Management Functions

### planHook

Analyzes a hook and returns its execution plan without evaluation.

```javascript
/**
 * Plan hook execution without evaluation
 * @param {Hook} hook - Hook to plan
 * @returns {HookPlan} Execution plan
 */
function planHook(hook)
```

#### Return Value

```javascript
interface HookPlan {
  queryPlan: {
    query: string;
    variables: string[];
    estimatedComplexity: 'low' | 'medium' | 'high';
  };
  predicatePlan: Array<{
    kind: string;
    complexity: 'low' | 'medium' | 'high';
    dependencies: string[];
  }>;
  combine: string;
  estimatedDuration: number;
}
```

### loadFrontmatterHook

Loads a hook definition from a Markdown file with YAML frontmatter.

```javascript
/**
 * Load hook from Markdown frontmatter
 * @param {string} filePath - Path to Markdown file
 * @returns {Promise<Hook>} Hook definition
 */
async function loadFrontmatterHook(filePath)
```

#### Example Markdown File

```markdown
---
hook:
  id: 'ex:ServiceHealth'
  name: 'Service Health Monitor'
  description: 'Monitors service error rates'
  select: 'SELECT ?service ?errorRate WHERE { ?service ex:errorRate ?errorRate }'
  predicates:
    - kind: 'THRESHOLD'
      spec:
        var: 'errorRate'
        op: '>'
        value: 0.02
  combine: 'OR'
---

# Service Health Hook

This hook monitors service health metrics and alerts when error rates exceed 2%.
```

### registerPredicateType

Registers a custom predicate type.

```javascript
/**
 * Register a custom predicate type
 * @param {string} kind - Predicate type name
 * @param {Function} evaluator - Evaluation function
 * @param {Function} planner - Planning function (optional)
 */
function registerPredicateType(kind, evaluator, planner)
```

#### Evaluator Function

```javascript
async function customEvaluator(spec, bindings, context) {
  // spec: predicate specification
  // bindings: SPARQL query results
  // context: evaluation context
  return {
    ok: boolean,
    meta: object,
    duration: number
  };
}
```

#### Planner Function

```javascript
function customPlanner(spec) {
  return {
    complexity: 'medium',
    dependencies: ['custom-dependency']
  };
}
```

## Error Types

### HookDefinitionError

Thrown when hook definition is invalid.

```javascript
class HookDefinitionError extends Error {
  constructor(message, details = {}) {
    super(message);
    this.name = 'HookDefinitionError';
    this.details = details;
  }
}
```

### HookEvaluationError

Thrown when hook evaluation fails.

```javascript
class HookEvaluationError extends Error {
  constructor(message, receipt = null) {
    super(message);
    this.name = 'HookEvaluationError';
    this.receipt = receipt;
  }
}
```

### HookTimeoutError

Thrown when hook evaluation exceeds timeout.

```javascript
class HookTimeoutError extends HookEvaluationError {
  constructor(timeout, receipt = null) {
    super(`Hook evaluation timed out after ${timeout}ms`, receipt);
    this.name = 'HookTimeoutError';
    this.timeout = timeout;
  }
}
```

## Type Definitions

```javascript
/**
 * @typedef {Object} Hook
 * @property {string} id - Hook identifier
 * @property {string} name - Human-readable name
 * @property {string} description - Hook description
 * @property {string} select - SPARQL query
 * @property {Array} predicates - Predicate specifications
 * @property {string} combine - Combination logic
 * @property {Object} output - Output configuration
 * @property {string} hash - SHA-256 hash of hook definition
 */

/**
 * @typedef {Object} HookReceipt
 * @property {string} id - Hook identifier
 * @property {boolean} fired - Whether hook fired
 * @property {Array} predicates - Predicate evaluation results
 * @property {Object} durations - Performance metrics
 * @property {Object} provenance - Cryptographic hashes
 * @property {string} at - ISO timestamp
 * @property {Object} input - Input statistics
 */

/**
 * @typedef {Object} HookPlan
 * @property {Object} queryPlan - Query execution plan
 * @property {Array} predicatePlan - Predicate evaluation plan
 * @property {string} combine - Combination logic
 * @property {number} estimatedDuration - Estimated duration in ms
 */
```

## Best Practices

### Hook Design

1. **Single Responsibility**: Each hook should monitor one logical condition
2. **Clear Identifiers**: Use descriptive IRIs for hook IDs
3. **Efficient Queries**: Optimize SPARQL queries for performance
4. **Appropriate Predicates**: Choose the most efficient predicate type

### Error Handling

```javascript
try {
  const receipt = await evaluateHook(hook, { timeout: 5000 });

  if (receipt.fired) {
    // Handle hook firing
    await processAlert(receipt);
  }
} catch (error) {
  if (error instanceof HookTimeoutError) {
    console.error('Hook evaluation timed out');
  } else if (error instanceof HookEvaluationError) {
    console.error('Hook evaluation failed:', error.receipt);
  } else {
    console.error('Unexpected error:', error);
  }
}
```

### Performance Optimization

1. **Query Optimization**: Use specific variable names and LIMIT clauses
2. **Predicate Selection**: Choose efficient predicate types (ASK is fastest)
3. **Baseline Management**: Use appropriate baseline strategies for delta detection
4. **Timeout Configuration**: Set appropriate timeouts for your use case

### Security

1. **Input Validation**: Validate all hook configurations
2. **Secure Webhooks**: Use HTTPS and proper authentication
3. **Signature Verification**: Always verify cryptographic signatures
4. **Access Control**: Implement proper authorization for hook management

## Examples

### Basic Service Monitoring

```javascript
const healthHook = defineHook({
  id: 'ex:ServiceHealthMonitor',
  name: 'Service Health Monitor',
  description: 'Monitors error rates for critical services',
  select: 'SELECT ?service ?errorRate WHERE { ?service ex:errorRate ?errorRate }',
  predicates: [
    { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.02 } }
  ],
  combine: 'OR'
});

const receipt = await evaluateHook(healthHook, { persist: true });
```

### Complex Compliance Hook

```javascript
const complianceHook = defineHook({
  id: 'ex:GDPRCompliance',
  name: 'GDPR Compliance Monitor',
  description: 'Ensures GDPR compliance for sensitive data',
  select: 'SELECT ?resource WHERE { ?resource ex:sensitive true }',
  predicates: [
    { kind: 'SHACL', spec: { shapes: 'ex:GDPRShape', strict: true } },
    { kind: 'ASK', spec: { query: 'ASK WHERE { ?resource ex:consentGiven false }', expected: false } }
  ],
  combine: 'AND'
});
```

### Configuration Drift Detection

```javascript
const driftHook = defineHook({
  id: 'ex:ConfigDrift',
  name: 'Configuration Drift Detector',
  description: 'Detects unauthorized configuration changes',
  select: 'SELECT ?config ?value WHERE { ?config ex:currentValue ?value }',
  predicates: [
    {
      kind: 'DELTA',
      spec: {
        change: 'any',
        key: ['config'],
        threshold: 0.01
      }
    }
  ],
  baseline: {
    store: 'approved-configs.ttl',
    key: 'configHash'
  }
});
```
