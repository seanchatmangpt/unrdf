# 🎯 Knowledge Hooks Guide

**The Complete Guide to unrdf's Knowledge Hooks - Enterprise-Grade Reactive Triggers for RDF Graphs**

## Overview

Knowledge Hooks are the crown jewel of unrdf - they transform static knowledge graphs into intelligent, reactive systems. Unlike traditional RDF applications that require external event systems or polling mechanisms, Knowledge Hooks provide a **unified, declarative approach** to RDF reactivity with built-in cryptographic provenance.

## What Are Knowledge Hooks?

Knowledge Hooks are pure functions that monitor RDF graphs for specific conditions and emit cryptographically signed receipts when those conditions are met. They eliminate the need for custom event systems while providing enterprise-grade audit trails.

```javascript
import { initStore, defineHook, evaluateHook } from 'unrdf';

// Define a hook to monitor service health
const healthHook = defineHook({
  id: 'ex:ServiceHealthMonitor',
  name: 'Critical Service Health Monitor',
  description: 'Monitors service error rates and latency',
  select: 'SELECT ?service ?errorRate ?latency WHERE { ?service ex:errorRate ?errorRate ; ex:latency ?latency }',
  predicates: [
    { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.02 } },
    { kind: 'THRESHOLD', spec: { var: 'latency', op: '>', value: 2000 } }
  ],
  combine: 'OR',
  output: {
    schema: z.object({ service: z.string(), alert: z.string() }),
    format: 'json',
    destination: 'webhook'
  }
});

// Evaluate with cryptographic receipt
const receipt = await evaluateHook(healthHook, { persist: true });

if (receipt.fired) {
  console.log('🚨 Service health issue detected!');
  console.log('Evidence:', receipt.predicates);
  console.log('Provenance:', receipt.provenance);
}
```

## Core Concepts

### 1. Hook Definition

A hook is defined by several key components:

- **`id`**: Unique identifier for the hook (IRI)
- **`select`**: SPARQL query that defines what data to monitor
- **`predicates`**: Array of conditions to evaluate against query results
- **`combine`**: Logic to combine multiple predicates (AND/OR/NOT)
- **`output`** (optional): How to format and emit results

### 2. Predicate Types

Knowledge Hooks support multiple predicate types for different monitoring scenarios:

#### ASK Predicates
Boolean queries that check for existence or absence of conditions.

```javascript
{
  kind: 'ASK',
  spec: {
    query: 'ASK WHERE { ?service ex:critical true }',
    expected: true  // Optional: invert the result
  }
}
```

#### THRESHOLD Predicates
Numeric comparisons against query result variables.

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

#### DELTA Predicates
Detect changes between evaluations using stable hashes.

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

#### SHACL Predicates
Validate against SHACL shapes.

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

#### COUNT Predicates
Check the cardinality of query results.

```javascript
{
  kind: 'COUNT',
  spec: {
    op: '>',                // Operator: >, >=, <, <=, ==, !=
    value: 10              // Expected count
  }
}
```

#### WINDOW Predicates
Time-based aggregations with comparisons.

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

### 3. Cryptographic Receipts

Every hook evaluation returns a cryptographically signed receipt:

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
}
```

## Advanced Features

### 1. Predicate Combinators

Combine multiple predicates with logical operators:

```javascript
const complexHook = defineHook({
  id: 'ex:ComplexMonitor',
  select: 'SELECT ?service ?errorRate ?latency WHERE { ?service ex:errorRate ?errorRate ; ex:latency ?latency }',
  predicates: [
    { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.05 } },
    { kind: 'THRESHOLD', spec: { var: 'latency', op: '>', value: 2000 } },
    { kind: 'ASK', spec: { query: 'ASK WHERE { ?service ex:critical true }' } }
  ],
  combine: 'OR',  // Fire if ANY predicate matches
  // combine: 'AND', // Fire only if ALL predicates match
  // combine: 'NOT'  // Fire only if predicates do NOT match
});
```

### 2. Baseline Comparisons

Track changes over time with baseline data:

```javascript
const driftHook = defineHook({
  id: 'ex:ConfigurationDrift',
  select: 'SELECT ?config ?value WHERE { ?config ex:currentValue ?value }',
  predicates: [
    {
      kind: 'DELTA',
      spec: {
        change: 'any',
        key: ['config'],
        baseline: {
          store: 'baseline-configs.ttl',
          key: 'configHash'
        }
      }
    }
  ]
});
```

### 3. Output Formatting and Delivery

Configure how hook results are formatted and delivered:

```javascript
const webhookHook = defineHook({
  id: 'ex:AlertWebhook',
  select: 'SELECT ?service ?metric ?value WHERE { ?service ex:hasMetric ?metric . ?metric ex:value ?value }',
  predicates: [
    { kind: 'THRESHOLD', spec: { var: 'value', op: '>', value: 95 } }
  ],
  output: {
    schema: z.object({
      service: z.string(),
      alert: z.string(),
      timestamp: z.string(),
      severity: z.enum(['warning', 'critical'])
    }),
    format: 'jsonld',        // 'json', 'turtle', 'jsonld'
    destination: 'webhook',  // 'webhook', 'console', 'file', 'custom'
    webhook: {
      url: 'https://api.example.com/alerts',
      method: 'POST',
      headers: { 'Authorization': 'Bearer token' }
    }
  }
});
```

## Enterprise Use Cases

### 1. Service Health Monitoring

Monitor critical infrastructure metrics:

```javascript
const serviceHealthHook = defineHook({
  id: 'ex:ServiceHealthMonitor',
  name: 'Critical Service Health Monitor',
  description: 'Detects service degradation and sudden error spikes',
  select: 'SELECT ?service ?errorRate ?responseTime WHERE { ?service ex:errorRate ?errorRate ; ex:responseTime ?responseTime }',
  predicates: [
    { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.05 } },
    { kind: 'THRESHOLD', spec: { var: 'responseTime', op: '>', value: 2000 } },
    { kind: 'DELTA', spec: { change: 'increase', key: ['service'], threshold: 0.1 } }
  ],
  combine: 'OR',
  output: {
    schema: z.object({
      service: z.string(),
      alert: z.string(),
      severity: z.enum(['warning', 'critical']),
      metrics: z.object({
        errorRate: z.number(),
        responseTime: z.number()
      })
    }),
    format: 'json',
    destination: 'webhook'
  }
});
```

### 2. Compliance Validation

Ensure all sensitive data processing complies with regulations:

```javascript
const complianceHook = defineHook({
  id: 'ex:GDPRComplianceGate',
  name: 'GDPR Data Compliance Gate',
  description: 'Ensures all sensitive data processing complies with GDPR',
  select: 'SELECT ?resource WHERE { ?resource ex:sensitive true }',
  predicates: [
    { kind: 'SHACL', spec: { shapes: 'ex:GDPRShape', strict: true } },
    { kind: 'ASK', spec: { query: 'ASK WHERE { ?resource ex:consentGiven false }', expected: false } }
  ],
  combine: 'AND',
  output: {
    schema: z.object({
      resource: z.string(),
      violations: z.array(z.string()),
      severity: z.literal('critical')
    }),
    destination: 'console'
  }
});
```

### 3. Configuration Drift Detection

Detect unauthorized changes to critical infrastructure:

```javascript
const configDriftHook = defineHook({
  id: 'ex:InfrastructureDrift',
  name: 'Infrastructure Configuration Drift',
  description: 'Detects unauthorized changes to critical infrastructure',
  select: 'SELECT ?config ?value ?environment WHERE { ?config ex:currentValue ?value ; ex:environment ?environment }',
  predicates: [
    { kind: 'DELTA', spec: { change: 'any', key: ['config', 'environment'] } },
    { kind: 'ASK', spec: { query: 'ASK WHERE { ?config ex:approved false }', expected: false } }
  ],
  combine: 'AND',
  baseline: {
    store: 'approved-configs.ttl',
    key: 'configHash'
  },
  output: {
    schema: z.object({
      config: z.string(),
      environment: z.string(),
      change: z.object({
        type: z.enum(['added', 'removed', 'modified']),
        magnitude: z.number()
      }),
      approved: z.boolean()
    }),
    destination: 'webhook'
  }
});
```

## Performance Considerations

### 1. Query Optimization

- Use specific variable names in SELECT clauses
- Include only necessary variables in the projection
- Use LIMIT when appropriate
- Consider using COUNT predicates instead of full result set analysis

```javascript
// Good: Specific variables, limited results
select: 'SELECT ?service ?errorRate WHERE { ?service ex:errorRate ?errorRate } LIMIT 100'

// Avoid: Too many variables, no limits
select: 'SELECT * WHERE { ?s ?p ?o }'
```

### 2. Predicate Efficiency

- Use ASK predicates for existence checks (fastest)
- Use COUNT predicates instead of full result set processing
- Combine multiple threshold checks in a single query when possible
- Use SHACL predicates for complex validation rules

### 3. Evaluation Frequency

- Consider the cost of hook evaluation
- Use appropriate polling intervals for non-real-time use cases
- Implement caching for expensive operations
- Monitor hook performance through receipt durations

## Best Practices

### 1. Hook Design

- **Single Responsibility**: Each hook should monitor one logical condition
- **Clear Naming**: Use descriptive IDs, names, and descriptions
- **Minimal Queries**: Include only necessary data in SELECT clauses
- **Appropriate Predicates**: Choose the most efficient predicate type for your use case

### 2. Error Handling

- Always handle hook evaluation errors gracefully
- Log receipt information for debugging and audit trails
- Implement fallback mechanisms for critical monitoring
- Monitor hook performance and adjust thresholds as needed

### 3. Security

- Use HTTPS for webhook endpoints
- Implement proper authentication for hook management
- Validate all input data before processing
- Keep cryptographic keys secure and rotate regularly

### 4. Monitoring and Maintenance

- Regularly review hook performance metrics
- Update thresholds based on baseline data
- Archive old receipts according to retention policies
- Monitor for hook failures and implement alerting

## Integration Patterns

### 1. Webhook Integration

```javascript
const webhookHook = defineHook({
  id: 'ex:WebhookAlert',
  select: 'SELECT ?alert ?level WHERE { ?alert ex:level ?level }',
  predicates: [
    { kind: 'THRESHOLD', spec: { var: 'level', op: '>=', value: 'warning' } }
  ],
  output: {
    schema: z.object({
      alert: z.string(),
      level: z.string(),
      timestamp: z.string()
    }),
    format: 'json',
    destination: 'webhook',
    webhook: {
      url: process.env.WEBHOOK_URL,
      method: 'POST',
      headers: {
        'Authorization': `Bearer ${process.env.API_TOKEN}`,
        'Content-Type': 'application/json'
      }
    }
  }
});
```

### 2. Database Persistence

```javascript
const persistentHook = defineHook({
  id: 'ex:PersistentMonitor',
  select: 'SELECT ?metric ?value WHERE { ?metric ex:value ?value }',
  predicates: [
    { kind: 'THRESHOLD', spec: { var: 'value', op: '>', value: 100 } }
  ],
  output: {
    schema: z.object({
      metric: z.string(),
      value: z.number(),
      timestamp: z.string(),
      fired: z.boolean()
    }),
    format: 'json',
    destination: 'database',
    database: {
      type: 'postgresql',
      connection: process.env.DATABASE_URL,
      table: 'hook_results'
    }
  }
});
```

### 3. Custom Output Handlers

```javascript
const customHook = defineHook({
  id: 'ex:CustomOutput',
  select: 'SELECT ?event ?data WHERE { ?event ex:data ?data }',
  predicates: [
    { kind: 'ASK', spec: { query: 'ASK WHERE { ?event ex:important true }' } }
  ],
  output: {
    schema: z.object({
      event: z.string(),
      data: z.any(),
      processed: z.boolean()
    }),
    format: 'json',
    destination: 'custom',
    handler: async (data) => {
      // Custom processing logic
      await processEvent(data);
      await notifyStakeholders(data);
      await updateDashboard(data);
    }
  }
});
```

## Troubleshooting

### Common Issues

#### Hook Not Firing
1. Check SPARQL query syntax and results
2. Verify predicate logic and thresholds
3. Ensure data is loaded into the correct graph
4. Check baseline data for DELTA predicates

#### Performance Issues
1. Optimize SPARQL queries
2. Use more efficient predicate types
3. Reduce query result set size with LIMIT
4. Consider caching strategies

#### Receipt Validation Failures
1. Verify cryptographic signatures
2. Check provenance hashes
3. Ensure consistent data canonicalization
4. Validate receipt schema

### Debugging Tools

```javascript
// Use planHook to debug execution plans
import { planHook } from 'unrdf';

const plan = planHook(hook);
console.log('Query Plan:', plan.queryPlan);
console.log('Predicate Plan:', plan.predicatePlan);

// Enable debug mode for detailed logging
const receipt = await evaluateHook(hook, { debug: true });

// Check receipt for detailed error information
if (receipt.error) {
  console.error('Hook Error:', receipt.error);
  console.error('Predicate Errors:', receipt.predicates.filter(p => !p.ok));
}
```

## Conclusion

Knowledge Hooks represent a paradigm shift in RDF application development - from static data processing to reactive, intelligent systems. By providing declarative triggers with cryptographic provenance, they eliminate the need for complex event systems while ensuring enterprise-grade auditability.

The combination of SPARQL's expressive query power with built-in reactivity makes Knowledge Hooks uniquely suited for modern, data-driven applications that require both flexibility and accountability.

For more examples and advanced patterns, see the [Knowledge Hooks Examples](../examples/knowledge-hooks/) directory.
