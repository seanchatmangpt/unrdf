# Daemon Knowledge Engine Rules Guide

## Overview

The Daemon Knowledge Engine provides an intelligent rule system for autonomous operation triggering. Rules combine SPARQL pattern matching, business logic evaluation, and inference chains to enable sophisticated decision-making in background operations.

**Key Concepts**:
- **Rules**: Declarative definitions combining conditions and actions
- **Patterns**: SPARQL queries and business logic functions for condition evaluation
- **Confidence**: Probabilistic scoring of rule matches (0-1 scale)
- **Inference Chains**: Dependency-based execution of related rules
- **A/B Testing**: Built-in variant selection for experimentation
- **Explanations**: Human-readable descriptions of why rules fired

---

## Quick Start

### Creating Your First Rule

```javascript
import { DaemonRuleEngine } from '@unrdf/daemon/integrations/knowledge-rules';

// Initialize the engine
const engine = new DaemonRuleEngine(daemon, {
  enableInference: true,
  enableExplanations: true,
  confidenceThreshold: 0.5,
});

// Define a rule
const rule = {
  id: 'rule-001',
  name: 'Scale on High Load',
  description: 'Automatically scale replicas when load exceeds 80%',
  version: '1.0.0',

  // Condition: what triggers the rule
  condition: {
    type: 'business-logic',
    evaluator: (metadata) => metadata.load > 80,
    description: 'System load exceeds 80%',
  },

  // Action: what to do when rule fires
  action: {
    type: 'scale',
    payload: { replicas: 10 },
    priority: 'high',
  },

  // Minimum confidence needed to execute
  minConfidence: 0.75,
};

// Register and evaluate
engine.registerRule(rule);
const result = await engine.evaluateRules({ load: 85 });

if (result.matchedRules.length > 0) {
  console.log('Rule fired!', result.matchedRules[0].explanation);
}
```

---

## Rule Authoring

### Rule Structure

Every rule requires:

| Field | Type | Description |
|-------|------|-------------|
| `id` | UUID | Unique identifier |
| `name` | string | Human-readable name |
| `version` | semver | Version (e.g., "1.0.0") |
| `condition` | object | Trigger condition |
| `action` | object | Operation to execute |
| `minConfidence` | number | Minimum match confidence [0-1] |

### Optional Fields

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `description` | string | - | Rule purpose |
| `dependencies` | array | [] | Rule IDs this rule depends on |
| `metadata` | object | {} | Tags, author, A/B testing config |

---

## Condition Types

### 1. Business Logic Conditions

Execute arbitrary JavaScript logic for flexible decision-making:

```javascript
{
  type: 'business-logic',
  evaluator: (metadata) => {
    // Return true/false for match
    return metadata.errorRate > 0.1 && metadata.uptime > 3600000;
  },
  description: 'High error rate with sufficient uptime',
}
```

**When to use**: Complex logic, multiple fields, custom algorithms

### 2. SPARQL Pattern Matching

Query RDF graphs for semantic pattern matching:

```javascript
{
  type: 'sparql',
  query: `
    SELECT ?entity WHERE {
      ?entity rdf:type ?type ;
              rdf:value ?value .
      FILTER (?value > 100)
    }
  `,
  bindings: {
    '?type': 'HighValueEntity',
  },
  timeout: 5000,
}
```

**When to use**: RDF graph queries, ontology reasoning, semantic patterns

### 3. Composite Conditions

Combine multiple conditions with logical operators:

```javascript
{
  type: 'composite',
  operator: 'and',  // or 'or', 'not'
  conditions: [
    {
      type: 'business-logic',
      evaluator: (m) => m.load > 75,
      description: 'High load',
    },
    {
      type: 'business-logic',
      evaluator: (m) => m.memory > 85,
      description: 'High memory',
    },
  ],
}
```

**Operators**:
- `and`: All conditions must match
- `or`: At least one condition must match
- `not`: Condition must NOT match

---

## Confidence Scoring

Confidence measures how certain a rule match is, on a scale from 0 to 1:

- **0.95+ (VERY_HIGH)**: Near-certain match, safe to execute immediately
- **0.75-0.95 (HIGH)**: Strong confidence, suitable for most cases
- **0.5-0.75 (MEDIUM)**: Moderate confidence, consider with caution
- **0.25-0.5 (LOW)**: Weak match, validate before execution
- **0-0.25 (VERY_LOW)**: Minimal confidence, treat as unlikely

### How Confidence is Calculated

1. **Base Confidence**: From condition evaluation
   - Business logic: 1.0 (true) or 0.0 (false)
   - SPARQL: 0.9 (match) or 0.1 (no match)
   - Composite: Average of sub-conditions

2. **Dependency Boost**: +0.1 if all dependencies present in metadata
3. **Dependency Penalty**: -0.1 if dependencies missing

4. **Threshold Check**: Rule fires only if confidence ≥ minConfidence

### Setting Confidence Thresholds

```javascript
const rule = {
  // ... rule definition ...
  minConfidence: 0.75,  // Require high confidence
};

// Or configure globally
const engine = new DaemonRuleEngine(daemon, {
  confidenceThreshold: 0.5,  // Default for all rules
});
```

---

## Inference Chains

Inference chains enable cascading rule execution, where one rule's match triggers dependent rules:

### Defining Dependencies

```javascript
const rule1 = {
  id: 'rule-1',
  name: 'Detect Anomaly',
  condition: { /* ... */ },
  action: { type: 'alert', payload: {} },
  dependencies: [],  // Independent rule
};

const rule2 = {
  id: 'rule-2',
  name: 'Isolate Service',
  condition: { /* ... */ },
  action: { type: 'isolate', payload: {} },
  dependencies: ['rule-1'],  // Depends on rule-1
};

engine.registerRule(rule1);
engine.registerRule(rule2);

const result = await engine.evaluateRules(metadata);
// If rule-1 matches, rule-2 becomes eligible
// Result includes inference chains showing execution order
```

### Inference Chain Properties

- **Depth Limit**: Maximum 10 levels by default (configurable)
- **Cycle Detection**: Prevented via visited set tracking
- **Parallel Eligible**: Rules with same dependency level can execute in parallel
- **Ordered Execution**: Respects dependency order

---

## A/B Testing

Rules support built-in A/B testing for experimentation:

```javascript
const ruleVariant = {
  id: 'rule-ab-001',
  name: 'Smart Scaling',
  version: '2.0.0',
  condition: { /* ... */ },
  action: { type: 'scale', payload: { replicas: 5 } },
  metadata: {
    abTest: {
      enabled: true,
      variant: 'treatment',  // 'control' or 'treatment'
      splitPercentage: 50,   // 50/50 split
    },
  },
};

engine.registerRule(ruleVariant);

// Engine automatically selects variant for each evaluation
const result = await engine.evaluateRules(metadata);
// Check result.matchedRules[i].action for selected variant
```

### A/B Test Configuration

| Setting | Default | Range | Purpose |
|---------|---------|-------|---------|
| `enabled` | false | boolean | Enable/disable experiment |
| `variant` | - | 'control'\|'treatment' | Rule variant |
| `splitPercentage` | 50 | 0-100 | % of traffic for treatment |

---

## Explanations

Rules generate human-readable explanations for match results:

```javascript
const result = await engine.evaluateRules(metadata);

if (result.matchedRules.length > 0) {
  const match = result.matchedRules[0];

  console.log(match.explanation.reason);
  // Output: "Rule 'Scale on High Load' matched with 92.5% confidence"

  console.log(match.explanation.matchedPatterns);
  // Output: ["SELECT ?load WHERE..."]

  console.log(match.explanation.inferenceChain);
  // Output: ['rule-1', 'rule-2']  // Dependent rules that matched
}
```

### Explanation Components

- **Reason**: Why rule fired or failed
- **matchedPatterns**: Array of patterns that matched
- **failedConditions**: Conditions that blocked execution
- **inferenceChain**: Dependent rules in execution order

---

## SPARQL Pattern Syntax

SPARQL patterns enable semantic matching against RDF graphs:

### Basic Triple Pattern

```sparql
SELECT ?entity WHERE {
  ?entity rdf:type ?type .
}
```

### Filter Expressions

```sparql
SELECT ?value WHERE {
  ?entity rdf:value ?value .
  FILTER (?value > 100 && ?value < 500)
}
```

### Property Paths

```sparql
SELECT ?derived WHERE {
  ?entity rdfs:subClassOf+ ?derived .
}
```

### Common Patterns in Daemon Rules

**High Resource Usage**:
```sparql
SELECT ?entity WHERE {
  ?entity rdf:type Resource ;
          rdf:cpu ?cpu ;
          rdf:memory ?memory .
  FILTER (?cpu > 80 && ?memory > 85)
}
```

**Error Detection**:
```sparql
SELECT ?error WHERE {
  ?error rdf:type ErrorEvent ;
         dbo:count ?count .
  FILTER (?count > 10)
}
```

**Service Topology**:
```sparql
SELECT ?service WHERE {
  ?service rdf:type Service ;
           dbo:dependsOn+ ?dependency .
  FILTER NOT EXISTS { ?dependency rdf:health "healthy" }
}
```

---

## Best Practices

### 1. Clear Naming
Use descriptive rule names that explain the scenario:
```javascript
// Good
name: 'Scale Replicas When CPU Exceeds 80%'

// Avoid
name: 'Rule 1'
```

### 2. Appropriate Confidence Thresholds
Match thresholds to action consequences:
```javascript
// Critical action - require high confidence
{ action: { type: 'terminate', ... }, minConfidence: 0.9 }

// Advisory action - lower threshold acceptable
{ action: { type: 'alert', ... }, minConfidence: 0.5 }
```

### 3. Document Complex Logic
Use description field for business logic:
```javascript
{
  type: 'business-logic',
  evaluator: (m) => { /* complex calculation */ },
  description: 'Calculates adaptive scaling factor based on load trends'
}
```

### 4. Test Dependencies
Verify dependent rules exist before registering:
```javascript
for (const depId of rule.dependencies) {
  if (!engine.getRule(depId)) {
    throw new Error(`Dependency ${depId} not found`);
  }
}
```

### 5. Monitor for Conflicts
Detect conflicting rules before production:
```javascript
const conflicts = engine.detectConflicts();
if (conflicts.length > 0) {
  console.warn('Rule conflicts detected:', conflicts);
}
```

### 6. Version Your Rules
Increment version when changing behavior:
```javascript
// v1.0.0 - Initial release
// v1.0.1 - Bug fix in confidence calculation
// v1.1.0 - New feature: alert on errors
// v2.0.0 - Breaking change: different action payload
```

---

## Performance Considerations

### Evaluation Latency
- **1000 rules, 100 operations**: < 5 seconds
- **Average per evaluation**: < 5ms per rule
- **Inference chains**: < 1 second for 20-level deep chains

### Memory Usage
- **Rule registry**: ~1KB per rule
- **Execution history**: ~500 bytes per evaluation (keep last 100)
- **Cache**: Cleared periodically to prevent memory leaks

### Optimization Tips

1. **Limit rule count**: Use dependencies instead of individual rules
2. **Cache SPARQL results**: Pre-compute patterns in separate service
3. **Use business logic for simple checks**: Faster than SPARQL
4. **Batch evaluations**: Evaluate multiple operations together
5. **Monitor history size**: Clear old history periodically

---

## Advanced Topics

### Custom Evaluator Functions

```javascript
const rule = {
  // ...
  condition: {
    type: 'business-logic',
    evaluator: (metadata) => {
      // Access to daemon operations
      const uptime = metadata.uptime || 0;
      const errorRate = metadata.errorRate || 0;

      // Complex decision logic
      return errorRate > 0.05 && uptime > 600000;
    },
    description: 'Triggers after 10 minutes with >5% error rate',
  },
};
```

### Metadata Structure

Operations pass metadata to rules:

```javascript
{
  operationType: 'write',
  entityType: 'Document',
  size: 2048,
  load: 85,
  errorRate: 0.08,
  uptime: 3600000,
  timestamp: Date.now(),
  customField: 'custom-value',
  // ... any fields relevant to rules
}
```

### Rule Metrics

```javascript
const metrics = engine.getMetrics();
console.log(metrics);
// {
//   engineId: 'test-engine',
//   totalRules: 10,
//   totalEvaluations: 100,
//   matchedRules: 35,
//   failedRules: 2,
//   averageConfidence: 0.78,
//   successRate: 98,
//   executionHistorySize: 100,
// }
```

---

## Troubleshooting

### Rule Not Firing
1. Check confidence: Actual vs. minimum required
2. Verify metadata: All required fields present
3. Review explanation: Diagnostic info provided
4. Test condition separately: Evaluate in isolation

### High False Positive Rate
1. Increase `minConfidence` threshold
2. Add additional filter conditions
3. Refine SPARQL patterns
4. Add business logic guards

### Performance Issues
1. Check execution history size
2. Reduce rule count or simplify conditions
3. Profile with `engine.getMetrics()`
4. Monitor inference chain depth

---

## See Also

- [Daemon Package](../README.md)
- [Knowledge Engine API](../../knowledge-engine/docs)
- [SPARQL 1.1 Specification](https://www.w3.org/TR/sparql11-query/)
- [RDF Specification](https://www.w3.org/RDF/)
