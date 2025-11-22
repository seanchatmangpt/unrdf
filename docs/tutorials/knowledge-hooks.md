# Tutorial: Knowledge Hooks

Learn how to create reactive, auditable knowledge graph triggers using UNRDF's Knowledge Hooks system.

## Learning Objectives

By the end of this tutorial, you will:

- Understand the Knowledge Hooks architecture
- Define hooks with declarative triggers
- Implement the before/run/after lifecycle
- Configure cryptographic receipts for audit trails
- Apply hooks to real-world use cases

## Prerequisites

- Completed [Creating RDF Documents](./creating-rdf-documents.md) tutorial
- Understanding of SPARQL ASK queries
- Familiarity with async/await patterns

## What are Knowledge Hooks?

Knowledge Hooks transform static knowledge graphs into **reactive systems**. They:

1. **Monitor** - Watch for changes matching SPARQL/SHACL conditions
2. **React** - Execute logic when conditions are met
3. **Audit** - Create cryptographic receipts for provenance

Traditional RDF applications require custom event systems. Knowledge Hooks provide a **declarative, auditable** alternative.

## Step 1: Your First Hook

```javascript
// src/first-hook.mjs
import { defineHook } from 'unrdf/knowledge-engine';

const myFirstHook = defineHook({
  meta: {
    name: 'tutorial:first-hook',
    description: 'A simple demonstration hook'
  },
  when: {
    kind: 'sparql-ask',
    ref: {
      uri: 'file://hooks/first-hook.rq',
      sha256: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
      mediaType: 'application/sparql-query'
    }
  },
  async run({ payload, context }) {
    console.log('Hook triggered!');
    console.log('Payload:', payload);
    return { result: { status: 'success' } };
  }
});

console.log('Hook defined:', myFirstHook.meta.name);
```

## Step 2: Understanding the Hook Contract

### The 80/20 Contract

UNRDF's hook contract is designed for 80% of use cases with minimal configuration:

```javascript
const hookContract = {
  // Required: Metadata for discovery
  meta: {
    name: 'namespace:hook-name',      // Unique identifier
    description: 'What this hook does',
    ontology: ['foaf', 'prov']        // Related ontologies
  },

  // Required: Trigger condition (content-addressed)
  when: {
    kind: 'sparql-ask' | 'sparql-select' | 'shacl',
    ref: {
      uri: 'file://path/to/query.rq',
      sha256: 'hash-of-file-contents',
      mediaType: 'application/sparql-query'
    }
  },

  // Optional: Graph observation scope
  channel: {
    graphs: ['urn:graph:production'],
    view: 'delta' | 'before' | 'after'
  },

  // Optional: Determinism configuration
  determinism: { seed: 42 },

  // Optional: Audit trail strategy
  receipt: { anchor: 'git-notes' | 'none' },

  // Optional: Pre-execution gate
  async before({ payload, context }) { },

  // Required: Main execution logic
  async run({ payload, context }) { },

  // Optional: Post-execution cleanup
  async after({ result, cancelled, reason }) { }
};
```

### Why Content-Addressed Conditions?

Conditions are **external files**, not inline strings. This ensures:

1. **Verifiability** - Query logic is a standalone artifact
2. **Auditability** - Changes to conditions are tracked
3. **Integrity** - SHA-256 hash prevents tampering
4. **Reusability** - Same query can be used by multiple hooks

## Step 3: The Lifecycle Functions

### before() - Pre-Execution Gate

Validates and transforms the payload before execution:

```javascript
async before({ payload, context }) {
  // Validation: Cancel if invalid
  if (!payload || !payload.transactionId) {
    return {
      cancel: true,
      reason: 'Missing required transactionId'
    };
  }

  // Transformation: Enrich payload
  return {
    ...payload,
    enrichedAt: new Date().toISOString(),
    sessionId: context?.env?.sessionId || 'default'
  };
}
```

### run() - Main Execution

Performs the core logic and returns results:

```javascript
import { DataFactory } from 'n3';
const { namedNode, literal, quad } = DataFactory;

async run({ payload, context }) {
  // Business logic
  const analysis = await analyzeData(payload);

  // Return result with optional assertions
  return {
    result: {
      status: analysis.passed ? 'passed' : 'failed',
      score: analysis.score
    },
    // Optional: Add triples to the graph
    assertions: [
      quad(
        namedNode(`urn:analysis:${payload.id}`),
        namedNode('urn:analysis:score'),
        literal(analysis.score.toString())
      )
    ]
  };
}
```

### after() - Post-Execution Cleanup

Handles cleanup, logging, and finalization:

```javascript
async after({ result, cancelled, reason }) {
  if (cancelled) {
    console.log(`Hook cancelled: ${reason}`);
    return { result: { finalStatus: 'cancelled', reason } };
  }

  console.log(`Hook completed: ${result?.result?.status}`);

  // Send notifications, update metrics, etc.
  await notifyCompletion(result);

  return {
    result: {
      finalStatus: 'completed',
      completedAt: new Date().toISOString()
    }
  };
}
```

## Step 4: Practical Examples

### Example 1: Compliance Monitoring

```javascript
// hooks/compliance-monitor.mjs
import { defineHook } from 'unrdf/knowledge-engine';
import { DataFactory } from 'n3';

const { namedNode, literal, quad } = DataFactory;

export const complianceMonitor = defineHook({
  meta: {
    name: 'compliance:transaction-monitor',
    description: 'Monitors financial transactions for compliance violations',
    ontology: ['fibo', 'prov', 'audit']
  },
  channel: {
    graphs: ['urn:graph:financial:transactions'],
    view: 'delta'  // Only check new/changed data
  },
  when: {
    kind: 'sparql-ask',
    ref: {
      uri: 'file://hooks/compliance/large-transaction.rq',
      sha256: 'a1b2c3d4e5f6789012345678901234567890abcdef1234567890abcdef123456',
      mediaType: 'application/sparql-query'
    }
  },
  receipt: { anchor: 'git-notes' },  // Create audit trail

  async before({ payload }) {
    if (!payload?.transactionId || !payload?.amount) {
      return { cancel: true, reason: 'Invalid transaction data' };
    }

    const threshold = payload.threshold || 10000;
    return { ...payload, threshold, checkedAt: new Date().toISOString() };
  },

  async run({ payload }) {
    const { transactionId, amount, threshold } = payload;
    const isLargeTransaction = amount > threshold;

    return {
      result: {
        transactionId,
        amount,
        isLargeTransaction,
        requiresReview: isLargeTransaction
      },
      assertions: isLargeTransaction ? [
        quad(
          namedNode(`urn:transaction:${transactionId}`),
          namedNode('urn:compliance:flaggedAt'),
          literal(payload.checkedAt)
        ),
        quad(
          namedNode(`urn:transaction:${transactionId}`),
          namedNode('urn:compliance:threshold'),
          literal(threshold.toString())
        )
      ] : []
    };
  },

  async after({ result, cancelled }) {
    if (cancelled) return { result: { status: 'cancelled' } };

    if (result?.result?.requiresReview) {
      console.log(`ALERT: Large transaction ${result.result.transactionId}`);
      // Send alert, create ticket, etc.
    }

    return { result: { status: 'completed' } };
  }
});
```

### Example 2: Data Quality Validation

```javascript
// hooks/data-quality.mjs
export const dataQualityHook = defineHook({
  meta: {
    name: 'quality:shacl-validation',
    description: 'Validates data quality using SHACL shapes'
  },
  channel: {
    graphs: ['urn:graph:production'],
    view: 'after'  // Validate final state
  },
  when: {
    kind: 'shacl',
    ref: {
      uri: 'file://shapes/data-quality.shacl.ttl',
      sha256: 'b2c3d4e5f6789012345678901234567890abcdef1234567890abcdef1234567',
      mediaType: 'text/turtle'
    }
  },

  async before({ payload, context }) {
    if (!context?.graph) {
      return { cancel: true, reason: 'No graph available for validation' };
    }
    return { ...payload, startedAt: Date.now() };
  },

  async run({ payload, context }) {
    // SHACL validation happens automatically based on 'when' config
    // This run function handles the results

    const validationReport = context.validationReport;
    const conforms = validationReport?.conforms ?? true;
    const violationCount = validationReport?.results?.length ?? 0;

    return {
      result: {
        status: conforms ? 'valid' : 'invalid',
        conforms,
        violationCount,
        duration: Date.now() - payload.startedAt
      }
    };
  },

  async after({ result }) {
    const { status, violationCount, duration } = result?.result || {};
    console.log(`Validation: ${status} (${violationCount} violations, ${duration}ms)`);
    return { result: { finalStatus: status } };
  }
});
```

### Example 3: Service Health Monitor

```javascript
// hooks/service-health.mjs
export const serviceHealthHook = defineHook({
  meta: {
    name: 'ops:service-health',
    description: 'Monitors service health metrics and triggers alerts'
  },
  when: {
    kind: 'sparql-select',
    ref: {
      uri: 'file://hooks/ops/unhealthy-services.rq',
      sha256: 'c3d4e5f6789012345678901234567890abcdef1234567890abcdef12345678',
      mediaType: 'application/sparql-query'
    }
  },
  determinism: { seed: 42 },  // Reproducible random operations

  async run({ payload, context }) {
    // context.queryResults contains SPARQL SELECT results
    const unhealthyServices = context.queryResults || [];

    if (unhealthyServices.length === 0) {
      return { result: { status: 'healthy', services: [] } };
    }

    const alerts = unhealthyServices.map(service => ({
      serviceId: service.service.value,
      errorRate: parseFloat(service.errorRate.value),
      lastCheck: new Date().toISOString()
    }));

    return {
      result: {
        status: 'unhealthy',
        services: alerts,
        alertCount: alerts.length
      }
    };
  },

  async after({ result }) {
    if (result?.result?.status === 'unhealthy') {
      // Send PagerDuty alert, Slack notification, etc.
      console.log(`ALERT: ${result.result.alertCount} unhealthy services`);
    }
    return { result: { processed: true } };
  }
});
```

## Step 5: Creating SPARQL Condition Files

Create the query files referenced by hooks:

### hooks/compliance/large-transaction.rq

```sparql
PREFIX fibo: <https://spec.edmcouncil.org/fibo/ontology/FND/>
PREFIX ex: <http://example.org/>

ASK {
  ?transaction a fibo:Transaction ;
               ex:amount ?amount .
  FILTER (?amount > 10000)
}
```

### hooks/ops/unhealthy-services.rq

```sparql
PREFIX ex: <http://example.org/>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT ?service ?errorRate ?lastCheck
WHERE {
  ?service a ex:Service ;
           ex:errorRate ?errorRate ;
           ex:lastHealthCheck ?lastCheck .
  FILTER (?errorRate > 0.05)
}
ORDER BY DESC(?errorRate)
```

## Step 6: Computing SHA-256 Hashes

Hook conditions require SHA-256 hashes for integrity:

```javascript
// tools/compute-hash.mjs
import { createHash } from 'node:crypto';
import { readFileSync } from 'node:fs';

function computeSHA256(filePath) {
  const content = readFileSync(filePath, 'utf-8');
  return createHash('sha256').update(content).digest('hex');
}

// Usage
const hash = computeSHA256('./hooks/compliance/large-transaction.rq');
console.log(`SHA-256: ${hash}`);
```

Or use the CLI:

```bash
shasum -a 256 hooks/compliance/large-transaction.rq
```

## Step 7: Receipt Anchoring

Receipts create cryptographic audit trails:

```javascript
// git-notes anchoring stores receipts in git
const hookWithReceipt = defineHook({
  meta: { name: 'audit:important-action' },
  receipt: { anchor: 'git-notes' },
  // ... rest of config
});

// Receipt structure
{
  hookId: 'audit:important-action',
  executedAt: '2024-01-15T10:30:00.000Z',
  inputHash: 'sha256:abc123...',
  outputHash: 'sha256:def456...',
  signature: 'ed25519:...'
}
```

## Exercise: Build a Policy Engine

Create a policy engine that enforces business rules:

```javascript
// exercises/policy-engine.mjs
import { defineHook } from 'unrdf/knowledge-engine';

// Policy 1: No duplicate customers
export const noDuplicatesPolicy = defineHook({
  meta: {
    name: 'policy:no-duplicate-customers',
    description: 'Prevents duplicate customer records'
  },
  when: {
    kind: 'sparql-ask',
    ref: {
      uri: 'file://policies/duplicate-check.rq',
      sha256: 'your-hash-here',
      mediaType: 'application/sparql-query'
    }
  },
  async run({ payload, context }) {
    const hasDuplicates = context.askResult;
    return {
      result: {
        passed: !hasDuplicates,
        policy: 'no-duplicates',
        action: hasDuplicates ? 'BLOCK' : 'ALLOW'
      }
    };
  }
});

// Policy 2: Minimum data requirements
export const minimumDataPolicy = defineHook({
  meta: {
    name: 'policy:minimum-customer-data',
    description: 'Ensures customers have required fields'
  },
  when: {
    kind: 'shacl',
    ref: {
      uri: 'file://policies/customer-shape.shacl.ttl',
      sha256: 'your-hash-here',
      mediaType: 'text/turtle'
    }
  },
  async run({ payload, context }) {
    return {
      result: {
        passed: context.validationReport?.conforms ?? false,
        policy: 'minimum-data',
        violations: context.validationReport?.results?.length ?? 0
      }
    };
  }
});

// Policy Engine
class PolicyEngine {
  constructor(policies) {
    this.policies = policies;
  }

  async evaluate(data) {
    const results = [];

    for (const policy of this.policies) {
      const result = await this.runPolicy(policy, data);
      results.push(result);

      if (result.action === 'BLOCK') {
        return { allowed: false, results, blockedBy: policy.meta.name };
      }
    }

    return { allowed: true, results };
  }

  async runPolicy(policy, data) {
    // Simplified - actual implementation would evaluate conditions
    const hookResult = await policy.run({
      payload: data,
      context: { /* mock context */ }
    });
    return hookResult.result;
  }
}

// Usage
const engine = new PolicyEngine([
  noDuplicatesPolicy,
  minimumDataPolicy
]);

const decision = await engine.evaluate({ customerId: 'C123', name: 'Alice' });
console.log('Policy decision:', decision);
```

## Common Mistakes

### Mistake 1: Inline Queries

```javascript
// Wrong - inline SPARQL not allowed
when: {
  kind: 'sparql-ask',
  query: 'ASK { ?s ?p ?o }'  // This will fail validation
}

// Correct - use content-addressed reference
when: {
  kind: 'sparql-ask',
  ref: {
    uri: 'file://hooks/my-query.rq',
    sha256: '...',
    mediaType: 'application/sparql-query'
  }
}
```

### Mistake 2: Missing Await in Lifecycle

```javascript
// Wrong - not awaiting async operations
async run({ payload }) {
  const data = fetchData(payload);  // Missing await!
  return { result: data };
}

// Correct
async run({ payload }) {
  const data = await fetchData(payload);
  return { result: data };
}
```

### Mistake 3: Modifying Payload Directly

```javascript
// Wrong - mutating input
async before({ payload }) {
  payload.timestamp = Date.now();  // Mutation!
  return payload;
}

// Correct - return new object
async before({ payload }) {
  return {
    ...payload,
    timestamp: Date.now()
  };
}
```

## Summary

You learned how to:

- Define Knowledge Hooks with the 80/20 contract
- Implement before/run/after lifecycle functions
- Create content-addressed SPARQL conditions
- Configure audit trail receipts
- Build practical hooks for compliance, quality, and monitoring

## Next Steps

- [SPARQL Tutorial](./sparql.md) - Advanced query patterns
- [Validation Guide](../guides/validation-rules.md) - SHACL shapes
- [Architecture Explanation](../explanation/knowledge-hooks-architecture.md) - Deep dive
