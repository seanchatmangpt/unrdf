# How-To: Define Knowledge Hooks

Task-oriented guide for creating reactive Knowledge Hooks in UNRDF.

## Quick Reference

```javascript
import { defineHook } from 'unrdf/knowledge-engine';

const myHook = defineHook({
  meta: { name: 'namespace:hook-name' },
  when: {
    kind: 'sparql-ask',
    ref: { uri: 'file://query.rq', sha256: '...', mediaType: 'application/sparql-query' }
  },
  async run({ payload }) {
    return { result: { status: 'success' } };
  }
});
```

## How to Create a Basic Hook

```javascript
import { defineHook } from 'unrdf/knowledge-engine';

const basicHook = defineHook({
  meta: {
    name: 'example:basic-hook',
    description: 'A simple demonstration hook'
  },
  when: {
    kind: 'sparql-ask',
    ref: {
      uri: 'file://hooks/basic.rq',
      sha256: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
      mediaType: 'application/sparql-query'
    }
  },
  async run({ payload, context }) {
    console.log('Hook triggered with:', payload);
    return { result: { processed: true } };
  }
});
```

## How to Validate Input with before()

```javascript
const validatingHook = defineHook({
  meta: { name: 'example:validating' },
  when: { /* ... */ },

  async before({ payload }) {
    // Cancel if invalid
    if (!payload || !payload.id) {
      return { cancel: true, reason: 'Missing required id' };
    }

    // Validate format
    if (!/^[A-Z]{2}\d{4}$/.test(payload.id)) {
      return { cancel: true, reason: 'Invalid id format (expected: XX9999)' };
    }

    // Enrich and continue
    return {
      ...payload,
      validatedAt: new Date().toISOString()
    };
  },

  async run({ payload }) {
    return { result: { id: payload.id, status: 'processed' } };
  }
});
```

## How to Add Audit Logging with after()

```javascript
const auditedHook = defineHook({
  meta: { name: 'example:audited' },
  when: { /* ... */ },
  receipt: { anchor: 'git-notes' },

  async run({ payload }) {
    const result = await processPayload(payload);
    return { result };
  },

  async after({ result, cancelled, reason }) {
    if (cancelled) {
      await logAudit('CANCELLED', { reason });
      return { result: { status: 'cancelled' } };
    }

    await logAudit('COMPLETED', {
      resultStatus: result?.result?.status,
      timestamp: new Date().toISOString()
    });

    return { result: { finalStatus: 'audited' } };
  }
});

async function logAudit(action, details) {
  console.log(`[AUDIT] ${action}:`, JSON.stringify(details));
}
```

## How to Generate RDF Assertions

```javascript
import { DataFactory } from 'n3';

const { namedNode, literal, quad } = DataFactory;

const assertingHook = defineHook({
  meta: { name: 'example:asserting' },
  when: { /* ... */ },

  async run({ payload }) {
    const resourceId = `urn:resource:${payload.id}`;

    return {
      result: { status: 'created' },
      assertions: [
        quad(
          namedNode(resourceId),
          namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
          namedNode('http://example.org/ProcessedResource')
        ),
        quad(
          namedNode(resourceId),
          namedNode('http://example.org/processedAt'),
          literal(new Date().toISOString(), namedNode('http://www.w3.org/2001/XMLSchema#dateTime'))
        ),
        quad(
          namedNode(resourceId),
          namedNode('http://example.org/status'),
          literal('active')
        )
      ]
    };
  }
});
```

## How to Compute SHA-256 Hash for Condition Files

```javascript
// tools/hash-query.mjs
import { createHash } from 'node:crypto';
import { readFileSync } from 'node:fs';

const filePath = process.argv[2];
if (!filePath) {
  console.error('Usage: node hash-query.mjs <file>');
  process.exit(1);
}

const content = readFileSync(filePath, 'utf-8');
const hash = createHash('sha256').update(content).digest('hex');

console.log(`File: ${filePath}`);
console.log(`SHA-256: ${hash}`);
console.log(`\nUse in defineHook:`);
console.log(`ref: {`);
console.log(`  uri: 'file://${filePath}',`);
console.log(`  sha256: '${hash}',`);
console.log(`  mediaType: 'application/sparql-query'`);
console.log(`}`);
```

Run:

```bash
node tools/hash-query.mjs hooks/my-query.rq
```

## How to Create a Compliance Hook

```javascript
const complianceHook = defineHook({
  meta: {
    name: 'compliance:large-transaction',
    description: 'Flags transactions over threshold for review',
    ontology: ['fibo', 'prov']
  },
  channel: {
    graphs: ['urn:graph:transactions'],
    view: 'delta'
  },
  when: {
    kind: 'sparql-ask',
    ref: {
      uri: 'file://hooks/compliance/large-tx.rq',
      sha256: 'your-hash-here',
      mediaType: 'application/sparql-query'
    }
  },
  receipt: { anchor: 'git-notes' },

  async before({ payload }) {
    if (!payload?.amount || !payload?.accountId) {
      return { cancel: true, reason: 'Missing amount or accountId' };
    }
    return { ...payload, threshold: 10000 };
  },

  async run({ payload }) {
    const { amount, accountId, threshold } = payload;
    const requiresReview = amount > threshold;

    return {
      result: {
        accountId,
        amount,
        requiresReview,
        decision: requiresReview ? 'REVIEW_REQUIRED' : 'AUTO_APPROVED'
      }
    };
  },

  async after({ result }) {
    if (result?.result?.requiresReview) {
      // Trigger review workflow
      await notifyCompliance(result.result);
    }
    return { result: { processed: true } };
  }
});
```

## How to Create a Data Quality Hook

```javascript
const qualityHook = defineHook({
  meta: {
    name: 'quality:validate-customer',
    description: 'Validates customer data quality'
  },
  when: {
    kind: 'shacl',
    ref: {
      uri: 'file://shapes/customer.shacl.ttl',
      sha256: 'your-hash-here',
      mediaType: 'text/turtle'
    }
  },

  async run({ payload, context }) {
    const report = context.validationReport;

    return {
      result: {
        conforms: report?.conforms ?? true,
        violationCount: report?.results?.length ?? 0,
        qualityScore: report?.conforms ? 100 : Math.max(0, 100 - (report?.results?.length * 10))
      }
    };
  }
});
```

## How to Handle Errors Gracefully

```javascript
const robustHook = defineHook({
  meta: { name: 'example:robust' },
  when: { /* ... */ },

  async run({ payload }) {
    try {
      const result = await riskyOperation(payload);
      return { result: { status: 'success', data: result } };
    } catch (error) {
      console.error('Hook error:', error.message);

      return {
        result: {
          status: 'error',
          errorMessage: error.message,
          errorCode: error.code || 'UNKNOWN'
        }
      };
    }
  },

  async after({ result }) {
    if (result?.result?.status === 'error') {
      await alertOps({
        hook: 'example:robust',
        error: result.result.errorMessage
      });
    }
    return { result };
  }
});
```

## How to Chain Multiple Hooks

```javascript
// Define individual hooks
const validateHook = defineHook({ meta: { name: 'pipeline:validate' }, /* ... */ });
const transformHook = defineHook({ meta: { name: 'pipeline:transform' }, /* ... */ });
const persistHook = defineHook({ meta: { name: 'pipeline:persist' }, /* ... */ });

// Create a pipeline runner
async function runPipeline(hooks, initialPayload) {
  let payload = initialPayload;

  for (const hook of hooks) {
    // Run before
    if (hook.before) {
      const beforeResult = await hook.before({ payload, context: {} });
      if (beforeResult.cancel) {
        return { cancelled: true, reason: beforeResult.reason };
      }
      payload = beforeResult;
    }

    // Run main
    const result = await hook.run({ payload, context: {} });

    // Run after
    if (hook.after) {
      await hook.after({ result });
    }

    // Update payload for next hook
    payload = { ...payload, ...result.result };
  }

  return { success: true, payload };
}

// Execute
const result = await runPipeline(
  [validateHook, transformHook, persistHook],
  { data: 'input' }
);
```

## Troubleshooting

### Hook Not Triggering

1. Verify condition file exists at URI
2. Check SHA-256 hash matches file content
3. Ensure `when.kind` matches file type
4. Verify graph name in `channel.graphs`

### Validation Errors

```javascript
// defineHook throws on invalid config
try {
  const hook = defineHook({ /* invalid config */ });
} catch (error) {
  console.error('Hook definition error:', error.message);
}
```

### Missing Context Data

```javascript
async run({ payload, context }) {
  // Always check context properties
  const graph = context?.graph;
  const queryResults = context?.queryResults || [];

  if (!graph) {
    return { result: { error: 'No graph in context' } };
  }
}
```

## Related

- [Knowledge Hooks Tutorial](../tutorials/knowledge-hooks.md) - In-depth learning
- [API Reference](../reference/api-reference.md) - Full hook API
- [Architecture](../explanation/knowledge-hooks-architecture.md) - How hooks work
