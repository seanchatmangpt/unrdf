# Tutorial 1: Your First Knowledge Hook

**Time to Complete**: 15 minutes
**Difficulty**: Beginner
**Prerequisites**: Node.js 18+, basic JavaScript knowledge

## What You'll Learn

In this tutorial, you'll learn how to:
- Define a Knowledge Hook with `defineHook`
- Understand the hook lifecycle (before, run, after)
- Execute a hook manually
- Add basic validation and error handling

## Introduction

Knowledge Hooks are the core of UNRDF's autonomic system. They allow you to monitor RDF data changes and automatically respond with validation, transformation, or audit logic. Think of them as "database triggers" but for knowledge graphs.

## Prerequisites

Make sure you have UNRDF installed:

```bash
npm install unrdf
# or
pnpm add unrdf
```

## Step 1: Understanding the Hook Lifecycle

Every Knowledge Hook has three phases:

1. **before**: Pre-execution validation and payload transformation
2. **run**: Main execution logic
3. **after**: Post-execution cleanup and audit trail

This pattern ensures that hooks are testable, composable, and auditable.

## Step 2: Create Your First Hook

Let's create a simple hook that monitors system health. Create a file `my-first-hook.mjs`:

```javascript
import { defineHook } from 'unrdf/knowledge-engine';

const healthCheckHook = defineHook({
  // Metadata: describes the hook
  meta: {
    name: 'system:health-check',
    description: 'Monitor system health and alert on issues',
    ontology: ['ex'], // Ontologies this hook uses
  },

  // Channel: which graphs to watch
  channel: {
    graphs: ['urn:graph:system'],
    view: 'delta', // Watch for changes
  },

  // Condition: when to trigger
  when: {
    kind: 'sparql-ask', // Trigger on SPARQL ASK query
    ref: {
      uri: 'file://hooks/health-check.ask.rq',
      sha256: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
      mediaType: 'application/sparql-query',
    },
  },

  // Determinism: reproducible execution
  determinism: {
    seed: 42,
  },

  // Receipt: audit trail configuration
  receipt: {
    anchor: 'none', // Use 'git-notes' for production
  },

  // Phase 1: Pre-execution validation
  async before({ payload }) {
    console.log('[BEFORE] Validating payload...');

    // Validate payload structure
    if (!payload || typeof payload !== 'object') {
      return {
        cancel: true,
        reason: 'Invalid payload: must be an object',
      };
    }

    // Normalize and enhance payload
    return {
      ...payload,
      timestamp: Date.now(),
      validated: true,
    };
  },

  // Phase 2: Main execution
  async run({ payload }) {
    console.log('[RUN] Health check triggered!');
    console.log('Payload:', JSON.stringify(payload, null, 2));

    // Your hook logic goes here
    const healthStatus = {
      status: 'healthy',
      checks: {
        database: 'ok',
        cache: 'ok',
        api: 'ok',
      },
      timestamp: payload.timestamp,
    };

    // Return result and optional RDF assertions
    return {
      result: healthStatus,
      assertions: [], // Add RDF quads here if needed
    };
  },

  // Phase 3: Post-execution cleanup
  async after({ result, cancelled, reason }) {
    if (cancelled) {
      console.log(`[AFTER] Hook cancelled: ${reason}`);
      return {
        result: {
          finalStatus: 'cancelled',
          reason,
        },
      };
    }

    console.log('[AFTER] Health check completed');
    console.log('Result:', JSON.stringify(result, null, 2));

    // Audit logging
    return {
      result: {
        ...result,
        finalStatus: 'completed',
        audited: true,
      },
    };
  },
});

export default healthCheckHook;
```

## Step 3: Execute the Hook

Now let's test the hook manually:

```javascript
// Simulate a hook trigger event
const testEvent = {
  name: 'manual-trigger',
  payload: {
    source: 'test',
    data: 'sample',
  },
  context: {
    env: {
      environment: 'development',
    },
  },
};

// Execute the lifecycle
const beforeResult = await healthCheckHook.before(testEvent);
console.log('Before result:', beforeResult);

if (!beforeResult.cancel) {
  // Update payload with before result
  testEvent.payload = beforeResult;

  // Execute run
  const runResult = await healthCheckHook.run(testEvent);
  console.log('Run result:', runResult);

  // Execute after
  const afterResult = await healthCheckHook.after({
    ...testEvent,
    result: runResult.result,
    cancelled: false,
  });
  console.log('After result:', afterResult);
}
```

## Step 4: Run Your Hook

Execute the script:

```bash
node my-first-hook.mjs
```

You should see output like:

```
[BEFORE] Validating payload...
Before result: { source: 'test', data: 'sample', timestamp: 1696284567890, validated: true }
[RUN] Health check triggered!
Payload: {
  "source": "test",
  "data": "sample",
  "timestamp": 1696284567890,
  "validated": true
}
Run result: {
  result: {
    status: 'healthy',
    checks: { database: 'ok', cache: 'ok', api: 'ok' },
    timestamp: 1696284567890
  },
  assertions: []
}
[AFTER] Health check completed
Result: { status: 'healthy', checks: { database: 'ok', cache: 'ok', api: 'ok' }, timestamp: 1696284567890 }
After result: {
  result: {
    status: 'healthy',
    checks: { database: 'ok', cache: 'ok', api: 'ok' },
    timestamp: 1696284567890,
    finalStatus: 'completed',
    audited: true
  }
}
```

## Understanding the Output

Let's break down what happened:

1. **before phase**: Validated the payload and added a timestamp
2. **run phase**: Executed health check logic and returned status
3. **after phase**: Added audit metadata and marked as completed

## What You Learned

- How to define a Knowledge Hook with `defineHook`
- The three-phase lifecycle: before, run, after
- How to validate payloads and cancel execution
- How to return results and RDF assertions
- How to execute hooks manually for testing

## Next Steps

In the next tutorial, you'll learn how to work with RDF data using UNRDF's parsing, serialization, and query capabilities.

**Continue to**: [Tutorial 2: Working with RDF Data](./02-rdf-operations.md)

## Complete Example

See the full working example at: `examples/basic-knowledge-hook.mjs`

## Reference

- [defineHook API Reference](../reference/api/knowledge-engine.md#defineHook)
- [Hook Specification Schema](../reference/schemas.md#hook-specification)
- [Knowledge Hooks Concepts](../explanation/concepts/knowledge-hooks.md)
