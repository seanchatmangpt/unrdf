# Hook Lifecycle

Knowledge Hooks follow a three-phase lifecycle inspired by biological reflex arcs: **Before → Run → After**. This chapter explains each phase in detail.

## Lifecycle Overview

```
┌─────────────────────────────────────────────┐
│             Hook Execution Flow             │
└─────────────────────────────────────────────┘

   Transaction Request
          ↓
   ┌─────────────┐
   │   BEFORE    │  Gate/Normalize
   │   Phase     │  • Validate payload
   └──────┬──────┘  • Normalize data
          │         • Early cancellation
          ↓
   ┌─────────────┐
   │  CONDITION  │  Evaluate
   │  Evaluation │  • SPARQL ASK/SELECT
   └──────┬──────┘  • SHACL validation
          │         • Custom predicates
          ↓
          ?
      Satisfied?
          │
         Yes
          ↓
   ┌─────────────┐
   │     RUN     │  Main Effect
   │    Phase    │  • Core logic
   └──────┬──────┘  • Assertions
          │         • Transformations
          ↓
   ┌─────────────┐
   │    AFTER    │  Cleanup/Audit
   │    Phase    │  • Metrics
   └──────┬──────┘  • Logging
          │         • Receipts
          ↓
   Transaction Complete
```

## Phase 1: Before (Optional)

The **before** phase is a pre-condition gate for:
- Payload validation and normalization
- Early cancellation (avoid expensive condition evaluation)
- Context enrichment

### Function Signature

```javascript
async before({ payload, context, name }) {
  // Return modified payload OR cancellation signal
  return {
    ...payload,
    normalized: true
  };
  // OR
  return {
    cancel: true,
    reason: 'Invalid payload'
  };
}
```

### Example: Payload Validation

```javascript
defineHook({
  meta: { name: 'validate-transaction' },
  when: { kind: 'sparql-ask', query: '...' },

  async before({ payload }) {
    // Validate required fields
    if (!payload.actor) {
      return {
        cancel: true,
        reason: 'Transaction requires an actor'
      };
    }

    if (!payload.additions || payload.additions.length === 0) {
      return {
        cancel: true,
        reason: 'Transaction requires at least one addition'
      };
    }

    // Normalize payload
    return {
      ...payload,
      timestamp: new Date().toISOString(),
      validated: true
    };
  },

  run: async (event) => {
    // Payload now has timestamp and validated flag
    console.log(`Processing transaction at ${event.payload.timestamp}`);
  }
});
```

### Example: Early Cancellation

```javascript
defineHook({
  meta: { name: 'rate-limiter' },
  when: { kind: 'window', duration: '1m' },

  async before({ payload, context }) {
    // Check rate limit before expensive SPARQL evaluation
    const actorRequestCount = context.rateLimitCache?.get(payload.actor) || 0;

    if (actorRequestCount > 100) {
      return {
        cancel: true,
        reason: `Rate limit exceeded for actor ${payload.actor}`
      };
    }

    // Update cache
    context.rateLimitCache?.set(payload.actor, actorRequestCount + 1);

    return payload;
  },

  run: async (event) => {
    // Only runs if rate limit not exceeded
    console.log('Processing allowed transaction');
  }
});
```

### When to Use Before

✅ **Use before when:**
- Validating payload structure
- Normalizing data formats
- Checking simple conditions (avoid SPARQL for basic checks)
- Enriching context
- Implementing fast-path cancellation

❌ **Don't use before for:**
- Complex business logic (belongs in `run`)
- SPARQL queries (use `when` condition)
- Side effects (belongs in `run` or `after`)

## Phase 2: Condition Evaluation (Required)

The **when** clause defines the trigger condition. This phase evaluates the condition against the knowledge graph.

### Condition Types

UNRDF supports multiple condition types (detailed in [Predicates](predicates.md)):

1. **sparql-ask** - Boolean queries
2. **sparql-select** - Result set queries
3. **shacl** - Shape validation
4. **delta** - Change pattern detection
5. **threshold** - Numeric comparisons
6. **count** - Cardinality constraints
7. **window** - Time-based aggregations

### Evaluation Process

```javascript
// Condition evaluation happens automatically between before and run
const hook = defineHook({
  when: {
    kind: 'sparql-ask',
    query: `
      ASK {
        ?person a foaf:Person .
        FILTER NOT EXISTS { ?person foaf:name ?name }
      }
    `
  },
  run: async (event) => {
    // event.result contains the ASK query result (true/false)
    if (event.result === true) {
      throw new Error('Person without name detected');
    }
  }
});
```

### Condition Result Structure

The `event.result` passed to `run` contains:

```javascript
// For sparql-ask
event.result = true | false

// For sparql-select
event.result = [
  { name: 'Alice', age: 30 },
  { name: 'Bob', age: 25 }
]

// For shacl
event.result = {
  conforms: false,
  results: [
    {
      path: 'ex:name',
      message: 'Name is required',
      severity: 'Violation'
    }
  ]
}

// For threshold
event.result = {
  value: 15000,
  threshold: 10000,
  operator: 'gt',
  satisfied: true
}
```

## Phase 3: Run (Required)

The **run** phase executes the main hook logic. This is where you:
- Enforce business rules
- Transform data
- Generate new assertions
- Trigger external actions

### Function Signature

```javascript
async run({ result, payload, context, name }) {
  // Perform main logic
  // Return result object
  return {
    result: 'processed',
    assertions: [/* RDF quads */],
    deltas: { additions: [], removals: [] }
  };
}
```

### Example: Data Validation

```javascript
defineHook({
  meta: { name: 'age-validation' },
  when: {
    kind: 'sparql-ask',
    query: `
      ASK {
        ?person ex:age ?age .
        FILTER (?age < 18)
      }
    `
  },

  run: async (event) => {
    if (event.result === true) {
      // Throw error to reject transaction
      throw new Error('All persons must be 18 or older');
    }

    return { result: 'valid' };
  }
});
```

### Example: Data Transformation

```javascript
defineHook({
  meta: { name: 'auto-generate-id' },
  when: {
    kind: 'delta',
    pattern: { predicate: 'rdf:type', object: 'foaf:Person' }
  },

  run: async (event) => {
    const newPersons = event.delta.additions.filter(
      quad => quad.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'
    );

    const assertions = [];
    for (const quad of newPersons) {
      const personIri = quad.subject;
      const id = generateUUID();

      assertions.push(
        quad(personIri, namedNode('ex:id'), literal(id))
      );
    }

    return {
      result: `Generated ${assertions.length} IDs`,
      assertions
    };
  }
});
```

### Example: External Notification

```javascript
defineHook({
  meta: { name: 'webhook-notification' },
  when: {
    kind: 'threshold',
    value: 10000,
    operator: 'gte'
  },

  run: async (event) => {
    await fetch('https://api.example.com/webhooks/large-transaction', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        value: event.result.value,
        timestamp: new Date().toISOString()
      })
    });

    return {
      result: 'notification-sent',
      timestamp: new Date().toISOString()
    };
  }
});
```

### Veto Pattern

Throwing an error in `run` **vetoes** the transaction:

```javascript
run: async (event) => {
  if (event.result === true) {
    // Veto: reject the entire transaction
    throw new Error('Validation failed');
  }

  // Allow: transaction proceeds
  return { result: 'valid' };
}
```

## Phase 4: After (Optional)

The **after** phase is for cleanup, auditing, and metrics. It runs **after** the `run` phase completes, regardless of success or cancellation.

### Function Signature

```javascript
async after({ result, cancelled, reason, payload, context, name }) {
  // Cleanup and audit logic
  // Return final result
  return {
    finalStatus: cancelled ? 'cancelled' : 'completed'
  };
}
```

### Example: Metrics Collection

```javascript
defineHook({
  meta: { name: 'performance-tracking' },
  when: { kind: 'sparql-ask', query: '...' },

  run: async (event) => {
    const startTime = Date.now();
    // Main logic
    const result = await processTransaction(event);
    const duration = Date.now() - startTime;

    return { result, duration };
  },

  async after({ result, cancelled, reason }) {
    if (cancelled) {
      console.log(`❌ Hook cancelled: ${reason}`);
      await metrics.increment('hook.cancelled');
    } else {
      console.log(`✅ Hook completed in ${result.duration}ms`);
      await metrics.histogram('hook.duration', result.duration);
    }

    return {
      finalStatus: cancelled ? 'cancelled' : 'completed',
      timestamp: new Date().toISOString()
    };
  }
});
```

### Example: Audit Logging

```javascript
defineHook({
  meta: { name: 'audit-trail' },
  when: { kind: 'delta', pattern: {} },

  run: async (event) => {
    // Main processing
    return {
      result: 'processed',
      affectedTriples: event.delta.additions.length
    };
  },

  async after({ result, cancelled, payload }) {
    const auditEntry = {
      hookName: 'audit-trail',
      actor: payload.actor,
      timestamp: new Date().toISOString(),
      status: cancelled ? 'cancelled' : 'completed',
      affectedTriples: result?.affectedTriples || 0
    };

    await auditLog.write(auditEntry);

    return { auditRecorded: true };
  }
});
```

### When to Use After

✅ **Use after when:**
- Recording metrics
- Writing audit logs
- Cleanup operations
- Final notifications
- Collecting execution metadata

❌ **Don't use after for:**
- Main business logic (belongs in `run`)
- Transaction validation (belongs in `run`)
- Expensive operations (after runs even on failure)

## Context Isolation

Each hook execution has an isolated context:

```javascript
{
  name: 'hook-name',           // Hook identifier
  payload: {                   // Transaction data
    additions: [...],
    removals: [...],
    actor: 'user@example.org'
  },
  context: {                   // Execution context
    graph: Store,              // RDF graph
    env: {...},                // Environment variables
    metadata: {...}            // Custom metadata
  },
  result: ...,                 // Condition evaluation result
  cancelled: false,            // Cancellation flag
  reason: null                 // Cancellation reason
}
```

## Error Handling

### In Before Phase

```javascript
before: async ({ payload }) => {
  try {
    validate(payload);
    return payload;
  } catch (error) {
    return {
      cancel: true,
      reason: error.message
    };
  }
}
```

### In Run Phase

```javascript
run: async (event) => {
  try {
    if (event.result === true) {
      throw new Error('Validation failed');
    }
    return { result: 'valid' };
  } catch (error) {
    // Error propagates to transaction (veto)
    throw error;
  }
}
```

### In After Phase

```javascript
after: async ({ result, cancelled }) => {
  try {
    await recordMetrics(result);
  } catch (error) {
    // Errors in after don't fail the transaction
    console.error('Metrics recording failed:', error);
  }

  return { finalStatus: 'completed' };
}
```

## Performance Considerations

### Optimize Before Phase

```javascript
// ✅ Good: Fast checks in before
before: async ({ payload }) => {
  if (!payload.actor) {
    return { cancel: true, reason: 'Missing actor' };
  }
  return payload;
}

// ❌ Bad: Slow operations in before
before: async ({ payload }) => {
  await complexDatabaseQuery(); // Slow!
  return payload;
}
```

### Minimize After Phase Work

```javascript
// ✅ Good: Async fire-and-forget
after: async ({ result }) => {
  // Fire and forget (don't await)
  sendNotification(result).catch(console.error);
  return { status: 'completed' };
}

// ❌ Bad: Blocking operations
after: async ({ result }) => {
  await slowExternalAPI(result); // Blocks transaction!
  return { status: 'completed' };
}
```

## Complete Lifecycle Example

```javascript
import { defineHook } from 'unrdf';

const comprehensiveHook = defineHook({
  meta: {
    name: 'comprehensive-example',
    description: 'Demonstrates full lifecycle'
  },

  when: {
    kind: 'sparql-ask',
    query: `
      ASK {
        ?person a foaf:Person .
        FILTER NOT EXISTS { ?person foaf:name ?name }
      }
    `
  },

  // Phase 1: Before
  async before({ payload }) {
    // Validate payload
    if (!payload || !payload.actor) {
      return {
        cancel: true,
        reason: 'Transaction requires an actor'
      };
    }

    // Normalize payload
    return {
      ...payload,
      validatedAt: new Date().toISOString(),
      normalizedActor: payload.actor.toLowerCase()
    };
  },

  // Phase 2: When (automatic)
  // SPARQL ASK query evaluates automatically

  // Phase 3: Run
  async run({ result, payload }) {
    console.log(`Validating transaction by ${payload.normalizedActor}`);

    if (result === true) {
      // Veto: person without name detected
      throw new Error('All persons must have a name');
    }

    return {
      result: 'valid',
      validatedAt: payload.validatedAt,
      validator: 'comprehensive-example'
    };
  },

  // Phase 4: After
  async after({ result, cancelled, reason, payload }) {
    if (cancelled) {
      console.log(`❌ Validation cancelled: ${reason}`);
      await metrics.increment('validation.cancelled');
    } else {
      console.log(`✅ Validation passed at ${result.validatedAt}`);
      await metrics.increment('validation.passed');
    }

    // Record audit trail
    await auditLog.write({
      hook: 'comprehensive-example',
      actor: payload.normalizedActor,
      status: cancelled ? 'cancelled' : 'completed',
      timestamp: new Date().toISOString()
    });

    return {
      finalStatus: cancelled ? 'cancelled' : 'completed',
      auditRecorded: true
    };
  }
});
```

## Next Steps

Continue to learn about:
- **[Predicates](predicates.md)** - Different condition types (ASK, THRESHOLD, DELTA, SHACL, etc.)
- **[Effects](effects.md)** - What hooks can do (veto, log, notify, transform)
- **[Policy Packs](policy-packs.md)** - Organizing hooks into reusable packages
