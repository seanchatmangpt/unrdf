# Your First Knowledge Hook

Knowledge Hooks are the heart of UNRDF's autonomic capabilities. They transform your knowledge graph from a passive data store into an intelligent, self-governing system.

## What is a Knowledge Hook?

A **Knowledge Hook** is a policy-driven trigger that:

1. **Observes** - Monitors the knowledge graph for specific conditions
2. **Evaluates** - Checks if a condition is met (using SPARQL, SHACL, or custom logic)
3. **Reacts** - Executes effects when triggered (validate, transform, notify, etc.)

Think of hooks as "business rules" or "database triggers" for knowledge graphs, but with:

- **Declarative conditions** (SPARQL queries, SHACL shapes)
- **Cryptographic provenance** (every hook execution is logged)
- **Effect sandboxing** (hooks run in isolated environments)
- **Performance optimization** (hooks are batched and parallelized)

```admonish info
Knowledge Hooks are inspired by Nitro's `defineTask` and React's `useEffect`, but designed specifically for knowledge-native, policy-first systems.
```

## Creating Your First Hook

Let's create a simple validation hook that ensures all persons have names.

### Step 1: Define the Hook

```javascript
import { defineHook } from 'unrdf';

const nameValidationHook = defineHook({
  meta: {
    name: 'person-name-required',
    description: 'Ensures all persons have names'
  },
  when: {
    kind: 'sparql-ask',
    query: `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>

      ASK {
        ?person a foaf:Person .
        FILTER NOT EXISTS { ?person foaf:name ?name }
      }
    `
  },
  run: async (event) => {
    if (event.result === true) {
      throw new Error('All persons must have a name');
    }
  }
});
```

**Let's break this down:**

- **`meta`** - Identifies the hook for logging and debugging
- **`when`** - Defines the trigger condition (SPARQL ASK query)
- **`run`** - Executes when the condition is true

```admonish tip
The `when` clause uses a SPARQL ASK query that returns `true` if any person exists without a name. The `run` function then throws an error to prevent the transaction.
```

### Step 2: Register the Hook

```javascript
import { registerHook } from 'unrdf';

await registerHook(nameValidationHook);

console.log('✅ Hook registered successfully');
```

Once registered, the hook is **active** and will evaluate on every transaction.

### Step 3: Test the Hook

```javascript
import { createDarkMatterCore } from 'unrdf';
import { namedNode, quad, literal } from '@rdfjs/data-model';

const system = await createDarkMatterCore();

// This transaction will FAIL (person without name)
try {
  await system.executeTransaction({
    additions: [
      quad(
        namedNode('http://example.org/alice'),
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://xmlns.com/foaf/0.1/Person')
      )
      // Missing foaf:name!
    ],
    removals: [],
    actor: 'test-user'
  });
} catch (error) {
  console.log('❌ Transaction rejected:', error.message);
  // Output: ❌ Transaction rejected: All persons must have a name
}

// This transaction will SUCCEED (person with name)
await system.executeTransaction({
  additions: [
    quad(
      namedNode('http://example.org/bob'),
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      namedNode('http://xmlns.com/foaf/0.1/Person')
    ),
    quad(
      namedNode('http://example.org/bob'),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal('Bob')
    )
  ],
  removals: [],
  actor: 'test-user'
});

console.log('✅ Transaction succeeded');

await system.cleanup();
```

```admonish success title="Congratulations!"
You've created your first Knowledge Hook! Your knowledge graph now **automatically enforces** the "persons must have names" policy.
```

## Complete Example

Here's a complete, runnable example:

```javascript
import { createDarkMatterCore, defineHook, registerHook } from 'unrdf';
import { namedNode, quad, literal } from '@rdfjs/data-model';

// 1. Define the hook
const nameValidationHook = defineHook({
  meta: {
    name: 'person-name-required',
    description: 'Ensures all persons have names'
  },
  when: {
    kind: 'sparql-ask',
    query: `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>

      ASK {
        ?person a foaf:Person .
        FILTER NOT EXISTS { ?person foaf:name ?name }
      }
    `
  },
  run: async (event) => {
    if (event.result === true) {
      throw new Error('All persons must have a name');
    }
  }
});

// 2. Create system and register hook
const system = await createDarkMatterCore();
await registerHook(nameValidationHook);

// 3. Test valid transaction (should succeed)
await system.executeTransaction({
  additions: [
    quad(
      namedNode('http://example.org/alice'),
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      namedNode('http://xmlns.com/foaf/0.1/Person')
    ),
    quad(
      namedNode('http://example.org/alice'),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal('Alice')
    )
  ],
  removals: [],
  actor: 'admin'
});

console.log('✅ Valid transaction succeeded');

// 4. Test invalid transaction (should fail)
try {
  await system.executeTransaction({
    additions: [
      quad(
        namedNode('http://example.org/bob'),
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://xmlns.com/foaf/0.1/Person')
      )
      // Missing name!
    ],
    removals: [],
    actor: 'admin'
  });
} catch (error) {
  console.log('❌ Invalid transaction rejected:', error.message);
}

// 5. Cleanup
await system.cleanup();
```

Save this as `first-hook.mjs` and run:

```bash
node first-hook.mjs
```

**Expected output:**

```
✅ Valid transaction succeeded
❌ Invalid transaction rejected: All persons must have a name
```

## Hook Types

UNRDF supports multiple hook types for different use cases:

### 1. SPARQL ASK Hooks

Use SPARQL ASK queries to check conditions:

```javascript
defineHook({
  meta: { name: 'age-validation', description: 'Ensure age >= 18' },
  when: {
    kind: 'sparql-ask',
    query: `
      PREFIX ex: <http://example.org/>

      ASK {
        ?person ex:age ?age .
        FILTER (?age < 18)
      }
    `
  },
  run: async (event) => {
    if (event.result === true) {
      throw new Error('All persons must be 18 or older');
    }
  }
});
```

### 2. SHACL Hooks

Use SHACL shapes for structural validation:

```javascript
defineHook({
  meta: { name: 'shacl-validation', description: 'Validate against shapes' },
  when: {
    kind: 'shacl',
    shapes: shaclShapesStore // N3.Store containing SHACL shapes
  },
  run: async (event) => {
    if (!event.conforms) {
      throw new Error(`SHACL validation failed: ${event.results.length} violations`);
    }
  }
});
```

### 3. Delta Hooks

React to specific changes in the graph:

```javascript
defineHook({
  meta: { name: 'audit-logger', description: 'Log all person changes' },
  when: {
    kind: 'delta',
    pattern: {
      predicate: namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      object: namedNode('http://xmlns.com/foaf/0.1/Person')
    }
  },
  run: async (event) => {
    console.log(`Person added: ${event.delta.additions.length} triples`);
  }
});
```

### 4. Threshold Hooks

Trigger when numeric values exceed thresholds:

```javascript
defineHook({
  meta: { name: 'large-transaction', description: 'Alert on large transactions' },
  when: {
    kind: 'threshold',
    value: 1000,
    operator: 'gt'
  },
  run: async (event) => {
    console.log(`Large transaction detected: ${event.value} triples`);
  }
});
```

### 5. Count Hooks

Enforce cardinality constraints:

```javascript
defineHook({
  meta: { name: 'max-persons', description: 'Limit total persons' },
  when: {
    kind: 'count',
    min: 0,
    max: 10000
  },
  run: async (event) => {
    if (event.count > 10000) {
      throw new Error('Maximum 10,000 persons allowed');
    }
  }
});
```

### 6. Window Hooks

Time-based aggregations:

```javascript
defineHook({
  meta: { name: 'rate-limiter', description: 'Limit transaction rate' },
  when: {
    kind: 'window',
    duration: '5m'
  },
  run: async (event) => {
    if (event.count > 100) {
      throw new Error('Rate limit exceeded: max 100 transactions per 5 minutes');
    }
  }
});
```

## Hook Lifecycle

Hooks have three lifecycle phases:

### Before Phase (Optional)

Pre-condition gate for payload normalization or early cancellation:

```javascript
defineHook({
  meta: { name: 'example', description: 'Example with before' },
  when: { kind: 'sparql-ask', query: '...' },

  async before({ payload }) {
    // Validate payload
    if (!payload.actor) {
      return { cancel: true, reason: 'Actor required' };
    }

    // Normalize payload
    return { ...payload, timestamp: new Date().toISOString() };
  },

  run: async (event) => {
    // Main logic
  }
});
```

### Run Phase (Required)

The core effect or analysis:

```javascript
run: async (event) => {
  // event.result - Condition evaluation result
  // event.payload - Transaction payload
  // event.context - Execution context

  if (event.result === true) {
    throw new Error('Condition violated');
  }

  return { result: 'success' };
}
```

### After Phase (Optional)

Post-execution step for auditing and cleanup:

```javascript
defineHook({
  meta: { name: 'example', description: 'Example with after' },
  when: { kind: 'sparql-ask', query: '...' },

  run: async (event) => {
    // Main logic
    return { result: 'processed' };
  },

  async after({ result, cancelled, reason }) {
    if (cancelled) {
      console.log(`Hook cancelled: ${reason}`);
    } else {
      console.log(`Hook completed: ${result}`);
    }

    return { finalStatus: cancelled ? 'cancelled' : 'completed' };
  }
});
```

## Advanced Example: Age Validation with Lifecycle

```javascript
import { defineHook, registerHook } from 'unrdf';

const ageValidationHook = defineHook({
  meta: {
    name: 'age-validation',
    description: 'Ensure age is valid and >= 18'
  },

  when: {
    kind: 'sparql-ask',
    query: `
      PREFIX ex: <http://example.org/>

      ASK {
        ?person ex:age ?age .
        FILTER (?age < 18)
      }
    `
  },

  async before({ payload }) {
    // Validate payload has required fields
    if (!payload || !payload.actor) {
      return {
        cancel: true,
        reason: 'Transaction requires an actor'
      };
    }

    // Add timestamp for audit
    return {
      ...payload,
      validatedAt: new Date().toISOString()
    };
  },

  async run({ result, payload }) {
    console.log(`Validating age for transaction by ${payload.actor}`);

    if (result === true) {
      throw new Error('All persons must be 18 or older');
    }

    return {
      result: 'valid',
      validatedAt: payload.validatedAt
    };
  },

  async after({ result, cancelled, reason }) {
    if (cancelled) {
      console.log(`❌ Validation cancelled: ${reason}`);
    } else {
      console.log(`✅ Validation passed at ${result.validatedAt}`);
    }

    return {
      finalStatus: cancelled ? 'cancelled' : 'completed',
      timestamp: new Date().toISOString()
    };
  }
});

await registerHook(ageValidationHook);
```

## Managing Hooks

### List Registered Hooks

```javascript
import { getRegisteredHooks } from 'unrdf';

const hooks = getRegisteredHooks();
console.log(`${hooks.length} hooks registered:`);

hooks.forEach(hook => {
  console.log(`- ${hook.meta.name}: ${hook.meta.description}`);
});
```

### Deregister a Hook

```javascript
import { deregisterHook } from 'unrdf';

await deregisterHook('person-name-required');
console.log('Hook deregistered');
```

### Reset All Hooks

```javascript
import { resetGlobalHookManager } from 'unrdf';

resetGlobalHookManager();
console.log('All hooks cleared');
```

## Best Practices

### 1. Keep Hooks Focused

```javascript
// ❌ Bad: Hook does too much
defineHook({
  meta: { name: 'do-everything', description: 'Does everything' },
  when: { kind: 'sparql-ask', query: '...' },
  run: async (event) => {
    validateAge(event);
    validateName(event);
    validateEmail(event);
    sendNotification(event);
    updateCache(event);
  }
});

// ✅ Good: Separate hooks for separate concerns
defineHook({ meta: { name: 'validate-age' }, ... });
defineHook({ meta: { name: 'validate-name' }, ... });
defineHook({ meta: { name: 'validate-email' }, ... });
```

### 2. Use Descriptive Names

```javascript
// ❌ Bad
defineHook({ meta: { name: 'hook1' }, ... });

// ✅ Good
defineHook({ meta: { name: 'person-age-validation' }, ... });
```

### 3. Handle Errors Gracefully

```javascript
run: async (event) => {
  try {
    if (event.result === true) {
      throw new Error('Validation failed');
    }
  } catch (error) {
    // Log for debugging
    console.error(`Hook ${event.name} failed:`, error);

    // Re-throw to fail transaction
    throw error;
  }
}
```

### 4. Use `before` for Early Cancellation

```javascript
before: async ({ payload }) => {
  // Cancel early if payload is invalid
  if (!isValidPayload(payload)) {
    return { cancel: true, reason: 'Invalid payload' };
  }

  // Don't proceed to expensive SPARQL evaluation
  return payload;
}
```

## What's Next?

Now that you understand Knowledge Hooks, explore:

1. **Policy Packs** - Organize related hooks into reusable packages
2. **Advanced Hooks** - Learn about effect composition and multi-agent coordination
3. **Performance Tuning** - Optimize hook execution for production

```admonish tip
For production systems, enable hook observability to track performance:

```javascript
const system = await createDarkMatterCore({
  enableObservability: true
});

// After running hooks
const metrics = system.getComponent('observability').getPerformanceMetrics();
console.log(`Hook p95 latency: ${metrics.hookExecutionLatency.p95}ms`);
```
```

---

**Congratulations!** You've mastered the basics of Knowledge Hooks. Your knowledge graph is now an intelligent, self-governing system.
