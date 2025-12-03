# How-To: Create Knowledge Hooks

**Problem**: You need to automatically respond to graph changes, validate transactions, or trigger side effects when RDF data is modified.

## Solution

Knowledge Hooks are autonomic event-driven functions that execute during the lifecycle of RDF operations. Define hooks with `defineHook()` and register them with `registerHook()`.

### Basic Hook Structure

Every hook has three lifecycle phases:

```javascript
import { defineHook, registerHook } from 'unrdf';

const myHook = defineHook({
  // Metadata
  meta: {
    name: 'my-first-hook',
    description: 'Responds to transaction events',
    ontology: 'http://example.org/ontology#MyHook'
  },

  // Channel (what to watch)
  channel: {
    graphs: ['http://example.org/graph1'],  // Watch specific graphs, or ['*'] for all
    view: 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }'  // Optional SPARQL filter
  },

  // Trigger condition
  when: {
    kind: 'transaction',  // or 'query', 'validation', 'custom'
    ref: {
      uri: 'http://example.org/hook-def',
      sha256: 'abc123...',  // Optional content hash
      mediaType: 'application/javascript'
    }
  },

  // Lifecycle methods
  async before(event) {
    // Pre-execution validation
    console.log('Before transaction:', event.delta);
    return { validated: true };
  },

  async run(event) {
    // Main execution
    console.log('Processing transaction...');

    // Access event data
    const { store, delta, metadata } = event;

    // Perform work
    return { success: true };
  },

  async after(result) {
    // Post-execution cleanup
    console.log('Transaction completed:', result);
  }
});

// Register hook globally
registerHook(myHook);
```

### Transaction Validation Hook

Validate data before committing:

```javascript
import { defineHook, validateShacl, getValidationErrors } from 'unrdf';

const validationHook = defineHook({
  meta: {
    name: 'shacl-enforcer',
    description: 'Enforces SHACL constraints on all transactions'
  },

  channel: {
    graphs: ['*']  // Watch all graphs
  },

  when: { kind: 'transaction' },

  async before(event) {
    // Load SHACL shapes
    const shapesTtl = await fs.readFile('./shapes.ttl', 'utf-8');

    // Validate entire store after delta applied
    const report = await validateShacl(event.store, shapesTtl);

    if (!report.conforms) {
      const errors = getValidationErrors(report);
      throw new Error(`SHACL violation: ${errors[0].message}`);
    }

    return { valid: true };
  }
});

registerHook(validationHook);
```

### Audit Trail Hook

Log all changes to immutable log:

```javascript
import { defineHook, LockchainWriter } from 'unrdf';

const lockchain = new LockchainWriter();

const auditHook = defineHook({
  meta: {
    name: 'audit-logger',
    description: 'Logs all transactions to Lockchain'
  },

  channel: { graphs: ['*'] },
  when: { kind: 'transaction' },

  async after(result) {
    const { event, success } = result;

    if (success) {
      // Write immutable receipt
      const receipt = await lockchain.append({
        timestamp: Date.now(),
        delta: event.delta,
        metadata: event.metadata,
        actor: event.metadata.userId || 'system'
      });

      console.log(`Audit receipt: ${receipt.id}`);
    }
  }
});

registerHook(auditHook);
```

### Notification Hook

Send alerts on specific patterns:

```javascript
import { defineHook, ask } from 'unrdf';

const notificationHook = defineHook({
  meta: {
    name: 'alert-on-person-add',
    description: 'Sends notification when new Person added'
  },

  channel: {
    graphs: ['http://example.org/people'],
    view: `
      PREFIX schema: <http://schema.org/>
      SELECT ?person WHERE {
        ?person a schema:Person .
      }
    `
  },

  when: { kind: 'transaction' },

  async run(event) {
    // Check if Person was added
    const hasNewPerson = event.delta.additions.some(quad =>
      quad.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' &&
      quad.object.value === 'http://schema.org/Person'
    );

    if (hasNewPerson) {
      await sendEmail({
        to: 'admin@example.org',
        subject: 'New Person Added',
        body: `${event.delta.additions.length} quads added`
      });
    }

    return { notificationSent: hasNewPerson };
  }
});

registerHook(notificationHook);
```

### Computed Property Hook

Automatically derive values:

```javascript
import { defineHook, select, DataFactory } from 'unrdf';

const computedHook = defineHook({
  meta: {
    name: 'full-name-computer',
    description: 'Computes fullName from firstName + lastName'
  },

  channel: { graphs: ['*'] },
  when: { kind: 'transaction' },

  async run(event) {
    const { store } = event;

    // Find persons with first + last name but no fullName
    const persons = select(store, `
      PREFIX schema: <http://schema.org/>

      SELECT ?person ?firstName ?lastName WHERE {
        ?person schema:givenName ?firstName ;
                schema:familyName ?lastName .
        FILTER NOT EXISTS { ?person schema:name ?fullName }
      }
    `);

    // Add computed fullName
    persons.forEach(row => {
      const fullName = `${row.firstName.value} ${row.lastName.value}`;
      const quad = DataFactory.quad(
        row.person,
        DataFactory.namedNode('http://schema.org/name'),
        DataFactory.literal(fullName)
      );

      store.addQuad(quad);
    });

    return { computed: persons.length };
  }
});

registerHook(computedHook);
```

### Hook Composition

Chain multiple hooks in sequence:

```javascript
import { TransactionManager } from 'unrdf';

const txManager = new TransactionManager();

// Add hooks in order
txManager.addHook(validationHook);    // 1. Validate
txManager.addHook(computedHook);      // 2. Compute derived values
txManager.addHook(auditHook);         // 3. Log to audit trail
txManager.addHook(notificationHook);  // 4. Send notifications

// Apply transaction - all hooks execute automatically
await txManager.apply(store, {
  additions: [quad1, quad2],
  removals: []
});
```

## Variations

### Conditional Execution

Use SPARQL conditions in `channel.view`:

```javascript
const conditionalHook = defineHook({
  meta: { name: 'high-value-alert' },

  channel: {
    graphs: ['*'],
    view: `
      PREFIX ex: <http://example.org/>
      SELECT ?order WHERE {
        ?order ex:amount ?amount .
        FILTER (?amount > 1000)
      }
    `
  },

  when: { kind: 'transaction' },

  async run(event) {
    // Only executes if condition matches
    console.log('High-value order detected');
  }
});
```

### Deterministic Seeds

Ensure reproducible execution:

```javascript
const deterministicHook = defineHook({
  meta: { name: 'reproducible-processor' },

  channel: { graphs: ['*'] },
  when: { kind: 'transaction' },

  determinism: {
    seed: 'fixed-seed-12345'  // Same seed → same results
  },

  async run(event) {
    // Use seeded randomness
    const random = seededRandom(event.determinism.seed);
    const value = random();  // Deterministic

    return { value };
  }
});
```

### Receipt Anchoring

Link to external proof systems:

```javascript
const anchoredHook = defineHook({
  meta: { name: 'blockchain-anchor' },

  channel: { graphs: ['*'] },
  when: { kind: 'transaction' },

  receipt: {
    anchor: 'ethereum://mainnet/tx/0xabc123...'
  },

  async after(result) {
    // Anchor receipt to blockchain
    const txHash = await ethereum.anchor({
      data: result.receipt,
      contract: '0x...'
    });

    console.log(`Anchored: ${txHash}`);
  }
});
```

### Hook Management

List and control registered hooks:

```javascript
import { getRegisteredHooks, deregisterHook, resetGlobalHookManager } from 'unrdf';

// List all hooks
const hooks = getRegisteredHooks();
hooks.forEach(hook => {
  console.log(`Hook: ${hook.meta.name}`);
});

// Remove specific hook
deregisterHook(validationHook.meta.name);

// Clear all hooks
resetGlobalHookManager();
```

## Hook Lifecycle Diagram

Knowledge Hooks follow a structured three-phase lifecycle:

```
Event Triggered (transaction, query, validation, etc)
    ↓
┌─────────────────────────────────────────────┐
│ BEFORE PHASE (Pre-execution gate)           │
│ before(event) → Validate, normalize, cancel │
│ Returns: payload or { cancel: true }        │
└─────────────────────────────────────────────┘
    ↓ (if not cancelled)
┌─────────────────────────────────────────────┐
│ RUN PHASE (Main execution)                  │
│ run(event) → Perform work, create output    │
│ Returns: { result, assertions, deltas }     │
└─────────────────────────────────────────────┘
    ↓
┌─────────────────────────────────────────────┐
│ AFTER PHASE (Post-execution cleanup)        │
│ after(result) → Audit, notify, persist      │
│ Returns: { result } or modified result      │
└─────────────────────────────────────────────┘
    ↓
Lifecycle Complete (hook execution finished)
```

**Key Points**:
- `before` can cancel execution by returning `{ cancel: true, reason: '...' }`
- `run` executes only if `before` doesn't cancel
- `after` always executes, regardless of success/cancellation
- Each phase receives different event context
- All phases are async-compatible

---

## Testing Knowledge Hooks

Test hooks using `evaluateHook` and assertion patterns:

### Unit Test: Basic Hook

```javascript
import { describe, it, expect } from 'vitest';
import { defineHook, evaluateHook, Store } from 'unrdf';

describe('Knowledge Hook Testing', () => {
  it('should validate payload in before phase', async () => {
    const hook = defineHook({
      meta: { name: 'test-hook' },
      when: { kind: 'sparql-ask', query: 'ASK { ?s ?p ?o }' },

      async before({ payload }) {
        if (!payload || !payload.amount) {
          return { cancel: true, reason: 'Missing amount' };
        }
        return payload;
      },

      async run({ payload }) {
        return { result: payload.amount * 2 };
      }
    });

    const store = new Store();

    // Test 1: Invalid payload cancels
    const result1 = await evaluateHook(hook, store, {
      payload: {}
    });
    expect(result1.cancelled).toBe(true);
    expect(result1.reason).toBe('Missing amount');

    // Test 2: Valid payload executes
    const result2 = await evaluateHook(hook, store, {
      payload: { amount: 100 }
    });
    expect(result2.result).toBe(200);
  });

  it('should add RDF quads in run phase', async () => {
    const hook = defineHook({
      meta: { name: 'quad-adder' },
      when: { kind: 'transaction' },

      async run({ payload }) {
        const quad = DataFactory.quad(
          DataFactory.namedNode('http://example.org/item'),
          DataFactory.namedNode('http://schema.org/name'),
          DataFactory.literal('Test Item')
        );

        return {
          result: { itemId: 'item' },
          assertions: [quad]
        };
      }
    });

    const store = new Store();
    const result = await evaluateHook(hook, store);

    expect(result.result.itemId).toBe('item');
    expect(result.output.assertions).toHaveLength(1);
  });

  it('should execute cleanup in after phase', async () => {
    let cleanupRan = false;

    const hook = defineHook({
      meta: { name: 'cleanup-test' },
      when: { kind: 'transaction' },

      async run() {
        return { result: 'executed' };
      },

      async after({ result, cancelled }) {
        if (!cancelled) {
          cleanupRan = true;
        }
      }
    });

    const store = new Store();
    await evaluateHook(hook, store);

    expect(cleanupRan).toBe(true);
  });
});
```

### Integration Test: Multiple Hooks

```javascript
describe('Hook Composition', () => {
  it('should execute hooks in sequence with TransactionManager', async () => {
    const executionLog = [];

    const hook1 = defineHook({
      meta: { name: 'first-hook' },
      when: { kind: 'transaction' },
      async run() {
        executionLog.push('hook1');
        return { result: 'step1' };
      }
    });

    const hook2 = defineHook({
      meta: { name: 'second-hook' },
      when: { kind: 'transaction' },
      async run() {
        executionLog.push('hook2');
        return { result: 'step2' };
      }
    });

    const manager = new TransactionManager();
    manager.addHook(hook1);
    manager.addHook(hook2);

    const store = new Store();
    const delta = {
      additions: [DataFactory.quad(...)],
      removals: []
    };

    const receipt = await manager.apply(store, delta);

    expect(executionLog).toEqual(['hook1', 'hook2']);
    expect(receipt.committed).toBe(true);
  });
});
```

### Testing Error Handling

```javascript
describe('Hook Error Handling', () => {
  it('should handle errors in before phase', async () => {
    const hook = defineHook({
      meta: { name: 'error-test' },
      when: { kind: 'transaction' },

      async before() {
        throw new Error('Validation failed');
      },

      async run() {
        return { result: 'should not reach here' };
      }
    });

    const store = new Store();

    const result = await evaluateHook(hook, store).catch(err => ({
      error: err.message
    }));

    expect(result.error).toBe('Validation failed');
  });

  it('should recover from run phase errors', async () => {
    const hook = defineHook({
      meta: { name: 'error-recovery' },
      when: { kind: 'transaction' },

      async run() {
        throw new Error('Processing failed');
      },

      async after({ error }) {
        return {
          result: {
            status: 'failed',
            errorMessage: error?.message
          }
        };
      }
    });

    const store = new Store();

    try {
      await evaluateHook(hook, store);
    } catch (err) {
      expect(err.message).toContain('Processing failed');
    }
  });
});
```

### Testing Conditions

```javascript
describe('Hook Conditions', () => {
  it('should evaluate SPARQL ASK condition', async () => {
    const hook = defineHook({
      meta: { name: 'ask-test' },
      when: {
        kind: 'sparql-ask',
        query: 'ASK { ?s ?p ?o }'
      },
      async run() {
        return { result: 'condition satisfied' };
      }
    });

    const store = new Store();
    store.addQuad(DataFactory.quad(
      DataFactory.namedNode('http://example.org/s'),
      DataFactory.namedNode('http://example.org/p'),
      DataFactory.namedNode('http://example.org/o')
    ));

    const result = await evaluateHook(hook, store);
    expect(result.satisfied).toBe(true);
    expect(result.output.result).toBe('condition satisfied');
  });

  it('should handle unsatisfied conditions', async () => {
    const hook = defineHook({
      meta: { name: 'unmet-ask' },
      when: {
        kind: 'sparql-ask',
        query: 'ASK { ?s ?p ?o }' // Will be false on empty store
      },
      async run() {
        return { result: 'should not execute' };
      }
    });

    const store = new Store(); // Empty store

    const result = await evaluateHook(hook, store);
    expect(result.satisfied).toBe(false);
    expect(result.output).toBeUndefined(); // run didn't execute
  });
});
```

---

## Error Handling Patterns

### Pattern 1: Graceful Degradation

```javascript
const resilientHook = defineHook({
  meta: { name: 'fault-tolerant' },
  when: { kind: 'transaction' },

  async before({ payload }) {
    try {
      const validated = await externalValidator(payload);
      return validated;
    } catch (err) {
      // Log but don't cancel
      console.warn('Validation service unavailable:', err);
      return payload; // Proceed with unvalidated data
    }
  },

  async run({ payload }) {
    try {
      const result = await processPayload(payload);
      return { result };
    } catch (err) {
      // Fall back to identity
      return { result: payload };
    }
  }
});
```

### Pattern 2: Explicit Error Boundaries

```javascript
const failFastHook = defineHook({
  meta: { name: 'strict-validation' },
  when: { kind: 'transaction' },

  async before({ payload }) {
    // Validate strictly - fail on any issue
    const errors = validateStrict(payload);
    if (errors.length > 0) {
      return {
        cancel: true,
        reason: `Validation errors: ${errors.join(', ')}`
      };
    }
    return payload;
  }
});
```

### Pattern 3: Transactional Rollback

```javascript
const transactionalHook = defineHook({
  meta: { name: 'atomic-ops' },
  when: { kind: 'transaction' },

  async run({ payload, store }) {
    // Create savepoint
    const savepoint = store.getQuads().slice();

    try {
      // Perform operations
      const operations = await complexOperation(store, payload);

      return {
        result: { operations },
        assertions: []
      };
    } catch (err) {
      // Rollback to savepoint
      store.clear();
      savepoint.forEach(quad => store.addQuad(quad));

      throw new Error(`Operation failed, rolled back: ${err.message}`);
    }
  }
});
```

---

## Related Guides

- [How-To: Handle Transactions](./handle-transactions.md) - TransactionManager integration
- [How-To: Validate RDF Data](./validate-rdf-data.md) - Validation hooks
- [How-To: Use Hooks in React](./use-hooks-in-react.md) - React integration
- [Tutorial: Knowledge Hooks](../tutorials/02-knowledge-hooks.md) - Complete walkthrough
- [Explanation: Knowledge Hooks](../explanation/concepts/knowledge-hooks.md) - Hook philosophy
