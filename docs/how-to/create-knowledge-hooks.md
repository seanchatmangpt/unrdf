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

## Related Guides

- [How-To: Handle Transactions](./handle-transactions.md) - TransactionManager integration
- [How-To: Validate RDF Data](./validate-rdf-data.md) - Validation hooks
- [Tutorial: Knowledge Hooks](../tutorials/02-knowledge-hooks.md) - Complete walkthrough
- [Explanation: Knowledge Hooks](../explanation/concepts/knowledge-hooks.md) - Hook philosophy
