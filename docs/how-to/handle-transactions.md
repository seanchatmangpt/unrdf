# How-To: Handle Transactions

**Problem**: You need to apply atomic changes to RDF graphs with validation, hooks, and rollback capabilities.

## Solution

Use `TransactionManager` to coordinate hook-driven transactions. All changes apply atomically - either all succeed or none apply.

### Basic Transaction

Apply delta with additions and removals:

```javascript
import { TransactionManager, parseTurtle, DataFactory } from 'unrdf';

const txManager = new TransactionManager();
const store = parseTurtle(`
  @prefix ex: <http://example.org/> .
  ex:alice a ex:Person .
`);

// Define delta
const delta = {
  additions: [
    DataFactory.quad(
      DataFactory.namedNode('http://example.org/alice'),
      DataFactory.namedNode('http://schema.org/name'),
      DataFactory.literal('Alice')
    )
  ],
  removals: []
};

// Apply transaction
await txManager.apply(store, delta);

console.log(`Store now has ${store.size} quads`);
```

### Transaction with Validation

Add hook for pre-transaction validation:

```javascript
import { TransactionManager, validateShacl, getValidationErrors } from 'unrdf';

const txManager = new TransactionManager();

// Add validation hook
txManager.addHook({
  async before(event) {
    const shapesTtl = `
      @prefix sh: <http://www.w3.org/ns/shacl#> .
      @prefix schema: <http://schema.org/> .

      ex:PersonShape a sh:NodeShape ;
        sh:targetClass schema:Person ;
        sh:property [ sh:path schema:name ; sh:minCount 1 ] .
    `;

    const report = await validateShacl(event.store, shapesTtl);

    if (!report.conforms) {
      const errors = getValidationErrors(report);
      throw new Error(`SHACL violation: ${errors[0].message}`);
    }

    return { validated: true };
  }
});

// Apply transaction - validation runs automatically
try {
  await txManager.apply(store, delta);
  console.log('✓ Transaction committed');
} catch (err) {
  console.error('✗ Transaction rejected:', err.message);
}
```

### Multiple Hooks

Chain hooks for complex workflows:

```javascript
import { TransactionManager, LockchainWriter } from 'unrdf';

const txManager = new TransactionManager();
const lockchain = new LockchainWriter();

// Hook 1: Validation
txManager.addHook({
  async before(event) {
    const report = await validateShacl(event.store, shapesTtl);
    if (!report.conforms) {
      throw new Error('Validation failed');
    }
  }
});

// Hook 2: Computed properties
txManager.addHook({
  async run(event) {
    // Add derived data
    const { store } = event;
    const results = select(store, `
      SELECT ?person ?firstName ?lastName WHERE {
        ?person schema:givenName ?firstName ;
                schema:familyName ?lastName .
        FILTER NOT EXISTS { ?person schema:name ?fullName }
      }
    `);

    results.forEach(row => {
      const fullName = `${row.firstName.value} ${row.lastName.value}`;
      store.addQuad(/* create fullName quad */);
    });
  }
});

// Hook 3: Audit logging
txManager.addHook({
  async after(result) {
    if (result.success) {
      await lockchain.append({
        timestamp: Date.now(),
        delta: result.event.delta,
        metadata: { userId: 'system' }
      });
    }
  }
});

// Apply - all hooks execute in order
await txManager.apply(store, delta);
```

### Transaction Rollback

Hooks can abort transactions:

```javascript
txManager.addHook({
  async before(event) {
    // Check business rules
    const highValueChanges = event.delta.additions.filter(quad =>
      quad.predicate.value === 'http://example.org/amount' &&
      parseInt(quad.object.value) > 10000
    );

    if (highValueChanges.length > 0) {
      // Abort transaction
      throw new Error('High-value changes require approval');
    }

    return { approved: true };
  }
});

// Transaction will rollback on exception
try {
  await txManager.apply(store, delta);
} catch (err) {
  console.log('Transaction aborted:', err.message);
  // Store unchanged
}
```

### Transaction Metadata

Pass context to hooks:

```javascript
const metadata = {
  userId: 'alice@example.org',
  timestamp: Date.now(),
  source: 'web-ui',
  reason: 'user-requested-update'
};

// Hooks receive metadata
txManager.addHook({
  async before(event) {
    console.log(`Transaction by: ${event.metadata.userId}`);

    // Authorization check
    if (!isAuthorized(event.metadata.userId, event.delta)) {
      throw new Error('Unauthorized');
    }
  }
});

// Apply with metadata
await txManager.apply(store, delta, metadata);
```

### Transaction Statistics

Track transaction metrics:

```javascript
const txManager = new TransactionManager();

// Apply multiple transactions
await txManager.apply(store, delta1);
await txManager.apply(store, delta2);
await txManager.apply(store, delta3);

// Get statistics
const stats = txManager.getStats();
console.log(`Total transactions: ${stats.totalTransactions}`);
console.log(`Successful: ${stats.successfulTransactions}`);
console.log(`Failed: ${stats.failedTransactions}`);
console.log(`Total quads added: ${stats.totalQuadsAdded}`);
console.log(`Total quads removed: ${stats.totalQuadsRemoved}`);
console.log(`Average duration: ${stats.averageDuration}ms`);
```

### Knowledge Hooks Integration

Use `defineHook()` with TransactionManager:

```javascript
import { defineHook, registerHook, TransactionManager } from 'unrdf';

// Define Knowledge Hook
const validationHook = defineHook({
  meta: { name: 'tx-validator' },
  channel: { graphs: ['*'] },
  when: { kind: 'transaction' },

  async before(event) {
    const report = await validateShacl(event.store, shapesTtl);
    if (!report.conforms) {
      throw new Error('Invalid data');
    }
  }
});

// Register globally
registerHook(validationHook);

// TransactionManager will auto-invoke registered hooks
const txManager = new TransactionManager();
await txManager.apply(store, delta);
```

## Variations

### Batch Transactions

Apply multiple deltas efficiently:

```javascript
async function batchTransactions(store, deltas) {
  const txManager = new TransactionManager();

  // Add validation once
  txManager.addHook(validationHook);

  // Apply all deltas
  const results = await Promise.all(
    deltas.map(delta => txManager.apply(store, delta))
  );

  const successful = results.filter(r => r.success).length;
  console.log(`${successful}/${deltas.length} transactions committed`);

  return results;
}
```

### Conditional Transactions

Apply delta only if condition met:

```javascript
async function conditionalTransaction(store, delta, condition) {
  const txManager = new TransactionManager();

  txManager.addHook({
    async before(event) {
      const conditionMet = ask(event.store, condition);

      if (!conditionMet) {
        throw new Error('Condition not satisfied');
      }
    }
  });

  return await txManager.apply(store, delta);
}

// Apply only if Alice exists
await conditionalTransaction(store, delta, `
  ASK { ex:alice a schema:Person }
`);
```

### Two-Phase Commit

Simulate distributed transactions:

```javascript
class TwoPhaseTransactionManager {
  constructor() {
    this.prepared = new Map();
  }

  async prepare(store, delta, txId) {
    // Phase 1: Prepare
    const validation = await validateShacl(store, shapesTtl);

    if (validation.conforms) {
      this.prepared.set(txId, { store, delta, validated: true });
      return { prepared: true };
    } else {
      return { prepared: false, error: 'Validation failed' };
    }
  }

  async commit(txId) {
    // Phase 2: Commit
    const tx = this.prepared.get(txId);

    if (!tx || !tx.validated) {
      throw new Error('Transaction not prepared');
    }

    const txManager = new TransactionManager();
    await txManager.apply(tx.store, tx.delta);

    this.prepared.delete(txId);
    return { committed: true };
  }

  abort(txId) {
    this.prepared.delete(txId);
  }
}
```

### Delta Construction

Build deltas from SPARQL results:

```javascript
import { select, DataFactory } from 'unrdf';

function createDeltaFromQuery(store, selectQuery, updateFn) {
  const results = select(store, selectQuery);

  const additions = [];
  const removals = [];

  results.forEach(row => {
    const updates = updateFn(row);

    if (updates.add) {
      additions.push(...updates.add);
    }

    if (updates.remove) {
      removals.push(...updates.remove);
    }
  });

  return { additions, removals };
}

// Example: Update ages
const delta = createDeltaFromQuery(
  store,
  `SELECT ?person ?age WHERE { ?person schema:age ?age }`,
  (row) => ({
    remove: [/* old age quad */],
    add: [/* new age quad */]
  })
);

await txManager.apply(store, delta);
```

## Related Guides

- [How-To: Create Knowledge Hooks](./create-knowledge-hooks.md) - Hook definition
- [How-To: Validate RDF Data](./validate-rdf-data.md) - Validation hooks
- [Tutorial: Transactions](../tutorials/03-transactions.md) - Complete walkthrough
- [Explanation: Transactions](../explanation/concepts/transactions.md) - Transaction model
