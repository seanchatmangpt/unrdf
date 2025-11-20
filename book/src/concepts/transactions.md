# Transactions

UNRDF provides **atomic transactions** with ACID guarantees, cryptographic provenance, and Knowledge Hooks integration. This chapter explains how to use transactions to ensure data integrity and auditability in your RDF applications.

## What are Transactions?

Transactions are **atomic operations** that either completely succeed or completely fail—no partial updates. In UNRDF, transactions provide:

1. **Atomicity** - All changes succeed or all fail
2. **Consistency** - Maintains data integrity via hooks
3. **Isolation** - Concurrent transactions don't interfere
4. **Durability** - Changes are permanent and auditable
5. **Provenance** - Cryptographic proof of what changed and when

## Transaction Lifecycle

A transaction goes through several phases:

```text
1. Create Delta (additions + removals)
       ↓
2. Execute Pre-Hooks (validation)
       ↓
3. Apply Changes to Store
       ↓
4. Execute Post-Hooks (side effects)
       ↓
5. Generate Receipt (cryptographic proof)
       ↓
6. Write to Lockchain (optional audit trail)
```

## Basic Transaction Usage

### Creating a Transaction Manager

```javascript
import { TransactionManager } from 'unrdf';
import { Store } from 'n3';

// Create transaction manager
const txManager = new TransactionManager({
  maxHooks: 100,
  enableLockchain: true,
  enableResolution: true,
  observability: {
    enableOTEL: true
  }
});

// Create a store
const store = new Store();

// Define a delta (changes)
const delta = {
  additions: [
    quad(
      namedNode('http://example.org/alice'),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal('Alice Smith')
    )
  ],
  removals: []
};

// Apply transaction
const result = await txManager.apply(store, delta, {
  actor: 'alice@example.org',
  skipHooks: false,
  timeoutMs: 5000
});

console.log('Committed:', result.receipt.committed);
console.log('Transaction ID:', result.receipt.id);
console.log('Duration:', result.receipt.durationMs, 'ms');
```

**Source Reference** (`src/knowledge-engine/transaction.mjs:88-140`)

## Deltas: Additions and Removals

A **delta** represents the changes in a transaction:

```javascript
import { namedNode, literal, quad } from '@rdfjs/data-model';

const delta = {
  additions: [
    // Add new person
    quad(
      namedNode('http://example.org/bob'),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal('Bob Jones')
    ),
    quad(
      namedNode('http://example.org/bob'),
      namedNode('http://xmlns.com/foaf/0.1/age'),
      literal('28', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
    )
  ],
  removals: [
    // Remove old data
    quad(
      namedNode('http://example.org/alice'),
      namedNode('http://xmlns.com/foaf/0.1/age'),
      literal('29', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
    )
  ]
};
```

### Delta from Store Comparison

Use `useDelta()` composable to generate deltas:

```javascript
import { initStore, useDelta, useTurtle } from 'unrdf';

const runApp = initStore();

runApp(() => {
  const delta = useDelta();
  const turtle = useTurtle();

  // Load current state
  turtle.parse(`
    @prefix ex: <http://example.org/> .
    ex:alice ex:age 29 .
  `, { addToStore: true });

  // Parse new state
  const newData = turtle.parse(`
    @prefix ex: <http://example.org/> .
    ex:alice ex:age 30 .
    ex:bob ex:age 28 .
  `);

  // Generate delta
  const changes = delta.compareWith(newData);

  console.log('Delta:');
  console.log('- Added:', changes.addedCount, 'quads');
  console.log('- Removed:', changes.removedCount, 'quads');

  // Use in transaction
  // const result = await txManager.apply(store, changes);
});
```

## Transaction Receipts

Every transaction generates a **receipt** with complete audit information:

```javascript
const result = await txManager.apply(store, delta, {
  actor: 'alice@example.org'
});

const receipt = result.receipt;

console.log('Receipt:');
console.log('- ID:', receipt.id);                    // UUID
console.log('- Timestamp:', receipt.timestamp);      // Unix timestamp
console.log('- Actor:', receipt.actor);              // Who made the change
console.log('- Committed:', receipt.committed);      // Success/failure
console.log('- Duration:', receipt.durationMs);      // Execution time

// Cryptographic hashes
console.log('- Store Hash (SHA3):', receipt.storeHash.sha3);
console.log('- Store Hash (BLAKE3):', receipt.storeHash.blake3);
console.log('- Before Hash:', receipt.beforeHash);
console.log('- After Hash:', receipt.afterHash);

// Delta information
console.log('- Added Quads:', receipt.delta.additions.length);
console.log('- Removed Quads:', receipt.delta.removals.length);

// Hook execution
console.log('- Hook Results:', receipt.hookResults);
console.log('- Hook Errors:', receipt.hookErrors);
```

### Receipt Structure

```typescript
{
  id: string,              // Transaction UUID
  timestamp: number,       // Unix timestamp (ms)
  durationMs: number,      // Execution time
  actor: string,           // Who executed the transaction
  committed: boolean,      // Success/failure
  delta: {
    additions: Quad[],     // Quads added
    removals: Quad[]       // Quads removed
  },
  beforeHash: {
    sha3: string,          // Store hash before transaction
    blake3: string
  },
  afterHash: {
    sha3: string,          // Store hash after transaction
    blake3: string
  },
  storeHash: {
    sha3: string,          // Alias for afterHash
    blake3: string
  },
  hookResults: HookResult[], // Hook execution results
  hookErrors: string[]       // Hook errors
}
```

## Transaction Hooks

Hooks enable **policy-driven transactions** with validation and side effects:

### Hook Types

1. **Pre-Hooks** - Execute **before** changes (validation, veto)
2. **Post-Hooks** - Execute **after** changes (notifications, side effects)

### Hook Effects

- **`veto`** - Block the transaction if condition fails
- **`log`** - Log information
- **`notify`** - Send notifications
- **`custom`** - Custom side effects

### Adding Hooks

```javascript
const txManager = new TransactionManager();

// Pre-hook: Veto transactions that add "Eve"
txManager.addHook({
  id: 'no-eve',
  mode: 'pre',
  condition: async (store, delta) => {
    // Return true to allow, false to veto
    const hasEve = delta.additions.some(q =>
      q.object.value && q.object.value.includes('Eve')
    );
    return !hasEve;
  },
  effect: 'veto',
  metadata: {
    description: 'Prevent adding Eve to the database',
    severity: 'high'
  }
});

// Post-hook: Log all transactions
txManager.addHook({
  id: 'audit-log',
  mode: 'post',
  condition: async () => true, // Always execute
  effect: 'log',
  metadata: {
    description: 'Audit trail for all transactions'
  }
});

// Try to add Eve (will be vetoed)
const delta = {
  additions: [
    quad(
      namedNode('http://example.org/eve'),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal('Eve')
    )
  ],
  removals: []
};

const result = await txManager.apply(store, delta);

console.log('Committed:', result.receipt.committed);  // false
console.log('Hook results:', result.receipt.hookResults);
```

**Source Reference** (`src/knowledge-engine/transaction.mjs:155-170`)

### Hook Lifecycle

```javascript
// Pre-hook lifecycle
1. Transaction starts
2. Pre-hooks execute in order
3. If any pre-hook vetoes → transaction fails
4. If all pre-hooks pass → continue to apply changes

// Post-hook lifecycle
1. Changes applied to store
2. Post-hooks execute in order
3. Post-hook failures are logged but don't block transaction
4. Receipt generated with hook results
```

### Managing Hooks

```javascript
// Add hook
txManager.addHook({
  id: 'validation-hook',
  mode: 'pre',
  condition: async (store, delta) => {
    // Validation logic
    return true;
  },
  effect: 'veto'
});

// Remove hook
const removed = txManager.removeHook('validation-hook');
console.log('Hook removed:', removed);

// Get all hooks
const hooks = txManager.getHooks();
console.log('Registered hooks:', hooks.length);

// Clear all hooks
txManager.clearHooks();
```

## Cryptographic Hashing

UNRDF uses **dual hashing** for maximum security:

1. **SHA3-256** - NIST-approved cryptographic hash
2. **BLAKE3** - High-performance cryptographic hash

### Hash Generation

```javascript
import { canonicalize } from 'unrdf';
import { sha3_256 } from '@noble/hashes/sha3.js';
import { blake3 } from '@noble/hashes/blake3.js';
import { utf8ToBytes, bytesToHex } from '@noble/hashes/utils.js';

async function hashStore(store) {
  // Canonicalize store (deterministic ordering)
  const canonical = await canonicalize(store);

  // Convert to bytes
  const bytes = utf8ToBytes(canonical);

  // Generate hashes
  const sha3Hash = bytesToHex(sha3_256(bytes));
  const blake3Hash = bytesToHex(blake3(bytes));

  return {
    sha3: sha3Hash,
    blake3: blake3Hash
  };
}

const hashes = await hashStore(store);
console.log('SHA3:', hashes.sha3);
console.log('BLAKE3:', hashes.blake3);
```

**Source Reference** (`src/knowledge-engine/transaction.mjs:60-82`)

### Why Dual Hashing?

- **SHA3-256**: NIST standard, widely recognized, cryptographically secure
- **BLAKE3**: Faster, modern, resistant to length extension attacks
- **Together**: Defense in depth, future-proof against cryptanalysis

## Lockchain: Git-Based Audit Trail

The **Lockchain** provides immutable, verifiable audit trails using Git:

### Basic Lockchain Usage

```javascript
import { LockchainWriter } from 'unrdf';

const lockchain = new LockchainWriter({
  gitRepo: './audit-trail',
  refName: 'refs/notes/lockchain',
  enableMerkle: true,
  enableGitAnchoring: true,
  batchSize: 10
});

// Initialize lockchain
await lockchain.init();

// Write receipt
const entry = await lockchain.writeReceipt(receipt, {
  merkleRoot: '...'
});

console.log('Lockchain entry:', entry.id);
console.log('Signature:', entry.signature);
console.log('Merkle root:', entry.merkleRoot);

// Batch commit to Git
const commit = await lockchain.commitBatch();
console.log('Committed to Git:', commit.commitHash);

// Verify receipt
const isValid = await lockchain.verifyReceipt(entry.id);
console.log('Receipt valid:', isValid);
```

**Source Reference** (`src/knowledge-engine/lockchain-writer.mjs:54-262`)

### Lockchain Features

1. **Git Anchoring** - Receipts stored in Git refs
2. **Merkle Trees** - Batch verification via merkle roots
3. **Cryptographic Signatures** - Ed25519, ECDSA, or RSA
4. **Tamper Detection** - Verify integrity of entire chain
5. **Batch Commits** - Efficient Git operations

### Lockchain Entry Structure

```typescript
{
  id: string,              // UUID
  timestamp: number,       // Unix timestamp
  receipt: Receipt,        // Transaction receipt
  signature: {
    algorithm: string,     // 'ed25519', 'ecdsa', 'rsa'
    value: string,         // Signature bytes (hex)
    publicKey?: string     // Optional public key
  },
  previousHash?: string,   // Previous entry hash (chain)
  merkleRoot?: string,     // Merkle root for batch
  gitCommit?: string,      // Git commit SHA
  gitRef?: string          // Git ref name
}
```

### Verifying Lockchain Entries

```javascript
// Verify single entry
const result = await lockchain.verifyEntry(entryId);

if (result.valid) {
  console.log('✓ Entry is valid');
  console.log('- Signature verified');
  console.log('- Git commit verified');
  console.log('- Merkle root verified');
} else {
  console.error('✗ Entry invalid:', result.error);
}

// Verify receipt (simplified)
const isValid = await lockchain.verifyReceipt(receipt);
console.log('Receipt valid:', isValid);
```

## Transaction Patterns

### Pattern 1: Simple Data Update

```javascript
import { TransactionManager } from 'unrdf';
import { Store, namedNode, literal, quad } from 'n3';

const txManager = new TransactionManager();
const store = new Store();

// Update person's age
const delta = {
  additions: [
    quad(
      namedNode('http://example.org/alice'),
      namedNode('http://xmlns.com/foaf/0.1/age'),
      literal('31', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
    )
  ],
  removals: [
    quad(
      namedNode('http://example.org/alice'),
      namedNode('http://xmlns.com/foaf/0.1/age'),
      literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
    )
  ]
};

const result = await txManager.apply(store, delta, {
  actor: 'system',
  timeoutMs: 5000
});

console.log('Update committed:', result.receipt.committed);
```

### Pattern 2: Validated Transaction

```javascript
const txManager = new TransactionManager();

// Add validation hook
txManager.addHook({
  id: 'age-validation',
  mode: 'pre',
  condition: async (store, delta) => {
    // Validate that age is >= 0 and <= 150
    for (const quad of delta.additions) {
      if (quad.predicate.value === 'http://xmlns.com/foaf/0.1/age') {
        const age = parseInt(quad.object.value);
        if (age < 0 || age > 150) {
          return false; // Veto
        }
      }
    }
    return true; // Allow
  },
  effect: 'veto',
  metadata: {
    description: 'Validate age is between 0 and 150'
  }
});

// Valid transaction
const validDelta = {
  additions: [
    quad(
      namedNode('http://example.org/bob'),
      namedNode('http://xmlns.com/foaf/0.1/age'),
      literal('28', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
    )
  ],
  removals: []
};

const result1 = await txManager.apply(store, validDelta);
console.log('Valid transaction committed:', result1.receipt.committed); // true

// Invalid transaction (will be vetoed)
const invalidDelta = {
  additions: [
    quad(
      namedNode('http://example.org/charlie'),
      namedNode('http://xmlns.com/foaf/0.1/age'),
      literal('200', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
    )
  ],
  removals: []
};

const result2 = await txManager.apply(store, invalidDelta);
console.log('Invalid transaction committed:', result2.receipt.committed); // false
```

### Pattern 3: Audited Transaction

```javascript
const txManager = new TransactionManager({
  enableLockchain: true,
  lockchainConfig: {
    gitRepo: './audit-trail',
    enableMerkle: true,
    batchSize: 10
  }
});

// Transaction with full audit trail
const delta = {
  additions: [
    quad(
      namedNode('http://example.org/alice'),
      namedNode('http://xmlns.com/foaf/0.1/salary'),
      literal('75000', namedNode('http://www.w3.org/2001/XMLSchema#decimal'))
    )
  ],
  removals: []
};

const result = await txManager.apply(store, delta, {
  actor: 'payroll@example.org'
});

// Receipt automatically written to lockchain
console.log('Transaction ID:', result.receipt.id);
console.log('Actor:', result.receipt.actor);
console.log('Timestamp:', new Date(result.receipt.timestamp));

// Later: Verify the transaction
const lockchain = txManager.lockchainWriter;
const isValid = await lockchain.verifyReceipt(result.receipt.id);
console.log('Audit trail valid:', isValid);
```

### Pattern 4: Multi-Step Transaction

```javascript
import { useDelta, useStoreContext, initStore } from 'unrdf';

const runApp = initStore();

runApp(async () => {
  const ctx = useStoreContext();
  const delta = useDelta();

  // Step 1: Load data
  // ... load initial data ...

  // Step 2: Make changes
  const newData = // ... parse new data ...
  const changes = delta.compareWith(newData);

  // Step 3: Execute transaction
  const txManager = new TransactionManager();
  const result = await txManager.apply(ctx.store, changes, {
    actor: 'migration@example.org'
  });

  if (!result.receipt.committed) {
    console.error('Transaction failed');
    return;
  }

  // Step 4: Verify
  console.log('✓ Transaction committed');
  console.log('- Added:', result.receipt.delta.additions.length);
  console.log('- Removed:', result.receipt.delta.removals.length);
  console.log('- Hash:', result.receipt.afterHash.sha3);
});
```

## Error Handling

```javascript
const txManager = new TransactionManager();

try {
  const result = await txManager.apply(store, delta, {
    actor: 'alice@example.org',
    timeoutMs: 5000
  });

  if (!result.receipt.committed) {
    console.error('Transaction failed:');
    console.error('- Hook errors:', result.receipt.hookErrors);
    console.error('- Hook results:', result.receipt.hookResults);
    return;
  }

  console.log('✓ Transaction successful');
} catch (error) {
  if (error.message.includes('timeout')) {
    console.error('Transaction timed out');
  } else if (error.message.includes('validation')) {
    console.error('Validation error:', error.message);
  } else {
    console.error('Transaction error:', error.message);
  }
}
```

## Rollback Strategies

```javascript
import { useDelta } from 'unrdf';

const runApp = initStore();

runApp(async () => {
  const delta = useDelta();

  // Execute transaction
  const result = await txManager.apply(store, changes);

  if (!result.receipt.committed) {
    console.error('Transaction failed - no rollback needed');
    return;
  }

  // Later: Rollback by inverting the delta
  const inverted = delta.invert(result.receipt.delta);

  const rollbackResult = await txManager.apply(store, inverted, {
    actor: 'system-rollback'
  });

  console.log('Rollback committed:', rollbackResult.receipt.committed);
});
```

## Performance Considerations

### Transaction Timeout

```javascript
const result = await txManager.apply(store, delta, {
  timeoutMs: 10000  // 10 seconds
});
```

### Hook Limit

```javascript
const txManager = new TransactionManager({
  maxHooks: 50  // Limit number of hooks
});
```

### Batch Lockchain Commits

```javascript
const lockchain = new LockchainWriter({
  batchSize: 20  // Commit every 20 receipts
});

// Receipts accumulate in memory
await lockchain.writeReceipt(receipt1);
await lockchain.writeReceipt(receipt2);
// ... 18 more ...

// Auto-commits when batch size reached
await lockchain.writeReceipt(receipt20);  // Triggers Git commit

// Or manually commit
await lockchain.commitBatch();
```

### Fast Hashing

```javascript
// Skip canonicalization for performance (less secure)
const hashes = await hashStore(store, {
  afterHashOnly: true  // Skip canonicalization
});
```

## Best Practices

### 1. Always Specify Actor

```javascript
// ✅ Good: Track who made the change
const result = await txManager.apply(store, delta, {
  actor: 'alice@example.org'
});

// ❌ Avoid: No accountability
const result = await txManager.apply(store, delta);
```

### 2. Use Hooks for Validation

```javascript
// ✅ Good: Centralized validation
txManager.addHook({
  id: 'data-validation',
  mode: 'pre',
  condition: validateData,
  effect: 'veto'
});

// ❌ Avoid: Manual validation everywhere
if (!isValid(delta)) {
  throw new Error('Invalid');
}
```

### 3. Enable Lockchain for Audit

```javascript
// ✅ Good: Full audit trail
const txManager = new TransactionManager({
  enableLockchain: true
});

// ❌ Avoid: No audit trail for critical data
const txManager = new TransactionManager();
```

### 4. Handle Errors Gracefully

```javascript
// ✅ Good: Comprehensive error handling
try {
  const result = await txManager.apply(store, delta);
  if (!result.receipt.committed) {
    console.log('Transaction vetoed:', result.receipt.hookErrors);
  }
} catch (error) {
  console.error('Transaction failed:', error.message);
}

// ❌ Avoid: Ignoring failures
await txManager.apply(store, delta);  // No error handling
```

## Summary

- **Transactions** provide ACID guarantees for RDF operations
- **Deltas** define additions and removals
- **Receipts** provide cryptographic proof of changes
- **Hooks** enable policy-driven validation and side effects
- **Lockchain** provides Git-based immutable audit trails
- **Dual Hashing** (SHA3 + BLAKE3) ensures integrity
- **Error Handling** prevents data corruption

Transactions are essential for building reliable, auditable RDF applications with UNRDF.

## Next Steps

- **[Store Context](./store-context.md)** - Review context management
- **[Composables](./composables.md)** - Learn about composable functions
- **[RDF Fundamentals](./rdf-fundamentals.md)** - RDF basics
- **Knowledge Hooks Guide** - Policy-driven automation (coming soon)
