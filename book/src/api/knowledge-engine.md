# Knowledge Engine API Reference

The Knowledge Engine provides transaction management, knowledge hooks, policy packs, and cryptographic provenance for RDF graphs.

## Transaction Manager

### `TransactionManager(options)`

Create a transaction manager for atomic RDF graph operations with hooks and receipts.

**Parameters:**
- `options` (Object, optional): Manager options
  - `maxHooks` (number): Maximum hooks allowed. Default: 100
  - `strictMode` (boolean): Enable strict error handling. Default: false
  - `afterHashOnly` (boolean): Skip canonicalization for performance. Default: false
  - `enableLockchain` (boolean): Enable lockchain writer. Default: false
  - `enableResolution` (boolean): Enable resolution layer. Default: false
  - `observability` (Object): Observability configuration

**Returns:** `TransactionManager` - New transaction manager instance

**Example:**
```javascript
import { TransactionManager } from 'unrdf/knowledge-engine';

const tx = new TransactionManager({
  maxHooks: 50,
  strictMode: true,
  enableLockchain: true
});
```

---

### Transaction Manager Methods

#### `addHook(hook)`

Register a transaction hook.

**Parameters:**
- `hook` (Object): Hook definition
  - `id` (string): Unique hook identifier
  - `mode` (string): Hook mode ('pre' or 'post')
  - `condition` (Function): Async condition function `(store, delta) => boolean`
  - `effect` (string|Function): Effect ('veto' or function)

**Throws:**
- `Error` - If hook is invalid or limit exceeded
- `Error` - If hook with same ID already exists

**Example:**
```javascript
tx.addHook({
  id: 'no-eve',
  mode: 'pre',
  condition: async (store, delta) => {
    // Veto if any addition mentions "eve"
    return !delta.additions.some(q =>
      q.object.value && q.object.value.includes('eve')
    );
  },
  effect: 'veto'
});
```

---

#### `removeHook(hookId)`

Remove a hook by ID.

**Parameters:**
- `hookId` (string): Hook identifier to remove

**Returns:** `boolean` - True if hook was removed, false if not found

**Example:**
```javascript
const removed = tx.removeHook('no-eve');
console.log('Hook removed:', removed);
```

---

#### `getHooks()`

Get all registered hooks.

**Returns:** `Array<Hook>` - Array of hook definitions

**Example:**
```javascript
const hooks = tx.getHooks();
console.log(`${hooks.length} hooks registered`);
```

---

#### `clearHooks()`

Clear all hooks.

**Example:**
```javascript
tx.clearHooks();
```

---

#### `apply(store, delta, options)`

Apply a transaction with hooks and generate receipt.

**Parameters:**
- `store` (Store): The store to apply the transaction to
- `delta` (Object): The delta to apply
  - `additions` (Array<Quad>): Quads to add
  - `removals` (Array<Quad>): Quads to remove
- `options` (Object, optional): Transaction options
  - `actor` (string): Actor identifier
  - `skipHooks` (boolean): Skip hook execution. Default: false
  - `timeoutMs` (number): Transaction timeout. Default: 30000

**Returns:** `Promise<Object>` - Transaction result
- `store` (Store): Updated store
- `receipt` (Receipt): Transaction receipt

**Throws:** `Error` - If transaction fails

**Example:**
```javascript
import { Store, DataFactory } from 'n3';

const store = new Store();
const { namedNode, quad } = DataFactory;

const delta = {
  additions: [
    quad(
      namedNode('http://example.org/alice'),
      namedNode('http://xmlns.com/foaf/0.1/knows'),
      namedNode('http://example.org/bob')
    )
  ],
  removals: []
};

const result = await tx.apply(store, delta, {
  actor: 'system',
  timeoutMs: 10000
});

console.log('Committed:', result.receipt.committed);
console.log('Duration:', result.receipt.durationMs + 'ms');
console.log('Hook results:', result.receipt.hookResults);
```

---

#### `createSession(initialStore, sessionOptions)`

Create a transaction session for batch operations.

**Parameters:**
- `initialStore` (Store): Initial store state
- `sessionOptions` (Object, optional): Session options

**Returns:** `Object` - Transaction session
- `addDelta(delta)`: Add a delta to the session
- `applyAll(options)`: Apply all deltas
- `getCurrentStore()`: Get current store state
- `getReceipts()`: Get all receipts
- `reset()`: Reset session
- `getStats()`: Get session statistics

**Example:**
```javascript
const session = tx.createSession(store);

// Add multiple deltas
session.addDelta(delta1);
session.addDelta(delta2);
session.addDelta(delta3);

// Apply all at once
const receipts = await session.applyAll({ actor: 'batch-processor' });

// Get session stats
const stats = session.getStats();
console.log(`Success rate: ${stats.successRate * 100}%`);

// Get final state
const finalStore = session.getCurrentStore();
```

---

#### `getStats()`

Get transaction manager statistics.

**Returns:** `Object` - Manager statistics
- `totalHooks` (number): Total number of hooks
- `preHooks` (number): Number of pre-transaction hooks
- `postHooks` (number): Number of post-transaction hooks
- `maxHooks` (number): Maximum hooks allowed
- `strictMode` (boolean): Strict mode enabled
- `lockchain` (Object, optional): Lockchain statistics
- `performance` (Object): Performance metrics

**Example:**
```javascript
const stats = tx.getStats();
console.log(`Hooks: ${stats.totalHooks}/${stats.maxHooks}`);
console.log(`Pre: ${stats.preHooks}, Post: ${stats.postHooks}`);
```

---

#### `commitLockchain()`

Commit pending lockchain entries (if lockchain enabled).

**Returns:** `Promise<Object>` - Commit result

**Throws:** `Error` - If lockchain is not enabled

**Example:**
```javascript
if (tx.lockchainWriter) {
  const result = await tx.commitLockchain();
  console.log('Lockchain committed:', result);
}
```

---

### Transaction Receipts

Transaction receipts provide comprehensive metadata about each transaction:

```javascript
{
  id: 'uuid',                      // Transaction UUID
  timestamp: 1234567890,           // Unix timestamp
  durationMs: 42,                  // Duration in milliseconds
  actor: 'system',                 // Actor identifier
  committed: true,                 // Success flag
  delta: {                         // Applied delta
    additions: [...],
    removals: [...]
  },
  hookResults: [                   // Hook execution results
    {
      hookId: 'no-eve',
      mode: 'pre',
      result: true
    }
  ],
  hookErrors: [],                  // Hook errors (if any)
  beforeHash: {                    // Pre-transaction hash
    sha3: '...',
    blake3: '...'
  },
  afterHash: {                     // Post-transaction hash
    sha3: '...',
    blake3: '...'
  },
  error: null                      // Error message (if failed)
}
```

---

### `printReceipt(receipt, options)`

Print a receipt in a consistent format.

**Parameters:**
- `receipt` (Receipt): The receipt to print
- `options` (Object, optional): Print options
  - `verbose` (boolean): Include detailed information. Default: false

**Example:**
```javascript
import { printReceipt } from 'unrdf/knowledge-engine';

const result = await tx.apply(store, delta);
printReceipt(result.receipt, { verbose: true });
```

**Output:**
```
📋 Transaction Receipt abc-123
   Status: ✅ Committed
   Duration: 42ms
   Actor: system
   Hooks: 3 executed
     ✅ no-eve (pre)
     ✅ validate-schema (pre)
     ✅ update-index (post)
```

---

## Knowledge Hook Manager

### `KnowledgeHookManager(options)`

Extended transaction manager with knowledge hook support.

**Parameters:**
- `options` (Object, optional): Manager options (extends TransactionManager)
  - `basePath` (string): Base path for file resolution. Default: `process.cwd()`
  - `enableKnowledgeHooks` (boolean): Enable knowledge hook execution. Default: true
  - `strictMode` (boolean): Enable strict error handling. Default: false

**Returns:** `KnowledgeHookManager` - New knowledge hook manager instance

**Example:**
```javascript
import { KnowledgeHookManager } from 'unrdf/knowledge-engine';

const manager = new KnowledgeHookManager({
  basePath: '/path/to/hooks',
  enableKnowledgeHooks: true,
  strictMode: true
});
```

---

### Knowledge Hook Manager Methods

#### `addKnowledgeHook(hook)`

Register a knowledge hook.

**Parameters:**
- `hook` (Object): Knowledge hook definition
  - `meta` (Object): Hook metadata
    - `name` (string): Hook name
    - `version` (string): Hook version
    - `description` (string): Hook description
  - `when` (Object): Trigger condition
  - `run` (Function): Hook execution function `(event) => Promise<result>`

**Throws:**
- `Error` - If knowledge hooks are disabled
- `Error` - If hook is invalid
- `Error` - If hook with same name already exists

**Example:**
```javascript
manager.addKnowledgeHook({
  meta: {
    name: 'validate-person',
    version: '1.0.0',
    description: 'Validate person data'
  },
  when: {
    'sparql-ask': `
      ASK {
        ?s a <http://schema.org/Person> .
      }
    `
  },
  run: async (event) => {
    // Validation logic
    const { delta } = event.payload;

    for (const quad of delta.additions) {
      // Validate quad
    }

    return {
      success: true,
      metadata: { validated: delta.additions.length }
    };
  }
});
```

---

#### `removeKnowledgeHook(hookName)`

Remove a knowledge hook.

**Parameters:**
- `hookName` (string): The hook name to remove

**Returns:** `boolean` - True if hook was removed

**Example:**
```javascript
manager.removeKnowledgeHook('validate-person');
```

---

#### `getKnowledgeHooks()`

Get all registered knowledge hooks.

**Returns:** `Array` - Array of knowledge hook definitions

**Example:**
```javascript
const hooks = manager.getKnowledgeHooks();
console.log(`${hooks.length} knowledge hooks registered`);
```

---

#### `executeKnowledgeHook(hookName, event, options)`

Execute a knowledge hook directly.

**Parameters:**
- `hookName` (string): The hook name
- `event` (Object): The hook event
  - `name` (string): Event name
  - `payload` (Object): Event payload
  - `context` (Object): Execution context
- `options` (Object, optional): Execution options

**Returns:** `Promise<Object>` - Hook execution result
- `success` (boolean): Success flag
- `metadata` (Object): Result metadata
- `error` (string, optional): Error message

**Example:**
```javascript
const result = await manager.executeKnowledgeHook('validate-person', {
  name: 'transaction-apply',
  payload: { delta, storeSize: 100 },
  context: { graph: store }
});

console.log('Hook succeeded:', result.success);
```

---

#### `loadPolicyPack(packName)`

Load and activate a policy pack.

**Parameters:**
- `packName` (string): Policy pack name

**Returns:** `Promise<boolean>` - Success

**Example:**
```javascript
await manager.loadPolicyPack('governance:core');

// All hooks from the policy pack are now active
```

---

#### `deactivatePolicyPack(packName)`

Deactivate a policy pack.

**Parameters:**
- `packName` (string): Policy pack name

**Returns:** `boolean` - Success

**Example:**
```javascript
manager.deactivatePolicyPack('governance:core');
```

---

## Policy Pack Manager

### `PolicyPackManager(basePath)`

Manage multiple policy packs.

**Parameters:**
- `basePath` (string, optional): Base path for policy packs. Default: `process.cwd()`

**Returns:** `PolicyPackManager` - New policy pack manager instance

**Example:**
```javascript
import { PolicyPackManager } from 'unrdf/knowledge-engine';

const ppm = new PolicyPackManager('/path/to/policy-packs');
```

---

### Policy Pack Manager Methods

#### `loadPolicyPack(manifestPath)`

Load a policy pack from manifest file.

**Parameters:**
- `manifestPath` (string): Path to manifest file

**Returns:** `Promise<PolicyPack>` - Loaded policy pack

**Throws:** `Error` - If manifest file not found or invalid

**Example:**
```javascript
const pack = await ppm.loadPolicyPack('/path/to/manifest.json');
console.log(`Loaded pack: ${pack.manifest.meta.name}`);
```

---

#### `loadAllPolicyPacks(packsDir)`

Load all policy packs from a directory.

**Parameters:**
- `packsDir` (string, optional): Directory containing policy packs

**Returns:** `Promise<Array<PolicyPack>>` - Array of loaded policy packs

**Example:**
```javascript
const packs = await ppm.loadAllPolicyPacks('./policy-packs');
console.log(`Loaded ${packs.length} policy packs`);
```

---

#### `activatePolicyPack(packName)`

Activate a policy pack.

**Parameters:**
- `packName` (string): Policy pack name

**Returns:** `boolean` - Success

**Throws:**
- `Error` - If policy pack not found
- `Error` - If policy pack is disabled

**Example:**
```javascript
ppm.activatePolicyPack('governance:core');
```

---

#### `getActivePolicyPacks()`

Get all active policy packs.

**Returns:** `Array<PolicyPack>` - Array of active policy packs

**Example:**
```javascript
const active = ppm.getActivePolicyPacks();
console.log('Active packs:', active.map(p => p.manifest.meta.name));
```

---

#### `getActiveHooks()`

Get all hooks from active policy packs.

**Returns:** `Array` - Array of hook definitions (sorted by priority)

**Example:**
```javascript
const hooks = ppm.getActiveHooks();
console.log(`${hooks.length} active hooks`);
```

---

## Policy Pack Structure

Policy packs are versioned governance units that bundle related hooks:

```
policy-packs/
  governance:core/
    manifest.json          # Pack metadata and configuration
    hooks/
      validate-schema.mjs  # Hook implementations
      check-permissions.mjs
    conditions/
      has-rdf-type.sparql  # Reusable conditions
    resources/
      schema.ttl           # Related resources
```

### Manifest Format

```json
{
  "id": "uuid",
  "meta": {
    "name": "governance:core",
    "version": "1.0.0",
    "description": "Core governance policies",
    "author": "Your Team",
    "license": "MIT"
  },
  "config": {
    "enabled": true,
    "priority": 50,
    "strictMode": false,
    "timeout": 30000
  },
  "hooks": [
    {
      "name": "validate-schema",
      "file": "hooks/validate-schema.mjs",
      "enabled": true,
      "priority": 80
    }
  ],
  "conditions": [
    {
      "name": "has-rdf-type",
      "file": "conditions/has-rdf-type.sparql",
      "type": "sparql-ask"
    }
  ]
}
```

---

## Best Practices

### Hook Organization

Group related hooks into policy packs:

```javascript
// Load policy packs
await manager.loadPolicyPack('governance:core');
await manager.loadPolicyPack('security:validation');
await manager.loadPolicyPack('compliance:gdpr');

// All hooks are now active and prioritized
```

### Error Handling

Use strict mode for critical systems:

```javascript
const tx = new TransactionManager({
  strictMode: true  // Fail fast on hook errors
});
```

### Performance

Use `afterHashOnly` for high-throughput systems:

```javascript
const tx = new TransactionManager({
  afterHashOnly: true  // Skip expensive canonicalization
});
```

### Receipt Verification

Always verify transaction receipts:

```javascript
const result = await tx.apply(store, delta);

if (result.receipt.committed) {
  console.log('✅ Transaction committed');
  console.log('Hash:', result.receipt.afterHash.sha3);
} else {
  console.error('❌ Transaction failed');
  console.error('Error:', result.receipt.error);
}
```
