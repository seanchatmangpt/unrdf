# v6 Pattern Tutorials (DIATAXIS: Tutorial)

**Format**: Learning-Oriented (Step-by-step walkthroughs)
**Audience**: Developers new to v6 patterns
**Goal**: Build confidence through hands-on practice

---

## Tutorial 1: Add Receipt to Your First Operation (15 min)

**What You'll Learn**: Generate cryptographic receipts for any operation using the Receipt HOF Pattern.

**Prerequisites**:
- Node.js 18+
- Basic JavaScript knowledge
- UNRDF v6 installed

### Step 1: Install Dependencies

```bash
cd /home/user/unrdf
pnpm install
pnpm add @unrdf/v6-compat
```

### Step 2: Create a Simple Operation

Create `/tmp/my-operation.mjs`:

```javascript
/**
 * Simple data processing function (no receipts yet)
 */
async function processData(userId, action) {
  // Simulate some work
  await new Promise(r => setTimeout(r, 100));

  return {
    userId,
    action,
    status: 'completed',
    timestamp: Date.now()
  };
}

// Test it
const result = await processData('user-123', 'approve');
console.log(result);
// { userId: 'user-123', action: 'approve', status: 'completed', timestamp: 1704... }
```

**Run it**:
```bash
node /tmp/my-operation.mjs
```

### Step 3: Add Receipt HOF

Update `/tmp/my-operation.mjs`:

```javascript
import { withReceipt } from '@unrdf/v6-compat/adapters';

// Original function (unchanged)
async function processData(userId, action) {
  await new Promise(r => setTimeout(r, 100));

  return {
    userId,
    action,
    status: 'completed',
    timestamp: Date.now()
  };
}

// Wrap with receipt generation
const processDataWithReceipt = withReceipt(processData, {
  operation: 'user.processData'
});

// Use it
const { result, receipt } = await processDataWithReceipt('user-123', 'approve');

console.log('Result:', result);
// { userId: 'user-123', action: 'approve', status: 'completed', ... }

console.log('Receipt:', receipt);
// {
//   version: '6.0.0-alpha.1',
//   operation: 'user.processData',
//   timestamp: 1704...,
//   duration: 102.5,
//   args: '["user-123","approve"]',
//   result: '{"userId":"user-123",...}'
// }
```

**Run it**:
```bash
node /tmp/my-operation.mjs
```

**Expected Output**:
```
Result: { userId: 'user-123', action: 'approve', status: 'completed', timestamp: 1704067200000 }
Receipt: { version: '6.0.0-alpha.1', operation: 'user.processData', timestamp: 1704067200000, duration: 102.5, ... }
```

### Step 4: Verify Receipt

Add verification to `/tmp/my-operation.mjs`:

```javascript
import { withReceipt } from '@unrdf/v6-compat/adapters';
import { computeBlake3 } from '@unrdf/v6-core/receipts/base-receipt';

// ... (previous code) ...

// Generate receipt
const { result, receipt } = await processDataWithReceipt('user-123', 'approve');

// Add hash to receipt
receipt.hash = await computeBlake3(receipt);

console.log('Receipt Hash:', receipt.hash);
// '9a3f52e1...' (64-char BLAKE3 hash)

// Verify receipt integrity
const recomputedHash = await computeBlake3(receipt);
const isValid = receipt.hash === recomputedHash;

console.log('Receipt Valid:', isValid);
// true
```

### Step 5: Chain Receipts

Create a receipt chain:

```javascript
import { withReceipt } from '@unrdf/v6-compat/adapters';
import { computeBlake3, computeChainHash } from '@unrdf/v6-core/receipts/base-receipt';

const receipts = [];
let previousHash = null;

// Operation 1
const op1 = withReceipt(async () => 'step-1-complete', { operation: 'step1' });
const { receipt: r1 } = await op1();
r1.previousHash = previousHash;
r1.payloadHash = await computeBlake3({ operation: r1.operation });
r1.receiptHash = await computeChainHash(r1.previousHash, r1.payloadHash);
receipts.push(r1);
previousHash = r1.receiptHash;

// Operation 2
const op2 = withReceipt(async () => 'step-2-complete', { operation: 'step2' });
const { receipt: r2 } = await op2();
r2.previousHash = previousHash;
r2.payloadHash = await computeBlake3({ operation: r2.operation });
r2.receiptHash = await computeChainHash(r2.previousHash, r2.payloadHash);
receipts.push(r2);
previousHash = r2.receiptHash;

console.log('Receipt Chain:', receipts.map(r => r.receiptHash));
// ['abc123...', 'def456...']

// Verify chain integrity
console.log('Chain link valid:', receipts[1].previousHash === receipts[0].receiptHash);
// true
```

### What You Learned

✅ How to wrap any function with `withReceipt()`
✅ Receipt structure (timestamp, duration, args, result)
✅ Computing BLAKE3 hashes for integrity
✅ Chaining receipts with previousHash

### Next Steps

- **Tutorial 2**: Create your first Delta proposal
- **How-To**: Verify receipt chains
- **Reference**: Receipt schema documentation

---

## Tutorial 2: Create and Apply a Delta (20 min)

**What You'll Learn**: Propose state changes using the Delta Contract Pattern.

**Prerequisites**:
- Completed Tutorial 1
- Understanding of RDF triples

### Step 1: Set Up Store

Create `/tmp/delta-tutorial.mjs`:

```javascript
import { createStore } from '@unrdf/oxigraph';
import { DeltaGate, createDelta } from '@unrdf/v6-core/delta';

// Create RDF store
const store = await createStore();

console.log('Store created. Initial quads:', await store.size());
// 0
```

### Step 2: Create Your First Delta

Add to `/tmp/delta-tutorial.mjs`:

```javascript
// Create delta to add a triple
const delta = createDelta(
  'add',                              // operation
  'http://example.org/user-123',      // subject
  'http://example.org/name',          // predicate
  '"Alice Smith"',                    // object
  {
    package: '@unrdf/tutorial',
    actor: 'tutorial-user'
  }
);

console.log('Delta created:', delta);
// {
//   id: 'uuid-...',
//   timestamp_iso: '2025-12-27T...',
//   t_ns: 1704...n,
//   operations: [{ op: 'add', subject: '...', ... }],
//   source: { package: '@unrdf/tutorial', actor: 'tutorial-user' }
// }
```

### Step 3: Apply Delta Through Gate

```javascript
// Create delta gate (no policies for now)
const gate = new DeltaGate();

// Propose delta
const receipt = await gate.proposeDelta(delta, store);

console.log('Receipt:', receipt);
// {
//   deltaId: 'uuid-...',
//   applied: true,
//   timestamp_ns: 1704...n,
//   operationsApplied: 1
// }

// Check store
console.log('Store size after delta:', await store.size());
// 1
```

### Step 4: Add Policy Validation

```javascript
// Create gate with policy
const gateWithPolicy = new DeltaGate({
  policies: {
    'require-actor': async (delta, store) => {
      // Reject if no actor specified
      if (!delta.source.actor) {
        return false;
      }
      return true;
    }
  }
});

// This delta will be accepted (has actor)
const validDelta = createDelta('add', 'http://ex.org/s', 'http://ex.org/p', 'value', {
  package: '@unrdf/tutorial',
  actor: 'alice'
});

const validReceipt = await gateWithPolicy.proposeDelta(validDelta, store);
console.log('Valid delta accepted:', validReceipt.applied);
// true

// This delta will be rejected (no actor)
const invalidDelta = createDelta('add', 'http://ex.org/s2', 'http://ex.org/p', 'value', {
  package: '@unrdf/tutorial'
  // No actor!
});

const invalidReceipt = await gateWithPolicy.proposeDelta(invalidDelta, store);
console.log('Invalid delta rejected:', invalidReceipt.applied);
// false
console.log('Rejection reason:', invalidReceipt.reason);
// 'Policy check failed: require-actor'
```

### Step 5: Multi-Operation Delta

```javascript
// Create composite delta (add user + assign role)
const compositeDelta = {
  id: crypto.randomUUID(),
  timestamp_iso: new Date().toISOString(),
  t_ns: BigInt(Date.now()) * 1_000_000n,

  operations: [
    {
      op: 'add',
      subject: 'http://example.org/bob',
      predicate: 'http://schema.org/name',
      object: '"Bob Johnson"'
    },
    {
      op: 'add',
      subject: 'http://example.org/bob',
      predicate: 'http://example.org/role',
      object: 'http://example.org/roles/admin'
    }
  ],

  source: {
    package: '@unrdf/tutorial',
    actor: 'admin-user'
  }
};

const compositeReceipt = await gate.proposeDelta(compositeDelta, store);

console.log('Composite delta applied:', compositeReceipt.applied);
// true
console.log('Operations applied:', compositeReceipt.operationsApplied);
// 2

console.log('Store size:', await store.size());
// 3 (1 from step 3 + 2 from composite)
```

### What You Learned

✅ Creating deltas with `createDelta()`
✅ Applying deltas through `DeltaGate`
✅ Adding policy validation
✅ Multi-operation atomic deltas
✅ Handling rejection receipts

### Next Steps

- **Tutorial 3**: Add Zod validation to deltas
- **How-To**: Query delta history with SPARQL
- **Explanation**: Why Delta Contract improves traceability

---

## Tutorial 3: Validate Inputs with Zod (15 min)

**What You'll Learn**: Add runtime type safety using the Zod Validation Envelope Pattern.

**Prerequisites**:
- Basic TypeScript/JSDoc knowledge
- Understanding of schemas

### Step 1: Define a Schema

Create `/tmp/zod-tutorial.mjs`:

```javascript
import { z } from 'zod';

// Define user schema
const UserSchema = z.object({
  id: z.string().uuid(),
  name: z.string().min(1).max(100),
  email: z.string().email(),
  age: z.number().int().positive().max(150)
});

console.log('Schema defined');
```

### Step 2: Validate Valid Data

```javascript
const validUser = {
  id: crypto.randomUUID(),
  name: 'Alice Smith',
  email: 'alice@example.com',
  age: 30
};

try {
  const validated = UserSchema.parse(validUser);
  console.log('✅ Valid user:', validated);
} catch (error) {
  console.error('❌ Validation failed:', error.errors);
}

// ✅ Valid user: { id: '...', name: 'Alice Smith', email: 'alice@example.com', age: 30 }
```

### Step 3: Catch Invalid Data

```javascript
const invalidUser = {
  id: 'not-a-uuid',           // ❌ Invalid UUID
  name: '',                   // ❌ Too short
  email: 'invalid-email',     // ❌ Not an email
  age: -5                     // ❌ Negative age
};

try {
  const validated = UserSchema.parse(invalidUser);
  console.log('✅ Valid user:', validated);
} catch (error) {
  console.error('❌ Validation failed:');
  error.errors.forEach(err => {
    console.error(`  - ${err.path.join('.')}: ${err.message}`);
  });
}

// ❌ Validation failed:
//   - id: Invalid uuid
//   - name: String must contain at least 1 character(s)
//   - email: Invalid email
//   - age: Number must be greater than 0
```

### Step 4: Compose with Receipt HOF

```javascript
import { withReceipt } from '@unrdf/v6-compat/adapters';

// Create validated operation
function createUser(userData) {
  // Validate input
  const user = UserSchema.parse(userData);

  // Business logic
  return {
    success: true,
    userId: user.id,
    message: `User ${user.name} created`
  };
}

// Wrap with receipt
const createUserWithReceipt = withReceipt(createUser, {
  operation: 'user.create'
});

// Valid input
try {
  const { result, receipt } = await createUserWithReceipt({
    id: crypto.randomUUID(),
    name: 'Bob',
    email: 'bob@example.com',
    age: 25
  });

  console.log('✅ Success:', result);
  console.log('Receipt:', receipt.operation);
} catch (error) {
  console.error('❌ Failed:', error.message);
}

// Invalid input - fails before receipt generation
try {
  await createUserWithReceipt({ id: 'bad-id' });
} catch (error) {
  console.error('❌ Validation error:', error.message);
  // No receipt generated for invalid input
}
```

### Step 5: Output Schema Validation

```javascript
const OutputSchema = z.object({
  success: z.boolean(),
  userId: z.string().uuid(),
  message: z.string()
});

function createUserValidated(userData) {
  // Input validation
  const user = UserSchema.parse(userData);

  // Business logic
  const result = {
    success: true,
    userId: user.id,
    message: `User ${user.name} created`
  };

  // Output validation
  return OutputSchema.parse(result);
}

// Test
const output = createUserValidated({
  id: crypto.randomUUID(),
  name: 'Charlie',
  email: 'charlie@example.com',
  age: 35
});

console.log('✅ Validated output:', output);
```

### What You Learned

✅ Defining Zod schemas
✅ Validating input data with `parse()`
✅ Handling validation errors
✅ Composing validation with Receipt HOF
✅ Output schema validation

### Next Steps

- **Tutorial 4**: Prove determinism with 100x runs
- **How-To**: Generate schemas from JSDoc
- **Reference**: Zod schema API

---

## Tutorial 4: Prove Determinism (25 min)

**What You'll Learn**: Guarantee reproducible outputs using the Determinism Proof Pattern.

**Prerequisites**:
- Understanding of hashing
- Completed Tutorial 1

### Step 1: Create Deterministic Function

Create `/tmp/determinism-tutorial.mjs`:

```javascript
import { computeBlake3, deterministicSerialize } from '@unrdf/v6-core/receipts/base-receipt';

// Enable deterministic mode
process.env.DETERMINISTIC = '1';

async function hashData(data) {
  const serialized = deterministicSerialize(data);
  return await computeBlake3(serialized);
}

// Test with identical inputs
const hash1 = await hashData({ x: 1, y: 2 });
const hash2 = await hashData({ x: 1, y: 2 });

console.log('Hash 1:', hash1);
console.log('Hash 2:', hash2);
console.log('Match:', hash1 === hash2);
// true
```

### Step 2: Run 100x Test

```javascript
// Run 100 times
const hashes = await Promise.all(
  Array.from({ length: 100 }, () =>
    hashData({ action: 'test', value: 42 })
  )
);

const uniqueHashes = new Set(hashes);

console.log('Total runs:', hashes.length);
// 100
console.log('Unique hashes:', uniqueHashes.size);
// 1
console.log('✅ DETERMINISTIC: All hashes match');
```

### Step 3: Test Key Ordering

```javascript
// Different key orders should produce same hash
const obj1 = { a: 1, b: 2, c: 3 };
const obj2 = { c: 3, a: 1, b: 2 }; // Different order
const obj3 = { b: 2, c: 3, a: 1 }; // Another order

const hash1 = await hashData(obj1);
const hash2 = await hashData(obj2);
const hash3 = await hashData(obj3);

console.log('All hashes match:', hash1 === hash2 && hash2 === hash3);
// true

console.log('Serialized obj1:', deterministicSerialize(obj1));
// {"a":1,"b":2,"c":3} (always alphabetical)
console.log('Serialized obj2:', deterministicSerialize(obj2));
// {"a":1,"b":2,"c":3} (same)
```

### Step 4: Create Deterministic Receipt

```javascript
import { generateUUID } from '@unrdf/v6-core/receipts/base-receipt';

async function generateDeterministicReceipt(payload) {
  // Deterministic timestamp from payload hash
  const payloadHash = await computeBlake3(deterministicSerialize(payload));

  // Use payload hash to generate deterministic timestamp
  const timestampCache = new Map();
  let timestamp;

  if (timestampCache.has(payloadHash)) {
    timestamp = timestampCache.get(payloadHash);
  } else {
    timestamp = BigInt(Date.now()) * 1_000_000n;
    timestampCache.set(payloadHash, timestamp);
  }

  return {
    id: generateUUID(), // Still unique per call
    payloadHash,
    timestamp,
    payload
  };
}

// Generate receipt 100 times for same payload
const receipts = await Promise.all(
  Array.from({ length: 100 }, () =>
    generateDeterministicReceipt({ action: 'test' })
  )
);

// All payload hashes should match
const payloadHashes = new Set(receipts.map(r => r.payloadHash));
console.log('Unique payload hashes:', payloadHashes.size);
// 1 (deterministic)

// IDs should be unique
const ids = new Set(receipts.map(r => r.id));
console.log('Unique IDs:', ids.size);
// 100 (non-deterministic, as intended)
```

### Step 5: Verify Receipt Chain Determinism

```javascript
async function buildDeterministicChain(events) {
  const receipts = [];
  let previousHash = null;

  for (const event of events) {
    const payloadHash = await computeBlake3(deterministicSerialize(event));
    const receiptHash = await computeBlake3(
      deterministicSerialize({ previousHash, payloadHash })
    );

    receipts.push({
      id: generateUUID(),
      previousHash,
      payloadHash,
      receiptHash,
      event
    });

    previousHash = receiptHash;
  }

  return receipts;
}

// Build chain 100 times
const events = [
  { action: 'create', id: 1 },
  { action: 'update', id: 1 },
  { action: 'delete', id: 1 }
];

const chains = await Promise.all(
  Array.from({ length: 100 }, () => buildDeterministicChain(events))
);

// All chains should have identical hashes
const chainHashes = chains.map(chain =>
  chain.map(r => r.receiptHash).join(':')
);

const uniqueChainHashes = new Set(chainHashes);

console.log('Unique chain hashes:', uniqueChainHashes.size);
// 1
console.log('✅ DETERMINISTIC CHAIN: All 100 runs produced identical hashes');
```

### What You Learned

✅ Deterministic serialization (sorted keys)
✅ BLAKE3 hashing for consistency
✅ 100x test methodology
✅ Deterministic receipt generation
✅ Receipt chain determinism

### Next Steps

- **Tutorial 5**: Compose cross-package workflows
- **How-To**: Run determinism checker tool
- **Explanation**: Why determinism enables replay

---

## Tutorial 5: Compose Cross-Package Workflows (30 min)

**What You'll Learn**: Integrate multiple packages using the Composition Layer Pattern (L5).

**Prerequisites**:
- Completed Tutorials 1-4
- Understanding of all patterns

### Step 1: Define Module Interfaces

Create `/tmp/composition-tutorial.mjs`:

```javascript
import { z } from 'zod';
import { withReceipt } from '@unrdf/v6-compat/adapters';

// Module A: Data Creator
const ModuleAOutputSchema = z.object({
  data: z.string(),
  timestamp: z.number()
});

const moduleA = withReceipt(
  async () => ({
    data: 'created',
    timestamp: Date.now()
  }),
  { operation: 'moduleA.create' }
);

moduleA.outputSchema = ModuleAOutputSchema;

// Module B: Data Processor
const ModuleBInputSchema = z.object({
  data: z.string(),
  timestamp: z.number()
});

const ModuleBOutputSchema = z.object({
  processed: z.string(),
  originalTimestamp: z.number()
});

const moduleB = withReceipt(
  async (input) => {
    // Validate input
    const validated = ModuleBInputSchema.parse(input);

    return {
      processed: validated.data.toUpperCase(),
      originalTimestamp: validated.timestamp
    };
  },
  { operation: 'moduleB.process' }
);

moduleB.inputSchema = ModuleBInputSchema;
moduleB.outputSchema = ModuleBOutputSchema;
```

### Step 2: Check Compatibility

```javascript
class CompositionChecker {
  canCompose(moduleA, moduleB) {
    try {
      // Generate test data from A's output schema
      const testData = {
        data: 'test',
        timestamp: 1704067200000
      };

      // Validate with A's output schema
      moduleA.outputSchema.parse(testData);

      // Validate with B's input schema
      moduleB.inputSchema.parse(testData);

      return true;
    } catch {
      return false;
    }
  }
}

const checker = new CompositionChecker();
const compatible = checker.canCompose(moduleA, moduleB);

console.log('Modules A and B compatible:', compatible);
// true
```

### Step 3: Compose Operations

```javascript
async function composeOperations(opA, opB) {
  // Execute A
  const { result: resultA, receipt: receiptA } = await opA();

  console.log('Module A result:', resultA);
  console.log('Module A receipt:', receiptA.operation);

  // Check compatibility
  const compatible = opB.inputSchema.safeParse(resultA).success;

  if (!compatible) {
    throw new Error('Incompatible modules: A output does not match B input');
  }

  // Execute B with A's output
  const { result: resultB, receipt: receiptB } = await opB(resultA);

  console.log('Module B result:', resultB);
  console.log('Module B receipt:', receiptB.operation);

  // Chain receipts
  receiptB.previousReceipt = receiptA.operation;

  return {
    result: resultB,
    receipts: [receiptA, receiptB]
  };
}

// Compose A → B
const { result, receipts } = await composeOperations(moduleA, moduleB);

console.log('Final result:', result);
// { processed: 'CREATED', originalTimestamp: 1704... }

console.log('Receipt chain:', receipts.map(r => r.operation));
// ['moduleA.create', 'moduleB.process']
```

### Step 4: Three-Module Composition

```javascript
// Module C: Data Validator
const ModuleCInputSchema = z.object({
  processed: z.string(),
  originalTimestamp: z.number()
});

const ModuleCOutputSchema = z.object({
  valid: z.boolean(),
  message: z.string()
});

const moduleC = withReceipt(
  async (input) => {
    const validated = ModuleCInputSchema.parse(input);

    return {
      valid: validated.processed.length > 0,
      message: `Validated ${validated.processed}`
    };
  },
  { operation: 'moduleC.validate' }
);

moduleC.inputSchema = ModuleCInputSchema;
moduleC.outputSchema = ModuleCOutputSchema;

// Compose A → B → C
async function composeThree(opA, opB, opC) {
  const { result: r1, receipt: rec1 } = await opA();
  const { result: r2, receipt: rec2 } = await opB(r1);
  const { result: r3, receipt: rec3 } = await opC(r2);

  rec2.previousReceipt = rec1.operation;
  rec3.previousReceipt = rec2.operation;

  return {
    result: r3,
    receipts: [rec1, rec2, rec3]
  };
}

const { result: finalResult, receipts: allReceipts } = await composeThree(
  moduleA,
  moduleB,
  moduleC
);

console.log('Final result:', finalResult);
// { valid: true, message: 'Validated CREATED' }

console.log('Receipt chain:', allReceipts.map(r => r.operation));
// ['moduleA.create', 'moduleB.process', 'moduleC.validate']
```

### Step 5: Build Compatibility Matrix

```javascript
const modules = {
  A: { outputSchema: ModuleAOutputSchema },
  B: { inputSchema: ModuleBInputSchema, outputSchema: ModuleBOutputSchema },
  C: { inputSchema: ModuleCInputSchema, outputSchema: ModuleCOutputSchema }
};

function buildCompatibilityMatrix(modules) {
  const matrix = [];

  for (const [nameA, modA] of Object.entries(modules)) {
    for (const [nameB, modB] of Object.entries(modules)) {
      if (nameA === nameB) continue;
      if (!modA.outputSchema || !modB.inputSchema) continue;

      const compatible = checker.canCompose(modA, modB);

      matrix.push({
        from: nameA,
        to: nameB,
        compatible
      });
    }
  }

  return matrix;
}

const matrix = buildCompatibilityMatrix(modules);

console.table(matrix);
// ┌─────┬──────┬──────┬────────────┐
// │ idx │ from │ to   │ compatible │
// ├─────┼──────┼──────┼────────────┤
// │ 0   │ A    │ B    │ true       │
// │ 1   │ A    │ C    │ false      │
// │ 2   │ B    │ A    │ false      │
// │ 3   │ B    │ C    │ true       │
// │ 4   │ C    │ A    │ false      │
// │ 5   │ C    │ B    │ false      │
// └─────┴──────┴──────┴────────────┘

console.log('Valid compositions: A→B→C');
```

### What You Learned

✅ Defining module interfaces with Zod schemas
✅ Checking schema compatibility
✅ Composing operations with receipt chains
✅ Multi-module workflows
✅ Building compatibility matrices

### Next Steps

- **How-To**: Debug composition failures
- **Reference**: Composition API documentation
- **Explanation**: Why composition enables modularity

---

## Summary

You've completed all 5 core v6 pattern tutorials:

1. ✅ **Receipt HOF**: Generate receipts for any operation
2. ✅ **Delta Contract**: Explicit state transitions
3. ✅ **Zod Validation**: Runtime type safety
4. ✅ **Determinism Proof**: Reproducible outputs
5. ✅ **Composition Layer**: Cross-package integration

**Next**: Apply these patterns to migrate your packages to v6!

---

## Additional Resources

- **How-To Guides**: [/docs/v6/PATTERN_HOWTO.md](/home/user/unrdf/docs/v6/PATTERN_HOWTO.md)
- **Reference**: [/docs/v6/PATTERNS.md](/home/user/unrdf/docs/v6/PATTERNS.md)
- **Migration Runbooks**: [/docs/v6/MIGRATION_RUNBOOKS.md](/home/user/unrdf/docs/v6/MIGRATION_RUNBOOKS.md)
- **v6 Program Charter**: [/docs/v6/PROGRAM_CHARTER.md](/home/user/unrdf/docs/v6/PROGRAM_CHARTER.md)
