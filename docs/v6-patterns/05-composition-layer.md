# Pattern 5: Composition Layer (L5 Criterion)

**Version**: v6.0.0
**Maturity Target**: L5 (Stable contracts, deterministic, adversarial-safe, **composable**)
**Copy-Exact Template**: Yes - Use AS-IS for all P0+P1 packages

---

## Overview

**Problem**: Can two L5 modules compose to create a new L5 module? How do receipts chain? How do deltas compose?

**Solution**: Composition Layer = Formal rules for combining L5 modules while preserving L5 properties.

**L5 Composability Criteria**:
1. Stable contracts (interfaces don't break)
2. Deterministic execution (same inputs → same outputs)
3. Receipt chaining (receipts compose via cryptographic linking)
4. Delta composition (deltas merge without conflicts)

**Invariant**: `L5(A) ⊕ L5(B) → L5(C)` (Composition preserves L5)

---

## Core Concepts

### 1. What is an L5 Module?

An **L5 Module** satisfies:

```javascript
/**
 * L5 Module Interface
 *
 * @typedef {Object} L5Module
 * @property {string} name - Module name
 * @property {string} version - Semantic version (stable API)
 * @property {Function} operation - Main operation (deterministic)
 * @property {ZodSchema} inputSchema - Input validation schema
 * @property {ZodSchema} outputSchema - Output validation schema
 * @property {Function} withReceipt - Receipt-generating wrapper
 * @property {Function} createDelta - Delta factory (if stateful)
 */
```

**Example L5 Module**:

```javascript
/**
 * Math Module - L5 Example
 */
export const MathModule = {
  name: '@unrdf/math',
  version: '1.0.0',

  // Input schema
  AddInputSchema: z.object({
    x: z.number(),
    y: z.number(),
  }),

  // Output schema
  AddOutputSchema: z.number(),

  // Deterministic operation
  add(input, context) {
    const validated = this.AddInputSchema.parse(input);
    const result = validated.x + validated.y;
    return this.AddOutputSchema.parse(result);
  },

  // Receipt wrapper
  addWithReceipt(input, context) {
    return withReceipt(
      () => this.add(input, context),
      { operationName: 'math.add', actor: 'math-module', ...context }
    );
  },
};
```

### 2. Composition Operators

#### Sequential Composition (`then`)

```
A then B = A's output becomes B's input
Receipt chain: receiptA → receiptB
```

#### Parallel Composition (`and`)

```
A and B = Execute A and B independently, combine results
Receipt chain: [receiptA, receiptB] → receiptMerge
```

#### Conditional Composition (`if`)

```
A if P else B = Execute A if predicate P is true, else B
Receipt chain: receiptP → (receiptA | receiptB)
```

---

## Copy-Exact Templates

### 1. Sequential Composition (`then`)

```javascript
/**
 * Compose Sequential - Chain operations A → B
 *
 * @template T1, T2, T3
 * @param {Function} opA - First operation (input: T1, output: T2)
 * @param {Function} opB - Second operation (input: T2, output: T3)
 * @param {DeterminismContext} context - Determinism context
 * @returns {Function} Composed operation (input: T1, output: T3)
 *
 * @example
 * const double = (x) => x * 2;
 * const increment = (x) => x + 1;
 * const doubleAndIncrement = composeThen(double, increment, context);
 * const {result, receipts} = doubleAndIncrement(5); // (5 * 2) + 1 = 11
 */
export function composeThen(opA, opB, context) {
  return function composedOperation(input) {
    // Execute A
    const {result: resultA, receipt: receiptA} = withReceipt(
      () => opA(input),
      { operationName: 'opA', actor: 'composer', ...context }
    );

    // Execute B (chained from A)
    const {result: resultB, receipt: receiptB} = withReceipt(
      () => opB(resultA),
      {
        operationName: 'opB',
        actor: 'composer',
        previousReceiptHash: receiptA.receiptHash, // ✅ Chain receipts
        ...context
      }
    );

    // Return final result + chained receipts
    return {
      result: resultB,
      receipts: [receiptA, receiptB],
    };
  };
}

/**
 * Example: Math.double then Math.increment
 */
const double = (x) => x * 2;
const increment = (x) => x + 1;

const context = createFakeContext({ randomSeed: 42, initialTime: 1000n });
const doubleAndIncrement = composeThen(double, increment, context);

const {result, receipts} = doubleAndIncrement(5);
console.assert(result === 11); // (5 * 2) + 1
console.assert(receipts.length === 2);
console.assert(receipts[1].previousHash === receipts[0].receiptHash); // ✅ Chained
```

### 2. Parallel Composition (`and`)

```javascript
/**
 * Compose Parallel - Execute A and B independently, merge results
 *
 * @template T1, T2, T3
 * @param {Function} opA - First operation (input: T1, output: T2)
 * @param {Function} opB - Second operation (input: T1, output: T3)
 * @param {DeterminismContext} context - Determinism context
 * @returns {Function} Composed operation (input: T1, output: {a: T2, b: T3})
 *
 * @example
 * const double = (x) => x * 2;
 * const square = (x) => x * x;
 * const doubleAndSquare = composeAnd(double, square, context);
 * const {result, receipts} = doubleAndSquare(5); // {a: 10, b: 25}
 */
export function composeAnd(opA, opB, context) {
  return function composedOperation(input) {
    // Execute A
    const {result: resultA, receipt: receiptA} = withReceipt(
      () => opA(input),
      { operationName: 'opA', actor: 'composer', ...context }
    );

    // Execute B (parallel, same input)
    const {result: resultB, receipt: receiptB} = withReceipt(
      () => opB(input),
      { operationName: 'opB', actor: 'composer', ...context }
    );

    // Merge receipts (both link to same "genesis" for this composition)
    const mergedReceipt = mergeReceipts([receiptA, receiptB], context);

    return {
      result: { a: resultA, b: resultB },
      receipts: [receiptA, receiptB, mergedReceipt],
    };
  };
}

/**
 * Merge Receipts - Create aggregated receipt
 *
 * @param {Array<Receipt>} receipts
 * @param {DeterminismContext} context
 * @returns {Receipt}
 */
function mergeReceipts(receipts, context) {
  const payload = {
    operation: 'merge',
    receiptHashes: receipts.map(r => r.receiptHash),
  };

  const payloadHash = computeBlake3Sync(payload);
  const receiptHash = computeChainHashSync(null, payloadHash);

  return {
    id: context.random.uuid(),
    receiptType: 'execution',
    t_ns: context.time.now(),
    timestamp_iso: context.time.toISO(context.time.now()),
    previousHash: null,
    payloadHash,
    receiptHash,
    payload,
  };
}
```

### 3. Conditional Composition (`if`)

```javascript
/**
 * Compose Conditional - Execute A if predicate true, else B
 *
 * @template T1, T2
 * @param {Function} predicate - Predicate function (input: T1, output: boolean)
 * @param {Function} opA - Operation if true (input: T1, output: T2)
 * @param {Function} opB - Operation if false (input: T1, output: T2)
 * @param {DeterminismContext} context - Determinism context
 * @returns {Function} Composed operation (input: T1, output: T2)
 *
 * @example
 * const isEven = (x) => x % 2 === 0;
 * const double = (x) => x * 2;
 * const triple = (x) => x * 3;
 * const doubleIfEven = composeIf(isEven, double, triple, context);
 * const {result} = doubleIfEven(4); // 8 (even, so double)
 * const {result} = doubleIfEven(5); // 15 (odd, so triple)
 */
export function composeIf(predicate, opA, opB, context) {
  return function composedOperation(input) {
    // Evaluate predicate (with receipt)
    const {result: predicateResult, receipt: predicateReceipt} = withReceipt(
      () => predicate(input),
      { operationName: 'predicate', actor: 'composer', ...context }
    );

    // Execute A or B based on predicate
    const selectedOp = predicateResult ? opA : opB;
    const {result, receipt: opReceipt} = withReceipt(
      () => selectedOp(input),
      {
        operationName: predicateResult ? 'opA' : 'opB',
        actor: 'composer',
        previousReceiptHash: predicateReceipt.receiptHash, // ✅ Chain from predicate
        ...context
      }
    );

    return {
      result,
      receipts: [predicateReceipt, opReceipt],
    };
  };
}
```

---

## Delta Composition

### 1. Sequential Delta Composition

```javascript
/**
 * Compose Deltas Sequentially
 *
 * Apply delta1, then delta2 on resulting state.
 *
 * @param {Delta} delta1
 * @param {Delta} delta2
 * @param {Object} store - Knowledge store
 * @param {DeltaGate} gate - Delta gate
 * @returns {Promise<{receipts: Array<DeltaReceipt>, finalStateHash: string}>}
 */
export async function composeDeltasSequential(delta1, delta2, store, gate) {
  // Apply delta1
  const receipt1 = await gate.proposeDelta(delta1, store);
  if (!receipt1.applied) {
    throw new Error(`Delta 1 failed: ${receipt1.reason}`);
  }

  // Apply delta2 (on state after delta1)
  const receipt2 = await gate.proposeDelta(delta2, store);
  if (!receipt2.applied) {
    throw new Error(`Delta 2 failed: ${receipt2.reason}`);
  }

  return {
    receipts: [receipt1, receipt2],
    finalStateHash: receipt2.stateHash,
  };
}

/**
 * Example: Add Alice, then Update Alice's age
 */
const delta1 = createDelta({
  operations: [
    { op: 'add', subject: 'http://ex.org/Alice', predicate: 'http://ex.org/age', object: '30' }
  ],
  package: '@unrdf/test',
});

const delta2 = createDelta({
  operations: [
    { op: 'update', subject: 'http://ex.org/Alice', predicate: 'http://ex.org/age', oldObject: '30', newObject: '31' }
  ],
  package: '@unrdf/test',
});

const {receipts, finalStateHash} = await composeDeltasSequential(delta1, delta2, store, gate);
console.assert(receipts.length === 2);
```

### 2. Parallel Delta Composition (Merge)

```javascript
/**
 * Compose Deltas in Parallel (Merge)
 *
 * Merge delta1 and delta2 into a single delta with all operations.
 *
 * @param {Delta} delta1
 * @param {Delta} delta2
 * @returns {Delta} Merged delta
 */
export function composeDeltasParallel(delta1, delta2) {
  // Check for conflicts
  const conflicts = detectConflicts(delta1, delta2);
  if (conflicts.length > 0) {
    throw new Error(`Delta conflicts detected: ${JSON.stringify(conflicts)}`);
  }

  // Merge operations
  const mergedDelta = createDelta({
    operations: [...delta1.operations, ...delta2.operations],
    package: delta1.source.package,
    actor: delta1.source.actor,
  });

  return mergedDelta;
}

/**
 * Detect Conflicts - Check if deltas conflict
 *
 * @param {Delta} delta1
 * @param {Delta} delta2
 * @returns {Array<{triple: string, conflict: string}>}
 */
function detectConflicts(delta1, delta2) {
  const conflicts = [];

  for (const op1 of delta1.operations) {
    for (const op2 of delta2.operations) {
      const sameTriple =
        op1.subject === op2.subject &&
        op1.predicate === op2.predicate;

      if (sameTriple && op1.op !== op2.op) {
        conflicts.push({
          triple: `${op1.subject} ${op1.predicate}`,
          conflict: `${op1.op} vs ${op2.op}`,
        });
      }
    }
  }

  return conflicts;
}

/**
 * Example: Add Alice's age and Alice's name (parallel, no conflict)
 */
const delta1 = createDelta({
  operations: [
    { op: 'add', subject: 'http://ex.org/Alice', predicate: 'http://ex.org/age', object: '30' }
  ],
  package: '@unrdf/test',
});

const delta2 = createDelta({
  operations: [
    { op: 'add', subject: 'http://ex.org/Alice', predicate: 'http://ex.org/name', object: 'Alice' }
  ],
  package: '@unrdf/test',
});

const mergedDelta = composeDeltasParallel(delta1, delta2);
console.assert(mergedDelta.operations.length === 2);
```

---

## Composition Proofs

### 1. Sequential Composition Proof

**Claim**: `L5(A) then L5(B) → L5(C)`

**Proof**:
1. **Stable contracts**: If A's output schema = B's input schema, composition is well-typed.
2. **Deterministic**: Both A and B are deterministic → C is deterministic.
3. **Receipt chaining**: receiptB.previousHash === receiptA.receiptHash → cryptographic chain.
4. **L5 → L5**: All properties preserved.

```javascript
/**
 * Test: Sequential composition preserves L5
 */
const moduleA = {
  operation: (x) => x * 2,
  outputSchema: z.number(),
};

const moduleB = {
  operation: (x) => x + 1,
  inputSchema: z.number(), // Matches A's output
  outputSchema: z.number(),
};

// Compose
const moduleC = {
  operation: composeThen(moduleA.operation, moduleB.operation, context),
  inputSchema: moduleA.inputSchema,
  outputSchema: moduleB.outputSchema,
};

// Test determinism
const context1 = createFakeContext({ randomSeed: 42, initialTime: 1000n });
const {result: r1, receipts: recs1} = moduleC.operation(5);

const context2 = createFakeContext({ randomSeed: 42, initialTime: 1000n });
const {result: r2, receipts: recs2} = moduleC.operation(5);

console.assert(r1 === r2); // ✅ Deterministic
console.assert(recs1[1].previousHash === recs1[0].receiptHash); // ✅ Chained
console.assert(recs2[1].previousHash === recs2[0].receiptHash); // ✅ Chained
console.assert(recs1[1].receiptHash === recs2[1].receiptHash); // ✅ Same hash (deterministic)
```

### 2. Parallel Composition Proof

**Claim**: `L5(A) and L5(B) → L5(C)`

**Proof**:
1. **Stable contracts**: Both A and B have stable schemas → C's schema = {a: A.output, b: B.output}.
2. **Deterministic**: Both A and B are deterministic → C is deterministic (same order).
3. **Receipt merge**: Merge receipt contains hashes of both A and B → cryptographic proof.
4. **L5 → L5**: All properties preserved.

```javascript
/**
 * Test: Parallel composition preserves L5
 */
const moduleA = { operation: (x) => x * 2 };
const moduleB = { operation: (x) => x * x };

const moduleC = {
  operation: composeAnd(moduleA.operation, moduleB.operation, context),
};

// Test determinism
const context1 = createFakeContext({ randomSeed: 42, initialTime: 1000n });
const {result: r1, receipts: recs1} = moduleC.operation(5);

const context2 = createFakeContext({ randomSeed: 42, initialTime: 1000n });
const {result: r2, receipts: recs2} = moduleC.operation(5);

console.assert(r1.a === r2.a); // ✅ Deterministic
console.assert(r1.b === r2.b); // ✅ Deterministic
console.assert(recs1[2].payload.receiptHashes[0] === recs1[0].receiptHash); // ✅ Merged
console.assert(recs1[2].receiptHash === recs2[2].receiptHash); // ✅ Same merge hash
```

---

## Module Boundary Pattern

### 1. L5 Module Template

```javascript
/**
 * L5 Module Template
 *
 * Copy-exact template for creating L5 modules.
 */

import { z } from 'zod';
import { withReceipt } from './receipt-hof.mjs';

/**
 * Input Schema - Define what module accepts
 */
const InputSchema = z.object({
  param1: z.string(),
  param2: z.number(),
});

/**
 * Output Schema - Define what module returns
 */
const OutputSchema = z.object({
  result: z.string(),
  metadata: z.any().optional(),
});

/**
 * Module Definition
 */
export const MyModule = {
  // Metadata
  name: '@unrdf/my-module',
  version: '1.0.0',

  // Schemas
  InputSchema,
  OutputSchema,

  /**
   * Core Operation (deterministic)
   *
   * @param {z.infer<typeof InputSchema>} input
   * @param {DeterminismContext} context
   * @returns {z.infer<typeof OutputSchema>}
   */
  operation(input, context) {
    // 1. Validate input
    const validatedInput = InputSchema.parse(input);

    // 2. Execute deterministic logic (NO Date.now(), Math.random(), process.env)
    const result = `${validatedInput.param1}-${validatedInput.param2}`;

    // 3. Build output
    const output = {
      result,
      metadata: { timestamp: context.time.now() },
    };

    // 4. Validate output
    return OutputSchema.parse(output);
  },

  /**
   * Operation with Receipt
   *
   * @param {z.infer<typeof InputSchema>} input
   * @param {DeterminismContext} context
   * @returns {{result: z.infer<typeof OutputSchema>, receipt: Receipt}}
   */
  operationWithReceipt(input, context) {
    return withReceipt(
      () => this.operation(input, context),
      { operationName: 'my-module.operation', actor: 'my-module', ...context }
    );
  },

  /**
   * Compose with another L5 module
   *
   * @param {L5Module} other - Another L5 module
   * @param {DeterminismContext} context
   * @returns {Function} Composed operation
   */
  composeThen(other, context) {
    return composeThen(this.operation, other.operation, context);
  },
};
```

---

## Common Pitfalls

### Pitfall 1: Non-Matching Schemas

```javascript
// ❌ WRONG - A's output type ≠ B's input type
const moduleA = { operation: (x) => x.toString(), outputSchema: z.string() };
const moduleB = { operation: (x) => x * 2, inputSchema: z.number() }; // Expects number!

const composed = composeThen(moduleA.operation, moduleB.operation, context);
// ❌ Runtime error: string → number mismatch

// ✅ CORRECT - Match schemas or add adapter
const adapter = (str) => parseInt(str, 10);
const composedFixed = composeThen(
  moduleA.operation,
  (input) => moduleB.operation(adapter(input)),
  context
);
```

### Pitfall 2: Breaking Receipt Chain

```javascript
// ❌ WRONG - Not chaining receipts
const {receipt: receiptA} = withReceipt(opA, { operationName: 'opA', actor: 'a' });
const {receipt: receiptB} = withReceipt(opB, { operationName: 'opB', actor: 'b' }); // No previousHash!

// ✅ CORRECT - Chain receipts
const {receipt: receiptA} = withReceipt(opA, { operationName: 'opA', actor: 'a' });
const {receipt: receiptB} = withReceipt(opB, {
  operationName: 'opB',
  actor: 'b',
  previousReceiptHash: receiptA.receiptHash, // ✅ Chained
});
```

### Pitfall 3: Delta Conflicts in Parallel Merge

```javascript
// ❌ WRONG - Conflicting deltas (both update same triple)
const delta1 = createDelta({
  operations: [{ op: 'update', subject: 's', predicate: 'p', oldObject: 'v1', newObject: 'v2' }],
  package: '@unrdf/test',
});

const delta2 = createDelta({
  operations: [{ op: 'update', subject: 's', predicate: 'p', oldObject: 'v1', newObject: 'v3' }],
  package: '@unrdf/test',
});

const merged = composeDeltasParallel(delta1, delta2); // ❌ Conflict!

// ✅ CORRECT - Apply sequentially or resolve conflict
const {receipts} = await composeDeltasSequential(delta1, delta2, store, gate);
```

---

## Testing Template

```javascript
import { describe, it, expect } from 'vitest';

describe('Composition Layer', () => {
  it('composes two L5 modules sequentially', () => {
    const double = (x) => x * 2;
    const increment = (x) => x + 1;

    const context = createFakeContext({ randomSeed: 42, initialTime: 1000n });
    const composed = composeThen(double, increment, context);

    const {result, receipts} = composed(5);

    expect(result).toBe(11); // (5 * 2) + 1
    expect(receipts).toHaveLength(2);
    expect(receipts[1].previousHash).toBe(receipts[0].receiptHash); // Chained
  });

  it('composes two L5 modules in parallel', () => {
    const double = (x) => x * 2;
    const square = (x) => x * x;

    const context = createFakeContext({ randomSeed: 42, initialTime: 1000n });
    const composed = composeAnd(double, square, context);

    const {result, receipts} = composed(5);

    expect(result).toEqual({ a: 10, b: 25 });
    expect(receipts).toHaveLength(3); // 2 operations + 1 merge
  });

  it('composition is deterministic', () => {
    const double = (x) => x * 2;
    const increment = (x) => x + 1;

    const context1 = createFakeContext({ randomSeed: 42, initialTime: 1000n });
    const {receipts: recs1} = composeThen(double, increment, context1)(5);

    const context2 = createFakeContext({ randomSeed: 42, initialTime: 1000n });
    const {receipts: recs2} = composeThen(double, increment, context2)(5);

    expect(recs1[1].receiptHash).toBe(recs2[1].receiptHash); // Same final hash
  });
});
```

---

## Copy-Paste Checklist

- [ ] Copy `composition.mjs` with compose functions
- [ ] Copy `l5-module-template.mjs` for new modules
- [ ] Define input/output schemas with Zod
- [ ] Implement deterministic operation (inject context)
- [ ] Add `operationWithReceipt` wrapper
- [ ] Test sequential composition (then)
- [ ] Test parallel composition (and)
- [ ] Verify receipt chaining (previousHash links)
- [ ] Verify determinism (same context → same receipts)

---

## References

- Composition Theory: Category theory (functors, monads)
- Existing Implementations:
  - Receipt chaining: `/home/user/unrdf/packages/yawl/src/receipt-chain.mjs`
  - Delta composition: `/home/user/unrdf/packages/v6-core/src/delta/`
- L5 Maturity Model: See `/home/user/unrdf/docs/v6-patterns/06-l5-validation-criteria.md`
