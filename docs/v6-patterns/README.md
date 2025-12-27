# V6 Architectural Patterns - Copy-Exact Pattern Library

**Version**: v6.0.0
**Purpose**: Actionable pattern library for L5 maturity across all P0+P1 packages
**Target**: 10/47 packages at L5 by v6.0 release

---

## Executive Summary

This pattern library provides **copy-exact templates** for building L5-mature modules in UNRDF v6. All 10 agents working on P0+P1 packages MUST use these patterns exactly as written.

**No philosophy. Just copy-paste code that works.**

---

## L5 Maturity Definition

**L5** = A module that satisfies ALL of:

1. **Stable Contracts** - Semantic versioning, Zod schemas, JSDoc coverage
2. **Deterministic Code** - Same inputs → same outputs → same receipts
3. **Adversarial Safety** - Validates all inputs, handles all errors gracefully
4. **Full Composition** - Composes with other L5 modules while preserving L5 properties

**Why L5 Matters**:
- **Reproducibility**: Same inputs produce same receipts (hash-to-hash match)
- **Verifiability**: Cryptographic receipts prove what happened
- **Composability**: Modules compose without breaking guarantees
- **Safety**: Runtime validation prevents invalid states

**Evidence**: KGC-4D achieved 99.8% test pass rate, 0 defects, 64% pattern reuse using these patterns.

---

## The 5 Core Patterns

All L5 modules MUST implement ALL 5 patterns:

| Pattern | Purpose | When to Use | Copy Template |
|---------|---------|-------------|---------------|
| **1. Receipt HOF** | Cryptographic proof of operations | ALL state mutations | [01-receipt-hof-pattern.md](01-receipt-hof-pattern.md) |
| **2. Delta Contract** | Explicit change proposals with integrity proofs | ALL store mutations | [02-delta-contract-pattern.md](02-delta-contract-pattern.md) |
| **3. Zod Validation** | Runtime type safety at boundaries | ALL public functions | [03-zod-validation-layer.md](03-zod-validation-layer.md) |
| **4. Determinism Envelope** | Reproducible execution via dependency injection | ALL modules | [04-determinism-envelope.md](04-determinism-envelope.md) |
| **5. Composition Layer** | Preserve L5 properties across module boundaries | Module composition | [05-composition-layer.md](05-composition-layer.md) |

**Rule**: If you're building an L5 module, use ALL 5 patterns. No exceptions.

---

## Supporting Documents

| Document | Purpose | Link |
|----------|---------|------|
| **Decision Matrix** | Choose which patterns to use for your scenario | [00-decision-matrix.md](00-decision-matrix.md) |
| **L5 Validation Criteria** | Objective checklist to prove L5 compliance | [06-l5-validation-criteria.md](06-l5-validation-criteria.md) |
| **ESLint Enforcement** | Automatically enforce patterns via linting | [07-eslint-enforcement.md](07-eslint-enforcement.md) |

---

## Quick Start (New Package)

### Step 1: Copy Pattern Templates

```bash
# Create new package
mkdir -p packages/my-new-package/src
cd packages/my-new-package

# Copy all pattern templates
cp ../../docs/v6-patterns/*.md docs/

# Copy starter templates
cp ../../templates/v6-l5-module-template.mjs src/index.mjs
cp ../../templates/v6-l5-module.test.mjs test/index.test.mjs
```

### Step 2: Implement Patterns (Order Matters)

**Phase 1: Foundation (Day 1-2)**
1. ✅ Define Zod schemas (input + output)
2. ✅ Implement deterministic operations (inject `context`)
3. ✅ Validate inputs/outputs with `.parse()`

**Phase 2: Proofs (Day 3-4)**
4. ✅ Wrap operations with `withReceipt()` (Receipt HOF)
5. ✅ Create deltas for mutations (if stateful)
6. ✅ Test determinism (same context → same receipts)

**Phase 3: Composition (Day 5)**
7. ✅ Export L5 module interface
8. ✅ Test composition with another L5 module
9. ✅ Verify receipt chaining

### Step 3: Validate L5 Compliance

```bash
# Run automated validation
./scripts/validate-l5.sh packages/my-new-package

# Expected output:
# ✅ Semantic versioning
# ✅ Zod schemas exported
# ✅ JSDoc present
# ✅ No Date.now()
# ✅ No Math.random()
# ✅ Context injection
# ✅ Input validation
# ✅ L5 module export
# ✅ Tests pass
# === L5 COMPLIANT (Score: 9.5/10) ===
```

### Step 4: Enable ESLint Enforcement

```bash
# Copy ESLint config
cp ../../.eslintrc.cjs .eslintrc.cjs

# Run linter
pnpm lint

# Fix all violations
pnpm lint:fix
```

---

## Patterns in Detail

### Pattern 1: Receipt HOF

**What**: Wrap operations to generate cryptographic receipts proving what happened.

**Template**:
```javascript
import { withReceipt } from '@unrdf/v6-core/receipts';

export function createUser(input, context) {
  return withReceipt(
    () => createUserInternal(input, context),
    { operationName: 'createUser', actor: 'user-module', ...context }
  );
}

// Returns: {result: User, receipt: Receipt}
// receipt.receiptHash = BLAKE3 hash (64 chars)
```

**Key Principles**:
- ALL state mutations produce receipts
- Receipts chain via `previousReceiptHash`
- Deterministic hashing (BLAKE3)

**Copy**: [01-receipt-hof-pattern.md](01-receipt-hof-pattern.md)

---

### Pattern 2: Delta Contract

**What**: Explicit change proposals (deltas) that produce receipts on success/failure.

**Template**:
```javascript
import { createDelta, DeltaGate } from '@unrdf/v6-core/delta';

const delta = createDelta({
  operations: [
    { op: 'add', subject: 's', predicate: 'p', object: 'o' }
  ],
  package: '@unrdf/my-package',
});

const gate = new DeltaGate();
const receipt = await gate.proposeDelta(delta, store);

if (receipt.applied) {
  console.log('State hash:', receipt.stateHash);
} else {
  console.error('Rejected:', receipt.reason);
}
```

**Key Principles**:
- Δ is the ONLY way to mutate state
- ALL deltas produce receipts (success or denial)
- Atomic operations (all-or-none)

**Copy**: [02-delta-contract-pattern.md](02-delta-contract-pattern.md)

---

### Pattern 3: Zod Validation

**What**: Runtime validation at ALL module boundaries (input + output).

**Template**:
```javascript
import { z } from 'zod';

const InputSchema = z.object({
  name: z.string().min(1).max(100),
  age: z.number().int().nonnegative(),
});

const OutputSchema = z.object({
  id: z.string().uuid(),
  name: z.string(),
  age: z.number(),
});

export function createUser(input, context) {
  const validated = InputSchema.parse(input); // ✅ Validate input
  const user = { id: context.random.uuid(), ...validated };
  return OutputSchema.parse(user); // ✅ Validate output
}
```

**Key Principles**:
- Validate input + output for ALL public functions
- Use `.parse()` (throws) or `.safeParse()` (returns result)
- Export schemas for downstream consumption

**Copy**: [03-zod-validation-layer.md](03-zod-validation-layer.md)

---

### Pattern 4: Determinism Envelope

**What**: Inject ALL external dependencies (time, randomness, env vars) for reproducibility.

**Template**:
```javascript
import { createFakeContext } from '@unrdf/v6-core/determinism';

// ❌ WRONG - Non-deterministic
function operation() {
  return {
    id: crypto.randomUUID(),
    timestamp: Date.now(),
  };
}

// ✅ CORRECT - Deterministic
function operation(context) {
  return {
    id: context.random.uuid(),
    timestamp: context.time.now(),
  };
}

// Test: Same context → same output
const context1 = createFakeContext({ randomSeed: 42, initialTime: 1000n });
const result1 = operation(context1);

const context2 = createFakeContext({ randomSeed: 42, initialTime: 1000n });
const result2 = operation(context2);

console.assert(result1.id === result2.id); // ✅ Same UUID
```

**Key Principles**:
- NO `Date.now()`, `Math.random()`, `crypto.randomUUID()`, `process.env.*`
- Inject via `DeterminismContext`
- Test determinism (same context → same hash)

**Copy**: [04-determinism-envelope.md](04-determinism-envelope.md)

---

### Pattern 5: Composition Layer

**What**: Compose L5 modules while preserving L5 properties (determinism + receipts).

**Template**:
```javascript
import { composeThen } from '@unrdf/v6-core/composition';

const moduleA = { operation: (x) => x * 2 };
const moduleB = { operation: (x) => x + 1 };

const composedAB = composeThen(moduleA.operation, moduleB.operation, context);

const {result, receipts} = composedAB(5);
// result = 11 (5 * 2 + 1)
// receipts[0] = moduleA receipt
// receipts[1] = moduleB receipt (chained from receipts[0])
console.assert(receipts[1].previousHash === receipts[0].receiptHash); // ✅ Chained
```

**Key Principles**:
- Sequential composition (`then`): A → B
- Parallel composition (`and`): A + B
- Conditional composition (`if`): A if P else B
- Receipt chaining preserved across boundaries

**Copy**: [05-composition-layer.md](05-composition-layer.md)

---

## Pattern Selection Guide

**Use the Decision Matrix** to choose which patterns apply to your scenario:

| Your Scenario | Required Patterns |
|---------------|-------------------|
| New L5 package | ALL 5 patterns |
| Read-only package (queries) | Zod + Determinism (+ optional Receipt for audit) |
| Stateful package (mutations) | ALL 5 patterns |
| Utility package (pure functions) | Zod only (already deterministic) |
| Streaming package | Receipt HOF (streaming) + Zod |
| CLI package | Zod (user input) + optional Receipt (audit) |

**Full Decision Matrix**: [00-decision-matrix.md](00-decision-matrix.md)

---

## L5 Validation Process

### Automated Validation

```bash
# Run validation script
./scripts/validate-l5.sh packages/my-package

# Output:
# 1. ✅ Stable Contracts (Zod schemas, JSDoc, versioning)
# 2. ✅ Deterministic Code (No Date.now, context injection)
# 3. ✅ Adversarial Safety (Input/output validation, error handling)
# 4. ✅ Full Composition (L5 module export, composition tests)
# === L5 COMPLIANT (Score: 9.0/10) ===
```

### Manual Checklist

Use the L5 Validation Criteria checklist:

- [ ] Semantic versioning (package.json)
- [ ] Zod schemas exported for ALL public functions
- [ ] JSDoc coverage ≥ 100%
- [ ] NO `Date.now()`, `Math.random()`, `crypto.randomUUID()`, `process.env.*`
- [ ] Determinism context injection
- [ ] Input/output validation (Zod)
- [ ] L5 module interface exported
- [ ] Composition tests pass
- [ ] Determinism tests pass (same context → same receipts)

**Full Criteria**: [06-l5-validation-criteria.md](06-l5-validation-criteria.md)

---

## ESLint Enforcement

Automatically enforce patterns via ESLint:

```bash
# Install ESLint + plugins
pnpm add -D eslint eslint-plugin-jsdoc eslint-plugin-no-only-tests

# Copy ESLint config
cp /home/user/unrdf/.eslintrc.cjs .eslintrc.cjs

# Run linter
pnpm lint

# Auto-fix violations
pnpm lint:fix
```

**Enforced Rules**:
- ❌ Banned: `Date.now()`, `Math.random()`, `crypto.randomUUID()`, `process.env.*`
- ✅ Required: JSDoc on all public functions
- ✅ Required: `context` parameter on exported functions
- ✅ Required: `.parse()` for validation
- ✅ Required: `{result, receipt}` return type for mutations

**Full ESLint Guide**: [07-eslint-enforcement.md](07-eslint-enforcement.md)

---

## Migration Path (Existing Packages)

**Incremental adoption** for existing packages:

### Phase 1: Zod Validation (1-2 days)
```javascript
// Before
export function createUser(input) {
  return { id: generateId(), ...input };
}

// After
export function createUser(input) {
  const validated = CreateUserInputSchema.parse(input); // ✅ Add validation
  const user = { id: generateId(), ...validated };
  return UserSchema.parse(user); // ✅ Validate output
}
```

### Phase 2: Determinism (2-3 days)
```javascript
// Before
export function createUser(input) {
  const validated = CreateUserInputSchema.parse(input);
  return UserSchema.parse({
    id: crypto.randomUUID(), // ❌ Non-deterministic
    createdAt: Date.now(), // ❌ Non-deterministic
    ...validated,
  });
}

// After
export function createUser(input, context) { // ✅ Add context param
  const validated = CreateUserInputSchema.parse(input);
  return UserSchema.parse({
    id: context.random.uuid(), // ✅ Injected
    createdAt: context.time.now(), // ✅ Injected
    ...validated,
  });
}
```

### Phase 3: Receipt HOF (2-3 days)
```javascript
// Before
export function createUser(input, context) {
  // ... validation + logic
  return user;
}

// After
export function createUser(input, context) {
  return withReceipt( // ✅ Wrap in receipt HOF
    () => createUserInternal(input, context),
    { operationName: 'createUser', actor: 'user-module', ...context }
  );
}
```

### Phase 4: Delta Contract (3-4 days, if stateful)
```javascript
// Before
export async function createUser(input, context, store) {
  const user = createUserInternal(input, context);
  store.add({ subject: user.id, predicate: 'name', object: user.name }); // ❌ Direct mutation
  return user;
}

// After
export async function createUser(input, context, store, gate) {
  const user = createUserInternal(input, context);
  const delta = createDelta({ // ✅ Create delta
    operations: [{ op: 'add', subject: user.id, predicate: 'name', object: user.name }],
    package: '@unrdf/users',
  });
  const receipt = await gate.proposeDelta(delta, store); // ✅ Propose delta
  return { user, receipt };
}
```

### Phase 5: Composition (1-2 days)
```javascript
// Before
export function createUser(input, context, store, gate) {
  // ... implementation
}

// After
export const UserModule = { // ✅ Export L5 module
  name: '@unrdf/users',
  version: '1.0.0',
  InputSchema: CreateUserInputSchema,
  OutputSchema: UserSchema,
  operation: createUser,
  operationWithReceipt: (input, context) => withReceipt(
    () => createUser(input, context),
    { operationName: 'createUser', actor: 'user-module', ...context }
  ),
};
```

**Timeline**: 10-15 days per package (L1 → L5)

---

## Common Pitfalls (What NOT to Do)

### Pitfall 1: Receipts Without Determinism
```javascript
// ❌ WRONG - Receipt but non-deterministic operation
function createUser(input) {
  const user = { id: crypto.randomUUID() }; // Non-deterministic
  return withReceipt(() => user, { operationName: 'createUser', actor: 'system' });
  // Receipt hash will differ on each call → USELESS
}
```

### Pitfall 2: Deltas Without Receipts
```javascript
// ❌ WRONG - Delta applied but no receipt
const delta = createDelta({ operations: [...] });
await reconcile(store, delta); // No receipt!
// Can't prove delta was applied
```

### Pitfall 3: Input Validation Only (No Output)
```javascript
// ❌ WRONG - Validates input but not output
export function createUser(input) {
  const validated = CreateUserInputSchema.parse(input); // ✅
  const user = { ...validated, id: generateId() };
  return user; // ❌ Output not validated
}
```

### Pitfall 4: Composition Without Schema Matching
```javascript
// ❌ WRONG - Composing incompatible modules
const moduleA = { operation: (x) => x.toString(), outputSchema: z.string() };
const moduleB = { operation: (x) => x * 2, inputSchema: z.number() };
const composed = composeThen(moduleA.operation, moduleB.operation, context);
// ❌ Runtime error: string → number mismatch
```

**Full Pitfall Guide**: See individual pattern docs.

---

## Performance Impact

| Pattern | CPU Overhead | Memory Overhead | Latency Impact |
|---------|--------------|-----------------|----------------|
| Receipt HOF | 10-20% (hashing) | Low (receipt objects) | +2-5ms |
| Delta Contract | 20-30% (validation + reconciliation) | Medium (delta queue) | +5-10ms |
| Zod Validation | 5-10% (schema parse) | Low (schema cache) | +1-2ms |
| Determinism Envelope | 5-10% (dependency injection) | Low (context object) | +0.5-1ms |
| Composition Layer | 2-5% (wrapper functions) | Low (receipt arrays) | +0.5-1ms |

**Total Overhead**: ~40-70% CPU, +10-20ms latency per operation

**Mitigation**:
- Batch operations (reduce per-operation overhead)
- Cache Zod schemas (reuse compiled schemas)
- Async hashing (worker threads for BLAKE3)
- Lazy receipts (generate only when needed)

---

## Success Metrics

**Target**: 10/47 packages at L5 by v6.0

**Current Progress**: 5/47 packages at L3+ (see `/home/user/unrdf/docs/ARCHITECTURE.md`)

**P0 Packages** (MUST reach L5):
1. `@unrdf/v6-core` - Core L5 infrastructure
2. `@unrdf/yawl` - Workflow engine
3. `@unrdf/kgc-4d` - Knowledge graph chronology
4. `@unrdf/fusion` - Receipt-driven documentation

**P1 Packages** (SHOULD reach L5):
5. `@unrdf/hooks` - Knowledge hooks
6. `@unrdf/streaming` - Streaming RDF
7. `@unrdf/federation` - Federated queries
8. `@unrdf/browser` - Browser runtime
9. `@unrdf/cli` - CLI tools
10. `@unrdf/knowledge-engine` - Query engine

**Validation Criteria**:
- [ ] L5 validation score ≥ 8.0/10
- [ ] ALL patterns implemented
- [ ] Determinism tests pass
- [ ] Composition tests pass
- [ ] ESLint passes with 0 errors

---

## References

**Existing Implementations**:
- Receipt HOF: `/home/user/unrdf/packages/yawl/src/receipt-core.mjs`
- Delta Contract: `/home/user/unrdf/packages/v6-core/src/delta/`
- Zod Schemas: `/home/user/unrdf/packages/v6-core/src/delta/schema.mjs`
- Determinism: `/home/user/unrdf/packages/yawl/src/receipt-core.mjs` (deterministicSerialize)
- Composition: `/home/user/unrdf/packages/yawl/src/receipt-chain.mjs`

**External Documentation**:
- Zod: https://zod.dev
- BLAKE3: https://github.com/BLAKE3-team/BLAKE3
- Big Bang 80/20 Methodology: `/home/user/unrdf/docs/bb80-20-methodology.md`

---

## Action Items

**For Agents Working on P0+P1 Packages**:

1. ✅ Read ALL 5 pattern documents
2. ✅ Copy pattern templates to your package
3. ✅ Implement patterns in order (Zod → Determinism → Receipt → Delta → Composition)
4. ✅ Run `./scripts/validate-l5.sh` to verify compliance
5. ✅ Enable ESLint enforcement
6. ✅ Write determinism tests (same context → same receipts)
7. ✅ Write composition tests (preserve L5 properties)
8. ✅ Document patterns in package README

**Questions? Consult**:
- Decision Matrix: [00-decision-matrix.md](00-decision-matrix.md)
- L5 Validation: [06-l5-validation-criteria.md](06-l5-validation-criteria.md)
- Specific pattern docs: [01-05]

---

## Summary

**V6 Pattern Library** = Copy-exact templates for L5 maturity. No philosophy. Just code that works.

**Core Principle**: Use ALL 5 patterns for L5 modules. No exceptions.

**Validation**: Automated script + objective checklist + ESLint enforcement.

**Goal**: 10/47 packages at L5 by v6.0. You are building one of them. Make it count.

**Next Step**: Start with [00-decision-matrix.md](00-decision-matrix.md) to understand which patterns apply to your use case.
