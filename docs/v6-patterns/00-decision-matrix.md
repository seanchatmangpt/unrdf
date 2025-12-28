# V6 Pattern Decision Matrix

**Version**: v6.0.0
**Purpose**: Choose the right pattern for your use case

---

## Pattern Selection Guide

Use this matrix to decide WHICH pattern(s) to apply for your scenario.

### Quick Reference

| Pattern | When to Use | Primary Benefit | Overhead |
|---------|-------------|-----------------|----------|
| **Receipt HOF** | ALL state mutations | Cryptographic proof of changes | Low (10-20% runtime) |
| **Delta Contract** | ALL store mutations | Explicit change proposals with receipts | Medium (validation + reconciliation) |
| **Zod Validation** | ALL public functions | Runtime type safety | Low (schema parse: ~1ms) |
| **Determinism Envelope** | ALL modules targeting L5 | Reproducibility + verifiable receipts | Medium (dependency injection) |
| **Composition Layer** | Combining ≥2 L5 modules | Preserve L5 properties across boundaries | Low (wrapper overhead) |

---

## Scenario-Based Decisions

### Scenario 1: Building a New Package (P0/P1)

**Goal**: Create a new package that will reach L5 maturity.

**Decision**:
- ✅ **MUST use**: All 5 patterns
  - Receipt HOF: For all state-changing operations
  - Delta Contract: If package mutates RDF store
  - Zod Validation: For all public functions (input + output)
  - Determinism Envelope: For reproducibility
  - Composition Layer: To enable composition with other L5 modules

**Why**: L5 maturity REQUIRES all patterns. No exceptions.

**Template to Copy**:
```bash
# Copy all pattern templates
cp /home/user/unrdf/docs/v6-patterns/*.md packages/my-new-package/docs/
```

---

### Scenario 2: Refactoring Existing Package (L1-L3 → L5)

**Goal**: Upgrade existing package to L5.

**Decision** (prioritized order):
1. ✅ **Start with**: Zod Validation (easiest, highest safety gain)
2. ✅ **Then add**: Determinism Envelope (enables testing)
3. ✅ **Then add**: Receipt HOF (cryptographic proof)
4. ✅ **Then add**: Delta Contract (if stateful)
5. ✅ **Finally**: Composition Layer (enables L5 composition)

**Why**: Incremental adoption minimizes risk. Zod catches bugs immediately. Determinism enables reproducibility. Receipts provide proof. Deltas enforce structure. Composition enables reuse.

**Migration Path**:
```javascript
// Phase 1: Add Zod (1 day)
export function createUser(input) {
  const validated = CreateUserInputSchema.parse(input); // Add this
  // ... existing logic
  return UserSchema.parse(result); // Add this
}

// Phase 2: Add Determinism (2 days)
export function createUser(input, context) { // Add context param
  const validated = CreateUserInputSchema.parse(input);
  const id = context.random.uuid(); // Replace crypto.randomUUID()
  const createdAt = context.time.now(); // Replace Date.now()
  // ...
}

// Phase 3: Add Receipt (2 days)
export function createUser(input, context) {
  return withReceipt( // Wrap in receipt HOF
    () => createUserInternal(input, context),
    { operationName: 'createUser', actor: 'user-module', ...context }
  );
}

// Phase 4: Add Delta (3 days, if stateful)
export async function createUser(input, context, store, gate) {
  const delta = createDelta({
    operations: [{ op: 'add', subject: userId, predicate: 'name', object: input.name }],
    package: '@unrdf/users',
  });
  const receipt = await gate.proposeDelta(delta, store);
  return receipt;
}

// Phase 5: Composition (1 day)
export const UserModule = {
  name: '@unrdf/users',
  version: '1.0.0',
  InputSchema: CreateUserInputSchema,
  OutputSchema: UserSchema,
  operation: createUser,
  operationWithReceipt: (input, context) => withReceipt(...),
};
```

---

### Scenario 3: Read-Only Package (Queries, Parsers)

**Goal**: Package only reads data, never mutates.

**Decision**:
- ✅ **MUST use**: Zod Validation, Determinism Envelope
- ⚠️ **OPTIONAL**: Receipt HOF (if audit trail needed)
- ❌ **NOT NEEDED**: Delta Contract (read-only)

**Why**: Read-only packages still need validation (Zod) and determinism (for reproducibility). Receipts are optional (useful for audit logs). Deltas are not applicable (no mutations).

**Example**: SPARQL Query Engine

```javascript
/**
 * Query Engine - Read-only L5 module
 */
export const QueryEngine = {
  name: '@unrdf/query',
  version: '1.0.0',

  InputSchema: z.object({
    sparql: z.string(),
    bindings: z.record(z.string(), z.string()).optional(),
  }),

  OutputSchema: z.array(z.record(z.string(), z.any())),

  /**
   * Execute Query (deterministic, read-only)
   */
  query(input, context) {
    const validated = this.InputSchema.parse(input);
    const results = executeQueryDeterministic(validated.sparql, validated.bindings);
    return this.OutputSchema.parse(results);
  },

  // Optional: Receipt for audit trail
  queryWithReceipt(input, context) {
    return withReceipt(
      () => this.query(input, context),
      { operationName: 'query', actor: 'query-engine', ...context }
    );
  },
};
```

---

### Scenario 4: Streaming/Real-Time Package

**Goal**: Package processes streaming data (e.g., event log, WebSocket).

**Decision**:
- ✅ **MUST use**: Receipt HOF (streaming variant), Zod Validation
- ⚠️ **OPTIONAL**: Delta Contract (if streaming mutations)
- ⚠️ **CHALLENGE**: Determinism Envelope (streaming is often non-deterministic)

**Why**: Streaming data is inherently non-deterministic (arrival time varies). Use Receipt HOF to create a receipt PER CHUNK for audit trail. Zod validates each chunk. Determinism is hard (may need to relax or use content hashing).

**Example**: Event Stream Processor

```javascript
/**
 * Process Event Stream - With receipts per chunk
 */
export async function* processEventStream(stream, context) {
  let previousReceiptHash = null;

  for await (const chunk of stream) {
    // Validate chunk
    const validatedChunk = EventChunkSchema.parse(chunk);

    // Process chunk (deterministic on chunk content)
    const result = processChunk(validatedChunk, context);

    // Generate receipt for chunk
    const {receipt} = withReceipt(
      () => result,
      {
        operationName: 'process_chunk',
        actor: 'stream-processor',
        previousReceiptHash,
        ...context,
      }
    );

    yield { result, receipt };
    previousReceiptHash = receipt.receiptHash; // Chain next receipt
  }
}
```

---

### Scenario 5: CLI Package (User Interaction)

**Goal**: Package provides CLI commands (interactive, user input).

**Decision**:
- ✅ **MUST use**: Zod Validation (user input is untrusted)
- ⚠️ **OPTIONAL**: Receipt HOF (if commands mutate state)
- ❌ **NOT NEEDED**: Determinism Envelope (user input is non-deterministic)

**Why**: CLI packages are inherently non-deterministic (user input varies). Zod validates user input (security). Receipts are useful if commands mutate state (audit log). Full determinism is not realistic (user input is non-deterministic).

**Example**: CLI Command

```javascript
/**
 * CLI Command: Create User
 */
export async function createUserCommand(args) {
  // Validate CLI args (Zod)
  const validated = CreateUserArgsSchema.parse(args);

  // Create user (with receipt for audit)
  const context = createRealContext(); // Real context (non-deterministic)
  const {result, receipt} = createUserWithReceipt(validated, context);

  console.log('User created:', result.id);
  console.log('Receipt:', receipt.receiptHash);
}
```

---

### Scenario 6: Utility Package (Pure Functions)

**Goal**: Package provides pure utility functions (math, string manipulation).

**Decision**:
- ✅ **MUST use**: Zod Validation (if public API)
- ⚠️ **OPTIONAL**: Determinism Envelope (already deterministic)
- ❌ **NOT NEEDED**: Receipt HOF (no state changes), Delta Contract (no mutations)

**Why**: Pure functions are already deterministic. Zod validates inputs/outputs. Receipts and Deltas are not applicable (no state).

**Example**: Math Utilities

```javascript
/**
 * Math Utilities - Pure L5 module
 */
export const MathUtils = {
  name: '@unrdf/math-utils',
  version: '1.0.0',

  AddInputSchema: z.object({ x: z.number(), y: z.number() }),
  AddOutputSchema: z.number(),

  add(input) {
    const validated = this.AddInputSchema.parse(input);
    const result = validated.x + validated.y;
    return this.AddOutputSchema.parse(result);
  },
};
```

---

## Pattern Overhead Analysis

### Performance Impact

| Pattern | CPU Overhead | Memory Overhead | Latency Impact |
|---------|--------------|-----------------|----------------|
| Receipt HOF | 10-20% (hashing) | Low (receipt objects) | +2-5ms per operation |
| Delta Contract | 20-30% (validation + reconciliation) | Medium (delta queue) | +5-10ms per delta |
| Zod Validation | 5-10% (schema parse) | Low (schema cache) | +1-2ms per validation |
| Determinism Envelope | 5-10% (dependency injection) | Low (context object) | +0.5-1ms |
| Composition Layer | 2-5% (wrapper functions) | Low (receipt arrays) | +0.5-1ms |

**Total Overhead** (all patterns): ~40-70% CPU, +10-20ms latency per operation

**Mitigation Strategies**:
1. **Batch operations**: Reduce per-operation overhead
2. **Cache schemas**: Reuse compiled Zod schemas
3. **Async hashing**: Use worker threads for BLAKE3
4. **Lazy receipts**: Generate receipts only when needed

---

## Anti-Patterns (What NOT to Do)

### Anti-Pattern 1: Receipts Without Determinism

```javascript
// ❌ WRONG - Receipts but non-deterministic operation
function createUser(input) {
  const user = {
    id: crypto.randomUUID(), // ❌ Non-deterministic
    createdAt: Date.now(), // ❌ Non-deterministic
  };

  return withReceipt(() => user, { operationName: 'createUser', actor: 'system' });
  // Receipt hash will differ on each call → USELESS
}
```

**Fix**: Add Determinism Envelope (inject randomness + time).

### Anti-Pattern 2: Deltas Without Receipts

```javascript
// ❌ WRONG - Delta applied but no receipt
const delta = createDelta({ operations: [...] });
await reconcile(store, delta); // No receipt generated!
// Can't prove delta was applied
```

**Fix**: Use DeltaGate which ALWAYS generates receipts.

### Anti-Pattern 3: Validation Only on Input (Not Output)

```javascript
// ❌ WRONG - Validates input but not output
export function createUser(input) {
  const validated = CreateUserInputSchema.parse(input); // ✅ Input validated
  const user = { ...validated, id: generateId() };
  return user; // ❌ Output NOT validated
}
```

**Fix**: Validate output with `UserSchema.parse(user)`.

### Anti-Pattern 4: Composition Without Schema Matching

```javascript
// ❌ WRONG - Composing modules with incompatible schemas
const moduleA = { operation: (x) => x.toString(), outputSchema: z.string() };
const moduleB = { operation: (x) => x * 2, inputSchema: z.number() };

const composed = composeThen(moduleA.operation, moduleB.operation, context);
// ❌ Runtime error: string → number mismatch
```

**Fix**: Add adapter or ensure schemas match.

---

## Pattern Compatibility Matrix

| Pattern A | Pattern B | Compatible? | Notes |
|-----------|-----------|-------------|-------|
| Receipt HOF | Delta Contract | ✅ YES | Deltas produce receipts (synergy) |
| Receipt HOF | Zod Validation | ✅ YES | Validate before receipting (recommended order) |
| Receipt HOF | Determinism Envelope | ✅ YES | Determinism enables reproducible receipts |
| Delta Contract | Zod Validation | ✅ YES | Validate delta schema before application |
| Delta Contract | Determinism Envelope | ✅ YES | Deterministic deltas enable testing |
| Zod Validation | Determinism Envelope | ✅ YES | Validate context + inputs |
| Composition Layer | All patterns | ✅ YES | Composition requires all L5 patterns |

**Key Insight**: All patterns are designed to work together. Use ALL for L5 maturity.

---

## Decision Tree

```
START: What are you building?

├─ New package (P0/P1)?
│  └─ Use ALL 5 patterns (L5 requirement)
│
├─ Refactoring existing package?
│  ├─ Read-only?
│  │  └─ Use: Zod + Determinism (+ optional Receipt for audit)
│  ├─ Stateful?
│  │  └─ Use: ALL 5 patterns (incremental adoption: Zod → Determinism → Receipt → Delta → Composition)
│  └─ Utility (pure functions)?
│     └─ Use: Zod only (already deterministic)
│
├─ Streaming/Real-time?
│  └─ Use: Receipt HOF (streaming) + Zod (+ optional Delta if mutations)
│
├─ CLI/Interactive?
│  └─ Use: Zod (user input) + optional Receipt (audit)
│
└─ Library/Framework?
   └─ Use: ALL 5 patterns (enable downstream L5 composition)
```

---

## Summary

**Golden Rule**: When in doubt, use ALL 5 patterns. They are designed to work together and provide maximum safety, reproducibility, and composability.

**Minimum Viable L5**:
- Zod Validation (type safety)
- Determinism Envelope (reproducibility)
- Receipt HOF (proof)

**Full L5** (for stateful packages):
- ALL 5 patterns

**Quick Start**:
1. Copy `/home/user/unrdf/docs/v6-patterns/*.md` to your package
2. Start with Zod (validate inputs + outputs)
3. Add Determinism (inject context)
4. Add Receipt (wrap operations)
5. Add Delta (if stateful)
6. Add Composition (export L5 module interface)
