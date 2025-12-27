# KnowledgeStore Contract

## Purpose

Defines the interface for KGC-backed knowledge storage with immutable, deterministic operations.

## Input (O)

```typescript
interface KnowledgeStoreInput {
  store: KGCStore; // KGC-4D store instance
  quads: Quad[]; // RDF quads to add
  operation: 'add' | 'delete' | 'query';
  graph?: NamedNode; // Target graph (default: UNIVERSE)
}
```

## Output (A)

```typescript
interface StorageSnapshot {
  before_hash: string; // BLAKE3 hash before operation
  after_hash: string; // BLAKE3 hash after operation
  timestamp_ns: bigint; // Nanosecond precision timestamp
  quad_count: number; // Total quads in store
  delta_size: number; // Quads added/removed
  receipt_id: string; // UUID for this operation
  git_ref?: string; // Git commit ref (if frozen)
}
```

## Invariants (Σ)

### 1. Immutability

```
∀ operation: once hash(state_t) is computed, state_t cannot change
verify: recompute(state_t) === hash(state_t)
```

### 2. Canonical Ordering

```
∀ quad set S: serialize(S) produces deterministic order
sort by: (subject, predicate, object, graph) lexicographically
```

### 3. Hash Commitment

```
after_hash = BLAKE3(canonical_serialize(state_{t+1}))
chain_valid ⟺ ∀i: snapshot[i].after_hash === snapshot[i+1].before_hash
```

### 4. Atomicity

```
operation succeeds ⟹ all deltas applied
operation fails ⟹ no deltas applied (rollback to before_hash)
```

## Usage Example

```javascript
import { createStore } from '@unrdf/oxigraph';
import { appendDeltas } from '@unrdf/kgc-4d';

const store = createStore();
const snapshot = await appendDeltas(store, deltas);

// Verify invariants
assert(snapshot.after_hash.length === 64); // BLAKE3 = 64 hex chars
assert(snapshot.quad_count >= 0);
assert(snapshot.delta_size === deltas.length);
```

## Violation Handling

- **Invalid quad**: Reject with Zod schema error
- **Hash mismatch**: Rollback and throw `HashMismatchError`
- **Storage failure**: Preserve before_hash, mark operation failed
- **Concurrent write**: Use vector clocks for causality, last-write-wins on same VC

## Testing Contract

```javascript
// Test 1: Deterministic hashing
const store1 = createStore();
const store2 = createStore();
await appendDeltas(store1, quads);
await appendDeltas(store2, quads);
assert(hash(store1) === hash(store2));

// Test 2: Chain integrity
const s1 = await appendDeltas(store, batch1);
const s2 = await appendDeltas(store, batch2);
assert(s1.after_hash === s2.before_hash);
```

## Dependencies

- `@unrdf/oxigraph` - Quad store
- `@unrdf/kgc-4d` - Freeze/thaw operations
- `hash-wasm` - BLAKE3 hashing
- `zod` - Schema validation
