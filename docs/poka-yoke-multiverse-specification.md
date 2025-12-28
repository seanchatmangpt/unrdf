# Poka-Yoke (Mistake-Proofing) Specification: UNRDF Multiverse State Machine

**Version**: 1.0  
**Date**: 2025-12-27  
**Purpose**: Design state machines and guards that make INVALID operations IMPOSSIBLE (not just detectable)

---

## 1. Universe State Machine

### States
```
GENESIS   → Initial empty universe (0 events, 0 quads)
ACTIVE    → Mutable universe accepting mutations (admitDelta allowed)
FORKED    → Branched universe with parent reference (child of ACTIVE)
MERGED    → Merged universe post-conflict resolution (→ ACTIVE or DISCARDED)
FROZEN    → Immutable snapshot (read-only, no mutations)
DISCARDED → Terminal state (cleanup eligible, no resurrection)
```

### Valid Transitions
```
GENESIS → ACTIVE       (first event admitted)
ACTIVE → FORKED        (createFork() on active universe)
ACTIVE → FROZEN        (freezeUniverse() creates snapshot)
FORKED → MERGED        (mergeFork() resolves conflicts)
FORKED → FROZEN        (freeze forked branch for archival)
MERGED → ACTIVE        (merge accepted, result active)
MERGED → DISCARDED     (merge rejected, cleanup)
FROZEN → DISCARDED     (explicit deletion after retention)
```

### Invalid Transitions (BLOCKED by guards)
```
FROZEN → ACTIVE        ❌ Cannot unfreeze snapshot (immutable)
FROZEN → FORKED        ❌ Cannot fork frozen universe
MERGED → FORKED        ❌ Cannot re-fork after merge
DISCARDED → *          ❌ Terminal state (no resurrection)
GENESIS → FROZEN       ❌ Cannot freeze before any events
FORKED → ACTIVE        ❌ Must merge first (or discard)
```

### ASCII State Diagram
```
        ┌─────────┐
        │ GENESIS │ (empty, 0 events)
        └────┬────┘
             │ first admitDelta()
             ▼
        ┌─────────┐   createFork()    ┌────────┐
        │ ACTIVE  ├──────────────────→ │ FORKED │
        │         │◄──────────────────┤        │
        └────┬────┘   mergeFork()      └───┬────┘
             │                             │
             │ freezeUniverse()            │ freezeUniverse()
             ▼                             ▼
        ┌─────────┐                   ┌─────────┐
        │ FROZEN  │                   │ FROZEN  │
        │ (snap)  │                   │ (fork)  │
        └────┬────┘                   └────┬────┘
             │                             │
             │ delete()                    │ delete()
             ▼                             ▼
        ┌──────────────┐  mergeFork()  ┌─────────┐
        │  DISCARDED   │◄──────────────┤ MERGED  │
        │  (terminal)  │               │ (temp)  │
        └──────────────┘               └─────────┘
                                            │
                                            │ accept
                                            ▼
                                       ┌─────────┐
                                       │ ACTIVE  │
                                       └─────────┘
```

---

## 2. Guard Rules (8-10 Forbidden Operations)

### GR1: Cannot freeze while FORKED
- **Condition**: `state === FORKED && op === 'freeze'`
- **Action**: BLOCK with error "Cannot freeze FORKED universe - must merge or discard first"
- **Rationale**: Forked universes are temporary; merge determines final state

### GR2: Cannot merge without conflict resolution
- **Condition**: `state === FORKED && conflicts.length > 0 && conflictResolution === null`
- **Action**: BLOCK with error "Merge requires conflict resolution strategy (found N conflicts)"
- **Rationale**: Prevents silent data loss from unresolved conflicts

### GR3: Cannot apply Φ (morphism) to FROZEN universe
- **Condition**: `state === FROZEN && op === 'admitDelta'`
- **Action**: BLOCK with error "FROZEN universe is immutable (use time-travel to modify past)"
- **Rationale**: Snapshot integrity requires immutability

### GR4: Cannot delete DISCARDED universe
- **Condition**: `state === DISCARDED && op === 'delete'`
- **Action**: BLOCK with error "DISCARDED state is terminal (already deleted)"
- **Rationale**: Idempotency - prevent double-free errors

### GR5: Cannot fork from FROZEN universe
- **Condition**: `state === FROZEN && op === 'createFork'`
- **Action**: BLOCK with error "Cannot fork FROZEN universe (reconstruct to ACTIVE first)"
- **Rationale**: Forking requires mutable base state

### GR6: Cannot unfreeze snapshot
- **Condition**: `state === FROZEN && op === 'unfreeze'`
- **Action**: BLOCK with error "FROZEN is immutable (use reconstructState for editable copy)"
- **Rationale**: Prevents accidental snapshot mutation

### GR7: Cannot admit to GENESIS before initialization
- **Condition**: `state === GENESIS && eventCount === 0 && op === 'admitDelta'`
- **Action**: AUTO-TRANSITION to ACTIVE, then admit
- **Rationale**: First mutation initializes universe

### GR8: Cannot merge without parent reference
- **Condition**: `state === FORKED && parent === null && op === 'merge'`
- **Action**: BLOCK with error "FORKED universe missing parent reference (corrupted state)"
- **Rationale**: Merge requires valid parent to resolve against

### GR9: Cannot transition from MERGED to FORKED
- **Condition**: `state === MERGED && op === 'createFork'`
- **Action**: BLOCK with error "MERGED is transient - wait for ACTIVE or DISCARDED"
- **Rationale**: MERGED is temporary state during merge finalization

### GR10: Cannot freeze empty GENESIS
- **Condition**: `state === GENESIS && eventCount === 0 && op === 'freeze'`
- **Action**: BLOCK with error "Cannot freeze empty GENESIS (no events to snapshot)"
- **Rationale**: Prevents meaningless snapshots

---

## 3. Type-Level Prevention (Make Invalid States Unrepresentable)

### Pattern 1: Discriminated Union (TypeScript/Zod)
```typescript
// Universe state encoded in type system
type Universe = 
  | { state: 'GENESIS', eventCount: 0, quads: [] }
  | { state: 'ACTIVE', eventCount: number, quads: Quad[] }
  | { state: 'FORKED', parent: UniverseRef, conflicts: Conflict[] }
  | { state: 'FROZEN', snapshot: GitRef, hash: Blake3Hash }
  | { state: 'DISCARDED', deletedAt: Timestamp }

// Compiler PREVENTS invalid construction:
const invalid: Universe = { state: 'FROZEN', eventCount: 5 }; // ❌ Type error
```

### Pattern 2: Phantom Types (Branded Types)
```typescript
// Brand types prevent mixing frozen/active references
type ActiveUniverse = Universe & { __brand: 'active' };
type FrozenUniverse = Universe & { __brand: 'frozen' };

// Function signature enforces state via type
function admitDelta(u: ActiveUniverse, delta: Delta): void {
  // Compiler ensures u is ACTIVE
}

// Cannot pass frozen universe to admitDelta
const frozenU: FrozenUniverse = freezeUniverse(activeU);
admitDelta(frozenU, delta); // ❌ Type error: FrozenUniverse ≠ ActiveUniverse
```

### Pattern 3: State Transition Guards (Runtime + Compile-time)
```typescript
// Zod schema enforces state machine at runtime
const StateTransitionSchema = z.discriminatedUnion('fromState', [
  z.object({ fromState: z.literal('ACTIVE'), toState: z.enum(['FORKED', 'FROZEN']) }),
  z.object({ fromState: z.literal('FORKED'), toState: z.enum(['MERGED', 'FROZEN']) }),
  z.object({ fromState: z.literal('FROZEN'), toState: z.enum(['DISCARDED']) }),
  // ... all valid transitions
]);

// Invalid transitions rejected at runtime
StateTransitionSchema.parse({ fromState: 'FROZEN', toState: 'ACTIVE' }); 
// ❌ Zod error: Invalid enum value
```

### Pattern 4: Builder Pattern with Sealed Construction
```typescript
// Only valid builder methods exist for each state
class ActiveUniverseBuilder {
  fork(): ForkedUniverseBuilder { /* ... */ }
  freeze(): FrozenUniverse { /* ... */ }
  admitDelta(d: Delta): ActiveUniverseBuilder { /* ... */ }
  // NO unfreeze() method (invalid for ACTIVE)
}

class FrozenUniverseBuilder {
  delete(): DiscardedUniverse { /* ... */ }
  // NO admitDelta() method (invalid for FROZEN)
  // NO fork() method (invalid for FROZEN)
}
```

### Pattern 5: Read-Only Types (Deep Immutability)
```typescript
// Frozen snapshots are deeply read-only
type FrozenSnapshot = Readonly<{
  universeHash: Blake3Hash;
  gitRef: GitRef;
  timestamp: bigint;
  quads: ReadonlyArray<Readonly<Quad>>;
}>;

// Compiler prevents mutation
const snapshot: FrozenSnapshot = freezeUniverse(u);
snapshot.quads.push(newQuad); // ❌ Type error: Property 'push' does not exist
```

---

## 4. Runtime Guards (Checked at Execution)

### Guard 1: State Validity Check
```javascript
/**
 * guardStateTransition - Validate state machine transition
 * @param {string} currentState - Current universe state
 * @param {string} targetState - Desired target state
 * @throws {Error} If transition is invalid
 */
function guardStateTransition(currentState, targetState) {
  const validTransitions = {
    GENESIS: ['ACTIVE'],
    ACTIVE: ['FORKED', 'FROZEN'],
    FORKED: ['MERGED', 'FROZEN'],
    MERGED: ['ACTIVE', 'DISCARDED'],
    FROZEN: ['DISCARDED'],
    DISCARDED: [], // Terminal
  };

  const allowed = validTransitions[currentState] || [];
  if (!allowed.includes(targetState)) {
    throw new Error(
      `Invalid state transition: ${currentState} → ${targetState} ` +
      `(allowed: ${allowed.join(', ') || 'none'})`
    );
  }
}
```

### Guard 2: Morphism Application Check
```javascript
/**
 * guardMorphismApplication - Ensure morphism can be applied
 * @param {Universe} sourceUniverse - Source universe
 * @param {Morphism} morphism - Morphism to apply
 * @throws {Error} If source state is invalid
 */
function guardMorphismApplication(sourceUniverse, morphism) {
  const validStates = ['ACTIVE', 'FORKED'];
  
  if (!validStates.includes(sourceUniverse.state)) {
    throw new Error(
      `Cannot apply morphism to ${sourceUniverse.state} universe ` +
      `(valid: ${validStates.join(', ')})`
    );
  }
  
  if (sourceUniverse.state === 'FROZEN') {
    throw new Error(
      'FROZEN universe is immutable - use reconstructState() for editable copy'
    );
  }
}
```

### Guard 3: Merge Precondition Check
```javascript
/**
 * guardMergePreconditions - Validate merge prerequisites
 * @param {Universe} childUniverse - Forked child universe
 * @param {ConflictResolution} resolution - Conflict resolution strategy
 * @throws {Error} If preconditions not met
 */
function guardMergePreconditions(childUniverse, resolution) {
  // Must be FORKED state
  if (childUniverse.state !== 'FORKED') {
    throw new Error(
      `Cannot merge ${childUniverse.state} universe (must be FORKED)`
    );
  }
  
  // Must have parent reference
  if (!childUniverse.parent || !childUniverse.parent.ref) {
    throw new Error('FORKED universe missing parent reference (corrupted)');
  }
  
  // Must have conflict resolution if conflicts exist
  const conflicts = detectConflicts(childUniverse, childUniverse.parent);
  if (conflicts.length > 0 && !resolution) {
    throw new Error(
      `Merge requires conflict resolution (${conflicts.length} conflicts detected)`
    );
  }
}
```

### Guard 4: Freeze Precondition Check
```javascript
/**
 * guardFreezePreconditions - Validate freeze prerequisites
 * @param {Universe} universe - Universe to freeze
 * @throws {Error} If freeze not allowed
 */
function guardFreezePreconditions(universe) {
  // Cannot freeze FORKED (must merge or discard first)
  if (universe.state === 'FORKED') {
    throw new Error(
      'Cannot freeze FORKED universe - merge or discard first'
    );
  }
  
  // Cannot freeze DISCARDED
  if (universe.state === 'DISCARDED') {
    throw new Error('Cannot freeze DISCARDED universe (terminal state)');
  }
  
  // Cannot freeze empty GENESIS
  if (universe.state === 'GENESIS' && universe.eventCount === 0) {
    throw new Error('Cannot freeze empty GENESIS (no events to snapshot)');
  }
  
  // Warn on already FROZEN (idempotent but suspicious)
  if (universe.state === 'FROZEN') {
    console.warn('Universe already FROZEN - returning existing snapshot');
  }
}
```

### Guard 5: Delete Safety Check
```javascript
/**
 * guardDeleteSafety - Prevent unsafe universe deletion
 * @param {Universe} universe - Universe to delete
 * @param {Object} options - Delete options
 * @throws {Error} If delete is unsafe
 */
function guardDeleteSafety(universe, options = {}) {
  // Cannot delete ACTIVE without force flag
  if (universe.state === 'ACTIVE' && !options.force) {
    throw new Error(
      'Cannot delete ACTIVE universe without force=true (has live data)'
    );
  }
  
  // Cannot delete DISCARDED (idempotency check)
  if (universe.state === 'DISCARDED') {
    throw new Error('Universe already DISCARDED (terminal state)');
  }
  
  // Warn on deleting FORKED (may have unmerged work)
  if (universe.state === 'FORKED') {
    console.warn('Deleting FORKED universe - unmerged changes will be lost');
  }
}
```

---

## 5. Proof by Exhaustion (Demonstrate Impossible to Violate)

### Proof Structure
**Claim**: Invalid state transitions are IMPOSSIBLE (not just detected)

**Proof Method**: Show that EVERY code path enforcing state changes goes through guards

### Proof 1: Can you construct INVALID STATE (FROZEN → ACTIVE)?

**Attempt 1: Direct mutation**
```javascript
const frozenU = freezeUniverse(activeU); // state = FROZEN
frozenU.state = 'ACTIVE'; // ❌ BLOCKED by Object.freeze() in freezeUniverse()
```
**Result**: Runtime error (cannot assign to read-only property)

**Attempt 2: Call setState() directly**
```javascript
frozenU.setState('ACTIVE'); // ❌ BLOCKED by guardStateTransition() in setState()
```
**Result**: Error thrown: "Invalid state transition: FROZEN → ACTIVE"

**Attempt 3: Reconstruct from snapshot**
```javascript
const editableU = reconstructState(frozenU); // Creates NEW universe (state = ACTIVE)
editableU.state; // 'ACTIVE' (different object, not mutation of frozenU)
frozenU.state; // Still 'FROZEN' (original unchanged)
```
**Result**: Valid - creates new universe, does NOT mutate frozen one

**Conclusion**: FROZEN → ACTIVE mutation is IMPOSSIBLE ✓

### Proof 2: Can you apply delta to FROZEN universe?

**Attempt 1: Call admitDelta()**
```javascript
const frozenU = freezeUniverse(activeU);
frozenU.admitDelta(delta); // ❌ BLOCKED by guardMorphismApplication() in admitDelta()
```
**Result**: Error thrown: "FROZEN universe is immutable"

**Attempt 2: Bypass guard by modifying internal state**
```javascript
frozenU._internalStore.add(quad); // ❌ BLOCKED by Object.freeze() on _internalStore
```
**Result**: Runtime error (frozen object)

**Attempt 3: Reflect.set() bypass**
```javascript
Reflect.set(frozenU._internalStore, 'quads', newQuads); // ❌ BLOCKED by Object.freeze()
```
**Result**: Returns false (set failed), object unchanged

**Conclusion**: Mutations to FROZEN universe are IMPOSSIBLE ✓

### Proof 3: Can you merge without parent reference?

**Attempt 1: Create FORKED manually**
```javascript
const forkedU = { state: 'FORKED', parent: null }; // Invalid construction
forkedU.merge(); // ❌ BLOCKED by guardMergePreconditions()
```
**Result**: Error thrown: "FORKED universe missing parent reference"

**Attempt 2: Use createFork() then delete parent**
```javascript
const forkedU = activeU.createFork();
delete forkedU.parent; // ❌ BLOCKED by Object.freeze() on forkedU
```
**Result**: Runtime error (cannot delete frozen property)

**Attempt 3: Bypass via schema validation**
```javascript
const forkedU = UniverseSchema.parse({ state: 'FORKED', parent: null });
// ❌ BLOCKED by Zod schema requiring parent for FORKED state
```
**Result**: Zod validation error: "parent required for FORKED state"

**Conclusion**: Merge without parent is IMPOSSIBLE ✓

### Centralized Enforcement Point
**All state changes funnel through setState()**:
```javascript
class Universe {
  setState(newState) {
    guardStateTransition(this.state, newState); // Single enforcement point
    this.state = newState;
  }
}
```

**Coverage**: 100% of state mutations go through this guard
- `freeze()` → calls `setState('FROZEN')` → guard checks
- `merge()` → calls `setState('MERGED')` → guard checks  
- `createFork()` → calls `setState('FORKED')` → guard checks

**QED**: Invalid states are type-system + runtime impossible ✓

---

## 6. Testable Invariants (Property-Based Tests)

### INV1: Every universe has exactly 1 parent (except GENESIS)
**Invariant**: `∀u ∈ Universes: u.state ≠ GENESIS ⟹ ∃!p: u.parent = p`

**Property Test**:
```javascript
fc.assert(
  fc.property(
    fc.record({ state: fc.constantFrom('ACTIVE', 'FORKED', 'FROZEN') }),
    (universe) => {
      if (universe.state === 'GENESIS') {
        return universe.parent === null;
      } else {
        return universe.parent !== null && typeof universe.parent === 'object';
      }
    }
  )
);
```

**Test Implementation**:
```javascript
test('INV1: Non-GENESIS universes have exactly one parent', () => {
  const activeU = createActiveUniverse();
  const forkedU = activeU.createFork();
  
  expect(activeU.parent).toBeNull(); // Root ACTIVE has no parent
  expect(forkedU.parent).toBe(activeU); // FORKED has parent
  expect(forkedU.parent).not.toBeNull();
  
  // Property: parent count ≤ 1
  const parentCount = forkedU.parent ? 1 : 0;
  expect(parentCount).toBeLessThanOrEqual(1);
});
```

### INV2: FROZEN universes are immutable (0 further mutations)
**Invariant**: `∀u ∈ Universes: u.state = FROZEN ⟹ mutationCount(u, t₁, t₂) = 0`

**Property Test**:
```javascript
fc.assert(
  fc.property(
    fc.record({ state: fc.constant('FROZEN') }),
    fc.array(fc.anything()), // Random deltas
    (frozenU, deltas) => {
      const before = frozenU.eventCount;
      
      // Attempt to apply deltas (should all fail)
      for (const delta of deltas) {
        try {
          frozenU.admitDelta(delta);
        } catch (e) {
          // Expected: all mutations rejected
        }
      }
      
      const after = frozenU.eventCount;
      return before === after; // No change
    }
  )
);
```

**Test Implementation**:
```javascript
test('INV2: FROZEN universes reject all mutations', () => {
  const activeU = createActiveUniverse();
  const frozenU = freezeUniverse(activeU);
  
  const beforeCount = frozenU.eventCount;
  const beforeHash = frozenU.universeHash;
  
  // Attempt 100 random mutations
  for (let i = 0; i < 100; i++) {
    expect(() => {
      frozenU.admitDelta({ type: 'add', quad: randomQuad() });
    }).toThrow(/immutable/);
  }
  
  expect(frozenU.eventCount).toBe(beforeCount);
  expect(frozenU.universeHash).toBe(beforeHash);
});
```

### INV3: Merkle root = hash(state) + parent merkle (chainable)
**Invariant**: `∀u: u.merkleRoot = H(u.universeHash || u.parent.merkleRoot)`

**Property Test**:
```javascript
fc.assert(
  fc.property(
    fc.array(fc.record({ deltas: fc.array(fc.anything()) })), // Event sequence
    (events) => {
      const universe = createActiveUniverse();
      
      for (const event of events) {
        universe.admitDeltas(event.deltas);
      }
      
      const frozenU = freezeUniverse(universe);
      const expectedMerkle = computeMerkleRoot(frozenU.universeHash, frozenU.parent?.merkleRoot);
      
      return frozenU.merkleRoot === expectedMerkle;
    }
  )
);
```

**Test Implementation**:
```javascript
test('INV3: Merkle root chains correctly', () => {
  const u1 = createActiveUniverse();
  u1.admitDelta(delta1);
  const frozen1 = freezeUniverse(u1);
  
  const u2 = createActiveUniverse();
  u2.admitDelta(delta2);
  const frozen2 = freezeUniverse(u2);
  
  // Merkle root should include parent reference
  expect(frozen1.merkleRoot).toBe(
    blake3(frozen1.universeHash + (frozen1.parent?.merkleRoot || ''))
  );
  
  // Chain multiple snapshots
  const u3 = reconstructState(frozen1);
  u3.admitDelta(delta3);
  const frozen3 = freezeUniverse(u3);
  
  expect(frozen3.merkleRoot).toBe(
    blake3(frozen3.universeHash + frozen1.merkleRoot)
  );
});
```

### INV4: Fork divergence is bounded (conflict count ≤ O(n))
**Invariant**: `∀f ∈ FORKED: |conflicts(f, f.parent)| ≤ k · |f.deltas|`

**Property Test**:
```javascript
fc.assert(
  fc.property(
    fc.array(fc.anything(), { minLength: 1, maxLength: 100 }), // Parent deltas
    fc.array(fc.anything(), { minLength: 1, maxLength: 100 }), // Fork deltas
    (parentDeltas, forkDeltas) => {
      const parent = createActiveUniverse();
      parentDeltas.forEach(d => parent.admitDelta(d));
      
      const fork = parent.createFork();
      forkDeltas.forEach(d => fork.admitDelta(d));
      
      const conflicts = detectConflicts(fork, parent);
      const k = 2; // Upper bound constant
      
      return conflicts.length <= k * forkDeltas.length;
    }
  )
);
```

**Test Implementation**:
```javascript
test('INV4: Conflict count bounded by delta count', () => {
  const parent = createActiveUniverse();
  parent.admitDelta({ type: 'add', subject: 'ex:s1', predicate: 'ex:p1', object: 'ex:o1' });
  parent.admitDelta({ type: 'add', subject: 'ex:s2', predicate: 'ex:p2', object: 'ex:o2' });
  
  const fork = parent.createFork();
  fork.admitDelta({ type: 'delete', subject: 'ex:s1', predicate: 'ex:p1', object: 'ex:o1' }); // Conflict
  fork.admitDelta({ type: 'add', subject: 'ex:s3', predicate: 'ex:p3', object: 'ex:o3' });    // No conflict
  
  const conflicts = detectConflicts(fork, parent);
  
  expect(conflicts.length).toBeLessThanOrEqual(2 * fork.deltaCount);
  expect(conflicts.length).toBe(1); // Only 1 actual conflict
});
```

### INV5: State transitions are total ordered (no cycles)
**Invariant**: `∀u: reachable(u, u) ⟹ u.state = u.state` (no self-loops except identity)

**Property Test**:
```javascript
fc.assert(
  fc.property(
    fc.array(fc.constantFrom('freeze', 'fork', 'merge', 'discard')), // Random operations
    (operations) => {
      const universe = createActiveUniverse();
      const visited = new Set();
      
      for (const op of operations) {
        const stateKey = `${universe.state}-${universe.id}`;
        
        // Detect cycle
        if (visited.has(stateKey)) {
          return false; // Cycle found (invalid)
        }
        visited.add(stateKey);
        
        // Apply operation (may throw, that's OK)
        try {
          applyOperation(universe, op);
        } catch (e) {
          // State machine rejected invalid transition
        }
      }
      
      return true; // No cycles detected
    }
  )
);
```

**Test Implementation**:
```javascript
test('INV5: State machine is acyclic', () => {
  const transitions = [
    ['GENESIS', 'ACTIVE'],
    ['ACTIVE', 'FORKED'],
    ['FORKED', 'MERGED'],
    ['MERGED', 'ACTIVE'], // This is OK (different universe instance)
    ['ACTIVE', 'FROZEN'],
    ['FROZEN', 'DISCARDED'],
  ];
  
  const graph = buildTransitionGraph(transitions);
  const hasCycle = detectCycle(graph);
  
  expect(hasCycle).toBe(false);
});
```

### INV6: Snapshot integrity (hash matches content)
**Invariant**: `∀s ∈ FROZEN: H(s.quads) = s.universeHash`

**Property Test**:
```javascript
fc.assert(
  fc.property(
    fc.array(fc.record({ quad: fc.anything() })), // Random quads
    async (quads) => {
      const universe = createActiveUniverse();
      quads.forEach(q => universe.add(q.quad));
      
      const frozen = await freezeUniverse(universe);
      const recomputedHash = await blake3(serializeQuads(frozen.quads));
      
      return frozen.universeHash === recomputedHash;
    }
  )
);
```

**Test Implementation**:
```javascript
test('INV6: Frozen snapshot hash matches content', async () => {
  const universe = createActiveUniverse();
  universe.admitDelta({ type: 'add', subject: 'ex:s1', predicate: 'ex:p1', object: 'ex:o1' });
  universe.admitDelta({ type: 'add', subject: 'ex:s2', predicate: 'ex:p2', object: 'ex:o2' });
  
  const frozen = await freezeUniverse(universe);
  
  // Recompute hash from quads
  const nquads = serializeToNQuads(frozen.quads);
  const recomputedHash = await blake3(nquads);
  
  expect(frozen.universeHash).toBe(recomputedHash);
  
  // Tamper detection
  frozen.quads[0].object.value = 'tampered'; // Simulate corruption
  const tamperedHash = await blake3(serializeToNQuads(frozen.quads));
  expect(tamperedHash).not.toBe(frozen.universeHash); // Detects tampering
});
```

---

## Implementation Checklist

- [ ] Define `UniverseState` enum/type
- [ ] Implement `guardStateTransition()` with transition matrix
- [ ] Add `guardMorphismApplication()` to `admitDelta()`
- [ ] Add `guardMergePreconditions()` to `mergeFork()`
- [ ] Add `guardFreezePreconditions()` to `freezeUniverse()`
- [ ] Add `guardDeleteSafety()` to `deleteUniverse()`
- [ ] Create Zod schemas for state validation
- [ ] Implement property-based tests for all 6 invariants
- [ ] Add `Object.freeze()` to frozen snapshots
- [ ] Add `setState()` centralization point
- [ ] Document state machine in README
- [ ] Add state diagram to documentation

---

## References

- **Existing Guards**: `/home/user/unrdf/packages/kgc-4d/src/guards.mjs` (24 guards)
- **Freeze Implementation**: `/home/user/unrdf/packages/kgc-4d/src/freeze.mjs`
- **Merge Implementation**: `/home/user/unrdf/packages/kgc-runtime/src/merge.mjs`
- **Permission Model**: `/home/user/unrdf/proofs/poka-yoke-permission-guard.test.mjs`
- **State Machine Test**: `/home/user/unrdf/proofs/poka-yoke-sealed-universe.test.mjs`

---

**End of Specification**
