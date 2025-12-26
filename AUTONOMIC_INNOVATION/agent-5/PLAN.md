# Agent 5: Commutativity Analysis & Conflict Detection

## Mission
Determine if two capsules can safely reorder and provide minimal conflict certificates when they cannot.

## Commutativity Algorithm

### Core Principle
Two operations commute if:
```
apply(capsuleA, apply(capsuleB, state)) ≡ apply(capsuleB, apply(capsuleA, state))
```

### Detection Strategy

1. **Extract Write Sets**
   - For each capsule, identify all (subject, predicate) pairs being modified
   - Operations: 'add' quad or 'delete' quad
   - Write set = Set of {subject, predicate, operation, object}

2. **Extract Read Sets**
   - For each capsule, identify subjects referenced in intent/guards/profiles
   - Read set = Set of subjects that must be stable for intent validity

3. **Overlap Analysis**
   - **No overlap** → Capsules commute (disjoint operations)
   - **Write-Write overlap** → Check if operations are identical
     - Same (s, p, o, op) → Commutative (idempotent)
     - Different objects or operations → Conflict
   - **Read-Write overlap** → Capsule reading must execute before writer

4. **Conflict Classification**
   - **write-write**: Both modify same (s, p) with different values
   - **read-write**: One reads what other writes (dependency)
   - **contradiction**: Logical impossibility (add+delete same quad)

## Witness Format

Minimal evidence of conflict:

```javascript
{
  quadA: { subject: "...", predicate: "...", object: "...", operation: "add" },
  quadB: { subject: "...", predicate: "...", object: "...", operation: "del" },
  conflictType: "write-write" | "read-write" | "contradiction",
  canResolveBy: "order" | "merge" | "manual"
}
```

### Resolution Strategies
- **order**: Enforce specific sequence (dependency edge)
- **merge**: Combine operations (e.g., union of adds)
- **manual**: Human intervention required (semantic conflict)

## Conflict Certificate

Deterministic proof for auditing:

```
CONFLICT_CERT_V1
capsuleA_id: <hash>
capsuleB_id: <hash>
witness: <canonical JSON>
signature: SHA-256(<capsuleA_id>|<capsuleB_id>|<witness>)
timestamp: <ISO-8601>
```

### Properties
- **Deterministic**: Same conflict → same certificate (100x test)
- **Minimal**: Only conflicting quad pairs, not entire capsules
- **Verifiable**: Hash chain allows independent validation
- **Canonical**: JSON keys sorted, no whitespace variance

## Dependency Graph

### Nodes
Each capsule is a node with:
- ID
- Write set
- Read set

### Edges
Directed edge A → B if:
1. writeSet(A) ∩ readSet(B) ≠ ∅ (B depends on A's writes)
2. writeSet(A) ∩ writeSet(B) ≠ ∅ && !commutative (order matters)

### Applications
- **Parallel execution**: Nodes with no dependencies run concurrently
- **Topological sort**: Safe execution order
- **Conflict detection**: Cycles indicate unresolvable conflicts
- **Performance**: Graph analysis <200ms for 100 capsules

## Implementation Notes

### Pure Functions
All functions are pure - no side effects, no OTEL instrumentation.

### Performance Targets
- canReorder: <1ms for typical capsules
- dependencyGraph: <200ms for 100 capsules
- conflictCertificate: <5ms (hashing overhead)

### Edge Cases
1. Empty capsules → Always commute
2. Self-referential operations → Check quad identity
3. Wildcard predicates → Conservative conflict detection
4. Large deltas → Efficient set operations (Map/Set)

## Test Coverage

1. Commutative capsules (disjoint subjects)
2. Write-write conflict (same s,p different o)
3. Certificate determinism (100x identical hash)
4. Dependency graph (3-capsule chain)
5. Performance (100 capsules <200ms)
