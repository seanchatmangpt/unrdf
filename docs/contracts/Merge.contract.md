# Merge Contract

## Purpose

Defines the reconciliation operation Π (product operator) that merges N agent tranches into a unified global state.

## Input: Tranche Set

```typescript
interface TrancheSet {
  tranches: Receipt[]; // N receipts from agents 1-9
  merge_strategy: 'disjoint' | 'crdt' | 'manual';
  conflict_policy: 'fail_fast' | 'collect_all' | 'last_write_wins';
}
```

## Output: MergeResult

```typescript
interface MergeResult {
  // Status
  status: 'success' | 'conflicts' | 'failure';

  // Global state
  merged_hash: string; // BLAKE3 of final merged state
  global_receipt_id: string; // UUID of global receipt

  // Tranche summary
  tranches_merged: number; // Count of successfully merged tranches
  total_deltas: number; // Sum of all deltas
  total_artifacts: number; // Sum of all artifacts

  // Conflicts (if any)
  conflicts: Conflict[];

  // Verification
  proof_artifacts: string[]; // Paths to merge proof scripts
  verification_script: string; // Path to verify-merge.sh

  // Metrics
  merge_time_ms: number;
  parallelism_achieved: number; // % of work done in parallel
}

interface Conflict {
  type: 'file_ownership' | 'state_divergence' | 'hash_mismatch';
  agents: string[]; // Conflicting agent IDs
  resource: string; // File path or state URI
  details: string;
}
```

## Commutativity Law

The merge operator Π is **commutative** if tranches are disjoint:

```
Π(Δ₁, Δ₂, ..., Δₙ) = Π(σ(Δ₁, Δ₂, ..., Δₙ))  ∀ permutation σ

⟺ files(Δᵢ) ∩ files(Δⱼ) = ∅  ∀ i ≠ j
```

**Proof**:

- If no two agents modify the same file, order doesn't matter
- Each Δᵢ applies to disjoint subsets of state
- Union operation is commutative: S₁ ∪ S₂ = S₂ ∪ S₁

## Merge Algorithm

```typescript
async function merge(tranches: Receipt[]): MergeResult {
  // Phase 1: Validate all tranches
  const validationErrors = [];
  for (const tranche of tranches) {
    const valid = verify(tranche);
    if (!valid) {
      validationErrors.push({
        agent: tranche.agent_id,
        error: 'Receipt verification failed',
      });
    }
  }
  if (validationErrors.length > 0) {
    return { status: 'failure', conflicts: validationErrors };
  }

  // Phase 2: Detect conflicts
  const conflicts = detectConflicts(tranches);
  if (conflicts.length > 0) {
    return { status: 'conflicts', conflicts };
  }

  // Phase 3: Merge deltas
  const mergedDeltas = {
    deltaO: tranches.flatMap(t => t.deltas.deltaO),
    deltaPi: tranches.flatMap(t => t.deltas.deltaPi),
    deltaLambda: tranches.flatMap(t => t.deltas.deltaLambda),
    deltaQ: tranches.flatMap(t => t.deltas.deltaQ),
  };

  // Phase 4: Merge artifacts
  const mergedArtifacts = tranches.flatMap(t => t.artifacts);

  // Phase 5: Compute merged hash
  const mergedState = await applyDeltas(store, mergedDeltas);
  const mergedHash = await computeHash(mergedState);

  // Phase 6: Generate global receipt
  const globalReceipt = await createGlobalReceipt({
    tranches,
    mergedHash,
    mergedDeltas,
    mergedArtifacts,
  });

  return {
    status: 'success',
    merged_hash: mergedHash,
    global_receipt_id: globalReceipt.id,
    tranches_merged: tranches.length,
    conflicts: [],
  };
}
```

## Conflict Detection

```typescript
function detectConflicts(tranches: Receipt[]): Conflict[] {
  const conflicts: Conflict[] = [];

  // Check 1: File ownership conflicts
  const fileOwnership = new Map<string, string>();
  for (const tranche of tranches) {
    for (const artifact of tranche.artifacts) {
      if (!artifact.path) continue;

      if (fileOwnership.has(artifact.path)) {
        conflicts.push({
          type: 'file_ownership',
          agents: [fileOwnership.get(artifact.path)!, tranche.agent_id],
          resource: artifact.path,
          details: `Both agents modified ${artifact.path}`,
        });
      } else {
        fileOwnership.set(artifact.path, tranche.agent_id);
      }
    }
  }

  // Check 2: State divergence (same delta target, different values)
  const deltaTargets = new Map<string, Delta[]>();
  for (const tranche of tranches) {
    for (const delta of [...tranche.deltas.deltaO, ...tranche.deltas.deltaPi]) {
      if (!deltaTargets.has(delta.target)) {
        deltaTargets.set(delta.target, []);
      }
      deltaTargets.get(delta.target)!.push(delta);
    }
  }

  for (const [target, deltas] of deltaTargets) {
    if (deltas.length > 1) {
      const uniqueAfters = new Set(deltas.map(d => JSON.stringify(d.after)));
      if (uniqueAfters.size > 1) {
        conflicts.push({
          type: 'state_divergence',
          agents: deltas.map(d => 'unknown'), // Would track agent in delta
          resource: target,
          details: `Conflicting state changes to ${target}`,
        });
      }
    }
  }

  // Check 3: Hash chain continuity
  for (let i = 0; i < tranches.length - 1; i++) {
    // Only applies if tranches are sequential (rare)
    if (tranches[i].after_hash !== tranches[i + 1].before_hash) {
      // In parallel execution, this is expected
      // Only flag if marked as sequential dependency
    }
  }

  return conflicts;
}
```

## Disjoint Union Proof

For valid merge (no conflicts):

```
merged_state = ⋃ᵢ state(Δᵢ)

where:
  state(Δᵢ) = { resource → value | (resource, value) ∈ Δᵢ }

Disjoint property:
  ∀ i ≠ j: resources(Δᵢ) ∩ resources(Δⱼ) = ∅

Therefore:
  merged_state is well-defined
  |merged_state| = Σᵢ |state(Δᵢ)|
```

## Merge Strategies

### 1. Disjoint (Default)

```typescript
// Requires: No file/resource conflicts
// Guarantees: Commutative, deterministic
// Usage: Agent 0 verifies disjointness, fails if violated
```

### 2. CRDT (Future)

```typescript
// Requires: Resources are CRDTs (e.g., LWW-register, G-Counter)
// Guarantees: Eventual consistency
// Usage: State has built-in merge semantics
```

### 3. Manual

```typescript
// Requires: Human intervention
// Guarantees: None (depends on resolution)
// Usage: Agent 0 reports conflicts, blocks until resolved
```

## Verification Script

Every merge produces a verification script:

```bash
#!/usr/bin/env bash
# verify-merge.sh

set -euo pipefail

echo "=== Verifying Global Merge ==="

# 1. Check all tranche receipts exist
for agent in {1..9}; do
  RECEIPT="tranches/agent-$agent/RECEIPT.json"
  if [ ! -f "$RECEIPT" ]; then
    echo "❌ Missing receipt: $RECEIPT"
    exit 1
  fi
  echo "✅ Found: $RECEIPT"
done

# 2. Validate each receipt
node scripts/validate-receipt.mjs tranches/agent-1/RECEIPT.json
node scripts/validate-receipt.mjs tranches/agent-2/RECEIPT.json
# ... for all agents

# 3. Check for conflicts
CONFLICTS=$(node scripts/detect-conflicts.mjs tranches/**/RECEIPT.json)
if [ -n "$CONFLICTS" ]; then
  echo "❌ Conflicts detected:"
  echo "$CONFLICTS"
  exit 1
fi

# 4. Verify merged hash
COMPUTED_HASH=$(node scripts/compute-merged-hash.mjs)
EXPECTED_HASH=$(jq -r '.merged_hash' GLOBAL_RECEIPT.json)

if [ "$COMPUTED_HASH" != "$EXPECTED_HASH" ]; then
  echo "❌ Hash mismatch:"
  echo "  Expected: $EXPECTED_HASH"
  echo "  Computed: $COMPUTED_HASH"
  exit 1
fi

echo "✅ Merge verified successfully"
```

## Testing Contract

```javascript
// Test 1: Commutativity
const order1 = merge([t1, t2, t3]);
const order2 = merge([t3, t1, t2]);
assert(order1.merged_hash === order2.merged_hash);

// Test 2: Conflict detection
const conflicting = [
  { agent_id: 'agent-1', artifacts: [{ path: 'foo.mjs' }] },
  { agent_id: 'agent-2', artifacts: [{ path: 'foo.mjs' }] },
];
const result = merge(conflicting);
assert(result.status === 'conflicts');
assert(result.conflicts.length === 1);
assert(result.conflicts[0].type === 'file_ownership');

// Test 3: Associativity (for disjoint)
const merge12 = merge([t1, t2]);
const merge12_3 = merge([merge12.receipt, t3]);
const merge23 = merge([t2, t3]);
const merge1_23 = merge([t1, merge23.receipt]);
assert(merge12_3.merged_hash === merge1_23.merged_hash);
```

## Global Receipt Schema

```typescript
interface GlobalReceipt extends Receipt {
  phase: 'reconcile'; // Always 'reconcile' for Agent 0
  agent_id: 'agent-0'; // Always Agent 0

  // Merge-specific
  tranches_merged: number;
  tranche_receipts: string[]; // IDs of merged receipts
  merge_law: string; // "disjoint union"
  commutativity_verified: boolean;

  // Proof
  verification_script: string; // Path to verify-merge.sh
  replay_instructions: string; // How to reproduce merge
}
```

## Usage Example

```javascript
// Collect all tranche receipts
const trancheReceipts = await Promise.all([
  loadReceipt('tranches/agent-1/RECEIPT.json'),
  loadReceipt('tranches/agent-2/RECEIPT.json'),
  // ... agents 3-9
]);

// Merge
const mergeResult = await merge(trancheReceipts);

if (mergeResult.status === 'success') {
  console.log(`✅ Merged ${mergeResult.tranches_merged} tranches`);
  console.log(`Global hash: ${mergeResult.merged_hash}`);
  console.log(`Global receipt: ${mergeResult.global_receipt_id}`);

  // Write global receipt
  fs.writeFileSync('GLOBAL_RECEIPT.json', JSON.stringify(globalReceipt, null, 2));
} else {
  console.error(`❌ Merge failed: ${mergeResult.status}`);
  for (const conflict of mergeResult.conflicts) {
    console.error(`  - ${conflict.type}: ${conflict.details}`);
  }
  process.exit(1);
}
```

## Parallelism Metric

```typescript
function calculateParallelism(tranches: Receipt[]): number {
  // Ideal: all agents run in parallel
  const maxTime = Math.max(...tranches.map(t => t.metrics.execution_time_ms));
  const totalTime = tranches.reduce((sum, t) => sum + t.metrics.execution_time_ms, 0);

  // Parallelism = (total sequential time) / (actual wall clock time)
  // If all parallel: parallelism = N (number of agents)
  // If all sequential: parallelism = 1
  return totalTime / maxTime;
}
```

Example: 9 agents, each taking 60s

- Sequential: 540s total
- Parallel: 60s wall clock
- Parallelism achieved: 540 / 60 = 9x (100%)
