# Multi-Agent E2E Demo - Design Document

## Overview

This demo proves the complete KGC Multi-Agent substrate works end-to-end with deterministic execution and conflict-free merge semantics. It demonstrates how three independent agents can work concurrently on disjoint knowledge graph modifications and merge their results deterministically.

## Theoretical Foundation

### Agent Execution Model

Each agent executes the following workflow:

```
μ(O) = {
  1. Input(O) → WorkItem
  2. Allocate(WorkItem) → Executor
  3. Acquire(Executor) → Workspace(Shard)
  4. Execute(Workspace) → Δ(O)
  5. Seal(Δ) → Receipt
}
```

### Merge Operator (Π)

The merge operator Π ensures deterministic composition:

```
Π(R₁, R₂, ..., Rₙ) = {
  1. Collect(R₁...Rₙ) → [Receipt]
  2. Verify(∀i,j: scope(Rᵢ) ∩ scope(Rⱼ) = ∅) → Boolean
  3. Union(Δ₁...Δₙ) → Δ_merged
  4. Hash(Δ_merged) → Receipt_global
}
```

### Conflict Resolution Law (Λ)

When conflicts occur, they are resolved deterministically by law:

```
Λ(Δᵢ, Δⱼ) → Δ_winner where:
  1. priority(Δᵢ) > priority(Δⱼ) → Δᵢ wins
  2. t_ns(Δᵢ) < t_ns(Δⱼ) → Δᵢ wins (earlier)
  3. agentId(Δᵢ) < agentId(Δⱼ) → Δᵢ wins (lexicographic)
```

## Demo Architecture

### System Components

```
┌─────────────────────────────────────────────────────────────┐
│                     Demo Coordinator                        │
│  - Initializes KGCStore                                     │
│  - Orchestrates agent lifecycle                             │
│  - Performs deterministic merge                             │
│  - Generates global receipt                                 │
└─────────────────────────────────────────────────────────────┘
                            │
                            ├─────────────────────────────┐
                            │                             │
                            ▼                             ▼
          ┌─────────────────────────┐     ┌─────────────────────────┐
          │      Agent-A            │     │      Agent-B            │
          │  ──────────────         │     │  ──────────────         │
          │  WorkItem: add_entity   │     │  WorkItem: add_entity   │
          │  Scope: entity/A/**     │     │  Scope: entity/B/**     │
          │  Triple: A → Alpha      │     │  Triple: B → Beta       │
          │  Shard: disjoint        │     │  Shard: disjoint        │
          └─────────────────────────┘     └─────────────────────────┘
                            │                             │
                            │       ┌─────────────────────────┐
                            │       │      Agent-C            │
                            │       │  ──────────────         │
                            │       │  WorkItem: add_entity   │
                            │       │  Scope: entity/C/**     │
                            │       │  Triple: C → Gamma      │
                            └───────│  Shard: disjoint        │
                                    └─────────────────────────┘
                                                │
                                                ▼
                            ┌─────────────────────────────────┐
                            │  Π Merge Operator               │
                            │  ─────────────────              │
                            │  1. Collect 3 receipts          │
                            │  2. Verify disjoint scopes      │
                            │  3. Union 3 deltas              │
                            │  4. Generate global receipt     │
                            └─────────────────────────────────┘
```

### Agent Configuration (Deterministic)

Each agent is pre-configured with:

1. **Agent-A**
   - ID: `Agent-A`
   - Scope: `{ files: ['domain/entityA/**'], subjects: ['http://kgc.io/entity/A'] }`
   - Priority: 1
   - WorkItem: `{ type: 'add_entity', payload: { entity: 'EntityA', properties: { name: 'Alpha', value: 100 } } }`
   - Triple: `<http://kgc.io/entity/A> <http://kgc.io/name> "Alpha"`

2. **Agent-B**
   - ID: `Agent-B`
   - Scope: `{ files: ['domain/entityB/**'], subjects: ['http://kgc.io/entity/B'] }`
   - Priority: 1
   - WorkItem: `{ type: 'add_entity', payload: { entity: 'EntityB', properties: { name: 'Beta', value: 200 } } }`
   - Triple: `<http://kgc.io/entity/B> <http://kgc.io/name> "Beta"`

3. **Agent-C**
   - ID: `Agent-C`
   - Scope: `{ files: ['domain/entityC/**'], subjects: ['http://kgc.io/entity/C'] }`
   - Priority: 1
   - WorkItem: `{ type: 'add_entity', payload: { entity: 'EntityC', properties: { name: 'Gamma', value: 300 } } }`
   - Triple: `<http://kgc.io/entity/C> <http://kgc.io/name> "Gamma"`

## Execution Phases

### Phase 1: WorkItem Creation & Allocation

**For each agent (A, B, C):**

1. **Create WorkItem** (Deterministic Input)
   - Type: `add_entity`
   - Payload: Agent-specific entity data
   - Constraints: `{ requiredCapabilities: ['entity_management'], maxRetries: 0 }`
   - Budget: `{ maxDeltaSize: 10, maxToolOps: 5, maxFilesTouched: 5 }`

2. **Register Executor**
   - Executor ID: Agent ID
   - Capabilities: `['entity_management']`

3. **Allocate WorkItem**
   - Assign WorkItem to corresponding Executor
   - Verify assignment success
   - Transition: `queued → assigned`

**Invariants:**

- ✓ All WorkItems created successfully
- ✓ All executors registered
- ✓ All assignments succeed
- ✓ No conflicts at allocation stage

### Phase 2: Workspace Acquisition

**For each agent (A, B, C):**

1. **Create Shard** (Workspace)
   - Agent ID: Agent ID
   - Scope: Agent-specific file/subject patterns
   - Priority: 1 (equal for all agents)

2. **Verify Disjoint Scopes**
   - Scope(A) ∩ Scope(B) = ∅
   - Scope(B) ∩ Scope(C) = ∅
   - Scope(A) ∩ Scope(C) = ∅

**Invariants:**

- ✓ All shards created
- ✓ All scopes are disjoint
- ✓ No workspace conflicts

### Phase 3: Execute Work & Modify Knowledge Store

**For each agent (A, B, C):**

1. **Start Execution**
   - Transition: `assigned → executing`
   - Generate start receipt: `R_start`
   - Chain: `previousReceiptHash = null` for first receipt

2. **Add Delta to Shard**
   - Type: `add`
   - Target: Agent-specific subject URI
   - Data: Predicate-object pair

3. **Persist to KGCStore**
   - Event type: `AGENT_DELTA`
   - Deltas: RDF triple addition
   - Generate event receipt: `R_event`

4. **Complete WorkItem**
   - Transition: `executing → completed`
   - Generate completion receipt: `R_complete`
   - Chain: `previousReceiptHash = R_start.receiptHash`

**Invariants:**

- ✓ All deltas within shard scope
- ✓ All triples persisted to store
- ✓ All receipts chained correctly
- ✓ All WorkItems completed successfully

### Phase 4: Merge Coordination (Π Operator)

**Coordinator actions:**

1. **Collect Receipts**
   - Gather all receipts from A, B, C
   - Total: 3 start + 3 complete = 6 receipts minimum

2. **Verify No Conflicts**
   - Extract all delta targets
   - Check: `|targets| = |unique(targets)|`
   - Condition: All targets must be disjoint

3. **Merge Deltas**
   - Input: `[deltas_A, deltas_B, deltas_C]`
   - Apply Λ conflict resolution (none expected)
   - Output: `MergeResult { merged, conflicts, receiptHash }`

4. **Generate Global Receipt**
   - Aggregate all agent data:
     - `agentsRun: 3`
     - `totalTriplesAdded: 3`
     - `agentReceipts: [R_A, R_B, R_C]`
     - `mergeReceiptHash: hash(merged_deltas)`
   - Hash: `globalReceiptId = blake3(aggregated_data)`

**Invariants:**

- ✓ `conflicts.length = 0` (disjoint scopes)
- ✓ `merged.length = 3` (one delta per agent)
- ✓ Global receipt is deterministic
- ✓ All agent receipts included

## Proofs & Guarantees

### P1: Determinism

**Claim:** Given identical inputs, the demo produces identical outputs.

**Proof:**

1. All agent configurations are static (DEMO_CONFIG)
2. All UUIDs use crypto.randomUUID() or deterministic fallback
3. All timestamps use monotonic nanosecond clock
4. Merge order is deterministic (Λ law)
5. Hash functions (blake3) are deterministic

**Result:** For fixed inputs O, μ(O) produces fixed receipt R.

### P2: Conflict Freedom

**Claim:** No conflicts occur during merge.

**Proof:**

1. Scopes are defined: `Scope_A = {entity/A/**}`, `Scope_B = {entity/B/**}`, `Scope_C = {entity/C/**}`
2. By construction: `Scope_A ∩ Scope_B = ∅`, etc.
3. Each delta's target ∈ corresponding scope
4. Therefore: `targets_A ∩ targets_B = ∅`, etc.
5. Merge algorithm groups by target
6. With disjoint targets, no conflicts can occur

**Result:** `merge.conflicts.length = 0` (verified).

### P3: Receipt Integrity

**Claim:** All receipts form a tamper-evident chain.

**Proof:**

1. Each receipt includes: `receiptHash = blake3(receipt_data)`
2. Subsequent receipts include: `previousReceiptHash`
3. Chain: `R₀ ← R₁ ← R₂ ← ... ← Rₙ`
4. Tampering with Rᵢ breaks chain at Rᵢ₊₁
5. Global receipt includes all agent receipt hashes

**Result:** Receipt chain is cryptographically verifiable.

### P4: Completeness

**Claim:** All agent operations are captured in the final state.

**Proof:**

1. Each agent creates exactly 1 WorkItem
2. Each WorkItem adds exactly 1 triple
3. Store event count increments for each append
4. Final count: `store.eventCount = 3`
5. Merge result: `merged.length = 3`
6. Global receipt: `totalTriplesAdded = 3`

**Result:** No operations are lost; all are accounted for.

## Output Format

### Transcript

The demo produces a detailed transcript of all operations:

```json
{
  "agents_run": 3,
  "total_triples_added": 3,
  "final_store_hash": "<blake3_hash>",
  "global_receipt_id": "<blake3_hash>",
  "merge_receipt_hash": "<blake3_hash>",
  "final_event_count": 3,
  "agents": [
    {
      "id": "Agent-A",
      "work_item_id": "<uuid>",
      "shard_id": "<uuid>",
      "receipt_hash": "<blake3_hash>",
      "triple": {
        "subject": "http://kgc.io/entity/A",
        "predicate": "http://kgc.io/name",
        "object": "Alpha"
      }
    },
    // ... Agent-B, Agent-C
  ],
  "merge": {
    "merged_count": 3,
    "conflicts_count": 0,
    "receipt_hash": "<blake3_hash>"
  },
  "transcript": [
    { "phase": "create_workitem", "agentId": "Agent-A", ... },
    { "phase": "allocate", "agentId": "Agent-A", ... },
    { "phase": "acquire_workspace", "agentId": "Agent-A", ... },
    { "phase": "start_execution", "agentId": "Agent-A", ... },
    { "phase": "modify_store", "agentId": "Agent-A", ... },
    { "phase": "generate_receipt", "agentId": "Agent-A", ... },
    // ... repeat for Agent-B, Agent-C
    { "phase": "verify_conflicts", "conflict": false, ... },
    { "phase": "merge_deltas", "mergedCount": 3, ... },
    { "phase": "global_receipt", "globalReceiptId": "...", ... }
  ]
}
```

### RECEIPT.json

The final proof artifact includes:

```json
{
  "demo": "kgc-multi-agent-e2e",
  "version": "1.0.0",
  "timestamp": "2025-12-27T00:XX:XX.XXXZ",
  "proof": {
    "agents_run": 3,
    "total_triples_added": 3,
    "final_store_hash": "<blake3_hash>",
    "global_receipt_id": "<blake3_hash>",
    "merge_receipt_hash": "<blake3_hash>",
    "conflicts": 0,
    "determinism": "verified",
    "integrity": "verified"
  },
  "agents": [
    { "id": "Agent-A", "receipt_hash": "..." },
    { "id": "Agent-B", "receipt_hash": "..." },
    { "id": "Agent-C", "receipt_hash": "..." }
  ],
  "verification": {
    "all_agents_completed": true,
    "all_receipts_verified": true,
    "merge_conflict_free": true,
    "global_receipt_generated": true
  }
}
```

## Success Criteria

The demo succeeds if and only if:

1. ✅ All 3 agents create WorkItems successfully
2. ✅ All 3 WorkItems are allocated to executors
3. ✅ All 3 agents acquire disjoint workspaces
4. ✅ All 3 agents execute and modify the knowledge store
5. ✅ All 3 agents generate valid receipts
6. ✅ Merge verification finds 0 conflicts
7. ✅ Merge produces exactly 3 deltas
8. ✅ Global receipt is generated successfully
9. ✅ Final store event count = 3
10. ✅ All receipts are cryptographically chained

## Failure Modes

If any of these occur, the demo MUST fail:

1. ❌ WorkItem creation fails
2. ❌ Allocation fails (no executor, busy, dependencies)
3. ❌ Delta outside shard scope
4. ❌ Conflict detected during merge (`conflicts.length > 0`)
5. ❌ Receipt chain broken
6. ❌ Final event count ≠ expected
7. ❌ Global receipt generation fails

## Running the Demo

```bash
# From the kgc-claude package directory
pnpm run demo:e2e

# Or from monorepo root
pnpm --filter @unrdf/kgc-claude run demo:e2e
```

## Expected Output

The demo will print:

1. Initialization summary
2. Phase-by-phase progress for each agent
3. Merge coordination results
4. Final state summary
5. JSON output with full transcript

Exit code:

- `0`: Success (all criteria met)
- `1`: Failure (with error details)

## Extensions

This demo can be extended to test:

1. **Conflict Scenarios**: Overlapping scopes to verify Λ resolution
2. **Priority Ordering**: Different agent priorities
3. **Scale Testing**: 10, 100, 1000 agents
4. **Error Handling**: Simulated failures and retries
5. **Async Timing**: Variable delays to test race conditions
6. **Complex Deltas**: Multi-triple operations
7. **Dependency Chains**: Agent B depends on Agent A

## References

- KGC-4D Event Sourcing: `/packages/kgc-4d/`
- Shard Merge Implementation: `/packages/kgc-claude/src/shard-merge.mjs`
- Async Workflow: `/packages/kgc-claude/src/async-workflow.mjs`
- HDIT Theory: `/packages/kgc-4d/src/hdit/`
