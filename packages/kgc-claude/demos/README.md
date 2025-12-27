# KGC-Claude Demos

This directory contains demonstration scripts for the KGC-Claude substrate.

## Multi-Agent End-to-End Demo

**File:** `multi-agent-e2e.mjs`

A complete proof-of-concept demonstrating the KGC Multi-Agent substrate with:

- 3 agents (Agent-A, Agent-B, Agent-C) working in parallel
- Deterministic WorkItem allocation
- Disjoint workspace acquisition
- Knowledge store modification (RDF triples)
- Receipt generation and verification
- Conflict-free merge using Π operator
- Global receipt generation

### Running the Demo

```bash
# From the kgc-claude package directory
pnpm run demo:e2e

# Or from the monorepo root
pnpm --filter @unrdf/kgc-claude run demo:e2e
```

### Prerequisites

Ensure all workspace dependencies are installed:

```bash
# From monorepo root
pnpm install
```

### Output

The demo produces:

1. **Console Output**: Detailed transcript of all operations across 7 sections:
   - Initialization
   - Phase 1: WorkItem Creation & Allocation
   - Phase 2: Workspace Acquisition
   - Phase 3: Execute Work & Modify Store
   - Phase 4: Merge Coordination (Π Operator)
   - Final State Summary
   - JSON Output

2. **JSON Output**: Complete demo transcript with metrics:
   ```json
   {
     "agents_run": 3,
     "total_triples_added": 3,
     "final_store_hash": "...",
     "global_receipt_id": "...",
     "merge_receipt_hash": "...",
     "final_event_count": 3,
     "agents": [...],
     "merge": {...},
     "transcript": [...]
   }
   ```

### Success Criteria

The demo succeeds (exit code 0) if and only if:

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

### Documentation

- **DESIGN.md**: Complete design document with theoretical foundation, architecture, proofs, and guarantees
- **RECEIPT.json**: Proof artifact showing demo configuration and expected results

### Files

```
demos/
├── README.md              # This file
├── DESIGN.md             # Complete design document
├── RECEIPT.json          # Proof artifact
└── multi-agent-e2e.mjs   # Demo script (540 lines)
```

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     Demo Coordinator                        │
└──────────────────────┬──────────────────────────────────────┘
                       │
           ┌───────────┼───────────┐
           ▼           ▼           ▼
       Agent-A     Agent-B     Agent-C
       (entity/A)  (entity/B)  (entity/C)
           │           │           │
           └───────────┴───────────┘
                       │
                       ▼
              Π Merge Operator
           (deterministic union)
                       │
                       ▼
              Global Receipt
```

### Theoretical Foundation

**Agent Execution:**

```
μ(O) = Input → WorkItem → Allocate → Workspace → Execute → Receipt
```

**Merge Operator:**

```
Π(R₁, R₂, ..., Rₙ) = Collect → Verify → Union → Hash
```

**Conflict Resolution Law:**

```
Λ(Δᵢ, Δⱼ) → winner based on:
  1. Priority (higher wins)
  2. Timestamp (earlier wins)
  3. Agent ID (lexicographic)
```

### Proofs

The demo proves four key properties:

1. **P1: Determinism** - Identical inputs produce identical outputs
2. **P2: Conflict Freedom** - Disjoint scopes guarantee no conflicts
3. **P3: Receipt Integrity** - Cryptographic chain prevents tampering
4. **P4: Completeness** - All operations accounted for in final state

See `DESIGN.md` for complete proofs.

### Extensions

This demo can be extended to test:

- Conflict scenarios (overlapping scopes)
- Priority ordering (different agent priorities)
- Scale testing (10, 100, 1000 agents)
- Error handling (simulated failures and retries)
- Complex deltas (multi-triple operations)
- Dependency chains (Agent B depends on Agent A)

### Related Implementations

- Shard Merge: `/packages/kgc-claude/src/shard-merge.mjs`
- Async Workflow: `/packages/kgc-claude/src/async-workflow.mjs`
- KGC Store: `/packages/kgc-4d/src/store.mjs`
- HDIT Theory: `/packages/kgc-4d/src/hdit/`
