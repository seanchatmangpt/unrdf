# v6 Control Plane: ΔGate Architecture

**Status**: Design
**Last Updated**: 2025-12-27
**Related**: [PROGRAM_CHARTER.md](PROGRAM_CHARTER.md)

---

## Overview

**ΔGate** is the primary control plane for v6, implementing admissibility checking and receipt enforcement for all state changes. It serves as the single point of entry for mutations, ensuring determinism, provenance, and atomicity.

**Key Insight**: Hooks, receipts, AOT compilation, resource allocation, and workflows are **projections** of ΔGate - different views of the same underlying control flow.

---

## Architecture

### Component Diagram (C4 Level 2)

```
┌───────────────────────────────────────────────────────────────────┐
│                         ΔGate Control Plane                        │
│                                                                     │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐            │
│  │   Proposer   │  │ Reconciler   │  │   Applicator │            │
│  │              │  │              │  │              │            │
│  │ • Parse Δ    │→ │ • Validate   │→ │ • Receipt    │            │
│  │ • Schema     │  │ • μ(O ⊔ Δ)   │  │ • Apply      │            │
│  │ • Justif.    │  │ • Hooks      │  │ • Verify     │            │
│  └──────────────┘  └──────────────┘  └──────────────┘            │
│         ↓                 ↓                  ↓                     │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐            │
│  │ Delta Queue  │  │ Policy Engine│  │ Receipt Chain│            │
│  │ (FIFO/Prio)  │  │ (SPARQL ASK) │  │ (BLAKE3)     │            │
│  └──────────────┘  └──────────────┘  └──────────────┘            │
│                                                                     │
└───────────────────────────────────────────────────────────────────┘
                             ↓
     ┌─────────────────────────────────────────────────────────┐
     │                  Integration Layer                       │
     │  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌────────┐  │
     │  │KnowStore │  │  YAWL    │  │  Hooks   │  │  CLI   │  │
     │  │(Substrate)│  │(Workflow)│  │ (Policy) │  │(UX)    │  │
     │  └──────────┘  └──────────┘  └──────────┘  └────────┘  │
     └─────────────────────────────────────────────────────────┘
```

---

## Core Workflow: Δ Lifecycle

### 1. Propose Δ (Submission)

**Input**: Delta object (validated against `DeltaSchema`)

```javascript
const delta = {
  id: "delta-abc123",
  type: "update",
  target: {
    entity: "http://example.org/user/42",
    scope: "http://kgc.io/graph/universe"
  },
  changes: [
    {
      operation: "replace_value",
      subject: { type: "NamedNode", value: "http://example.org/user/42" },
      predicate: { type: "NamedNode", value: "http://schema.org/email" },
      object: { type: "Literal", value: "new@example.com" }
    }
  ],
  justification: {
    reasoning: "User requested email update",
    actor: "user:42",
    policyChecked: "email-update-policy"
  },
  preconditions: [
    {
      type: "sparql_ask",
      query: "ASK { <http://example.org/user/42> a <http://schema.org/Person> }"
    }
  ]
};
```

**Processing**:

1. **Parse**: Validate against `DeltaSchema` (Zod)
2. **Enqueue**: Add to Delta Queue (FIFO or priority-based)
3. **Return**: Delta ID for tracking

**Output**: `{ deltaId: "delta-abc123", status: "queued", position: 5 }`

---

### 2. Reconcile μ(O ⊔ Δ) (Validation & Merge)

**Input**: Delta from queue, current store state O

**Processing**:

1. **Check Preconditions**: Evaluate all precondition queries
   ```javascript
   for (const precond of delta.preconditions) {
     if (precond.type === 'sparql_ask') {
       const result = await store.query(precond.query);
       if (!result) throw new AdmissibilityError(`Precondition failed: ${precond.query}`);
     }
   }
   ```

2. **Execute Hooks** (Pre-Δ):
   ```javascript
   const hookResults = await hookRegistry.execute('pre-delta', {
     delta,
     store: storeSnapshot
   });
   if (hookResults.some(r => !r.approved)) {
     throw new PolicyViolation('Hook rejected delta', hookResults);
   }
   ```

3. **Compute Merge**: Calculate atomic change set A
   ```javascript
   const changeSet = {
     additions: [],
     deletions: [],
     replacements: []
   };

   for (const change of delta.changes) {
     if (change.operation === 'add_triple') {
       changeSet.additions.push(quad(change.subject, change.predicate, change.object));
     } else if (change.operation === 'remove_triple') {
       changeSet.deletions.push(quad(change.subject, change.predicate, change.object));
     } else if (change.operation === 'replace_value') {
       // Atomic read-modify-write
       const existing = store.match(change.subject, change.predicate, null);
       changeSet.deletions.push(...existing);
       changeSet.additions.push(quad(change.subject, change.predicate, change.object));
     }
   }
   ```

4. **Validate Admissibility**: Check invariants
   ```javascript
   // Example: Check that delta doesn't violate cardinality constraints
   for (const addition of changeSet.additions) {
     const predicate = addition.predicate.value;
     const schema = schemaRegistry.get(predicate);
     if (schema?.maxCardinality === 1) {
       const existing = changeSet.additions.filter(q =>
         q.subject.equals(addition.subject) &&
         q.predicate.equals(addition.predicate)
       );
       if (existing.length > 1) {
         throw new InvariantViolation(`Predicate ${predicate} has maxCardinality=1`);
       }
     }
   }
   ```

**Output**: Validated atomic change set A

---

### 3. Generate Receipt (Cryptographic Proof)

**Input**: Delta Δ, atomic change set A, previous receipt

**Processing**:

1. **Construct Payload**:
   ```javascript
   const payload = {
     deltaId: delta.id,
     deltaType: delta.type,
     changeSetHash: await computeBlake3(changeSet),
     affectedEntities: [...new Set(changeSet.additions.map(q => q.subject.value))],
     timestamp_ns: now()
   };
   ```

2. **Compute Hashes**:
   ```javascript
   const payloadHash = await computeBlake3(payload);
   const previousHash = previousReceipt?.receiptHash || null;
   const receiptHash = await computeChainHash(previousHash, payloadHash);
   ```

3. **Build Receipt**:
   ```javascript
   const receipt = {
     id: generateUUID(),
     profile: 'delta',
     previousReceiptHash: previousHash,
     payloadHash,
     receiptHash,
     t_ns: now(),
     timestamp_iso: toISO(now()),
     context: {
       deltaId: delta.id,
       nodeId: store.getNodeId()
     },
     payload
   };
   ```

4. **Validate**: Check against `ReceiptProfileSchema`

5. **Store**: Append to ReceiptChain in KnowledgeStore

**Output**: Signed receipt with hash chain

---

### 4. Apply Atomic A (Transaction Execution)

**Input**: Atomic change set A, receipt

**Processing**:

1. **Begin Transaction**: Create snapshot for rollback
   ```javascript
   const snapshot = await store.generateSnapshot();
   ```

2. **Apply Changes**: Execute all additions/deletions
   ```javascript
   try {
     for (const quad of changeSet.deletions) {
       await store.appendTriple('delete', quad.subject, quad.predicate, quad.object);
     }
     for (const quad of changeSet.additions) {
       await store.appendTriple('add', quad.subject, quad.predicate, quad.object);
     }
   } catch (error) {
     // Rollback on any failure
     await store.restoreSnapshot(snapshot.snapshot_id);
     throw new TransactionFailed('Apply failed, rolled back', error);
   }
   ```

3. **Execute Hooks** (Post-Δ):
   ```javascript
   const postHookResults = await hookRegistry.execute('post-delta', {
     delta,
     receipt,
     changeSet,
     store
   });
   ```

4. **Verify Effects**: Check postconditions
   ```javascript
   if (delta.expectedEffects) {
     const actualQuadCount = await store.getQuadCount();
     const expectedCount = snapshot.quad_count + (delta.expectedEffects.quadCountDelta || 0);
     if (actualQuadCount !== expectedCount) {
       throw new PostconditionFailed(`Expected ${expectedCount} quads, got ${actualQuadCount}`);
     }
   }
   ```

5. **Commit**: Finalize transaction
   ```javascript
   const commitment = await store.getStateCommitment();
   ```

**Output**: Applied delta with verified state commitment

---

### 5. Emit Event (Audit Trail)

**Input**: Receipt, commitment

**Processing**:

1. **Log to KGC-4D**:
   ```javascript
   await store.store.appendEvent({
     type: 'DELTA_APPLIED',
     payload: {
       deltaId: delta.id,
       receiptId: receipt.id,
       receiptHash: receipt.receiptHash,
       stateHash: commitment.state_hash,
       actor: delta.justification?.actor
     }
   });
   ```

2. **Publish to Subscribers** (if queue integration enabled):
   ```javascript
   await eventBus.publish('delta.applied', {
     deltaId: delta.id,
     receiptHash: receipt.receiptHash,
     timestamp: receipt.timestamp_iso
   });
   ```

**Output**: Event ID for correlation

---

## Integration with Existing Systems

### KnowledgeStore (Substrate)

**Role**: Persistence layer for ΔGate

**Integration Points**:

- **appendTriple**: Execute atomic changes
- **generateSnapshot**: Create rollback points
- **getStateCommitment**: Verify postconditions
- **match/query**: Evaluate preconditions

**Data Flow**:

```
ΔGate → appendTriple(operation, s, p, o) → KnowledgeStore
                                          ↓
                                   ReceiptChain.append(receipt)
                                          ↓
                                   GitBackbone.commit(snapshot)
```

---

### YAWL (Workflow)

**Role**: Orchestrate multi-step Δ sequences

**Integration Points**:

- **Task Execution**: Each task proposes Δ
- **Control Flow**: Routing based on receipt outcomes
- **Work Items**: Carry Δ proposals as payload
- **Receipts**: YAWL receipts chain to Δ receipts

**Example Workflow**:

```yaml
workflow: user-onboarding
tasks:
  - id: create-user
    type: automated
    delta:
      type: create
      target: { entity: "http://example.org/user/{{userId}}" }
      changes:
        - operation: add_triple
          subject: { value: "http://example.org/user/{{userId}}" }
          predicate: { value: "rdf:type" }
          object: { value: "schema:Person" }
    on_complete: assign-role

  - id: assign-role
    type: automated
    delta:
      type: update
      target: { entity: "http://example.org/user/{{userId}}" }
      changes:
        - operation: add_triple
          predicate: { value: "schema:role" }
          object: { value: "user" }
    preconditions:
      - type: receipt_exists
        receiptId: "{{tasks.create-user.receiptId}}"
```

**Receipt Chaining**:

```
R_create_user → R_assign_role → R_workflow_complete
     ↓               ↓                    ↓
   Δ_1            Δ_2                 (no Δ)
```

---

### Hooks (Policy Engine)

**Role**: Validate Δ against policies before/after reconciliation

**Hook Points**:

| Hook | Timing | Purpose | Can Block? |
|------|--------|---------|------------|
| `pre-delta` | Before reconcile | Check admissibility | Yes |
| `post-delta` | After apply | Verify effects | Yes (triggers rollback) |
| `on-failure` | On error | Cleanup, notification | No |

**Hook Contract**:

```javascript
const hookSchema = z.object({
  id: z.string(),
  priority: z.number(), // Lower = earlier execution
  phase: z.enum(['pre-delta', 'post-delta', 'on-failure']),
  handler: z.function().args(
    z.object({
      delta: DeltaSchema,
      store: z.any(), // KnowledgeStore snapshot
      receipt: ReceiptProfileSchema.optional()
    })
  ).returns(
    z.promise(z.object({
      approved: z.boolean(),
      reason: z.string().optional(),
      metadata: z.record(z.any()).optional()
    }))
  )
});
```

**Example Hook** (email validation):

```javascript
{
  id: 'validate-email-format',
  priority: 100,
  phase: 'pre-delta',
  async handler({ delta, store }) {
    const emailChanges = delta.changes.filter(
      c => c.predicate?.value === 'http://schema.org/email'
    );

    for (const change of emailChanges) {
      const email = change.object?.value;
      if (!email.match(/^[^\s@]+@[^\s@]+\.[^\s@]+$/)) {
        return {
          approved: false,
          reason: `Invalid email format: ${email}`
        };
      }
    }

    return { approved: true };
  }
}
```

---

### CLI (User Interface)

**Role**: Propose Δ from command line

**Commands**:

```bash
# Propose delta from file
kgc delta create --file user-update.json --dry-run
# → Validates schema, checks preconditions, shows what would change

# Apply delta
kgc delta apply --id delta-abc123
# → Executes ΔGate workflow, returns receipt

# Verify delta was applied
kgc delta inspect --id delta-abc123 --show-receipt
# → Shows delta, receipt, state commitment

# List pending deltas
kgc delta list --status queued --json
# → Shows all queued deltas

# Retry failed delta
kgc delta retry --id delta-xyz789 --reason "Precondition now satisfied"
```

**JSON Output** (deterministic envelope):

```json
{
  "ok": true,
  "data": {
    "deltaId": "delta-abc123",
    "receiptId": "receipt-def456",
    "receiptHash": "a1b2c3d4...",
    "stateCommitment": "e5f6g7h8...",
    "appliedAt": "2025-12-27T10:30:00.000Z"
  },
  "meta": {
    "timestamp": "2025-12-27T10:30:00.123Z",
    "nodeId": "ks-abc123",
    "executionTimeMs": 45
  }
}
```

---

## Projections: Different Views of ΔGate

### 1. Receipts Projection

**View**: Every Δ → exactly one receipt

**Query**:

```sparql
SELECT ?delta ?receipt ?hash WHERE {
  ?delta rdf:type :Delta .
  ?delta :hasReceipt ?receipt .
  ?receipt :receiptHash ?hash .
}
```

**Use Cases**:

- Audit trail
- Compliance reporting
- Tamper detection

---

### 2. AOT Compilation Projection

**View**: Δ preconditions → compiled query plans

**Optimization**: For frequently-executed Δ types, precompile SPARQL queries

```javascript
// Instead of:
const result = await store.query(delta.preconditions[0].query);

// Use compiled query plan:
const plan = compiledPlans.get(delta.type);
const result = await plan.execute(store, delta.target);
```

**Benefits**: 10-100x faster precondition checking

---

### 3. Resource Allocation Projection

**View**: Δ → resource requirements

**Schema**:

```javascript
const ResourceRequirementsSchema = z.object({
  deltaType: z.string(),
  estimatedQuads: z.number(),
  estimatedTimeMs: z.number(),
  memoryMB: z.number(),
  requiresLock: z.boolean()
});
```

**Use Case**: Scheduler can prioritize/throttle Δ based on resources

---

### 4. Workflow Projection

**View**: Sequence of Δ → YAWL case

**Mapping**:

```
YAWL Task → Δ Proposal
YAWL Control Flow → Δ Precondition (receipt_exists)
YAWL Work Item → Δ Payload
YAWL Case Receipt → Aggregate Δ Receipts
```

---

## Error Handling

### Error Taxonomy

| Error Type | Phase | Recovery |
|------------|-------|----------|
| `SchemaValidationError` | Propose | Reject, return error to client |
| `PreconditionFailed` | Reconcile | Reject, suggest retry condition |
| `PolicyViolation` | Reconcile (hooks) | Reject, log violation |
| `InvariantViolation` | Reconcile | Reject, alert admin |
| `TransactionFailed` | Apply | Rollback, retry once |
| `PostconditionFailed` | Apply | Rollback, alert admin |

### Error Receipt

Failed Δ still get receipts (for audit trail):

```javascript
{
  id: "receipt-error-123",
  profile: "delta",
  payload: {
    deltaId: "delta-abc123",
    status: "failed",
    errorType: "PreconditionFailed",
    errorMessage: "User does not exist",
    failedAt: "reconcile"
  },
  receiptHash: "..." // Still chained!
}
```

**Why**: Audit trail must include failures (compliance, debugging)

---

## Performance Characteristics

### Latency Budget

| Phase | Target | p99 |
|-------|--------|-----|
| Propose | 5ms | 20ms |
| Reconcile | 50ms | 200ms |
| Receipt | 10ms | 30ms |
| Apply | 100ms | 500ms |
| Total | 165ms | 750ms |

### Throughput

- **Sequential**: 10-20 Δ/sec (limited by BLAKE3 + disk I/O)
- **Batched**: 100-500 Δ/sec (amortize hash computation)
- **Parallel**: 1000+ Δ/sec (independent Δ on different entities)

### Optimization Strategies

1. **Batch Receipts**: Merkle tree for N Δ → 1 root receipt
2. **Async Hooks**: Non-blocking hook execution with timeout
3. **Query Caching**: Cache SPARQL ASK results for preconditions
4. **Compiled Plans**: AOT compile frequent Δ patterns

---

## Security Considerations

### Threat Model

| Threat | Mitigation |
|--------|------------|
| Malicious Δ (invalid data) | Schema validation, hooks |
| Replay attack (resubmit Δ) | Receipt chain prevents duplicates |
| Tampering (modify receipt) | BLAKE3 hash, tamper detection |
| Unauthorized Δ (privilege escalation) | Actor validation in hooks |
| DoS (flood Δ queue) | Rate limiting, priority queue |

### Access Control

```javascript
// Hook validates actor permissions
{
  id: 'check-write-permission',
  phase: 'pre-delta',
  async handler({ delta, store }) {
    const actor = delta.justification?.actor;
    const target = delta.target.entity;

    const canWrite = await store.query(`
      ASK {
        <${actor}> :hasPermission [
          :on <${target}> ;
          :action :write
        ]
      }
    `);

    return {
      approved: canWrite,
      reason: canWrite ? null : `${actor} cannot write to ${target}`
    };
  }
}
```

---

## Monitoring and Observability

### Metrics

```javascript
// Prometheus-style metrics
deltagate_proposals_total{status="queued|rejected"}
deltagate_reconcile_duration_seconds{phase="preconditions|hooks|merge"}
deltagate_apply_duration_seconds{status="success|failed|rolled_back"}
deltagate_receipts_total{profile="delta"}
deltagate_queue_depth_current
```

### Traces (OpenTelemetry)

```
Span: delta.apply
├─ Span: delta.propose (5ms)
├─ Span: delta.reconcile (50ms)
│  ├─ Span: preconditions.check (10ms)
│  ├─ Span: hooks.execute (30ms)
│  └─ Span: merge.compute (10ms)
├─ Span: receipt.generate (10ms)
├─ Span: transaction.apply (100ms)
│  ├─ Span: store.delete (40ms)
│  ├─ Span: store.add (40ms)
│  └─ Span: verify.postconditions (20ms)
└─ Span: event.emit (5ms)
```

---

## Future Directions

### v6.1: Distributed ΔGate

- **Multi-node coordination**: CRDTs for concurrent Δ
- **Consensus**: Raft/Paxos for Δ ordering
- **Sharding**: Partition Δ by entity namespace

### v6.2: Optimistic Δ

- **Apply immediately**: Don't wait for full reconcile
- **Compensate on conflict**: Generate inverse Δ if precondition fails later
- **Use case**: High-throughput scenarios where conflicts are rare

### v6.3: Smart Contracts

- **Programmable policies**: Deploy hooks as WASM modules
- **Gas metering**: Resource limits for hook execution
- **Formal verification**: Prove hook properties (termination, correctness)

---

## References

- [PROGRAM_CHARTER.md](PROGRAM_CHARTER.md) - v6 overall architecture
- [KnowledgeStore.mjs](../../packages/kgc-substrate/src/KnowledgeStore.mjs) - Substrate implementation
- [receipt-core.mjs](../../packages/yawl/src/receipt-core.mjs) - Receipt generation
- [registry.mjs](../../packages/kgc-cli/src/lib/registry.mjs) - CLI integration
- [ADR-001: Why ΔGate](ADRs/001-deltagate-rationale.md) (TODO)

---

**Last Updated**: 2025-12-27
**Author**: System Architecture Designer
**Review Status**: Draft
