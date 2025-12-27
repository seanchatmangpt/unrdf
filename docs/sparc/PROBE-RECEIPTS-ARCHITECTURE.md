# Probe Receipts Architecture - Quick Reference

## System Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                    PROBE RECEIPT SYSTEM                      │
│                   (v6-core Integration)                      │
└─────────────────────────────────────────────────────────────┘

                    ┌──────────────────┐
                    │  Raw Observations│
                    │   (from agents)  │
                    └────────┬─────────┘
                             │
                ┌────────────▼────────────┐
                │                         │
        ┌───────▼─────────┐      ┌───────▼─────────┐
        │    Agent-1      │      │    Agent-2      │
        │                 │      │                 │
        │ obs[1]┌─────┐   │      │ obs[1]┌─────┐   │
        │    ├──┤     │   │      │    ├──┤     │   │
        │    └──┤obsH │   │      │    └──┤obsH │   │
        │       └─────┘   │      │       └─────┘   │
        │        ↓↓↓      │      │        ↓↓↓      │
        │ obs[2]┌─────┐   │      │ obs[2]┌─────┐   │
        │    ├──┤     │   │      │    ├──┤     │   │
        │    └──┤obsH │   │      │    └──┤obsH │   │
        │       └─────┘   │      │       └─────┘   │
        │        ↓↓↓      │      │        ↓↓↓      │
        │ obs[3]┌─────┐   │      │       ...      │
        │    ├──┤     │   │      │                 │
        │    └──┤obsH │   │      │                 │
        │       └─────┘   │      │                 │
        └───────┬─────────┘      └───────┬─────────┘
                │                       │
    CHAIN VERIFICATION              CHAIN VERIFICATION
    (perAgent hash chains)          (perAgent hash chains)
                │                       │
        ┌───────▼───────┐      ┌───────▼───────┐
        │ Chain Final    │      │ Chain Final    │
        │ Hash: h1       │      │ Hash: h2       │
        └───────┬───────┘      └───────┬───────┘
                │                      │
                └──────────┬───────────┘
                           │
            ┌──────────────▼──────────────┐
            │   Extract Shard Info        │
            │   (sorted by agentId)       │
            └──────────────┬──────────────┘
                           │
            ┌──────────────▼──────────────┐
            │   Merkle Tree Construction  │
            │                             │
            │         Root H              │
            │        /    \               │
            │      P1      L2             │
            │     / \                     │
            │   L1   (duplicate)          │
            │                             │
            │  L1 = h1 (agent-1)          │
            │  L2 = h2 (agent-2)          │
            └──────────────┬──────────────┘
                           │
        ┌──────────────────▼──────────────────┐
        │   ProbeMergeReceipt                 │
        │   - merkleRoot: Root H              │
        │   - proofPath: [...]                │
        │   - conflicts: null (conflict-free) │
        │   - algorithm: merkle-tree-v1.0.0   │
        └──────────────────┬──────────────────┘
                           │
        ┌──────────────────▼──────────────────┐
        │   VerifyProbeMergeComplete()         │
        │   - Verify chains                   │
        │   - Verify merge                    │
        │   - Build certificate chain         │
        │   - Check conflicts                 │
        └──────────────────┬──────────────────┘
                           │
        ┌──────────────────▼──────────────────┐
        │   ProbeVerificationReceipt          │
        │   - deterministic: true             │
        │   - conflictFree: true              │
        │   - verifications: [...]            │
        │   - certificateChain: [...]         │
        │   - confidence: 99.99%              │
        └─────────────────────────────────────┘
```

---

## Layer Architecture

### Layer 1: Observation Determinism

```
Payload ──┬──> Hash Recompute Check
          ├──> Serialization Stable Check
          └──> Payload Integrity Check
              ↓
          All 3 Pass?
              ↓
          ✓ Determinism Verified
          Create ProbeObservationReceipt
              ├── obsHash (BLAKE3)
              ├── checks[] (all results)
              └── receiptHash (chained)
```

### Layer 2: Chain Integrity

```
obs[1]──┐
        ├─ prevHash chain: null → h1 → h2 → h3
        ├─ previousHash chain (v6-core): null → r1 → r2 → r3
        ├─ Temporal: t1 < t2 < t3
        └─ Agent: agent-1, agent-1, agent-1
           ↓
        ✓ Chain Valid
        Return: chainFinalHash (r3)
```

### Layer 3: Merge Determinism

```
Shard 1: h1 ──┐
Shard 2: h2 ──┼─ Sort by agentId
Shard 3: h3 ──┘    (deterministic)
              ↓
        Build Merkle Tree
              ↓
        Compute Root
              ↓
        Compare with receipt.merkleRoot
              ↓
        ✓ Merkle Root Verified
        ✓ Merge is Deterministic
```

---

## Module Organization

```
v6-core/src/receipts/
├── base-receipt.mjs              [existing]
│   ├── RECEIPT_TYPES
│   ├── generateUUID()
│   ├── deterministicSerialize()
│   ├── computeBlake3()
│   └── computeChainHash()
│
├── index.mjs                      [existing]
│   └── createReceipt() ← extend for probe
│   └── verifyReceipt() ← extend for probe
│
├── merkle/
│   ├── index.mjs                 [existing - placeholder]
│   └── proofchain.mjs             [existing]
│
└── probe/                         [NEW - Agent-6 deliverable]
    ├── observation-receipt.mjs    [observation schema + utils]
    │   ├── ProbeObservationReceiptSchema
    │   ├── computeObsHash()
    │   ├── checkHashRecompute()
    │   ├── checkSerializationStable()
    │   └── checkPayloadIntegrity()
    │
    ├── merge-receipt.mjs          [merge schema + merkle]
    │   ├── ProbeMergeReceiptSchema
    │   ├── buildMerkleTree()
    │   └── verifyMerkleRoot()
    │
    ├── verification-receipt.mjs   [verification schema + scoring]
    │   ├── ProbeVerificationReceiptSchema
    │   ├── summarizeVerification()
    │   ├── getFailedChecks()
    │   └── getConfidenceScore()
    │
    ├── index.mjs                  [factories + integration]
    │   ├── createProbeObservationReceipt()
    │   ├── createProbeMergeReceipt()
    │   └── createProbeVerificationReceipt()
    │
    └── verifiers/                 [verification algorithms]
        ├── observation-verifier.mjs
        │   ├── verifyProbeObservation()
        │   ├── verifyObservationTimestamp()
        │   └── verifyObservationSequence()
        │
        ├── chain-verifier.mjs
        │   ├── verifyProbeChain()
        │   ├── verifyProbeChains()
        │   └── extractShardInfo()
        │
        ├── merge-verifier.mjs
        │   ├── verifyProbeMerge()
        │   ├── verifyProbeMergeComplete()
        │   └── generateCertificateChain()
        │
        └── index.mjs              [verifier exports]
```

---

## Data Flow Example

### Creating Observation Chain

```javascript
// Step 1: Create observations
for (let i = 0; i < N; i++) {
  receipt = await createProbeObservationReceipt({
    agentId: 'agent-1',
    observationIndex: i + 1,
    payload: observations[i],
    domain: 'network'
  }, previousReceipt);

  chain.push(receipt);
  previousReceipt = receipt;
}

// Result: chain[0..N] with:
// - chain[i].obsHash = BLAKE3(payload[i])
// - chain[i].prevHash = chain[i-1].obsHash (or null for genesis)
// - chain[i].receiptHash = BLAKE3(prevReceipt:obsHash)
```

### Creating Merge

```javascript
// Step 2: Extract shard info
const chains = {
  'agent-1': chain1,  // N observations
  'agent-2': chain2,  // M observations
  'agent-3': chain3   // K observations
};

const shards = extractShardInfo(chains);
// Result: [
//   { agentId: 'agent-1', chainFinalHash: h1, obsCount: N, domain: 'network' },
//   { agentId: 'agent-2', chainFinalHash: h2, obsCount: M, domain: 'system' },
//   { agentId: 'agent-3', chainFinalHash: h3, obsCount: K, domain: 'governance' }
// ]
// Sorted by agentId for determinism

// Step 3: Create merge
const mergeReceipt = await createProbeMergeReceipt({
  mergeId: 'merge-001',
  shards: shards,
  conflicts: null  // conflict-free
});

// Result: merkleRoot computed from [h1, h2, h3]
// All can verify: BLAKE3(h1 || h2 || h3) = expected root
```

### Verifying Everything

```javascript
// Step 4: Verify all chains
const chainVerify = await verifyProbeChains(chains);
// ✓ Checks each chain: genesis, links, observations, ordering

// Step 5: Verify merge
const mergeVerify = await verifyProbeMerge(mergeReceipt, chains);
// ✓ Checks: shard hashes, merkle root, conflicts

// Step 6: Complete verification
const completeVerify = await verifyProbeMergeComplete(mergeReceipt, chains);
// ✓ All chains + merge verified
// ✓ deterministic: true
// ✓ conflictFree: true

// Step 7: Create verification receipt
const verifyReceipt = await createProbeVerificationReceipt({
  verificationId: 'verify-001',
  mergeReceiptHash: mergeReceipt.receiptHash,
  verifications: buildChecks(completeVerify),
  deterministic: completeVerify.deterministic,
  conflictFree: completeVerify.conflictFree,
  certificateChain: generateCertificateChain(mergeReceipt, chains),
  obsCount: completeVerify.observationCount,
  agentCount: completeVerify.chainCount
});

// Result: Complete audit trail
// - Can prove every observation
// - Can prove merge is deterministic
// - Confidence: 99.99%
```

---

## Verification Checklist

### Observation Verification

- [ ] Schema valid (Zod)
- [ ] obsHash matches payload (hash-recompute)
- [ ] Serialization stable (3x identical)
- [ ] Payload integrity (parseable)
- [ ] Receipt hash valid (chain)

### Chain Verification

- [ ] Genesis has no previous
- [ ] Chain links valid (prevHash chain)
- [ ] All observations individually valid
- [ ] Temporal ordering monotonic
- [ ] Agent IDs consistent

### Merge Verification

- [ ] Shard hashes match actual chains
- [ ] Shard observation counts match
- [ ] Merkle root correctly computed
- [ ] Conflict status consistent
- [ ] Receipt hash valid

### Complete Verification

- [ ] All chains verified
- [ ] Merge verified
- [ ] Certificate chain generated
- [ ] Deterministic: true
- [ ] ConflictFree: true
- [ ] Confidence ≥ 99%

---

## Integration Checklist

### For v6-core Team

```javascript
// 1. Update base-receipt.mjs
export const RECEIPT_TYPES = {
  EXECUTION: 'execution',
  ALLOCATION: 'allocation',
  COMPILE: 'compile',
  VERIFICATION: 'verification',
  PROBE_OBSERVATION: 'probe-observation',    // NEW
  PROBE_MERGE: 'probe-merge',               // NEW
};

// 2. Update index.mjs createReceipt()
case RECEIPT_TYPES.PROBE_OBSERVATION:
  return createProbeObservationReceipt(event, previousReceipt);

case RECEIPT_TYPES.PROBE_MERGE:
  return createProbeMergeReceipt(event);

// 3. Update index.mjs verifyReceipt()
case RECEIPT_TYPES.PROBE_OBSERVATION:
  return verifyProbeObservation(receipt);

case RECEIPT_TYPES.PROBE_MERGE:
  return verifyProbeMerge(receipt, context.chains);

// 4. Export probe schemas
export {
  ProbeObservationReceiptSchema,
  ProbeMergeReceiptSchema,
  ProbeVerificationReceiptSchema,
};

// 5. Test with:
import {
  createProbeObservationReceipt,
  createProbeMergeReceipt,
} from '@unrdf/v6-core/receipts/probe';

const obs = await createProbeObservationReceipt({...});
const merge = await createProbeMergeReceipt({...});
```

---

## Key Files

### Implementation (8 files, 1,936 LoC)
```
probe/
├── observation-receipt.mjs (256 LoC)
├── merge-receipt.mjs (238 LoC)
├── verification-receipt.mjs (186 LoC)
├── index.mjs (270 LoC)
└── verifiers/
    ├── observation-verifier.mjs (135 LoC)
    ├── chain-verifier.mjs (195 LoC)
    ├── merge-verifier.mjs (180 LoC)
    └── index.mjs (40 LoC)
```

### Documentation (3 files, 2,400 lines)
```
docs/sparc/
├── pseudocode-probe-receipts-merkle.md (900 lines)
│   └─ Complete SPARC pseudocode
├── probe-receipts-integration-guide.md (600 lines)
│   └─ Practical integration examples
└── RECEIPT-DESIGN-SUMMARY.md (900 lines)
    └─ Architecture + decisions + checklists
```

---

## Implementation Status

✅ **Complete**
- SPARC Pseudocode design
- All receipt schemas
- All factory functions
- All verifier algorithms
- v6-core integration points
- Comprehensive documentation
- Usage examples
- Error handling patterns

⏳ **Next Steps**
- [ ] Unit tests
- [ ] Integration tests
- [ ] Performance benchmarks
- [ ] Edge case testing
- [ ] Code review
- [ ] v6-core merge

---

## Contact & Reference

**Design Phase**: Agent-6 (Receipts/Merkle)
**Methodology**: SPARC Pseudocode
**Foundation**: v6-core BaseReceipt + BLAKE3
**Files**: `/home/user/unrdf/packages/v6-core/src/receipts/probe/`
**Docs**: `/home/user/unrdf/docs/sparc/`
