# Probe Receipts Integration Guide

## Overview

This guide provides practical examples of using probe receipts in v6-core systems.

The probe receipt system provides three layers of proof:
1. **Observation Layer** - Individual observations with determinism proof
2. **Chain Layer** - Per-agent hash chains with integrity proof
3. **Merge Layer** - Orchestrator merge with merkle proof + conflict resolution

---

## Part 1: Creating Observations

### 1.1 Simple Observation Creation

```javascript
import {
  createProbeObservationReceipt,
} from '@unrdf/v6-core/receipts/probe';

// First observation (genesis)
const obsReceipt1 = await createProbeObservationReceipt({
  agentId: 'agent-1',
  observationIndex: 1,
  payload: {
    timestamp: Date.now(),
    value: 42,
    source: 'sensor-a',
  },
  domain: 'network',
});

// Second observation (chains to first)
const obsReceipt2 = await createProbeObservationReceipt({
  agentId: 'agent-1',
  observationIndex: 2,
  payload: {
    timestamp: Date.now() + 1000,
    value: 43,
    source: 'sensor-a',
  },
  domain: 'network',
}, obsReceipt1);  // Previous receipt for chaining
```

**Result Structure**:
```json
{
  "id": "550e8400-e29b-41d4-a716-446655440000",
  "receiptType": "probe-observation",
  "agentId": "agent-1",
  "observationIndex": 1,
  "domain": "network",
  "obsHash": "a1b2c3d4...(64 hex chars)",
  "prevHash": null,
  "previousHash": null,
  "observation": {
    "payload": { ... },
    "timestamp": 1234567890000000000n,
    "hash": "a1b2c3d4...",
    "metadata": {
      "serializationVersion": "1.0",
      "encoding": "json-deterministic",
      "deterministic": true
    }
  },
  "checks": [
    {
      "checkType": "hash-recompute",
      "passed": true,
      "details": { ... }
    },
    {
      "checkType": "serialization-stable",
      "passed": true,
      "details": { ... }
    },
    {
      "checkType": "payload-integrity",
      "passed": true,
      "details": { ... }
    }
  ],
  "payloadHash": "b2c3d4e5...",
  "receiptHash": "f1e2d3c4...",
  "t_ns": 1234567890123456789n,
  "timestamp_iso": "2025-01-15T10:30:45.123Z"
}
```

### 1.2 Building a Complete Chain

```javascript
import {
  createProbeObservationReceipt,
} from '@unrdf/v6-core/receipts/probe';

async function buildObservationChain(agentId, observations, domain) {
  const chain = [];
  let previousReceipt = null;

  for (let i = 0; i < observations.length; i++) {
    const receipt = await createProbeObservationReceipt({
      agentId,
      observationIndex: i + 1,
      payload: observations[i],
      domain,
    }, previousReceipt);

    chain.push(receipt);
    previousReceipt = receipt;
  }

  return chain;
}

// Usage
const rawObservations = [
  { value: 10, timestamp: Date.now() },
  { value: 11, timestamp: Date.now() + 1000 },
  { value: 12, timestamp: Date.now() + 2000 },
];

const agent1Chain = await buildObservationChain(
  'agent-1',
  rawObservations,
  'network'
);

const agent2Chain = await buildObservationChain(
  'agent-2',
  rawObservations.map(o => ({ ...o, value: o.value + 100 })),
  'system'
);
```

**Chain Structure**:
```
agent-1: obs[1] → obs[2] → obs[3]
         hash₁  → hash₂  → hash₃
         │        │        │
         prevHash prevHash prevHash
         links to  links to  links to
         chain previous hashes

agent-2: obs[1] → obs[2]
         hash₁  → hash₂
```

---

## Part 2: Verifying Observations

### 2.1 Verify Single Observation

```javascript
import {
  verifyProbeObservation,
} from '@unrdf/v6-core/receipts/probe/verifiers/observation-verifier.mjs';

const result = await verifyProbeObservation(obsReceipt);

if (result.valid) {
  console.log('✓ Observation is valid and deterministic');
  console.log('  - Hash integrity:', result.checks.hashIntegrityValid);
  console.log('  - Determinism:', result.checks.determinismValid);
} else {
  console.log('✗ Observation failed verification');
  console.log('  Errors:', result.errors);
}
```

**Verification Checks**:
1. **Schema** - Valid JSON schema
2. **Hash Integrity** - Payload hash matches
3. **Determinism** - All 3 stability checks pass
4. **Chain Integrity** - Receipt hash valid

### 2.2 Verify Complete Chain

```javascript
import {
  verifyProbeChain,
} from '@unrdf/v6-core/receipts/probe/verifiers/chain-verifier.mjs';

const chainResult = await verifyProbeChain(agent1Chain, 'agent-1');

if (chainResult.valid) {
  console.log(`✓ Chain valid: ${chainResult.chainLength} observations`);
  console.log(`  Final hash: ${chainResult.chainFinalHash}`);
} else {
  console.log('✗ Chain failed verification');
  chainResult.errors.forEach(err => console.log(`  - ${err}`));
}
```

**Chain Verification Checks**:
1. **Genesis** - First obs has no previous
2. **Chain Links** - Each obs links to previous via obsHash
3. **Observations** - All obs individually valid
4. **Temporal** - Monotonic timestamps

---

## Part 3: Creating Merges

### 3.1 Extract Shard Information

```javascript
import {
  createProbeMergeReceipt,
} from '@unrdf/v6-core/receipts/probe';

import {
  extractShardInfo,
} from '@unrdf/v6-core/receipts/probe/verifiers/chain-verifier.mjs';

// Build chains (from Part 1)
const chains = {
  'agent-1': agent1Chain,
  'agent-2': agent2Chain,
  'agent-3': agent3Chain,
};

// Extract shard info
const shards = extractShardInfo(chains);

// Shards are automatically sorted by agentId for determinism
console.log(shards);
// Output:
// [
//   {
//     agentId: 'agent-1',
//     chainFinalHash: 'f1e2d3c4...',
//     obsCount: 3,
//     domain: 'network'
//   },
//   {
//     agentId: 'agent-2',
//     chainFinalHash: 'g2f3e4d5...',
//     obsCount: 2,
//     domain: 'system'
//   },
//   ...
// ]
```

### 3.2 Create Merge Receipt

```javascript
const mergeReceipt = await createProbeMergeReceipt({
  mergeId: 'merge-20250115-001',
  shards: shards,  // From extractShardInfo
  conflicts: null,  // null = no conflicts, deterministic
  orchestratorId: 'orchestrator-main',
  mergedAt: new Date().toISOString(),
});

console.log('Merge Receipt:');
console.log(`  Merge ID: ${mergeReceipt.mergeId}`);
console.log(`  Shards: ${mergeReceipt.shards.length}`);
console.log(`  Merkle Root: ${mergeReceipt.merkleRoot}`);
console.log(`  Proof Path Steps: ${mergeReceipt.proofPath.length}`);
console.log(`  Conflicts: ${mergeReceipt.conflicts ? mergeReceipt.conflicts.length : 'none'}`);
```

**Merkle Tree Example** (3 agents):
```
           merkleRoot
           /        \
         P1          L3
        /  \
      L1    L2

L1 = agent-1.chainFinalHash
L2 = agent-2.chainFinalHash
L3 = agent-3.chainFinalHash

Tree built bottom-up from sorted leaves
All nodes deterministically computed
```

### 3.3 Handle Conflicts

```javascript
// If conflicts are detected during merge
const conflicts = [
  {
    type: 'hash-divergence',
    agents: ['agent-2', 'agent-3'],
    description: 'Agent 2 and 3 report different hashes for same observation index',
    resolution: 'Manual review required - network partition suspected'
  }
];

const conflictingMergeReceipt = await createProbeMergeReceipt({
  mergeId: 'merge-20250115-002',
  shards: shards,
  conflicts: conflicts,  // Not null - conflicts found
  orchestratorId: 'orchestrator-main',
});

console.log('Merge has conflicts - NOT conflict-free');
console.log('Conflicts:', conflictingMergeReceipt.conflicts);
```

---

## Part 4: Verifying Merges

### 4.1 Verify Merge Structure

```javascript
import {
  verifyProbeMerge,
} from '@unrdf/v6-core/receipts/probe/verifiers/merge-verifier.mjs';

const mergeVerification = await verifyProbeMerge(mergeReceipt, chains);

if (mergeVerification.valid) {
  if (mergeVerification.conflictFree) {
    console.log('✓ Merge is VALID and CONFLICT-FREE');
  } else {
    console.log('✓ Merge is valid but has conflicts');
    console.log('  Conflicts:', mergeReceipt.conflicts);
  }
} else {
  console.log('✗ Merge verification failed');
  mergeVerification.errors.forEach(err => console.log(`  - ${err}`));
}
```

**Merge Verification Checks**:
1. **Schema** - Valid JSON schema
2. **Shard Hashes** - Match actual chain final hashes
3. **Merkle Root** - Correctly computed
4. **Conflict Status** - Consistent with results

### 4.2 End-to-End Verification

```javascript
import {
  verifyProbeMergeComplete,
} from '@unrdf/v6-core/receipts/probe/verifiers/merge-verifier.mjs';

const completeResult = await verifyProbeMergeComplete(mergeReceipt, chains);

console.log('End-to-End Verification:');
console.log(`  Overall Valid: ${completeResult.valid}`);
console.log(`  Chains Valid: ${completeResult.chainsValid}`);
console.log(`  Merge Valid: ${completeResult.mergeValid}`);
console.log(`  Deterministic: ${completeResult.deterministic}`);
console.log(`  Conflict-Free: ${completeResult.conflictFree}`);
console.log(`  Chains Verified: ${completeResult.chainCount}`);
console.log(`  Observations Verified: ${completeResult.observationCount}`);

if (!completeResult.valid) {
  console.log('Errors:');
  completeResult.errors.forEach(err => console.log(`  - ${err}`));
}
```

---

## Part 5: Creating Verification Receipts

### 5.1 Build Complete Verification

```javascript
import {
  createProbeVerificationReceipt,
} from '@unrdf/v6-core/receipts/probe';

import {
  generateCertificateChain,
} from '@unrdf/v6-core/receipts/probe/verifiers/merge-verifier.mjs';

const verifyResult = await verifyProbeMergeComplete(mergeReceipt, chains);

// Generate certificate chain (audit trail)
const certificateChain = generateCertificateChain(mergeReceipt, chains);

// Create verification receipt
const verificationReceipt = await createProbeVerificationReceipt({
  verificationId: `verification-${Date.now()}`,
  mergeReceiptHash: mergeReceipt.receiptHash,
  verifications: [
    {
      checkType: 'chain-integrity',
      passed: verifyResult.chainsValid,
      details: { chainCount: verifyResult.chainCount }
    },
    {
      checkType: 'observation-hash-recompute',
      passed: verifyResult.chainsValid,
      details: { obsCount: verifyResult.observationCount }
    },
    {
      checkType: 'merkle-root-recompute',
      passed: verifyResult.mergeValid,
    },
  ],
  deterministic: verifyResult.deterministic,
  conflictFree: verifyResult.conflictFree,
  certificateChain: certificateChain,
  obsCount: verifyResult.observationCount,
  agentCount: verifyResult.chainCount,
  verifierId: 'verifier-main',
  verifiedAt: new Date().toISOString(),
});

console.log('Verification Receipt Created:');
console.log(`  ID: ${verificationReceipt.verificationId}`);
console.log(`  Deterministic: ${verificationReceipt.deterministic}`);
console.log(`  Conflict-Free: ${verificationReceipt.conflictFree}`);
console.log(`  Certificate Chain Length: ${verificationReceipt.certificateChain.length}`);
```

### 5.2 Analyze Verification Results

```javascript
import {
  summarizeVerification,
  getFailedChecks,
  getConfidenceScore,
} from '@unrdf/v6-core/receipts/probe/verification-receipt.mjs';

// Get summary
const summary = summarizeVerification(verificationReceipt);
console.log('Verification Summary:');
console.log(`  Passed: ${summary.passed}`);
console.log(`  Total Checks: ${summary.totalChecks}`);
console.log(`  Passed: ${summary.passedChecks}`);
console.log(`  Failed: ${summary.failedChecks}`);
console.log(`  Deterministic: ${summary.deterministic}`);
console.log(`  Conflict-Free: ${summary.conflictFree}`);

// Get failed checks
const failedChecks = getFailedChecks(verificationReceipt);
if (failedChecks.length > 0) {
  console.log('\nFailed Checks:');
  failedChecks.forEach(check => {
    console.log(`  - ${check.checkType}: ${check.errorMessage}`);
  });
}

// Get confidence score
const confidence = getConfidenceScore(verificationReceipt);
console.log(`\nConfidence Score: ${confidence}/100`);
```

---

## Part 6: Storage & Retrieval

### 6.1 Store Receipts

```javascript
// Store observation receipts
const observationDB = new Map();
observationDB.set(obsReceipt.id, obsReceipt);

// Store chain metadata
const chainDB = new Map();
chainDB.set('agent-1', {
  agentId: 'agent-1',
  chainFinalHash: agent1Chain[agent1Chain.length - 1].receiptHash,
  obsCount: agent1Chain.length,
  domain: 'network',
  receipts: agent1Chain.map(r => r.id),
});

// Store merge
const mergeDB = new Map();
mergeDB.set(mergeReceipt.mergeId, mergeReceipt);

// Store verification
const verificationDB = new Map();
verificationDB.set(verificationReceipt.verificationId, verificationReceipt);
```

### 6.2 Retrieve & Verify

```javascript
async function retrieveAndVerifyChain(agentId) {
  const chainMetadata = chainDB.get(agentId);
  if (!chainMetadata) return null;

  // Retrieve receipts
  const receipts = chainMetadata.receipts.map(id => observationDB.get(id));

  // Verify chain
  const result = await verifyProbeChain(receipts, agentId);
  return {
    metadata: chainMetadata,
    receipts: receipts,
    verification: result,
  };
}

// Usage
const retrievedChain = await retrieveAndVerifyChain('agent-1');
console.log(`Chain ${retrievedChain.metadata.agentId}:`);
console.log(`  Valid: ${retrievedChain.verification.valid}`);
console.log(`  Final Hash: ${retrievedChain.verification.chainFinalHash}`);
```

---

## Part 7: v6-core Integration Points

### 7.1 Extending Receipt Factory

```javascript
// In v6-core/src/receipts/index.mjs

import {
  createProbeObservationReceipt,
  createProbeMergeReceipt,
  createProbeVerificationReceipt,
} from './probe/index.mjs';

// Extended factory
export async function createReceipt(type, event, previousReceipt = null) {
  switch (type) {
    // Existing types
    case 'execution':
    case 'allocation':
    case 'compile':
    case 'verification':
      return createExistingReceipt(type, event, previousReceipt);

    // NEW: Probe types
    case 'probe-observation':
      return createProbeObservationReceipt(event, previousReceipt);

    case 'probe-merge':
      return createProbeMergeReceipt(event);

    case 'probe-verification':
      return createProbeVerificationReceipt(event);

    default:
      throw new Error(`Unknown receipt type: ${type}`);
  }
}
```

### 7.2 Extending Receipt Verifier

```javascript
import {
  verifyProbeObservation,
} from './probe/verifiers/observation-verifier.mjs';

import {
  verifyProbeMerge,
} from './probe/verifiers/merge-verifier.mjs';

// Extended verifier
export async function verifyReceipt(receipt, context = {}) {
  // First verify base receipt
  const baseVerification = await verifyBaseReceipt(receipt);
  if (!baseVerification.valid) {
    return baseVerification;
  }

  // Then type-specific verification
  switch (receipt.receiptType) {
    case 'probe-observation':
      return verifyProbeObservation(receipt);

    case 'probe-merge':
      return verifyProbeMerge(receipt, context.chains);

    // ... existing types
    default:
      return baseVerification;
  }
}
```

---

## Part 8: Error Handling & Recovery

### 8.1 Handling Determinism Failures

```javascript
async function handleDeterminismFailure(obsReceipt, error) {
  const failedChecks = obsReceipt.checks.filter(c => !c.passed);

  for (const check of failedChecks) {
    if (check.checkType === 'hash-recompute') {
      console.error('Payload hash mismatch - payload may have been modified');
      console.error('  Details:', check.details);
    } else if (check.checkType === 'serialization-stable') {
      console.error('Serialization not stable - payload not deterministic');
      console.error('  Hashes:', check.details.hashes);
    }
  }

  // Cannot proceed - observation is not deterministic
  throw new Error('Observation determinism verification failed');
}
```

### 8.2 Handling Chain Breaks

```javascript
async function repairChain(chain) {
  const errors = [];

  for (let i = 0; i < chain.length - 1; i++) {
    const current = chain[i];
    const next = chain[i + 1];

    if (next.prevHash !== current.obsHash) {
      errors.push({
        position: i,
        issue: 'Chain link broken',
        expected: current.obsHash,
        actual: next.prevHash,
      });
    }
  }

  if (errors.length > 0) {
    throw new Error(`Chain has ${errors.length} broken links - cannot repair`);
  }

  return chain;
}
```

### 8.3 Handling Merge Conflicts

```javascript
async function resolveMergeConflicts(mergeReceipt) {
  if (!mergeReceipt.conflicts || mergeReceipt.conflicts.length === 0) {
    return { resolved: true, result: mergeReceipt };
  }

  const conflicts = mergeReceipt.conflicts;

  for (const conflict of conflicts) {
    console.log(`Conflict: ${conflict.type}`);
    console.log(`  Agents: ${conflict.agents.join(', ')}`);
    console.log(`  Description: ${conflict.description}`);

    if (conflict.resolution) {
      console.log(`  Suggested Resolution: ${conflict.resolution}`);
    }
  }

  // Cannot automatically resolve - requires manual intervention
  throw new Error(`Cannot auto-resolve ${conflicts.length} conflicts`);
}
```

---

## Summary

### Complete Workflow

```javascript
// 1. Build observation chains from raw data
const chain1 = await buildObservationChain('agent-1', observations1, 'network');
const chain2 = await buildObservationChain('agent-2', observations2, 'system');

// 2. Verify chains
const chainVerify1 = await verifyProbeChain(chain1, 'agent-1');
const chainVerify2 = await verifyProbeChain(chain2, 'agent-2');

// 3. Create merge
const shards = extractShardInfo({ 'agent-1': chain1, 'agent-2': chain2 });
const mergeReceipt = await createProbeMergeReceipt({
  mergeId: `merge-${Date.now()}`,
  shards: shards,
});

// 4. Verify merge
const mergeVerify = await verifyProbeMergeComplete(
  mergeReceipt,
  { 'agent-1': chain1, 'agent-2': chain2 }
);

// 5. Create verification receipt
const certChain = generateCertificateChain(mergeReceipt, { 'agent-1': chain1, 'agent-2': chain2 });
const verifyReceipt = await createProbeVerificationReceipt({
  verificationId: `verify-${Date.now()}`,
  mergeReceiptHash: mergeReceipt.receiptHash,
  verifications: buildVerificationChecks(mergeVerify),
  deterministic: mergeVerify.deterministic,
  conflictFree: mergeVerify.conflictFree,
  certificateChain: certChain,
  obsCount: mergeVerify.observationCount,
  agentCount: mergeVerify.chainCount,
});

console.log('✓ Complete workflow succeeded');
console.log(`  Deterministic: ${verifyReceipt.deterministic}`);
console.log(`  Conflict-Free: ${verifyReceipt.conflictFree}`);
console.log(`  Confidence: ${getConfidenceScore(verifyReceipt)}/100`);
```

---

## Files Reference

**Created Implementation Files**:
- `/home/user/unrdf/packages/v6-core/src/receipts/probe/observation-receipt.mjs` - Observation schema
- `/home/user/unrdf/packages/v6-core/src/receipts/probe/merge-receipt.mjs` - Merge schema
- `/home/user/unrdf/packages/v6-core/src/receipts/probe/verification-receipt.mjs` - Verification schema
- `/home/user/unrdf/packages/v6-core/src/receipts/probe/index.mjs` - Factory functions
- `/home/user/unrdf/packages/v6-core/src/receipts/probe/verifiers/observation-verifier.mjs` - Observation verification
- `/home/user/unrdf/packages/v6-core/src/receipts/probe/verifiers/chain-verifier.mjs` - Chain verification
- `/home/user/unrdf/packages/v6-core/src/receipts/probe/verifiers/merge-verifier.mjs` - Merge verification
- `/home/user/unrdf/packages/v6-core/src/receipts/probe/verifiers/index.mjs` - Verifier exports

**Design Documents**:
- `/home/user/unrdf/docs/sparc/pseudocode-probe-receipts-merkle.md` - Complete SPARC pseudocode
