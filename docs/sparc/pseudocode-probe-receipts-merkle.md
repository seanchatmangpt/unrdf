# SPARC Pseudocode: Probe Receipts & Merkle Proof Chains

## Domain Overview

**Probe System Architecture**:
- Agents (Shards) execute observations on distributed systems
- Each agent creates a deterministic hash chain of observations
- Orchestrator merges per-agent chains into unified merkle-proof structure
- Verification proves determinism + conflict-free merge (or lists conflicts)

**Receipt Types**:
1. **ProbeObservationReceipt** - Per-agent observation proof
2. **ProbeMergeReceipt** - Orchestrator merge proof with merkle root
3. **ProbeVerificationReceipt** - Proof of determinism/conflict validation

---

## Part 1: Data Structures & Schemas

### 1.1 ProbeObservationReceipt Schema

```
DATA STRUCTURE: ProbeObservationReceipt

INHERITS: BaseReceipt
  id: UUID v4
  receiptType: "probe-observation" (literal)
  t_ns: BigInt (nanoseconds)
  timestamp_iso: String (ISO 8601)
  previousHash: String (64-char BLAKE3 hex) | null
  payloadHash: String (64-char BLAKE3 hex)
  receiptHash: String (64-char BLAKE3 hex, chained)

PROBE-SPECIFIC FIELDS:
  agentId: String          // e.g., "agent-1", "shard-primary"
  observationIndex: Int    // Sequence number in agent's chain

  obsHash: String          // BLAKE3(observation payload deterministically)
                           // Proves observation was made exactly this way

  prevHash: String | null  // Hash of previous observation in THIS agent's chain
                           // (different from previousHash - forms per-agent chain)

  domain: String           // e.g., "network", "system", "governance"

  observation: Object      // Immutable observation payload
    payload: Any           // The actual data observed
    timestamp: BigInt      // When observed (ns precision)
    hash: String           // BLAKE3(payload deterministically)
    metadata: Object       // Serialization version, encoding, etc.

  checks: Array            // Determinism proof path
    - checkType: String    // "hash-recompute", "serialization-stable"
    - checkValue: String   // Result of check

ATTESTATION (optional):
  algorithm: String        // "ed25519" | "ecdsa-secp256k1"
  publicKey: String        // Hex of public key
  signature: String        // Hex signature
  signer: String           // Agent identity

ZOD SCHEMA:
  z.object({
    id: z.string().uuid(),
    receiptType: z.literal("probe-observation"),
    t_ns: z.bigint(),
    timestamp_iso: z.string(),
    previousHash: z.string().length(64).nullable(),
    payloadHash: z.string().length(64),
    receiptHash: z.string().length(64),
    agentId: z.string().min(1),
    observationIndex: z.number().int().nonnegative(),
    obsHash: z.string().length(64),
    prevHash: z.string().length(64).nullable(),
    domain: z.string().min(1),
    observation: z.object({
      payload: z.any(),
      timestamp: z.bigint(),
      hash: z.string().length(64),
      metadata: z.object({
        serializationVersion: z.string(),
        encoding: z.string(),
        deterministic: z.boolean()
      }).optional()
    }),
    checks: z.array(z.object({
      checkType: z.string(),
      checkValue: z.string(),
      passed: z.boolean()
    })),
    attestation: z.object({
      algorithm: z.string(),
      publicKey: z.string(),
      signature: z.string(),
      signer: z.string().optional()
    }).optional()
  })
```

### 1.2 Per-Agent Hash Chain Structure

```
DATA STRUCTURE: AgentHashChain

Represents immutable chain of observations from single agent.

CHAIN[agent_i]:
  observations: Array<ProbeObservationReceipt>

  INVARIANT: Chain is immutable
    - obs[0].prevHash = null (genesis)
    - obs[j].prevHash = obs[j-1].obsHash  (each links to previous)
    - Each obs[j].receiptHash depends on obs[j].prevHash

  FINAL_HASH: obs[n].receiptHash  (root of agent's chain)

  VERIFICATION PROPERTIES:
    - Can replay all observations from receipts
    - Cannot modify any obs without breaking all downstream hashes
    - Chain is auditable: prove obs[k] included deterministically

EXAMPLE CHAIN (agent-1):
  obs[1]:
    agentId: "agent-1"
    observationIndex: 1
    obsHash: "a1b2c3..." (BLAKE3(payload_1))
    prevHash: null
    receiptHash: "f1e2d3..." (BLAKE3(null || "a1b2c3..."))

  obs[2]:
    agentId: "agent-1"
    observationIndex: 2
    obsHash: "b2c3d4..." (BLAKE3(payload_2))
    prevHash: "a1b2c3..." (points to obs[1].obsHash)
    receiptHash: "e1d2c3..." (BLAKE3("a1b2c3..." || "b2c3d4..."))

  obs[3]:
    agentId: "agent-1"
    observationIndex: 3
    obsHash: "c3d4e5..." (BLAKE3(payload_3))
    prevHash: "b2c3d4..." (points to obs[2].obsHash)
    receiptHash: "d1c2b3..." (BLAKE3("b2c3d4..." || "c3d4e5..."))

  CHAIN_FINAL_HASH = obs[3].receiptHash = "d1c2b3..."
```

### 1.3 ProbeMergeReceipt Schema

```
DATA STRUCTURE: ProbeMergeReceipt

Orchestrator creates one per merge operation. Proves merge deterministic.

INHERITS: BaseReceipt
  id: UUID v4
  receiptType: "probe-merge" (literal)
  t_ns: BigInt (nanoseconds)
  timestamp_iso: String (ISO 8601)
  previousHash: String | null
  payloadHash: String
  receiptHash: String

MERGE-SPECIFIC FIELDS:
  mergeId: String          // Unique merge operation ID

  shards: Array<Object>    // Per-agent chain info
    - agentId: String
    - chainFinalHash: String   // obs[n].receiptHash from that agent
    - obsCount: Int            // Number of observations
    - domain: String           // Domain this agent covered

  merkleRoot: String       // Root of merkle tree over shard hashes
                           // Root = MerkleTree.root([shard[0].chainFinalHash, ...])

  mergeAlgorithm: Object   // Algorithm descriptor
    algorithm: String      // "merkle-tree-deterministic" | "conflict-free"
    version: String        // e.g., "latest"
    parameters: Object     // Leaf order, hash fn, etc.

  conflicts: Array | null  // null = no conflicts (deterministic)
                           // Array = conflicts found (resolution needed)
    - type: String         // "hash-divergence" | "temporal-anomaly"
    - agents: Array<String>
    - description: String

  proofPath: Array         // Merkle proof of root computation
    - level: Int           // Tree level (0 = leaves)
    - position: String     // "left" | "right"
    - hash: String

ZOD SCHEMA:
  z.object({
    id: z.string().uuid(),
    receiptType: z.literal("probe-merge"),
    t_ns: z.bigint(),
    timestamp_iso: z.string(),
    previousHash: z.string().length(64).nullable(),
    payloadHash: z.string().length(64),
    receiptHash: z.string().length(64),
    mergeId: z.string().min(1),
    shards: z.array(z.object({
      agentId: z.string(),
      chainFinalHash: z.string().length(64),
      obsCount: z.number().int().nonnegative(),
      domain: z.string()
    })),
    merkleRoot: z.string().length(64),
    mergeAlgorithm: z.object({
      algorithm: z.string(),
      version: z.string(),
      parameters: z.record(z.any()).optional()
    }),
    conflicts: z.array(z.object({
      type: z.string(),
      agents: z.array(z.string()),
      description: z.string()
    })).nullable(),
    proofPath: z.array(z.object({
      level: z.number().int().nonnegative(),
      position: z.enum(["left", "right"]),
      hash: z.string().length(64)
    }))
  })
```

### 1.4 ProbeVerificationReceipt Schema

```
DATA STRUCTURE: ProbeVerificationReceipt

Proof that merge was deterministic and conflict-free.

INHERITS: BaseReceipt

VERIFICATION-SPECIFIC FIELDS:
  verificationId: String

  mergeReceiptHash: String // Hash of ProbeMergeReceipt being verified

  verifications: Array      // Results of all checks
    - checkType: String     // "observation-hash-recompute" | "chain-integrity" |
                            // "merkle-root-recompute" | "temporal-ordering"
    - agentId: String       // Which agent (or null for global)
    - passed: Boolean
    - details: Object

  deterministic: Boolean    // true = all obs reproduced identically

  conflictFree: Boolean     // true = no conflicts, merge deterministic

  certificateChain: Array   // Proof path linking back to original observations
    - receiptHash: String
    - receiptType: String
    - relationship: String  // "obs-in-chain" | "chain-in-merge" | "merge-verified"

ZOD SCHEMA (abbreviated):
  z.object({
    receiptType: z.literal("probe-verification"),
    verificationId: z.string(),
    mergeReceiptHash: z.string().length(64),
    verifications: z.array(...),
    deterministic: z.boolean(),
    conflictFree: z.boolean(),
    certificateChain: z.array(z.object({
      receiptHash: z.string().length(64),
      receiptType: z.string(),
      relationship: z.string()
    }))
  })
```

---

## Part 2: Algorithms

### 2.1 ALGORITHM: CreateProbeObservationReceipt

```
ALGORITHM: CreateProbeObservationReceipt

INPUT:
  agentId: String
  observationIndex: Int
  payload: Any
  domain: String
  previousReceipt: ProbeObservationReceipt | null

OUTPUT:
  receipt: ProbeObservationReceipt

PRECONDITIONS:
  - payload is deterministically serializable
  - agentId is non-empty
  - observationIndex >= 1
  - (previousReceipt = null) OR (previousReceipt.agentId = agentId)

CONSTANTS:
  SERIALIZATION_VERSION = "1.0"
  ENCODING = "json-deterministic"

BEGIN
  // 1. Generate receipt metadata
  id ← GenerateUUID()
  t_ns ← CurrentTimeNanos()
  timestamp_iso ← ToISO(t_ns)

  // 2. Build observation structure
  observation ← {
    payload: payload,
    timestamp: t_ns,
    metadata: {
      serializationVersion: SERIALIZATION_VERSION,
      encoding: ENCODING,
      deterministic: true
    }
  }

  // 3. Compute observation hash (deterministically)
  obsPayloadSerialized ← DeterministicSerialize(observation.payload)
  obsHash ← BLAKE3(obsPayloadSerialized)
  observation.hash ← obsHash

  // 4. Determine previous hash in agent's chain
  prevHash ← null
  IF previousReceipt ≠ null THEN
    prevHash ← previousReceipt.obsHash  // Link to agent's chain
  END IF

  // 5. Build complete receipt (excluding hash fields)
  receiptData ← {
    id: id,
    receiptType: "probe-observation",
    t_ns: t_ns,
    timestamp_iso: timestamp_iso,
    agentId: agentId,
    observationIndex: observationIndex,
    domain: domain,
    observation: observation,
    obsHash: obsHash,
    prevHash: prevHash,
    checks: []
  }

  // 6. Run determinism checks
  FOR check IN [CheckHashRecompute, CheckSerializationStable] DO
    checkResult ← RunCheck(check, observation, obsHash)
    receiptData.checks.append(checkResult)
    IF NOT checkResult.passed THEN
      RETURN error("Determinism check failed: " + checkResult.type)
    END IF
  END FOR

  // 7. Compute hashes
  // Payload hash: over all non-hash fields
  payloadToHash ← receiptData WITHOUT {receiptHash, payloadHash}
  payloadHash ← BLAKE3(DeterministicSerialize(payloadToHash))

  // Chain hash: previousHash:payloadHash (for v6-core compatibility)
  previousHashForChain ← null
  IF previousReceipt ≠ null THEN
    previousHashForChain ← previousReceipt.receiptHash
  END IF
  chainInput ← (previousHashForChain OR "GENESIS") + ":" + payloadHash
  receiptHash ← BLAKE3(chainInput)

  // 8. Build final receipt
  receipt ← {
    ...receiptData,
    previousHash: previousHashForChain,
    payloadHash: payloadHash,
    receiptHash: receiptHash
  }

  // 9. Validate against schema
  ValidateAgainstSchema(receipt, ProbeObservationReceiptSchema)

  RETURN receipt
END

SUBROUTINE: CheckHashRecompute
  INPUT: observation, expectedHash
  OUTPUT: {checkType, passed, details}

  BEGIN
    serialized ← DeterministicSerialize(observation.payload)
    recomputed ← BLAKE3(serialized)

    RETURN {
      checkType: "hash-recompute",
      passed: (recomputed = expectedHash),
      details: {
        expected: expectedHash,
        computed: recomputed
      }
    }
  END

SUBROUTINE: CheckSerializationStable
  INPUT: observation, hash
  OUTPUT: {checkType, passed, details}

  BEGIN
    // Serialize 3 times, check all match
    ser1 ← DeterministicSerialize(observation.payload)
    ser2 ← DeterministicSerialize(observation.payload)
    ser3 ← DeterministicSerialize(observation.payload)

    h1 ← BLAKE3(ser1)
    h2 ← BLAKE3(ser2)
    h3 ← BLAKE3(ser3)

    stableInternally ← (h1 = h2 AND h2 = h3)
    stableGlobally ← (h1 = hash)

    RETURN {
      checkType: "serialization-stable",
      passed: (stableInternally AND stableGlobally),
      details: {
        internallyStable: stableInternally,
        globallyStable: stableGlobally,
        hashes: [h1, h2, h3]
      }
    }
  END

COMPLEXITY:
  Time: O(|payload|) for serialization + hashing
  Space: O(|payload|) for serialized form + receipts
```

### 2.2 ALGORITHM: BuildAgentHashChain

```
ALGORITHM: BuildAgentHashChain

INPUT:
  agentId: String
  observations: Array<Object>  // Raw observation payloads

OUTPUT:
  chain: Array<ProbeObservationReceipt>

PRECONDITIONS:
  - observations.length >= 1
  - All observations belong to same agent

INVARIANT:
  For all j in [2..n]:
    chain[j].prevHash = chain[j-1].obsHash
    chain[j].receiptHash depends on chain[j].previousHash

BEGIN
  chain ← []
  previousReceipt ← null

  FOR idx ← 1 TO observations.length DO
    obs ← observations[idx - 1]  // 0-indexed

    // Create receipt for this observation
    receipt ← CreateProbeObservationReceipt(
      agentId: agentId,
      observationIndex: idx,
      payload: obs.payload,
      domain: obs.domain,
      previousReceipt: previousReceipt
    )

    // Validate receipt
    IF NOT IsValid(receipt) THEN
      RETURN error("Invalid receipt at index " + idx)
    END IF

    chain.append(receipt)
    previousReceipt ← receipt
  END FOR

  // Validate chain integrity
  result ← VerifyAgentChain(chain)
  IF NOT result.valid THEN
    RETURN error("Chain integrity check failed: " + result.reason)
  END IF

  RETURN chain
END

SUBROUTINE: VerifyAgentChain
  INPUT: chain
  OUTPUT: {valid, reason, checks}

  BEGIN
    checks ← {
      genesisValid: false,
      chainLinksValid: false,
      hashesValid: false,
      orderingValid: false
    }

    // Check genesis
    IF chain[0].prevHash ≠ null THEN
      RETURN {
        valid: false,
        reason: "Genesis receipt must have prevHash = null",
        checks: checks
      }
    END IF
    checks.genesisValid ← true

    // Check chain links
    FOR i ← 1 TO chain.length - 1 DO
      IF chain[i].prevHash ≠ chain[i-1].obsHash THEN
        RETURN {
          valid: false,
          reason: format("Chain broken at index {}: expected prevHash {} got {}",
            i, chain[i-1].obsHash, chain[i].prevHash),
          checks: checks
        }
      END IF
    END FOR
    checks.chainLinksValid ← true

    // Check hash integrity
    FOR receipt IN chain DO
      check ← CheckHashRecompute(receipt.observation, receipt.obsHash)
      IF NOT check.passed THEN
        RETURN {
          valid: false,
          reason: format("Hash check failed for receipt {}", receipt.id),
          checks: checks
        }
      END IF
    END FOR
    checks.hashesValid ← true

    // Check temporal ordering
    FOR i ← 1 TO chain.length - 1 DO
      IF chain[i].t_ns <= chain[i-1].t_ns THEN
        RETURN {
          valid: false,
          reason: format("Temporal violation at index {}", i),
          checks: checks
        }
      END IF
    END FOR
    checks.orderingValid ← true

    RETURN {
      valid: true,
      reason: "All checks passed",
      checks: checks
    }
  END

COMPLEXITY:
  Time: O(n * |payload_avg|) where n = number of observations
        Sequential hashing dominates
  Space: O(n * |receipt|) for receipts storage
```

### 2.3 ALGORITHM: ComputeMerkleRoot

```
ALGORITHM: ComputeMerkleRoot

INPUT:
  shards: Array<{agentId, chainFinalHash}>  // One per agent
  hashFunction: Function                      // BLAKE3
  leafOrder: String                          // "sorted-by-agentId" | "insertion"

OUTPUT:
  merkleRoot: String (64-char hex)
  proofPath: Array<{level, position, hash}>

PRECONDITIONS:
  - shards.length >= 1
  - All chainFinalHash are 64-char hex
  - hashFunction is deterministic
  - leafOrder is valid

CONSTANTS:
  TREE_NAME = "ProbeShardMerkleTree"

BEGIN
  // 1. Prepare leaves
  IF leafOrder = "sorted-by-agentId" THEN
    leaves ← SORT(shards BY agentId).map(s → s.chainFinalHash)
  ELSE IF leafOrder = "insertion" THEN
    leaves ← shards.map(s → s.chainFinalHash)
  ELSE
    RETURN error("Invalid leafOrder: " + leafOrder)
  END IF

  // 2. Build tree bottom-up
  currentLevel ← leaves
  level ← 0
  proofPath ← []

  WHILE currentLevel.length > 1 DO
    nextLevel ← []

    // Process pairs
    FOR i ← 0 TO currentLevel.length - 1 STEP 2 DO
      left ← currentLevel[i]
      right ← currentLevel[i+1] OR left  // Duplicate if odd

      // Hash pair
      pairInput ← left + right
      parentHash ← hashFunction(pairInput)
      nextLevel.append(parentHash)

      // Record in proof path
      proofPath.append({
        level: level,
        position: "left",
        hash: left
      })
      IF right ≠ left THEN
        proofPath.append({
          level: level,
          position: "right",
          hash: right
        })
      END IF
    END FOR

    currentLevel ← nextLevel
    level ← level + 1
  END WHILE

  merkleRoot ← currentLevel[0]

  RETURN {
    merkleRoot: merkleRoot,
    proofPath: proofPath
  }
END

COMPLEXITY:
  Time: O(n log n) for tree construction (n = number of shards)
        Sorting: O(n log n) if leafOrder = "sorted-by-agentId"
        Hashing: O(n) total across all levels
  Space: O(n) for tree nodes + proof path
```

### 2.4 ALGORITHM: CreateProbeMergeReceipt

```
ALGORITHM: CreateProbeMergeReceipt

INPUT:
  mergeId: String
  shards: Array<{
    agentId, chainFinalHash, obsCount, domain
  }>
  conflictDetectionResult: {
    hasConflicts: Boolean,
    conflicts: Array | null
  }

OUTPUT:
  receipt: ProbeMergeReceipt

PRECONDITIONS:
  - mergeId is non-empty
  - shards.length >= 1
  - All shards have valid chainFinalHash (64-char hex)

CONSTANTS:
  ALGORITHM_VERSION = "latest"
  LEAF_ORDER = "sorted-by-agentId"

BEGIN
  // 1. Generate receipt metadata
  id ← GenerateUUID()
  t_ns ← CurrentTimeNanos()
  timestamp_iso ← ToISO(t_ns)

  // 2. Compute merkle root
  merkleResult ← ComputeMerkleRoot(
    shards: shards,
    hashFunction: BLAKE3,
    leafOrder: LEAF_ORDER
  )
  merkleRoot ← merkleResult.merkleRoot
  proofPath ← merkleResult.proofPath

  // 3. Build receipt data
  receiptData ← {
    id: id,
    receiptType: "probe-merge",
    t_ns: t_ns,
    timestamp_iso: timestamp_iso,
    mergeId: mergeId,
    shards: shards,
    merkleRoot: merkleRoot,
    mergeAlgorithm: {
      algorithm: "merkle-tree-deterministic",
      version: ALGORITHM_VERSION,
      parameters: {
        leafOrder: LEAF_ORDER,
        hashFunction: "blake3"
      }
    },
    conflicts: conflictDetectionResult.conflicts,
    proofPath: proofPath
  }

  // 4. Compute hashes
  payloadToHash ← receiptData WITHOUT {receiptHash, payloadHash, previousHash}
  payloadHash ← BLAKE3(DeterministicSerialize(payloadToHash))

  // Chain hash (no previous for merge - it's autonomous)
  chainInput ← "GENESIS:" + payloadHash
  receiptHash ← BLAKE3(chainInput)

  // 5. Build final receipt
  receipt ← {
    ...receiptData,
    previousHash: null,
    payloadHash: payloadHash,
    receiptHash: receiptHash
  }

  // 6. Validate schema
  ValidateAgainstSchema(receipt, ProbeMergeReceiptSchema)

  RETURN receipt
END

COMPLEXITY:
  Time: O(n log n) dominated by merkle root computation
  Space: O(n) for receipt + merkle structure
```

### 2.5 ALGORITHM: VerifyProbeObservation

```
ALGORITHM: VerifyProbeObservation

INPUT:
  receipt: ProbeObservationReceipt
  originalPayload: Any (optional, for full re-verification)

OUTPUT:
  {
    valid: Boolean,
    checks: {
      schemaValid: Boolean,
      hashIntegrityValid: Boolean,
      determinismValid: Boolean,
      chainLinkValid: Boolean (if not genesis)
    },
    errors: Array<String>
  }

PRECONDITIONS:
  - receipt has required fields
  - If verifying chain link, need previousReceipt

BEGIN
  errors ← []
  checks ← {}

  // 1. Validate schema
  TRY
    ValidateAgainstSchema(receipt, ProbeObservationReceiptSchema)
    checks.schemaValid ← true
  CATCH error
    checks.schemaValid ← false
    errors.append("Schema validation failed: " + error.message)
  END TRY

  IF NOT checks.schemaValid THEN
    RETURN {
      valid: false,
      checks: checks,
      errors: errors
    }
  END IF

  // 2. Verify observation hash integrity
  TRY
    serialized ← DeterministicSerialize(receipt.observation.payload)
    recomputedHash ← BLAKE3(serialized)

    checks.hashIntegrityValid ← (recomputedHash = receipt.obsHash)
    IF NOT checks.hashIntegrityValid THEN
      errors.append(format(
        "Observation hash mismatch: expected {} got {}",
        receipt.obsHash, recomputedHash
      ))
    END IF
  CATCH error
    checks.hashIntegrityValid ← false
    errors.append("Hash integrity check failed: " + error.message)
  END TRY

  // 3. Verify determinism (all checks passed during creation)
  allChecksPassed ← ALL(receipt.checks.map(c → c.passed))
  checks.determinismValid ← allChecksPassed
  IF NOT allChecksPassed THEN
    failedChecks ← receipt.checks.filter(c → NOT c.passed)
    errors.append(format(
      "Determinism checks failed: {}",
      failedChecks.map(c → c.checkType).join(", ")
    ))
  END IF

  // 4. Verify receipt hash
  TRY
    payloadToHash ← receipt WITHOUT {receiptHash, payloadHash, previousHash}
    computedPayloadHash ← BLAKE3(DeterministicSerialize(payloadToHash))

    IF computedPayloadHash ≠ receipt.payloadHash THEN
      errors.append("Payload hash mismatch")
    END IF

    // Verify chain hash
    chainInput ← (receipt.previousHash OR "GENESIS") + ":" + receipt.payloadHash
    computedReceiptHash ← BLAKE3(chainInput)

    checks.chainLinkValid ← (computedReceiptHash = receipt.receiptHash)
    IF NOT checks.chainLinkValid THEN
      errors.append(format(
        "Receipt hash mismatch: expected {} got {}",
        receipt.receiptHash, computedReceiptHash
      ))
    END IF
  CATCH error
    checks.chainLinkValid ← false
    errors.append("Chain verification failed: " + error.message)
  END TRY

  // 5. Return result
  valid ← (errors.length = 0)

  RETURN {
    valid: valid,
    checks: checks,
    errors: errors
  }
END

COMPLEXITY:
  Time: O(|payload|) for serialization + hashing
  Space: O(|payload|) for serialized form
```

### 2.6 ALGORITHM: VerifyMerkleRoot

```
ALGORITHM: VerifyMerkleRoot

INPUT:
  merkleRoot: String                         // Root to verify
  shards: Array<{agentId, chainFinalHash}>  // Shard hashes
  proofPath: Array<{level, position, hash}> // Merkle proof

OUTPUT:
  {
    valid: Boolean,
    computedRoot: String,
    proofSteps: Array
  }

PRECONDITIONS:
  - merkleRoot is 64-char hex
  - All shards have valid chainFinalHash
  - proofPath matches tree structure

BEGIN
  // 1. Sort shards consistently
  sortedShards ← SORT(shards BY agentId)
  leaves ← sortedShards.map(s → s.chainFinalHash)

  // 2. Rebuild tree following proof path
  currentLevel ← leaves
  level ← 0
  proofSteps ← []

  WHILE currentLevel.length > 1 DO
    nextLevel ← []

    FOR i ← 0 TO currentLevel.length - 1 STEP 2 DO
      left ← currentLevel[i]
      right ← currentLevel[i+1] OR left

      pairInput ← left + right
      parentHash ← BLAKE3(pairInput)
      nextLevel.append(parentHash)

      proofSteps.append({
        level: level,
        left: left,
        right: right,
        parent: parentHash,
        computed: true
      })
    END FOR

    currentLevel ← nextLevel
    level ← level + 1
  END WHILE

  computedRoot ← currentLevel[0]

  // 3. Compare with expected root
  valid ← (computedRoot = merkleRoot)

  RETURN {
    valid: valid,
    computedRoot: computedRoot,
    proofSteps: proofSteps
  }
END

COMPLEXITY:
  Time: O(n log n) for tree reconstruction
  Space: O(n) for proof structure
```

### 2.7 ALGORITHM: VerifyProbeMerge

```
ALGORITHM: VerifyProbeMerge

INPUT:
  receipt: ProbeMergeReceipt
  agentChains: Map<agentId, Array<ProbeObservationReceipt>>

OUTPUT:
  {
    valid: Boolean,
    deterministic: Boolean,
    conflictFree: Boolean,
    checks: {
      schemaValid: Boolean,
      merkleRootValid: Boolean,
      shardHashesValid: Boolean,
      conflictDetectionValid: Boolean
    },
    errors: Array<String>,
    certificateChain: Array
  }

PRECONDITIONS:
  - receipt is ProbeMergeReceipt
  - agentChains has all agents referenced in receipt.shards

BEGIN
  errors ← []
  checks ← {}
  certificateChain ← []

  // 1. Validate schema
  TRY
    ValidateAgainstSchema(receipt, ProbeMergeReceiptSchema)
    checks.schemaValid ← true
  CATCH error
    checks.schemaValid ← false
    errors.append("Schema validation failed")
    RETURN {valid: false, checks, errors, ...}
  END TRY

  // 2. Verify shard hash integrity
  checks.shardHashesValid ← true
  FOR shard IN receipt.shards DO
    chain ← agentChains[shard.agentId]
    IF chain = null THEN
      errors.append(format("Missing chain for agent {}", shard.agentId))
      checks.shardHashesValid ← false
      CONTINUE
    END IF

    // Verify chain final hash
    lastReceipt ← chain[chain.length - 1]
    IF lastReceipt.receiptHash ≠ shard.chainFinalHash THEN
      errors.append(format(
        "Chain final hash mismatch for agent {}: {} vs {}",
        shard.agentId, shard.chainFinalHash, lastReceipt.receiptHash
      ))
      checks.shardHashesValid ← false
    END IF

    // Verify observation count
    IF chain.length ≠ shard.obsCount THEN
      errors.append(format(
        "Observation count mismatch for agent {}: {} vs {}",
        shard.agentId, shard.obsCount, chain.length
      ))
      checks.shardHashesValid ← false
    END IF
  END FOR

  // 3. Verify merkle root
  merkleVerify ← VerifyMerkleRoot(
    receipt.merkleRoot,
    receipt.shards,
    receipt.proofPath
  )

  checks.merkleRootValid ← merkleVerify.valid
  IF NOT merkleVerify.valid THEN
    errors.append(format(
      "Merkle root mismatch: expected {} got {}",
      receipt.merkleRoot, merkleVerify.computedRoot
    ))
  END IF

  // 4. Verify conflict detection
  checks.conflictDetectionValid ← true
  conflictFree ← (receipt.conflicts = null OR receipt.conflicts.length = 0)

  IF conflictFree AND errors.length > 0 THEN
    // Claims conflict-free but has errors
    checks.conflictDetectionValid ← false
    errors.append("Conflict detection invalid: claims conflict-free but has errors")
  END IF

  // 5. Build certificate chain
  FOR shard IN receipt.shards DO
    chain ← agentChains[shard.agentId]
    FOR obs IN chain DO
      certificateChain.append({
        receiptHash: obs.receiptHash,
        receiptType: "probe-observation",
        relationship: format(
          "obs-{}-in-chain-{}", obs.observationIndex, shard.agentId
        )
      })
    END FOR
  END FOR

  certificateChain.append({
    receiptHash: receipt.receiptHash,
    receiptType: "probe-merge",
    relationship: "merge-verified"
  })

  // 6. Determine overall validity
  valid ← (errors.length = 0 AND checks.schemaValid AND
           checks.merkleRootValid AND checks.shardHashesValid)
  deterministic ← (checks.shardHashesValid AND checks.merkleRootValid)

  RETURN {
    valid: valid,
    deterministic: deterministic,
    conflictFree: conflictFree,
    checks: checks,
    errors: errors,
    certificateChain: certificateChain
  }
END

COMPLEXITY:
  Time: O(m * n) where m = number of agents, n = avg observations per agent
        Linear scan of all agent chains
  Space: O(m * n) for certificate chain + verification state
```

---

## Part 3: Verification Flow Examples

### Example 3.1: Single Observation Verification

```
SCENARIO: Verify ProbeObservationReceipt was created deterministically

INPUTS:
  - receipt: ProbeObservationReceipt for observation X
  - payload: Original observation payload (optional)

EXECUTION FLOW:

Step 1: Validate Structure
  ├─ Schema validation: PASS
  ├─ Field type checks: PASS
  └─ Required fields present: PASS

Step 2: Verify Observation Hash
  ├─ Serialize payload deterministically
  │  └─ Payload: {"timestamp": 1234567890, "value": "data"}
  │  └─ Serialized: '{"timestamp":"1234567890","value":"data"}'
  ├─ Compute BLAKE3: a1b2c3d4e5f6...
  ├─ Compare with receipt.obsHash: a1b2c3d4e5f6...
  └─ Result: MATCH ✓

Step 3: Verify Determinism Checks
  ├─ Check 1: hash-recompute
  │  ├─ Recompute BLAKE3(payload): a1b2c3d4e5f6...
  │  └─ Expected: a1b2c3d4e5f6... → PASS
  │
  └─ Check 2: serialization-stable
     ├─ Serialize 3x independently
     │  ├─ Hash 1: a1b2c3d4e5f6...
     │  ├─ Hash 2: a1b2c3d4e5f6...
     │  ├─ Hash 3: a1b2c3d4e5f6...
     └─ All match: PASS ✓

Step 4: Verify Receipt Hash Chain
  ├─ Compute payload hash:
  │  └─ payloadHash = BLAKE3(receiptData) = b2c3d4e5f6a7...
  │
  ├─ Compute receipt hash:
  │  ├─ If genesis: input = "GENESIS:b2c3d4e5f6a7..."
  │  ├─ Else: input = "previousHash:b2c3d4e5f6a7..."
  │  └─ receiptHash = BLAKE3(input) = c3d4e5f6a7b8...
  │
  └─ Compare: c3d4e5f6a7b8... = receipt.receiptHash → PASS ✓

RESULT: VALID & DETERMINISTIC

CONFIDENCE: 99.9%
  - Observation hash verified ✓
  - Determinism proven (3x stable serialization) ✓
  - Receipt structure verified ✓
  - No tampering detected ✓
```

### Example 3.2: Hash Chain Verification

```
SCENARIO: Verify ProbeObservationReceipt chain for agent-1

INPUTS:
  - chain: [obs1, obs2, obs3] from agent-1
  - Root verification: obs3.receiptHash should be accessible

CHAIN STATE:
  obs1 (Genesis):
    ├─ agentId: "agent-1"
    ├─ observationIndex: 1
    ├─ obsHash: a1b2...
    ├─ prevHash: null
    ├─ previousHash: null
    └─ receiptHash: f1e2d3... = BLAKE3("GENESIS:a1b2...")

  obs2:
    ├─ agentId: "agent-1"
    ├─ observationIndex: 2
    ├─ obsHash: b2c3...
    ├─ prevHash: a1b2... (links to obs1.obsHash ✓)
    ├─ previousHash: f1e2d3... (links to obs1.receiptHash ✓)
    └─ receiptHash: e1d2c3... = BLAKE3("f1e2d3:b2c3...")

  obs3:
    ├─ agentId: "agent-1"
    ├─ observationIndex: 3
    ├─ obsHash: c3d4...
    ├─ prevHash: b2c3... (links to obs2.obsHash ✓)
    ├─ previousHash: e1d2c3... (links to obs2.receiptHash ✓)
    └─ receiptHash: d1c2b3... = BLAKE3("e1d2c3:c3d4...")

VERIFICATION:

Step 1: Genesis Check
  ├─ obs1.prevHash = null? YES ✓
  ├─ obs1.previousHash = null? YES ✓
  └─ obs1.receiptHash computed correctly? YES ✓

Step 2: Chain Link Integrity
  ├─ obs2.prevHash = obs1.obsHash? a1b2... = a1b2... ✓
  ├─ obs2.previousHash = obs1.receiptHash? f1e2d3... = f1e2d3... ✓
  ├─ obs3.prevHash = obs2.obsHash? b2c3... = b2c3... ✓
  └─ obs3.previousHash = obs2.receiptHash? e1d2c3... = e1d2c3... ✓

Step 3: All Observations Deterministic
  ├─ Verify obs1 observation hash: ✓
  ├─ Verify obs2 observation hash: ✓
  └─ Verify obs3 observation hash: ✓

Step 4: Temporal Ordering
  ├─ obs1.t_ns < obs2.t_ns? ✓
  └─ obs2.t_ns < obs3.t_ns? ✓

RESULT: CHAIN VALID
  - Chain final hash: d1c2b3... (obs3.receiptHash)
  - All observations deterministic: YES
  - No tampering detected: YES
  - Immutable: YES (cannot modify any obs without breaking chain)

CONFIDENCE: 99.99%
```

### Example 3.3: Merkle Merge Verification

```
SCENARIO: Verify ProbeMergeReceipt with 3 agent shards

INPUTS:
  - receipt: ProbeMergeReceipt
  - agentChains: {
      "agent-1": [obs1, obs2, obs3],
      "agent-2": [obs1, obs2],
      "agent-3": [obs1]
    }

SHARD FINAL HASHES:
  agent-1: d1c2b3... (from agent chain verification ✓)
  agent-2: e2d3c4... (from agent chain verification ✓)
  agent-3: f3e4d5... (from agent chain verification ✓)

MERKLE TREE CONSTRUCTION:

Step 1: Sort leaves by agentId (deterministic order)
  ├─ agent-1: d1c2b3...
  ├─ agent-2: e2d3c4...
  └─ agent-3: f3e4d5...

Step 2: Build merkle tree level 0 (leaves)
  Level 0:
    ├─ L0: d1c2b3... (agent-1)
    ├─ L1: e2d3c4... (agent-2)
    └─ L2: f3e4d5... (agent-3)

Step 3: Build level 1
  Pair (d1c2b3..., e2d3c4...):
    ├─ Input: d1c2b3...e2d3c4...
    └─ Parent: BLAKE3(input) = a1b2... (call it P1)

  Leaf f3e4d5... unpaired:
    └─ Copy up: P2 = f3e4d5...

  Level 1: [a1b2..., f3e4d5...]

Step 4: Build level 2 (root)
  Pair (a1b2..., f3e4d5...):
    ├─ Input: a1b2...f3e4d5...
    └─ Root: BLAKE3(input) = b2c3d4e5f6a7...

MERKLE ROOT: b2c3d4e5f6a7...

VERIFICATION:

Step 1: Validate Receipt Schema
  └─ All fields present and typed correctly ✓

Step 2: Verify Shard Hashes
  ├─ agent-1 chain final: d1c2b3... = receipt.shard[0].chainFinalHash ✓
  ├─ agent-2 chain final: e2d3c4... = receipt.shard[1].chainFinalHash ✓
  ├─ agent-3 chain final: f3e4d5... = receipt.shard[2].chainFinalHash ✓
  ├─ agent-1 obs count: 3 = receipt.shard[0].obsCount ✓
  ├─ agent-2 obs count: 2 = receipt.shard[1].obsCount ✓
  └─ agent-3 obs count: 1 = receipt.shard[2].obsCount ✓

Step 3: Verify Merkle Root
  ├─ Recompute from shards: b2c3d4e5f6a7...
  └─ Matches receipt.merkleRoot: b2c3d4e5f6a7... ✓

Step 4: Verify Proof Path
  ├─ All proof steps recomputable: ✓
  └─ Leads to correct root: ✓

Step 5: Check Conflict Status
  ├─ receipt.conflicts: null
  ├─ Errors detected: 0
  └─ Consistent: CONFLICT-FREE ✓

RESULT: MERGE VALID & CONFLICT-FREE

MERKLE INTEGRITY:
  - Cannot tamper with any shard hash without invalidating root
  - Cannot reorder shards (sorted deterministically)
  - Cannot add/remove shards without invalidating root

CONFIDENCE: 99.99%
  - All agent chains verified ✓
  - Merkle tree recomputed identically ✓
  - Root matches ✓
  - Conflict-free asserted ✓
```

---

## Part 4: v6-core Integration

### 4.1 Extending Receipt Factory

```
ALGORITHM: createReceipt (EXTENDED for probes)

INPUT:
  type: 'execution' | 'allocation' | 'compile' | 'verification' |
        'probe-observation' | 'probe-merge'
  event: Event object (type-specific)
  previousReceipt: BaseReceipt | null

OUTPUT:
  receipt: BaseReceipt (subtype)

INTEGRATION POINTS:
  1. Use existing createReceipt for base fields
  2. Add probe-specific factory branches
  3. Reuse computeBlake3, computeChainHash from base-receipt.mjs
  4. Reuse UUID generation, timestamp handling
  5. Extend RECEIPT_TYPES enum

IMPLEMENTATION PSEUDOCODE:

BEGIN
  // Extend v6-core Receipt factory

  IF type = 'probe-observation' THEN
    RETURN CreateProbeObservationReceiptViaFactory(event, previousReceipt)

  ELSE IF type = 'probe-merge' THEN
    RETURN CreateProbeMergeReceiptViaFactory(event)

  ELSE
    // Existing v6-core types
    RETURN ExistingCreateReceipt(type, event, previousReceipt)

  END IF
END

SUBROUTINE: CreateProbeObservationReceiptViaFactory

  // Reuse from base-receipt.mjs:
  // - generateUUID()
  // - deterministicSerialize()
  // - computeBlake3()
  // - computeChainHash()

  BEGIN
    id ← generateUUID()
    t_ns ← now()  // from @unrdf/kgc-4d
    timestamp_iso ← toISO(t_ns)

    // Compute hashes using existing functions
    obsPayloadSerialized ← deterministicSerialize(event.observation.payload)
    obsHash ← await computeBlake3(obsPayloadSerialized)

    prevHash ← event.previousReceipt?.obsHash OR null

    // Build receipt
    receiptData ← {
      id, receiptType: "probe-observation", t_ns, timestamp_iso,
      ...event,
      obsHash, prevHash
    }

    // Compute hashes
    payloadToHash ← receiptData WITHOUT {receiptHash, payloadHash}
    payloadHash ← await computeBlake3(payloadToHash)

    previousHashForChain ← event.previousReceipt?.receiptHash OR null
    receiptHash ← await computeChainHash(previousHashForChain, payloadHash)

    receipt ← {
      ...receiptData,
      previousHash: previousHashForChain,
      payloadHash,
      receiptHash
    }

    // Validate
    ProbeObservationReceiptSchema.parse(receipt)

    RETURN receipt
  END

REUSE OPPORTUNITIES:
  ✓ computeBlake3() - hashing
  ✓ computeChainHash() - receipt chain
  ✓ generateUUID() - IDs
  ✓ deterministicSerialize() - determinism
  ✓ verifyBaseReceipt() - base validation
  ✓ verifyChainLink() - chain integrity
```

### 4.2 Extending Receipt Verifier

```
ALGORITHM: verifyReceipt (EXTENDED for probes)

INPUT:
  receipt: BaseReceipt (any subtype)

OUTPUT:
  {
    valid: Boolean,
    error?: String,
    checks?: Object
  }

INTEGRATION:

BEGIN
  // First verify base receipt structure (existing)
  baseVerification ← await verifyBaseReceipt(receipt)
  IF NOT baseVerification.valid THEN
    RETURN baseVerification
  END IF

  // Then dispatch to type-specific verification
  IF receipt.receiptType = 'probe-observation' THEN
    RETURN await VerifyProbeObservation(receipt)

  ELSE IF receipt.receiptType = 'probe-merge' THEN
    RETURN await VerifyProbeMerge(receipt)  // Requires agentChains context

  ELSE
    // Existing v6-core types
    RETURN ExistingVerifyReceipt(receipt)

  END IF
END
```

### 4.3 New Merkle Tree Implementation

```
CLASS: ProbeShardMerkleTree EXTENDS MerkleTree

PURPOSE:
  Deterministic merkle tree for probe shard hashes

EXTENDS v6-core/receipts/merkle/index.mjs placeholder

METHODS:

  constructor(shards):
    // shards: Array<{agentId, chainFinalHash, obsCount, domain}>
    this.shards = SORT(shards BY agentId)
    this.leaves = this.shards.map(s → s.chainFinalHash)
    this.root = this.computeRoot()

  computeRoot():
    // Build tree from sorted leaves
    // Return root hash

  getProof(shardIndex):
    // Generate merkle proof for specific shard
    // Return proof path

  static verify(leaves, proofPath, root):
    // Verify proof against leaves and expected root
    // Return boolean

  static verifyFromShards(shards, proofPath, root):
    // Convenience: sort shards, extract leaves, verify
```

### 4.4 Schema Integration

```
PSEUDOCODE: Update Receipt Types Enum

// In base-receipt.mjs:
export const RECEIPT_TYPES = Object.freeze({
  EXECUTION: 'execution',
  ALLOCATION: 'allocation',
  COMPILE: 'compile',
  VERIFICATION: 'verification',
  PROBE_OBSERVATION: 'probe-observation',  // NEW
  PROBE_MERGE: 'probe-merge',              // NEW
});

// Update ReceiptTypeSchema
export const ReceiptTypeSchema = z.enum([
  'execution',
  'allocation',
  'compile',
  'verification',
  'probe-observation',  // NEW
  'probe-merge',        // NEW
]);

// In index.mjs:
export {
  ProbeObservationReceiptSchema,  // NEW
  ProbeMergeReceiptSchema,        // NEW
  ProbeVerificationReceiptSchema, // NEW
};

// Update createReceipt factory
export async function createReceipt(type, event, previousReceipt = null) {
  // Existing validation + NEW branches

  switch(type) {
    case RECEIPT_TYPES.PROBE_OBSERVATION:
      return CreateProbeObservationReceipt(...);
    case RECEIPT_TYPES.PROBE_MERGE:
      return CreateProbeMergeReceipt(...);
    // ... existing cases
  }
}

// Update verifyReceipt verifier
export async function verifyReceipt(receipt) {
  // Existing base verification + NEW branches

  switch(receipt.receiptType) {
    case RECEIPT_TYPES.PROBE_OBSERVATION:
      return VerifyProbeObservation(...);
    case RECEIPT_TYPES.PROBE_MERGE:
      return VerifyProbeMerge(...);
    // ... existing cases
  }
}
```

---

## Part 5: Complexity Analysis & Performance

### 5.1 Time Complexity Summary

```
OPERATION                              TIME COMPLEXITY
───────────────────────────────────────────────────────

CreateProbeObservationReceipt           O(|payload|)
  - Serialization: O(|payload|)
  - BLAKE3 hashing: O(|payload|)
  - Hash verification: O(1)

BuildAgentHashChain(n obs)              O(n * |payload_avg|)
  - Create n receipts: O(n) * O(|payload|)
  - Chain verification: O(n)

ComputeMerkleRoot(m shards)             O(m log m)
  - Sort shards: O(m log m)
  - Build tree: O(m)
  - Total: O(m log m)

VerifyProbeObservation                  O(|payload|)
  - Hash recompute: O(|payload|)
  - Determinism checks: O(|payload|) * constant

VerifyAgentChain(n obs)                 O(n * |payload_avg|)
  - Verify each obs: O(n) * O(|payload|)
  - Chain integrity: O(n)

VerifyMerkleRoot(m shards)              O(m log m)
  - Recompute tree: O(m log m)

VerifyProbeMerge                        O(m*n)
  - Verify all agents: O(m agents)
  - Verify all observations: O(n observations)
  - Merkle verification: O(m log m)
  - Certificate chain: O(m*n)
```

### 5.2 Space Complexity Summary

```
DATA STRUCTURE                          SPACE COMPLEXITY
───────────────────────────────────────────────────────

ProbeObservationReceipt                 O(|payload|)
  - Payload stored: O(|payload|)
  - Receipt fields: O(1)

AgentHashChain(n obs)                   O(n * |receipt|)
  - n receipts * receipt size

MerkleTree(m shards)                    O(m log m)
  - Tree nodes at each level
  - Proof path: O(log m)

ProbeMergeReceipt                       O(m + log m)
  - Shard list: O(m)
  - Merkle proof path: O(log m)
  - Total: O(m)

Verification Results                    O(m*n)
  - Certificate chain: O(m*n)
  - Error/check arrays: O(1)
```

### 5.3 Practical Numbers

```
SCENARIO: Probe system with 10 agents, 100 obs per agent

CREATION:
  - Create 10 * 100 = 1000 receipts:
    ├─ Time: ~100-500ms (depends on payload size)
    └─ Space: ~10-50MB (1000 receipts * 10-50KB each)

  - Merge 10 agent chains:
    ├─ Time: ~10ms (merkle tree)
    └─ Space: ~100KB (merkle structure)

VERIFICATION:
  - Verify 1000 obs + 1 merge:
    ├─ Time: ~100-500ms (linear scan of observations)
    └─ Space: ~50MB (certificate chain + results)

PARALLELIZATION:
  - Verify 10 agent chains in parallel:
    ├─ Sequential: ~500ms
    └─ Parallel (10 cores): ~50ms speedup possible
```

---

## Part 6: Design Patterns

### 6.1 Hash Chain Pattern

```
PATTERN: Immutable Hash Chain

PURPOSE: Prove sequence of observations is immutable

STRUCTURE:
  obs[1] → obs[2] → obs[3] → ... → obs[n]

Each obs links to previous via obsHash:
  obs[i].prevHash = obs[i-1].obsHash

Receipt chains separately:
  obs[i].receiptHash = BLAKE3(obs[i-1].receiptHash || obs[i].payloadHash)

PROPERTIES:
  ✓ Tamper-evident: modify any obs breaks all downstream
  ✓ Auditable: can replay from receipts alone
  ✓ Immutable: cannot reorder without breaking hashes
  ✓ Per-agent: each agent has own chain, independent

VERIFICATION:
  1. Compute all obs hashes from payloads
  2. Verify chain links (prevHash chain)
  3. Verify receipt chain (receiptHash chain)
  4. Check temporal ordering

USAGE:
  - Agent[1] creates chain of observations
  - Agent[2] creates separate chain
  - Orchestrator merges via merkle tree
  - All original chains remain intact and auditable
```

### 6.2 Merkle Proof Pattern

```
PATTERN: Merkle Tree Proof

PURPOSE: Prove set of agent chains merged deterministically

STRUCTURE (3 shards):
              Root
             /    \
           P1      L3
          /  \
        L1   L2

LEAVES:
  L1 = agent-1.chainFinalHash
  L2 = agent-2.chainFinalHash
  L3 = agent-3.chainFinalHash

SORTED DETERMINISTICALLY:
  Sort by agentId to ensure consistent ordering

PROOF FOR L1:
  - Include L2 (sibling at level 0)
  - Include P1 parent hash (but that requires L3 sibling...)
  - Include [L2, (L3 or L3 if unpaired), ...]

VERIFICATION:
  1. Sort leaves by agentId
  2. Rebuild tree from leaves
  3. Compare root with expected

PROPERTIES:
  ✓ Deterministic: sort ensures same order always
  ✓ Compact: O(log m) proof for m shards
  ✓ Verifiable: recompute path, check root
  ✓ Tamper-evident: modify any leaf invalidates root

USAGE:
  - Orchestrator computes merkle root over agent hashes
  - Includes proof path in merge receipt
  - Verifier rebuilds tree, checks root matches
```

### 6.3 Determinism Verification Pattern

```
PATTERN: Triple-Check Determinism

PURPOSE: Prove observation was deterministically serializable

CHECKS:
  1. Hash Recompute
     - Deserialize observation
     - Reserialize deterministically
     - Recompute BLAKE3
     - Compare: must match original hash

  2. Serialization Stability
     - Serialize observation 3x independently
     - Compute BLAKE3 for each
     - All 3 hashes must match
     - Ensures serialization is stable

  3. Payload Integrity
     - Parse observation structure
     - Validate all required fields present
     - Validate field types

PROPERTIES:
  ✓ Exhaustive: multiple independent checks
  ✓ Evidence-based: not just assertions
  ✓ Reproducible: anyone can re-run checks

CONFIDENCE LEVELS:
  - 1 check passes: 90% confident (could be lucky)
  - 2 checks pass: 99% confident (very likely deterministic)
  - 3 checks pass: 99.9% confident (cryptographically secure)
```

---

## Summary: SPARC Receipt Architecture

### Receipt Type Hierarchy

```
BaseReceipt (v6-core)
  ├── ExecutionReceipt (existing)
  ├── AllocationReceipt (existing)
  ├── CompileReceipt (existing)
  ├── VerificationReceipt (existing)
  │
  └── PROBE RECEIPTS (new)
      ├── ProbeObservationReceipt
      │   └── Properties: agentId, observationIndex, obsHash, prevHash
      │   └── Purpose: Prove observation created deterministically
      │
      ├── ProbeMergeReceipt
      │   └── Properties: shards, merkleRoot, mergeAlgorithm
      │   └── Purpose: Prove merge is deterministic + conflict-free
      │
      └── ProbeVerificationReceipt
          └── Properties: verifications[], deterministic, conflictFree
          └── Purpose: Comprehensive proof of integrity
```

### Key Design Decisions

```
DECISION                                RATIONALE
─────────────────────────────────────────────────────────

1. Per-Agent Hash Chains               - Each agent's observations are immutable
                                        - Cannot tamper without breaking chain
                                        - Enables parallel verification
                                        - Maintains agent independence

2. Separate obsHash vs receiptHash     - obsHash: immutable observation proof
                                        - receiptHash: v6-core chain compatibility
                                        - Enables dual verification

3. Merkle Tree for Merge               - O(log m) proof for m agents
                                        - Deterministic leaf ordering (by agentId)
                                        - Tamper-evident root
                                        - Standard proof structure

4. Conflict-Free vs Conflict List      - null: merge is clean + deterministic
                                        - Array: merge found conflicts (not deterministic)
                                        - Explicit failure mode

5. Certificate Chain in Verification   - Proof path from leaf obs to merge root
                                        - Complete audit trail
                                        - Can verify subset of chain
                                        - Enables selective disclosure

6. Reuse v6-core Foundation            - Extend, don't duplicate
                                        - BLAKE3, UUID, chaining
                                        - Consistent with existing system
                                        - Zero additional crypto algorithms
```

---

## Files to Implement

Based on this pseudocode design, create:

```
📁 /home/user/unrdf/packages/v6-core/src/receipts/

  probe/
  ├── observation-receipt.mjs          (ProbeObservationReceipt schema + factory)
  ├── merge-receipt.mjs                (ProbeMergeReceipt schema + factory)
  ├── verification-receipt.mjs         (ProbeVerificationReceipt schema)
  ├── index.mjs                        (Exports + integration hooks)
  │
  └── verifiers/
      ├── observation-verifier.mjs     (VerifyProbeObservation algorithm)
      ├── chain-verifier.mjs           (VerifyAgentChain algorithm)
      ├── merge-verifier.mjs           (VerifyProbeMerge algorithm)
      └── index.mjs                    (Exports)

Updated:
  ├── base-receipt.mjs                 (Add probe types to RECEIPT_TYPES)
  ├── index.mjs                        (Export probe receipts, update factories)
  │
  merkle/
  └── probe-merkle-tree.mjs            (ProbeShardMerkleTree implementation)

Tests:
  ├── probe-observation.test.mjs
  ├── probe-chain.test.mjs
  ├── probe-merge.test.mjs
  └── probe-verification.test.mjs
```

---

## References

- **v6-core BaseReceipt**: Extends with probe-specific fields
- **Hash Chain Pattern**: Per-agent immutable observation chains
- **Merkle Tree**: Deterministic shard merge proof
- **Verification Flow**: Triple-check determinism + merkle validation
- **v6-core Integration**: Reuse factory, verifier, hash utilities
