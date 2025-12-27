# Orchestrator Merge Algorithm Π: Shard Monoid Algebra

**SPARC Phase**: Pseudocode Design
**Complexity**: O(n log n) where n = total claims across 10 shards
**Properties**: Commutative, Associative, Idempotent (LWW semantics)
**Conflict Model**: Last-Write-Wins with Conflict Register

---

## 1. MERGE MONOID DEFINITION

### 1.1 Monoid Algebra

The merge function Π implements a commutative monoid over agent outputs:

```
Structure: (A, ⊕, identity)

Where:
  A = Set of agent shard outputs {A_1, A_2, ..., A_10}
  ⊕ = Merge operator
  identity = empty world model {}

Properties:
  Closure:         A ⊕ B ∈ A (merge of 2 shards is valid output)
  Associativity:   (A ⊕ B) ⊕ C = A ⊕ (B ⊕ C)
  Commutativity:   A ⊕ B = B ⊕ A
  Idempotence:     A ⊕ A = A (absorbed duplicate)
  Identity:        A ⊕ ∅ = A

Conflict Rule (LWW):
  For claim C with key K and timestamps T_A, T_B:
    IF T_A > T_B: keep C_A
    IF T_A < T_B: keep C_B
    IF T_A = T_B AND C_A ≠ C_B: CONFLICT (log both)
```

### 1.2 Shard Input Format

Each shard A_i provides:

```
AgentShard {
  agent_id: string              // Unique agent identifier
  domain: string                // e.g., "rdf:store", "code:analysis"
  timestamp_ns: int64           // Nanosecond wall-clock time
  claims: Claim[]               // Observations with proofs
  observations: Observation[]   // Raw data points
  metadata: {
    cpu_usage_ns: int64
    memory_bytes: int64
    execution_time_ns: int64
    method: string              // "exhaustive", "heuristic", "sample"
  }
}

Claim {
  key: string                   // Unique identifier (domain:resource:method)
  value: any                    // Claim content (RDF triple, metric, etc.)
  timestamp_ns: int64           // When claim was made
  proof: Proof                  // Evidence backing claim
  confidence: float             // 0.0 to 1.0
  agent_id: string              // Source agent
}

Observation {
  observation_id: string        // UUID
  timestamp_ns: int64
  method: string                // Measurement method
  result: any                   // Raw result
  error: string OR null         // If failed
}

Proof {
  type: "direct" | "inferred" | "aggregated"
  evidence: Evidence[]
  reasoning: string
}
```

---

## 2. MERGE ALGORITHM Π

### 2.1 Main Orchestrator Function

```
ALGORITHM: MergeShards
INPUT: shards[1..10] (array of AgentShard)
       config (MergeConfig)
OUTPUT: merged (MergedWorldModel)

CONSTANTS:
  CONFLICT_TTL_NS = 86400e9  // 24 hours in nanoseconds
  MAX_CONFLICTS = 10000      // Track up to 10k conflicts

BEGIN
  // Phase 1: Flatten and validate
  allClaims ← []
  shardMap ← {}

  FOR i ← 1 TO 10 DO
    shard ← shards[i]

    IF shard is null OR shard.claims is empty THEN
      continue  // Skip empty shards
    END IF

    FOR EACH claim IN shard.claims DO
      // Validate claim structure
      IF NOT ValidateClaim(claim) THEN
        LogError("Invalid claim format", {shard_id: shard.agent_id, claim: claim})
        continue
      END IF

      allClaims.append({
        claim: claim,
        shard_id: shard.agent_id,
        shard_timestamp: shard.timestamp_ns
      })
    END FOR

    shardMap[shard.agent_id] ← shard
  END FOR

  IF allClaims is empty THEN
    RETURN {claims: {}, conflicts: [], receipt: Hash({})}
  END IF

  // Phase 2: Sort all claims by deterministic key
  sortKey ← FUNCTION(claimWrapper):
    claim ← claimWrapper.claim
    RETURN (claim.domain, claim.method, claim.key, claim.timestamp_ns)
  END FUNCTION

  StableSortBy(allClaims, sortKey)

  // Phase 3: Apply LWW merge
  mergedClaims ← {}    // key → {claim, timestamp, agent_id}
  conflictRegister ← []

  FOR EACH claimWrapper IN allClaims DO
    claim ← claimWrapper.claim
    key ← claim.key

    IF key NOT IN mergedClaims THEN
      // First occurrence of this key
      mergedClaims[key] ← {
        claim: claim,
        timestamp_ns: claim.timestamp_ns,
        agent_id: claimWrapper.shard_id,
        version: 1
      }

    ELSE
      // Key already seen
      existing ← mergedClaims[key]

      IF claim.timestamp_ns > existing.timestamp_ns THEN
        // Newer claim - LWW wins
        oldClaim ← existing.claim

        IF claim.value ≠ oldClaim.value THEN
          // Value changed - record conflict
          conflictRegister.append(RecordConflict(
            key,
            oldClaim,
            claim,
            existing.timestamp_ns,
            claim.timestamp_ns
          ))
        END IF

        mergedClaims[key] ← {
          claim: claim,
          timestamp_ns: claim.timestamp_ns,
          agent_id: claimWrapper.shard_id,
          version: existing.version + 1
        }

      ELSE IF claim.timestamp_ns = existing.timestamp_ns THEN
        // Same timestamp - detect conflict without replacing
        IF claim.value ≠ existing.claim.value THEN
          conflictRegister.append(RecordConflict(
            key,
            existing.claim,
            claim,
            existing.timestamp_ns,
            claim.timestamp_ns
          ))
        END IF
        // Keep existing (first-write-wins on ties after LWW)

      ELSE
        // Older claim - skip
        continue
      END IF
    END IF
  END FOR

  // Phase 4: Build merged RDF universe
  probeUniverse ← BuildProbeUniverse(mergedClaims, shardMap)

  // Phase 5: Compute Merkle receipt
  receipt ← ComputeMerkleReceipt(
    shards,
    mergedClaims,
    conflictRegister,
    config.algorithm_version
  )

  // Phase 6: Prune old conflicts (cleanup)
  currentTime ← GetCurrentTimeNs()
  activeConflicts ← []

  FOR EACH conflict IN conflictRegister DO
    age ← currentTime - conflict.first_timestamp_ns
    IF age < CONFLICT_TTL_NS AND conflict.resolution = null THEN
      activeConflicts.append(conflict)
    ELSE
      LogArchived("Conflict aged out or resolved", {key: conflict.key})
    END IF
  END FOR

  // Phase 7: Return merged world model
  RETURN MergedWorldModel {
    claims: mergedClaims,
    universe: probeUniverse,
    conflicts: activeConflicts,
    receipt: receipt,
    stats: {
      total_shards: 10,
      total_claims: Length(allClaims),
      merged_unique_keys: Length(mergedClaims),
      conflicts_detected: Length(activeConflicts),
      timestamp_ns: currentTime
    }
  }
END
```

### 2.2 Validation Subroutine

```
SUBROUTINE: ValidateClaim
INPUT: claim (Claim)
OUTPUT: valid (boolean)

BEGIN
  // Check required fields
  IF claim.key is empty OR claim.key is null THEN
    RETURN false
  END IF

  IF claim.timestamp_ns is null OR claim.timestamp_ns < 0 THEN
    RETURN false
  END IF

  IF claim.agent_id is empty THEN
    RETURN false
  END IF

  // Check domain syntax
  IF NOT MatchesPattern(claim.domain, "^[a-z]+:[a-z_]+$") THEN
    RETURN false
  END IF

  // Check key uniqueness pattern
  parts ← Split(claim.key, ":")
  IF Length(parts) < 3 THEN
    RETURN false  // Must be domain:resource:method at minimum
  END IF

  RETURN true
END
```

---

## 3. CONFLICT DETECTION & RESOLUTION

### 3.1 Conflict Record Schema

```
ConflictRecord {
  conflict_id: UUID                    // Unique identifier
  resource_key: string                 // The contested resource

  claim_a: Claim                       // Earlier claim
  claim_b: Claim                       // Later claim

  timestamp_a_ns: int64                // When claim_a made
  timestamp_b_ns: int64                // When claim_b made

  agent_a: string                      // Source of claim_a
  agent_b: string                      // Source of claim_b

  value_a: any                         // Claim A's value
  value_b: any                         // Claim B's value

  conflict_type: enum                  // Type of conflict
    "value_divergence"                 // Different values
    "proof_mismatch"                   // Different evidence
    "confidence_drop"                  // Confidence decreased
    "method_change"                    // Different measurement method

  first_timestamp_ns: int64            // When conflict first detected
  resolution: Resolution OR null       // How conflict resolved

  metadata: {
    value_a_hash: string               // SHA-256 of value_a
    value_b_hash: string               // SHA-256 of value_b
    similarity: float                  // Levenshtein distance [0,1]
    is_transitive: boolean             // Can resolve via transitivity?
  }
}

Resolution {
  resolved_at_ns: int64
  strategy: enum
    "lww"                              // Last-write-wins
    "manual"                           // Human decision
    "heuristic"                        // Voting majority
    "merge"                            // Partial union

  chosen_value: any
  reason: string
  resolved_by: string                  // Agent or human ID
}
```

### 3.2 Conflict Recording

```
SUBROUTINE: RecordConflict
INPUT: key (string)
       claimOld (Claim)
       claimNew (Claim)
       oldTimestamp (int64)
       newTimestamp (int64)
OUTPUT: conflict (ConflictRecord)

BEGIN
  conflict ← ConflictRecord {
    conflict_id: GenerateUUID(),
    resource_key: key,

    claim_a: claimOld,
    claim_b: claimNew,

    timestamp_a_ns: oldTimestamp,
    timestamp_b_ns: newTimestamp,

    agent_a: claimOld.agent_id,
    agent_b: claimNew.agent_id,

    value_a: claimOld.value,
    value_b: claimNew.value,

    conflict_type: DetermineConflictType(claimOld, claimNew),
    first_timestamp_ns: GetCurrentTimeNs(),
    resolution: null
  }

  // Compute similarity metrics
  conflict.metadata.value_a_hash ← SHA256(Serialize(claimOld.value))
  conflict.metadata.value_b_hash ← SHA256(Serialize(claimNew.value))
  conflict.metadata.similarity ← LevenshteinSimilarity(
    Serialize(claimOld.value),
    Serialize(claimNew.value)
  )

  // Log conflict
  LogConflict("Value divergence detected", {
    key: key,
    agent_a: claimOld.agent_id,
    agent_b: claimNew.agent_id,
    timestamp_a_ns: oldTimestamp,
    timestamp_b_ns: newTimestamp,
    similarity: conflict.metadata.similarity
  })

  RETURN conflict
END
```

### 3.3 Conflict Type Detection

```
SUBROUTINE: DetermineConflictType
INPUT: claimOld (Claim), claimNew (Claim)
OUTPUT: conflictType (enum)

BEGIN
  // Check value equality
  IF claimOld.value ≠ claimNew.value THEN
    RETURN "value_divergence"
  END IF

  // Check proof consistency
  IF Hash(claimOld.proof) ≠ Hash(claimNew.proof) THEN
    RETURN "proof_mismatch"
  END IF

  // Check confidence trend
  IF claimNew.confidence < (claimOld.confidence * 0.9) THEN
    RETURN "confidence_drop"
  END IF

  // Check method consistency
  IF claimOld.proof.method ≠ claimNew.proof.method THEN
    RETURN "method_change"
  END IF

  // If we reach here, no conflict
  RETURN null
END
```

---

## 4. INCREMENTAL MERGE PROPERTY

### 4.1 Problem Statement

Given:
- Merged result A = Π(A_1, A_2, ..., A_10)
- New shard A_11

Can we compute A' = Π(A_1, ..., A_11) = Π(A, A_11) incrementally?

**Answer**: YES, with O(m log m) where m = |A_11.claims| instead of O(n log n).

### 4.2 Incremental Merge Algorithm

```
ALGORITHM: IncrementalMerge
INPUT: mergedModel (MergedWorldModel)  // Result of Π(A_1..A_10)
       newShard (AgentShard)            // A_11
       config (MergeConfig)
OUTPUT: mergedModelNew (MergedWorldModel)

BEGIN
  // Phase 1: Extract existing claims
  existingClaims ← mergedModel.claims
  newClaims ← newShard.claims

  // Phase 2: Prepare delta
  delta ← []

  FOR EACH claim IN newClaims DO
    IF claim.key NOT IN existingClaims THEN
      // New key - just append
      delta.append({
        operation: "insert",
        claim: claim,
        shard_id: newShard.agent_id
      })
    ELSE
      // Key exists - compare timestamps
      existing ← existingClaims[claim.key]

      IF claim.timestamp_ns > existing.timestamp_ns THEN
        delta.append({
          operation: "update",
          claim: claim,
          shard_id: newShard.agent_id,
          old_claim: existing.claim
        })
      ELSE IF claim.timestamp_ns = existing.timestamp_ns THEN
        IF claim.value ≠ existing.claim.value THEN
          delta.append({
            operation: "conflict",
            claim: claim,
            shard_id: newShard.agent_id,
            existing_claim: existing.claim
          })
        END IF
      ELSE
        // Older claim - skip
        continue
      END IF
    END IF
  END FOR

  // Phase 3: Apply delta
  newMergedClaims ← Copy(existingClaims)
  newConflicts ← Copy(mergedModel.conflicts)

  FOR EACH delta_entry IN delta DO
    operation ← delta_entry.operation
    claim ← delta_entry.claim
    key ← claim.key

    IF operation = "insert" THEN
      newMergedClaims[key] ← {
        claim: claim,
        timestamp_ns: claim.timestamp_ns,
        agent_id: delta_entry.shard_id,
        version: 1
      }

    ELSE IF operation = "update" THEN
      oldVersion ← newMergedClaims[key].version

      newMergedClaims[key] ← {
        claim: claim,
        timestamp_ns: claim.timestamp_ns,
        agent_id: delta_entry.shard_id,
        version: oldVersion + 1
      }

    ELSE IF operation = "conflict" THEN
      conflict ← RecordConflict(
        key,
        delta_entry.existing_claim,
        claim,
        newMergedClaims[key].timestamp_ns,
        claim.timestamp_ns
      )
      newConflicts.append(conflict)
    END IF
  END FOR

  // Phase 4: Update receipt
  newReceipt ← ComputeMerkleReceipt(
    {mergedModel.receipt.input_shards... + newShard},
    newMergedClaims,
    newConflicts,
    config.algorithm_version
  )

  // Phase 5: Return updated model
  currentTime ← GetCurrentTimeNs()

  RETURN MergedWorldModel {
    claims: newMergedClaims,
    universe: UpdateProbeUniverse(mergedModel.universe, delta),
    conflicts: newConflicts,
    receipt: newReceipt,
    stats: {
      total_shards: 11,
      total_claims: Length(newMergedClaims),
      merged_unique_keys: Length(newMergedClaims),
      conflicts_detected: Length(newConflicts),
      timestamp_ns: currentTime,
      delta_size: Length(delta)
    }
  }
END
```

### 4.3 Monoid Property Proof

```
THEOREM: Incremental Merge Satisfies Monoid Law

Claim: Π(A, B) = Π(Π(A), B)
       where A = Π(A_1..A_10), B = A_11

Proof:
  1. Both paths apply LWW rule identically
  2. Both sort claims by (domain, method, key, timestamp) deterministically
  3. Merge order irrelevant due to commutativity
  4. Therefore: IncrementalMerge(A, B) = MergeShards(A ∪ {B})

Time Complexity:
  Brute force: O(n log n) where n = |A_1| + ... + |A_11|
  Incremental: O(m log m) where m = |A_11| = |B|
  Speedup: n/m ≈ 10-100x for practical shards
```

---

## 5. ORDERING GUARANTEES

### 5.1 Sort Key Definition

```
STRUCTURE: SortKey = (domain, method, resource_key, timestamp_ns)

Domain Ordering:
  Lexicographic by string (e.g., "code:analysis" < "rdf:store")

Method Ordering:
  Lexicographic by string (e.g., "exhaustive" < "heuristic" < "sample")

Resource Key Ordering:
  Lexicographic by string (e.g., "A:B:C" < "A:B:D")

Timestamp Ordering:
  Numeric ascending: older timestamps first, newer last

Stability:
  For ties on (domain, method, resource_key, timestamp):
    Use agent_id (lexicographic) as tiebreaker
    → Ensures deterministic ordering across runs
```

### 5.2 Determinism Proof

```
THEOREM: Merge Result Hash is Deterministic

Claim: Hash(Π(shards)) = Hash(Π(permutation(shards)))
       for any permutation of input shards

Proof:
  1. All claims sorted by deterministic key (SortKey)
  2. Merge algorithm processes claims in sorted order
  3. LWW rule deterministic: timestamp comparison → unique winner
  4. Tie-breaking via agent_id deterministic: lexicographic
  5. Therefore: merge result identical regardless of input order

Algorithm:
  resultHash ← SHA256(SerializeMerged(MergeShards(shards)))

  For any permutation P:
    Π(shards) = Π(P(shards))  // Structure preserved
    Hash(Π(shards)) = Hash(Π(P(shards)))  // Hash identical
```

### 5.3 Ordering Algorithm

```
ALGORITHM: DeterministicSort
INPUT: claims (array of ClaimWrapper)
OUTPUT: sorted (array of ClaimWrapper)

BEGIN
  // Define comparison function
  compareFunc ← FUNCTION(claimA, claimB):
    // 1. Compare domain
    domainCmp ← StringCompare(claimA.domain, claimB.domain)
    IF domainCmp ≠ 0 THEN RETURN domainCmp END IF

    // 2. Compare method
    methodCmp ← StringCompare(claimA.method, claimB.method)
    IF methodCmp ≠ 0 THEN RETURN methodCmp END IF

    // 3. Compare resource key
    keyCmp ← StringCompare(claimA.key, claimB.key)
    IF keyCmp ≠ 0 THEN RETURN keyCmp END IF

    // 4. Compare timestamp (ascending)
    timestampCmp ← NumericCompare(claimA.timestamp_ns, claimB.timestamp_ns)
    IF timestampCmp ≠ 0 THEN RETURN timestampCmp END IF

    // 5. Tie-breaker: agent_id
    agentCmp ← StringCompare(claimA.agent_id, claimB.agent_id)
    RETURN agentCmp
  END FUNCTION

  // Use stable sort (e.g., Timsort, Mergesort)
  StableSortBy(claims, compareFunc)

  RETURN claims
END
```

---

## 6. MERKLE RECEIPT COMPUTATION

### 6.1 Receipt Structure

```
MerkleReceipt {
  algorithm_version: string            // "1.0"

  input_shards: {
    agent_id: {
      shard_hash: string               // SHA-256 of shard content
      claim_count: int
      timestamp_ns: int64
    }
  }

  merge_tree: {
    root_hash: string                  // Final merkle root
    levels: int                        // Depth of tree
    leaf_hashes: string[]              // Sorted leaf hashes
  }

  claims_hash: string                  // SHA-256 of all merged claims
  conflicts_hash: string               // SHA-256 of conflict register

  algorithm_params: {
    conflict_ttl_ns: int64
    max_conflicts: int
    sort_key_schema: string            // "(domain, method, key, timestamp)"
  }

  computed_at_ns: int64
  computed_by: string                  // Agent ID

  // For verification
  verification_token: string           // HMAC-SHA-256 proof
}
```

### 6.2 Receipt Computation

```
ALGORITHM: ComputeMerkleReceipt
INPUT: shards (array of AgentShard)
       mergedClaims (map of claims)
       conflicts (array of ConflictRecord)
       algorithmVersion (string)
OUTPUT: receipt (MerkleReceipt)

BEGIN
  // Phase 1: Hash all input shards
  shardHashes ← {}

  FOR i ← 1 TO Length(shards) DO
    shard ← shards[i]

    shardContent ← Serialize({
      agent_id: shard.agent_id,
      claims: shard.claims,
      timestamp_ns: shard.timestamp_ns
    })

    shardHash ← SHA256(shardContent)

    shardHashes[shard.agent_id] ← {
      shard_hash: shardHash,
      claim_count: Length(shard.claims),
      timestamp_ns: shard.timestamp_ns
    }
  END FOR

  // Phase 2: Build merkle tree of claims
  claimsList ← []
  FOR EACH (key, claimWrapper) IN mergedClaims DO
    claimsList.append(claimWrapper)
  END FOR

  // Sort for determinism
  StableSortBy(claimsList, SortKeyComparator)

  // Build leaf hashes
  leafHashes ← []
  FOR EACH claimWrapper IN claimsList DO
    claimHash ← SHA256(Serialize(claimWrapper))
    leafHashes.append(claimHash)
  END FOR

  // Phase 3: Build merkle tree bottom-up
  merkleTree ← BuildMerkleTree(leafHashes)

  // Phase 4: Hash conflicts
  conflictsSerialized ← Serialize(conflicts)
  conflictsHash ← SHA256(conflictsSerialized)

  // Phase 5: Compute master hash
  masterContent ← Serialize({
    shard_hashes: shardHashes,
    merkle_root: merkleTree.root_hash,
    conflicts_hash: conflictsHash,
    algorithm_version: algorithmVersion
  })

  masterHash ← SHA256(masterContent)

  // Phase 6: Create verification token
  verificationToken ← HMAC-SHA256(
    key: "orchestrator-secret",
    message: masterHash + GetCurrentTimeNs()
  )

  // Phase 7: Return receipt
  RETURN MerkleReceipt {
    algorithm_version: algorithmVersion,
    input_shards: shardHashes,
    merge_tree: {
      root_hash: merkleTree.root_hash,
      levels: merkleTree.height,
      leaf_hashes: leafHashes
    },
    claims_hash: SHA256(Serialize(mergedClaims)),
    conflicts_hash: conflictsHash,
    algorithm_params: {
      conflict_ttl_ns: 86400e9,
      max_conflicts: 10000,
      sort_key_schema: "(domain, method, key, timestamp)"
    },
    computed_at_ns: GetCurrentTimeNs(),
    computed_by: "orchestrator-agent-7",
    verification_token: verificationToken
  }
END
```

### 6.3 Merkle Tree Construction

```
SUBROUTINE: BuildMerkleTree
INPUT: leafHashes (array of strings)
OUTPUT: merkleTree (MerkleTreeNode)

BEGIN
  IF Length(leafHashes) = 0 THEN
    RETURN {
      root_hash: SHA256(""),
      height: 0,
      children: []
    }
  END IF

  // Initialize leaf level
  currentLevel ← []
  FOR EACH hash IN leafHashes DO
    node ← {
      hash: hash,
      children: []
    }
    currentLevel.append(node)
  END FOR

  // Build tree bottom-up
  height ← 0
  WHILE Length(currentLevel) > 1 DO
    nextLevel ← []

    // Process pairs
    FOR i ← 0 TO Length(currentLevel) - 1 STEP 2 DO
      left ← currentLevel[i]

      IF i + 1 < Length(currentLevel) THEN
        right ← currentLevel[i + 1]
      ELSE
        right ← left  // Duplicate last for odd count
      END IF

      parentHash ← SHA256(left.hash + right.hash)

      parent ← {
        hash: parentHash,
        children: [left, right]
      }

      nextLevel.append(parent)
    END FOR

    currentLevel ← nextLevel
    height ← height + 1
  END WHILE

  root ← currentLevel[0]

  RETURN {
    root_hash: root.hash,
    height: height,
    children: root.children
  }
END
```

---

## 7. PROBE UNIVERSE CONSTRUCTION

### 7.1 ProbeUniverse Format

```
ProbeUniverse {
  metadata: {
    created_at_ns: int64
    version: string
    total_agents: int
    total_claims: int
  }

  rdf_quads: Quad[]                    // All RDF assertions
  metrics_store: {
    metric_name: MetricValue[]         // Time series metrics
  }

  resource_index: {
    resource_id: Resource              // Index by resource
  }

  agent_contributions: {
    agent_id: {
      claims_count: int
      domains: string[]
      last_update_ns: int64
    }
  }
}

Quad {
  subject: string
  predicate: string
  object: any
  graph: string (optional)
  timestamp_ns: int64
  confidence: float
  agent_id: string
}

Resource {
  id: string
  type: string[]
  properties: {
    key: Value[]
  }
  updated_at_ns: int64
  version: int
  agent_id: string
}
```

### 7.2 Universe Construction

```
ALGORITHM: BuildProbeUniverse
INPUT: mergedClaims (map)
       shardMap (map of shard metadata)
OUTPUT: universe (ProbeUniverse)

BEGIN
  universe ← ProbeUniverse {
    metadata: {
      created_at_ns: GetCurrentTimeNs(),
      version: "1.0",
      total_agents: Length(shardMap),
      total_claims: Length(mergedClaims)
    },
    rdf_quads: [],
    metrics_store: {},
    resource_index: {},
    agent_contributions: {}
  }

  // Index claims by domain
  claimsByDomain ← {}
  FOR EACH (key, claimWrapper) IN mergedClaims DO
    domain ← claimWrapper.claim.domain

    IF domain NOT IN claimsByDomain THEN
      claimsByDomain[domain] ← []
    END IF

    claimsByDomain[domain].append(claimWrapper)
  END FOR

  // Process claims by domain
  FOR EACH (domain, claims) IN claimsByDomain DO
    IF domain = "rdf:store" THEN
      ProcessRDFClaims(universe, claims)
    ELSE IF domain = "metrics:*" THEN
      ProcessMetricClaims(universe, claims)
    ELSE
      ProcessGenericClaims(universe, claims)
    END IF
  END FOR

  // Build agent contribution map
  FOR EACH (key, claimWrapper) IN mergedClaims DO
    agent_id ← claimWrapper.agent_id

    IF agent_id NOT IN universe.agent_contributions THEN
      universe.agent_contributions[agent_id] ← {
        claims_count: 0,
        domains: [],
        last_update_ns: 0
      }
    END IF

    contrib ← universe.agent_contributions[agent_id]
    contrib.claims_count ← contrib.claims_count + 1

    domain ← claimWrapper.claim.domain
    IF domain NOT IN contrib.domains THEN
      contrib.domains.append(domain)
    END IF

    contrib.last_update_ns ← MAX(
      contrib.last_update_ns,
      claimWrapper.claim.timestamp_ns
    )
  END FOR

  RETURN universe
END
```

---

## 8. DATA STRUCTURES & SCHEMAS

### 8.1 MergedWorldModel

```
MergedWorldModel {
  claims: {
    key: MergedClaimEntry              // Unique key → latest claim
  }

  universe: ProbeUniverse              // RDF + metrics
  conflicts: ConflictRecord[]          // Detected conflicts
  receipt: MerkleReceipt               // Integrity proof

  stats: {
    total_shards: int
    total_claims: int
    merged_unique_keys: int
    conflicts_detected: int
    timestamp_ns: int64
  }
}

MergedClaimEntry {
  claim: Claim
  timestamp_ns: int64
  agent_id: string
  version: int                         // How many times updated
}
```

### 8.2 MergeConfig

```
MergeConfig {
  algorithm_version: string            // "1.0"

  conflict_strategy: enum
    "lww"                              // Last-write-wins
    "majority_vote"                    // Requires >5 shards agreement
    "quorum"                           // Requires 7+ shards agreement

  conflict_ttl_ns: int64               // Seconds to keep conflicts
  max_conflicts: int                   // Max conflict records

  enable_incremental: boolean          // Use incremental merge
  verify_receipt: boolean              // Validate merkle proof

  logging: {
    log_level: enum ("debug", "info", "warn", "error")
    log_conflicts: boolean
    log_timing: boolean
  }
}
```

---

## 9. COMPLEXITY ANALYSIS

### 9.1 Time Complexity

```
Phase 1 (Flatten & Validate):
  Per shard: O(|claims_i|)
  Total: O(n) where n = Σ|claims_i|
  Space: O(n)

Phase 2 (Sort):
  Comparison-based sort: O(n log n)
  Comparator complexity: O(k) where k = max key length
  Total: O(n log n * k)

Phase 3 (Merge & LWW):
  Hash table insertions: O(n)
  Per claim: O(1) amortized
  Conflict detection: O(n)
  Total: O(n)

Phase 4 (Build Universe):
  Index by domain: O(n)
  Process by domain: O(n)
  Total: O(n)

Phase 5 (Merkle Receipt):
  Shard hashing: O(n)
  Merkle tree: O(n log n) (n leaf nodes)
  Total: O(n log n)

OVERALL: O(n log n) dominated by sort + merkle tree

Where n = |A_1.claims| + |A_2.claims| + ... + |A_10.claims|
```

### 9.2 Space Complexity

```
Input storage:      O(n)               // All claims
Sorted claims:      O(n)               // In-memory sort
Merged claims:      O(u) where u ≤ n   // Unique keys
Conflict register:  O(c) where c ≤ u   // Conflicts
Merkle tree:        O(n)               // Balanced tree
ProbeUniverse:      O(n)               // Full RDF

OVERALL: O(n) linear space
```

### 9.3 Incremental Merge Complexity

```
Delta computation:  O(m) where m = |A_11.claims|
Merge delta:        O(m) amortized
Update merkle:      O(m log n) incremental proof
Total:              O(m log n)

vs. Brute force:    O(n log n)
Speedup: n/m ≈ 10-100x for typical shards
```

---

## 10. ORCHESTRATOR OUTPUT SPECIFICATION

### 10.1 Output Structure

```json
{
  "orchestration": {
    "request_id": "req-12345",
    "timestamp_ns": 1672531200000000000,
    "algorithm": "MergeShards-v1.0"
  },

  "merged_world_model": {
    "claims": {
      "domain:resource:method:hash": {
        "claim": { /* Claim object */ },
        "timestamp_ns": 1672531200000000000,
        "agent_id": "agent-1",
        "version": 3
      }
    },

    "universe": {
      "metadata": {
        "created_at_ns": 1672531200000000000,
        "version": "1.0",
        "total_agents": 10,
        "total_claims": 45000
      },

      "rdf_quads": [ /* RDF assertions */ ],
      "metrics_store": { /* Time series */ },
      "agent_contributions": { /* Attribution */ }
    },

    "conflicts": [
      {
        "conflict_id": "conf-1",
        "resource_key": "domain:resource:method",
        "claim_a": { /* Claim A */ },
        "claim_b": { /* Claim B */ },
        "conflict_type": "value_divergence",
        "agent_a": "agent-1",
        "agent_b": "agent-5",
        "timestamp_a_ns": 1672531100000000000,
        "timestamp_b_ns": 1672531200000000000,
        "resolution": {
          "resolved_at_ns": 1672531300000000000,
          "strategy": "lww",
          "chosen_value": "winner",
          "reason": "Last-write-wins: B newer than A"
        }
      }
    ]
  },

  "merkle_receipt": {
    "algorithm_version": "1.0",
    "input_shards": {
      "agent-1": {
        "shard_hash": "sha256:abc123...",
        "claim_count": 4500,
        "timestamp_ns": 1672530000000000000
      }
    },
    "merge_tree": {
      "root_hash": "sha256:def456...",
      "levels": 12,
      "leaf_hashes": [ /* 45000 hashes */ ]
    },
    "claims_hash": "sha256:ghi789...",
    "conflicts_hash": "sha256:jkl012...",
    "computed_at_ns": 1672531200000000000,
    "verification_token": "hmac:mno345..."
  },

  "statistics": {
    "total_shards": 10,
    "total_claims": 45000,
    "merged_unique_keys": 38000,
    "conflicts_detected": 150,
    "resolved_conflicts": 120,
    "unresolved_conflicts": 30,
    "shard_agreement_ratio": 0.95
  },

  "performance": {
    "phase_1_flatten_ms": 120,
    "phase_2_sort_ms": 450,
    "phase_3_merge_ms": 280,
    "phase_4_universe_ms": 320,
    "phase_5_merkle_ms": 510,
    "total_ms": 1680,
    "throughput_claims_per_sec": 26786
  }
}
```

### 10.2 Output Verification

```
ALGORITHM: VerifyMergeOutput
INPUT: output (MergeOutput)
OUTPUT: valid (boolean), issues (string[])

BEGIN
  issues ← []

  // Check receipt integrity
  IF NOT VerifyMerkleReceipt(output.merkle_receipt) THEN
    issues.append("Merkle receipt verification failed")
  END IF

  // Check claim counts
  totalClaims ← Σ(output.statistics.total_claims)
  IF Length(output.merged_world_model.universe.rdf_quads) ≠ totalClaims THEN
    issues.append("Claim count mismatch")
  END IF

  // Check conflicts < claims
  IF output.statistics.conflicts_detected > output.statistics.merged_unique_keys THEN
    issues.append("More conflicts than claims (logical error)")
  END IF

  // Check agent contribution sums
  contributionSum ← Σ(claims_count for each agent)
  IF contributionSum ≠ output.statistics.total_claims THEN
    issues.append("Agent contribution counts don't sum correctly")
  END IF

  // Check timestamps monotonic
  IF NOT CheckTimestampMonotonic(output.merged_world_model.universe.rdf_quads) THEN
    issues.append("Non-monotonic timestamps detected")
  END IF

  valid ← Length(issues) = 0
  RETURN {valid: valid, issues: issues}
END
```

---

## 11. DESIGN PATTERNS

### 11.1 Monoid Pattern

The merge function implements a **commutative monoid**:

```
Pattern Name: Monoid Reduction
Location: MergeShards algorithm
Benefit: Enables associative + commutative merges
  → Can reorder operations without affecting result
  → Supports incremental, parallel, distributed merging

Implementation:
  merge(A, B) = merge(B, A)        // Commutative
  merge(merge(A, B), C) = merge(A, merge(B, C))  // Associative
  merge(A, ∅) = A                  // Identity
```

### 11.2 Last-Write-Wins (LWW) Pattern

```
Pattern Name: Last-Write-Wins
Location: Conflict resolution
Benefit: Deterministic, no blocking
Tradeoff: Potential data loss if concurrent writes

Implementation:
  timestamp_newer > timestamp_old  → Keep newer
  timestamp_equal ≠ value         → Log conflict, keep first
```

### 11.3 Merkle Tree Pattern

```
Pattern Name: Merkle Tree Integrity
Location: Receipt computation
Benefit: Verify all data without replaying computation
Tradeoff: O(log n) proof size instead of O(1)

Implementation:
  Hash leaves (claims)
  → Parent = Hash(Left || Right)
  → Root integrity proof
```

### 11.4 Strategy Pattern for Conflict Resolution

```
Pattern Name: Strategy
Location: Future conflict resolution
Benefit: Pluggable resolution strategies

INTERFACE: ConflictResolutionStrategy
  resolve(conflictA, conflictB): value

IMPLEMENTATIONS:
  LastWriteWins: Choose newer
  MajorityVote: >5/10 shards agree
  QuorumLease: 7/10 shards agree + timeout
  Heuristic: Choose by confidence score
```

---

## 12. IMPLEMENTATION ROADMAP

### Phase 1: Core Merge Engine (Week 1)
- [ ] Implement MergeShards algorithm
- [ ] Add ValidateClaim & DeterministicSort
- [ ] Create test harness with 10 mock shards
- [ ] Target: O(n log n) verified, basic tests passing

### Phase 2: Conflict Detection (Week 2)
- [ ] Implement ConflictRecord tracking
- [ ] Add RecordConflict & DetermineConflictType
- [ ] Conflict register + TTL cleanup
- [ ] Target: Detect all conflicts correctly

### Phase 3: Merkle Integrity (Week 2)
- [ ] Implement BuildMerkleTree
- [ ] Add ComputeMerkleReceipt
- [ ] Hash verification + HMAC tokens
- [ ] Target: 100% receipt verification

### Phase 4: Incremental Merge (Week 3)
- [ ] Implement IncrementalMerge
- [ ] Verify monoid properties (A ⊕ A = A)
- [ ] Benchmark vs. brute-force merge
- [ ] Target: 10-100x speedup measured

### Phase 5: ProbeUniverse Construction (Week 3)
- [ ] Implement BuildProbeUniverse
- [ ] RDF quad indexing + metrics store
- [ ] Agent contribution tracking
- [ ] Target: RDF compliant quads

### Phase 6: Output & Validation (Week 4)
- [ ] Implement VerifyMergeOutput
- [ ] Full JSON output schema
- [ ] Performance metrics collection
- [ ] Target: All checks passing

### Phase 7: Testing & Docs (Week 4)
- [ ] Unit tests (all subroutines)
- [ ] Integration tests (10-shard merge)
- [ ] Chaos tests (conflicting claims, duplicates)
- [ ] Performance benchmarks
- [ ] Target: 100% pass, <2ms per claim

---

## 13. APPENDIX: PSEUDOCODE SYNTAX REFERENCE

```
// Control flow
IF condition THEN ... END IF
WHILE condition DO ... END WHILE
FOR EACH item IN collection DO ... END FOR
FOR i ← 1 TO n DO ... END FOR

// Functions
ALGORITHM: Name
SUBROUTINE: Name
INPUT: param (type)
OUTPUT: result (type)
BEGIN ... END

// Collections
array.append(item)
map[key] ← value
set.contains(item)
Length(collection)

// Functions
Hash(value)          // SHA-256
Serialize(object)    // JSON
Copy(object)         // Deep copy
StableSortBy(array, comparator)

// Comparison
a = b                // Equality
a ≠ b                // Inequality
a < b, a > b         // Ordering
```

---

## Summary

| Component | Complexity | Key Property |
|-----------|-----------|--------------|
| MergeShards | O(n log n) | Commutative monoid |
| ConflictDetection | O(n) | LWW semantics |
| MerkleReceipt | O(n log n) | Integrity proof |
| IncrementalMerge | O(m log n) | Monoid closure |
| ProbeUniverse | O(n) | RDF-compliant output |

**Trust Model**: Merkle receipt + test output = validation

**Next Steps**: Implement Phase 1 with 10-shard test harness
