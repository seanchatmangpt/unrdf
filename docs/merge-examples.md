# Orchestrator Merge Algorithm: Examples & Test Cases

**Purpose**: Concrete walkthroughs of the merge algorithm with real data

---

## Example 1: Simple Two-Shard Merge (No Conflicts)

### Input Shards

**Shard A_1** (Agent-1, timestamp = 1000):
```json
{
  "agent_id": "agent-1",
  "domain": "rdf:store",
  "timestamp_ns": 1000,
  "claims": [
    {
      "key": "rdf:store:assertion:claim1",
      "domain": "rdf:store",
      "method": "assertion",
      "value": {"subject": "ex:alice", "predicate": "foaf:name", "object": "Alice"},
      "timestamp_ns": 1000,
      "agent_id": "agent-1",
      "confidence": 0.95,
      "proof": {
        "type": "direct",
        "evidence": [{
          "evidence_id": "e1",
          "type": "direct",
          "timestamp_ns": 1000,
          "source": "agent-1",
          "data": "Triple verified",
          "method": "pattern_match"
        }],
        "reasoning": "Direct triple from RDF parse"
      }
    }
  ]
}
```

**Shard A_2** (Agent-2, timestamp = 2000):
```json
{
  "agent_id": "agent-2",
  "domain": "rdf:store",
  "timestamp_ns": 2000,
  "claims": [
    {
      "key": "rdf:store:assertion:claim2",
      "domain": "rdf:store",
      "method": "assertion",
      "value": {"subject": "ex:bob", "predicate": "foaf:age", "object": 30},
      "timestamp_ns": 2000,
      "agent_id": "agent-2",
      "confidence": 0.99,
      "proof": {
        "type": "direct",
        "evidence": [{
          "evidence_id": "e2",
          "type": "direct",
          "timestamp_ns": 2000,
          "source": "agent-2",
          "data": "Age data",
          "method": "schema_validation"
        }],
        "reasoning": "Direct data from source"
      }
    }
  ]
}
```

### Merge Process

**Phase 1: Flatten & Validate**
```
allClaims = [
  {claim: claim1, shard_id: "agent-1", shard_timestamp: 1000},
  {claim: claim2, shard_id: "agent-2", shard_timestamp: 2000}
]
Status: ✓ All claims valid
```

**Phase 2: Sort**
```
After deterministic sort by (domain, method, key, timestamp):
[
  {claim: claim1, shard_id: "agent-1"},  // rdf:store:assertion:claim1, ts=1000
  {claim: claim2, shard_id: "agent-2"}   // rdf:store:assertion:claim2, ts=2000
]
Status: ✓ Sorted
```

**Phase 3: Apply LWW Merge**
```
mergedClaims = {
  "rdf:store:assertion:claim1": {
    claim: claim1,
    timestamp_ns: 1000,
    agent_id: "agent-1",
    version: 1
  },
  "rdf:store:assertion:claim2": {
    claim: claim2,
    timestamp_ns: 2000,
    agent_id: "agent-2",
    version: 1
  }
}
Status: ✓ No conflicts (different keys)
conflicts = []
```

### Output

```json
{
  "merged_world_model": {
    "claims": {
      "rdf:store:assertion:claim1": {
        "claim": { /* claim1 */ },
        "timestamp_ns": 1000,
        "agent_id": "agent-1",
        "version": 1
      },
      "rdf:store:assertion:claim2": {
        "claim": { /* claim2 */ },
        "timestamp_ns": 2000,
        "agent_id": "agent-2",
        "version": 1
      }
    },
    "conflicts": [],
    "stats": {
      "total_shards": 2,
      "total_claims": 2,
      "merged_unique_keys": 2,
      "conflicts_detected": 0
    }
  }
}
```

---

## Example 2: Conflict Detection (Same Key, Different Values)

### Input Shards

**Shard A_1** (Agent-1, timestamp = 1000):
```json
{
  "agent_id": "agent-1",
  "domain": "code:analysis",
  "timestamp_ns": 1000,
  "claims": [
    {
      "key": "code:analysis:complexity:module_x",
      "domain": "code:analysis",
      "method": "complexity",
      "value": {"cyclomatic_complexity": 5, "estimated_maintainability": 0.85},
      "timestamp_ns": 1000,
      "agent_id": "agent-1",
      "confidence": 0.92,
      "proof": {
        "type": "direct",
        "evidence": [{"evidence_id": "e1", "type": "direct", "timestamp_ns": 1000, "source": "agent-1", "data": "Static analysis", "method": "cyclomatic_metric"}],
        "reasoning": "Measured with McCabe metric"
      }
    }
  ]
}
```

**Shard A_2** (Agent-2, timestamp = 1500 - NEWER):
```json
{
  "agent_id": "agent-2",
  "domain": "code:analysis",
  "timestamp_ns": 1500,
  "claims": [
    {
      "key": "code:analysis:complexity:module_x",
      "domain": "code:analysis",
      "method": "complexity",
      "value": {"cyclomatic_complexity": 7, "estimated_maintainability": 0.72},
      "timestamp_ns": 1500,
      "agent_id": "agent-2",
      "confidence": 0.88,
      "proof": {
        "type": "direct",
        "evidence": [{"evidence_id": "e2", "type": "direct", "timestamp_ns": 1500, "source": "agent-2", "data": "Dynamic analysis", "method": "flow_graph"}],
        "reasoning": "Measured with flow graph analysis"
      }
    }
  ]
}
```

### Merge Process

**Phase 3: LWW Merge with Conflict**
```
Key = "code:analysis:complexity:module_x"

1. First claim (from A_1): timestamp=1000
   mergedClaims[key] = {claim: from_A1, timestamp_ns: 1000, agent_id: "agent-1"}

2. Second claim (from A_2): timestamp=1500 (NEWER)
   1500 > 1000 → LWW wins
   value_A1 = {cyclomatic: 5, maintainability: 0.85}
   value_A2 = {cyclomatic: 7, maintainability: 0.72}
   value_A1 ≠ value_A2 → CONFLICT DETECTED

   RecordConflict:
   - conflict_id: "conf-abc123"
   - resource_key: "code:analysis:complexity:module_x"
   - agent_a: "agent-1", agent_b: "agent-2"
   - conflict_type: "value_divergence"
   - timestamp_a_ns: 1000, timestamp_b_ns: 1500

   Result: Keep claim from A_2 (newer)
   mergedClaims[key].version = 2
```

### Output

```json
{
  "merged_world_model": {
    "claims": {
      "code:analysis:complexity:module_x": {
        "claim": {
          "key": "code:analysis:complexity:module_x",
          "value": {"cyclomatic_complexity": 7, "estimated_maintainability": 0.72},
          "timestamp_ns": 1500,
          "agent_id": "agent-2",
          "confidence": 0.88
        },
        "timestamp_ns": 1500,
        "agent_id": "agent-2",
        "version": 2
      }
    },
    "conflicts": [
      {
        "conflict_id": "conf-abc123",
        "resource_key": "code:analysis:complexity:module_x",
        "agent_a": "agent-1",
        "agent_b": "agent-2",
        "timestamp_a_ns": 1000,
        "timestamp_b_ns": 1500,
        "conflict_type": "value_divergence",
        "value_a": {"cyclomatic_complexity": 5, "estimated_maintainability": 0.85},
        "value_b": {"cyclomatic_complexity": 7, "estimated_maintainability": 0.72},
        "resolution": {
          "resolved_at_ns": 1600,
          "strategy": "lww",
          "chosen_value": {"cyclomatic_complexity": 7, "estimated_maintainability": 0.72},
          "reason": "Last-write-wins: Agent-2 claim (ts=1500) newer than Agent-1 (ts=1000)"
        }
      }
    ],
    "stats": {
      "total_shards": 2,
      "total_claims": 2,
      "merged_unique_keys": 1,
      "conflicts_detected": 1,
      "resolved_conflicts": 1
    }
  }
}
```

---

## Example 3: Simultaneous Timestamp Conflict (Same Timestamp)

### Input Shards

**Shard A_1** (Agent-1):
```json
{
  "agent_id": "agent-1",
  "claims": [
    {
      "key": "metrics:performance:latency_p99",
      "value": {"latency_ms": 150, "unit": "milliseconds"},
      "timestamp_ns": 2000,
      "agent_id": "agent-1",
      "confidence": 0.95
    }
  ]
}
```

**Shard A_2** (Agent-2):
```json
{
  "agent_id": "agent-2",
  "claims": [
    {
      "key": "metrics:performance:latency_p99",
      "value": {"latency_ms": 155, "unit": "milliseconds"},
      "timestamp_ns": 2000,  // SAME TIMESTAMP
      "agent_id": "agent-2",
      "confidence": 0.92
    }
  ]
}
```

### Merge Process

```
Key = "metrics:performance:latency_p99"

1. First claim (A_1): timestamp=2000
   mergedClaims[key] = {claim: from_A1, timestamp_ns: 2000}

2. Second claim (A_2): timestamp=2000 (SAME)
   2000 = 2000 → Same timestamp
   value_A1 = {latency_ms: 150}
   value_A2 = {latency_ms: 155}
   value_A1 ≠ value_A2 → CONFLICT (but keep existing)

   RecordConflict (but DON'T update merged result)
   conflict_type: "value_divergence"

   Keep existing: mergedClaims[key] still from_A1 (first-write-wins on ties)
```

### Output

```json
{
  "conflicts": [
    {
      "conflict_id": "conf-xyz789",
      "resource_key": "metrics:performance:latency_p99",
      "agent_a": "agent-1",
      "agent_b": "agent-2",
      "timestamp_a_ns": 2000,
      "timestamp_b_ns": 2000,
      "conflict_type": "value_divergence",
      "value_a": {"latency_ms": 150},
      "value_b": {"latency_ms": 155},
      "resolution": null,  // Unresolved (same timestamp)
      "metadata": {
        "similarity": 0.98  // Very similar values
      }
    }
  ],
  "stats": {
    "conflicts_detected": 1,
    "resolved_conflicts": 0,
    "unresolved_conflicts": 1
  }
}
```

---

## Example 4: Incremental Merge Property

### Initial Merge: Π(A_1, A_2) = A

```
Merged result A has:
  - Key K1 = "rdf:store:fact:1" → timestamp=1000
  - Key K2 = "rdf:store:fact:2" → timestamp=2000

Total: 2 unique claims
```

### New Shard A_3 Arrives

```json
{
  "agent_id": "agent-3",
  "claims": [
    {
      "key": "rdf:store:fact:1",  // Existing key!
      "value": {"new": "data"},
      "timestamp_ns": 1500        // Between 1000 and 2000
    },
    {
      "key": "rdf:store:fact:3",  // NEW key
      "value": {"new_fact": true},
      "timestamp_ns": 3000
    }
  ]
}
```

### Incremental Merge Process

**Delta Computation** (only process A_3 claims):

```
For claim 1 (key="rdf:store:fact:1"):
  - Exists in A? YES
  - existing.timestamp_ns = 1000
  - claim.timestamp_ns = 1500
  - 1500 > 1000 → UPDATE
  delta.append({operation: "update", ...})

For claim 2 (key="rdf:store:fact:3"):
  - Exists in A? NO
  - → INSERT
  delta.append({operation: "insert", ...})
```

**Apply Delta** (2 operations instead of re-sorting all 5 claims):

```
Complexity: O(2) ≈ O(m) where m=|A_3|
Vs. Brute-force: O(5 log 5) = O(n log n)
Speedup: ~5x for this example
```

**Result: A' = Π(A_1, A_2, A_3)**

```
Merged A' now has:
  - Key K1: UPDATED to A_3 version (timestamp=1500)
  - Key K2: unchanged from A_2 (timestamp=2000)
  - Key K3: NEW from A_3 (timestamp=3000)

Total: 3 unique claims

Proof of monoid property:
  A' = IncrementalMerge(A, A_3)
  A' = MergeShards(A_1, A_2, A_3)  // Would give same result
  Therefore: closure property ✓
```

---

## Example 5: 10-Shard Merge Scenario

### Setup

**10 agents, each with different claims:**

| Agent | Domain | Claims | Timestamp | Status |
|-------|--------|--------|-----------|--------|
| A_1 | rdf:store | 4500 | 1000 | ✓ |
| A_2 | code:analysis | 4200 | 2000 | ✓ |
| A_3 | metrics:perf | 3800 | 1500 | ✓ |
| A_4 | rdf:store | 4100 | 3000 | ✓ conflict with A_1 |
| A_5 | docs:coverage | 4000 | 2500 | ✓ |
| A_6 | code:analysis | 3900 | 3500 | ✓ conflict with A_2 |
| A_7 | metrics:perf | 3500 | 2000 | ✓ |
| A_8 | cache:stats | 3200 | 4000 | ✓ |
| A_9 | rdf:store | 3100 | 3200 | ✓ |
| A_10 | security:audit | 2900 | 2800 | ✓ |

**Total input claims**: 41,200

### Merge Execution

**Phase 1: Flatten (O(n))**
```
Processing 41,200 claims from 10 shards
Status: ✓ All valid
Duration: ~50ms
```

**Phase 2: Sort (O(n log n))**
```
Sort by (domain, method, key, timestamp)
Comparisons: 41,200 * log(41,200) ≈ 600K comparisons
Duration: ~200ms
```

**Phase 3: LWW Merge (O(n))**
```
Process sorted claims, apply LWW
Unique keys found: 38,000
Conflicts detected: 150 (all same key, different values)
Duration: ~150ms
```

**Phase 4: Universe Construction (O(n))**
```
Build RDF quads + metrics index
RDF quads: 21,000
Metrics time series: 17,000
Duration: ~180ms
```

**Phase 5: Merkle Receipt (O(n log n))**
```
Build merkle tree of 38,000 leaves
Tree height: log(38,000) ≈ 15
Hashing: 75,000 nodes
Duration: ~250ms
```

### Output Statistics

```json
{
  "merged_world_model": {
    "stats": {
      "total_shards": 10,
      "total_claims": 41200,
      "merged_unique_keys": 38000,
      "conflicts_detected": 150,
      "resolved_conflicts": 150,
      "shard_agreement_ratio": 0.963
    }
  },
  "performance": {
    "phase_1_flatten_ms": 50,
    "phase_2_sort_ms": 200,
    "phase_3_merge_ms": 150,
    "phase_4_universe_ms": 180,
    "phase_5_merkle_ms": 250,
    "total_ms": 830,
    "throughput_claims_per_sec": 49639
  }
}
```

---

## Example 6: Conflict Register Sample

### 5 Representative Conflicts

**Conflict 1: Code Complexity Divergence**
```json
{
  "conflict_id": "conf-1",
  "resource_key": "code:analysis:complexity:module_auth",
  "agent_a": "agent-2",
  "agent_b": "agent-6",
  "timestamp_a_ns": 2000,
  "timestamp_b_ns": 3500,
  "conflict_type": "value_divergence",
  "value_a": {"cyclomatic": 5, "maintainability": 0.85},
  "value_b": {"cyclomatic": 8, "maintainability": 0.71},
  "metadata": {
    "similarity": 0.60,
    "is_transitive": false
  },
  "resolution": {
    "strategy": "lww",
    "chosen_value": {"cyclomatic": 8, "maintainability": 0.71},
    "reason": "Agent-6 (ts=3500) newer than Agent-2 (ts=2000). Suggests code refactoring."
  }
}
```

**Conflict 2: Performance Metrics Variance**
```json
{
  "conflict_id": "conf-2",
  "resource_key": "metrics:perf:p99_latency_ms",
  "agent_a": "agent-3",
  "agent_b": "agent-7",
  "timestamp_a_ns": 1500,
  "timestamp_b_ns": 2000,
  "conflict_type": "value_divergence",
  "value_a": {"p99_latency": 150, "unit": "ms"},
  "value_b": {"p99_latency": 162, "unit": "ms"},
  "metadata": {
    "similarity": 0.95,
    "is_transitive": true  // Similar agents, similar methods
  },
  "resolution": {
    "strategy": "lww",
    "chosen_value": {"p99_latency": 162, "unit": "ms"},
    "reason": "Agent-7 (ts=2000) more recent. Small variance (8%) expected in measurements."
  }
}
```

**Conflict 3: Same Timestamp (Unresolved)**
```json
{
  "conflict_id": "conf-3",
  "resource_key": "cache:stats:hit_ratio",
  "agent_a": "agent-8",
  "agent_b": "unknown",  // From cache, not in final shards
  "timestamp_a_ns": 4000,
  "timestamp_b_ns": 4000,
  "conflict_type": "value_divergence",
  "value_a": {"hit_ratio": 0.78, "samples": 1000},
  "value_b": {"hit_ratio": 0.75, "samples": 1000},
  "resolution": null,  // Unresolved
  "metadata": {
    "similarity": 0.98,
    "is_transitive": false
  }
}
```

---

## Example 7: Merkle Receipt Verification

### Receipt Structure

```json
{
  "algorithm_version": "1.0",

  "input_shards": {
    "agent-1": {
      "shard_hash": "sha256:a1b2c3...",
      "claim_count": 4500,
      "timestamp_ns": 1000
    },
    "agent-2": {
      "shard_hash": "sha256:d4e5f6...",
      "claim_count": 4200,
      "timestamp_ns": 2000
    }
    // ... 8 more agents
  },

  "merge_tree": {
    "root_hash": "sha256:merged_root_xyz",
    "levels": 16,  // log2(38000) ≈ 15 levels
    "leaf_hashes": [
      "sha256:claim_1_hash",
      "sha256:claim_2_hash",
      // ... 37,998 more leaf hashes
    ]
  },

  "claims_hash": "sha256:all_claims_hash",
  "conflicts_hash": "sha256:all_conflicts_hash",

  "algorithm_params": {
    "conflict_ttl_ns": 86400000000000,
    "max_conflicts": 10000,
    "sort_key_schema": "(domain, method, key, timestamp)"
  },

  "computed_at_ns": 1704067200000000000,
  "computed_by": "orchestrator-agent-7",
  "verification_token": "hmac:sha256:verification_proof"
}
```

### Verification Process

```
VERIFY(receipt, merge_output):

1. Recompute input shard hashes
   FOR EACH agent shard:
     actual_hash = SHA256(serialize(shard))
     IF actual_hash ≠ receipt.input_shards[agent].shard_hash THEN
       FAIL("Input shard corrupted")
     END IF

2. Recompute merkle tree
   actual_root = BuildMerkleTree(receipt.leaf_hashes).root_hash
   IF actual_root ≠ receipt.merge_tree.root_hash THEN
     FAIL("Merkle tree corrupted")
   END IF

3. Verify claims hash
   actual_claims_hash = SHA256(serialize(merge_output.claims))
   IF actual_claims_hash ≠ receipt.claims_hash THEN
     FAIL("Claims corrupted")
   END IF

4. Verify HMAC token
   expected_token = HMAC-SHA256("secret", root_hash + timestamp)
   IF expected_token ≠ receipt.verification_token THEN
     FAIL("Invalid signature")
   END IF

5. All checks pass?
   RETURN ✓ Merge verified
```

---

## Test Case Matrix

| Scenario | Shards | Claims | Unique Keys | Conflicts | Status |
|----------|--------|--------|-------------|-----------|--------|
| T1: Empty | 0 | 0 | 0 | 0 | → Empty universe |
| T2: Single shard | 1 | 100 | 100 | 0 | → Pass-through |
| T3: No conflicts | 2 | 200 | 200 | 0 | → All keys unique |
| T4: LWW conflicts | 2 | 200 | 100 | 100 | → All detected |
| T5: Same timestamp | 2 | 200 | 150 | 50 | → Unresolved |
| T6: Incremental | 11 | 2000 | 1900 | 50 | → Monoid closure |
| T7: 10-shard merge | 10 | 41K | 38K | 150 | → Production |
| T8: Duplicate shard | 2 | 200 | 100 | 0 | → Idempotence |
| T9: Reordered shards | 2 | 200 | 100 | 0 | → Commutativity |
| T10: Large RDF | 5 | 50K | 48K | 100 | → Performance |

---

## Performance Baselines

```
Configuration: 41,200 total claims from 10 shards

Metric                  Value       Notes
──────────────────────────────────────────────
Flatten time            50ms        Per-shard validation
Sort time               200ms       O(n log n) comparison
Merge time              150ms       Hash table ops
Universe time           180ms       RDF indexing
Merkle time             250ms       Tree hashing
──────────────────────────────────────────────
Total                   830ms       49.6K claims/sec
Throughput              49.6K/sec
Latency p99             1.2s        With OTEL overhead

Incremental (1 shard)   50ms        10.8K claims/sec
Speedup factor          ~10x        vs. full merge
```

---

## Validation Checklist

For each test case:

- [ ] All input shards validate against AgentShardSchema
- [ ] Merge output validates against MergeOutputSchema
- [ ] Merkle receipt verifies against receipt hash
- [ ] No claims lost (total = merged + conflicted)
- [ ] Timestamps monotonic within each agent
- [ ] Conflict register contains exactly conflicts
- [ ] LWW rule applied correctly (timestamp wins)
- [ ] Idempotence: Π(A, A) = A ✓
- [ ] Commutativity: Π(A, B) = Π(B, A) ✓
- [ ] Associativity: Π(Π(A,B),C) = Π(A,Π(B,C)) ✓
- [ ] Performance: O(n log n) within bounds
- [ ] Zero data corruption in output
