## 4. Knowledge Hooks: Architecture and Predicate Types

### 4.1 Predicate Type Specifications

#### 4.1.1 ASK Predicates

**Formal Specification**:

```
πₐₛₖ(Q, expected) = {
  result ← Execute SPARQL ASK query Q over G
  return (expected = ∅) ? result : (result = expected)
}
```

**Use Cases**:
- Feature flags: `ASK { :feature :enabled true }`
- Permission checks: `ASK { :user :hasRole :admin }`
- Existence tests: `ASK { :resource a :CriticalAsset }`

**Complexity**: O(n) where n = |G|, optimized by SPARQL engine

#### 4.1.2 SHACL Predicates

**Formal Specification**:

```
πₛₕₐᴄₗ(S, mode, strict) = {
  report ← Validate G against shapes S
  violations ← {v ∈ report | v.severity ≥ threshold}

  if mode = 'conforms':
    return |violations| = 0
  else:
    return |violations| > 0 ∧ (strict ⟹ fail_fast)
}
```

**Use Cases**:
- Data quality gates: Validate schema conformance
- Compliance checks: Ensure regulatory requirements
- Business rules: Enforce domain constraints

**Complexity**: O(|S| × |G|) for shape validation

#### 4.1.3 DELTA Predicates

**Formal Specification**:

```
πᴅₑₗₜₐ(B, B_prev, K, change, δ) = {
  For each binding b ∈ B:
    key ← project(b, K)
    hash_curr ← H₂₅₆(can(b))
    hash_prev ← lookup(B_prev, key)

    if hash_prev ≠ ∅:
      diff ← |hash_curr - hash_prev| / hash_prev

      if change = 'any' ∧ hash_curr ≠ hash_prev:
        return true
      else if change = 'increase' ∧ diff > δ:
        return true
      else if change = 'decrease' ∧ diff < -δ:
        return true

  return false
}
```

**Row Digest Computation**:

```
digest(row, K) = {
  canonical_form ← serialize_canonical(row, K)
  return H₂₅₆(canonical_form)
}
```

**Use Cases**:
- Configuration drift detection
- Audit trail for state changes
- Change-data-capture patterns

**Complexity**: O(|B| log |B|) for hash table lookup

#### 4.1.4 THRESHOLD Predicates

**Formal Specification**:

```
πₜₕᵣ(B, var, op, θ, agg) = {
  values ← {b[var] | b ∈ B ∧ b[var] is numeric}

  if agg ≠ ∅:
    v ← aggregate(values, agg)  // avg, sum, count, max, min
  else:
    return ∃v ∈ values: compare(v, op, θ)

  return compare(v, op, θ)
}
```

**Use Cases**:
- KPI monitoring: Error rate > 0.02
- Resource thresholds: CPU usage > 95%
- Performance alerts: Latency > 2000ms

**Complexity**: O(|B|) for aggregation

#### 4.1.5 COUNT Predicates

**Formal Specification**:

```
πᴄₒᴜₙₜ(B, op, n) = {
  cardinality ← |B|
  return compare(cardinality, op, n)
}
```

**Use Cases**:
- Inventory checks: Stock count < minimum
- Quota limits: Active sessions > maximum
- Cardinality constraints: Exactly n values required

**Complexity**: O(1) given |B|

#### 4.1.6 WINDOW Predicates

**Formal Specification**:

```
πᴡɪɴᴅₒᴡ(B, var, size, op, cmp) = {
  window ← filter_by_time(B, current_time - size, current_time)
  values ← {b[var] | b ∈ window}
  result ← aggregate(values, op)  // count, sum, avg

  return compare(result, cmp.op, cmp.value)
}
```

**Tumbling Window Implementation**:

```
Window(t_start, t_end) = {b ∈ B | t_start ≤ b.timestamp < t_end}
```

**Use Cases**:
- Trend analysis: Average requests in last 5 minutes
- Rate limiting: Requests per hour > threshold
- Temporal patterns: Peak load detection

**Complexity**: O(|B|) with time-based indexing

### 4.2 Combinator Functions

#### Definition 4.1 (Logical Combinators)

```
φ_AND(r₁, r₂, ..., rₙ) = ⋀ rᵢ
φ_OR(r₁, r₂, ..., rₙ) = ⋁ rᵢ
φ_NOT(r₁, r₂, ..., rₙ) = ¬(⋁ rᵢ)
```

**Custom Combinators** (threshold-based):

```
φ_THRESHOLD(r₁, ..., rₙ, k) = (Σ rᵢ) ≥ k
```

### 4.3 Receipt Structure

#### Definition 4.2 (Cryptographic Receipt)

```
R = {
  // Hook identification
  id: IRI,
  fired: boolean,

  // Evaluation results
  predicates: [{
    kind: string,
    ok: boolean,
    metadata: object,
    duration_ms: float
  }],

  // Performance metrics
  durations: {
    total_ms: float,
    query_ms: float,
    predicates_ms: float,
    canonicalization_ms: float
  },

  // Cryptographic provenance
  provenance: {
    hook_hash: hex_string,      // SHA3-256
    query_hash: hex_string,
    graph_hash: hex_string,
    baseline_hash: hex_string,
    receipt_hash: hex_string
  },

  // Metadata
  timestamp: ISO8601,
  actor: IRI,
  input: {
    bindings_count: integer,
    variables: [string]
  }
}
```

---

