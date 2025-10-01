# Chapter 4: Knowledge Hooks — Predicate Algebra and Formal Semantics

## Abstract

This chapter formalizes Knowledge Hooks as a compositional predicate algebra over RDF graph bindings. We provide executable lambda calculus, denotational semantics, complexity bounds, and cryptographic provenance guarantees with formal proofs.

## Table of Contents

1. [Predicate Algebra Foundations](#1-predicate-algebra-foundations)
2. [Denotational Semantics](#2-denotational-semantics)
3. [Predicate Type System](#3-predicate-type-system)
4. [Complexity Analysis](#4-complexity-analysis)
5. [Receipt Provenance](#5-receipt-provenance)
6. [Algebraic Laws](#6-algebraic-laws)

---

## 1. Predicate Algebra Foundations

### 1.1 Core Type Definitions

**Definition 1.1** (Binding Type)
```
Binding ≡ Var → Value
Bindings ≡ Set[Binding]
```

**Definition 1.2** (Predicate Type)
```
Π ≡ Bindings → Bool

With monoid structure:
  ε_Π : Π              // Identity: λb. ⊤
  ∘_Π : Π → Π → Π     // Composition: conjunction
```

**Definition 1.3** (Effect Type)
```
Effect ≡ Graph → Graph ⊎ Error
ε : Effect              // No-op effect: λg. g
```

**Definition 1.4** (Hook Type)
```
Hook ≡ {
  query : SPARQL,
  predicates : [Π],
  combinator : [Bool] → Bool,
  effect : Effect
}
```

### 1.2 Predicate Algebra Operations

**Definition 1.5** (Conjunction)
```
(π₁ ∧ π₂) : Π
(π₁ ∧ π₂) = λb. π₁(b) ∧ π₂(b)
```

**Definition 1.6** (Disjunction)
```
(π₁ ∨ π₂) : Π
(π₁ ∨ π₂) = λb. π₁(b) ∨ π₂(b)
```

**Definition 1.7** (Negation)
```
(¬π) : Π
(¬π) = λb. ¬π(b)
```

**Definition 1.8** (Threshold Combinator)
```
thresholdₖ : [Π] → Π
thresholdₖ(π₁, ..., πₙ) = λb. (|{i | πᵢ(b) = ⊤}|) ≥ k
```

### 1.3 Monoid Laws

**Theorem 1.1** (Π forms a monoid under ∧)
```
(Π, ∧, ε_Π) is a monoid with:
  1. Identity:      π ∧ ε_Π = ε_Π ∧ π = π
  2. Associativity: (π₁ ∧ π₂) ∧ π₃ = π₁ ∧ (π₂ ∧ π₃)
  3. Commutativity: π₁ ∧ π₂ = π₂ ∧ π₁
```

**Proof**: By λ-calculus reduction
```
(π₁ ∧ π₂)(b)
  = (λb. π₁(b) ∧ π₂(b))(b)
  → π₁(b) ∧ π₂(b)           [β-reduction]
  = π₂(b) ∧ π₁(b)           [∧-commutativity]
  = (π₂ ∧ π₁)(b)
```
∎

---

## 2. Denotational Semantics

### 2.1 Semantic Domains

**Definition 2.1** (Boolean Domain)
```
𝔹 = {⊤, ⊥}
⟦_⟧_𝔹 : Π → (Bindings → 𝔹)
```

**Definition 2.2** (Graph Domain)
```
𝔾 = Set[Triple]
Triple = (Subject × Predicate × Object)
```

**Definition 2.3** (Effect Domain)
```
ℰ = 𝔾 → (𝔾 ⊎ Error)
Error = {TimeoutError, ValidationError, RuntimeError, ...}
```

### 2.2 Compositional Evaluation

**Definition 2.4** (Predicate Denotation)
```
⟦π⟧ : Bindings → 𝔹

Compositional rules:
  ⟦π₁ ∧ π₂⟧(b) = ⟦π₁⟧(b) ∧_𝔹 ⟦π₂⟧(b)
  ⟦π₁ ∨ π₂⟧(b) = ⟦π₁⟧(b) ∨_𝔹 ⟦π₂⟧(b)
  ⟦¬π⟧(b)      = ¬_𝔹 ⟦π⟧(b)
  ⟦ε_Π⟧(b)     = ⊤
```

**Definition 2.5** (Effect Denotation)
```
⟦ε⟧ : 𝔾 → (𝔾 ⊎ Error)

Effect composition:
  ⟦ε₁ ; ε₂⟧(g) = match ⟦ε₁⟧(g) with
    | Left(err)  → Left(err)
    | Right(g')  → ⟦ε₂⟧(g')
```

**Theorem 2.1** (Compositionality)
```
∀π₁, π₂ : Π, b : Bindings.
  ⟦π₁ ∧ π₂⟧(b) = ⟦π₁⟧(b) ∧ ⟦π₂⟧(b)
```

**Proof**: Direct from Definition 2.4 ∎

---

## 3. Predicate Type System

### 3.1 ASK Predicates

**Type Signature**:
```
πₐₛₖ : SPARQL → Option[Bool] → Π
```

**Lambda Definition**:
```
πₐₛₖ(Q, expected) = λb : Bindings.
  let result = ⟦Q⟧(G) in          // Execute ASK query
  match expected with
    | None         → result
    | Some(exp)    → result = exp
```

**Denotational Semantics**:
```
⟦πₐₛₖ(Q, expected)⟧ : Bindings → 𝔹

⟦πₐₛₖ(Q, None)⟧(b) = ASK(Q, G)
⟦πₐₛₖ(Q, Some(v))⟧(b) = ASK(Q, G) = v
```

**Selectivity Analysis**:
```
selectivity(πₐₛₖ) = |{b ∈ B | ⟦πₐₛₖ⟧(b) = ⊤}| / |B|

Optimization:
  If selectivity < 0.1, prefer indexed lookups
  If selectivity > 0.9, skip evaluation (always true)
```

### 3.2 SHACL Predicates

**Type Signature**:
```
πₛₕₐᴄₗ : Shapes → Mode → Strict → Π

where
  Shapes = Set[Shape]
  Mode   = Conforms | Violates
  Strict = Bool
```

**Lambda Definition**:
```
πₛₕₐᴄₗ(S, mode, strict) = λb : Bindings.
  let report = validate(G, S) in
  let violations = {v ∈ report | severity(v) ≥ threshold} in
  match mode with
    | Conforms  → |violations| = 0
    | Violates  → |violations| > 0 ∧ (strict ⟹ fail_fast)
```

**Denotational Semantics**:
```
⟦πₛₕₐᴄₗ(S, Conforms, _)⟧(b) = conforms(G, S)
⟦πₛₕₐᴄₗ(S, Violates, strict)⟧(b) = ¬conforms(G, S) ∧ (strict ⟹ ⊥)
```

**Pruning Strategy**:
```
Early termination:
  For Conforms mode: stop on first violation
  For Violates mode: stop when k violations found
```

### 3.3 DELTA Predicates

**Type Signature**:
```
πᴅₑₗₜₐ : Bindings → Baseline → Keys → ChangeMode → Threshold → Π

where
  Baseline   = Map[Key, Hash]
  ChangeMode = Any | Increase | Decrease
  Threshold  = ℝ
```

**Lambda Definition**:
```
πᴅₑₗₜₐ(B, B_prev, K, change, δ) = λb : Bindings.
  let key = project(b, K) in
  let h_curr = H₂₅₆(canonical(b)) in
  let h_prev = lookup(B_prev, key) in
  match h_prev with
    | None    → false
    | Some(h) →
        let diff = |h_curr - h| / h in
        match change with
          | Any       → h_curr ≠ h
          | Increase  → diff > δ
          | Decrease  → diff < -δ
```

**Hash Function Properties**:
```
H₂₅₆ : Binding → {0,1}²⁵⁶

Properties:
  1. Deterministic: H(b) = H(b')  ⟺  b = b'
  2. Pre-image resistance: ∀h. hard to find b s.t. H(b) = h
  3. Collision resistance: hard to find b ≠ b' s.t. H(b) = H(b')
```

**Canonical Form**:
```
canonical : Binding → String
canonical(b) = sort([(k, v) | (k, v) ∈ b])
```

### 3.4 THRESHOLD Predicates

**Type Signature**:
```
πₜₕᵣ : Bindings → Var → Op → Value → Option[Agg] → Π

where
  Op  = LT | LE | EQ | GE | GT
  Agg = Sum | Avg | Count | Max | Min
```

**Lambda Definition**:
```
πₜₕᵣ(B, var, op, θ, agg) = λb : Bindings.
  let values = {b[var] | b ∈ B ∧ isNumeric(b[var])} in
  let v = match agg with
    | None        → ∃v ∈ values
    | Some(Sum)   → Σ values
    | Some(Avg)   → (Σ values) / |values|
    | Some(Count) → |values|
    | Some(Max)   → max(values)
    | Some(Min)   → min(values)
  in
  compare(v, op, θ)
```

**Comparison Semantics**:
```
compare : Value → Op → Value → Bool

compare(v, LT, θ) = v < θ
compare(v, LE, θ) = v ≤ θ
compare(v, EQ, θ) = v = θ
compare(v, GE, θ) = v ≥ θ
compare(v, GT, θ) = v > θ
```

### 3.5 COUNT Predicates

**Type Signature**:
```
πᴄₒᴜₙₜ : Bindings → Op → ℕ → Π
```

**Lambda Definition**:
```
πᴄₒᴜₙₜ(B, op, n) = λb : Bindings.
  compare(|B|, op, n)
```

**Denotational Semantics**:
```
⟦πᴄₒᴜₙₜ(B, op, n)⟧(b) = compare(|B|, op, n)
```

### 3.6 WINDOW Predicates

**Type Signature**:
```
πᴡɪɴᴅₒᴡ : Bindings → Var → Duration → Agg → Comparison → Π

where
  Duration   = Milliseconds
  Comparison = {op: Op, value: Value}
```

**Lambda Definition**:
```
πᴡɪɴᴅₒᴡ(B, var, size, agg, cmp) = λb : Bindings.
  let t_now = now() in
  let window = {b ∈ B | t_now - size ≤ b.timestamp < t_now} in
  let values = {b[var] | b ∈ window} in
  let result = aggregate(values, agg) in
  compare(result, cmp.op, cmp.value)
```

**Tumbling Window**:
```
Window(t₀, Δt) = {b ∈ B | t₀ ≤ b.timestamp < t₀ + Δt}

Non-overlapping:
  Window(t₀, Δt) ∩ Window(t₀ + Δt, Δt) = ∅
```

---

## 4. Complexity Analysis

### 4.1 Time Complexity Bounds

**Theorem 4.1** (ASK Predicate Complexity)
```
T(πₐₛₖ) = O(|G|)

With SPARQL optimizations:
  - Index-based: O(log |G|)
  - Selectivity-pruned: O(s·|G|) where s = selectivity
```

**Proof**:
ASK query requires full graph scan in worst case. With B-tree indexing on triple patterns, lookup reduces to O(log |G|). Selectivity pruning reduces effective graph size to s·|G|. ∎

**Theorem 4.2** (SHACL Predicate Complexity)
```
T(πₛₕₐᴄₗ) = O(|S| × |G|)

With pruning:
  - Early termination: O(k·|G|) where k = violations threshold
  - Shape caching: O(|S| + |G|) amortized
```

**Proof**:
Each shape S_i validates against entire graph G, yielding |S| × |G| operations. Early termination stops after k violations, reducing to k·|G|. Shape caching stores validation results for reuse. ∎

**Theorem 4.3** (DELTA Predicate Complexity)
```
T(πᴅₑₗₜₐ) = O(|B| log |B|)

Hash table implementation:
  - Lookup: O(1) expected, O(log |B|) worst case
  - Hash computation: O(|b|) per binding
  - Total: O(|B| × |b| + |B| log |B|)
```

**Proof**:
For each binding b ∈ B:
1. Project to key: O(|K|)
2. Hash computation: O(|b|)
3. Hash table lookup: O(1) expected
Total: O(|B| × (|K| + |b|)) = O(|B| log |B|) ∎

**Theorem 4.4** (THRESHOLD Predicate Complexity)
```
T(πₜₕᵣ) = O(|B|)

Aggregation analysis:
  - Sum/Count: O(|B|)
  - Avg: O(|B|)
  - Max/Min: O(|B|) with linear scan, O(|B| log |B|) for sorted
```

**Theorem 4.5** (COUNT Predicate Complexity)
```
T(πᴄₒᴜₙₜ) = O(1)

Cardinality is pre-computed during binding evaluation.
```

**Theorem 4.6** (WINDOW Predicate Complexity)
```
T(πᴡɪɴᴅₒᴡ) = O(|B|)

With time-based indexing:
  - Range query: O(log |B| + k) where k = window size
  - Aggregation: O(k)
  - Total: O(log |B| + k)
```

### 4.2 Space Complexity

**Theorem 4.7** (Space Bounds)
```
S(πₐₛₖ)     = O(1)              // Boolean result
S(πₛₕₐᴄₗ)   = O(|violations|)  // Validation report
S(πᴅₑₗₜₐ)   = O(|B_prev|)      // Baseline storage
S(πₜₕᵣ)     = O(|values|)      // Numeric values
S(πᴄₒᴜₙₜ)   = O(1)              // Cardinality
S(πᴡɪɴᴅₒᴡ)  = O(k)              // Window buffer
```

---

## 5. Receipt Provenance

### 5.1 Cryptographic Commitment Scheme

**Definition 5.1** (Receipt Type)
```
Receipt ≡ {
  id          : IRI,
  fired       : Bool,
  predicates  : [PredicateResult],
  durations   : Metrics,
  provenance  : Provenance,
  timestamp   : Timestamp,
  actor       : IRI
}
```

**Definition 5.2** (Provenance Type)
```
Provenance ≡ {
  hook_hash      : Hash,
  query_hash     : Hash,
  graph_hash     : Hash,
  baseline_hash  : Hash,
  receipt_hash   : Hash
}

where Hash = {0,1}²⁵⁶
```

**Definition 5.3** (Hash Commitment)
```
commit : Receipt → Hash
commit(R) = H₂₅₆(canonical(R))

Properties:
  1. Binding: commit(R) uniquely identifies R
  2. Hiding: R cannot be derived from commit(R)
  3. Collision-resistant: hard to find R ≠ R' with commit(R) = commit(R')
```

### 5.2 Digital Signature Scheme

**Definition 5.4** (Signature Type)
```
Signature ≡ {
  receipt_hash : Hash,
  signature    : {0,1}ᵏ,
  public_key   : {0,1}ⁿ,
  algorithm    : SignatureAlgorithm
}

where
  SignatureAlgorithm = Ed25519 | ECDSA | RSA
```

**Definition 5.5** (Signing Function)
```
sign : Receipt → PrivateKey → Signature
sign(R, sk) = {
  receipt_hash: commit(R),
  signature:    Sign_sk(commit(R)),
  public_key:   derive_pk(sk),
  algorithm:    Ed25519
}
```

**Definition 5.6** (Verification Function)
```
verify : Receipt → Signature → Bool
verify(R, σ) =
  Verify_pk(commit(R), σ.signature)
  ∧ commit(R) = σ.receipt_hash
```

**Theorem 5.1** (Non-Repudiation)
```
∀R : Receipt, sk : PrivateKey.
  let σ = sign(R, sk) in
  verify(R, σ) = ⊤
  ∧ ∀R' ≠ R. verify(R', σ) = ⊥
```

**Proof**: By Ed25519 correctness and collision resistance of H₂₅₆
```
verify(R, sign(R, sk))
  = Verify_pk(commit(R), Sign_sk(commit(R)))
  = ⊤                                          [Ed25519 correctness]

verify(R', sign(R, sk)) where R' ≠ R
  = Verify_pk(commit(R'), Sign_sk(commit(R)))
  = ⊥                                          [commit(R) ≠ commit(R')]
```
∎

### 5.3 Tamper-Evidence

**Definition 5.7** (Merkle Tree for Receipt Chain)
```
MerkleTree ≡ Tree[Hash]

root : MerkleTree → Hash
root(Leaf(h))         = h
root(Branch(l, r))    = H₂₅₆(root(l) ∥ root(r))

Receipt chain:
  receipts = [R₁, R₂, ..., Rₙ]
  tree     = build_merkle([commit(R₁), ..., commit(Rₙ)])
  root_hash = root(tree)
```

**Theorem 5.2** (Tamper Detection)
```
∀i. modify(receipts[i]) ⟹ root(tree) changes
```

**Proof**:
Modifying R_i changes commit(R_i), which propagates up the Merkle tree, changing root hash. ∎

---

## 6. Algebraic Laws

### 6.1 Boolean Algebra

**Theorem 6.1** (Distributivity)
```
π₁ ∧ (π₂ ∨ π₃) = (π₁ ∧ π₂) ∨ (π₁ ∧ π₃)
```

**Proof**:
```
⟦π₁ ∧ (π₂ ∨ π₃)⟧(b)
  = ⟦π₁⟧(b) ∧ ⟦π₂ ∨ π₃⟧(b)
  = ⟦π₁⟧(b) ∧ (⟦π₂⟧(b) ∨ ⟦π₃⟧(b))
  = (⟦π₁⟧(b) ∧ ⟦π₂⟧(b)) ∨ (⟦π₁⟧(b) ∧ ⟦π₃⟧(b))   [Boolean distributivity]
  = ⟦(π₁ ∧ π₂) ∨ (π₁ ∧ π₃)⟧(b)
```
∎

**Theorem 6.2** (De Morgan's Laws)
```
¬(π₁ ∧ π₂) = (¬π₁) ∨ (¬π₂)
¬(π₁ ∨ π₂) = (¬π₁) ∧ (¬π₂)
```

**Proof**: By λ-reduction
```
⟦¬(π₁ ∧ π₂)⟧(b)
  = ¬(⟦π₁⟧(b) ∧ ⟦π₂⟧(b))
  = (¬⟦π₁⟧(b)) ∨ (¬⟦π₂⟧(b))        [De Morgan]
  = ⟦(¬π₁) ∨ (¬π₂)⟧(b)
```
∎

### 6.2 Combinator Laws

**Theorem 6.3** (Threshold Monotonicity)
```
k₁ ≤ k₂ ⟹ thresholdₖ₂(π₁, ..., πₙ) ⟹ thresholdₖ₁(π₁, ..., πₙ)
```

**Proof**:
```
If |{i | πᵢ(b) = ⊤}| ≥ k₂, then |{i | πᵢ(b) = ⊤}| ≥ k₁ since k₁ ≤ k₂
```
∎

**Theorem 6.4** (Threshold Equivalences)
```
threshold₀(π₁, ..., πₙ) ≡ ⊤
threshold₁(π₁, ..., πₙ) ≡ π₁ ∨ ... ∨ πₙ
thresholdₙ(π₁, ..., πₙ) ≡ π₁ ∧ ... ∧ πₙ
```

**Proof**: By cardinality analysis
```
threshold₀: always satisfied (0 predicates needed)
threshold₁: at least one predicate true (disjunction)
thresholdₙ: all n predicates true (conjunction)
```
∎

### 6.3 Effect Algebra

**Theorem 6.5** (Effect Composition Associativity)
```
(ε₁ ; ε₂) ; ε₃ = ε₁ ; (ε₂ ; ε₃)
```

**Proof**: By monadic bind associativity in (𝔾 ⊎ Error) ∎

**Theorem 6.6** (Effect Identity)
```
ε ; ε_id = ε_id ; ε = ε
where ε_id = λg. Right(g)
```

**Proof**:
```
⟦ε ; ε_id⟧(g)
  = match ⟦ε⟧(g) with
    | Left(e)   → Left(e)
    | Right(g') → ⟦ε_id⟧(g')
  = match ⟦ε⟧(g) with
    | Left(e)   → Left(e)
    | Right(g') → Right(g')
  = ⟦ε⟧(g)
```
∎

---

## 7. Executable Lambda Calculus

### 7.1 Implementation in λ-Calculus

**ASK Predicate (Church Encoding)**:
```
πₐₛₖ = λQ. λexp. λb.
  let result = ask(Q)(graph) in
  if-then-else
    (is-none exp)
    result
    (eq result (unwrap exp))
```

**SHACL Predicate**:
```
πₛₕₐᴄₗ = λS. λmode. λstrict. λb.
  let report = validate(graph)(S) in
  let viols = filter(λv. severity(v) ≥ threshold)(report) in
  if-then-else
    (eq mode CONFORMS)
    (eq (length viols) 0)
    (and (gt (length viols) 0) (implies strict fail-fast))
```

**DELTA Predicate**:
```
πᴅₑₗₜₐ = λB. λB_prev. λK. λchange. λδ. λb.
  let key = project(b)(K) in
  let h_curr = hash(canonical(b)) in
  let h_prev = lookup(B_prev)(key) in
  match h_prev with
    | None   → false
    | Some h →
        let diff = div (abs (sub h_curr h)) h in
        match change with
          | ANY      → neq h_curr h
          | INCREASE → gt diff δ
          | DECREASE → lt diff (neg δ)
```

### 7.2 Y-Combinator for Recursive Predicates

**Definition 7.1** (Fixed-Point Combinator)
```
Y = λf. (λx. f (x x)) (λx. f (x x))

Recursive predicate:
  πᵣₑc = Y (λf. λb. ... f ... )
```

**Example: Recursive SHACL Validation**:
```
validate_recursive = Y (λvalidate. λshape. λnode.
  if-then-else
    (has-children shape)
    (and
      (validate shape node)
      (all (λchild. validate child node) (children shape)))
    (validate shape node))
```

---

## 8. Type Safety

### 8.1 Type Preservation

**Theorem 8.1** (Type Preservation)
```
If ⊢ π : Π and ⊢ b : Bindings, then ⊢ π(b) : Bool
```

**Proof**: By structural induction on π
- Base case: π = πₐₛₖ(Q, exp) has type Π by Definition 3.1
- Inductive case: If π₁, π₂ : Π, then (π₁ ∧ π₂) : Π by Definition 1.5
∎

**Theorem 8.2** (Progress)
```
If ⊢ π : Π and ⊢ b : Bindings, then ∃v : Bool. π(b) ⇓ v
```

**Proof**: By strong normalization of simply-typed λ-calculus ∎

---

## 9. Summary

This chapter formalized Knowledge Hooks as:

1. **Predicate Algebra**: Monoid structure with compositional semantics
2. **Type System**: 6 predicate types with λ-calculus definitions
3. **Complexity Bounds**: Formal analysis with optimization strategies
4. **Cryptographic Provenance**: Non-repudiation via digital signatures
5. **Algebraic Laws**: Distributivity, De Morgan, effect composition

**Key Results**:
- Predicates form a Boolean algebra (Theorems 6.1-6.2)
- Type preservation and progress (Theorems 8.1-8.2)
- Non-repudiation via Ed25519 signatures (Theorem 5.1)
- Complexity bounds: O(|G|) to O(|B| log |B|) (Theorems 4.1-4.6)

**Next Chapter**: Chapter 5 formalizes the Dark Matter Engine with operational semantics and proof of correctness.

---

## References

1. Pierce, B. C. (2002). *Types and Programming Languages*. MIT Press.
2. Baader, F., et al. (2003). *The Description Logic Handbook*. Cambridge.
3. Knublauch, H., & Kontokostas, D. (2017). *Shapes Constraint Language (SHACL)*. W3C Recommendation.
4. Harris, S., & Seaborne, A. (2013). *SPARQL 1.1 Query Language*. W3C Recommendation.
5. Boneh, D., & Shoup, V. (2020). *A Graduate Course in Applied Cryptography*. Cambridge.
