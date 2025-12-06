# Query Execution: @unrdf/core

How queries actually run.

---

## Query Execution Pipeline

```
Input SPARQL
    ↓
1. Parse (check syntax)
    ↓
2. Plan (figure out order)
    ↓
3. Execute (match patterns)
    ↓
4. Filter (apply FILTER clauses)
    ↓
5. Aggregate (GROUP BY, COUNT, etc.)
    ↓
6. Sort (ORDER BY)
    ↓
7. Limit (LIMIT/OFFSET)
    ↓
Output Results
```

---

## Phase 1: Parse

**Input:**
```sparql
SELECT ?name WHERE {
  ?person foaf:name ?name .
  ?person foaf:age ?age .
  FILTER (?age > 30)
}
```

**Validates:**
- Syntax is correct (matching braces, etc.)
- Keywords are spelled right (SELECT, WHERE, etc.)
- Variables start with `?`
- IRIs in angle brackets

**Output:** Parsed query structure

**Time:** 1-2ms

**Errors:** SPARQL syntax errors → thrown immediately

---

## Phase 2: Plan

**Figure out execution order:**

```
Pattern 1: ?person foaf:name ?name
Pattern 2: ?person foaf:age ?age
Filter: ?age > 30
```

**Strategy:** Which pattern first?

Option A:
1. Find all people with names → binds ?person, ?name
2. For each, check if they have age → binds ?age
3. Filter by ?age > 30

Option B:
1. Find all people with ages → binds ?person, ?age
2. For each, get their name → binds ?name
3. Filter by ?age > 30

**Choice:** Start with most selective pattern.

If predicate `foaf:age` is less common than `foaf:name`, start with age (reduces working set).

**Time:** <1ms (simple heuristics)

---

## Phase 3: Execute Patterns

Execute pattern matching in planned order.

### Pattern Matching

```javascript
// Pattern: ?person foaf:name ?name
// Store: [
//   { s: alice, p: foaf:name, o: "Alice" },
//   { s: bob, p: foaf:name, o: "Bob" },
//   { s: alice, p: foaf:age, o: "30" },
// ]

// Using index on predicate:
// Find all quads where p = foaf:name
// Result: [
//   { s: alice, o: "Alice", binding: { person: alice, name: "Alice" } },
//   { s: bob, o: "Bob", binding: { person: bob, name: "Bob" } },
// ]
```

**Time:** O(n) where n = quads with that predicate

### Join Patterns

```javascript
// Pattern 1 results:
// [{ person: alice, name: "Alice" }, { person: bob, name: "Bob" }]

// Pattern 2: ?person foaf:age ?age
// Apply to each result, looking for quads with person=alice, person=bob
// Result:
// [
//   { person: alice, name: "Alice", age: "30" },
//   { person: bob, name: "Bob", age: "25" },
// ]
```

**Time:** O(n*m) where n = results from pattern 1, m = quads with that pattern

---

## Phase 4: Filter

Apply FILTER clauses:

```sparql
FILTER (?age > 30)
```

For each binding, evaluate:
- alice: age="30" → 30 > 30 → false → remove
- bob: age="25" → 25 > 30 → false → remove

**Result:** Empty (no one matches all filters)

**Time:** O(k) where k = remaining bindings

---

## Phase 5: Aggregate

If GROUP BY or aggregate functions:

```sparql
SELECT (COUNT(?person) AS ?count)
WHERE { ... }
GROUP BY ?age
```

Group results:
```
Age 30: [alice]          → count = 1
Age 25: [bob]            → count = 1
```

**Time:** O(k) where k = distinct groups

---

## Phase 6: Sort

Apply ORDER BY:

```sparql
ORDER BY ?name
```

Sort results:
```
alice (name="Alice")
bob (name="Bob")
```

**Time:** O(k log k) where k = results (acceptable for LIMIT case)

---

## Phase 7: Limit

Apply LIMIT/OFFSET:

```sparql
LIMIT 10 OFFSET 20
```

Return results 20-29.

**Time:** O(1) (just array slice)

---

## Example: Full Execution

**Query:**
```sparql
SELECT ?name WHERE {
  ?person foaf:age ?age .
  ?person foaf:name ?name .
  FILTER (?age > 30)
}
```

**Store (3 quads):**
```
alice foaf:age "35"
alice foaf:name "Alice"
bob foaf:age "25"
bob foaf:name "Bob"
```

**Execution:**

1. **Parse:** OK
2. **Plan:** Start with age (more selective)
3. **Execute Pattern 1:** Find foaf:age
   - Results: [{ person: alice, age: "35" }, { person: bob, age: "25" }]
4. **Execute Pattern 2:** Find foaf:name for each
   - Results: [
       { person: alice, age: "35", name: "Alice" },
       { person: bob, age: "25", name: "Bob" }
     ]
5. **Filter:** ?age > 30
   - alice: 35 > 30 ✓
   - bob: 25 > 30 ✗
   - Results: [{ person: alice, age: "35", name: "Alice" }]
6. **Order:** (none)
7. **Limit:** (none)
8. **Output:** [{ name: Literal("Alice") }]

---

## Cost Estimation

### Simple Query (1 pattern)

```sparql
SELECT ?name WHERE { ?x foaf:name ?name }
```

- Pattern match: O(q) where q = quads with foaf:name
- Total: ~5-10ms

### JOIN Query (2 patterns)

```sparql
SELECT ?name WHERE {
  ?person foaf:age ?age .
  ?person foaf:name ?name .
}
```

- Pattern 1: O(q₁)
- Pattern 2: O(r₁ * q₂) where r₁ = results from pattern 1
- Total: ~20-50ms

### Complex JOIN (3+ patterns)

```sparql
SELECT ?name WHERE {
  ?x rdf:type foaf:Person .
  ?x foaf:knows ?y .
  ?y foaf:name ?name .
}
```

- Pattern 1: O(q₁)
- Pattern 2: O(r₁ * q₂)
- Pattern 3: O(r₂ * q₃)
- Total: ~100-500ms (depends on selectivity)

---

## Optimization Strategies

### 1. Start with Selective Pattern

```sparql
# ❌ SLOW: Start with foaf:name (many matches)
?person foaf:name ?name .
?person foaf:age ?age .
FILTER (?age > 65)

# ✅ FAST: Start with foaf:age (filtered)
?person foaf:age ?age .
FILTER (?age > 65)
?person foaf:name ?name .
```

### 2. Filter Early

```sparql
# ❌ SLOW: Filter after JOIN
?person foaf:age ?age .
?person foaf:name ?name .
FILTER (?age > 30)

# ✅ FAST: Filter within pattern
?person foaf:age ?age .
FILTER (?age > 30)
?person foaf:name ?name .
```

### 3. Use Concrete IRIs

```sparql
# ❌ SLOW: Pattern matches any person
?person foaf:knows ?friend .

# ✅ FAST: Only alice
<http://example.com/alice> foaf:knows ?friend .
```

---

## Memory Usage

```javascript
// One binding per result
const binding = {
  person: NamedNode,
  name: Literal,
  age: Literal
};
// ~300 bytes per binding (rough estimate)

// For 100,000 results:
// 100,000 * 300 = 30 MB
```

**Strategy:** Use LIMIT to cap results.

---

## Lazy Evaluation?

@unrdf/core does **not** use lazy evaluation:

```javascript
// All results computed immediately
const results = executeQuerySync(store, query);
// Could have 1 million results in array!
```

**Why?** Simpler implementation, fits with synchronous API.

**Trade-off:** Use LIMIT for large result sets.

---

## Key Performance Insight

**80% of execution time = pattern matching (finding quads in store)**

**20% of time = everything else (joining, filtering, sorting)**

Optimization should focus on reducing quads examined in patterns.

---

## Next Reading

- **optimize-sparql-queries** (How-To) - Apply optimization strategies
- **performance-tuning** (How-To) - System-level optimization
- **architecture** (Explanation) - How indexing enables fast matching
