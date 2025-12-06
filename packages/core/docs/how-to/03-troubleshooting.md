# How To: Troubleshoot Common Issues

**Time estimate:** 5-7 hours
**Difficulty:** Beginner to Intermediate
**Context:** Something isn't working and you need to fix it

---

## Diagnosis Flowchart

```
Problem
├─ "No results" → Check query and data
├─ "Unexpected results" → Verify URIs and logic
├─ "Error message" → Find error type below
├─ "Slow query" → See optimization guide
└─ "Memory issues" → Reduce dataset size
```

---

## Issue: Query Returns No Results

### Diagnosis

1. **Check the data exists:**
```javascript
const allQuads = Array.from(store.match());
console.log(`Store contains ${allQuads.length} quads`);
console.log('Sample quads:');
allQuads.slice(0, 5).forEach(q => {
  console.log(`  ${q.subject.value} - ${q.predicate.value}`);
});
```

2. **Check URIs match exactly:**
```javascript
// Your data might have:
// http://example.com/Alice (capital A)

// But you query:
// <http://example.com/alice> (lowercase a)
// ❌ MISMATCH - no results!
```

3. **Simplify the query:**
```javascript
// Start with:
const simple = `SELECT ?x WHERE { ?x ?p ?o }`;
const results = executeQuerySync(store, simple);
console.log(`Found ${results.length} results`);

// If 0 results, store is empty!
// If >0 results, store has data, move to next step
```

### Solution

```javascript
// Debug query: Print matched bindings
const debugQuery = `
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  SELECT ?s ?p ?o {
    ?s ?p ?o .
  }
  LIMIT 10
`;

const results = executeQuerySync(store, debugQuery);
console.log('First 10 quads in store:');
results.forEach(r => {
  console.log(`  ${r.s.value} ${r.p.value} ${r.o.value}`);
});
```

---

## Issue: Unexpected Query Results

### Problem: Wrong Variables Selected

```sparql
# ❌ WRONG: Returns blank node IDs, not useful
SELECT ?id WHERE {
  ?person foaf:name "Alice" .
  BIND(?person AS ?id)
}

# ✅ RIGHT: Returns readable names
SELECT ?name WHERE {
  ?person foaf:name ?name .
  FILTER (CONTAINS(?name, "Alice"))
}
```

### Problem: Case Sensitivity

```sparql
# ❌ Doesn't match "Alice"
FILTER (?name = "alice")

# ✅ Case-insensitive match
FILTER (REGEX(?name, "^alice$", "i"))
```

### Problem: Missing Joins

```sparql
# ❌ Returns people AND all companies (cartesian product)
SELECT ?name ?company WHERE {
  ?person foaf:name ?name .
  ?org foaf:name ?company .
}

# ✅ Proper join
SELECT ?name ?company WHERE {
  ?person foaf:name ?name ;
          workAt ?org .
  ?org foaf:name ?company .
}
```

### Problem: Type Mismatch

```sparql
# ❌ Age is string, comparison fails
SELECT ?person WHERE {
  ?person foaf:age ?age .
  FILTER (?age > 30)
}

# ✅ Cast to integer
SELECT ?person WHERE {
  ?person foaf:age ?age .
  FILTER (xsd:integer(?age) > 30)
}
```

---

## Issue: Error Messages

### "Variable not found"

```
Error: Variable ?unknown is not bound
```

**Cause:** Using variable in SELECT that's not in WHERE clause

```sparql
# ❌ ERROR: ?companyName not selected
SELECT ?name ?companyName WHERE {
  ?person foaf:name ?name .
}

# ✅ FIXED: Include the join
SELECT ?name ?companyName WHERE {
  ?person foaf:name ?name ;
          workAt ?company .
  ?company foaf:name ?companyName .
}
```

### "Syntax error in SPARQL"

```
Error: Unexpected token at position 45
```

**Cause:** Malformed SPARQL

```sparql
# ❌ Missing WHERE keyword
SELECT ?name {
  ?person foaf:name ?name .
}

# ✅ Correct syntax
SELECT ?name WHERE {
  ?person foaf:name ?name .
}
```

Check for:
- Missing dots at end of patterns
- Unmatched braces { }
- Invalid URIs (missing angle brackets <...>)
- Typos in keywords (SELCT instead of SELECT)

### "Cannot convert to integer"

```
Error: Cannot cast "text" to xsd:integer
```

**Cause:** Data is text, not number

```sparql
# ❌ Age might be stored as string
FILTER (xsd:integer(?age) > 30)

# ✅ Check type first
FILTER (DATATYPE(?age) = xsd:integer && ?age > 30)

# ✅ Or handle both
FILTER (!BOUND(?age) || xsd:integer(?age) > 30)
```

### "Graph doesn't exist"

```
Error: Named graph not found
```

**Cause:** Querying graph that's not loaded

```sparql
# ❌ Graph specified but not in store
SELECT ?s WHERE {
  GRAPH <http://example.com/graph1> {
    ?s ?p ?o .
  }
}

# ✅ Query default graph
SELECT ?s WHERE {
  ?s ?p ?o .
}
```

---

## Issue: Memory Overflow

### Symptoms
- "Out of memory" error
- Node process crashes
- System becomes unresponsive

### Causes
- Loading multi-gigabyte file into memory
- Cartesian product (millions × millions matches)
- Memory leak (same query run repeatedly)

### Solutions

1. **Use streaming:**
```javascript
// ❌ BAD: Load entire file
const data = fs.readFileSync('huge.nt', 'utf-8');
const quads = parseNTriples(data);

// ✅ GOOD: Stream and process incrementally
const readline = require('readline');
const stream = fs.createReadStream('huge.nt');
const rl = readline.createInterface({ input: stream });

rl.on('line', (line) => {
  const quad = parseNTriplesLine(line);
  store.addQuad(quad);
});
```

2. **Limit result sets:**
```sparql
# ❌ Might return millions
SELECT ?x WHERE { ?x ?p ?o }

# ✅ Always use LIMIT
SELECT ?x WHERE { ?x ?p ?o }
LIMIT 1000
```

3. **Process in batches:**
```javascript
const batchSize = 10000;
for (let i = 0; i < allQuads.length; i += batchSize) {
  const batch = allQuads.slice(i, i + batchSize);
  processBatch(batch);

  // Allow garbage collection
  if (global.gc) global.gc();
}
```

---

## Issue: Slow Queries

See "optimize-sparql-queries" how-to guide. Quick checklist:

- [ ] Add LIMIT to result set
- [ ] Filter before joining
- [ ] Avoid REGEX, use CONTAINS
- [ ] Reduce OPTIONAL clauses
- [ ] Check pattern ordering

---

## Issue: Data Consistency

### Problem: Duplicates

```sparql
# Might return same person twice
SELECT ?name WHERE {
  ?person foaf:name ?name ;
          foaf:knows ?friend .
}
UNION {
  ?friend foaf:knows ?person ;
          foaf:name ?name .
}
```

**Solution:** Add DISTINCT
```sparql
SELECT DISTINCT ?name WHERE { ... }
```

### Problem: Stale Data

After adding data, old queries might not reflect it:

```javascript
// Add new quad
store.addQuad(newQuad);

// Query immediately - should include new data
const results = executeQuerySync(store, query);
```

If data doesn't appear:
- Check quad was added (use `store.match()`)
- Verify URI spelling/case
- Ensure query filter isn't excluding it

---

## Debugging Tools

### Console Logging

```javascript
const query = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name ?age WHERE {
    ?person foaf:name ?name .
    ?person foaf:age ?age .
  }
`;

const results = executeQuerySync(store, query);
console.log('Results:');
results.forEach((binding, idx) => {
  console.log(`${idx + 1}. ${binding.name.value} (${binding.age.value})`);
});
```

### Extract and Inspect Individual Quads

```javascript
function findQuads(store, pattern) {
  const results = executeQuerySync(store, `
    SELECT ?s ?p ?o WHERE {
      ?s ?p ?o .
    }
  `);

  return results.filter(r => {
    return Object.values(r).some(v =>
      v.value.includes(pattern)
    );
  });
}

// Find all quads mentioning "Alice"
const aliceQuads = findQuads(store, 'alice');
console.log('Quads about Alice:');
aliceQuads.forEach(q => {
  console.log(`  ${q.s.value} ${q.p.value} ${q.o.value}`);
});
```

### Validate Data

```javascript
function validateData(store) {
  const issues = [];

  // Check for dangling references
  const people = executeQuerySync(store, `
    SELECT DISTINCT ?person WHERE {
      ?person a foaf:Person .
    }
  `);

  people.forEach(p => {
    const details = executeQuerySync(store, `
      SELECT ?property ?value WHERE {
        <${p.person.value}> ?property ?value .
      }
    `);

    if (details.length === 0) {
      issues.push(`Person ${p.person.value} has no properties`);
    }
  });

  return issues;
}

const issues = validateData(store);
if (issues.length) {
  console.log('Data validation issues:');
  issues.forEach(issue => console.log(`  - ${issue}`));
}
```

---

## Common Mistakes Checklist

- [ ] URIs have mismatched case
- [ ] Missing angle brackets around URIs: `<...>`
- [ ] Missing quotes around string literals: `"..."`
- [ ] Missing dots after patterns: `?x ?p ?o .`
- [ ] Trying to query empty store
- [ ] Using undefined variables in SELECT
- [ ] OPTIONAL with no content
- [ ] Comparing different types (string vs number)
- [ ] Not using DISTINCT with UNION
- [ ] Expecting results from FILTER that's too restrictive

---

## When to Ask for Help

Provide:
1. **Minimal reproducible example**
   ```javascript
   const store = createUnrdfStore();
   store.addQuad({ /* ... */ });
   const result = executeQuerySync(store, `...`);
   console.log(result);  // What you get
   console.log('Expected: [...]');  // What you expected
   ```

2. **Error message or unexpected output**

3. **What you've tried**
   - Simplified queries
   - Checked data exists
   - Verified URIs

---

## Next Reading

- **optimize-sparql-queries** (How-To) - Make slow queries fast
- **query-execution** (Explanation) - Understand how queries work
- **api.md** (Reference) - API function details
