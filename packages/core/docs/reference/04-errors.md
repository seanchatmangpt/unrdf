# Errors Reference: @unrdf/core

Common errors, their causes, and solutions.

---

## SPARQL Syntax Errors

### "Unexpected token at position X"

**Cause:** Malformed SPARQL

**Common Issues:**
- Missing WHERE keyword
- Missing dot at end of pattern
- Unmatched braces
- Invalid prefix syntax

**Solution:**
```sparql
# ❌ ERROR: Missing WHERE
SELECT ?name {
  ?x foaf:name ?name .
}

# ✅ FIXED:
SELECT ?name WHERE {
  ?x foaf:name ?name .
}
```

### "Unknown variable ?X"

**Cause:** Variable used in SELECT not in WHERE

**Solution:**
```sparql
# ❌ ERROR:
SELECT ?name ?age WHERE {
  ?x foaf:name ?name .
}  # ?age not defined!

# ✅ FIXED:
SELECT ?name ?age WHERE {
  ?x foaf:name ?name ;
     foaf:age ?age .
}
```

### "Invalid IRI format"

**Cause:** IRI not in angle brackets

**Solution:**
```sparql
# ❌ ERROR:
SELECT ?x WHERE { ?x foaf:name "Alice" }

# ✅ FIXED:
SELECT ?x WHERE {
  ?x <http://xmlns.com/foaf/0.1/name> "Alice"
}
```

---

## Execution Errors

### "Query timeout"

**Cause:** Query took too long

**Solution:**
1. Add LIMIT to result set
2. Simplify query
3. Use filters earlier
4. Increase timeout:
   ```javascript
   executeQuerySync(store, query, { timeout: 30000 })
   ```

### "Cannot bind variable"

**Cause:** Variable can't take on requested value

**Solution:**
```sparql
# ❌ ERROR: Multiple values for same variable
SELECT ?x WHERE {
  ?x foaf:name "Alice" .
  ?x foaf:name "Bob" .
}

# ✅ FIXED: Use UNION if alternatives intended
SELECT ?x WHERE {
  { ?x foaf:name "Alice" }
  UNION
  { ?x foaf:name "Bob" }
}
```

### "Type error: cannot convert X to Y"

**Cause:** Data type mismatch

**Solution:**
```sparql
# ❌ ERROR: age might be string
FILTER (?age > 30)

# ✅ FIXED: Explicit conversion
FILTER (xsd:integer(?age) > 30)
```

### "Invalid function call"

**Cause:** Function doesn't exist or wrong arguments

**Solution:**
```sparql
# ❌ ERROR: CONTAINS takes 2 args
FILTER (CONTAINS(?x, ?y, ?z))

# ✅ FIXED:
FILTER (CONTAINS(?x, "substring"))
```

---

## Store Errors

### "Store is full"

**Cause:** Reached maxSize limit

**Solution:**
```javascript
// Option 1: Remove old quads
store.removeQuad(oldQuad);

// Option 2: Increase limit
const store = createUnrdfStore({
  maxSize: 10000000  // Increase size
});

// Option 3: Enable autoGC
const store = createUnrdfStore({
  autoGC: true  // Remove old quads automatically
});
```

### "Invalid quad: subject must be NamedNode or BlankNode"

**Cause:** Quad has literal as subject

**Solution:**
```javascript
// ❌ ERROR:
store.addQuad({
  subject: literal("Alice"),  // Invalid!
  predicate: namedNode('...'),
  object: namedNode('...')
});

// ✅ FIXED:
store.addQuad({
  subject: namedNode('http://example.com/alice'),
  predicate: namedNode('...'),
  object: namedNode('...')
});
```

### "Invalid quad: predicate must be NamedNode"

**Cause:** Predicate is not a NamedNode

**Solution:**
```javascript
// ❌ ERROR:
store.addQuad({
  subject: namedNode('...'),
  predicate: blankNode('b1'),  // Invalid!
  object: namedNode('...')
});

// ✅ FIXED:
store.addQuad({
  subject: namedNode('...'),
  predicate: namedNode('http://xmlns.com/foaf/0.1/knows'),
  object: namedNode('...')
});
```

---

## Logic Errors (No Error Thrown)

### Query returns no results when expected

**Diagnosis:**
```javascript
// Check: Is store empty?
console.log(`Store size: ${store.size}`);

// Check: Is data there?
const allQuads = store.match();
console.log(`All quads: ${allQuads.length}`);

// Check: URI case sensitivity
const query1 = 'SELECT ?x WHERE { ?x foaf:name "alice" }';
const query2 = 'SELECT ?x WHERE { ?x foaf:name "Alice" }';
// These might return different results!
```

**Solution:** Verify exact URIs and values

### Query returns unexpected results

**Check:**
```sparql
# Verify pattern matches what you expect
SELECT ?x ?y WHERE {
  ?x ?y ?z .
}
LIMIT 10
```

Look for:
- Case mismatches
- Wrong namespace URIs
- Typos in property names
- Missing relationships

### Duplicate results

**Cause:** Same match found multiple ways

**Solution:**
```sparql
# Add DISTINCT
SELECT DISTINCT ?name WHERE {
  { ?x foaf:name ?name }
  UNION
  { ?x foaf:knows ?y . ?y foaf:name ?name }
}
```

---

## Performance Issues (Not Errors)

### Slow queries

**Symptoms:** Query takes >100ms

**Solutions:**
1. Add LIMIT to limit results
2. Filter early in pattern
3. Avoid REGEX, use CONTAINS
4. Check indexing strategy

See "optimize-sparql-queries" how-to guide.

### Memory issues

**Symptoms:** Out of memory, process crashes

**Solutions:**
1. Stream data instead of loading all
2. Set maxSize limit
3. Reduce dataset
4. Process in batches

See "performance-tuning" how-to guide.

---

## Debug Checklist

When something goes wrong:

- [ ] Read error message completely
- [ ] Check SPARQL syntax (valid query?)
- [ ] Check data exists (is store empty?)
- [ ] Verify URIs match exactly (case-sensitive)
- [ ] Simplify query (does simpler version work?)
- [ ] Check variable bindings (all SELECT vars in WHERE?)
- [ ] Look for type errors (is data the expected type?)
- [ ] Review filter logic (is FILTER too restrictive?)
- [ ] Enable logging (console.log patterns)

---

## Error Codes Reference

| Code | Meaning | Check |
|------|---------|-------|
| SYNTAX | SPARQL parse error | Query syntax |
| TYPE | Type mismatch | Data types |
| UNBOUND | Variable not bound | SELECT/WHERE match |
| CONVERT | Cannot convert value | Type casting |
| TIMEOUT | Query too slow | Query optimization |
| FULL | Store size exceeded | Increase maxSize |
| INVALID | Invalid quad | Subject/predicate types |

---

## Getting Help

When reporting an error, include:

1. **Exact error message**
2. **Minimal reproducible example:**
   ```javascript
   const store = createUnrdfStore();
   store.addQuad({ ... });
   const result = executeQuerySync(store, '...');
   // Error occurs here
   ```
3. **What you expected vs what you got**
4. **What you've tried to fix it**

---

## Next Reading

- **troubleshooting** (How-To) - Diagnosis and debugging
- **API.md** (Reference) - Function signatures
- **CONFIGURATION.md** (Reference) - Configuration options
