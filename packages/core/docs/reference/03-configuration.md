# Configuration Reference: @unrdf/core

Configuration options for stores and queries.

---

## Store Configuration

### UnrdfStoreOptions

```javascript
const store = createUnrdfStore({
  indexing: 'predicate',    // 'predicate' | 'object' | 'none'
  maxSize: 1000000,         // Max quads in memory
  autoGC: true              // Enable garbage collection
});
```

### Indexing Strategies

| Strategy | Best For | Speed | Memory |
|----------|----------|-------|--------|
| **predicate** | Type queries, property lookups | Fast | Low |
| **object** | Finding values, literals | Medium | Medium |
| **none** | Memory-critical apps | Slow | Lowest |

**Default:** `'predicate'`

### maxSize

Maximum number of quads allowed in store.

- Default: `Infinity` (unlimited)
- When exceeded: Store rejects new quads with error
- Use for: Memory-limited environments

### autoGC

Automatic garbage collection on store full.

- Default: `true`
- When `true`: Old quads removed to make space
- When `false`: Raises error when full

---

## Query Configuration

### QueryOptions

```javascript
executeQuerySync(store, query, {
  timeout: 5000,              // Max milliseconds
  base: 'http://example.com/' // Base IRI
});
```

### timeout

Maximum query execution time.

- Unit: milliseconds
- Default: `Infinity` (no limit)
- When exceeded: Throws error
- Use for: Preventing runaway queries

### base

Base IRI for resolving relative references.

- Default: No base
- Format: Full IRI
- Use for: Relative URI resolution in SPARQL

---

## Format Configuration

### Parsing Options

For custom parsers (not in core, but important):

| Option | Type | Default | Purpose |
|--------|------|---------|---------|
| validate | boolean | true | Validate RDF conformance |
| baseIRI | string | '' | Base for relative IRIs |
| encoding | string | 'utf-8' | Text encoding |
| streaming | boolean | false | Process line-by-line |

---

## Performance Configuration

### Recommended Settings

**For small graphs (<100K quads):**
```javascript
const store = createUnrdfStore({
  indexing: 'predicate'
});
```

**For large graphs (>1M quads):**
```javascript
const store = createUnrdfStore({
  indexing: 'predicate',
  maxSize: 10000000,
  autoGC: true
});
```

**For memory-critical (limited RAM):**
```javascript
const store = createUnrdfStore({
  indexing: 'none',
  autoGC: true
});
```

**For query performance:**
```javascript
const store = createUnrdfStore({
  indexing: 'predicate'  // Fastest for common queries
});

executeQuerySync(store, query, {
  timeout: 10000  // 10 second limit
});
```

---

## Environment Variables

Configure via environment:

```bash
# Node.js
NODE_OPTIONS="--max-old-space-size=4096" node app.js
```

```javascript
// Check in code
if (process.env.NODE_ENV === 'production') {
  store = createUnrdfStore({ indexing: 'predicate' });
} else {
  store = createUnrdfStore({ indexing: 'none' });
}
```

---

## Default Values Summary

| Setting | Default | Min | Max |
|---------|---------|-----|-----|
| indexing | 'predicate' | - | - |
| maxSize | Infinity | 1 | Infinity |
| autoGC | true | - | - |
| timeout | Infinity | 0 | Infinity |

---

## Configuration Examples

### Development Setup

```javascript
const store = createUnrdfStore({
  indexing: 'predicate',
  autoGC: false  // See memory leaks immediately
});
```

### Production Setup

```javascript
const store = createUnrdfStore({
  indexing: 'predicate',
  maxSize: 50000000,  // 50M quads max
  autoGC: true
});
```

### Testing Setup

```javascript
const store = createUnrdfStore({
  indexing: 'none',  // No overhead
  autoGC: true
});
```

### Data Import Setup

```javascript
// Batch import with high limits
const store = createUnrdfStore({
  maxSize: Infinity,
  autoGC: false
});

// Then apply limits after import
store.maxSize = 10000000;
store.autoGC = true;
```

---

## Advanced Tuning

### Memory Pressure

When low on memory:
1. Set `indexing: 'none'`
2. Set `maxSize` to available RAM / 2
3. Process data in batches
4. Enable `autoGC: true`

### Query Timeout Tuning

Too strict (queries fail):
```javascript
{ timeout: 1000 }   // Too low
```

Appropriate:
```javascript
{ timeout: 10000 }  // 10 seconds
```

Too loose (runaway queries):
```javascript
{ timeout: Infinity }  // No limit
```

---

## Validation

Check configuration:

```javascript
function validateConfig(config) {
  const errors = [];

  if (config.indexing && !['predicate', 'object', 'none'].includes(config.indexing)) {
    errors.push('Invalid indexing strategy');
  }

  if (config.maxSize && config.maxSize < 1) {
    errors.push('maxSize must be >= 1');
  }

  if (config.timeout && config.timeout < 0) {
    errors.push('timeout cannot be negative');
  }

  return errors;
}
```

---

## Next Reading

- **API.md** (Reference) - Available functions
- **ERRORS.md** (Reference) - Error codes and solutions
- **performance-tuning** (How-To) - Optimization techniques
