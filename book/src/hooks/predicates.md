# Predicates Overview

Predicates are the **condition types** that determine when a Knowledge Hook triggers. UNRDF supports multiple predicate types, each optimized for different use cases.

## Predicate Types

| Type | Purpose | Returns | Use Case |
|------|---------|---------|----------|
| **ASK** | Boolean condition | `true\|false` | Existence checks, validation |
| **SELECT** | Result bindings | `Array<Object>` | Data queries, analysis |
| **SHACL** | Shape validation | `{conforms, results}` | Schema validation |
| **DELTA** | Change detection | `{changed}` | Reactive updates |
| **THRESHOLD** | Numeric comparison | `{satisfied, value}` | Limits, quotas |
| **COUNT** | Cardinality | `{count, satisfied}` | Size limits |
| **WINDOW** | Time-based | `{count, duration}` | Rate limiting |

## How Predicates Work

Predicates define the `when` clause of a hook:

```javascript
defineHook({
  meta: { name: 'my-hook' },

  // Predicate: defines trigger condition
  when: {
    kind: 'sparql-ask',  // Predicate type
    query: 'ASK { ... }' // Condition logic
  },

  // Effect: executes when predicate is satisfied
  run: async (event) => {
    // event.result contains predicate evaluation result
  }
});
```

## Inline vs. File-Based Predicates

### Inline Queries (Development)

For rapid development and testing:

```javascript
when: {
  kind: 'sparql-ask',
  query: `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    ASK {
      ?person a foaf:Person .
      FILTER NOT EXISTS { ?person foaf:name ?name }
    }
  `
}
```

✅ **Pros:** Quick to write, easy to debug
❌ **Cons:** Not versioned, no cryptographic proof

### File-Based References (Production)

For production systems:

```javascript
when: {
  kind: 'sparql-ask',
  ref: {
    uri: 'file://hooks/compliance/person-name-required.ask.rq',
    sha256: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
    mediaType: 'application/sparql-query'
  }
}
```

✅ **Pros:** Versioned, content-addressed, cryptographic proof
❌ **Cons:** Requires file management

```admonish info
UNRDF supports both approaches. Use inline queries for development, migrate to file-based references for production.
```

## Combining Predicates

### Sequential Evaluation (AND)

Use multiple hooks for AND logic:

```javascript
// Hook 1: Check age
defineHook({
  meta: { name: 'check-age' },
  when: {
    kind: 'sparql-ask',
    query: 'ASK { ?person ex:age ?age . FILTER (?age >= 18) }'
  },
  run: async (event) => {
    if (!event.result) throw new Error('Must be 18+');
  }
});

// Hook 2: Check consent
defineHook({
  meta: { name: 'check-consent' },
  when: {
    kind: 'sparql-ask',
    query: 'ASK { ?person ex:hasConsent true }'
  },
  run: async (event) => {
    if (!event.result) throw new Error('Consent required');
  }
});

// Both hooks must pass (AND logic)
```

### Parallel Evaluation (OR)

Use single hook with SPARQL UNION:

```javascript
defineHook({
  meta: { name: 'check-age-or-guardian' },
  when: {
    kind: 'sparql-ask',
    query: `
      ASK {
        {
          # Condition 1: Age >= 18
          ?person ex:age ?age .
          FILTER (?age >= 18)
        }
        UNION
        {
          # Condition 2: Has guardian
          ?person ex:hasGuardian ?guardian
        }
      }
    `
  },
  run: async (event) => {
    if (!event.result) {
      throw new Error('Must be 18+ or have a guardian');
    }
  }
});
```

### Complex Logic

Use SPARQL `FILTER` for advanced conditions:

```javascript
defineHook({
  meta: { name: 'complex-validation' },
  when: {
    kind: 'sparql-ask',
    query: `
      ASK {
        ?person a foaf:Person ;
                ex:age ?age ;
                ex:country ?country .

        # Complex logic: (age >= 18) OR (age >= 16 AND country = 'UK')
        FILTER (
          ?age >= 18 ||
          (?age >= 16 && ?country = 'UK')
        )
      }
    `
  },
  run: async (event) => {
    if (!event.result) {
      throw new Error('Age requirements not met');
    }
  }
});
```

## Predicate Evaluation Order

UNRDF evaluates predicates in this order:

1. **Before phase** (if defined)
2. **Predicate evaluation** (when clause)
3. **Run phase** (if predicate satisfied)
4. **After phase** (if defined)

```
Transaction
     ↓
┌─────────┐
│ Before  │ ← Fast validation, normalization
└────┬────┘
     ↓
┌─────────┐
│ When    │ ← Predicate evaluation (SPARQL, SHACL, etc.)
└────┬────┘
     │
   Satisfied?
     │
    Yes
     ↓
┌─────────┐
│  Run    │ ← Main effect
└────┬────┘
     ↓
┌─────────┐
│ After   │ ← Cleanup, audit
└─────────┘
```

## Custom Predicate Creation

You can create custom predicates by combining existing ones:

```javascript
// Custom predicate: "large person dataset"
function createLargePersonDatasetPredicate(threshold = 10000) {
  return {
    kind: 'sparql-select',
    query: `
      SELECT (COUNT(?person) AS ?count)
      WHERE {
        ?person a foaf:Person
      }
    `,
    evaluator: async (result) => {
      const count = result[0]?.count?.value || 0;
      return {
        satisfied: count > threshold,
        count: parseInt(count),
        threshold
      };
    }
  };
}

// Use custom predicate
defineHook({
  meta: { name: 'limit-person-dataset' },
  when: createLargePersonDatasetPredicate(10000),
  run: async (event) => {
    if (event.result.satisfied) {
      throw new Error(`Dataset too large: ${event.result.count} persons`);
    }
  }
});
```

## Performance Optimization

### Cache Predicate Results

```javascript
const predicateCache = new Map();

when: {
  kind: 'sparql-ask',
  query: 'ASK { ... }',
  cache: {
    enabled: true,
    ttl: 60000 // Cache for 60 seconds
  }
}
```

### Use Early Termination

```javascript
before: async ({ payload }) => {
  // Fast check before expensive SPARQL
  if (!payload.additions || payload.additions.length === 0) {
    return { cancel: true, reason: 'No additions' };
  }
  return payload;
}
```

### Optimize SPARQL Queries

```javascript
// ❌ Slow: Full graph scan
ASK {
  ?person a foaf:Person .
  FILTER NOT EXISTS { ?person foaf:name ?name }
}

// ✅ Fast: Limited scope
ASK {
  ?person a foaf:Person .
  FILTER NOT EXISTS {
    ?person foaf:name ?name
  }
}
LIMIT 1
```

## Predicate Examples

### ASK: Existence Check

```javascript
when: {
  kind: 'sparql-ask',
  query: `
    ASK {
      ?person a foaf:Person .
      FILTER NOT EXISTS { ?person foaf:name ?name }
    }
  `
}
```

### SELECT: Data Query

```javascript
when: {
  kind: 'sparql-select',
  query: `
    SELECT ?person ?age
    WHERE {
      ?person a foaf:Person ;
              ex:age ?age .
      FILTER (?age < 18)
    }
  `
}
```

### SHACL: Shape Validation

```javascript
when: {
  kind: 'shacl',
  shapes: personShapeStore // N3.Store with SHACL shapes
}
```

### DELTA: Change Detection

```javascript
when: {
  kind: 'delta',
  pattern: {
    predicate: namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    object: namedNode('http://xmlns.com/foaf/0.1/Person')
  }
}
```

### THRESHOLD: Numeric Comparison

```javascript
when: {
  kind: 'threshold',
  value: 10000,
  operator: 'gt' // >, <, >=, <=, ==, !=
}
```

### COUNT: Cardinality

```javascript
when: {
  kind: 'count',
  min: 0,
  max: 10000
}
```

### WINDOW: Time-Based

```javascript
when: {
  kind: 'window',
  duration: '5m' // 5 minutes
}
```

## Next Steps

Explore each predicate type in detail:
- **[ASK Predicates](predicates/ask.md)** - Boolean existence checks
- **[THRESHOLD Predicates](predicates/threshold.md)** - Numeric comparisons
- **[DELTA Predicates](predicates/delta.md)** - Change detection
- **[SHACL Predicates](predicates/shacl.md)** - Shape validation
- **[Custom Predicates](predicates/custom.md)** - Build your own
