# How To: Query Your Knowledge Graph

**Problem:** You need to explore and analyze your RDF data using SPARQL queries.

**Solution:** Use SPARQL to query both the current Universe and the immutable EventLog.

## When to Use This

- **Exploration**: "What's in my knowledge graph?"
- **Analysis**: "Which entities have changed?"
- **Reporting**: "Generate reports from historical data"
- **Validation**: "Do all entities meet my constraints?"
- **Debugging**: "Why did this mutation fail?"

## Query the Current State (Universe)

The Universe graph contains the current observable state:

```javascript
import { KGCStore } from '@unrdf/kgc-4d';

const store = new KGCStore();

// Add some data
await store.appendEvent(/* ... */);

// Query current state
const query = `
  PREFIX ex: <http://example.org/>
  SELECT ?person ?name
  WHERE {
    GRAPH <kgc:Universe> {
      ?person a ex:Person ;
              ex:name ?name .
    }
  }
`;

const results = store.querySync(query);
results.forEach(binding => {
  console.log(binding.get('person').value, '->', binding.get('name').value);
});
```

## Query the Event Log

The EventLog contains the immutable history of all mutations:

```javascript
// Find all CREATE events
const eventQuery = `
  PREFIX kgc: <kgc:>
  SELECT ?eventId ?payload ?timestamp
  WHERE {
    GRAPH <kgc:EventLog> {
      ?eventId kgc:type kgc:CREATE ;
               kgc:payload ?payload ;
               kgc:T_NS ?timestamp .
    }
  }
  ORDER BY ?timestamp
`;

const events = store.querySync(eventQuery);
events.forEach(binding => {
  console.log('Event:', binding.get('eventId').value);
  console.log('Payload:', JSON.parse(binding.get('payload').value));
});
```

## Common Query Patterns

### Count All Triples

```sparql
PREFIX kgc: <kgc:>
SELECT (COUNT(*) as ?count)
WHERE {
  GRAPH <kgc:Universe> {
    ?s ?p ?o .
  }
}
```

### Find All People

```sparql
PREFIX ex: <http://example.org/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT ?person
WHERE {
  GRAPH <kgc:Universe> {
    ?person rdf:type ex:Person .
  }
}
```

### Get Relationships

```sparql
PREFIX ex: <http://example.org/>

SELECT ?person1 ?person2
WHERE {
  GRAPH <kgc:Universe> {
    ?person1 ex:knows ?person2 .
  }
}
```

### Filter by Property Value

```sparql
PREFIX ex: <http://example.org/>

SELECT ?person ?name
WHERE {
  GRAPH <kgc:Universe> {
    ?person ex:name ?name ;
            ex:age ?age .
    FILTER (?age > 25)
  }
}
```

### Find Recently Modified Entities

```sparql
PREFIX kgc: <kgc:>
PREFIX ex: <http://example.org/>

SELECT ?entity ?lastModified
WHERE {
  GRAPH <kgc:EventLog> {
    ?event kgc:type kgc:UPDATE ;
           kgc:T_NS ?lastModified ;
           kgc:subject ?entity .
  }
}
ORDER BY DESC(?lastModified)
LIMIT 10
```

## Complex Analytical Queries

### Track Entity Lifecycle

Find when an entity was created, modified, and deleted:

```sparql
PREFIX kgc: <kgc:>

SELECT ?entity
       (MIN(?createdAt) as ?firstEvent)
       (MAX(?modifiedAt) as ?lastEvent)
       (COUNT(*) as ?totalChanges)
WHERE {
  GRAPH <kgc:EventLog> {
    ?event kgc:subject ?entity ;
           kgc:T_NS ?timestamp .
    BIND(?timestamp as ?createdAt)
    BIND(?timestamp as ?modifiedAt)
  }
}
GROUP BY ?entity
```

### Find Property Evolution

See how a property changed over time:

```sparql
PREFIX kgc: <kgc:>
PREFIX ex: <http://example.org/>

SELECT ?entity ?value ?timestamp
WHERE {
  GRAPH <kgc:EventLog> {
    ?event kgc:subject ?entity ;
           kgc:predicate ex:status ;
           kgc:object ?value ;
           kgc:T_NS ?timestamp .
  }
}
ORDER BY ?entity ?timestamp
```

### Audit Trail for Specific Entity

```javascript
function auditTrail(entityUri) {
  return `
    PREFIX kgc: <kgc:>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

    SELECT ?timestamp ?eventType ?predicate ?oldValue ?newValue ?payload
    WHERE {
      GRAPH <kgc:EventLog> {
        ?event kgc:subject <${entityUri}> ;
               kgc:type ?eventType ;
               kgc:T_NS ?timestamp ;
               kgc:payload ?payload .
        OPTIONAL {
          ?event kgc:predicate ?predicate ;
                 kgc:object ?newValue .
        }
      }
    }
    ORDER BY ?timestamp
  `;
}

const audit = store.querySync(auditTrail('http://example.org/alice'));
audit.forEach(row => {
  console.log(`[${row.get('timestamp')}] ${row.get('eventType')}`);
});
```

## Aggregate Queries

### Statistics Across Time

```sparql
PREFIX kgc: <kgc:>

SELECT ?eventType (COUNT(*) as ?count)
WHERE {
  GRAPH <kgc:EventLog> {
    ?event kgc:type ?eventType .
  }
}
GROUP BY ?eventType
ORDER BY DESC(?count)
```

### Entity Modification Frequency

```sparql
PREFIX kgc: <kgc:>

SELECT ?entity (COUNT(*) as ?modificationCount)
WHERE {
  GRAPH <kgc:EventLog> {
    ?event kgc:subject ?entity .
  }
}
GROUP BY ?entity
ORDER BY DESC(?modificationCount)
LIMIT 20
```

## Query with Reconstruction

Combine querying with time travel:

```javascript
async function queryAtTime(store, git, timestamp, sparqlQuery) {
  // Reconstruct state at specific time
  const pastStore = await reconstructState(store, git, timestamp);

  // Query the historical state
  return pastStore.querySync(sparqlQuery);
}

// Compare state before and after an event
const dayAgo = now() - BigInt(86_400_000_000_000);

const beforeQuery = `
  PREFIX ex: <http://example.org/>
  SELECT (COUNT(*) as ?count)
  WHERE {
    GRAPH <kgc:Universe> {
      ?person a ex:Person .
    }
  }
`;

const before = await queryAtTime(store, git, dayAgo, beforeQuery);
const now_ = store.querySync(beforeQuery);

console.log('People yesterday:', before[0].get('count').value);
console.log('People today:', now_[0].get('count').value);
```

## Validation Queries

### Find Inconsistencies

```sparql
PREFIX ex: <http://example.org/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT ?person
WHERE {
  GRAPH <kgc:Universe> {
    ?person rdf:type ex:Person .
    FILTER NOT EXISTS { ?person ex:name ?name }
  }
}
```

### Check Constraints

```javascript
function validateConstraints(store) {
  const violations = [];

  // Constraint 1: All people must have names
  const noNames = store.querySync(`
    PREFIX ex: <http://example.org/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT ?person
    WHERE {
      GRAPH <kgc:Universe> {
        ?person rdf:type ex:Person .
        FILTER NOT EXISTS { ?person ex:name ?name }
      }
    }
  `);

  if (noNames.length > 0) {
    violations.push({
      constraint: 'All people must have names',
      violations: noNames.map(r => r.get('person').value),
    });
  }

  // Constraint 2: All ages must be positive integers
  const invalidAges = store.querySync(`
    PREFIX ex: <http://example.org/>
    SELECT ?person ?age
    WHERE {
      GRAPH <kgc:Universe> {
        ?person ex:age ?age .
        FILTER (?age < 0)
      }
    }
  `);

  if (invalidAges.length > 0) {
    violations.push({
      constraint: 'All ages must be positive',
      violations: invalidAges,
    });
  }

  return violations;
}
```

## Performance Tips

1. **Use FILTER instead of optional matching** - More efficient
2. **Limit result sets** - Add `LIMIT` when you don't need all results
3. **Index frequently queried properties** - Store creates implicit indexes
4. **Avoid nested queries** - Flatten with JOINs when possible
5. **Use sync queries for read-only operations** - No async overhead

## Troubleshooting

**Q: Query returns no results but I know the data exists**
A: Check your graph URI. Use `<kgc:Universe>` not `<http://...>`. Check namespace prefixes match your data.

**Q: Query is very slow**
A: Try reducing the result set with FILTER or LIMIT. Complex aggregations on large datasets can be slow.

**Q: How do I export query results?**
A: Convert results to JSON or CSV:

```javascript
const results = store.querySync(query);
const json = results.map(binding => {
  const obj = {};
  for (const [key, value] of binding.entries()) {
    obj[key] = value.value;
  }
  return obj;
});
console.log(JSON.stringify(json, null, 2));
```

## Summary

- Query `<kgc:Universe>` for current state
- Query `<kgc:EventLog>` for history
- Use SPARQL FILTER for efficient filtering
- Combine with reconstructState() for time-travel analysis
- Validate data consistency with constraint queries
