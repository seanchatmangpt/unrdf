# How-To: Query with SPARQL

**Problem**: You need to extract, filter, or transform RDF data using SPARQL queries (SELECT, ASK, CONSTRUCT, DESCRIBE).

## Solution

UNRDF provides a universal `query()` function plus specialized functions for each query type. All queries execute against N3 Store instances.

### SELECT Queries

Extract data with variable bindings:

```javascript
import { parseTurtle, select } from 'unrdf';

const ttl = `
@prefix ex: <http://example.org/> .
@prefix schema: <http://schema.org/> .

ex:alice a schema:Person ;
  schema:name "Alice" ;
  schema:age 30 ;
  schema:knows ex:bob .

ex:bob a schema:Person ;
  schema:name "Bob" ;
  schema:age 25 .
`;

const store = parseTurtle(ttl);

// Simple SELECT
const query = `
  PREFIX schema: <http://schema.org/>

  SELECT ?person ?name WHERE {
    ?person a schema:Person ;
            schema:name ?name .
  }
`;

const results = select(store, query);
results.forEach(row => {
  console.log(`Person: ${row.person.value}, Name: ${row.name.value}`);
});
// Output:
// Person: http://example.org/alice, Name: Alice
// Person: http://example.org/bob, Name: Bob
```

### ASK Queries

Boolean checks for pattern existence:

```javascript
import { ask } from 'unrdf';

// Check if Alice exists
const exists = ask(store, `
  PREFIX ex: <http://example.org/>
  PREFIX schema: <http://schema.org/>

  ASK { ex:alice a schema:Person }
`);

console.log(`Alice exists: ${exists}`);  // true

// Check for relationships
const hasFriends = ask(store, `
  PREFIX schema: <http://schema.org/>

  ASK {
    ?person schema:knows ?friend .
  }
`);

console.log(`Has relationships: ${hasFriends}`);  // true
```

### CONSTRUCT Queries

Transform graphs into new shapes:

```javascript
import { construct, toTurtle } from 'unrdf';

// Transform schema.org → FOAF
const newStore = construct(store, `
  PREFIX schema: <http://schema.org/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  CONSTRUCT {
    ?person a foaf:Person ;
            foaf:name ?name ;
            foaf:knows ?friend .
  }
  WHERE {
    ?person a schema:Person ;
            schema:name ?name .
    OPTIONAL { ?person schema:knows ?friend }
  }
`);

console.log(toTurtle(newStore));
// Output uses FOAF vocabulary
```

### DESCRIBE Queries

Extract all information about resources:

```javascript
import { describe } from 'unrdf';

// Get all triples about Alice
const aliceStore = describe(store, `
  PREFIX ex: <http://example.org/>

  DESCRIBE ex:alice
`);

console.log(`Alice has ${aliceStore.size} triples`);
```

### SPARQL UPDATE

Modify graphs with INSERT/DELETE:

```javascript
import { update } from 'unrdf';

// Add new data
update(store, `
  PREFIX ex: <http://example.org/>
  PREFIX schema: <http://schema.org/>

  INSERT DATA {
    ex:charlie a schema:Person ;
                schema:name "Charlie" ;
                schema:age 35 .
  }
`);

// Delete data
update(store, `
  PREFIX ex: <http://example.org/>
  PREFIX schema: <http://schema.org/>

  DELETE DATA {
    ex:alice schema:age 30 .
  }
`);

// Conditional update
update(store, `
  PREFIX ex: <http://example.org/>
  PREFIX schema: <http://schema.org/>

  DELETE { ?person schema:age ?oldAge }
  INSERT { ?person schema:age 31 }
  WHERE {
    ex:alice schema:age ?oldAge .
  }
`);
```

### Complex Filters

Use FILTER for advanced selection:

```javascript
const adults = select(store, `
  PREFIX schema: <http://schema.org/>

  SELECT ?person ?name ?age WHERE {
    ?person a schema:Person ;
            schema:name ?name ;
            schema:age ?age .
    FILTER (?age >= 18)
  }
  ORDER BY DESC(?age)
`);

// Regular expressions
const aNames = select(store, `
  PREFIX schema: <http://schema.org/>

  SELECT ?name WHERE {
    ?person schema:name ?name .
    FILTER REGEX(?name, "^A", "i")
  }
`);
```

### Aggregations

Use GROUP BY and aggregation functions:

```javascript
const stats = select(store, `
  PREFIX schema: <http://schema.org/>

  SELECT (COUNT(?person) as ?count) (AVG(?age) as ?avgAge) WHERE {
    ?person a schema:Person ;
            schema:age ?age .
  }
`);

console.log(`Total: ${stats[0].count.value}, Average Age: ${stats[0].avgAge.value}`);

// Group by category
const byType = select(store, `
  PREFIX schema: <http://schema.org/>

  SELECT ?type (COUNT(?entity) as ?count) WHERE {
    ?entity a ?type .
  }
  GROUP BY ?type
`);
```

### Composable Queries

Use composables for concise queries:

```javascript
import { initStore, useGraph } from 'unrdf';

await initStore();
const graph = useGraph();

// Add data
graph.update(`
  PREFIX ex: <http://example.org/>
  PREFIX schema: <http://schema.org/>

  INSERT DATA {
    ex:alice a schema:Person ; schema:name "Alice" .
  }
`);

// Query
const results = graph.select(`
  SELECT ?name WHERE { ?person schema:name ?name }
`);

// Boolean check
const exists = graph.ask(`ASK { ex:alice a schema:Person }`);

// Transform
const newGraph = graph.construct(`
  CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }
`);
```

## Variations

### SPARQL Builder

Use builder for programmatic queries:

```javascript
import { createSPARQLBuilder } from 'unrdf/utils';

const builder = createSPARQLBuilder();

const query = builder
  .select('?person', '?name')
  .where('?person', 'a', 'schema:Person')
  .where('?person', 'schema:name', '?name')
  .filter('?age > 18')
  .orderBy('?name')
  .limit(10)
  .build();

const results = select(store, query);
```

### Federated Queries

Query across multiple stores:

```javascript
import { mergeStores, select } from 'unrdf';

const localStore = await readTurtleFile('./local.ttl');
const remoteStore = await readTurtleFile('./remote.ttl');

// Merge for federated query
const federated = mergeStores(localStore, remoteStore);

const results = select(federated, `
  SELECT ?s ?p ?o WHERE {
    ?s ?p ?o .
    FILTER (STRSTARTS(STR(?s), "http://remote.example.org/"))
  }
`);
```

### Parameterized Queries

Build safe queries with parameters:

```javascript
import { escapeSPARQLString } from 'unrdf/utils';

function findPersonByName(store, name) {
  const safeName = escapeSPARQLString(name);

  return select(store, `
    PREFIX schema: <http://schema.org/>

    SELECT ?person WHERE {
      ?person schema:name ${safeName} .
    }
  `);
}

const results = findPersonByName(store, "Alice");
```

### Pagination

Implement LIMIT/OFFSET for large results:

```javascript
function paginateResults(store, query, page = 0, pageSize = 10) {
  const offset = page * pageSize;

  const paginatedQuery = `
    ${query}
    LIMIT ${pageSize}
    OFFSET ${offset}
  `;

  return select(store, paginatedQuery);
}

// Get page 2 (items 10-19)
const page2 = paginateResults(store, baseQuery, 2, 10);
```

## Related Guides

- [How-To: Parse RDF Formats](./parse-rdf-formats.md) - Load data for querying
- [How-To: Optimize Queries](./optimize-queries.md) - Performance tuning
- [How-To: Validate RDF Data](./validate-rdf-data.md) - SPARQL-based validation
- [How-To: Use Composables](./use-composables.md) - High-level query APIs
