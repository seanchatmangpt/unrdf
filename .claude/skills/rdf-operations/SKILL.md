# RDF Operations Skill

Expert knowledge for RDF graph operations, SPARQL queries, and knowledge graph patterns in UNRDF.

## Activation

Use this skill when:
- Working with RDF graphs and triples
- Writing SPARQL queries
- Using Oxigraph for graph storage

## Core Rule: Always Use Oxigraph

```javascript
// CORRECT - Use @unrdf/oxigraph
import { createStore, dataFactory } from '@unrdf/oxigraph';
const store = createStore();

// FORBIDDEN - Never import n3 directly in app code
import { Store } from 'n3';  // ❌ NEVER DO THIS
```

## Triple Operations

### Creating Triples
```javascript
import { dataFactory } from '@unrdf/oxigraph';
const { namedNode, literal, quad } = dataFactory;

const triple = quad(
  namedNode('http://example.org/subject'),
  namedNode('http://example.org/predicate'),
  literal('object value')
);
```

### Adding to Store
```javascript
import { createStore } from '@unrdf/oxigraph';

const store = createStore();
store.add(triple);
```

## SPARQL Queries

### SELECT Query
```javascript
const results = store.query(`
  SELECT ?s ?p ?o
  WHERE {
    ?s ?p ?o .
    FILTER(?p = <http://example.org/name>)
  }
  LIMIT 100
`);
```

### CONSTRUCT Query
```javascript
const graph = store.query(`
  CONSTRUCT {
    ?s <http://example.org/hasName> ?name .
  }
  WHERE {
    ?s <http://schema.org/name> ?name .
  }
`);
```

### ASK Query
```javascript
const exists = store.query(`
  ASK {
    <http://example.org/entity> a <http://example.org/Person> .
  }
`);
```

## Common Prefixes

```sparql
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX schema: <http://schema.org/>
PREFIX unrdf: <http://unrdf.org/ns/>
```

## Performance Guidelines

| Operation | Expected Latency |
|-----------|------------------|
| Single triple insert | <0.1ms |
| SPARQL SELECT (100 results) | <10ms |
| Graph traversal (depth 3) | <50ms |
| Bulk insert (1000 triples) | <100ms |

## SHACL Validation

```javascript
import { validateWithShacl } from '@unrdf/core';

const shapes = `
  @prefix sh: <http://www.w3.org/ns/shacl#> .

  ex:PersonShape a sh:NodeShape ;
    sh:targetClass ex:Person ;
    sh:property [
      sh:path ex:name ;
      sh:minCount 1 ;
      sh:datatype xsd:string ;
    ] .
`;

const report = validateWithShacl(dataGraph, shapes);
if (!report.conforms) {
  console.error(report.results);
}
```

## Error Handling

```javascript
try {
  const results = store.query(sparqlQuery);
} catch (error) {
  if (error.message.includes('parse error')) {
    // SPARQL syntax error
  } else if (error.message.includes('timeout')) {
    // Query timeout - optimize query
  }
}
```
