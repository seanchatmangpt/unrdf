# @unrdf/rdf-graphql

Type-safe GraphQL interface for RDF knowledge graphs with automatic schema generation from ontologies.

## Overview

`@unrdf/rdf-graphql` automatically generates GraphQL schemas from RDF ontologies (RDFS/OWL) and provides type-safe query execution backed by the Oxigraph SPARQL engine. It bridges the semantic web and modern API development.

## Features

- **Automatic Schema Generation**: Convert RDFS/OWL ontologies to GraphQL schemas
- **Type Safety**: Full type mapping from XSD datatypes to GraphQL scalars
- **SPARQL Translation**: GraphQL queries automatically translated to SPARQL
- **High Performance**: Powered by Oxigraph's native SPARQL engine
- **Query Caching**: Optional result caching for improved performance
- **Federation Support**: Designed for distributed knowledge graphs
- **Zero Configuration**: Works out-of-the-box with standard ontologies

## Installation

```bash
pnpm add @unrdf/rdf-graphql @unrdf/oxigraph graphql
```

## Quick Start

```javascript
import { createAdapter } from '@unrdf/rdf-graphql';

// Create adapter with namespace configuration
const adapter = createAdapter({
  namespaces: {
    ex: 'http://example.org/schema#',
  },
  enableCache: true,
});

// Load RDF ontology (defines schema)
const ontology = `
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <http://example.org/schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:Person a rdfs:Class ;
  rdfs:label "Person" .

ex:name a rdf:Property ;
  rdfs:domain ex:Person ;
  rdfs:range xsd:string .

ex:age a rdf:Property ;
  rdfs:domain ex:Person ;
  rdfs:range xsd:integer .
`;

await adapter.loadOntology(ontology);

// Load instance data
const data = `
@prefix ex: <http://example.org/schema#> .

<http://example.org/people/alice> a ex:Person ;
  ex:name "Alice Smith" ;
  ex:age 30 .
`;

await adapter.loadData(data);

// Generate GraphQL schema
const schema = adapter.generateSchema();

// Execute GraphQL queries
const result = await adapter.executeQuery(`
  query {
    persons {
      id
      name
      age
    }
  }
`);

console.log(result.data.persons);
// [{ id: "http://example.org/people/alice", name: "Alice Smith", age: 30 }]
```

## Ontology to GraphQL Mapping Rules

### Class Mapping

| RDF/OWL | GraphQL | Example |
|---------|---------|---------|
| `rdfs:Class` | `ObjectType` | `ex:Person` → `type Person` |
| `owl:Class` | `ObjectType` | `owl:Thing` → `type Thing` |
| Class local name | Type name | `ex:WorkflowCase` → `WorkflowCase` |

### Property Mapping

| RDF/OWL | GraphQL | Example |
|---------|---------|---------|
| `rdf:Property` | Field | `ex:name` → `name: String` |
| `rdfs:domain` | Parent type | `ex:Person` → field in `Person` |
| `rdfs:range` | Field type | `xsd:string` → `String` |
| `owl:ObjectProperty` | Object field | `ex:manager` → `manager: Person` |
| `owl:DatatypeProperty` | Scalar field | `ex:email` → `email: String` |

### Datatype Mapping

| XSD Datatype | GraphQL Type |
|--------------|--------------|
| `xsd:string` | `String` |
| `xsd:integer`, `xsd:int`, `xsd:long` | `Int` |
| `xsd:decimal`, `xsd:float`, `xsd:double` | `Float` |
| `xsd:boolean` | `Boolean` |
| `xsd:dateTime`, `xsd:anyURI` | `String` |
| Unknown/Missing | `String` (fallback) |

### Query Generation

For each class, two root query fields are automatically generated:

```graphql
# Single item query
person(id: ID!): Person

# List query with pagination
persons(limit: Int, offset: Int): [Person]
```

### Field Resolution

| GraphQL Query | SPARQL Translation |
|---------------|-------------------|
| `{ id }` | `?s` (subject IRI) |
| `{ name }` | `?s ex:name ?name` |
| `{ age }` | `?s ex:age ?age` |
| Nested objects | Additional graph patterns |

## YAWL Workflow Example

```javascript
import { createWorkflowAdapter, EXAMPLE_QUERIES } from '@unrdf/rdf-graphql/examples/workflow-schema';

// Create adapter with YAWL ontology
const { adapter, schema } = await createWorkflowAdapter();

// Query workflows
const result = await adapter.executeQuery(
  EXAMPLE_QUERIES.getWorkflow,
  { id: 'http://example.org/workflows/order-fulfillment' }
);

console.log(result.data.workflow);
// {
//   id: "http://example.org/workflows/order-fulfillment",
//   workflowId: "wf-001",
//   name: "Order Fulfillment",
//   description: "Process customer orders..."
// }

// List cases with pagination
const cases = await adapter.executeQuery(
  EXAMPLE_QUERIES.listCases,
  { limit: 10, offset: 0 }
);

console.log(cases.data.cases);
```

## API Reference

### RDFGraphQLAdapter

Main entry point for the library.

#### Constructor Options

```javascript
{
  namespaces: Record<string, string>,     // Namespace prefixes
  excludeClasses: string[],               // Classes to exclude from schema
  includeInferred: boolean,               // Include inferred types (default: false)
  enableCache: boolean,                   // Enable query result caching (default: false)
  typeMapping: Record<string, string>     // Custom GraphQL type to RDF class mapping
}
```

#### Methods

**`loadOntology(rdfData, format?, baseIRI?)`**
Load RDF ontology defining the schema.

**`loadData(rdfData, format?, baseIRI?)`**
Load RDF instance data.

**`generateSchema(options?)`**
Generate GraphQL schema from loaded ontology.

**`executeQuery(query, variables?, context?)`**
Execute GraphQL query and return results.

**`executeSPARQL(query)`**
Execute SPARQL query directly against the store.

**`getSchema()`**
Get the generated GraphQL schema.

**`getStore()`**
Get the underlying Oxigraph store.

**`introspectClasses()`**
Get all classes in the ontology.

**`introspectProperties()`**
Get all properties in the ontology.

**`getStatistics()`**
Get store statistics (triple count, class count, instance count).

**`clearCache()`**
Clear query result cache.

**`getCacheStats()`**
Get cache statistics.

### SPARQLQueryBuilder

Translates GraphQL queries to SPARQL.

```javascript
import { SPARQLQueryBuilder } from '@unrdf/rdf-graphql/query';

const builder = new SPARQLQueryBuilder({
  namespaces: {
    ex: 'http://example.org/schema#',
  },
});

// Add custom namespace
builder.addNamespace('custom', 'http://custom.org/');

// Build queries
const sparql = builder.buildListQuery(info, typeIRI, { limit: 10, offset: 0 });
```

### RDFSchemaGenerator

Generates GraphQL schemas from RDF ontologies.

```javascript
import { RDFSchemaGenerator } from '@unrdf/rdf-graphql/schema';

const generator = new RDFSchemaGenerator({
  namespaces: { ex: 'http://example.org/' },
  excludeClasses: ['http://example.org/InternalClass'],
});

await generator.loadOntology(rdfData);
const schema = generator.generateSchema();
```

## Advanced Usage

### Custom Type Mapping

```javascript
const adapter = createAdapter({
  namespaces: { yawl: 'http://example.org/yawl#' },
  typeMapping: {
    Workflow: 'http://example.org/yawl#Workflow',
    Case: 'http://example.org/yawl#Case',
    Task: 'http://example.org/yawl#Task',
  },
});
```

### Query Caching

```javascript
const adapter = createAdapter({
  enableCache: true,
});

// ... execute queries ...

// Check cache stats
console.log(adapter.getCacheStats());
// { enabled: true, size: 42 }

// Clear cache
adapter.clearCache();
```

### Direct SPARQL Execution

```javascript
const results = adapter.executeSPARQL(`
  SELECT ?subject ?predicate ?object WHERE {
    ?subject ?predicate ?object .
  }
  LIMIT 10
`);

console.log(results);
// [{ subject: "...", predicate: "...", object: "..." }, ...]
```

### Ontology Introspection

```javascript
// Get all classes
const classes = adapter.introspectClasses();
console.log(classes);
// [{ iri: "...", label: "...", comment: "..." }, ...]

// Get all properties
const properties = adapter.introspectProperties();
console.log(properties);
// [{ iri: "...", domain: "...", range: "...", label: "..." }, ...]

// Get statistics
const stats = await adapter.getStatistics();
console.log(stats);
// { tripleCount: 1234, classCount: 10, instanceCount: 567 }
```

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    GraphQL Client                        │
└────────────────────┬────────────────────────────────────┘
                     │ GraphQL Query
                     ▼
┌─────────────────────────────────────────────────────────┐
│              RDFGraphQLAdapter                           │
│  ┌────────────────┐  ┌──────────────────────────────┐  │
│  │ Schema         │  │ Resolver Factory              │  │
│  │ Generator      │  │ (creates resolvers)           │  │
│  └────────────────┘  └──────────────────────────────┘  │
└──────────┬──────────────────────┬───────────────────────┘
           │ RDFS/OWL             │ SPARQL
           ▼                      ▼
┌──────────────────────┐  ┌──────────────────────────────┐
│  Ontology (Schema)   │  │  SPARQLQueryBuilder          │
│  - Classes           │  │  (GraphQL → SPARQL)          │
│  - Properties        │  └──────────┬───────────────────┘
│  - Datatypes         │             │ SPARQL Query
└──────────────────────┘             ▼
                          ┌──────────────────────────────┐
                          │  Oxigraph Store              │
                          │  (RDF triple store)          │
                          └──────────────────────────────┘
```

## Performance Considerations

1. **Schema Generation**: One-time cost at startup
2. **Query Translation**: Minimal overhead (< 1ms)
3. **SPARQL Execution**: Depends on query complexity and data size
4. **Caching**: Recommended for read-heavy workloads
5. **Indexing**: Oxigraph automatically indexes RDF data

## Federation (Future)

Designed for distributed knowledge graphs:

```javascript
// Future API (not yet implemented)
const federatedAdapter = createFederatedAdapter({
  services: [
    { name: 'workflows', url: 'http://service1/graphql' },
    { name: 'users', url: 'http://service2/graphql' },
  ],
});
```

## Testing

```bash
# Run all tests
pnpm test

# Run with timeout (recommended)
timeout 5s pnpm test
```

## Use Cases

1. **Semantic Web APIs**: Expose RDF knowledge graphs via GraphQL
2. **Workflow Systems**: Query YAWL workflows with type safety
3. **Knowledge Management**: Type-safe access to ontology-based data
4. **Data Integration**: Unified GraphQL interface for heterogeneous RDF sources
5. **Research Data**: Query scientific datasets with GraphQL

## Limitations

1. **No Mutations**: Read-only (queries only, no mutations/subscriptions yet)
2. **Simple Relationships**: Nested queries limited to one level
3. **No Inference**: RDFS/OWL reasoning not yet implemented
4. **Scalar Types**: Limited to XSD datatypes mapped in table above

## Roadmap

- [ ] GraphQL mutations (INSERT/UPDATE/DELETE)
- [ ] Subscriptions for real-time updates
- [ ] RDFS/OWL inference support
- [ ] GraphQL federation support
- [ ] Custom scalar types
- [ ] Performance optimizations (query planning)
- [ ] GraphQL schema stitching

## License

MIT

## Contributing

Contributions welcome! Please ensure:
- All tests pass (`pnpm test`)
- Code follows existing patterns
- JSDoc comments for all public APIs
- No TypeScript in source (use JSDoc + Zod)

## References

- [GraphQL Specification](https://spec.graphql.org/)
- [SPARQL 1.1 Query Language](https://www.w3.org/TR/sparql11-query/)
- [RDF Schema (RDFS)](https://www.w3.org/TR/rdf-schema/)
- [OWL Web Ontology Language](https://www.w3.org/TR/owl2-overview/)
- [Oxigraph Documentation](https://github.com/oxigraph/oxigraph)
