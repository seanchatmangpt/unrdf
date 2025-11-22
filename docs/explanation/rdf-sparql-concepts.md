# Understanding RDF and SPARQL Concepts

Deep explanation of RDF, SPARQL, and how UNRDF leverages them.

## What is RDF?

**RDF (Resource Description Framework)** is a W3C standard for representing information as a graph of connected resources. Unlike tables (SQL) or documents (JSON), RDF naturally represents relationships and interconnected data.

### The Triple Model

RDF data consists of **triples** - statements with three parts:

```
Subject  ->  Predicate  ->  Object
(who/what)   (relationship) (value/target)
```

**Example:**

```
Alice     knows     Bob
(Subject) (Predicate) (Object)
```

In RDF notation:

```turtle
<http://example.org/alice> <http://xmlns.com/foaf/0.1/knows> <http://example.org/bob> .
```

### Why Graphs, Not Tables?

| Aspect | Tables (SQL) | Graphs (RDF) |
|--------|--------------|--------------|
| Schema | Fixed columns | Schema-flexible |
| Relationships | Foreign keys | First-class edges |
| Data Integration | ETL required | Automatic via URIs |
| Queries | Joins | Graph traversal |

**Key Insight:** RDF represents knowledge as interconnected facts, not isolated records.

## URIs as Global Identifiers

Every resource in RDF is identified by a **URI (Uniform Resource Identifier)**:

```
http://xmlns.com/foaf/0.1/Person
     ^       ^          ^
  scheme  authority   path
```

**Why URIs matter:**

1. **Global uniqueness** - No collisions across systems
2. **Dereferenceable** - Can fetch definitions
3. **Namespacing** - Organize concepts into vocabularies

### Prefixes for Readability

Full URIs are verbose. **Prefixes** make them readable:

```turtle
# Full URIs
<http://example.org/alice> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .

# With prefixes
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:alice rdf:type foaf:Person .

# Even shorter (Turtle shorthand)
ex:alice a foaf:Person .
```

## Literal Values

Not everything is a URI. **Literals** represent data values:

```turtle
# String literal
ex:alice foaf:name "Alice Smith" .

# Typed literal (integer)
ex:alice foaf:age "30"^^xsd:integer .

# Language-tagged literal
ex:alice rdfs:label "Alice"@en .
ex:alice rdfs:label "Alicia"@es .
```

### Common Datatypes

| XSD Type | Example | Description |
|----------|---------|-------------|
| `xsd:string` | `"hello"` | Text |
| `xsd:integer` | `42` | Whole number |
| `xsd:decimal` | `3.14` | Decimal number |
| `xsd:boolean` | `true` | True/false |
| `xsd:dateTime` | `"2024-01-15T10:30:00Z"` | Timestamp |
| `xsd:date` | `"2024-01-15"` | Date |

## Blank Nodes

**Blank nodes** represent anonymous resources (resources without URIs):

```turtle
# The address is anonymous
ex:alice ex:address [
    ex:city "New York" ;
    ex:zip "10001"
] .

# Equivalent long form
_:address1 ex:city "New York" .
_:address1 ex:zip "10001" .
ex:alice ex:address _:address1 .
```

**When to use blank nodes:**

- Intermediate structures (addresses, measurements)
- Resources you don't need to reference externally
- List structures

## Named Graphs

RDF can be partitioned into **named graphs** - subgraphs with URIs:

```turtle
# Graph 1: Production data
GRAPH <http://example.org/production> {
    ex:alice foaf:name "Alice" .
}

# Graph 2: Test data
GRAPH <http://example.org/test> {
    ex:test foaf:name "Test User" .
}
```

**Use cases:**

- Provenance tracking (where did data come from?)
- Access control (who can see what?)
- Versioning (snapshots of data)
- Multi-tenancy (separate customer data)

## SPARQL Query Language

**SPARQL** is the query language for RDF, analogous to SQL for databases.

### Query Types

| Type | Purpose | Returns |
|------|---------|---------|
| SELECT | Retrieve variable bindings | Table of values |
| ASK | Check existence | Boolean |
| CONSTRUCT | Create new graph | RDF graph |
| DESCRIBE | Get information about resource | RDF graph |

### SELECT Queries

```sparql
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT ?name ?email
WHERE {
    ?person a foaf:Person ;
            foaf:name ?name ;
            foaf:mbox ?email .
}
ORDER BY ?name
LIMIT 10
```

**How it works:**

1. `?person`, `?name`, `?email` are variables
2. Triple patterns match against the graph
3. Results are returned as a table

### Pattern Matching

SPARQL uses **graph patterns** to match data:

```sparql
# Basic pattern
?person foaf:name ?name .

# Multiple patterns (implicit AND)
?person foaf:name ?name .
?person foaf:age ?age .

# OPTIONAL patterns
?person foaf:name ?name .
OPTIONAL { ?person foaf:age ?age }

# UNION (OR)
{ ?person foaf:name ?name }
UNION
{ ?person rdfs:label ?name }
```

### FILTER Expressions

Constrain results with **FILTER**:

```sparql
# Numeric comparison
FILTER (?age > 30)

# String functions
FILTER (CONTAINS(LCASE(?name), "alice"))

# Regular expressions
FILTER (REGEX(?email, "@example\\.com$"))

# Existence check
FILTER (BOUND(?email))

# Type checking
FILTER (isIRI(?resource))
```

### Property Paths

Navigate relationships with **property paths**:

```sparql
# Sequence (knows someone who knows)
ex:alice foaf:knows/foaf:knows ?fof .

# One or more hops
ex:alice foaf:knows+ ?reachable .

# Zero or more hops
ex:alice foaf:knows* ?anyone .

# Inverse path
?friend foaf:knows/^foaf:knows ex:alice .
```

### Aggregations

Compute statistics with aggregations:

```sparql
SELECT ?dept (COUNT(?person) AS ?count) (AVG(?salary) AS ?avgSalary)
WHERE {
    ?person ex:department ?dept ;
            ex:salary ?salary .
}
GROUP BY ?dept
HAVING (COUNT(?person) > 5)
```

## How UNRDF Uses These Concepts

### Unified API

UNRDF provides a consistent JavaScript API over standard RDF:

```javascript
// Parse Turtle -> N3 Store (RDF graph)
const store = await parseTurtle(turtle);

// Query with SPARQL
const results = await select(store, sparql);

// Works exactly like the spec defines
```

### Knowledge Hooks Connection

Knowledge Hooks use SPARQL/SHACL as trigger conditions:

```javascript
defineHook({
  when: {
    kind: 'sparql-ask',  // Uses SPARQL ASK query
    ref: { uri: 'file://condition.rq' }
  }
});
```

The hook fires when the SPARQL ASK query returns `true`.

### N3 Store Implementation

UNRDF uses **N3.Store** as its RDF store:

- Indexed triple storage
- Efficient pattern matching
- Named graph support
- Standards-compliant RDF/JS interface

```javascript
// Direct access to RDF/JS methods
for (const quad of store.getQuads(null, foaf.name, null)) {
  console.log(quad.object.value);
}
```

## Key Takeaways

1. **RDF is a graph** - Think nodes and edges, not rows and columns
2. **URIs are identifiers** - Global, unique, potentially dereferenceable
3. **SPARQL is pattern matching** - Find subgraphs that match your pattern
4. **Named graphs partition data** - Organize, version, and control access
5. **UNRDF wraps the standards** - Clean API over proven implementations

## Further Reading

- [W3C RDF Primer](https://www.w3.org/TR/rdf11-primer/) - Official introduction
- [SPARQL 1.1 Specification](https://www.w3.org/TR/sparql11-query/) - Full language spec
- [Turtle Syntax](https://www.w3.org/TR/turtle/) - Turtle format specification
- [JSON-LD](https://json-ld.org/) - JSON-based RDF serialization

## Related

- [SPARQL Tutorial](../tutorials/sparql.md) - Hands-on learning
- [Creating RDF Documents](../tutorials/creating-rdf-documents.md) - Practical guide
- [System Design](./system-design.md) - How UNRDF is built
