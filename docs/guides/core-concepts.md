# Core Concepts

This guide covers the fundamental concepts you need to understand to work effectively with UNRDF and RDF in general.

## What is RDF?

RDF (Resource Description Framework) is a standard for representing information on the web. It provides a way to describe resources and their relationships in a machine-readable format.

### Key Principles

1. **Everything is a resource** - Documents, people, concepts, etc.
2. **Resources have properties** - Attributes and relationships
3. **Properties have values** - Literals or other resources
4. **Statements are triples** - Subject, predicate, object

## RDF Triples

The basic unit of RDF is a **triple** (also called a **statement**):

```
Subject → Predicate → Object
```

### Example

```
ex:john → foaf:name → "John Doe"
```

- **Subject**: `ex:john` (the resource being described)
- **Predicate**: `foaf:name` (the property)
- **Object**: `"John Doe"` (the value)

## RDF Terms

RDF uses three types of terms:

### 1. IRIs (Internationalized Resource Identifiers)

IRIs are unique identifiers for resources:

```javascript
// Examples of IRIs
"http://example.org/john"
"http://xmlns.com/foaf/0.1/name"
"https://schema.org/Person"
```

### 2. Literals

Literals are values like strings, numbers, dates:

```javascript
// Examples of literals
"John Doe"                    // Plain string
"30"^^xsd:integer            // Typed literal
"Hello"@en                   // Language-tagged string
"2023-01-01"^^xsd:date      // Date literal
```

### 3. Blank Nodes

Blank nodes are anonymous resources (no IRI):

```javascript
// Blank node (represented as _:b1, _:b2, etc.)
_:b1 → foaf:name → "Anonymous Person"
```

## RDF Graphs

A collection of RDF triples forms an **RDF graph**:

```javascript
// Example RDF graph
ex:john → foaf:name → "John Doe"
ex:john → foaf:age → "30"^^xsd:integer
ex:john → foaf:knows → ex:jane
ex:jane → foaf:name → "Jane Smith"
ex:jane → foaf:age → "28"^^xsd:integer
```

## RDF Formats

RDF can be represented in various formats:

### Turtle (TTL)

Human-readable format:

```turtle
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix ex: <http://example.org/> .

ex:john a foaf:Person ;
    foaf:name "John Doe" ;
    foaf:age 30 ;
    foaf:knows ex:jane .

ex:jane a foaf:Person ;
    foaf:name "Jane Smith" ;
    foaf:age 28 .
```

### JSON-LD

JSON format for RDF:

```json
{
  "@context": {
    "foaf": "http://xmlns.com/foaf/0.1/",
    "ex": "http://example.org/"
  },
  "@id": "ex:john",
  "@type": "foaf:Person",
  "foaf:name": "John Doe",
  "foaf:age": 30,
  "foaf:knows": {
    "@id": "ex:jane"
  }
}
```

### N-Triples

Simple line-based format:

```ntriples
<http://example.org/john> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .
<http://example.org/john> <http://xmlns.com/foaf/0.1/name> "John Doe" .
<http://example.org/john> <http://xmlns.com/foaf/0.1/age> "30"^^<http://www.w3.org/2001/XMLSchema#integer> .
```

## SPARQL

SPARQL is the query language for RDF. It allows you to:

- **Query** RDF data
- **Update** RDF data
- **Construct** new RDF graphs
- **Ask** boolean questions

### Basic SPARQL Patterns

#### SELECT Queries

Return variable bindings:

```sparql
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX ex: <http://example.org/>

SELECT ?name ?age WHERE {
  ?person a foaf:Person ;
    foaf:name ?name ;
    foaf:age ?age .
}
```

#### CONSTRUCT Queries

Create new RDF graphs:

```sparql
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX ex: <http://example.org/>

CONSTRUCT {
  ?person ex:hasName ?name .
} WHERE {
  ?person foaf:name ?name .
}
```

#### ASK Queries

Return true/false:

```sparql
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

ASK {
  ?person a foaf:Person .
}
```

## Namespaces and Prefixes

Namespaces help organize and shorten IRIs:

```turtle
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix ex: <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
```

Common namespaces:

- `rdf:` - RDF vocabulary
- `rdfs:` - RDF Schema
- `owl:` - Web Ontology Language
- `foaf:` - Friend of a Friend
- `dc:` - Dublin Core
- `schema:` - Schema.org

## RDF Schema (RDFS)

RDFS provides basic vocabulary for describing RDF:

### Classes

```turtle
ex:Person a rdfs:Class .
ex:Student rdfs:subClassOf ex:Person .
```

### Properties

```turtle
ex:hasName a rdf:Property ;
    rdfs:domain ex:Person ;
    rdfs:range xsd:string .
```

### Instances

```turtle
ex:john a ex:Person ;
    ex:hasName "John Doe" .
```

## Web Ontology Language (OWL)

OWL extends RDFS with more expressive constructs:

### Class Relationships

```turtle
ex:Student owl:equivalentClass [
    a owl:Restriction ;
    owl:onProperty ex:enrolledIn ;
    owl:someValuesFrom ex:Course
] .
```

### Property Characteristics

```turtle
ex:hasParent a owl:ObjectProperty ;
    owl:inverseOf ex:hasChild ;
    rdf:type owl:TransitiveProperty .
```

## SHACL (Shapes Constraint Language)

SHACL provides validation for RDF data:

### Node Shapes

```turtle
ex:PersonShape a sh:NodeShape ;
    sh:targetClass ex:Person ;
    sh:property [
        sh:path ex:hasName ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
        sh:maxCount 1
    ] .
```

### Property Shapes

```turtle
ex:AgeShape a sh:PropertyShape ;
    sh:path ex:hasAge ;
    sh:datatype xsd:integer ;
    sh:minInclusive 0 ;
    sh:maxInclusive 150 .
```

## UNRDF's Approach

UNRDF provides a composable, opinionated approach to RDF:

### Composables

Each function has a specific purpose:

```javascript
const store = useStore();        // Data storage
const graph = useGraph(store);   // Graph operations
const turtle = useTurtle();      // Format handling
const validator = useValidator(); // Validation
const reasoner = useReasoner();  // Reasoning
const zodHelper = useZod();      // Type safety
```

### Type Safety

Integration with Zod for runtime validation:

```javascript
const PersonSchema = z.object({
  id: z.string().url(),
  name: z.string().min(1),
  age: z.number().int().min(0).max(150).optional()
});

const quads = zodHelper.toRdf(personData, PersonSchema);
```

### Modern JavaScript

ES modules, async/await, and modern patterns:

```javascript
// Async/await for all operations
const quads = await turtle.parse(turtleData);
await graph.addQuads(quads);

// Streaming for large datasets
for await (const binding of graph.query(sparql)) {
  console.log(binding.get('name').value);
}
```

## Best Practices

### 1. Use Meaningful IRIs

```javascript
// Good
"http://example.org/people/john-doe"

// Avoid
"http://example.org/p1"
```

### 2. Use Appropriate Literal Types

```javascript
// Good
"30"^^xsd:integer
"2023-01-01"^^xsd:date
"true"^^xsd:boolean

// Avoid
"30"  // String instead of integer
```

### 3. Organize with Namespaces

```javascript
// Good
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

// Avoid
<http://example.org/person1> <http://xmlns.com/foaf/0.1/name> "John" .
```

### 4. Validate Your Data

```javascript
// Always validate RDF data
const result = await validator.validate(dataQuads, shapeQuads);
if (!result.conforms) {
  console.log('Validation errors:', result.results);
}
```

### 5. Use Type Safety

```javascript
// Define schemas for your data
const PersonSchema = z.object({
  id: z.string().url(),
  name: z.string().min(1)
});

// Validate before processing
const validated = PersonSchema.parse(personData);
```

## Common Patterns

### 1. Loading and Querying Data

```javascript
// Load data
const quads = await turtle.parse(turtleData);
await graph.addQuads(quads);

// Query data
const results = await graph.query(sparqlQuery);
for await (const binding of results) {
  console.log(binding.get('name').value);
}
```

### 2. Data Validation

```javascript
// Validate against SHACL shapes
const result = await validator.validate(dataQuads, shapeQuads);
if (result.conforms) {
  console.log('Data is valid');
} else {
  console.log('Validation errors:', result.results);
}
```

### 3. Reasoning and Inference

```javascript
// Perform OWL reasoning
const reasonedQuads = await reasoner.reason(originalQuads);
await graph.addQuads(reasonedQuads);
```

### 4. Type-Safe Operations

```javascript
// Convert between RDF and structured data
const quads = zodHelper.toRdf(structuredData, schema);
const reconstructed = zodHelper.fromRdf(quads, schema);
```

## Next Steps

Now that you understand the core concepts:

1. **Try the [Getting Started Guide](./getting-started.md)** for hands-on experience
2. **Explore [SPARQL Examples](../examples/sparql.md)** for query patterns
3. **Learn about [Validation](./validation.md)** for data quality
4. **Discover [Reasoning](./reasoning.md)** for inference
5. **Check out the [API Reference](../api/)** for detailed documentation

Understanding these core concepts will help you work effectively with UNRDF and RDF in general. The composable nature of UNRDF makes it easy to build complex RDF applications while maintaining clean, readable code.
