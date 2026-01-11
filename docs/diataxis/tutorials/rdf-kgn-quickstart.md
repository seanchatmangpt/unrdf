# RDF-KGN Quickstart Tutorial

**Duration:** 10 minutes
**Level:** Beginner
**Prerequisites:** Basic JavaScript knowledge

## What You'll Learn

In this quickstart, you'll learn how to:
- Generate RDF triples from JavaScript code documentation
- Query RDF data with SPARQL
- Validate RDF with SHACL shapes

## Setup

First, ensure you have the required packages:

```bash
cd /home/user/unrdf
pnpm install
```

## Step 1: Parse JavaScript Code to RDF (3 min)

Let's convert JavaScript documentation to RDF triples.

Create a file `/tmp/example.mjs`:

```javascript
/**
 * Calculate the sum of two numbers
 * @param {number} a - First number
 * @param {number} b - Second number
 * @returns {number} Sum of a and b
 * @example
 * const result = add(2, 3); // returns 5
 */
export function add(a, b) {
  return a + b;
}

/**
 * User class representing a person
 * @param {string} name - User's name
 */
export class User {
  constructor(name) {
    this.name = name;
  }

  /**
   * Greet the user
   * @returns {string} Greeting message
   */
  greet() {
    return `Hello, ${this.name}!`;
  }
}
```

Create a conversion script `/tmp/convert-to-rdf.mjs`:

```javascript
import { parseFile } from '@unrdf/kgn/src/doc-generator/parser.mjs';
import { buildRDFGraph } from '@unrdf/kgn/src/doc-generator/rdf-builder.mjs';

// Parse the JavaScript file
const parsed = parseFile('/tmp/example.mjs', '/tmp');

// Generate RDF triples in Turtle format
const rdf = buildRDFGraph(parsed);

console.log('Generated RDF:');
console.log(rdf);
```

Run it:

```bash
cd /home/user/unrdf/packages/kgn
timeout 5s node /tmp/convert-to-rdf.mjs
```

**Expected Output:**
```turtle
@prefix code: <http://unrdf.org/vocab/code#> .
@prefix fs: <http://unrdf.org/vocab/fs#> .
@prefix doc: <http://unrdf.org/vocab/doc#> .

# File: example.mjs
<http://unrdf.org/packages/example-mjs>
  a fs:File , code:Module ;
  fs:path "example.mjs" ;
  code:commentCount 2 .

# Export: add
<http://unrdf.org/packages/example-mjs#add>
  a code:Function ;
  code:name "add" ;
  doc:description "Calculate the sum of two numbers" ;
  code:param _:add_param_a_... ;
  code:param _:add_param_b_... ;
  code:returns "number" .
```

**What happened?** The parser extracted JSDoc comments and the RDF builder converted them to semantic triples!

## Step 2: Query RDF with SPARQL (4 min)

Now let's query the generated RDF data.

Create `/tmp/query-rdf.mjs`:

```javascript
import { createStore } from '@unrdf/oxigraph';
import { executeSelectSync } from '@unrdf/core/sparql/executor-sync.mjs';
import { Parser } from 'n3';

// Parse RDF from Step 1 (simplified example)
const turtleData = `
@prefix code: <http://unrdf.org/vocab/code#> .
@prefix doc: <http://unrdf.org/vocab/doc#> .

<http://unrdf.org/packages/example-mjs#add>
  a code:Function ;
  code:name "add" ;
  doc:description "Calculate the sum of two numbers" .

<http://unrdf.org/packages/example-mjs#User>
  a code:Class ;
  code:name "User" .
`;

// Create store and load data
const store = createStore();
const parser = new Parser({ format: 'text/turtle' });

parser.parse(turtleData).forEach(quad => {
  store.add(quad);
});

// Query for all functions
const query = `
  PREFIX code: <http://unrdf.org/vocab/code#>
  PREFIX doc: <http://unrdf.org/vocab/doc#>

  SELECT ?name ?description
  WHERE {
    ?func a code:Function ;
          code:name ?name ;
          doc:description ?description .
  }
`;

const results = executeSelectSync(store, query);

console.log('Functions found:');
results.forEach(row => {
  console.log(`- ${row.name.value}: ${row.description.value}`);
});
```

Run it:

```bash
timeout 5s node /tmp/query-rdf.mjs
```

**Expected Output:**
```
Functions found:
- add: Calculate the sum of two numbers
```

## Step 3: Validate with SHACL (3 min)

Create a SHACL shape to validate RDF data.

Create `/tmp/validate-rdf.mjs`:

```javascript
import { KGenSHACLTemplates } from '@unrdf/kgn/src/base/shacl-templates.js';

// Create SHACL template engine
const shacl = new KGenSHACLTemplates({
  namespace: 'http://example.org/shapes#',
  baseIRI: 'http://example.org/'
});

// Generate a function validation shape
const shape = shacl.generateShape('basic_node_shape', {
  shapeName: 'Function',
  targetClass: 'Function',
  description: 'Validates that functions have required properties',
  properties: [
    {
      path: 'name',
      datatype: 'xsd:string',
      minCount: 1,
      message: 'Function must have a name'
    },
    {
      path: 'description',
      datatype: 'xsd:string',
      minCount: 1,
      message: 'Function must have a description'
    }
  ]
});

console.log('Generated SHACL Shape:');
console.log(shape.content);
```

Run it:

```bash
cd /home/user/unrdf/packages/kgn
timeout 5s node /tmp/validate-rdf.mjs
```

**Expected Output:**
```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .

ex:FunctionShape
    a sh:NodeShape ;
    sh:targetClass ex:Function ;
    sh:property [
        sh:path ex:name ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
        sh:message "Function must have a name" ;
    ] .
```

## Next Steps

Congratulations! You've completed the quickstart. You now know how to:

- Convert JavaScript code to RDF triples
- Query RDF data with SPARQL
- Generate SHACL validation shapes

### Continue Learning

- **Tutorial:** [Building Ontologies with Templates](./building-ontologies-with-templates.md) - 20 min
- **Tutorial:** [SPARQL Query Generation](./sparql-query-generation.md) - 15 min
- **How-to:** [Generate RDF from Templates](../how-to/generate-rdf-from-templates.md)

### Key Concepts

- **RDF Triples:** Subject-Predicate-Object statements
- **SPARQL:** Query language for RDF data
- **SHACL:** Shape Constraint Language for validation
- **Turtle:** Human-readable RDF serialization format

## Troubleshooting

### Import errors

If you see module import errors, ensure you're in the correct directory:

```bash
cd /home/user/unrdf/packages/kgn
```

### Parser fails

Ensure the JavaScript file has valid JSDoc comments. The parser requires:
- `/**` comment blocks (not `//`)
- Valid `@param`, `@returns` tags

### Query returns no results

Check that:
- RDF data was loaded into the store
- SPARQL query uses correct prefix namespaces
- Property paths match the RDF structure

## Reference

- [RDF-KGN API Reference](../reference/rdf-kgn-api.md)
- [SPARQL Query Builder API](../reference/sparql-query-builder-api.md)
- [RDF Filters Reference](../reference/rdf-filters-reference.md)
