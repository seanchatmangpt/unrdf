# RDF Filters Reference

Complete reference for RDF template filters in RDF-KGN.

## Overview

RDF filters are Nunjucks template filters for generating valid RDF syntax. They ensure correct formatting of URIs, literals, datatypes, and SPARQL variables.

## Installation

```javascript
import { rdfFilters } from '@unrdf/kgn/src/filters/rdf.js';
import { TemplateEngine } from '@unrdf/kgn';

const engine = new TemplateEngine({ templatesDir: './templates' });

// Register all RDF filters
Object.entries(rdfFilters).forEach(([name, filter]) => {
  engine.env.addFilter(name, filter);
});
```

## URI Filters

### `expand`

Expand CURIE (Compact URI) to full URI.

**Signature:**
```javascript
expand(curie, prefixes = {})
```

**Parameters:**
- `curie` (string) - CURIE in format `'prefix:localName'`
- `prefixes` (Object) - Custom prefix mappings (optional)

**Returns:** `string` - Expanded URI or original if no prefix found

**Default Prefixes:**
- `rdf`, `rdfs`, `owl`, `xsd`, `dc`, `foaf`, `skos`, `schema`, `ex`

**Template Usage:**
```nunjucks
{{ 'foaf:Person' | expand }}
<!-- http://xmlns.com/foaf/0.1/Person -->

{{ 'schema:Person' | expand }}
<!-- https://schema.org/Person -->

{{ 'myprefix:Thing' | expand({ myprefix: 'http://example.org/' }) }}
<!-- http://example.org/Thing -->
```

**JavaScript Usage:**
```javascript
import { expand } from '@unrdf/kgn/src/filters/rdf.js';

console.log(expand('foaf:Person'));
// http://xmlns.com/foaf/0.1/Person

console.log(expand('custom:Thing', { custom: 'http://example.org/' }));
// http://example.org/Thing
```

### `contract`

Contract full URI to CURIE if possible.

**Signature:**
```javascript
contract(uri, prefixes = {})
```

**Parameters:**
- `uri` (string) - Full URI
- `prefixes` (Object) - Custom prefix mappings (optional)

**Returns:** `string` - CURIE or original URI if no prefix matches

**Template Usage:**
```nunjucks
{{ 'http://xmlns.com/foaf/0.1/Person' | contract }}
<!-- foaf:Person -->

{{ 'https://schema.org/Person' | contract }}
<!-- schema:Person -->
```

**JavaScript Usage:**
```javascript
import { contract } from '@unrdf/kgn/src/filters/rdf.js';

console.log(contract('http://xmlns.com/foaf/0.1/Person'));
// foaf:Person

console.log(contract('http://example.org/Thing'));
// http://example.org/Thing (no prefix defined)
```

### `rdfResource`

Create RDF resource (URI reference).

**Signature:**
```javascript
rdfResource(uri)
```

**Parameters:**
- `uri` (string) - URI string

**Returns:** `string` - URI wrapped in angle brackets or CURIE as-is

**Template Usage:**
```nunjucks
{{ 'http://example.org/alice' | rdfResource }}
<!-- <http://example.org/alice> -->

{{ 'foaf:Person' | rdfResource }}
<!-- foaf:Person -->

{{ homepage | rdfResource }}
<!-- <https://alice.example.org> -->
```

**JavaScript Usage:**
```javascript
import { rdfResource } from '@unrdf/kgn/src/filters/rdf.js';

console.log(rdfResource('http://example.org/alice'));
// <http://example.org/alice>

console.log(rdfResource('foaf:Person'));
// foaf:Person
```

## Literal Filters

### `rdfLiteral`

Create RDF literal with optional language tag or datatype.

**Signature:**
```javascript
rdfLiteral(value, langOrType = null)
```

**Parameters:**
- `value` (any) - Literal value
- `langOrType` (string) - Language tag (e.g., `'en'`) or datatype CURIE (optional)

**Returns:** `string` - RDF literal string

**Template Usage:**
```nunjucks
{{ "Hello World" | rdfLiteral }}
<!-- "Hello World" -->

{{ "Hello World" | rdfLiteral('en') }}
<!-- "Hello World"@en -->

{{ "Bonjour" | rdfLiteral('fr') }}
<!-- "Bonjour"@fr -->

{{ name | rdfLiteral }}
<!-- "Alice Smith" -->
```

**JavaScript Usage:**
```javascript
import { rdfLiteral } from '@unrdf/kgn/src/filters/rdf.js';

console.log(rdfLiteral('Hello World'));
// "Hello World"

console.log(rdfLiteral('Hello World', 'en'));
// "Hello World"@en

console.log(rdfLiteral('Bonjour', 'fr'));
// "Bonjour"@fr
```

### `rdfDatatype`

Add RDF datatype to literal.

**Signature:**
```javascript
rdfDatatype(value, datatype = 'string')
```

**Parameters:**
- `value` (any) - Value to type
- `datatype` (string) - XSD datatype (without `xsd:` prefix)

**Returns:** `string` - Typed literal

**Common Datatypes:**
- `string`, `integer`, `decimal`, `boolean`, `date`, `dateTime`, `time`
- `float`, `double`, `byte`, `short`, `int`, `long`
- `unsignedByte`, `unsignedShort`, `unsignedInt`, `unsignedLong`

**Template Usage:**
```nunjucks
{{ 30 | rdfDatatype('integer') }}
<!-- "30"^^xsd:integer -->

{{ 3.14 | rdfDatatype('decimal') }}
<!-- "3.14"^^xsd:decimal -->

{{ true | rdfDatatype('boolean') }}
<!-- "true"^^xsd:boolean -->

{{ "2024-01-01" | rdfDatatype('date') }}
<!-- "2024-01-01"^^xsd:date -->

{{ age | rdfDatatype('integer') }}
<!-- "25"^^xsd:integer -->
```

**JavaScript Usage:**
```javascript
import { rdfDatatype } from '@unrdf/kgn/src/filters/rdf.js';

console.log(rdfDatatype(30, 'integer'));
// "30"^^xsd:integer

console.log(rdfDatatype(true, 'boolean'));
// "true"^^xsd:boolean
```

## String Filters

### `turtleEscape`

Escape string for Turtle/N3 syntax.

**Signature:**
```javascript
turtleEscape(str)
```

**Parameters:**
- `str` (string) - String to escape

**Returns:** `string` - Escaped string

**Escapes:**
- `\` → `\\`
- `"` → `\"`
- `\n` → `\\n`
- `\r` → `\\r`
- `\t` → `\\t`

**Template Usage:**
```nunjucks
{{ 'Line 1\nLine 2' | turtleEscape }}
<!-- Line 1\\nLine 2 -->

{{ 'She said "Hello"' | turtleEscape }}
<!-- She said \\"Hello\\" -->
```

**JavaScript Usage:**
```javascript
import { turtleEscape } from '@unrdf/kgn/src/filters/rdf.js';

console.log(turtleEscape('Line 1\nLine 2'));
// Line 1\\nLine 2
```

## SPARQL Filters

### `sparqlVar`

Create SPARQL variable with valid name.

**Signature:**
```javascript
sparqlVar(name)
```

**Parameters:**
- `name` (string) - Variable name (with or without `?` prefix)

**Returns:** `string` - SPARQL variable with `?` prefix

**Name Sanitization:**
- Removes existing `?` or `$` prefix
- Replaces invalid characters with `_`
- Ensures alphanumeric + underscore only

**Template Usage:**
```nunjucks
{{ 'person' | sparqlVar }}
<!-- ?person -->

{{ '?name' | sparqlVar }}
<!-- ?name -->

{{ 'my-var' | sparqlVar }}
<!-- ?my_var -->

SELECT {{ vars | map('sparqlVar') | join(' ') }}
<!-- SELECT ?var1 ?var2 ?var3 -->
```

**JavaScript Usage:**
```javascript
import { sparqlVar } from '@unrdf/kgn/src/filters/rdf.js';

console.log(sparqlVar('person'));
// ?person

console.log(sparqlVar('my-variable'));
// ?my_variable
```

### `sparql`

Execute mock SPARQL query (deterministic results for templates).

**Signature:**
```javascript
async sparql(query, dataset = null)
```

**Parameters:**
- `query` (string) - SPARQL query string
- `dataset` (any) - Mock dataset (ignored in stub)

**Returns:** `Promise<Array>` - Mock query results

**Template Usage:**
```nunjucks
{% set results = 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }' | sparql | await %}
{% for row in results %}
  {{ row.s }} {{ row.p }} {{ row.o }}
{% endfor %}
```

**JavaScript Usage:**
```javascript
import { sparql } from '@unrdf/kgn/src/filters/rdf.js';

const results = await sparql('SELECT ?s ?p ?o WHERE { ?s ?p ?o }');
console.log(results);
// Mock results based on query hash
```

## Collection Filters

### `rdfList`

Create RDF list (collection).

**Signature:**
```javascript
rdfList(items)
```

**Parameters:**
- `items` (Array) - Array items

**Returns:** `string` - RDF list syntax

**Template Usage:**
```nunjucks
{{ ['ex:a', 'ex:b', 'ex:c'] | rdfList }}
<!-- ( ex:a ex:b ex:c ) -->

{{ [] | rdfList }}
<!-- rdf:nil -->

ex:MyList rdf:value {{ items | rdfList }} .
```

**JavaScript Usage:**
```javascript
import { rdfList } from '@unrdf/kgn/src/filters/rdf.js';

console.log(rdfList(['ex:a', 'ex:b', 'ex:c']));
// ( ex:a ex:b ex:c )

console.log(rdfList([]));
// rdf:nil
```

### `blankNode`

Create blank node identifier.

**Signature:**
```javascript
blankNode(id = null)
```

**Parameters:**
- `id` (string) - Optional identifier

**Returns:** `string` - Blank node with `_:` prefix

**Deterministic:**
- With `id`: Uses sanitized ID
- Without `id`: Uses deterministic timestamp-based ID

**Template Usage:**
```nunjucks
{{ 'address' | blankNode }}
<!-- _:address -->

{{ null | blankNode }}
<!-- _:blank4321 (deterministic) -->

ex:person foaf:address {{ 'addr_1' | blankNode }} .
{{ 'addr_1' | blankNode }}
  vcard:street "123 Main St" ;
  vcard:city "Springfield" .
```

**JavaScript Usage:**
```javascript
import { blankNode } from '@unrdf/kgn/src/filters/rdf.js';

console.log(blankNode('address'));
// _:address

console.log(blankNode());
// _:blank4321 (deterministic)
```

## Validation Filters

### `shaclValidate`

Mock SHACL validation (always passes in stub).

**Signature:**
```javascript
shaclValidate(data, shape)
```

**Parameters:**
- `data` (any) - Data to validate
- `shape` (any) - SHACL shape

**Returns:** `Object`
- `conforms` (boolean) - Always `true` in mock
- `results` (Array) - Empty in mock
- `message` (string) - Status message

**Template Usage:**
```nunjucks
{% set validation = data | shaclValidate(shape) %}
{% if validation.conforms %}
  ✅ Data is valid
{% else %}
  ❌ Validation failed
{% endif %}
```

**JavaScript Usage:**
```javascript
import { shaclValidate } from '@unrdf/kgn/src/filters/rdf.js';

const result = shaclValidate(myData, myShape);
console.log(result.conforms); // true (mock)
```

## Reasoning Filters

### `infer`

Mock reasoning/inference (no-op in stub).

**Signature:**
```javascript
infer(graph, reasoner = 'rdfs')
```

**Parameters:**
- `graph` (any) - RDF graph
- `reasoner` (string) - Reasoner type (`'rdfs'`, `'owl'`, etc.)

**Returns:** `any` - Original graph (no inference in mock)

**Template Usage:**
```nunjucks
{% set inferredGraph = graph | infer('rdfs') %}
```

**JavaScript Usage:**
```javascript
import { infer } from '@unrdf/kgn/src/filters/rdf.js';

const inferredGraph = infer(myGraph, 'rdfs');
// Returns myGraph unchanged in stub
```

## Filter Combinations

### Common Patterns

#### Pattern 1: Person with Language-Tagged Name

```nunjucks
ex:{{ person.id }}
  a foaf:Person ;
  foaf:name {{ person.name | rdfLiteral('en') }} ;
  foaf:mbox {{ person.email | rdfResource }} ;
  foaf:age {{ person.age | rdfDatatype('integer') }} .
```

#### Pattern 2: Document with Typed Properties

```nunjucks
ex:{{ doc.id }}
  a schema:Document ;
  schema:title {{ doc.title | rdfLiteral }} ;
  schema:datePublished {{ doc.published | rdfDatatype('date') }} ;
  schema:wordCount {{ doc.wordCount | rdfDatatype('integer') }} ;
  schema:url {{ doc.url | rdfResource }} .
```

#### Pattern 3: Address with Blank Nodes

```nunjucks
ex:{{ person.id }}
  vcard:hasAddress {{ person.id + '_address' | blankNode }} .

{{ person.id + '_address' | blankNode }}
  a vcard:Address ;
  vcard:street-address {{ address.street | rdfLiteral }} ;
  vcard:locality {{ address.city | rdfLiteral }} ;
  vcard:postal-code {{ address.zip | rdfLiteral }} .
```

#### Pattern 4: Collection with List

```nunjucks
ex:Team
  a org:Team ;
  org:hasMember {{ members | map('id') | map('contract') | rdfList }} .
```

## Default Prefixes

The following prefixes are available by default in `expand` and `contract` filters:

| Prefix | Namespace |
|--------|-----------|
| `rdf` | `http://www.w3.org/1999/02/22-rdf-syntax-ns#` |
| `rdfs` | `http://www.w3.org/2000/01/rdf-schema#` |
| `owl` | `http://www.w3.org/2002/07/owl#` |
| `xsd` | `http://www.w3.org/2001/XMLSchema#` |
| `dc` | `http://purl.org/dc/elements/1.1/` |
| `foaf` | `http://xmlns.com/foaf/0.1/` |
| `skos` | `http://www.w3.org/2004/02/skos/core#` |
| `ex` | `http://example.org/` |
| `schema` | `https://schema.org/` |

## Best Practices

### 1. Always Use Filters for RDF Values

```nunjucks
{# ❌ Bad: Raw values #}
ex:alice foaf:name "Alice Smith" .
ex:alice foaf:age 30 .

{# ✅ Good: Filtered values #}
ex:alice foaf:name {{ "Alice Smith" | rdfLiteral }} .
ex:alice foaf:age {{ 30 | rdfDatatype('integer') }} .
```

### 2. Use Language Tags for Human-Readable Text

```nunjucks
{# ✅ Good: Language-tagged literals #}
ex:article
  rdfs:label {{ title_en | rdfLiteral('en') }} ;
  rdfs:label {{ title_fr | rdfLiteral('fr') }} ;
  rdfs:label {{ title_de | rdfLiteral('de') }} .
```

### 3. Use Datatypes for Structured Data

```nunjucks
{# ✅ Good: Proper datatypes #}
ex:event
  schema:startDate {{ startDate | rdfDatatype('dateTime') }} ;
  schema:duration {{ duration | rdfDatatype('duration') }} ;
  schema:attendeeCount {{ count | rdfDatatype('integer') }} .
```

### 4. Contract URIs for Readability

```nunjucks
{# Define contract mapping #}
{% set myPrefixes = { 'myapp': 'http://example.org/myapp/' } %}

{# ✅ Good: Contracted URIs #}
{{ 'http://example.org/myapp/Person' | contract(myPrefixes) }}
<!-- myapp:Person -->
```

## Error Handling

### Null/Undefined Values

All filters handle null/undefined gracefully:

```javascript
console.log(rdfLiteral(null));       // ""
console.log(rdfResource(undefined)); // "<>"
console.log(expand(null));           // ""
console.log(sparqlVar(null));        // "?var"
```

### Invalid Input Types

Filters convert invalid inputs to strings:

```javascript
console.log(rdfLiteral(123));        // "123"
console.log(rdfLiteral(true));       // "true"
console.log(rdfLiteral({ a: 1 }));   // "[object Object]"
```

## Custom Filter Registration

### Register Individual Filter

```javascript
import { TemplateEngine } from '@unrdf/kgn';
import { rdfLiteral } from '@unrdf/kgn/src/filters/rdf.js';

const engine = new TemplateEngine();
engine.env.addFilter('rdfLiteral', rdfLiteral);
```

### Register All Filters

```javascript
import { TemplateEngine } from '@unrdf/kgn';
import { rdfFilters } from '@unrdf/kgn/src/filters/rdf.js';

const engine = new TemplateEngine();

Object.entries(rdfFilters).forEach(([name, filter]) => {
  engine.env.addFilter(name, filter);
});
```

### Create Custom Filter

```javascript
function customRdfFilter(value, options = {}) {
  // Your custom logic
  return processedValue;
}

engine.env.addFilter('customRdf', customRdfFilter);
```

## See Also

- [RDF-KGN API](./rdf-kgn-api.md)
- [Generate RDF from Templates How-To](../how-to/generate-rdf-from-templates.md)
- [Template-Driven RDF Generation](../explanation/template-driven-rdf-generation.md)
