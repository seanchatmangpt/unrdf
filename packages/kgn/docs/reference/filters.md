# Filter Reference

Complete reference for all filters available in @unrdf/kgn.

## RDF Filters

### `expand(curie, prefixes)`

Expands a CURIE (Compact URI) to a full URI using provided prefix mappings.

**Signature:**
```typescript
expand(curie: string, prefixes?: Record<string, string>): string
```

**Parameters:**
- `curie` - CURIE in format `prefix:localName`
- `prefixes` - Optional custom prefix mappings (uses defaults if not provided)

**Returns:** Expanded URI string

**Examples:**
```nunjucks
{{ "ex:Person" | expand(prefixes) }}
{# → "http://example.org/Person" #}

{{ "foaf:name" | expand }}
{# → "http://xmlns.com/foaf/0.1/name" #}
```

**Default Prefixes:**
```javascript
{
  'rdf': 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  'rdfs': 'http://www.w3.org/2000/01/rdf-schema#',
  'owl': 'http://www.w3.org/2002/07/owl#',
  'xsd': 'http://www.w3.org/2001/XMLSchema#',
  'dc': 'http://purl.org/dc/elements/1.1/',
  'foaf': 'http://xmlns.com/foaf/0.1/',
  'skos': 'http://www.w3.org/2004/02/skos/core#',
  'ex': 'http://example.org/',
  'schema': 'https://schema.org/'
}
```

---

### `contract(uri, prefixes)`

Contracts a full URI to a CURIE if a matching prefix is found.

**Signature:**
```typescript
contract(uri: string, prefixes?: Record<string, string>): string
```

**Parameters:**
- `uri` - Full URI to contract
- `prefixes` - Optional custom prefix mappings

**Returns:** CURIE string or original URI if no prefix matches

**Examples:**
```nunjucks
{{ "http://xmlns.com/foaf/0.1/Person" | contract }}
{# → "foaf:Person" #}

{{ "http://example.org/custom" | contract(prefixes) }}
{# → "ex:custom" #}
```

---

### `rdfLiteral(value, langOrType)`

Creates an RDF literal with optional language tag or datatype.

**Signature:**
```typescript
rdfLiteral(value: any, langOrType?: string): string
```

**Parameters:**
- `value` - Value to convert to RDF literal
- `langOrType` - Optional language tag (e.g., `'en'`) or datatype CURIE

**Returns:** RDF literal string with quotes

**Examples:**
```nunjucks
{{ "Hello World" | rdfLiteral }}
{# → "\"Hello World\"" #}

{{ "Bonjour" | rdfLiteral('fr') }}
{# → "\"Bonjour\"@fr" #}

{{ 42 | rdfLiteral('xsd:integer') }}
{# → "\"42\"^^xsd:integer" #}
```

---

### `rdfResource(uri)`

Wraps a URI in angle brackets for RDF serialization.

**Signature:**
```typescript
rdfResource(uri: string): string
```

**Parameters:**
- `uri` - URI string to wrap

**Returns:** URI wrapped in `<` and `>`

**Examples:**
```nunjucks
{{ "http://example.org/resource" | rdfResource }}
{# → "<http://example.org/resource>" #}

{{ entity.uri | expand | rdfResource }}
{# → "<http://example.org/Person1>" #}
```

---

### `turtleEscape(str)`

Escapes a string for safe use in Turtle/N3 syntax.

**Signature:**
```typescript
turtleEscape(str: string): string
```

**Parameters:**
- `str` - String to escape

**Returns:** Escaped string

**Escapes:**
- `\` → `\\`
- `"` → `\"`
- `\n` → `\\n`
- `\r` → `\\r`
- `\t` → `\\t`

**Examples:**
```nunjucks
{{ "Line 1\nLine 2" | turtleEscape }}
{# → "Line 1\\nLine 2" #}

{{ 'She said "Hello"' | turtleEscape }}
{# → "She said \\"Hello\\"" #}
```

---

### `sparqlVar(name)`

Creates a SPARQL variable with `?` prefix.

**Signature:**
```typescript
sparqlVar(name: string): string
```

**Parameters:**
- `name` - Variable name

**Returns:** SPARQL variable string

**Examples:**
```nunjucks
{{ "subject" | sparqlVar }}
{# → "?subject" #}

{{ "person_name" | sparqlVar }}
{# → "?person_name" #}
```

---

### `rdfList(items)`

Creates an RDF list (collection) from an array.

**Signature:**
```typescript
rdfList(items: any[]): string
```

**Parameters:**
- `items` - Array of items

**Returns:** RDF list syntax

**Examples:**
```nunjucks
{{ ["a", "b", "c"] | rdfList }}
{# → "( a b c )" #}

{{ [] | rdfList }}
{# → "rdf:nil" #}
```

---

### `blankNode(id)`

Creates a blank node identifier.

**Signature:**
```typescript
blankNode(id?: string): string
```

**Parameters:**
- `id` - Optional identifier (sanitized automatically)

**Returns:** Blank node with `_:` prefix

**Examples:**
```nunjucks
{{ "node1" | blankNode }}
{# → "_:node1" #}

{{ blankNode() }}
{# → "_:blank1234" (deterministic) #}
```

---

### `rdfDatatype(value, datatype)`

Adds RDF datatype to a literal.

**Signature:**
```typescript
rdfDatatype(value: any, datatype?: string): string
```

**Parameters:**
- `value` - Value to type
- `datatype` - XSD datatype name (without `xsd:` prefix)

**Returns:** Typed literal

**Examples:**
```nunjucks
{{ 42 | rdfDatatype('integer') }}
{# → "\"42\"^^xsd:integer" #}

{{ "2024-01-01" | rdfDatatype('date') }}
{# → "\"2024-01-01\"^^xsd:date" #}
```

---

### `sparql(query, dataset)` (async)

Executes a mock SPARQL query with deterministic results.

**Signature:**
```typescript
async sparql(query: string, dataset?: any): Promise<any[]>
```

**Parameters:**
- `query` - SPARQL query string
- `dataset` - Mock dataset (currently ignored)

**Returns:** Array of query results (mock data)

**Note:** This is a stub implementation for testing. In production, connect to a real SPARQL endpoint.

**Examples:**
```nunjucks
{# Note: requires async rendering #}
{% set results = "SELECT ?s ?p ?o WHERE { ?s ?p ?o }" | sparql %}
{% for result in results %}
  {{ result.s }} {{ result.p }} {{ result.o }}
{% endfor %}
```

---

## Text Filters

(See `src/filters/text.js` for text manipulation filters)

---

## Array Filters

(See `src/filters/array.js` for array manipulation filters)

---

## Data Filters

(See `src/filters/data.js` for data transformation filters)

---

## Usage

### Import Individual Filters

```javascript
import { expand, contract, rdfLiteral } from '@unrdf/kgn/filters';
```

### Import All RDF Filters

```javascript
import { rdfFilters } from '@unrdf/kgn/filters';

// rdfFilters = {
//   expand, contract, rdfLiteral, rdfResource,
//   turtleEscape, sparqlVar, rdfList, blankNode,
//   rdfDatatype, shaclValidate, infer
// }
```

### Add to Nunjucks Environment

```javascript
import nunjucks from 'nunjucks';
import { expand, rdfLiteral } from '@unrdf/kgn/filters';

const env = new nunjucks.Environment();
env.addFilter('expand', expand);
env.addFilter('rdfLiteral', rdfLiteral);
```

## See Also

- [How-To: Generate TypeScript](../how-to/generate-typescript.md)
- [Tutorial: Using RDF Filters](../tutorial/03-rdf-filters.md)
- [Explanation: Filter Design](../explanation/filter-design.md)
