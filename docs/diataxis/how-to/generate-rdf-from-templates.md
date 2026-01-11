# How to Generate RDF from Templates

**Goal:** Generate RDF triples using template-driven approach
**Time:** 5-10 minutes
**Difficulty:** Beginner

## Problem

You need to generate RDF data programmatically from structured input, but writing RDF by hand is error-prone and hard to maintain.

## Solution

Use RDF-KGN templates with Nunjucks syntax to generate RDF triples deterministically.

## Prerequisites

- `@unrdf/kgn` package installed
- Basic understanding of RDF and Turtle syntax

## Step-by-Step Guide

### 1. Create an RDF Template

Create a file `person-template.njk`:

```nunjucks
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

{% for person in people %}
ex:{{ person.id }}
    a foaf:Person ;
    foaf:name {{ person.name | rdfLiteral }} ;
    foaf:mbox {{ person.email | rdfResource }} ;
    {% if person.age %}
    foaf:age {{ person.age | rdfDatatype('integer') }} ;
    {% endif %}
    {% if person.knows %}
    {% for friend in person.knows %}
    foaf:knows ex:{{ friend }} ;
    {% endfor %}
    {% endif %}
    .

{% endfor %}
```

### 2. Prepare Input Data

Create `generate-rdf.mjs`:

```javascript
import { renderTemplate } from '@unrdf/kgn';
import { rdfFilters } from '@unrdf/kgn/src/filters/rdf.js';

const data = {
  people: [
    {
      id: 'alice',
      name: 'Alice Smith',
      email: 'mailto:alice@example.org',
      age: 30,
      knows: ['bob', 'charlie']
    },
    {
      id: 'bob',
      name: 'Bob Jones',
      email: 'mailto:bob@example.org',
      age: 25
    },
    {
      id: 'charlie',
      name: 'Charlie Brown',
      email: 'mailto:charlie@example.org',
      knows: ['alice']
    }
  ]
};

const result = await renderTemplate(
  'person-template.njk',
  data,
  {
    deterministicMode: true,
    templatesDir: '.'
  }
);

console.log(result.content);
```

### 3. Run Generation

```bash
timeout 5s node generate-rdf.mjs > output.ttl
```

### 4. Verify Output

```bash
cat output.ttl
```

**Expected result:**
```turtle
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:alice
    a foaf:Person ;
    foaf:name "Alice Smith" ;
    foaf:mbox <mailto:alice@example.org> ;
    foaf:age "30"^^xsd:integer ;
    foaf:knows ex:bob ;
    foaf:knows ex:charlie .

ex:bob
    a foaf:Person ;
    foaf:name "Bob Jones" ;
    foaf:mbox <mailto:bob@example.org> ;
    foaf:age "25"^^xsd:integer .

ex:charlie
    a foaf:Person ;
    foaf:name "Charlie Brown" ;
    foaf:mbox <mailto:charlie@example.org> ;
    foaf:knows ex:alice .
```

## Using RDF Filters

### Available Filters

| Filter | Purpose | Example |
|--------|---------|---------|
| `rdfLiteral` | Create RDF literal | `{{ name \| rdfLiteral }}` → `"value"` |
| `rdfLiteral('en')` | Literal with lang tag | `{{ name \| rdfLiteral('en') }}` → `"value"@en` |
| `rdfDatatype('type')` | Typed literal | `{{ age \| rdfDatatype('integer') }}` → `"25"^^xsd:integer` |
| `rdfResource` | IRI reference | `{{ url \| rdfResource }}` → `<url>` |
| `expand` | Expand CURIE | `{{ 'foaf:Person' \| expand }}` |
| `contract` | Contract to CURIE | `{{ uri \| contract }}` |
| `blankNode` | Create blank node | `{{ id \| blankNode }}` → `_:id_hash` |

### Example with All Filters

```nunjucks
@prefix ex: <http://example.org/> .

ex:Resource
    ex:title {{ title | rdfLiteral('en') }} ;
    ex:count {{ count | rdfDatatype('integer') }} ;
    ex:homepage {{ homepage | rdfResource }} ;
    ex:type {{ 'foaf:Document' | expand }} ;
    ex:reference {{ nodeId | blankNode }} .
```

## Common Patterns

### Pattern 1: Conditional Properties

```nunjucks
{% if person.phone %}
foaf:phone {{ person.phone | rdfLiteral }} ;
{% endif %}
```

### Pattern 2: Multiple Values

```nunjucks
{% for tag in article.tags %}
dc:subject {{ tag | rdfLiteral }} ;
{% endfor %}
```

### Pattern 3: Nested Resources

```nunjucks
ex:{{ person.id }}
    foaf:knows [
        a foaf:Person ;
        foaf:name {{ person.friend.name | rdfLiteral }}
    ] .
```

### Pattern 4: Blank Nodes for Complex Values

```nunjucks
ex:{{ person.id }}
    foaf:address [
        a vcard:Address ;
        vcard:street {{ address.street | rdfLiteral }} ;
        vcard:city {{ address.city | rdfLiteral }}
    ] .
```

## Troubleshooting

### Problem: Invalid RDF syntax

**Symptom:** Parser errors when loading generated RDF

**Solution:** Ensure you're using RDF filters:
- Use `rdfLiteral` for strings
- Use `rdfResource` for URIs
- Use `rdfDatatype` for typed values

### Problem: Duplicate prefixes

**Symptom:** Multiple `@prefix` declarations

**Solution:** Define prefixes once at the top of the template:

```nunjucks
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

{# Content here #}
```

### Problem: Non-deterministic output

**Symptom:** Output changes between runs

**Solution:** Enable deterministic mode:

```javascript
renderTemplate(template, data, { deterministicMode: true })
```

## Advanced Techniques

### Technique 1: Template Inheritance

Create base template `base.njk`:

```nunjucks
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

{% block prefixes %}{% endblock %}

{% block content %}{% endblock %}
```

Extend in `person.njk`:

```nunjucks
{% extends "base.njk" %}

{% block prefixes %}
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
{% endblock %}

{% block content %}
{{ person.id }} a foaf:Person .
{% endblock %}
```

### Technique 2: Macros for Reusable Patterns

```nunjucks
{% macro rdfTriple(subject, predicate, object) %}
{{ subject }} {{ predicate }} {{ object }} .
{% endmacro %}

{{ rdfTriple('ex:alice', 'foaf:name', '"Alice"') }}
{{ rdfTriple('ex:bob', 'foaf:name', '"Bob"') }}
```

### Technique 3: Include Shared Vocabularies

Create `vocabs.njk`:

```nunjucks
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix dc: <http://purl.org/dc/elements/1.1/> .
```

Use in templates:

```nunjucks
{% include "vocabs.njk" %}

ex:Resource a foaf:Document .
```

## Best Practices

1. **Use filters consistently:** Always use `rdfLiteral`, `rdfResource`, etc.
2. **Enable deterministic mode:** Ensures reproducible output
3. **Validate generated RDF:** Parse with an RDF library to catch errors
4. **Version templates:** Track changes with git
5. **Document variables:** Add comments explaining template variables
6. **Test with edge cases:** Empty arrays, null values, special characters

## Performance Tips

- **Cache templates:** Reuse `TemplateEngine` instance
- **Batch generation:** Process multiple entities in one template
- **Stream large datasets:** Use streaming APIs for millions of triples
- **Precompile templates:** Load templates once, render many times

## Related Guides

- [Build SPARQL Queries](./build-sparql-queries.md)
- [Validate RDF with SHACL](./validate-rdf-with-shacl.md)
- [Optimize RDF Serialization](./optimize-rdf-serialization.md)

## Reference

- [RDF-KGN API](../reference/rdf-kgn-api.md)
- [RDF Filters Reference](../reference/rdf-filters-reference.md)
- [Template-Driven RDF Generation](../explanation/template-driven-rdf-generation.md)
