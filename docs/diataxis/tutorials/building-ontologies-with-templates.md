# Building Ontologies with Templates

**Duration:** 20 minutes
**Level:** Intermediate
**Prerequisites:** Complete [RDF-KGN Quickstart](./rdf-kgn-quickstart.md)

## What You'll Learn

Learn how to build reusable RDF ontologies using template-driven generation:

- Define ontology classes and properties with templates
- Use Nunjucks template syntax for RDF generation
- Create custom vocabularies and namespaces
- Generate OWL ontologies from templates

## Overview

RDF-KGN allows you to define ontologies using templates instead of writing RDF by hand. This approach:

- **Reduces errors:** Templates enforce consistent structure
- **Improves maintainability:** Change once, regenerate everywhere
- **Enables versioning:** Track ontology changes via templates
- **Supports customization:** Variables make templates reusable

## Step 1: Define an Ontology Template (5 min)

Create `/tmp/ontology-template.njk`:

```nunjucks
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix {{ prefix }}: <{{ namespace }}> .

# Ontology Declaration
{{ namespace | rdfResource }}
    a owl:Ontology ;
    rdfs:label "{{ title }}" ;
    rdfs:comment "{{ description }}" ;
    owl:versionInfo "{{ version }}" .

{% for class in classes %}
# Class: {{ class.name }}
{{ prefix }}:{{ class.name }}
    a owl:Class ;
    rdfs:label "{{ class.label }}"@en ;
    rdfs:comment "{{ class.description }}"@en ;
    {% if class.subClassOf %}
    rdfs:subClassOf {{ prefix }}:{{ class.subClassOf }} ;
    {% endif %}
    .

{% endfor %}

{% for property in properties %}
# Property: {{ property.name }}
{{ prefix }}:{{ property.name }}
    a owl:{{ property.type | default('ObjectProperty') }} ;
    rdfs:label "{{ property.label }}"@en ;
    rdfs:comment "{{ property.description }}"@en ;
    {% if property.domain %}
    rdfs:domain {{ prefix }}:{{ property.domain }} ;
    {% endif %}
    {% if property.range %}
    rdfs:range {% if property.rangeType == 'xsd' %}xsd:{{ property.range }}{% else %}{{ prefix }}:{{ property.range }}{% endif %} ;
    {% endif %}
    .

{% endfor %}
```

This template defines:
- Ontology metadata (title, version, description)
- OWL classes with optional inheritance
- Properties with domain and range constraints

## Step 2: Render the Ontology (5 min)

Create `/tmp/render-ontology.mjs`:

```javascript
import { renderTemplate } from '@unrdf/kgn';
import { writeFileSync } from 'fs';

// Define ontology structure
const ontologyData = {
  prefix: 'ex',
  namespace: 'http://example.org/ontology#',
  title: 'Example Domain Ontology',
  description: 'An ontology for representing example domain concepts',
  version: '1.0.0',

  classes: [
    {
      name: 'Person',
      label: 'Person',
      description: 'A human being'
    },
    {
      name: 'Organization',
      label: 'Organization',
      description: 'A group of people with a common purpose'
    },
    {
      name: 'Employee',
      label: 'Employee',
      description: 'A person employed by an organization',
      subClassOf: 'Person'
    }
  ],

  properties: [
    {
      name: 'name',
      label: 'name',
      description: 'The name of an entity',
      type: 'DatatypeProperty',
      range: 'string',
      rangeType: 'xsd'
    },
    {
      name: 'worksFor',
      label: 'works for',
      description: 'Organization that employs the person',
      type: 'ObjectProperty',
      domain: 'Employee',
      range: 'Organization'
    },
    {
      name: 'employeeId',
      label: 'employee ID',
      description: 'Unique identifier for an employee',
      type: 'DatatypeProperty',
      domain: 'Employee',
      range: 'string',
      rangeType: 'xsd'
    },
    {
      name: 'founded',
      label: 'founded',
      description: 'The date the organization was founded',
      type: 'DatatypeProperty',
      domain: 'Organization',
      range: 'date',
      rangeType: 'xsd'
    }
  ]
};

// Render the ontology
const result = await renderTemplate(
  '/tmp/ontology-template.njk',
  ontologyData,
  {
    deterministicMode: true,
    templatesDir: '/tmp'
  }
);

// Save to file
writeFileSync('/tmp/example-ontology.ttl', result.content);

console.log('✅ Ontology generated successfully!');
console.log('📄 File: /tmp/example-ontology.ttl');
console.log(`📊 Classes: ${ontologyData.classes.length}`);
console.log(`📊 Properties: ${ontologyData.properties.length}`);
```

Run it:

```bash
cd /home/user/unrdf/packages/kgn
timeout 5s node /tmp/render-ontology.mjs
```

**Expected Output:**
```
✅ Ontology generated successfully!
📄 File: /tmp/example-ontology.ttl
📊 Classes: 3
📊 Properties: 4
```

Inspect the generated ontology:

```bash
cat /tmp/example-ontology.ttl
```

You should see a complete OWL ontology with classes and properties!

## Step 3: Use Custom RDF Filters (5 min)

RDF-KGN provides filters for working with RDF data in templates.

Create `/tmp/filtered-template.njk`:

```nunjucks
@prefix ex: <{{ baseIRI }}> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

{% for person in people %}
# Person: {{ person.name }}
ex:{{ person.name | pascalCase }}
    a ex:Person ;
    ex:name {{ person.name | rdfLiteral('en') }} ;
    ex:email {{ person.email | rdfLiteral }} ;
    ex:age {{ person.age | rdfDatatype('integer') }} ;
    {% if person.homepage %}
    ex:homepage {{ person.homepage | rdfResource }} ;
    {% endif %}
    .

{% endfor %}

# Person list
ex:PersonList
    a rdf:List ;
    rdf:value {{ people | map(attribute='name') | rdfList }} .
```

Create `/tmp/use-filters.mjs`:

```javascript
import { TemplateEngine } from '@unrdf/kgn';
import { rdfFilters } from '@unrdf/kgn/src/filters/rdf.js';

const engine = new TemplateEngine({
  templatesDir: '/tmp',
  deterministicMode: true
});

// Register RDF filters
Object.entries(rdfFilters).forEach(([name, filter]) => {
  engine.env.addFilter(name, filter);
});

const data = {
  baseIRI: 'http://example.org/',
  people: [
    {
      name: 'alice smith',
      email: 'alice@example.org',
      age: 30,
      homepage: 'https://alice.example.org'
    },
    {
      name: 'bob jones',
      email: 'bob@example.org',
      age: 25
    }
  ]
};

const result = await engine.render('filtered-template.njk', data);

console.log('Generated RDF with filters:');
console.log(result);
```

Run it:

```bash
cd /home/user/unrdf/packages/kgn
timeout 5s node /tmp/use-filters.mjs
```

**Expected Output:**
```turtle
ex:AliceSmith
    a ex:Person ;
    ex:name "alice smith"@en ;
    ex:email "alice@example.org" ;
    ex:age "30"^^xsd:integer ;
    ex:homepage <https://alice.example.org> .

ex:BobJones
    a ex:Person ;
    ex:name "bob jones"@en ;
    ex:email "bob@example.org" ;
    ex:age "25"^^xsd:integer .
```

**What happened?**

- `pascalCase` filter: Converted "alice smith" → "AliceSmith"
- `rdfLiteral('en')`: Added language tag `@en`
- `rdfDatatype('integer')`: Added XSD datatype `^^xsd:integer`
- `rdfResource`: Wrapped URL in angle brackets

## Step 4: Query the Generated Ontology (5 min)

Now let's query the ontology we created.

Create `/tmp/query-ontology.mjs`:

```javascript
import { createStore } from '@unrdf/oxigraph';
import { executeSelectSync } from '@unrdf/core/sparql/executor-sync.mjs';
import { Parser } from 'n3';
import { readFileSync } from 'fs';

// Load the ontology
const ontology = readFileSync('/tmp/example-ontology.ttl', 'utf-8');

const store = createStore();
const parser = new Parser({ format: 'text/turtle' });

parser.parse(ontology).forEach(quad => {
  store.add(quad);
});

// Query 1: Find all classes
console.log('=== Query 1: All Classes ===');
const classQuery = `
  PREFIX owl: <http://www.w3.org/2002/07/owl#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

  SELECT ?class ?label ?description
  WHERE {
    ?class a owl:Class ;
           rdfs:label ?label ;
           rdfs:comment ?description .
  }
`;

executeSelectSync(store, classQuery).forEach(row => {
  console.log(`Class: ${row.label.value}`);
  console.log(`  Description: ${row.description.value}\n`);
});

// Query 2: Find properties with their domains and ranges
console.log('=== Query 2: Properties with Domain/Range ===');
const propQuery = `
  PREFIX owl: <http://www.w3.org/2002/07/owl#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

  SELECT ?prop ?label ?domain ?range
  WHERE {
    ?prop a ?type ;
          rdfs:label ?label .
    FILTER(?type = owl:ObjectProperty || ?type = owl:DatatypeProperty)
    OPTIONAL { ?prop rdfs:domain ?domain }
    OPTIONAL { ?prop rdfs:range ?range }
  }
  ORDER BY ?label
`;

executeSelectSync(store, propQuery).forEach(row => {
  console.log(`Property: ${row.label.value}`);
  if (row.domain) console.log(`  Domain: ${row.domain.value}`);
  if (row.range) console.log(`  Range: ${row.range.value}`);
  console.log();
});
```

Run it:

```bash
timeout 5s node /tmp/query-ontology.mjs
```

**Expected Output:**
```
=== Query 1: All Classes ===
Class: Person
  Description: A human being

Class: Organization
  Description: A group of people with a common purpose

Class: Employee
  Description: A person employed by an organization

=== Query 2: Properties with Domain/Range ===
Property: employee ID
  Domain: http://example.org/ontology#Employee
  Range: http://www.w3.org/2001/XMLSchema#string

Property: founded
  Domain: http://example.org/ontology#Organization
  Range: http://www.w3.org/2001/XMLSchema#date
```

## Summary

You've learned how to:

✅ Define ontology templates with Nunjucks
✅ Render OWL ontologies from structured data
✅ Use RDF filters for proper RDF formatting
✅ Query generated ontologies with SPARQL

## Next Steps

- **Tutorial:** [SPARQL Query Generation](./sparql-query-generation.md) - 15 min
- **Tutorial:** [RDF Validation Workflow](./rdf-validation-workflow.md) - 25 min
- **How-to:** [Optimize RDF Serialization](../how-to/optimize-rdf-serialization.md)

## Key Takeaways

1. **Templates are reusable:** Define once, generate many variations
2. **Filters ensure correctness:** Use `rdfLiteral`, `rdfResource`, etc.
3. **Deterministic mode:** Guarantees identical output across runs
4. **SPARQL validates structure:** Query to verify ontology correctness

## Reference

- [RDF-KGN API](../reference/rdf-kgn-api.md)
- [RDF Filters Reference](../reference/rdf-filters-reference.md)
- [RDF-KGN Architecture](../explanation/rdf-kgn-architecture.md)
