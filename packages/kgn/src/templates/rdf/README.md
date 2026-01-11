# RDF Templates for @unrdf/kgn

Comprehensive Nunjucks templates for generating RDF/Semantic Web content with frontmatter-driven configuration.

## Overview

This template library provides production-ready templates for common RDF formats and vocabularies:

- **ontology.njk** - OWL ontologies with classes and properties
- **schema.njk** - RDFS vocabularies and schemas
- **dataset.njk** - DCAT dataset metadata
- **vocabulary.njk** - SKOS concept schemes and taxonomies
- **shapes.njk** - SHACL validation shapes
- **sparql-queries.njk** - SPARQL query collections
- **jsonld-context.njk** - JSON-LD context mappings

All templates:
- Accept structured data via YAML frontmatter
- Generate valid RDF/Turtle or JSON-LD output
- Include comprehensive prefix declarations
- Support customization through variables
- Include inline documentation and examples

## Usage

### Basic Pattern

```javascript
import { createEngine } from '@unrdf/kgn';

const engine = createEngine({
  templatePaths: ['./node_modules/@unrdf/kgn/src/templates/rdf']
});

const output = await engine.render('ontology.njk', {
  ontologyIRI: 'http://example.org/myonto',
  title: 'My Ontology',
  classes: [
    {
      iri: 'http://example.org/myonto#Person',
      label: 'Person',
      comment: 'Represents a person'
    }
  ]
});
```

### With Frontmatter Files

Create a `.njk` file with frontmatter:

```yaml
---
ontologyIRI: http://example.org/ontology/library
version: 1.0.0
title: Library Ontology
description: An ontology for library systems
creator: http://example.org/people/librarian
created: 2026-01-11
classes:
  - iri: http://example.org/ontology/library#Book
    label: Book
    comment: Represents a book in the library
  - iri: http://example.org/ontology/library#Author
    label: Author
    comment: Represents an author
objectProperties:
  - iri: http://example.org/ontology/library#writtenBy
    label: written by
    comment: Links a book to its author
    domain: http://example.org/ontology/library#Book
    range: http://example.org/ontology/library#Author
---
```

Then render:

```javascript
const output = await engine.renderFile('my-ontology.njk');
```

## Template Reference

### 1. ontology.njk - OWL Ontology

Generates complete OWL ontologies with metadata, classes, object properties, and datatype properties.

**Required Variables:**
- `ontologyIRI` (string) - The IRI of the ontology
- `title` (string) - Ontology title

**Optional Variables:**
- `version` (string) - Version string (e.g., "1.0.0")
- `description` (string) - Ontology description
- `creator` (string) - Creator IRI
- `created` (string) - Creation date (ISO format)
- `license` (string) - License IRI
- `prefix` (string) - Short prefix for the ontology namespace
- `classes` (array) - Class definitions
  - `iri` - Class IRI
  - `label` - rdfs:label
  - `comment` - rdfs:comment (optional)
  - `subClassOf` - Parent class IRI (optional)
- `objectProperties` (array) - Object property definitions
  - `iri` - Property IRI
  - `label` - rdfs:label
  - `comment` - rdfs:comment (optional)
  - `domain` - Domain class IRI (optional)
  - `range` - Range class IRI (optional)
- `datatypeProperties` (array) - Datatype property definitions
  - `iri` - Property IRI
  - `label` - rdfs:label
  - `comment` - rdfs:comment (optional)
  - `domain` - Domain class IRI (optional)
  - `range` - XSD datatype IRI (optional)

**Example Output:**

```turtle
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

<http://example.org/ontology/myonto> a owl:Ontology ;
    dc:title "My Example Ontology"@en ;
    owl:versionInfo "1.0.0" .

<http://example.org/ontology/myonto#Person> a owl:Class ;
    rdfs:label "Person"@en ;
    rdfs:isDefinedBy <http://example.org/ontology/myonto> .
```

### 2. schema.njk - RDFS Schema

Generates RDFS vocabularies with classes and properties.

**Required Variables:**
- `schemaIRI` (string) - The IRI of the schema
- `title` (string) - Schema title

**Optional Variables:**
- `description` (string) - Schema description
- `version` (string) - Version string
- `prefix` (string) - Short prefix
- `classes` (array) - RDFS class definitions
  - `iri` - Class IRI
  - `label` - rdfs:label
  - `comment` - rdfs:comment (optional)
  - `subClassOf` - Parent class IRI (optional)
- `properties` (array) - RDF property definitions
  - `iri` - Property IRI
  - `label` - rdfs:label
  - `comment` - rdfs:comment (optional)
  - `domain` - Domain class IRI (optional)
  - `range` - Range class/datatype IRI (optional)
  - `subPropertyOf` - Parent property IRI (optional)

**Example:**

```javascript
const schema = await engine.render('schema.njk', {
  schemaIRI: 'http://example.org/schema/vocab',
  title: 'Document Vocabulary',
  classes: [
    {
      iri: 'http://example.org/schema/vocab#Document',
      label: 'Document',
      comment: 'A textual document'
    }
  ],
  properties: [
    {
      iri: 'http://example.org/schema/vocab#title',
      label: 'title',
      domain: 'http://example.org/schema/vocab#Document',
      range: 'http://www.w3.org/2001/XMLSchema#string'
    }
  ]
});
```

### 3. dataset.njk - DCAT Dataset

Generates DCAT dataset descriptions for data catalogs.

**Required Variables:**
- `datasetIRI` (string) - The IRI of the dataset
- `title` (string) - Dataset title
- `description` (string) - Dataset description

**Optional Variables:**
- `publisher` (string) - Publisher IRI
- `creator` (string) - Creator IRI
- `issued` (string) - Issue date (ISO format)
- `modified` (string) - Modification date (ISO format)
- `license` (string) - License IRI
- `keywords` (array of strings) - Keywords
- `theme` (array of strings) - Theme IRIs
- `spatial` (string) - Spatial coverage IRI
- `temporal` (string) - Temporal coverage description
- `distributions` (array) - Distribution objects
  - `iri` - Distribution IRI
  - `title` - Distribution title
  - `format` - Media type (e.g., "text/turtle")
  - `accessURL` - Access URL
  - `downloadURL` - Download URL (optional)

**Example:**

```javascript
const dataset = await engine.render('dataset.njk', {
  datasetIRI: 'http://example.org/datasets/census-2020',
  title: 'Census Data 2020',
  description: 'Population census data for 2020',
  license: 'http://creativecommons.org/licenses/by/4.0/',
  keywords: ['census', 'population', 'statistics'],
  distributions: [
    {
      iri: 'http://example.org/datasets/census-2020/csv',
      title: 'CSV Distribution',
      format: 'text/csv',
      accessURL: 'http://example.org/data/census-2020.csv'
    }
  ]
});
```

### 4. vocabulary.njk - SKOS Vocabulary

Generates SKOS concept schemes for taxonomies and controlled vocabularies.

**Required Variables:**
- `schemeIRI` (string) - The IRI of the concept scheme
- `title` (string) - Scheme title

**Optional Variables:**
- `description` (string) - Scheme description
- `creator` (string) - Creator IRI
- `created` (string) - Creation date (ISO format)
- `prefix` (string) - Short prefix
- `concepts` (array) - Concept definitions
  - `iri` - Concept IRI
  - `prefLabel` - Preferred label
  - `altLabel` (array of strings) - Alternative labels (optional)
  - `definition` - Definition text (optional)
  - `notation` - Notation string (optional)
  - `broader` - Broader concept IRI (optional)
  - `narrower` (array of strings) - Narrower concept IRIs (optional)
  - `related` (array of strings) - Related concept IRIs (optional)

**Example:**

```javascript
const vocab = await engine.render('vocabulary.njk', {
  schemeIRI: 'http://example.org/vocab/topics',
  title: 'Topic Vocabulary',
  concepts: [
    {
      iri: 'http://example.org/vocab/topics#Science',
      prefLabel: 'Science',
      definition: 'Natural and formal sciences',
      narrower: [
        'http://example.org/vocab/topics#Physics',
        'http://example.org/vocab/topics#Biology'
      ]
    }
  ]
});
```

### 5. shapes.njk - SHACL Shapes

Generates SHACL shapes for RDF data validation.

**Required Variables:**
- `shapesGraphIRI` (string) - The IRI of the shapes graph
- `shapes` (array) - Shape definitions

**Optional Variables:**
- `title` (string) - Shapes graph title
- `description` (string) - Description
- `prefix` (string) - Short prefix

**Shape Object:**
- `iri` - Shape IRI
- `targetClass` - Target class IRI (optional)
- `targetNode` - Target node IRI (optional)
- `label` - Shape label (optional)
- `description` - Shape description (optional)
- `closed` (boolean) - Whether shape is closed (optional)
- `properties` (array) - Property constraints
  - `path` - Property path IRI
  - `minCount` (number) - Minimum cardinality (optional)
  - `maxCount` (number) - Maximum cardinality (optional)
  - `datatype` - Datatype IRI (optional)
  - `class` - Class IRI (optional)
  - `pattern` - Regex pattern (optional)
  - `minLength` (number) - Min string length (optional)
  - `maxLength` (number) - Max string length (optional)
  - `minInclusive` (number) - Min numeric value (optional)
  - `maxInclusive` (number) - Max numeric value (optional)

**Example:**

```javascript
const shapes = await engine.render('shapes.njk', {
  shapesGraphIRI: 'http://example.org/shapes/person',
  title: 'Person Shapes',
  shapes: [
    {
      iri: 'http://example.org/shapes/person#PersonShape',
      targetClass: 'http://xmlns.com/foaf/0.1/Person',
      properties: [
        {
          path: 'http://xmlns.com/foaf/0.1/name',
          minCount: 1,
          maxCount: 1,
          datatype: 'http://www.w3.org/2001/XMLSchema#string'
        }
      ]
    }
  ]
});
```

### 6. sparql-queries.njk - SPARQL Queries

Generates collections of SPARQL queries with metadata and comments.

**Required Variables:**
- `title` (string) - Query collection title
- `queries` (array) - Query definitions

**Optional Variables:**
- `description` (string) - Collection description

**Query Object:**
- `name` - Query name/identifier
- `description` - Query description
- `type` - Query type: "SELECT", "CONSTRUCT", "ASK", "DESCRIBE"
- `prefixes` (object) - Prefix mappings (e.g., `{foaf: 'http://xmlns.com/foaf/0.1/'}`)
- `variables` (array of strings) - Variable names for SELECT (optional)
- `where` (array of strings) - WHERE clause patterns
- `construct` (array of strings) - CONSTRUCT patterns (for CONSTRUCT queries)
- `resources` (array of strings) - Resources for DESCRIBE (optional)
- `orderBy` (array of strings) - ORDER BY variables (optional)
- `limit` (number) - LIMIT value (optional)
- `offset` (number) - OFFSET value (optional)

**Example:**

```javascript
const queries = await engine.render('sparql-queries.njk', {
  title: 'Person Queries',
  queries: [
    {
      name: 'getAllPersons',
      description: 'Get all persons with names',
      type: 'SELECT',
      prefixes: {
        foaf: 'http://xmlns.com/foaf/0.1/'
      },
      variables: ['person', 'name'],
      where: [
        '?person a foaf:Person',
        '?person foaf:name ?name'
      ],
      orderBy: ['name'],
      limit: 100
    }
  ]
});
```

### 7. jsonld-context.njk - JSON-LD Context

Generates JSON-LD context files for mapping JSON to RDF.

**Required Variables:**
- `terms` (object) - Term mappings

**Optional Variables:**
- `contextIRI` (string) - Context IRI (for @id)
- `vocab` (string) - Default vocabulary IRI (for @vocab)
- `base` (string) - Base IRI (for @base)
- `prefixes` (object) - Prefix mappings

**Terms Object:**
- Simple mapping: `termName: "iri"`
- Complex mapping: `termName: { id: "iri", type: "@id|@vocab|xsd:type", container: "@list|@set", ... }`

**Example:**

```javascript
const context = await engine.render('jsonld-context.njk', {
  contextIRI: 'http://example.org/contexts/person',
  vocab: 'http://schema.org/',
  prefixes: {
    foaf: 'http://xmlns.com/foaf/0.1/',
    xsd: 'http://www.w3.org/2001/XMLSchema#'
  },
  terms: {
    Person: 'foaf:Person',
    name: 'foaf:name',
    email: {
      id: 'foaf:mbox',
      type: '@id'
    },
    age: {
      id: 'foaf:age',
      type: 'xsd:integer'
    }
  }
});
```

## Best Practices

### 1. Use Consistent IRI Patterns

```yaml
# Good - consistent namespace
ontologyIRI: http://example.org/ontology/library
classes:
  - iri: http://example.org/ontology/library#Book
  - iri: http://example.org/ontology/library#Author

# Avoid - mixed namespaces without justification
classes:
  - iri: http://example.org/Book
  - iri: http://another.org/Author
```

### 2. Provide Comprehensive Metadata

```yaml
# Good - complete metadata
ontologyIRI: http://example.org/onto
version: 1.0.0
title: Example Ontology
description: Comprehensive description of the ontology purpose
creator: http://example.org/people/john-doe
created: 2026-01-11
license: http://creativecommons.org/licenses/by/4.0/
```

### 3. Use Labels and Comments

```yaml
# Good - human-readable annotations
classes:
  - iri: http://example.org/onto#Person
    label: Person
    comment: Represents a human being with social and legal rights
```

### 4. Leverage Template Inheritance

For complex ontologies, compose from multiple templates:

```javascript
// Generate base classes
const baseClasses = await engine.render('ontology.njk', baseClassesData);

// Generate specialized properties
const properties = await engine.render('ontology.njk', propertiesData);

// Combine outputs
const combined = baseClasses + '\n\n' + properties;
```

### 5. Validate Generated RDF

```javascript
import { Parser } from 'n3';

const parser = new Parser({ format: 'text/turtle' });
const quads = parser.parse(generatedTurtle);

// Validates syntax and structure
if (quads.length > 0) {
  console.log('Valid RDF generated:', quads.length, 'triples');
}
```

### 6. Use Prefixes for Readability

```yaml
# Define reusable prefix
prefix: lib

# Results in cleaner output:
# @prefix lib: <http://example.org/ontology/library#> .
# lib:Book a owl:Class .
```

## Integration Examples

### With @unrdf/core

```javascript
import { createEngine } from '@unrdf/kgn';
import { createStore, loadTurtle } from '@unrdf/core';

const engine = createEngine({
  templatePaths: ['./node_modules/@unrdf/kgn/src/templates/rdf']
});

// Generate ontology
const turtle = await engine.render('ontology.njk', ontologyData);

// Load into RDF store
const store = createStore();
await loadTurtle(store, turtle);

// Query the generated ontology
const classes = store.getQuads(null, RDF.type, OWL.Class);
console.log('Generated classes:', classes.length);
```

### Batch Generation

```javascript
const templates = [
  { template: 'ontology.njk', data: ontologyData, output: 'onto.ttl' },
  { template: 'shapes.njk', data: shapesData, output: 'shapes.ttl' },
  { template: 'dataset.njk', data: datasetData, output: 'metadata.ttl' }
];

for (const { template, data, output } of templates) {
  const result = await engine.render(template, data);
  await fs.writeFile(output, result, 'utf-8');
}
```

### Dynamic Frontmatter

```javascript
import matter from 'gray-matter';

// Load template with frontmatter
const templateContent = await fs.readFile('my-ontology.njk', 'utf-8');
const { data, content } = matter(templateContent);

// Modify frontmatter programmatically
data.version = '2.0.0';
data.modified = new Date().toISOString().split('T')[0];

// Render with updated data
const result = await engine.render('ontology.njk', data);
```

## Troubleshooting

### Common Issues

**Empty output:**
- Check that all required variables are provided
- Verify frontmatter YAML is valid
- Ensure template path is correct

**Invalid RDF:**
- Validate IRIs are absolute (http://, https://)
- Check for proper string escaping in labels/comments
- Ensure array variables are actual arrays, not strings

**Missing prefixes:**
- Add prefixes to `prefixes` object or template header
- Verify namespace IRIs end with # or /

### Debugging

Enable template debugging:

```javascript
const engine = createEngine({
  templatePaths: ['./node_modules/@unrdf/kgn/src/templates/rdf'],
  debug: true
});
```

Validate template syntax:

```javascript
import { lintTemplate } from '@unrdf/kgn';

const issues = await lintTemplate('ontology.njk');
if (issues.length > 0) {
  console.error('Template issues:', issues);
}
```

## License

MIT License - see @unrdf/kgn package for details.
