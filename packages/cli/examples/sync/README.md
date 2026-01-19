# Sync Command Examples

Real-world examples and patterns for the `unrdf sync` command. Each example is complete and ready to run.

## Table of Contents

1. [Basic Examples](#basic-examples)
2. [Zod Schema Generation](#zod-schema-generation)
3. [TypeScript Types](#typescript-types)
4. [OpenAPI Specification](#openapi-specification)
5. [GraphQL Schema](#graphql-schema)
6. [Database Migrations](#database-migrations)
7. [API Documentation](#api-documentation)
8. [Common Patterns](#common-patterns)

---

## Basic Examples

### Minimal Example

Generate a simple constant from an RDF class.

**Files needed:**
- `ontology/hello.ttl`
- `ggen.toml`
- `templates/hello.njk`

**ontology/hello.ttl**:
```turtle
@prefix ex: <http://example.org/#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Greeting a rdfs:Class ;
    rdfs:label "Hello" ;
    rdfs:comment "A friendly greeting" .
```

**ggen.toml**:
```toml
[project]
name = "hello-example"

[ontology]
source = "ontology/hello.ttl"

[generation]
output_dir = "lib"

[[generation.rules]]
name = "greeting"
template = "templates/hello.njk"
output_file = "greeting.mjs"
query = "SELECT ?label ?comment WHERE { ?class rdfs:label ?label ; rdfs:comment ?comment }"
```

**templates/hello.njk**:
```nunjucks
---
to: {{ output_dir }}/greeting.mjs
---
/**
 * Generated greeting
 */
{% for row in sparql_results %}
export const GREETING = "{{ row['?label'] }}";
export const MESSAGE = "{{ row['?comment'] }}";
{% endfor %}
```

**Run**: `npx unrdf sync`

**Output** (`lib/greeting.mjs`):
```javascript
/**
 * Generated greeting
 */
export const GREETING = "Hello";
export const MESSAGE = "A friendly greeting";
```

---

## Zod Schema Generation

Generate Zod validation schemas with full constraint support.

**ontology/product.ttl**:
```turtle
@prefix api: <http://example.org/api#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

api:Product a owl:Class ;
    rdfs:label "Product" ;
    rdfs:comment "A product in the catalog" .

api:sku a owl:DatatypeProperty ;
    rdfs:domain api:Product ;
    rdfs:range xsd:string ;
    rdfs:label "sku" ;
    rdfs:comment "Stock keeping unit" ;
    sh:minCount 1 ;
    sh:pattern "^[A-Z0-9-]{8,}$" .

api:name a owl:DatatypeProperty ;
    rdfs:domain api:Product ;
    rdfs:range xsd:string ;
    rdfs:label "name" ;
    sh:minCount 1 ;
    sh:minLength 3 ;
    sh:maxLength 100 .

api:price a owl:DatatypeProperty ;
    rdfs:domain api:Product ;
    rdfs:range xsd:decimal ;
    rdfs:label "price" ;
    sh:minCount 1 .

api:description a owl:DatatypeProperty ;
    rdfs:domain api:Product ;
    rdfs:range xsd:string ;
    rdfs:label "description" .
```

**ggen.toml**:
```toml
[project]
name = "product-api"

[ontology]
source = "ontology/product.ttl"

[generation]
output_dir = "lib"

[[generation.rules]]
name = "zod-schemas"
template = "templates/zod-schema.njk"
output_file = "schemas/product.mjs"
query = """
SELECT ?entityName ?propertyName ?propertyType ?required ?pattern ?minLength ?maxLength ?comment
WHERE {
  ?entity a owl:Class ; rdfs:label ?entityName .
  ?property rdfs:domain ?entity ;
            rdfs:range ?propertyType ;
            rdfs:label ?propertyName .
  OPTIONAL { ?property rdfs:comment ?comment }
  OPTIONAL { ?property sh:minCount ?minCount }
  OPTIONAL { ?property sh:pattern ?pattern }
  OPTIONAL { ?property sh:minLength ?minLength }
  OPTIONAL { ?property sh:maxLength ?maxLength }
  BIND(IF(BOUND(?minCount) && ?minCount > 0, "true", "false") AS ?required)
}
ORDER BY ?entityName ?propertyName
"""
```

**templates/zod-schema.njk**:
```nunjucks
---
to: {{ output_dir }}/schemas/product.mjs
---
import { z } from 'zod';

{% set entities = sparql_results | groupBy("?entityName") %}
{% for entityName, props in entities | items %}
/**
 * {{ entityName }} validation schema
 */
export const {{ entityName | camelCase }}Schema = z.object({
{% for prop in props %}
  {% set propName = prop["?propertyName"] | camelCase %}
  {% set baseType = prop["?propertyType"] | zodType %}
  {% set isRequired = prop["?required"] == "true" %}
  /** {{ prop["?comment"] | default(propName) }} */
  {{ propName }}: {{ baseType }}
  {%- if prop["?pattern"] and baseType == "z.string()" %}.regex(/{{ prop["?pattern"] }}/){% endif %}
  {%- if prop["?minLength"] and baseType == "z.string()" %}.min({{ prop["?minLength"] }}){% endif %}
  {%- if prop["?maxLength"] and baseType == "z.string()" %}.max({{ prop["?maxLength"] }}){% endif %}
  {%- if not isRequired %}.optional(){% endif %},
{% endfor %}
});

export function validate{{ entityName }}(data) {
  return {{ entityName | camelCase }}Schema.safeParse(data);
}
{% endfor %}
```

**Output**:
```javascript
import { z } from 'zod';

export const productSchema = z.object({
  /** Stock keeping unit */
  sku: z.string().regex(/^[A-Z0-9-]{8,}$/),
  /** name */
  name: z.string().min(3).max(100),
  /** price */
  price: z.number(),
  /** description */
  description: z.string().optional(),
});

export function validateProduct(data) {
  return productSchema.safeParse(data);
}
```

---

## TypeScript Types

Generate JSDoc type definitions for IDE autocomplete.

**templates/types.njk**:
```nunjucks
---
to: {{ output_dir }}/types/entities.d.mjs
---
/**
 * @file Entity Type Definitions
 * @generated {{ now | date("YYYY-MM-DD HH:mm:ss") }}
 */

{% set entities = sparql_results | groupBy("?entityName") %}
{% for entityName, props in entities | items %}
/**
 * {{ props[0]["?comment"] | default(entityName + " entity") }}
 * @typedef {Object} {{ entityName }}
{% for prop in props %}
{% set propName = prop["?propertyName"] | camelCase %}
{% set propType = prop["?propertyType"] | jsdocType %}
{% set isRequired = prop["?required"] == "true" %}
{% set comment = prop["?comment"] | default(propName) %}
{% if isRequired %}
 * @property {{ "{" }}{{ propType }}{{ "}" }} {{ propName }} - {{ comment }}
{% else %}
 * @property {{ "{" }}{{ propType }}{{ "}" }} [{{ propName }}] - {{ comment }}
{% endif %}
{% endfor %}
 */

{% endfor %}
```

**Usage**:
```javascript
/**
 * @param {import('./types/entities.d.mjs').Product} product
 */
function displayProduct(product) {
  console.log(product.sku); // IDE knows this exists
}
```

---

## OpenAPI Specification

Generate OpenAPI 3.0 specs from RDF API definitions.

**ontology/api-operations.ttl**:
```turtle
@prefix api: <http://example.org/api#> .

api:ListProducts a api:Operation ;
    api:path "/products" ;
    api:method "GET" ;
    api:operationId "listProducts" ;
    api:summary "List all products" ;
    api:tag "Products" .

api:GetProduct a api:Operation ;
    api:path "/products/{id}" ;
    api:method "GET" ;
    api:operationId "getProduct" ;
    api:summary "Get product by ID" ;
    api:tag "Products" .

api:CreateProduct a api:Operation ;
    api:path "/products" ;
    api:method "POST" ;
    api:operationId "createProduct" ;
    api:summary "Create new product" ;
    api:requestSchema "Product" ;
    api:responseSchema "Product" ;
    api:tag "Products" .
```

**SPARQL Query**:
```sparql
SELECT ?path ?method ?operationId ?summary ?tag
WHERE {
  ?op a api:Operation ;
      api:path ?path ;
      api:method ?method ;
      api:operationId ?operationId .
  OPTIONAL { ?op api:summary ?summary }
  OPTIONAL { ?op api:tag ?tag }
}
ORDER BY ?path ?method
```

**templates/openapi.njk**:
```nunjucks
---
to: {{ output_dir }}/openapi.yaml
---
openapi: "3.0.3"
info:
  title: {{ project.name }}
  version: {{ project.version }}

paths:
{% set paths = sparql_results | groupBy("?path") %}
{% for path, ops in paths | items %}
  {{ path }}:
{% for op in ops %}
    {{ op["?method"] | lower }}:
      operationId: {{ op["?operationId"] }}
      summary: {{ op["?summary"] }}
      tags:
        - {{ op["?tag"] }}
      responses:
        '200':
          description: Success
{% endfor %}
{% endfor %}
```

---

## GraphQL Schema

Generate GraphQL type definitions.

**templates/graphql.njk**:
```nunjucks
---
to: {{ output_dir }}/schema.graphql
---
# Generated GraphQL Schema
# {{ now | date("YYYY-MM-DD HH:mm:ss") }}

{% set entities = sparql_results | groupBy("?entityName") %}
{% for entityName, props in entities | items %}
type {{ entityName }} {
{% for prop in props %}
  {% set propName = prop["?propertyName"] | camelCase %}
  {% set propType = prop["?propertyType"] | localName %}
  {% set isRequired = prop["?required"] == "true" %}
  {{ propName }}: {% if propType == "string" %}String{% elif propType == "integer" %}Int{% elif propType == "boolean" %}Boolean{% elif propType == "decimal" %}Float{% else %}String{% endif %}{% if isRequired %}!{% endif %}
{% endfor %}
}

{% endfor %}
```

**Output**:
```graphql
type Product {
  sku: String!
  name: String!
  price: Float!
  description: String
}
```

---

## Database Migrations

Generate SQL CREATE TABLE statements.

**templates/migration.njk**:
```nunjucks
---
to: {{ output_dir }}/migrations/{{ now | date("YYYYMMDD") }}_create_tables.sql
---
-- Generated Migration
-- {{ now | date("YYYY-MM-DD HH:mm:ss") }}

{% set entities = sparql_results | groupBy("?entityName") %}
{% for entityName, props in entities | items %}
CREATE TABLE {{ entityName | snakeCase }}s (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
{% for prop in props %}
{% set propName = prop["?propertyName"] | snakeCase %}
{% set propType = prop["?propertyType"] | localName %}
{% set sqlType = "TEXT" %}
{% if propType == "string" %}{% set sqlType = "VARCHAR(255)" %}
{% elif propType == "integer" %}{% set sqlType = "INTEGER" %}
{% elif propType == "boolean" %}{% set sqlType = "BOOLEAN" %}
{% elif propType == "decimal" %}{% set sqlType = "DECIMAL(10,2)" %}
{% elif propType == "dateTime" %}{% set sqlType = "TIMESTAMP" %}
{% endif %}
  {{ propName }} {{ sqlType }}{% if prop["?required"] == "true" %} NOT NULL{% endif %},
{% endfor %}
  created_at TIMESTAMP NOT NULL DEFAULT NOW()
);

{% endfor %}
```

**Output**:
```sql
CREATE TABLE products (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  sku VARCHAR(255) NOT NULL,
  name VARCHAR(255) NOT NULL,
  price DECIMAL(10,2) NOT NULL,
  description TEXT,
  created_at TIMESTAMP NOT NULL DEFAULT NOW()
);
```

---

## API Documentation

Generate Markdown docs.

**templates/api-docs.njk**:
```nunjucks
---
to: {{ output_dir }}/API.md
---
# {{ project.name }} API Reference

> Generated: {{ now | date("YYYY-MM-DD HH:mm:ss") }}

## Entities

{% set entities = sparql_results | groupBy("?entityName") %}
{% for entityName, props in entities | items %}
### {{ entityName }}

**Properties:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
{% for prop in props %}
| `{{ prop["?propertyName"] }}` | `{{ prop["?propertyType"] | localName }}` | {{ "Yes" if prop["?required"] == "true" else "No" }} | {{ prop["?comment"] | default("-") }} |
{% endfor %}

**Example:**

\`\`\`json
{
{% for prop in props %}
{% set propName = prop["?propertyName"] %}
{% set propType = prop["?propertyType"] | localName %}
  "{{ propName }}": {% if propType == "string" %}"example"{% elif propType == "integer" %}123{% elif propType == "boolean" %}true{% elif propType == "decimal" %}99.99{% else %}"value"{% endif %}{% if not loop.last %},{% endif %}
{% endfor %}
}
\`\`\`

---
{% endfor %}
```

---

## Common Patterns

### Pattern: Optional Fields

Use SHACL `sh:minCount` to determine required vs optional:

```turtle
# Required
api:email a owl:DatatypeProperty ;
    sh:minCount 1 .

# Optional (no minCount)
api:bio a owl:DatatypeProperty .
```

Template:
```nunjucks
{% if prop["?required"] == "true" %}
  {{ propName }}: {{ propType }},
{% else %}
  {{ propName }}: {{ propType }}.optional(),
{% endif %}
```

### Pattern: Default Values

Store defaults in RDF:

```turtle
api:status a owl:DatatypeProperty ;
    api:defaultValue "draft" .
```

SPARQL:
```sparql
OPTIONAL { ?property api:defaultValue ?default }
```

Template:
```nunjucks
{% if prop["?default"] %}
  {{ propName }}: {{ propType }}.default("{{ prop["?default"] }}"),
{% endif %}
```

### Pattern: Enumerations

Define enum values:

```turtle
api:Status a owl:Class ;
    owl:oneOf (api:Draft api:Published api:Archived) .

api:Draft rdfs:label "draft" .
api:Published rdfs:label "published" .
api:Archived rdfs:label "archived" .
```

SPARQL:
```sparql
SELECT ?enumName ?value
WHERE {
  ?enum owl:oneOf ?list .
  ?list rdf:rest*/rdf:first ?item .
  ?item rdfs:label ?value .
}
```

Template:
```nunjucks
export const StatusEnum = z.enum([
{% for row in sparql_results %}
  '{{ row["?value"] }}',
{% endfor %}
]);
```

### Pattern: Relationships

For foreign keys:

```turtle
api:author a owl:ObjectProperty ;
    rdfs:domain api:Post ;
    rdfs:range api:User ;
    rdfs:label "authorId" .
```

Template handles as string ID:
```nunjucks
{% if prop["?propertyType"] | localName == "User" %}
  authorId: z.string(),  // Reference to User
{% endif %}
```

---

## Tips & Best Practices

### 1. Use Watch Mode During Development
```bash
npx unrdf sync --watch --verbose
```

### 2. Test SPARQL Queries First
Use [YASGUI](https://yasgui.triply.cc/) to test queries before adding to config.

### 3. Start Simple
Begin with minimal templates, then add complexity incrementally.

### 4. Handle Optional Values
Always provide defaults:
```nunjucks
{{ prop["?description"] | default("No description") }}
```

### 5. Escape User Content
```nunjucks
{{ description | replace("'", "\\'") }}
```

### 6. Use Frontmatter Variables
```nunjucks
---
to: {{ output_dir }}/{{ entityName }}.mjs
variables:
  includeHelpers: true
---
{% if includeHelpers %}
  // Helper functions
{% endif %}
```

---

## Filter Reference Quick Guide

### Case Conversion
- `camelCase`: `user-name` → `userName`
- `pascalCase`: `user-name` → `UserName`
- `snakeCase`: `userName` → `user_name`
- `kebabCase`: `userName` → `user-name`

### RDF Utilities
- `localName`: `http://ex.org/api#Product` → `Product`
- `namespace`: `http://ex.org/api#Product` → `http://ex.org/api#`

### Type Conversion
- `zodType`: `xsd:string` → `z.string()`
- `jsdocType`: `xsd:integer` → `number`

### Data Manipulation
- `groupBy("key")`: Group results by property
- `distinctValues("key")`: Get unique values
- `sortBy("key", "asc")`: Sort results
- `keys`: Get object keys
- `values`: Get object values
- `items`: Get key-value pairs

### String Utilities
- `indent(n)`: Indent lines by n spaces
- `quote`: Quote string
- `date("YYYY-MM-DD")`: Format date

---

## Troubleshooting

### Empty Output
```bash
npx unrdf sync --verbose
```
Check: `Query returned 0 results` - SPARQL query doesn't match ontology.

### Template Error
```
Error: expected variable end
```
Check for unmatched `{{ }}`, `{% %}`, missing `{% endif %}` or `{% endfor %}`.

### Wrong Output Path
Use `{{ output_dir }}` variable in frontmatter `to:` field.

---

## Additional Resources

- [Sync Command Reference](../../docs/sync-command.md)
- [Sync Tutorial](../../docs/sync-tutorial.md)
- [Nunjucks Docs](https://mozilla.github.io/nunjucks/)
- [SPARQL 1.1](https://www.w3.org/TR/sparql11-query/)
- [SHACL](https://www.w3.org/TR/shacl/)
