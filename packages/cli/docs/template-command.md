# unrdf template

Ontology-driven code generation via RDF data and Nunjucks templates.

## Overview

The `template` command transforms RDF knowledge graphs into code artifacts using:

1. **RDF data source** (Turtle, N-Triples, JSON-LD, etc.) — Your knowledge graph
2. **SPARQL queries** — Extract specific data patterns
3. **Nunjucks templates** — Shape extracted data into code
4. **Type filters** — Automatic Zod/JSDoc conversion

Common use cases:

- Generate TypeScript interfaces from RDF class hierarchies
- Create Zod validation schemas from ontology definitions
- Produce JSDoc type stubs from RDFS/OWL descriptions
- Build test fixtures from RDF instances
- Generate GraphQL resolvers from entity relationships

## Quick Start

**1. Create RDF file (`ontology/schema.ttl`):**

```turtle
@prefix ex: <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:User a rdfs:Class ;
  rdfs:label "User" ;
  rdfs:comment "Application user" .

ex:User_id a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:string ;
  rdfs:label "id" .

ex:User_email a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:string ;
  rdfs:label "email" .
```

**2. Create template (`templates/schema.njk`):**

```njk
---
to: src/schemas/{{ output | kebabCase }}.mjs
sparql: |
  SELECT ?className ?propName ?propType
  WHERE {
    ?class a rdfs:Class ; rdfs:label ?className .
    ?prop rdfs:domain ?class ; rdfs:label ?propName ; rdfs:range ?propType .
  }
---
/**
 * @file Auto-generated Zod schemas
 */
import { z } from 'zod';

{% set classMap = sparql_results | groupBy("?className") %}
{% for className, props in classMap | items %}
export const {{ className | camelCase }}Schema = z.object({
{% for prop in props %}
  {{ prop["?propName"] | camelCase }}: {{ prop["?propType"] | zodType }},
{% endfor %}
});
{% endfor %}
```

**3. Run generation:**

```bash
unrdf template generate ontology/schema.ttl --template templates/schema.njk
```

Output: `src/schemas/user.mjs` with Zod schemas ready to use.

## Frontmatter Directives

Template files begin with YAML frontmatter (between `---` markers). All keys are optional unless marked required.

### Output Path

| Key | Type | Description |
|-----|------|-------------|
| `to` | string (required) | Output file path; supports Nunjucks `{{ }}` interpolation |

**Examples:**

```yaml
---
to: src/schemas/user.mjs
---

---
to: src/types/{{ entityName | kebabCase }}.ts
---

---
to: {{ output_dir }}/generated/{{ class_name | snakeCase }}.mjs
---
```

### SPARQL Query

| Key | Type | Description |
|-----|------|-------------|
| `sparql` | string | SPARQL SELECT query; returns results as `sparql_results` variable |

**Note:** CLI `--sparql` flag overrides frontmatter.

```yaml
---
sparql: |
  SELECT ?name ?type ?required
  WHERE {
    ?entity rdfs:label ?name ; rdfs:range ?type .
    OPTIONAL { ?entity rdfs:comment ?required }
  }
---
```

### RDF Data Source

| Key | Type | Description |
|-----|------|-------------|
| `rdf` | string | Path to RDF file (relative to template dir or absolute) |

```yaml
---
rdf: ../ontology/schema.ttl
---
```

If not specified in frontmatter, provide as positional argument to `unrdf template generate`.

### Write Mode

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `mode` | string | `overwrite` | File write behavior: `overwrite` (replace), `append` (add to file), `skip_existing` (don't overwrite) |

```yaml
---
mode: skip_existing
---
```

### Description & Metadata

| Key | Type | Description |
|-----|------|-------------|
| `description` | string | Human-readable rule description |
| `variables` | object | Custom variables passed to template context |

```yaml
---
description: Generate Zod validation schemas from RDF ontology
variables:
  generatorVersion: "1.0.0"
  skipUndocumented: true
---
```

### Advanced: Conditional & Line-Based Modification

The template system supports file modification directives for Hygen integration:

| Key | Type | Description |
|-----|------|-------------|
| `inject` | boolean | Enable line-based injection mode |
| `before` | string | Inject content before matching line (regex) |
| `after` | string | Inject content after matching line (regex) |
| `append` | string | Append content to file matching pattern |
| `prepend` | string | Prepend content to file matching pattern |
| `lineAt` | number | Insert at specific line number |
| `skipIf` | string | Skip if line matches pattern |

**Example: Inject into existing file:**

```yaml
---
to: src/types/index.ts
inject: true
after: "^// Generated types$"
---
```

## SPARQL Context & Type Coercion

### Query Execution

SPARQL queries execute against your RDF store. Results are available as `sparql_results` (array) and `results` (alias).

Each row is a JavaScript object mapping SPARQL variable names to values:

```javascript
{
  "?entityName": "User",
  "?propertyName": "email",
  "?propertyType": "http://www.w3.org/2001/XMLSchema#string",
  "?required": "true"
}
```

### Type Coercion Examples

RDF types are automatically converted using Zod/JSDoc filters.

**XSD Types → Zod:**

| RDF Type | Zod Output | Example |
|----------|-----------|---------|
| `xsd:string` | `z.string()` | Email, name, description |
| `xsd:integer` | `z.number().int()` | Count, age, ID |
| `xsd:decimal` | `z.number()` | Price, percentage |
| `xsd:boolean` | `z.boolean()` | Active, verified |
| `xsd:date` | `z.string().date()` | Birth date, created |
| `xsd:dateTime` | `z.string().datetime()` | Timestamp |
| `xsd:anyURI` | `z.string().url()` | Website, URI |

**Template usage:**

```njk
{% for row in sparql_results %}
{{ row["?propertyName"] | camelCase }}: {{ row["?propertyType"] | zodType }},
{% endfor %}
```

**XSD Types → JSDoc:**

```njk
{% for row in sparql_results %}
@param {{{ row["?propertyType"] | jsdocType }}} {{ row["?propertyName"] }}
{% endfor %}
```

Produces:

```javascript
/**
 * @param {string} email
 * @param {number} age
 * @param {boolean} verified
 */
```

## Nunjucks Filters Reference

All standard Nunjucks filters plus custom filters below.

### Case Conversion

| Filter | Input | Output | Use Case |
|--------|-------|--------|----------|
| `camelCase` | `"user-name"`, `"user_name"`, `"user name"` | `"userName"` | Property names, function args |
| `pascalCase` | `"user-name"` | `"UserName"` | Class names, component names |
| `snakeCase` | `"userName"` | `"user_name"` | Database columns, env vars |
| `kebabCase` | `"userName"` | `"user-name"` | File names, CSS classes |

**Example:**

```njk
export class {{ entityName | pascalCase }}Service {
  create{{ entityName | pascalCase }}({{ entityName | camelCase }}: unknown) { }
}
```

### RDF Utilities

| Filter | Input | Output | Use Case |
|--------|-------|--------|----------|
| `localName` | `"http://schema.org/Person"` | `"Person"` | Extract short name from URI |
| `namespace` | `"http://schema.org/Person"` | `"http://schema.org/"` | Extract base namespace |
| `expand` | `"schema:Person"` | `"http://schema.org/Person"` | Resolve prefixed names |

**Example:**

```njk
{% for row in sparql_results %}
// {{ row["?entityUri"] | localName }} ({{ row["?entityUri"] | namespace }})
{% endfor %}
```

### Type Conversion (Zod)

| Filter | Input | Output |
|--------|-------|--------|
| `zodType` | `"xsd:string"` | `z.string()` |
| `zodType` | `"xsd:integer"` | `z.number().int()` |
| `zodType` | `"xsd:boolean"` | `z.boolean()` |
| `zodType` | `"xsd:date"` | `z.string().date()` |

With `.optional()` for nullable fields:

```njk
{{ row["?type"] | zodType }}{% if row["?required"] != "true" %}.optional(){% endif %}
```

### Type Conversion (JSDoc)

| Filter | Input | Output |
|--------|-------|--------|
| `jsdocType` | `"xsd:string"` | `string` |
| `jsdocType` | `"xsd:integer"` | `number` |
| `jsdocType` | `"xsd:boolean"` | `boolean` |

### Data Manipulation

| Filter | Description | Example |
|--------|-------------|---------|
| `groupBy(key)` | Group array of objects by property | `sparql_results \| groupBy("?entityName")` |
| `distinctValues(key)` | Extract unique values | `sparql_results \| distinctValues("?type")` |
| `sortBy(key, dir)` | Sort array (asc/desc) | `sparql_results \| sortBy("?name", "asc")` |
| `keys` | Extract object keys | `classMap \| keys` |
| `values` | Extract object values | `classMap \| values` |
| `items` | Object to [key, value] pairs | `classMap \| items` |

### String Utilities

| Filter | Description | Example |
|--------|-------------|---------|
| `indent(n)` | Indent each line by n spaces | `code \| indent(4)` |
| `quote(char)` | Wrap string in quotes | `name \| quote` or `name \| quote("'")` |
| `date(format)` | Format date object | `now \| date("YYYY-MM-DD HH:mm:ss")` |

**Date format tokens:** `YYYY`, `MM`, `DD`, `HH`, `mm`, `ss`

## Batch Mode

Process multiple RDF instances with one template.

**When to use:** Generate individual files per entity (one schema per class, one resolver per endpoint, etc.)

**Example: One file per RDF class**

```bash
unrdf template generate ontology/schema.ttl \
  --template templates/schema.njk \
  --batch \
  --class-uri "http://example.org/Entity"
```

**How it works:**

1. `--class-uri` specifies RDF class to enumerate
2. Template SPARQL must contain `?subject` variable
3. For each instance, `?subject` is replaced with that instance URI
4. Output path uses current subject for uniqueness

**Template example:**

```njk
---
to: src/types/{{ "?subject" | localName | kebabCase }}.ts
sparql: |
  SELECT ?propertyName ?propertyType
  WHERE {
    ?subject rdfs:label ?className .
    ?subject ?property ?propertyType .
    ?property rdfs:label ?propertyName .
  }
---
export interface {{ "?subject" | localName }} {
{% for prop in sparql_results %}
  {{ prop["?propertyName"] | camelCase }}: {{ prop["?propertyType"] | jsdocType }};
{% endfor %}
}
```

Produces `src/types/user.ts`, `src/types/post.ts`, etc.

## Hygen Integration

The template system integrates with Hygen generators for scaffolding.

**Hygen support features:**

- Line-based modifications (`before:`, `after:`, `append:`, `prepend:`)
- Conditional skip (`skipIf:`)
- Anchor patterns for precise file insertion

**Example: Add imports to existing index.ts**

```yaml
---
to: src/index.ts
inject: true
after: "^// Auto-generated schemas$"
---
export { userSchema, postSchema } from './schemas/index.mjs';
```

**Example: Inject after class definition**

```yaml
---
to: src/services/user.ts
inject: true
before: "^}"
prepend: |

  async validate(user: unknown) {
    return UserSchema.parse(user);
  }
---
```

## Error Handling

### Common Errors

**1. SPARQL query returns no results**

```
Error: No results returned from SPARQL query
```

Check:
- Prefix declarations match your RDF file
- Property names exist in data
- SPARQL syntax is valid (use `--verbose`)

**Debug:**

```bash
unrdf template generate schema.ttl --template t.njk --verbose
```

**2. Template file not found**

```
Error: Template not found: /path/to/template.njk
```

Verify path exists and is readable.

**3. Invalid YAML frontmatter**

```
Error: YAML parsing failed in frontmatter
```

Check YAML syntax (colons, indentation, quotes).

**4. Nunjucks rendering failed**

```
Error: Template rendering failed: [variable] is undefined
  Line 42, Column 8

Possible fixes:
  Use default filter: {{ variable | default("") }}
  Check variable spelling
  Verify SPARQL returns expected columns
```

Fix:
- Use `| default("")` for optional values
- Verify SPARQL variable names (include `?` prefix in results)
- Check template syntax (balanced `{% %}` tags)

### Validation Mode

Preview output without writing files:

```bash
unrdf template generate schema.ttl --template t.njk --dry-run
```

Shows:
- Output path
- Rendered content (first 50 lines)
- File size
- No files written

## Examples

### Example 1: Generate Zod Schemas

**Ontology (`ontology/api.ttl`):**

```turtle
@prefix api: <http://example.org/api#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

api:User a rdfs:Class ; rdfs:label "User" .
api:user_email a rdf:Property ;
  rdfs:domain api:User ; rdfs:range xsd:string ; rdfs:label "email" .
api:user_age a rdf:Property ;
  rdfs:domain api:User ; rdfs:range xsd:integer ; rdfs:label "age" .
```

**Template (`templates/zod.njk`):**

```njk
---
to: lib/schemas.mjs
sparql: |
  SELECT ?className ?propName ?propType
  WHERE {
    ?class a rdfs:Class ; rdfs:label ?className .
    ?prop rdfs:domain ?class ; rdfs:label ?propName ; rdfs:range ?propType .
  }
  ORDER BY ?className ?propName
---
import { z } from 'zod';

{% set classes = sparql_results | groupBy("?className") %}
{% for className, props in classes | items %}
export const {{ className | camelCase }}Schema = z.object({
{% for prop in props %}
  {{ prop["?propName"] | camelCase }}: {{ prop["?propType"] | zodType }},
{% endfor %}
});
{% endfor %}
```

**Output (`lib/schemas.mjs`):**

```javascript
import { z } from 'zod';

export const userSchema = z.object({
  email: z.string(),
  age: z.number().int(),
});
```

### Example 2: Generate TypeScript Interfaces

**Template (`templates/types.njk`):**

```njk
---
to: lib/types.ts
sparql: |
  SELECT ?className ?propName ?propType
  WHERE { ... }
---
{% set classes = sparql_results | groupBy("?className") %}
{% for className, props in classes | items %}
export interface {{ className | pascalCase }} {
{% for prop in props %}
  {{ prop["?propName"] | camelCase }}: {{ prop["?propType"] | jsdocType }};
{% endfor %}
}
{% endfor %}
```

**Output (`lib/types.ts`):**

```typescript
export interface User {
  email: string;
  age: number;
}
```

### Example 3: Batch Generate Per-Class Files

```bash
unrdf template generate ontology/api.ttl \
  --template templates/class-schema.njk \
  --batch \
  --class-uri "http://example.org/api#Class"
```

**Template:**

```njk
---
to: lib/schemas/{{ "?subject" | localName | kebabCase }}.mjs
sparql: |
  SELECT ?propName ?propType
  WHERE {
    ?subject rdfs:label ?className ;
             ?prop ?propType .
    ?prop rdfs:label ?propName .
  }
---
import { z } from 'zod';

export const schema = z.object({
{% for prop in sparql_results %}
  {{ prop["?propName"] | camelCase }}: {{ prop["?propType"] | zodType }},
{% endfor %}
});
```

Generates: `lib/schemas/user.mjs`, `lib/schemas/post.mjs`, etc.

### Example 4: Inject Into Existing File

**Template (`templates/add-resolvers.njk`):**

```yaml
---
to: src/resolvers/index.ts
inject: true
after: "^// Auto-generated resolvers$"
---
export { userResolver, postResolver } from './user.mjs';
export { postResolver } from './post.mjs';
```

Inserts after matching line without replacing file.

### Example 5: Generate JSDoc Type Definitions

```njk
---
to: lib/index.d.mjs
sparql: |
  SELECT ?className ?propName ?propType
  WHERE { ... }
---
/**
 * @typedef {Object} User
{% set classes = sparql_results | groupBy("?className") %}
{% for className, props in classes | items %}
 * @typedef {Object} {{ className | pascalCase }}
{% for prop in props %}
 * @property {{{ prop["?propType"] | jsdocType }}} {{ prop["?propName"] }}
{% endfor %}
{% endfor %}
 */
export {};
```

## CLI Options Reference

```
Usage: unrdf template generate [file] [options]

Positional Arguments:
  file                  RDF file (Turtle, N-Triples, JSON-LD, etc.)
                        Optional if template sets `rdf:` in frontmatter

Options:
  -t, --template PATH   Path to Nunjucks template file (required)
  -o, --output-dir DIR  Output directory for generated files (default: "./generated")
  -s, --subject URI     Focus on single RDF subject (replaces ?subject)
  -q, --sparql QUERY    SPARQL SELECT query (overrides frontmatter)
  --batch               Batch mode: one output per instance of --class-uri
  --class-uri URI       RDF class IRI for batch mode
  --dry-run             Preview output without writing files
  -f, --force           Overwrite existing files without prompting
  -v, --verbose         Show detailed execution info
  -h, --help            Show help message
```

## Performance Notes

- **Large result sets:** Batch processing with `--batch` reduces memory usage
- **Complex SPARQL:** Use `--verbose` to see query execution time
- **File I/O:** Batch mode writes multiple smaller files instead of one large file

## See Also

- [Nunjucks Template Language](https://mozilla.github.io/nunjucks/)
- [SPARQL 1.1 Query Language](https://www.w3.org/TR/sparql11-query/)
- [Zod Documentation](https://zod.dev/)
- [unrdf sync command](./sync-command.md)
