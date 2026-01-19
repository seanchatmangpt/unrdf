# unrdf sync

RDF ontology to code generation command. Transforms RDF knowledge graphs into typed code artifacts using SPARQL queries and Nunjucks templates.

## Overview

The `sync` command reads an RDF ontology (Turtle, N-Triples, JSON-LD, etc.), executes SPARQL queries against it, and renders the results through templates to generate code. Common use cases:

- Generate Zod validation schemas from RDF class definitions
- Create OpenAPI specifications from API ontologies
- Produce JSDoc type definitions from RDFS/OWL
- Build GraphQL schemas from RDF vocabularies

## Quick Start

**1. Create configuration file `ggen.toml`:**

```toml
[project]
name = "my-api"
version = "1.0.0"

[ontology]
source = "ontology/schema.ttl"
format = "turtle"

[generation]
output_dir = "lib"

[[generation.rules]]
name = "zod-schemas"
template = "templates/entities.njk"
output_file = "schemas/entities.mjs"
query = """
SELECT ?entityName ?propertyName ?propertyType
WHERE {
  ?entity a rdfs:Class ; rdfs:label ?entityName .
  ?property rdfs:domain ?entity ; rdfs:range ?propertyType ; rdfs:label ?propertyName .
}
ORDER BY ?entityName ?propertyName
"""
```

**2. Create template `templates/entities.njk`:**

```njk
---
to: {{ output_dir }}/schemas/entities.mjs
---
import { z } from 'zod';

{% for entityName, props in sparql_results | groupBy("?entityName") %}
export const {{ entityName | camelCase }}Schema = z.object({
{% for row in props %}
  {{ row["?propertyName"] | camelCase }}: {{ row["?propertyType"] | zodType }},
{% endfor %}
});
{% endfor %}
```

**3. Run sync:**

```bash
unrdf sync
```

## Configuration (ggen.toml)

### [project] Section

Project metadata (optional).

| Key | Type | Description |
|-----|------|-------------|
| `name` | string | Project name |
| `version` | string | Project version |
| `description` | string | Project description |
| `author` | string | Project author |
| `license` | string | License identifier |

```toml
[project]
name = "blog-api"
version = "1.0.0"
description = "Blog API example"
```

### [ontology] Section

RDF source configuration (required).

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `source` | string | **required** | Path to RDF file |
| `format` | string | auto-detect | RDF format: `turtle`, `ntriples`, `nquads`, `jsonld`, `rdfxml`, `trig` |
| `base_iri` | string | - | Base IRI for relative URIs |
| `prefixes` | object | - | Custom prefix mappings |
| `follow_imports` | boolean | `false` | Follow `owl:imports` declarations |

```toml
[ontology]
source = "ontology/api.ttl"
format = "turtle"
base_iri = "http://example.org/api/"

[ontology.prefixes]
api = "http://example.org/api#"
schema = "http://schema.org/"
```

### [generation] Section

Output configuration.

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `output_dir` | string | `"lib"` | Base output directory |
| `require_audit_trail` | boolean | `false` | Require generation receipts |
| `parallel` | boolean | `false` | Run rules in parallel |

### [[generation.rules]] Section

Array of generation rules. Each rule defines a SPARQL query and template pair.

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `name` | string | **required** | Rule identifier |
| `description` | string | - | Rule description |
| `query` | string | **required** | SPARQL SELECT query |
| `template` | string | **required** | Path to Nunjucks template |
| `output_file` | string | **required** | Output file path (relative to `output_dir`) |
| `enabled` | boolean | `true` | Enable/disable rule |
| `mode` | string | `"overwrite"` | Write mode: `overwrite`, `append`, `skip_existing` |
| `depends_on` | array | - | Rule dependencies (names) |

```toml
[[generation.rules]]
name = "zod-entities"
description = "Generate Zod validation schemas"
enabled = true
mode = "overwrite"
template = "templates/zod/entities.njk"
output_file = "schemas/entities.mjs"
query = """
SELECT ?entityName ?propertyName ?propertyType ?required
WHERE {
  ?entity a api:Entity ; api:name ?entityName ; api:hasProperty ?property .
  ?property api:name ?propertyName ; api:type ?propertyType .
  OPTIONAL { ?property api:required ?required }
}
ORDER BY ?entityName ?propertyName
"""

[[generation.rules]]
name = "openapi-spec"
description = "Generate OpenAPI specification"
template = "templates/openapi/spec.njk"
output_file = "openapi/api.yaml"
query = """
SELECT ?path ?method ?operationId ?summary ?requestBody ?responseType
WHERE {
  ?operation a api:Operation ;
             api:path ?path ;
             api:method ?method ;
             api:operationId ?operationId .
  OPTIONAL { ?operation api:summary ?summary }
  OPTIONAL { ?operation api:requestBody ?requestBody }
  OPTIONAL { ?operation api:responseType ?responseType }
}
"""
```

### Environment Variables

Configuration values support environment variable substitution:

```toml
[ontology]
source = "${ONTOLOGY_PATH:-ontology/default.ttl}"

[generation]
output_dir = "$OUTPUT_DIR"
```

Supported patterns:
- `${VAR}` - substitute variable (empty if unset)
- `${VAR:-default}` - substitute with default value
- `$VAR` - simple substitution

## Template Syntax

Templates use [Nunjucks](https://mozilla.github.io/nunjucks/) syntax with frontmatter support.

### Frontmatter

YAML frontmatter specifies output path and metadata:

```njk
---
to: {{ output_dir }}/schemas/{{ entityName | kebabCase }}.mjs
description: Generated schema for {{ entityName }}
mode: overwrite
variables:
  generatorVersion: "1.0.0"
---
```

| Key | Description |
|-----|-------------|
| `to` | Output file path (supports template variables) |
| `description` | Rule description |
| `mode` | Write mode: `overwrite`, `append`, `skip_existing` |
| `variables` | Custom variables available in template |

### Available Context Variables

| Variable | Type | Description |
|----------|------|-------------|
| `sparql_results` | array | SPARQL query results |
| `results` | array | Alias for `sparql_results` |
| `prefixes` | object | RDF prefix mappings |
| `project` | object | Project config from `[project]` |
| `output_dir` | string | Output directory path |
| `now` | Date | Current timestamp |

### SPARQL Results Structure

Each result row is an object with SPARQL variable bindings:

```javascript
{
  "?entityName": "User",
  "?propertyName": "email",
  "?propertyType": "xsd:string",
  "?required": "true"
}
```

Access values using bracket notation: `row["?entityName"]` or without prefix after grouping.

## Available Filters

### Case Conversion

| Filter | Input | Output |
|--------|-------|--------|
| `camelCase` | `"user-name"` | `"userName"` |
| `pascalCase` | `"user-name"` | `"UserName"` |
| `snakeCase` | `"userName"` | `"user_name"` |
| `kebabCase` | `"userName"` | `"user-name"` |

### RDF Utilities

| Filter | Input | Output |
|--------|-------|--------|
| `localName` | `"http://schema.org/Person"` | `"Person"` |
| `namespace` | `"http://schema.org/Person"` | `"http://schema.org/"` |

### Type Conversion

| Filter | Input | Output |
|--------|-------|--------|
| `zodType` | `"xsd:string"` | `z.string()` |
| `zodType` | `"xsd:integer"` | `z.number().int()` |
| `zodType` | `"xsd:boolean"` | `z.boolean()` |
| `zodType` | `"xsd:date"` | `z.string().date()` |
| `zodType` | `"xsd:anyURI"` | `z.string().url()` |
| `jsdocType` | `"xsd:string"` | `string` |
| `jsdocType` | `"xsd:integer"` | `number` |
| `jsdocType` | `"xsd:boolean"` | `boolean` |

### Data Manipulation

| Filter | Description | Example |
|--------|-------------|---------|
| `groupBy(key)` | Group array by property | `results \| groupBy("?entityName")` |
| `distinctValues(key)` | Unique values for key | `results \| distinctValues("?type")` |
| `sortBy(key, dir)` | Sort by property | `results \| sortBy("?name", "asc")` |
| `keys` | Object keys | `obj \| keys` |
| `values` | Object values | `obj \| values` |
| `items` | Key-value pairs | `obj \| items` |

### String Utilities

| Filter | Description | Example |
|--------|-------------|---------|
| `indent(n)` | Indent lines by n spaces | `code \| indent(4)` |
| `quote(char)` | Quote string | `name \| quote` |
| `date(format)` | Format date | `now \| date("YYYY-MM-DD")` |

Date format tokens: `YYYY`, `MM`, `DD`, `HH`, `mm`, `ss`

## CLI Options

```
Usage: unrdf sync [options]

Options:
  --config <path>    Path to ggen.toml (default: "ggen.toml")
  --dry-run          Preview changes without writing files
  -v, --verbose      Enable verbose output
  -f, --force        Overwrite existing files without prompting
  --rule <name>      Run only the specified rule by name
  --output <format>  Output format: text or json (default: "text")
  -w, --watch        Watch ontology and templates for changes
```

### Examples

```bash
# Basic sync with default config
unrdf sync

# Use custom config file
unrdf sync --config ./config/api-gen.toml

# Preview without writing
unrdf sync --dry-run --verbose

# Run specific rule only
unrdf sync --rule zod-entities

# Watch mode for development
unrdf sync --watch --verbose

# JSON output for CI/scripting
unrdf sync --output json
```

## Full Example: Zod Schema Generation

### Ontology (`ontology/blog-api.ttl`)

```turtle
@prefix api: <http://example.org/api#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

api:User a api:Entity ;
    api:name "User" ;
    api:hasProperty api:User_id, api:User_email, api:User_name .

api:User_id api:name "id" ; api:type xsd:string ; api:required true .
api:User_email api:name "email" ; api:type xsd:string ; api:required true .
api:User_name api:name "name" ; api:type xsd:string .

api:Post a api:Entity ;
    api:name "Post" ;
    api:hasProperty api:Post_id, api:Post_title, api:Post_authorId .

api:Post_id api:name "id" ; api:type xsd:string ; api:required true .
api:Post_title api:name "title" ; api:type xsd:string ; api:required true .
api:Post_authorId api:name "authorId" ; api:type xsd:string ; api:required true .
```

### Configuration (`ggen.toml`)

```toml
[project]
name = "blog-api"
version = "1.0.0"

[ontology]
source = "ontology/blog-api.ttl"
format = "turtle"

[ontology.prefixes]
api = "http://example.org/api#"

[generation]
output_dir = "lib"

[[generation.rules]]
name = "zod-entities"
template = "templates/zod-entities.njk"
output_file = "schemas/entities.mjs"
query = """
SELECT ?entityName ?propertyName ?propertyType ?required
WHERE {
  ?entity a api:Entity ; api:name ?entityName ; api:hasProperty ?property .
  ?property api:name ?propertyName ; api:type ?propertyType .
  OPTIONAL { ?property api:required ?required }
}
ORDER BY ?entityName ?propertyName
"""
```

### Template (`templates/zod-entities.njk`)

```njk
---
to: {{ output_dir }}/schemas/entities.mjs
description: Zod validation schemas from RDF ontology
---
/**
 * @file Entity Validation Schemas
 * @generated {{ now | date("YYYY-MM-DD HH:mm:ss") }}
 */
import { z } from 'zod';

{% set entities = sparql_results | groupBy("?entityName") %}
{% for entityName, props in entities | items %}
export const {{ entityName | camelCase }}Schema = z.object({
{% for row in props %}
  {{ row["?propertyName"] | camelCase }}: {{ row["?propertyType"] | zodType }}{% if row["?required"] != "true" %}.optional(){% endif %},
{% endfor %}
});

{% endfor %}
export const schemas = {
{% for entityName in entities | keys %}
  {{ entityName | camelCase }}: {{ entityName | camelCase }}Schema,
{% endfor %}
};
```

### Generated Output (`lib/schemas/entities.mjs`)

```javascript
/**
 * @file Entity Validation Schemas
 * @generated 2024-01-15 10:30:00
 */
import { z } from 'zod';

export const userSchema = z.object({
  id: z.string(),
  email: z.string(),
  name: z.string().optional(),
});

export const postSchema = z.object({
  id: z.string(),
  title: z.string(),
  authorId: z.string(),
});

export const schemas = {
  user: userSchema,
  post: postSchema,
};
```

## Full Example: OpenAPI Specification

### Configuration

```toml
[[generation.rules]]
name = "openapi"
template = "templates/openapi-spec.njk"
output_file = "openapi/api.yaml"
query = """
SELECT ?path ?method ?operationId ?summary ?tag ?requestSchema ?responseSchema
WHERE {
  ?op a api:Operation ;
      api:path ?path ;
      api:method ?method ;
      api:operationId ?operationId .
  OPTIONAL { ?op api:summary ?summary }
  OPTIONAL { ?op api:tag ?tag }
  OPTIONAL { ?op api:requestSchema ?requestSchema }
  OPTIONAL { ?op api:responseSchema ?responseSchema }
}
ORDER BY ?path ?method
"""
```

### Template

```njk
---
to: {{ output_dir }}/openapi/api.yaml
---
openapi: "3.0.3"
info:
  title: {{ project.name }}
  version: {{ project.version }}

paths:
{% set operations = sparql_results | groupBy("?path") %}
{% for path, ops in operations | items %}
  {{ path }}:
{% for op in ops %}
    {{ op["?method"] | lower }}:
      operationId: {{ op["?operationId"] }}
{% if op["?summary"] %}
      summary: {{ op["?summary"] }}
{% endif %}
{% if op["?tag"] %}
      tags:
        - {{ op["?tag"] }}
{% endif %}
      responses:
        "200":
          description: Success
{% if op["?responseSchema"] %}
          content:
            application/json:
              schema:
                $ref: "#/components/schemas/{{ op["?responseSchema"] | localName }}"
{% endif %}
{% endfor %}
{% endfor %}
```

## Troubleshooting

### Config file not found

```
Error: Configuration file not found: ggen.toml
```

Create `ggen.toml` in your project root or specify path with `--config`.

### Ontology file not found

```
Error: Ontology file not found: /path/to/schema.ttl
```

Check `[ontology].source` path is correct relative to config file location.

### SPARQL query errors

Use `--verbose` to see detailed query execution info. Verify:
- Prefix declarations match your ontology
- Property names exist in RDF data
- SPARQL syntax is valid

### Template rendering errors

```
Error: Template rendering failed: ...
```

Check template syntax. Common issues:
- Missing `{% endif %}` or `{% endfor %}`
- Invalid filter usage
- Undefined variables (use `| default("")` for optional values)

## See Also

- [Nunjucks Documentation](https://mozilla.github.io/nunjucks/)
- [SPARQL 1.1 Query Language](https://www.w3.org/TR/sparql11-query/)
- [@unrdf/core RDF Operations](/home/user/unrdf/packages/core/README.md)
