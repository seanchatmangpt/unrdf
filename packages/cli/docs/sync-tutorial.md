# Sync Command Tutorial

A step-by-step guide to generating code from RDF ontologies using the `unrdf sync` command.

## What You'll Learn

- How to create an RDF ontology
- How to configure the sync command
- How to write templates with Nunjucks
- How to execute SPARQL queries
- How to generate production-ready code

## Prerequisites

- Node.js 18+ installed
- Basic understanding of RDF/Turtle syntax
- Familiarity with JavaScript/TypeScript

## Tutorial Overview

This tutorial builds a blog API code generator from scratch. By the end, you'll have:

- ✅ RDF ontology defining blog entities (User, Post, Comment)
- ✅ SPARQL queries extracting entity metadata
- ✅ Templates generating Zod schemas, TypeScript types, and JSDoc
- ✅ Automated code generation workflow

**Time to complete**: 20-30 minutes

---

## Step 1: Create Project Structure

Create a new project directory and initialize it:

```bash
mkdir blog-codegen
cd blog-codegen
npm init -y
npm install @unrdf/cli zod
```

Create the directory structure:

```bash
mkdir -p ontology templates/zod templates/types lib/schemas lib/types
```

Your project should look like:

```
blog-codegen/
├── package.json
├── ggen.toml          # Configuration (we'll create this)
├── ontology/
│   └── blog.ttl       # RDF ontology (we'll create this)
├── templates/
│   ├── zod/           # Zod schema templates
│   └── types/         # TypeScript type templates
└── lib/               # Generated code output
    ├── schemas/
    └── types/
```

---

## Step 2: Define Your RDF Ontology

Create `ontology/blog.ttl` with your domain model:

```turtle
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix blog: <http://example.org/blog#> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

# ============================================================================
# Entity Definitions
# ============================================================================

blog:User a owl:Class ;
    rdfs:label "User" ;
    rdfs:comment "A registered user of the blog platform" .

blog:Post a owl:Class ;
    rdfs:label "Post" ;
    rdfs:comment "A blog post written by a user" .

blog:Comment a owl:Class ;
    rdfs:label "Comment" ;
    rdfs:comment "A comment on a blog post" .

# ============================================================================
# User Properties
# ============================================================================

blog:userId a owl:DatatypeProperty ;
    rdfs:domain blog:User ;
    rdfs:range xsd:string ;
    rdfs:label "id" ;
    rdfs:comment "Unique identifier for the user" ;
    sh:minCount 1 .  # Required field

blog:username a owl:DatatypeProperty ;
    rdfs:domain blog:User ;
    rdfs:range xsd:string ;
    rdfs:label "username" ;
    rdfs:comment "User's display name" ;
    sh:minCount 1 ;
    sh:pattern "^[a-zA-Z0-9_-]{3,20}$" .

blog:email a owl:DatatypeProperty ;
    rdfs:domain blog:User ;
    rdfs:range xsd:string ;
    rdfs:label "email" ;
    rdfs:comment "User's email address" ;
    sh:minCount 1 .

blog:bio a owl:DatatypeProperty ;
    rdfs:domain blog:User ;
    rdfs:range xsd:string ;
    rdfs:label "bio" ;
    rdfs:comment "User biography (optional)" .

blog:createdAt a owl:DatatypeProperty ;
    rdfs:domain blog:User ;
    rdfs:range xsd:dateTime ;
    rdfs:label "createdAt" ;
    rdfs:comment "Account creation timestamp" ;
    sh:minCount 1 .

# ============================================================================
# Post Properties
# ============================================================================

blog:postId a owl:DatatypeProperty ;
    rdfs:domain blog:Post ;
    rdfs:range xsd:string ;
    rdfs:label "id" ;
    rdfs:comment "Unique identifier for the post" ;
    sh:minCount 1 .

blog:title a owl:DatatypeProperty ;
    rdfs:domain blog:Post ;
    rdfs:range xsd:string ;
    rdfs:label "title" ;
    rdfs:comment "Post title" ;
    sh:minCount 1 ;
    sh:minLength 1 ;
    sh:maxLength 200 .

blog:content a owl:DatatypeProperty ;
    rdfs:domain blog:Post ;
    rdfs:range xsd:string ;
    rdfs:label "content" ;
    rdfs:comment "Post content in Markdown format" ;
    sh:minCount 1 .

blog:excerpt a owl:DatatypeProperty ;
    rdfs:domain blog:Post ;
    rdfs:range xsd:string ;
    rdfs:label "excerpt" ;
    rdfs:comment "Short excerpt or summary (optional)" .

blog:published a owl:DatatypeProperty ;
    rdfs:domain blog:Post ;
    rdfs:range xsd:boolean ;
    rdfs:label "published" ;
    rdfs:comment "Publication status" ;
    sh:minCount 1 .

blog:publishedAt a owl:DatatypeProperty ;
    rdfs:domain blog:Post ;
    rdfs:range xsd:dateTime ;
    rdfs:label "publishedAt" ;
    rdfs:comment "Publication timestamp (optional)" .

blog:author a owl:ObjectProperty ;
    rdfs:domain blog:Post ;
    rdfs:range blog:User ;
    rdfs:label "authorId" ;
    rdfs:comment "ID of the post author" ;
    sh:minCount 1 .

# ============================================================================
# Comment Properties
# ============================================================================

blog:commentId a owl:DatatypeProperty ;
    rdfs:domain blog:Comment ;
    rdfs:range xsd:string ;
    rdfs:label "id" ;
    rdfs:comment "Unique identifier for the comment" ;
    sh:minCount 1 .

blog:commentContent a owl:DatatypeProperty ;
    rdfs:domain blog:Comment ;
    rdfs:range xsd:string ;
    rdfs:label "content" ;
    rdfs:comment "Comment text content" ;
    sh:minCount 1 ;
    sh:minLength 1 ;
    sh:maxLength 1000 .

blog:commentAuthor a owl:ObjectProperty ;
    rdfs:domain blog:Comment ;
    rdfs:range blog:User ;
    rdfs:label "authorId" ;
    rdfs:comment "ID of the comment author" ;
    sh:minCount 1 .

blog:post a owl:ObjectProperty ;
    rdfs:domain blog:Comment ;
    rdfs:range blog:Post ;
    rdfs:label "postId" ;
    rdfs:comment "ID of the post being commented on" ;
    sh:minCount 1 .

blog:commentCreatedAt a owl:DatatypeProperty ;
    rdfs:domain blog:Comment ;
    rdfs:range xsd:dateTime ;
    rdfs:label "createdAt" ;
    rdfs:comment "Comment creation timestamp" ;
    sh:minCount 1 .
```

**Key concepts**:
- **Classes** (`owl:Class`): Entity types (User, Post, Comment)
- **Properties** (`owl:DatatypeProperty`, `owl:ObjectProperty`): Fields on entities
- **Constraints** (`sh:minCount`, `sh:pattern`): Validation rules
- **Metadata** (`rdfs:label`, `rdfs:comment`): Human-readable descriptions

---

## Step 3: Create Configuration File

Create `ggen.toml` at the project root:

```toml
[project]
name = "blog-api"
version = "1.0.0"
description = "Blog API code generation from RDF ontology"
author = "Your Name"

[ontology]
source = "ontology/blog.ttl"
format = "turtle"
base_iri = "http://example.org/blog#"

[ontology.prefixes]
blog = "http://example.org/blog#"
sh = "http://www.w3.org/ns/shacl#"

[generation]
output_dir = "lib"
require_audit_trail = false
parallel = false

# Rule 1: Generate Zod validation schemas
[[generation.rules]]
name = "zod-schemas"
description = "Generate Zod validation schemas for entities"
enabled = true
template = "templates/zod/entities.njk"
output_file = "schemas/entities.mjs"
query = """
SELECT ?entityName ?propertyName ?propertyType ?required ?description ?pattern ?minLength ?maxLength
WHERE {
  ?entity a owl:Class ;
          rdfs:label ?entityName .

  ?property rdfs:domain ?entity ;
            rdfs:range ?propertyType ;
            rdfs:label ?propertyName .

  OPTIONAL { ?property rdfs:comment ?description }
  OPTIONAL { ?property sh:minCount ?minCount }
  OPTIONAL { ?property sh:pattern ?pattern }
  OPTIONAL { ?property sh:minLength ?minLength }
  OPTIONAL { ?property sh:maxLength ?maxLength }

  BIND(IF(BOUND(?minCount) && ?minCount > 0, "true", "false") AS ?required)
}
ORDER BY ?entityName ?propertyName
"""

# Rule 2: Generate TypeScript/JSDoc type definitions
[[generation.rules]]
name = "jsdoc-types"
description = "Generate JSDoc type definitions for entities"
enabled = true
template = "templates/types/entities.njk"
output_file = "types/entities.mjs"
query = """
SELECT ?entityName ?entityDescription ?propertyName ?propertyType ?required ?propertyDescription
WHERE {
  ?entity a owl:Class ;
          rdfs:label ?entityName .

  OPTIONAL { ?entity rdfs:comment ?entityDescription }

  ?property rdfs:domain ?entity ;
            rdfs:range ?propertyType ;
            rdfs:label ?propertyName .

  OPTIONAL { ?property rdfs:comment ?propertyDescription }
  OPTIONAL { ?property sh:minCount ?minCount }

  BIND(IF(BOUND(?minCount) && ?minCount > 0, "true", "false") AS ?required)
}
ORDER BY ?entityName ?propertyName
"""
```

**Configuration sections**:
- `[project]`: Project metadata
- `[ontology]`: Where to find RDF data
- `[generation]`: Output settings and rules
- `[[generation.rules]]`: Individual generation rules (SPARQL + template pairs)

---

## Step 4: Create Zod Schema Template

Create `templates/zod/entities.njk`:

```nunjucks
---
to: {{ output_dir }}/schemas/entities.mjs
description: Zod validation schemas from RDF ontology
---
/**
 * @file Entity Validation Schemas
 * @module schemas/entities
 * @description Auto-generated Zod schemas from RDF ontology
 * @generated {{ now | date("YYYY-MM-DD HH:mm:ss") }}
 */
import { z } from 'zod';

{# Group results by entity name #}
{% set entities = sparql_results | groupBy("?entityName") %}

{% for entityName, properties in entities | items %}
/**
 * {{ entityName }} validation schema
 */
export const {{ entityName | camelCase }}Schema = z.object({
{% for prop in properties %}
  {% set propName = prop["?propertyName"] | camelCase %}
  {% set propType = prop["?propertyType"] | zodType %}
  {% set isRequired = prop["?required"] == "true" %}
  {% set description = prop["?description"] %}
  {% set pattern = prop["?pattern"] %}
  {% set minLength = prop["?minLength"] %}
  {% set maxLength = prop["?maxLength"] %}

  /** {{ description | default(propName) }} */
  {{ propName }}: {{ propType }}
  {%- if pattern and propType == "z.string()" %}.regex(/{{ pattern }}/){% endif %}
  {%- if minLength and propType == "z.string()" %}.min({{ minLength }}){% endif %}
  {%- if maxLength and propType == "z.string()" %}.max({{ maxLength }}){% endif %}
  {%- if not isRequired %}.optional(){% endif %}
  {%- if description %}.describe('{{ description | replace("'", "\\'") }}'){% endif %},
{% endfor %}
});

/**
 * Validate {{ entityName }} object
 * @param {unknown} data - Data to validate
 * @returns {{ "{" }}success: boolean, data?: any, error?: z.ZodError{{ "}" }}
 */
export function validate{{ entityName }}(data) {
  return {{ entityName | camelCase }}Schema.safeParse(data);
}

{% endfor %}

/**
 * All entity schemas
 */
export const schemas = {
{% for entityName in entities | keys %}
  {{ entityName | camelCase }}: {{ entityName | camelCase }}Schema,
{% endfor %}
};
```

**Template features**:
- **Frontmatter**: Defines output path and metadata
- **Filters**: `camelCase`, `zodType`, `groupBy` for data transformation
- **Conditionals**: `{% if %}` for optional logic
- **Loops**: `{% for %}` to iterate over SPARQL results

---

## Step 5: Create JSDoc Type Template

Create `templates/types/entities.njk`:

```nunjucks
---
to: {{ output_dir }}/types/entities.mjs
description: JSDoc type definitions from RDF ontology
---
/**
 * @file Entity Type Definitions
 * @module types/entities
 * @description Auto-generated JSDoc types from RDF ontology
 * @generated {{ now | date("YYYY-MM-DD HH:mm:ss") }}
 */

{% set entities = sparql_results | groupBy("?entityName") %}

{% for entityName, properties in entities | items %}
/**
 * {{ entities[entityName][0]["?entityDescription"] | default(entityName + " entity") }}
 * @typedef {Object} {{ entityName }}
{% for prop in properties %}
{% set propName = prop["?propertyName"] | camelCase %}
{% set propType = prop["?propertyType"] | jsdocType %}
{% set isRequired = prop["?required"] == "true" %}
{% set description = prop["?propertyDescription"] | default(propName) %}
{% if isRequired %}
 * @property {{ "{" }}{{ propType }}{{ "}" }} {{ propName }} - {{ description }}
{% else %}
 * @property {{ "{" }}{{ propType }}{{ "}" }} [{{ propName }}] - {{ description }}
{% endif %}
{% endfor %}
 */

{% endfor %}

/**
 * Entity name constants
 */
export const EntityNames = Object.freeze({
{% for entityName in entities | keys %}
  {{ entityName | upper }}: '{{ entityName }}',
{% endfor %}
});
```

---

## Step 6: Run Sync Command

Generate code from your ontology:

```bash
# Basic sync (single run)
npx unrdf sync

# Verbose output to see what's happening
npx unrdf sync --verbose

# Dry run to preview without writing files
npx unrdf sync --dry-run --verbose

# Watch mode for development (auto-regenerate on changes)
npx unrdf sync --watch --verbose
```

**Expected output**:

```
UNRDF Sync

Phase 1: Loading configuration...
   Config: ggen.toml
   Project: blog-api

Phase 2: Loading ontology...
   Loaded: 45 triples

Phase 3: Processing rules...
   Rule: zod-schemas
   Query returned 18 results
   OK lib/schemas/entities.mjs (2847 bytes)

   Rule: jsdoc-types
   Query returned 18 results
   OK lib/types/entities.mjs (1923 bytes)

Sync complete!
   Rules processed: 2
   Files generated: 2
   Duration: 127.4ms
```

---

## Step 7: Verify Generated Code

Check the generated Zod schemas in `lib/schemas/entities.mjs`:

```javascript
/**
 * @file Entity Validation Schemas
 * @module schemas/entities
 * @description Auto-generated Zod schemas from RDF ontology
 * @generated 2024-01-19 10:30:00
 */
import { z } from 'zod';

/**
 * User validation schema
 */
export const userSchema = z.object({
  /** Unique identifier for the user */
  id: z.string().describe('Unique identifier for the user'),

  /** User's display name */
  username: z.string().regex(/^[a-zA-Z0-9_-]{3,20}$/).describe("User's display name"),

  /** User's email address */
  email: z.string().describe("User's email address"),

  /** User biography (optional) */
  bio: z.string().optional().describe('User biography (optional)'),

  /** Account creation timestamp */
  createdAt: z.string().describe('Account creation timestamp'),
});

/**
 * Validate User object
 * @param {unknown} data - Data to validate
 * @returns {success: boolean, data?: any, error?: z.ZodError}
 */
export function validateUser(data) {
  return userSchema.safeParse(data);
}

// ... Post and Comment schemas follow same pattern ...
```

---

## Step 8: Use Generated Code

Create `example-usage.mjs` to test the generated schemas:

```javascript
import { userSchema, postSchema, validateUser } from './lib/schemas/entities.mjs';

// Valid user
const validUser = {
  id: 'user-123',
  username: 'alice_dev',
  email: 'alice@example.com',
  bio: 'Software engineer and blogger',
  createdAt: '2024-01-19T10:00:00Z',
};

const result = validateUser(validUser);
console.log('Valid user:', result.success); // true

// Invalid user (username too short)
const invalidUser = {
  id: 'user-456',
  username: 'ab',  // Too short! Pattern requires 3-20 chars
  email: 'bob@example.com',
  createdAt: '2024-01-19T11:00:00Z',
};

const result2 = validateUser(invalidUser);
console.log('Invalid user:', result2.success); // false
console.log('Errors:', result2.error?.errors);
```

Run it:

```bash
node example-usage.mjs
```

---

## Advanced Usage

### Watch Mode for Development

Start watch mode to auto-regenerate when ontology or templates change:

```bash
npx unrdf sync --watch --verbose
```

Now edit `ontology/blog.ttl` and save - the code will regenerate automatically!

### Run Specific Rules

Generate only Zod schemas:

```bash
npx unrdf sync --rule zod-schemas
```

### Custom Output Directory

Override the output directory:

```bash
npx unrdf sync --output-dir ./generated
```

### JSON Output for CI/CD

Get machine-readable output:

```bash
npx unrdf sync --output json > sync-result.json
```

---

## Common Patterns

### Pattern 1: Optional Fields

In your ontology, omit `sh:minCount` to make fields optional:

```turtle
blog:bio a owl:DatatypeProperty ;
    rdfs:domain blog:User ;
    rdfs:range xsd:string ;
    rdfs:label "bio" .
    # No sh:minCount = optional
```

Template handles this automatically:

```nunjucks
{% if prop["?required"] == "true" %}
  {{ propName }}: {{ propType }},
{% else %}
  {{ propName }}: {{ propType }}.optional(),
{% endif %}
```

### Pattern 2: Validation Constraints

Add SHACL constraints for validation:

```turtle
blog:username a owl:DatatypeProperty ;
    sh:minLength 3 ;
    sh:maxLength 20 ;
    sh:pattern "^[a-zA-Z0-9_-]+$" .
```

Template applies them:

```nunjucks
{{ propType }}
{%- if minLength %}.min({{ minLength }}){% endif %}
{%- if maxLength %}.max({{ maxLength }}){% endif %}
{%- if pattern %}.regex(/{{ pattern }}/){% endif %}
```

### Pattern 3: Relationships

Define relationships between entities:

```turtle
blog:author a owl:ObjectProperty ;
    rdfs:domain blog:Post ;
    rdfs:range blog:User ;
    rdfs:label "authorId" .
```

In template, treat as string ID:

```nunjucks
{% if propType == "blog:User" %}
  authorId: z.string(),
{% endif %}
```

---

## Troubleshooting

### Issue: "Configuration file not found"

```
Error: Configuration file not found: ggen.toml
```

**Solution**: Create `ggen.toml` in your project root, or specify path:

```bash
npx unrdf sync --config path/to/config.toml
```

### Issue: "Ontology file not found"

```
Error: Ontology file not found: /path/to/blog.ttl
```

**Solution**: Check `[ontology].source` path is correct relative to config file:

```toml
[ontology]
source = "ontology/blog.ttl"  # Relative to ggen.toml location
```

### Issue: "Template rendering failed"

```
Error: Template rendering failed: expected variable end
```

**Solution**: Check Nunjucks syntax. Common issues:
- Missing `{% endif %}` or `{% endfor %}`
- Unmatched `{{ }}` or `{% %}`
- Using `|` filter on undefined variables

**Fix**: Add `| default("")` for optional values:

```nunjucks
{{ prop["?description"] | default("No description") }}
```

### Issue: SPARQL Query Returns No Results

**Debug steps**:

1. Run with `--verbose` to see query output:
```bash
npx unrdf sync --verbose
```

2. Check your SPARQL query matches ontology structure:
```sparql
# Wrong - property doesn't exist
?entity blog:missingProperty ?value

# Right - use what's in your ontology
?entity rdfs:label ?label
```

3. Verify prefix declarations:
```toml
[ontology.prefixes]
blog = "http://example.org/blog#"  # Must match ontology
```

### Issue: Generated Code Has Syntax Errors

**Cause**: Template generated invalid JavaScript.

**Debug**:

1. Run dry-run to see output without writing:
```bash
npx unrdf sync --dry-run --verbose > output.txt
```

2. Check template escaping:
```nunjucks
{# Wrong - can break on quotes #}
.describe('{{ description }}')

{# Right - escape quotes #}
.describe('{{ description | replace("'", "\\'") }}')
```

---

## Next Steps

Now that you've completed the tutorial, you can:

1. **Extend your ontology**: Add more entities and properties
2. **Create custom templates**: Generate OpenAPI specs, GraphQL schemas, database migrations
3. **Add validation rules**: Use SHACL for advanced constraints
4. **Automate in CI/CD**: Run sync in GitHub Actions or other CI systems
5. **Explore advanced filters**: See [Sync Command Reference](./sync-command.md) for all available filters

## Additional Resources

- [Sync Command Reference](./sync-command.md) - Complete CLI reference
- [Examples Collection](../examples/sync/README.md) - More real-world examples
- [Nunjucks Documentation](https://mozilla.github.io/nunjucks/) - Template syntax
- [SPARQL 1.1 Query Language](https://www.w3.org/TR/sparql11-query/) - Query reference
- [SHACL Specification](https://www.w3.org/TR/shacl/) - Constraint language

## Summary

You've learned how to:

- ✅ Define RDF ontologies with classes and properties
- ✅ Configure `unrdf sync` with TOML
- ✅ Write SPARQL queries to extract metadata
- ✅ Create Nunjucks templates with filters and conditionals
- ✅ Generate production-ready code from ontologies
- ✅ Use watch mode for iterative development

The sync command transforms your ontology into a **single source of truth** for your codebase. Change the ontology, run sync, and your schemas, types, and documentation update automatically.

Happy code generating! 🚀
