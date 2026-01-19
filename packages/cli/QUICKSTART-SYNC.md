# @unrdf/cli sync - Quick Start Guide

**Get from RDF ontology to production code in 5 minutes.**

## What is the Sync Command?

The `sync` command transforms RDF ontologies into typed code artifacts using SPARQL queries and templates. Define your data model once in RDF (Turtle, JSON-LD, etc.), then automatically generate Zod schemas, TypeScript types, OpenAPI specs, and more - all from a single source of truth.

## Installation

```bash
pnpm add -D @unrdf/cli
```

Verify installation:
```bash
npx unrdf --version
```

## 5-Minute Tutorial

Follow these steps to generate your first code from an RDF ontology.

### Step 1: Create `ggen.toml` (30 seconds)

Create a configuration file in your project root:

```toml
[project]
name = "blog-api"
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
SELECT ?className ?propertyName ?propertyType
WHERE {
  ?class a <http://www.w3.org/2002/07/owl#Class> ;
         <http://www.w3.org/2000/01/rdf-schema#label> ?className .
  ?property <http://www.w3.org/2000/01/rdf-schema#domain> ?class ;
            <http://www.w3.org/2000/01/rdf-schema#label> ?propertyName ;
            <http://www.w3.org/2000/01/rdf-schema#range> ?propertyType .
}
ORDER BY ?className ?propertyName
"""
```

### Step 2: Create Ontology File (1 minute)

Create `ontology/schema.ttl`:

```turtle
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# User entity
<#User> a owl:Class ;
    rdfs:label "User" ;
    rdfs:comment "A user in the system" .

<#username> a owl:DatatypeProperty ;
    rdfs:domain <#User> ;
    rdfs:range xsd:string ;
    rdfs:label "username" .

<#email> a owl:DatatypeProperty ;
    rdfs:domain <#User> ;
    rdfs:range xsd:string ;
    rdfs:label "email" .

# Post entity
<#Post> a owl:Class ;
    rdfs:label "Post" ;
    rdfs:comment "A blog post" .

<#title> a owl:DatatypeProperty ;
    rdfs:domain <#Post> ;
    rdfs:range xsd:string ;
    rdfs:label "title" .

<#content> a owl:DatatypeProperty ;
    rdfs:domain <#Post> ;
    rdfs:range xsd:string ;
    rdfs:label "content" .
```

### Step 3: Create Template (1 minute)

Create `templates/entities.njk`:

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

{% set entities = sparql_results | groupBy("?className") %}
{% for className, props in entities | items %}
export const {{ className | camelCase }}Schema = z.object({
{% for row in props %}
  {{ row["?propertyName"] | camelCase }}: {{ row["?propertyType"] | zodType }},
{% endfor %}
});

{% endfor %}
export const schemas = {
{% for className in entities | keys %}
  {{ className | camelCase }}: {{ className | camelCase }}Schema,
{% endfor %}
};
```

### Step 4: Run Sync (10 seconds)

Generate the code:

```bash
npx unrdf sync
```

**Expected output:**
```
[unrdf sync] Loading configuration from ggen.toml
[unrdf sync] Parsing ontology: ontology/schema.ttl (turtle)
[unrdf sync] Found 1 generation rules

[unrdf sync] Executing rule: zod-schemas
  Query returned 5 results
  Generated: lib/schemas/entities.mjs (24 lines)

[unrdf sync] Generation complete!
  Rules executed: 1
  Files generated: 1
  Total lines: 24
```

### Step 5: Check Generated Output (30 seconds)

View the generated file:

```bash
cat lib/schemas/entities.mjs
```

**Expected output:**
```javascript
/**
 * @file Entity Validation Schemas
 * @generated 2026-01-19 12:00:00
 */
import { z } from 'zod';

export const userSchema = z.object({
  username: z.string(),
  email: z.string(),
});

export const postSchema = z.object({
  title: z.string(),
  content: z.string(),
});

export const schemas = {
  user: userSchema,
  post: postSchema,
};
```

**Test it works:**

```bash
cat > test.mjs << 'EOF'
import { userSchema } from './lib/schemas/entities.mjs';

const valid = { username: 'alice', email: 'alice@example.com' };
const invalid = { username: 'bob' }; // missing email

console.log('Valid:', userSchema.safeParse(valid).success);
console.log('Invalid:', userSchema.safeParse(invalid).success);
EOF

node test.mjs
```

**Output:**
```
Valid: true
Invalid: false
```

**Congratulations!** You just generated production-ready validation schemas from an RDF ontology.

## Next Steps

### Full Documentation

- **[Complete Sync Command Guide](./docs/sync-command.md)** - All features and options
- **[Tutorial: Your First API from RDF](../../docs/diataxis/tutorials/sync-first-api.md)** - 20-minute comprehensive tutorial
- **[Reference: ggen.toml Configuration](../../docs/diataxis/reference/sync-config.md)** - All configuration options
- **[Reference: Template Filters](../../docs/diataxis/reference/sync-filters.md)** - Available Nunjucks filters

### How-To Guides

- **[Add Custom Entity Types](../../docs/diataxis/how-to/sync-add-entity.md)** - Extend your ontology
- **[Watch Mode for Development](../../docs/diataxis/how-to/sync-watch-mode.md)** - Auto-regenerate on changes
- **[Custom Template Filters](../../docs/diataxis/how-to/sync-custom-filters.md)** - Extend Nunjucks

### Explanation

- **[Sync Architecture](../../docs/diataxis/explanation/sync-architecture.md)** - How it works
- **[Template Patterns](../../docs/diataxis/explanation/sync-template-patterns.md)** - Best practices

## Common Patterns

### Pattern 1: Generate Multiple Artifacts

Generate Zod schemas, JSDoc types, and OpenAPI specs from one ontology:

```toml
# ggen.toml
[project]
name = "my-api"

[ontology]
source = "ontology/api.ttl"

[generation]
output_dir = "lib"

# Rule 1: Zod schemas
[[generation.rules]]
name = "zod-schemas"
template = "templates/zod-entities.njk"
output_file = "schemas/entities.mjs"
query = """
SELECT ?className ?propertyName ?propertyType
WHERE {
  ?class a owl:Class ; rdfs:label ?className .
  ?property rdfs:domain ?class ; rdfs:label ?propertyName ; rdfs:range ?propertyType .
}
"""

# Rule 2: JSDoc types
[[generation.rules]]
name = "jsdoc-types"
template = "templates/jsdoc-types.njk"
output_file = "types/entities.mjs"
query = """
SELECT ?className ?propertyName ?propertyType
WHERE {
  ?class a owl:Class ; rdfs:label ?className .
  ?property rdfs:domain ?class ; rdfs:label ?propertyName ; rdfs:range ?propertyType .
}
"""

# Rule 3: OpenAPI spec
[[generation.rules]]
name = "openapi"
template = "templates/openapi-spec.njk"
output_file = "openapi/api.yaml"
query = """
SELECT ?path ?method ?operationId ?summary
WHERE {
  ?op a <#Operation> ;
      <#path> ?path ;
      <#method> ?method ;
      <#operationId> ?operationId .
  OPTIONAL { ?op <#summary> ?summary }
}
"""
```

Run all rules:
```bash
npx unrdf sync
```

### Pattern 2: Watch Mode for Development

Auto-regenerate when ontology or templates change:

```bash
npx unrdf sync --watch --verbose
```

Edit `ontology/schema.ttl` and save - code regenerates automatically.

**Press Ctrl+C to stop.**

### Pattern 3: Preview Before Writing

Check what would be generated without writing files:

```bash
npx unrdf sync --dry-run --verbose
```

**Output:**
```
[DRY RUN] Would execute rule: zod-schemas
  Query returned 5 results
  Would write: lib/schemas/entities.mjs (24 lines)

[unrdf sync] Dry run complete. No files written.
```

### Pattern 4: Run Specific Rule Only

Generate only one output when you have multiple rules:

```bash
npx unrdf sync --rule zod-schemas
```

Useful when debugging a single template.

## Available Filters

Use these in your templates to transform data:

### Case Conversion
- `camelCase` - `"user-name"` → `"userName"`
- `pascalCase` - `"user-name"` → `"UserName"`
- `snakeCase` - `"userName"` → `"user_name"`
- `kebabCase` - `"userName"` → `"user-name"`

### RDF Utilities
- `localName` - `"http://schema.org/Person"` → `"Person"`
- `namespace` - `"http://schema.org/Person"` → `"http://schema.org/"`

### Type Conversion
- `zodType` - `"xsd:string"` → `z.string()`
- `jsdocType` - `"xsd:string"` → `string`

### Data Manipulation
- `groupBy("?key")` - Group results by SPARQL variable
- `distinctValues("?key")` - Get unique values
- `sortBy("?key", "asc")` - Sort results
- `keys` - Object keys as array
- `values` - Object values as array
- `items` - Object as key-value pairs

### Formatting
- `indent(4)` - Indent lines by 4 spaces
- `quote` - Wrap in quotes
- `date("YYYY-MM-DD")` - Format timestamp

**Example:**
```njk
{% for className, props in sparql_results | groupBy("?className") | items %}
export const {{ className | pascalCase }}Schema = z.object({
{% for row in props %}
  {{ row["?propertyName"] | camelCase }}: {{ row["?propertyType"] | zodType }},
{% endfor %}
});
{% endfor %}
```

## CLI Options Reference

```bash
unrdf sync [options]
```

| Option | Alias | Default | Description |
|--------|-------|---------|-------------|
| `--config <path>` | - | `ggen.toml` | Path to configuration file |
| `--dry-run` | - | `false` | Preview without writing files |
| `--verbose` | `-v` | `false` | Enable detailed output |
| `--force` | `-f` | `false` | Overwrite without prompting |
| `--watch` | `-w` | `false` | Watch for file changes |
| `--rule <name>` | - | - | Run only specified rule |
| `--output <format>` | - | `text` | Output format: `text` or `json` |

## Troubleshooting

### Config file not found
```
Error: Configuration file not found: ggen.toml
```
**Solution:** Create `ggen.toml` in your project root or use `--config` to specify path.

### Ontology file not found
```
Error: Ontology file not found: ontology/schema.ttl
```
**Solution:** Check that `ontology.source` path in `ggen.toml` is correct.

### SPARQL query returns no results
```
[unrdf sync] Query returned 0 results
```
**Solution:** Use `--verbose` to see query details. Check that:
- Property URIs match your ontology
- PREFIX declarations are correct
- Filter conditions aren't too restrictive

### Template rendering error
```
Error: Template rendering failed: undefined is not iterable
```
**Solution:**
- Ensure you handle empty results with `| default("")`
- Check that `groupBy` receives correct variable name (with `?` prefix)
- Verify all variables in template exist in SPARQL results

### No files written
```
[unrdf sync] Skipping: lib/schemas/entities.mjs (already exists)
```
**Solution:** Use `--force` to overwrite existing files.

## Examples

More complete examples:

```bash
# Basic sync
npx unrdf sync

# Custom config location
npx unrdf sync --config ./config/api-gen.toml

# Dry run with details
npx unrdf sync --dry-run --verbose

# Watch mode for development
npx unrdf sync --watch

# Run specific rule only
npx unrdf sync --rule zod-schemas

# Force overwrite
npx unrdf sync --force

# JSON output for CI/CD
npx unrdf sync --output json > sync-report.json
```

## Support

- **Issues:** https://github.com/seanchatmangpt/unrdf/issues
- **Documentation:** See [docs/](../../docs/diataxis/)
- **Examples:** See [examples/](./examples/)
- **Full CLI Guide:** See [QUICKSTART-CLI.md](./QUICKSTART-CLI.md)

---

**Ready to build more?** Check out the [complete tutorial](../../docs/diataxis/tutorials/sync-first-api.md) for advanced patterns including OpenAPI generation, custom filters, and multi-format output.
