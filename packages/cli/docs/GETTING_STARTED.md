# Getting Started with @unrdf/cli

Start generating code from RDF ontologies in 5 minutes.

## Installation

### Option 1: Add to project

```bash
pnpm add -D @unrdf/cli
```

Then use via `pnpm`:

```bash
pnpm unrdf sync
pnpm unrdf template generate
```

### Option 2: Global installation

```bash
pnpm add -g @unrdf/cli
```

Then use directly:

```bash
unrdf sync
unrdf template generate
```

### Option 3: Run directly with npx

```bash
npx @unrdf/cli sync
npx @unrdf/cli template generate schema.ttl --template template.njk
```

## What is RDF? (Brief Overview)

RDF (Resource Description Framework) is a standard for describing data as **triples**:

```
Subject (entity) → Predicate (property) → Object (value)

User "john@example.com" has email "john@example.com"
User "john@example.com" has name "John Doe"
```

**Common RDF formats:**

- **Turtle** (`.ttl`) — Easy to read, most common
- **N-Triples** (`.nt`) — One triple per line
- **JSON-LD** (`.jsonld`) — JSON format
- **RDF/XML** (`.rdf`) — XML format

For this guide, we'll use **Turtle**.

## What is Nunjucks? (Brief Overview)

**Nunjucks** is a template language like Handlebars or Jinja2. It lets you:

- Loop over data: `{% for item in items %}`
- Use variables: `{{ item.name }}`
- Apply filters: `{{ name | camelCase }}`
- Use conditionals: `{% if x %}...{% endif %}`

This guide uses Nunjucks templates to shape RDF data into code.

## 5-Minute Quickstart

### Step 1: Create an RDF ontology

Create `ontology/blog.ttl`:

```turtle
@prefix blog: <http://example.org/blog#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

# Define the User class
blog:User a rdfs:Class ;
  rdfs:label "User" ;
  rdfs:comment "A user of the blog platform" .

# Define properties of User
blog:user_id a rdf:Property ;
  rdfs:domain blog:User ;
  rdfs:range xsd:string ;
  rdfs:label "id" .

blog:user_email a rdf:Property ;
  rdfs:domain blog:User ;
  rdfs:range xsd:string ;
  rdfs:label "email" .

blog:user_name a rdf:Property ;
  rdfs:domain blog:User ;
  rdfs:range xsd:string ;
  rdfs:label "name" ;
  rdfs:comment "Full name" .

# Define the Post class
blog:Post a rdfs:Class ;
  rdfs:label "Post" ;
  rdfs:comment "A blog post" .

blog:post_id a rdf:Property ;
  rdfs:domain blog:Post ;
  rdfs:range xsd:string ;
  rdfs:label "id" .

blog:post_title a rdf:Property ;
  rdfs:domain blog:Post ;
  rdfs:range xsd:string ;
  rdfs:label "title" .

blog:post_content a rdf:Property ;
  rdfs:domain blog:Post ;
  rdfs:range xsd:string ;
  rdfs:label "content" .

blog:post_authorId a rdf:Property ;
  rdfs:domain blog:Post ;
  rdfs:range xsd:string ;
  rdfs:label "authorId" .
```

### Step 2: Create a Nunjucks template

Create `templates/zod-schemas.njk`:

```njk
---
to: src/schemas.mjs
sparql: |
  SELECT ?className ?propertyName ?propertyType
  WHERE {
    ?class a rdfs:Class ; rdfs:label ?className .
    ?property rdfs:domain ?class ;
              rdfs:label ?propertyName ;
              rdfs:range ?propertyType .
  }
  ORDER BY ?className ?propertyName
---
/**
 * @file Auto-generated Zod validation schemas
 * @generated {{ now | date("YYYY-MM-DD HH:mm:ss") }}
 */
import { z } from 'zod';

{% set classes = sparql_results | groupBy("?className") %}
{% for className, props in classes | items %}
export const {{ className | camelCase }}Schema = z.object({
{% for prop in props %}
  {{ prop["?propertyName"] | camelCase }}: {{ prop["?propertyType"] | zodType }},
{% endfor %}
});

{% endfor %}
// Export all schemas as a namespace
export const schemas = {
{% for className in classes | keys %}
  {{ className | camelCase }}: {{ className | camelCase }}Schema,
{% endfor %}
};
```

**What this template does:**

1. Declares output path: `to: src/schemas.mjs`
2. Defines SPARQL query to extract classes and properties
3. Groups results by class name
4. For each class, generates a Zod schema with typed fields
5. Applies filters: `camelCase` (for names), `zodType` (for types)

### Step 3: Generate code

```bash
pnpm unrdf template generate ontology/blog.ttl --template templates/zod-schemas.njk
```

### Step 4: View output

Check `src/schemas.mjs`:

```javascript
/**
 * @file Auto-generated Zod validation schemas
 * @generated 2026-04-03 14:22:15
 */
import { z } from 'zod';

export const postSchema = z.object({
  id: z.string(),
  title: z.string(),
  content: z.string(),
  authorId: z.string(),
});

export const userSchema = z.object({
  id: z.string(),
  email: z.string(),
  name: z.string(),
});

// Export all schemas as a namespace
export const schemas = {
  post: postSchema,
  user: userSchema,
};
```

**Done!** You now have validated schemas from RDF data.

## Template Anatomy (Annotated)

Understanding template structure helps when creating your own.

```njk
---
frontmatter (YAML between --- markers)
---
template (Nunjucks content)
```

### Frontmatter Section

```yaml
---
to: src/schemas.mjs # Output file path (can use {{ }} vars)
sparql: | # SPARQL query (multiline)
  SELECT ?className ?propertyName
  WHERE { ... }
---
```

### Template Section

```njk
{% set classes = sparql_results | groupBy("?className") %}
                 ^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^^^^^^^^
                 SPARQL results   Nunjucks filter (group by class)

{% for className, props in classes | items %}
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Loop over grouped results (className, props)

{{ className | camelCase }}
   ^^^^^^^^^   ^^^^^^^^^^
   Variable   Filter (convert case)

{{ prop["?propertyName"] | camelCase }}: {{ prop["?propertyType"] | zodType }},
   ^^^^^^^^^^^^^^^^^^^^^^^^              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
   Access SPARQL result column           Convert XSD type to Zod syntax
```

## Common Patterns

### Pattern 1: Single-File Generation

Generate one file with all schemas/types.

**Template:**

```njk
---
to: lib/types.mjs
sparql: |
  SELECT ?className
  WHERE { ?class a rdfs:Class ; rdfs:label ?className }
---
{% for row in sparql_results %}
export interface {{ row["?className"] }} { }
{% endfor %}
```

**Run:**

```bash
pnpm unrdf template generate ontology.ttl --template templates/types.njk
```

**Output:** Single file `lib/types.mjs`

### Pattern 2: Batch Generation (Multiple Files)

Generate one file per entity.

**Template (`templates/class-schema.njk`):**

```njk
---
to: lib/schemas/{{ "?subject" | localName | kebabCase }}.mjs
sparql: |
  SELECT ?propertyName ?propertyType
  WHERE {
    ?subject rdfs:label ?className .
    ?subject ?property ?propertyType .
  }
---
export const schema = {
{% for prop in sparql_results %}
  {{ prop["?propertyName"] | camelCase }}: "{{ prop["?propertyType"] }}",
{% endfor %}
};
```

**Run with batch flag:**

```bash
pnpm unrdf template generate ontology.ttl \
  --template templates/class-schema.njk \
  --batch \
  --class-uri "http://example.org/Class"
```

**Output:** Multiple files: `lib/schemas/user.mjs`, `lib/schemas/post.mjs`, etc.

### Pattern 3: Conditional Skip (Dry Run)

Preview output without writing files.

```bash
pnpm unrdf template generate ontology.ttl \
  --template templates/types.njk \
  --dry-run --verbose
```

Shows:

- Output path
- First 50 lines of rendered content
- File size
- **No files written**

## Troubleshooting

### Error: "SPARQL query returns no results"

**Symptom:**

```
Error: No results returned from SPARQL query
```

**Fixes:**

1. **Check prefix declarations** — Ensure prefixes in SPARQL match your RDF file

```turtle
@prefix blog: <http://example.org/blog#> .

blog:User a rdfs:Class .
```

```sparql
PREFIX blog: <http://example.org/blog#>
SELECT ?class WHERE { ?class a rdfs:Class }
```

2. **Use `--verbose`** to see query execution details

```bash
pnpm unrdf template generate ontology.ttl --template t.njk --verbose
```

3. **Test SPARQL query** with a SPARQL editor (e.g., Apache Jena's SPARQL playground)

### Error: "Template file not found"

**Symptom:**

```
Error: Template not found: /path/to/template.njk
```

**Fix:** Verify path is correct and readable

```bash
ls -la templates/zod-schemas.njk
```

### Error: "Invalid YAML frontmatter"

**Symptom:**

```
Error: YAML parsing failed
```

**Fixes:**

- Check YAML indentation (use spaces, not tabs)
- Quote strings with special characters
- Verify colons are properly spaced

**Example (invalid):**

```yaml
---
to:src/schemas.mjs      # ❌ No space after colon
sparql: SELECT ?x       # ❌ Should be multiline string (use |)
---
```

**Example (valid):**

```yaml
---
to: src/schemas.mjs
sparql: |
  SELECT ?x
  WHERE { }
---
```

### Error: "Variable is undefined"

**Symptom:**

```
Error: Template rendering failed: className is undefined
```

**Fixes:**

1. **Use default filter** for optional values

```njk
{{ className | default("Unknown") }}
```

2. **Check SPARQL variable names** include `?` prefix in results

```njk
{% for row in sparql_results %}
{{ row["?className"] | camelCase }}
       ^
       Include ?
{% endfor %}
```

3. **Verify SPARQL returns expected columns**

```bash
pnpm unrdf template generate ontology.ttl --template t.njk --verbose
```

### Error: "Output path not specified"

**Symptom:**

```
Error: Template does not specify output path
```

**Fix:** Add `to:` in frontmatter

```yaml
---
to: src/output.mjs
---
```

## Available Filters Quick Reference

| Filter       | Example                                   | Result         |
| ------------ | ----------------------------------------- | -------------- |
| `camelCase`  | `"user-name" \| camelCase`                | `"userName"`   |
| `pascalCase` | `"user-name" \| pascalCase`               | `"UserName"`   |
| `snakeCase`  | `"userName" \| snakeCase`                 | `"user_name"`  |
| `kebabCase`  | `"userName" \| kebabCase`                 | `"user-name"`  |
| `zodType`    | `"xsd:string" \| zodType`                 | `"z.string()"` |
| `jsdocType`  | `"xsd:string" \| jsdocType`               | `"string"`     |
| `localName`  | `"http://schema.org/Person" \| localName` | `"Person"`     |
| `groupBy`    | `results \| groupBy("?class")`            | Grouped object |
| `sortBy`     | `results \| sortBy("?name", "asc")`       | Sorted array   |
| `indent(4)`  | `code \| indent(4)`                       | Indented text  |
| `date`       | `now \| date("YYYY-MM-DD")`               | `"2026-04-03"` |

For complete reference, see [template-command.md](./template-command.md).

## Next Steps

### 1. Explore More Examples

- **Batch generation:** See "Batch Generate Per-Class Files" in [template-command.md](./template-command.md#example-3-batch-generate-per-class-files)
- **TypeScript interfaces:** See "Generate TypeScript Interfaces" in [template-command.md](./template-command.md#example-2-generate-typescript-interfaces)
- **Inject into files:** See "Inject Into Existing File" in [template-command.md](./template-command.md#example-4-inject-into-existing-file)

### 2. Learn the Sync Command

For **large projects** with **multiple generation rules**, use `unrdf sync`:

```bash
pnpm unrdf sync --config unrdf.toml
```

See [sync-command.md](./sync-command.md) for:

- Configuration files (`unrdf.toml`)
- Multiple rules (batch operation)
- Dependency management between rules
- Watch mode (auto-regenerate on change)

### 3. Advanced Features

- **SPARQL context:** Extract and coerce RDF types
- **Hygen integration:** Line-based file modifications
- **Batch mode:** One file per RDF instance
- **Type filters:** Automatic Zod/JSDoc conversion

See [template-command.md](./template-command.md) for detailed reference.

## Tips & Best Practices

### Tip 1: Start Simple

Generate a single file with basic template before tackling batch mode or complex SPARQL.

### Tip 2: Use `--dry-run`

Always preview with `--dry-run` before writing files:

```bash
pnpm unrdf template generate ontology.ttl --template t.njk --dry-run --verbose
```

### Tip 3: Group SPARQL Results

Use `groupBy()` filter to organize results:

```njk
{% set classes = sparql_results | groupBy("?className") %}
{% for className, props in classes | items %}
  {% for prop in props %}
    ...
  {% endfor %}
{% endfor %}
```

### Tip 4: Test SPARQL Separately

Use a SPARQL editor to verify queries before adding to templates. This saves debugging time.

### Tip 5: Keep Templates DRY

Reuse filters and avoid duplicating logic:

```njk
{% macro schemaField(prop) %}
  {{ prop["?name"] | camelCase }}: {{ prop["?type"] | zodType }},
{% endmacro %}

{% for prop in sparql_results %}
  {{ schemaField(prop) }}
{% endfor %}
```

## Key Concepts

| Concept         | What It Does                                       | Example                                     |
| --------------- | -------------------------------------------------- | ------------------------------------------- |
| **RDF**         | Describes data as subject-predicate-object triples | User → hasEmail → john@example.com          |
| **SPARQL**      | Query language for RDF data                        | `SELECT ?name WHERE { ?x rdf:label ?name }` |
| **Nunjucks**    | Template language with filters                     | `{{ name \| camelCase }}`                   |
| **Frontmatter** | Metadata at top of template file                   | `to: src/output.mjs`                        |
| **Filter**      | Transforms template variables                      | `{{ text \| camelCase }}`                   |
| **Batch mode**  | Generate multiple files from one template          | One file per RDF instance                   |

## Limitations & Scope

| Feature                            | Status           | Notes                                     |
| ---------------------------------- | ---------------- | ----------------------------------------- |
| Multiple output files per template | ✅ Supported     | Use frontmatter `to:` with template vars  |
| Batch mode (per-instance files)    | ✅ Supported     | Requires `--batch` flag                   |
| Type coercion (Zod/JSDoc)          | ✅ Supported     | 20+ XSD types, custom filters             |
| File injection (Hygen-style)       | ✅ Supported     | `inject:`, `before:`, `after:` directives |
| Watch mode                         | ⚠️ Planned       | Currently use `--dry-run` for preview     |
| Custom SPARQL functions            | ❌ Not supported | Use standard SPARQL 1.1 only              |

## Getting Help

- **Command help:** `pnpm unrdf template generate --help`
- **Full reference:** [template-command.md](./template-command.md)
- **Sync command:** [sync-command.md](./sync-command.md)
- **Doctor diagnostics:** [doctor-command.md](./doctor-command.md)
- **Report issues:** GitHub Issues on [@unrdf/cli](https://github.com/unrdf/cli)

## Environment Validation

Before starting development, verify your environment is properly configured:

```bash
# Quick health check (30 seconds)
unrdf doctor

# Full diagnostics with quality checks
unrdf doctor --mode standard

# Auto-fix safe issues
unrdf doctor --fix

# Continuous monitoring
unrdf doctor --watch
```

**What doctor checks:**

- ✅ Node.js version (>= 18.0.0)
- ✅ pnpm version (>= 7.0.0)
- ✅ Build artifacts status
- ✅ MCP server status
- ✅ RDF store accessibility
- ✅ Port availability
- ✅ Code quality (coverage, lint, file size)
- ✅ External integrations (federation, OTEL, Docker)

See [doctor-command.md](./doctor-command.md) for complete documentation on diagnostics and troubleshooting.

---

**Ready to generate?** Start with the 5-minute quickstart above, then explore [template-command.md](./template-command.md) for advanced features.
