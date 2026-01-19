# Your First API from RDF Ontology

**Objective:** Generate a complete REST API contract (OpenAPI, Zod schemas, TypeScript types) from an RDF ontology using only CLI commands - no hand coding.

**Audience:** Intermediate - familiar with REST APIs and basic RDF concepts

**Prerequisites:**
- Node.js >= 18.0.0 installed
- pnpm installed (`npm install -g pnpm`)
- Basic understanding of RDF triples and Turtle syntax
- Familiarity with OpenAPI and Zod validation
- **Capability Atoms:** `@unrdf/cli` (sync), `@unrdf/oxigraph` (SPARQL)

**Estimated Time:** 20 minutes

---

## What You Will Build

By the end of this tutorial, you will:

1. Create an RDF ontology defining User and Post entities
2. Configure code generation rules with `ggen.toml`
3. Write Nunjucks templates for Zod schemas and OpenAPI
4. Generate production-ready code with `unrdf sync`
5. Verify the generated artifacts work correctly

**Final Result:** A complete API contract with Zod validation schemas, JSDoc types, and OpenAPI specification - all generated from a single source of truth (your RDF ontology).

```
blog-api/
  ontology/
    blog-api.ttl       # Your RDF ontology (source of truth)
  templates/
    zod-schemas.njk    # Zod schema template
    openapi-spec.njk   # OpenAPI template
    jsdoc-types.njk    # JSDoc types template
  lib/
    schemas/
      entities.mjs     # Generated Zod schemas
    types/
      entities.mjs     # Generated JSDoc types
    openapi/
      api.yaml         # Generated OpenAPI spec
```

---

## Step 1: Set Up Your Project (3 minutes)

Create a new project directory and initialize it with the UNRDF CLI.

**Create the project structure:**

```bash
mkdir -p blog-api/{ontology,templates,lib}
cd blog-api
```

**Initialize a package.json:**

```bash
pnpm init
```

**Install the UNRDF CLI:**

```bash
pnpm add -D @unrdf/cli
```

**Verify installation:**

```bash
npx unrdf --version
```

**Expected output:**

```
@unrdf/cli v6.0.0
```

You now have a working project with the UNRDF CLI installed.

---

## Step 2: Create Your RDF Ontology (5 minutes)

Create the ontology file that defines your API entities. This is your single source of truth - all generated code derives from this file.

**Create `ontology/blog-api.ttl`:**

```turtle
@prefix api: <http://example.org/api#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# =============================================================================
# ENTITIES
# =============================================================================

# User Entity
api:User a api:Entity ;
    api:name "User" ;
    api:description "A registered user in the system" ;
    api:hasProperty api:User_id, api:User_email, api:User_name, api:User_createdAt .

api:User_id
    api:name "id" ;
    api:type xsd:string ;
    api:required true ;
    api:description "Unique identifier" .

api:User_email
    api:name "email" ;
    api:type xsd:string ;
    api:format "email" ;
    api:required true ;
    api:description "User email address" .

api:User_name
    api:name "name" ;
    api:type xsd:string ;
    api:required true ;
    api:description "Display name" .

api:User_createdAt
    api:name "createdAt" ;
    api:type xsd:dateTime ;
    api:required false ;
    api:description "Account creation timestamp" .

# Post Entity
api:Post a api:Entity ;
    api:name "Post" ;
    api:description "A blog post authored by a user" ;
    api:hasProperty api:Post_id, api:Post_title, api:Post_content, api:Post_authorId, api:Post_published .

api:Post_id
    api:name "id" ;
    api:type xsd:string ;
    api:required true ;
    api:description "Unique identifier" .

api:Post_title
    api:name "title" ;
    api:type xsd:string ;
    api:required true ;
    api:description "Post title" .

api:Post_content
    api:name "content" ;
    api:type xsd:string ;
    api:required true ;
    api:description "Post body content" .

api:Post_authorId
    api:name "authorId" ;
    api:type xsd:string ;
    api:required true ;
    api:description "Reference to the author User" .

api:Post_published
    api:name "published" ;
    api:type xsd:boolean ;
    api:required false ;
    api:description "Whether the post is published" .

# =============================================================================
# API OPERATIONS
# =============================================================================

api:GetUsers a api:Operation ;
    api:path "/users" ;
    api:method "GET" ;
    api:operationId "getUsers" ;
    api:summary "List all users" ;
    api:tag "Users" ;
    api:responseSchema api:User .

api:GetUser a api:Operation ;
    api:path "/users/{id}" ;
    api:method "GET" ;
    api:operationId "getUser" ;
    api:summary "Get user by ID" ;
    api:tag "Users" ;
    api:responseSchema api:User .

api:CreateUser a api:Operation ;
    api:path "/users" ;
    api:method "POST" ;
    api:operationId "createUser" ;
    api:summary "Create a new user" ;
    api:tag "Users" ;
    api:requestSchema api:User ;
    api:responseSchema api:User .

api:GetPosts a api:Operation ;
    api:path "/posts" ;
    api:method "GET" ;
    api:operationId "getPosts" ;
    api:summary "List all posts" ;
    api:tag "Posts" ;
    api:responseSchema api:Post .

api:CreatePost a api:Operation ;
    api:path "/posts" ;
    api:method "POST" ;
    api:operationId "createPost" ;
    api:summary "Create a new post" ;
    api:tag "Posts" ;
    api:requestSchema api:Post ;
    api:responseSchema api:Post .
```

**What you defined:**

- **2 Entities**: User (4 properties) and Post (5 properties)
- **5 Operations**: CRUD endpoints for users and posts
- **Type information**: XSD types, required flags, descriptions, and formats

This ontology is the single source of truth for your entire API contract.

---

## Step 3: Configure Code Generation (3 minutes)

Create a configuration file that tells `unrdf sync` how to generate code from your ontology.

**Create `ggen.toml`:**

```toml
[project]
name = "blog-api"
version = "1.0.0"
description = "Blog API generated from RDF ontology"

[ontology]
source = "ontology/blog-api.ttl"
format = "turtle"

[ontology.prefixes]
api = "http://example.org/api#"
xsd = "http://www.w3.org/2001/XMLSchema#"

[generation]
output_dir = "lib"

# Rule 1: Generate Zod validation schemas
[[generation.rules]]
name = "zod-schemas"
description = "Generate Zod validation schemas for all entities"
template = "templates/zod-schemas.njk"
output_file = "schemas/entities.mjs"
query = """
SELECT ?entityName ?entityDesc ?propertyName ?propertyType ?required ?format ?propertyDesc
WHERE {
  ?entity a api:Entity ;
          api:name ?entityName ;
          api:hasProperty ?property .
  OPTIONAL { ?entity api:description ?entityDesc }
  ?property api:name ?propertyName ;
            api:type ?propertyType .
  OPTIONAL { ?property api:required ?required }
  OPTIONAL { ?property api:format ?format }
  OPTIONAL { ?property api:description ?propertyDesc }
}
ORDER BY ?entityName ?propertyName
"""

# Rule 2: Generate JSDoc type definitions
[[generation.rules]]
name = "jsdoc-types"
description = "Generate JSDoc type definitions"
template = "templates/jsdoc-types.njk"
output_file = "types/entities.mjs"
query = """
SELECT ?entityName ?entityDesc ?propertyName ?propertyType ?required ?propertyDesc
WHERE {
  ?entity a api:Entity ;
          api:name ?entityName ;
          api:hasProperty ?property .
  OPTIONAL { ?entity api:description ?entityDesc }
  ?property api:name ?propertyName ;
            api:type ?propertyType .
  OPTIONAL { ?property api:required ?required }
  OPTIONAL { ?property api:description ?propertyDesc }
}
ORDER BY ?entityName ?propertyName
"""

# Rule 3: Generate OpenAPI specification
[[generation.rules]]
name = "openapi-spec"
description = "Generate OpenAPI 3.0 specification"
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
  OPTIONAL { ?op api:requestSchema ?req . ?req api:name ?requestSchema }
  OPTIONAL { ?op api:responseSchema ?res . ?res api:name ?responseSchema }
}
ORDER BY ?path ?method
"""
```

**What this configures:**

- **Project metadata**: Name, version, description
- **Ontology source**: Path to your Turtle file with prefix mappings
- **3 generation rules**: Each rule pairs a SPARQL query with a Nunjucks template

The SPARQL queries extract structured data from your ontology, which templates transform into code.

---

## Step 4: Create Nunjucks Templates (5 minutes)

Create templates that transform SPARQL results into generated code.

### Template 1: Zod Schemas

**Create `templates/zod-schemas.njk`:**

```njk
---
to: {{ output_dir }}/schemas/entities.mjs
description: Zod validation schemas generated from RDF ontology
---
/**
 * @file Entity Validation Schemas
 * @description Auto-generated Zod schemas from blog-api.ttl
 * @generated {{ now | date("YYYY-MM-DD HH:mm:ss") }}
 * @see ontology/blog-api.ttl
 */
import { z } from 'zod';

{% set entities = sparql_results | groupBy("?entityName") %}
{% for entityName, props in entities | items %}
/**
 * {{ entityName }} schema
 * @description {{ props[0]["?entityDesc"] | default("No description") }}
 */
export const {{ entityName | camelCase }}Schema = z.object({
{% for row in props %}
  /**
   * {{ row["?propertyDesc"] | default(row["?propertyName"]) }}
   */
  {{ row["?propertyName"] | camelCase }}: {{ row["?propertyType"] | zodType }}{% if row["?format"] == "email" %}.email(){% endif %}{% if row["?required"] != "true" %}.optional(){% endif %},
{% endfor %}
});

/**
 * Inferred {{ entityName }} type from Zod schema
 * @typedef {z.infer<typeof {{ entityName | camelCase }}Schema>} {{ entityName }}
 */

{% endfor %}
/**
 * All entity schemas indexed by name
 */
export const schemas = {
{% for entityName in entities | keys %}
  {{ entityName | camelCase }}: {{ entityName | camelCase }}Schema,
{% endfor %}
};

/**
 * Validate data against a named schema
 * @param {string} schemaName - Name of the schema
 * @param {unknown} data - Data to validate
 * @returns {import('zod').SafeParseReturnType} Validation result
 */
export function validateEntity(schemaName, data) {
  const schema = schemas[schemaName];
  if (!schema) {
    throw new Error(`Unknown schema: ${schemaName}`);
  }
  return schema.safeParse(data);
}
```

### Template 2: JSDoc Types

**Create `templates/jsdoc-types.njk`:**

```njk
---
to: {{ output_dir }}/types/entities.mjs
description: JSDoc type definitions generated from RDF ontology
---
/**
 * @file Entity Type Definitions
 * @description Auto-generated JSDoc types from blog-api.ttl
 * @generated {{ now | date("YYYY-MM-DD HH:mm:ss") }}
 * @see ontology/blog-api.ttl
 */

{% set entities = sparql_results | groupBy("?entityName") %}
{% for entityName, props in entities | items %}
/**
 * {{ props[0]["?entityDesc"] | default(entityName + " entity") }}
 * @typedef {Object} {{ entityName }}
{% for row in props %}
 * @property {{ "{" }}{{ row["?propertyType"] | jsdocType }}{% if row["?required"] != "true" %}|undefined{% endif %}{{ "}" }} {{ row["?propertyName"] | camelCase }} - {{ row["?propertyDesc"] | default("No description") }}
{% endfor %}
 */

{% endfor %}
/**
 * Entity type registry for runtime type checking
 * @type {Object<string, string[]>}
 */
export const entityProperties = {
{% for entityName, props in entities | items %}
  {{ entityName }}: [{% for row in props %}'{{ row["?propertyName"] | camelCase }}'{% if not loop.last %}, {% endif %}{% endfor %}],
{% endfor %}
};

/**
 * Check if an object matches an entity structure
 * @param {string} entityName - Name of the entity type
 * @param {Object} obj - Object to check
 * @returns {boolean} Whether the object has all required properties
 */
export function hasEntityShape(entityName, obj) {
  const props = entityProperties[entityName];
  if (!props) return false;
  return props.every(prop => prop in obj);
}
```

### Template 3: OpenAPI Specification

**Create `templates/openapi-spec.njk`:**

```njk
---
to: {{ output_dir }}/openapi/api.yaml
description: OpenAPI 3.0 specification generated from RDF ontology
---
# Auto-generated OpenAPI specification
# Generated: {{ now | date("YYYY-MM-DD HH:mm:ss") }}
# Source: ontology/blog-api.ttl

openapi: "3.0.3"
info:
  title: {{ project.name }}
  version: {{ project.version }}
  description: {{ project.description }}

servers:
  - url: http://localhost:3000
    description: Development server

tags:
{% set operations = sparql_results | groupBy("?tag") %}
{% for tag in operations | keys %}
  - name: {{ tag }}
{% endfor %}

paths:
{% set byPath = sparql_results | groupBy("?path") %}
{% for path, ops in byPath | items %}
  {{ path }}:
{% for op in ops %}
    {{ op["?method"] | lower }}:
      operationId: {{ op["?operationId"] }}
      summary: {{ op["?summary"] | default(op["?operationId"]) }}
{% if op["?tag"] %}
      tags:
        - {{ op["?tag"] }}
{% endif %}
{% if op["?requestSchema"] %}
      requestBody:
        required: true
        content:
          application/json:
            schema:
              $ref: "#/components/schemas/{{ op["?requestSchema"] }}"
{% endif %}
      responses:
        "200":
          description: Success
{% if op["?responseSchema"] %}
          content:
            application/json:
              schema:
                $ref: "#/components/schemas/{{ op["?responseSchema"] }}"
{% endif %}
        "400":
          description: Bad Request
        "404":
          description: Not Found
{% endfor %}
{% endfor %}

components:
  schemas:
    User:
      type: object
      required:
        - id
        - email
        - name
      properties:
        id:
          type: string
          description: Unique identifier
        email:
          type: string
          format: email
          description: User email address
        name:
          type: string
          description: Display name
        createdAt:
          type: string
          format: date-time
          description: Account creation timestamp

    Post:
      type: object
      required:
        - id
        - title
        - content
        - authorId
      properties:
        id:
          type: string
          description: Unique identifier
        title:
          type: string
          description: Post title
        content:
          type: string
          description: Post body content
        authorId:
          type: string
          description: Reference to the author User
        published:
          type: boolean
          description: Whether the post is published
```

You now have three templates that will transform your ontology into code.

---

## Step 5: Generate Code (2 minutes)

Run the sync command to generate all code from your ontology.

**Preview changes first (dry run):**

```bash
npx unrdf sync --dry-run --verbose
```

**Expected output:**

```
[unrdf sync] Loading configuration from ggen.toml
[unrdf sync] Parsing ontology: ontology/blog-api.ttl (turtle)
[unrdf sync] Found 3 generation rules

[DRY RUN] Would execute rule: zod-schemas
  Query returned 9 results
  Would write: lib/schemas/entities.mjs

[DRY RUN] Would execute rule: jsdoc-types
  Query returned 9 results
  Would write: lib/types/entities.mjs

[DRY RUN] Would execute rule: openapi-spec
  Query returned 5 results
  Would write: lib/openapi/api.yaml

[unrdf sync] Dry run complete. No files written.
```

**Generate the code:**

```bash
npx unrdf sync --verbose
```

**Expected output:**

```
[unrdf sync] Loading configuration from ggen.toml
[unrdf sync] Parsing ontology: ontology/blog-api.ttl (turtle)
[unrdf sync] Found 3 generation rules

[unrdf sync] Executing rule: zod-schemas
  Query returned 9 results
  Generated: lib/schemas/entities.mjs (67 lines)

[unrdf sync] Executing rule: jsdoc-types
  Query returned 9 results
  Generated: lib/types/entities.mjs (42 lines)

[unrdf sync] Executing rule: openapi-spec
  Query returned 5 results
  Generated: lib/openapi/api.yaml (98 lines)

[unrdf sync] Generation complete!
  Rules executed: 3
  Files generated: 3
  Total lines: 207
```

Your API contract is now generated.

---

## Step 6: Verify Generated Output (2 minutes)

Inspect the generated files to confirm they match your ontology.

**Check generated Zod schemas:**

```bash
cat lib/schemas/entities.mjs
```

**Expected output (partial):**

```javascript
/**
 * @file Entity Validation Schemas
 * @description Auto-generated Zod schemas from blog-api.ttl
 * @generated 2026-01-18 12:00:00
 * @see ontology/blog-api.ttl
 */
import { z } from 'zod';

/**
 * User schema
 * @description A registered user in the system
 */
export const userSchema = z.object({
  /**
   * Account creation timestamp
   */
  createdAt: z.string().datetime().optional(),
  /**
   * User email address
   */
  email: z.string().email(),
  /**
   * Unique identifier
   */
  id: z.string(),
  /**
   * Display name
   */
  name: z.string(),
});

/**
 * Post schema
 * @description A blog post authored by a user
 */
export const postSchema = z.object({
  /**
   * Reference to the author User
   */
  authorId: z.string(),
  /**
   * Post body content
   */
  content: z.string(),
  /**
   * Unique identifier
   */
  id: z.string(),
  /**
   * Whether the post is published
   */
  published: z.boolean().optional(),
  /**
   * Post title
   */
  title: z.string(),
});

export const schemas = {
  user: userSchema,
  post: postSchema,
};
```

**Check generated OpenAPI spec:**

```bash
cat lib/openapi/api.yaml
```

**Verify the file structure:**

```bash
find lib -type f -name "*.mjs" -o -name "*.yaml" | sort
```

**Expected output:**

```
lib/openapi/api.yaml
lib/schemas/entities.mjs
lib/types/entities.mjs
```

**Test schema validation (optional):**

Create a quick test file to verify the schemas work:

```bash
cat > test-schemas.mjs << 'EOF'
import { userSchema, postSchema, validateEntity } from './lib/schemas/entities.mjs';

// Valid user
const validUser = {
  id: '123',
  email: 'user@example.com',
  name: 'Test User'
};

// Invalid user (missing required field)
const invalidUser = {
  id: '456',
  name: 'Missing Email'
};

console.log('Valid user:', userSchema.safeParse(validUser).success);
console.log('Invalid user:', userSchema.safeParse(invalidUser).success);
console.log('Validation errors:', userSchema.safeParse(invalidUser).error?.issues);
EOF

node test-schemas.mjs
```

**Expected output:**

```
Valid user: true
Invalid user: false
Validation errors: [ { code: 'invalid_type', expected: 'string', received: 'undefined', path: [ 'email' ], message: 'Required' } ]
```

---

## What You Learned

You have successfully completed this tutorial. Here is what you accomplished:

- Created an RDF ontology as a single source of truth for your API
- Configured code generation rules with SPARQL queries
- Built Nunjucks templates for Zod, JSDoc, and OpenAPI output
- Generated production-ready code with `unrdf sync`
- Verified the generated artifacts work correctly

**Key concepts:**

| Concept | What It Does |
|---------|--------------|
| **RDF Ontology** | Defines entities, properties, and operations semantically |
| **SPARQL Queries** | Extracts structured data from the ontology |
| **Nunjucks Templates** | Transforms query results into code |
| **ggen.toml** | Configures the generation pipeline |
| **unrdf sync** | Executes the generation process |

**The power of this approach:**

1. **Single source of truth**: Change the ontology, regenerate everything
2. **No hand-coding**: All schemas, types, and specs are derived
3. **Consistency guaranteed**: Generated code always matches the ontology
4. **Version controlled**: Ontology changes are tracked in git

---

## Next Steps

**Continue Learning:**
- **[Tutorial: Building Ontologies with Templates](./building-ontologies-with-templates.md)** - Advanced ontology design
- **[Tutorial: SPARQL Query Generation](./sparql-query-generation.md)** - Complex SPARQL patterns

**Solve Specific Problems:**
- **[How-To: Add Custom Filters to Templates](../how-to/custom-template-filters.md)** - Extend Nunjucks
- **[How-To: Watch Mode for Development](../how-to/sync-watch-mode.md)** - Auto-regenerate on changes

**Understand the Design:**
- **[Explanation: Why Ontology-Driven Code Generation](../explanation/ontology-driven-codegen.md)** - Design rationale
- **[Explanation: SPARQL as a Query Language](../explanation/sparql-for-code-generation.md)** - Why SPARQL

**API Reference:**
- **[Reference: ggen.toml Configuration](../reference/ggen-toml-schema.md)** - Complete configuration options
- **[Reference: Template Filters](../reference/template-filters.md)** - All available Nunjucks filters
- **[CLI Reference: unrdf sync](../reference/cli-sync.md)** - Full command options

---

## Troubleshooting

### Ontology file not found

```
Error: Ontology file not found: ontology/blog-api.ttl
```

**Solution:** Verify the path in `ggen.toml` is correct and the file exists.

### SPARQL query returns no results

```
[unrdf sync] Query returned 0 results
```

**Solution:** Check your SPARQL query prefixes match the ontology. Use `--verbose` to see the full query. Test the query in a SPARQL editor first.

### Template rendering error

```
Error: Template rendering failed: undefined is not iterable
```

**Solution:** Ensure you handle empty results with `| default("")` and check that `groupBy` receives the correct variable name (including the `?` prefix).

### Generated files not written

```
[unrdf sync] Skipping: lib/schemas/entities.mjs (already exists)
```

**Solution:** Use `--force` flag to overwrite existing files, or delete them first.

---

**Questions?** Check [/home/user/unrdf/docs/TROUBLESHOOTING.md](/home/user/unrdf/docs/TROUBLESHOOTING.md) or file an issue.
