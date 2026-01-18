# ggen.toml Configuration Reference

> **Document Type**: Reference
> **Audience**: Developers configuring RDF API code generation
> **Package**: @unrdf/cli

---

## File Location

The `ggen.toml` configuration file must be placed in your project root directory. The `unrdf sync` command searches for this file in the current working directory.

```
project-root/
├── ggen.toml          # Configuration file (required)
├── ontology/
│   └── schema.ttl     # RDF ontology source
├── templates/
│   └── class.mjs.hbs  # Handlebars templates
└── lib/
    └── generated/     # Output directory
```

---

## Schema

### Complete Configuration Structure

```toml
[project]
name = "string"           # Required - Project identifier
version = "string"        # Optional - Default: "1.0.0"
description = "string"    # Optional - Project description

[ontology]
source = "path"           # Required - Path to ontology file
format = "turtle"         # Optional - Auto-detected from extension
base_iri = "uri"          # Optional - Base IRI for resolution
prefixes = { key = "uri" } # Optional - Prefix mappings

[generation]
output_dir = "path"       # Optional - Default: "lib"
templates_dir = "path"    # Optional - Custom templates directory
incremental = true        # Optional - Default: true

[[generation.rules]]
name = "string"           # Required - Rule identifier
description = "string"    # Optional - Rule description
template = "path"         # Required - Path to template file
output_file = "path"      # Required - Output file path
query = """               # Required - SPARQL query (multiline)
SELECT ?s ?p ?o
WHERE { ?s ?p ?o }
"""
enabled = true            # Optional - Default: true
```

---

## Field Descriptions

### [project] Section

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `name` | string | Yes | - | Project identifier used in generated file headers and logging output |
| `version` | string | No | `"1.0.0"` | Semantic version string for generated artifacts |
| `description` | string | No | `""` | Human-readable project description |

### [ontology] Section

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `source` | path | Yes | - | Path to the RDF ontology file (relative to ggen.toml) |
| `format` | enum | No | auto | RDF serialization format: `turtle`, `ntriples`, `rdfxml` |
| `base_iri` | URI | No | `""` | Base IRI for resolving relative IRIs in the ontology |
| `prefixes` | table | No | `{}` | Prefix-to-namespace mappings for SPARQL queries |

**Supported Formats**:

| Format | Extension | MIME Type |
|--------|-----------|-----------|
| `turtle` | `.ttl` | `text/turtle` |
| `ntriples` | `.nt` | `application/n-triples` |
| `rdfxml` | `.rdf`, `.xml` | `application/rdf+xml` |

### [generation] Section

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `output_dir` | path | No | `"lib"` | Base directory for generated files |
| `templates_dir` | path | No | built-in | Directory containing Handlebars templates |
| `incremental` | boolean | No | `true` | Skip unchanged files during regeneration |

### [[generation.rules]] Array

Each rule defines a code generation target. Multiple rules can be defined.

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `name` | string | Yes | - | Unique identifier for this generation rule |
| `description` | string | No | `""` | Human-readable description of what this rule generates |
| `template` | path | Yes | - | Path to Handlebars template file |
| `output_file` | path | Yes | - | Output file path (relative to `output_dir`) |
| `query` | string | Yes | - | SPARQL SELECT query to extract data for template |
| `enabled` | boolean | No | `true` | Set to `false` to skip this rule |

---

## Environment Variables

Environment variable substitution is supported in all string values.

### Basic Syntax

```toml
[ontology]
source = "${ONTOLOGY_PATH}"
base_iri = "${BASE_IRI}"
```

### Default Value Syntax

```toml
[ontology]
source = "${ONTOLOGY_PATH:-./ontology/schema.ttl}"
base_iri = "${BASE_IRI:-https://example.org/}"

[generation]
output_dir = "${OUTPUT_DIR:-lib}"
```

### Supported Locations

Environment variables can be used in:

| Section | Fields |
|---------|--------|
| `[project]` | `name`, `version`, `description` |
| `[ontology]` | `source`, `base_iri`, prefix values |
| `[generation]` | `output_dir`, `templates_dir` |
| `[[generation.rules]]` | `template`, `output_file`, `query` |

### Examples

```toml
[project]
name = "${PROJECT_NAME:-my-rdf-api}"

[ontology]
source = "${SCHEMA_PATH:-./ontology/schema.ttl}"
prefixes = { ex = "${EXAMPLE_NS:-https://example.org/}" }

[generation]
output_dir = "${GEN_OUTPUT:-lib/generated}"
```

---

## Path Resolution

All relative paths are resolved from the directory containing `ggen.toml`.

### Resolution Rules

1. **Absolute paths**: Used as-is
2. **Relative paths**: Resolved from ggen.toml directory
3. **Tilde expansion**: `~` expands to user home directory

### Path Fields

| Field | Resolution Base |
|-------|-----------------|
| `ontology.source` | ggen.toml directory |
| `generation.output_dir` | ggen.toml directory |
| `generation.templates_dir` | ggen.toml directory |
| `generation.rules[].template` | `templates_dir` if set, else ggen.toml directory |
| `generation.rules[].output_file` | `output_dir` |

### Example Resolution

Given this structure:
```
/home/user/project/
├── ggen.toml
├── ontology/
│   └── schema.ttl
├── templates/
│   └── class.mjs.hbs
└── lib/
```

And this configuration:
```toml
[ontology]
source = "ontology/schema.ttl"  # Resolves to /home/user/project/ontology/schema.ttl

[generation]
output_dir = "lib"              # Resolves to /home/user/project/lib
templates_dir = "templates"     # Resolves to /home/user/project/templates

[[generation.rules]]
template = "class.mjs.hbs"      # Resolves to /home/user/project/templates/class.mjs.hbs
output_file = "models/User.mjs" # Resolves to /home/user/project/lib/models/User.mjs
```

---

## Complete Example

```toml
# ggen.toml - RDF API Code Generation Configuration
# Full example demonstrating all features

[project]
name = "user-management-api"
version = "2.1.0"
description = "Generated API from FOAF and custom user ontology"

[ontology]
source = "${ONTOLOGY_PATH:-./ontology/user-schema.ttl}"
format = "turtle"
base_iri = "https://example.org/users/"
prefixes = {
  foaf = "http://xmlns.com/foaf/0.1/",
  schema = "https://schema.org/",
  user = "https://example.org/users/",
  xsd = "http://www.w3.org/2001/XMLSchema#"
}

[generation]
output_dir = "${OUTPUT_DIR:-lib/generated}"
templates_dir = "./templates"
incremental = true

# Generate JavaScript class for each RDFS class
[[generation.rules]]
name = "js-classes"
description = "Generate JavaScript ES6 classes from RDFS classes"
template = "class.mjs.hbs"
output_file = "models/{{className}}.mjs"
enabled = true
query = """
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>

SELECT ?class ?label ?comment
WHERE {
  ?class a rdfs:Class .
  OPTIONAL { ?class rdfs:label ?label }
  OPTIONAL { ?class rdfs:comment ?comment }
  FILTER (!isBlank(?class))
}
ORDER BY ?class
"""

# Generate TypeScript type definitions
[[generation.rules]]
name = "ts-types"
description = "Generate TypeScript interfaces from RDFS classes"
template = "types.d.ts.hbs"
output_file = "types/index.d.ts"
enabled = true
query = """
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT ?class ?property ?range ?label
WHERE {
  ?class a rdfs:Class .
  ?property rdfs:domain ?class .
  OPTIONAL { ?property rdfs:range ?range }
  OPTIONAL { ?property rdfs:label ?label }
}
ORDER BY ?class ?property
"""

# Generate Zod validation schemas
[[generation.rules]]
name = "zod-schemas"
description = "Generate Zod validation schemas from SHACL shapes"
template = "schema.mjs.hbs"
output_file = "schemas/{{shapeName}}.schema.mjs"
enabled = true
query = """
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT ?shape ?property ?path ?datatype ?minCount ?maxCount
WHERE {
  ?shape a sh:NodeShape ;
         sh:property ?propSpec .
  ?propSpec sh:path ?path .
  OPTIONAL { ?propSpec sh:datatype ?datatype }
  OPTIONAL { ?propSpec sh:minCount ?minCount }
  OPTIONAL { ?propSpec sh:maxCount ?maxCount }
}
ORDER BY ?shape ?path
"""

# Generate API routes (disabled by default)
[[generation.rules]]
name = "api-routes"
description = "Generate Express.js API routes"
template = "routes.mjs.hbs"
output_file = "routes/index.mjs"
enabled = false
query = """
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX hydra: <http://www.w3.org/ns/hydra/core#>

SELECT ?operation ?method ?path ?expects ?returns
WHERE {
  ?operation a hydra:Operation ;
             hydra:method ?method .
  OPTIONAL { ?operation hydra:expects ?expects }
  OPTIONAL { ?operation hydra:returns ?returns }
}
"""
```

---

## Validation

The CLI validates `ggen.toml` on load and reports specific errors:

| Error | Cause | Resolution |
|-------|-------|------------|
| `Missing required field: project.name` | No project name specified | Add `name` under `[project]` |
| `Missing required field: ontology.source` | No ontology path | Add `source` under `[ontology]` |
| `Ontology file not found` | Invalid source path | Check path exists |
| `Invalid format` | Unsupported RDF format | Use `turtle`, `ntriples`, or `rdfxml` |
| `SPARQL parse error` | Invalid query syntax | Check query in rule |
| `Template not found` | Missing template file | Check template path |

---

## See Also

- [Tutorial: Your First Code Generation](/docs/diataxis/tutorials/sync-quickstart.md)
- [How-To: Custom Templates](/docs/diataxis/how-to/sync-custom-templates.md)
- [Explanation: Code Generation Architecture](/docs/diataxis/explanation/sync-architecture.md)
