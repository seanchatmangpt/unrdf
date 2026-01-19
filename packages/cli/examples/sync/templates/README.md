# Code Generation Templates

This directory contains example Nunjucks templates for generating code from RDF ontologies using the `unrdf sync` command.

## Available Templates

### 1. `entity-types.njk`

Generates JSDoc type definitions from RDF ontology classes and properties.

**Output**: `types/entities.mjs`

**Features**:
- JSDoc `@typedef` definitions for each entity
- TypeScript-compatible type annotations
- Entity type constants (enum-like object)
- Property name constants
- Utility functions for type checking
- Generation metadata

**Example Output**:
```javascript
/**
 * @typedef {Object} User
 * @property {string} email - User email address
 * @property {string} username - User's unique username
 */
```

### 2. `zod-schemas.njk`

Generates Zod validation schemas from RDF ontology.

**Output**: `schemas/entities.mjs`

**Features**:
- Zod schema definitions for runtime validation
- Type inference support (`z.infer<typeof schema>`)
- Validation functions (safe parse)
- Assertion functions (throws on error)
- Schema registry for dynamic lookup
- Base RDF schemas (IRI, Literal, BlankNode)
- Generation metadata

**Example Output**:
```javascript
export const userSchema = z.object({
  email: z.string().describe('User email address'),
  username: z.string(),
});

export function validateUser(data) {
  return userSchema.safeParse(data);
}
```

### 3. `constants.njk`

Generates constant definitions for RDF URIs, classes, and properties.

**Output**: `constants/ontology.mjs`

**Features**:
- Entity type URI constants
- Property URI constants (grouped and flattened)
- Namespace prefix definitions
- Common RDF/RDFS/OWL/XSD constants
- IRI expansion/compaction utilities
- Complete IRI registry
- Generation metadata

**Example Output**:
```javascript
export const ENTITY_TYPES = {
  USER: 'http://example.org/schema#User',
  POST: 'http://example.org/schema#Post',
};

export const PROPERTIES = {
  USER: {
    EMAIL: 'http://example.org/schema#email',
    USERNAME: 'http://example.org/schema#username',
  },
};
```

## Usage

### 1. Create a SPARQL Query

First, define a SPARQL query to extract data from your ontology:

```sparql
SELECT ?entityName ?entityIRI ?entityDescription
       ?propertyName ?propertyIRI ?propertyType ?propertyDescription ?required
WHERE {
  ?entity a owl:Class ;
          rdfs:label ?entityName .
  BIND(str(?entity) AS ?entityIRI)
  OPTIONAL { ?entity rdfs:comment ?entityDescription }

  OPTIONAL {
    ?property rdfs:domain ?entity ;
              rdfs:range ?propertyType ;
              rdfs:label ?propertyName .
    BIND(str(?property) AS ?propertyIRI)
    OPTIONAL { ?property rdfs:comment ?propertyDescription }
    OPTIONAL { ?property sh:minCount ?minCount }
    BIND(IF(BOUND(?minCount) && ?minCount > 0, "true", "false") AS ?required)
  }
}
ORDER BY ?entityName ?propertyName
```

### 2. Configure `ggen.toml`

Create a configuration file referencing the templates:

```toml
[project]
name = "my-project"
version = "1.0.0"

[ontology]
source = "schema.ttl"
format = "turtle"

[generation]
output_dir = "generated"

[[generation.rules]]
name = "types"
description = "Generate JSDoc type definitions"
query = "queries/entities.sparql"
template = "templates/entity-types.njk"
enabled = true

[[generation.rules]]
name = "schemas"
description = "Generate Zod validation schemas"
query = "queries/entities.sparql"
template = "templates/zod-schemas.njk"
enabled = true

[[generation.rules]]
name = "constants"
description = "Generate ontology constants"
query = "queries/entities.sparql"
template = "templates/constants.njk"
enabled = true
```

### 3. Run the Sync Command

```bash
# Generate all outputs
unrdf sync

# Generate specific rule only
unrdf sync --rule types

# Dry run (preview without writing)
unrdf sync --dry-run

# Verbose output
unrdf sync --verbose
```

## Template Context

All templates have access to:

### Variables

- `sparql_results` - Array of SPARQL query results
- `results` - Alias for `sparql_results`
- `now` - Current timestamp (Date object)
- `prefixes` - Namespace prefix mappings
- `output_dir` - Output directory path

### Filters

#### Case Conversion
- `camelCase` - `hello-world` → `helloWorld`
- `pascalCase` - `hello-world` → `HelloWorld`
- `snakeCase` - `helloWorld` → `hello_world`
- `kebabCase` - `HelloWorld` → `hello-world`
- `upper` - Convert to uppercase
- `lower` - Convert to lowercase

#### RDF Filters
- `localName` - Extract local name from IRI
- `namespace` - Extract namespace from IRI
- `expand` - Expand prefixed name to full IRI
- `toTurtle` - Convert triples to Turtle format

#### Type Filters
- `zodType` - Convert XSD type to Zod schema (e.g., `xsd:string` → `z.string()`)
- `jsdocType` - Convert XSD type to JSDoc type (e.g., `xsd:string` → `string`)

#### Data Filters
- `groupBy` - Group array by key
- `distinctValues` - Get unique values for key
- `sortBy` - Sort array by key
- `keys` - Get object keys
- `values` - Get object values
- `items` - Get object entries (key-value pairs)

#### String Filters
- `indent` - Indent text by N spaces
- `quote` - Quote string with character
- `date` - Format date (e.g., `YYYY-MM-DD`, `HH:mm:ss`)

### Example: Using Filters

```nunjucks
{# Group results by entity name #}
{% set entities = sparql_results | groupBy('entityName') %}

{# Iterate over grouped entities #}
{% for entityName, properties in entities | items | sortBy(0) %}
  {# Convert to PascalCase #}
  export const {{ entityName | pascalCase }}Schema = z.object({
    {% for prop in properties | sortBy('propertyName') %}
      {# Convert property name to camelCase and type to Zod #}
      {{ prop.propertyName | camelCase }}: {{ prop.propertyType | zodType }},
    {% endfor %}
  });
{% endfor %}
```

## Expected SPARQL Result Structure

Templates expect SPARQL results with these variable bindings:

```javascript
{
  "entityName": "User",              // Required: Entity/class name
  "entityIRI": "http://...",         // Optional: Full IRI
  "entityDescription": "...",        // Optional: Description
  "propertyName": "email",           // Optional: Property name
  "propertyIRI": "http://...",       // Optional: Property IRI
  "propertyType": "xsd:string",      // Optional: Property type
  "propertyDescription": "...",      // Optional: Property description
  "required": "true"                 // Optional: Is property required?
}
```

## Customization

To create custom templates:

1. Copy one of the example templates as a starting point
2. Modify the YAML frontmatter:
   - `to:` - Output path (supports template variables)
   - `description:` - Template description
3. Customize the Nunjucks template body
4. Use available filters and context variables
5. Add template to `ggen.toml` configuration

## Testing Templates

Test templates with a dry run:

```bash
# Preview generated output without writing files
unrdf sync --dry-run --verbose

# Generate to temporary directory
unrdf sync --output-dir /tmp/test-output
```

## More Information

- [Sync Command Documentation](../../../docs/sync-command.md)
- [Nunjucks Template Syntax](https://mozilla.github.io/nunjucks/templating.html)
- [SPARQL Query Language](https://www.w3.org/TR/sparql11-query/)
- [Zod Documentation](https://zod.dev/)
