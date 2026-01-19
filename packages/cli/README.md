# @unrdf/cli

Command-line interface for RDF knowledge graph operations, SPARQL queries, and ontology-driven code generation.

## Overview

`@unrdf/cli` provides a unified command-line tool for working with RDF data in the UNRDF ecosystem. Features include:

- **Graph Operations**: Create, load, query, and export RDF datasets
- **SPARQL Queries**: Execute SELECT, CONSTRUCT, ASK, and DESCRIBE queries
- **Code Generation**: Transform RDF ontologies into typed code artifacts (Zod schemas, OpenAPI specs, JSDoc types)
- **Context Management**: Manage RDF named graphs and contexts
- **Hook Evaluation**: Test and debug hook policies

## Installation

```bash
# Global installation
pnpm add -g @unrdf/cli

# Or use with pnpm exec
pnpm exec unrdf <command>

# Or via npx
npx @unrdf/cli <command>
```

## Quick Start

### Create and Query a Graph

```bash
# Create a new graph
unrdf graph create --name my-dataset

# Load RDF data
unrdf graph load --graph my-dataset --file data.ttl

# Execute SPARQL query
unrdf graph query --graph my-dataset --query "SELECT ?s WHERE { ?s ?p ?o }"

# Export to different format
unrdf graph export --graph my-dataset --format jsonld > output.jsonld
```

### Complete Workflow Example

```bash
# Create graph
unrdf graph create --name people

# Load Turtle data
unrdf graph load --graph people --file people.ttl

# Query with SPARQL
unrdf graph query --graph people --query "SELECT ?name WHERE { ?p foaf:name ?name }"

# Transform with CONSTRUCT
unrdf graph query --graph people --query "CONSTRUCT { ?s schema:name ?n } WHERE { ?s foaf:name ?n }" > transformed.ttl

# Export as JSON-LD
unrdf graph export --graph people --format jsonld > people.jsonld
```

## Code Generation (sync)

The `sync` command transforms RDF ontologies into typed code artifacts using SPARQL queries and Nunjucks templates. This enables ontology-driven development where your RDF schema becomes the single source of truth for generated code.

**Common use cases:**
- Generate Zod validation schemas from RDF class definitions
- Create OpenAPI specifications from API ontologies
- Produce JSDoc type definitions from RDFS/OWL vocabularies
- Build GraphQL schemas from RDF domain models

### Quick Example

**1. Create `ggen.toml` configuration:**

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
SELECT ?entityName ?propertyName ?propertyType ?required
WHERE {
  ?entity a rdfs:Class ; rdfs:label ?entityName .
  ?property rdfs:domain ?entity ; rdfs:label ?propertyName ; rdfs:range ?propertyType .
  OPTIONAL { ?property api:required ?required }
}
ORDER BY ?entityName ?propertyName
"""
```

**2. Run sync command:**

```bash
# Generate code from ontology
unrdf sync

# Preview without writing files
unrdf sync --dry-run --verbose

# Run specific generation rule
unrdf sync --rule zod-schemas

# Watch mode for development
unrdf sync --watch
```

The command reads your RDF ontology, executes the SPARQL query against it, and renders the results through your Nunjucks template to generate code. Templates support filters for case conversion (`camelCase`, `pascalCase`), RDF utilities (`localName`, `namespace`), and type mapping (`zodType`, `jsdocType`).

**Full documentation:** See [docs/sync-command.md](./docs/sync-command.md) for complete configuration reference, template syntax, available filters, and end-to-end examples.

## Available Commands

### Graph Operations

- `unrdf graph create --name <name>` - Create new named graph
- `unrdf graph load --graph <name> --file <path>` - Load RDF data into graph
- `unrdf graph query --graph <name> --query <sparql>` - Execute SPARQL query
- `unrdf graph export --graph <name> --format <format>` - Export graph data

**Supported formats:** `turtle`, `ntriples`, `nquads`, `jsonld`, `rdfxml`, `trig`

### Context Management

- `unrdf context create --name <name>` - Create RDF context
- `unrdf context list` - List all contexts
- `unrdf context delete --name <name>` - Delete context

### Hook Evaluation

- `unrdf hook eval --policy <path>` - Evaluate hook policy
- `unrdf hook validate --policy <path>` - Validate hook configuration

### Code Generation

- `unrdf sync [--config <path>]` - Generate code from RDF ontology (see above)

## Advanced Topics

### Automation Scripts

Integrate CLI commands into Node.js automation workflows:

```javascript
// automation-script.mjs
import { exec } from 'child_process';
import { promisify } from 'util';

const execAsync = promisify(exec);

async function pipeline() {
  await execAsync('unrdf graph create --name daily-import');
  await execAsync('unrdf graph load --graph daily-import --file daily.ttl');

  const { stdout } = await execAsync(
    'unrdf graph query --graph daily-import --query "SELECT * WHERE { ?s ?p ?o }"'
  );

  console.log('Query results:', stdout);
}

pipeline();
```

### CI/CD Integration

```yaml
# .github/workflows/ontology-sync.yml
name: Ontology Sync

on:
  push:
    paths:
      - 'ontology/**'
      - 'ggen.toml'

jobs:
  sync:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: pnpm/action-setup@v2
      - run: pnpm add -g @unrdf/cli
      - run: unrdf sync --verbose
      - run: git add lib/
      - run: git commit -m "chore: sync generated code from ontology"
      - run: git push
```

### Environment Configuration

Set default options via environment variables:

```bash
export UNRDF_DEFAULT_GRAPH=main
export UNRDF_DEFAULT_FORMAT=turtle
export UNRDF_CONFIG_PATH=./config/ggen.toml

unrdf graph query --query "SELECT * WHERE { ?s ?p ?o }"
# Uses UNRDF_DEFAULT_GRAPH automatically
```

## Examples

See [examples/](./examples/) directory for complete examples:

- `01-minimal-parse-query.mjs` - Basic RDF parsing and querying
- `basic-knowledge-hook.mjs` - Hook policy evaluation
- Additional sync examples in [docs/sync-command.md](./docs/sync-command.md)

## Documentation

- **Quick Start**: [QUICKSTART-CLI.md](./QUICKSTART-CLI.md)
- **Sync Command**: [docs/sync-command.md](./docs/sync-command.md)
- **Daemon CLI**: [docs/daemon-cli.md](./docs/daemon-cli.md)
- **Core Package**: [@unrdf/core](../core/README.md)

## Support

- **Issues**: [GitHub Issues](https://github.com/unrdf/unrdf/issues)
- **Discussions**: [GitHub Discussions](https://github.com/unrdf/unrdf/discussions)
- **Examples**: [examples/](./examples/)

## License

MIT
