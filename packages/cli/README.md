# @unrdf/cli

RDF ontology to code generation. Transform your RDF knowledge graphs into typed code artifacts—Zod schemas, OpenAPI specs, JSDoc types, GraphQL schemas—using SPARQL queries and Nunjucks templates.

**Version**: 26.4.9 | **Node.js**: >=18.0.0

## Installation

### Global Install (recommended for CLI usage)

```bash
# Using pnpm (recommended)
pnpm add -g @unrdf/cli

# Using npm
npm install -g @unrdf/cli

# Using yarn
yarn global add @unrdf/cli
```

Verify installation:

```bash
unrdf --version
# Output: 26.4.9
```

### Project-Local Install

```bash
# Install as dev dependency
pnpm add -D @unrdf/cli

# Run via pnpm exec
pnpm exec unrdf sync

# Or add script to package.json
# "scripts": { "sync": "unrdf sync" }
```

### Use Without Installing (npx)

```bash
npx @unrdf/cli sync
```

**Note**: npx downloads the package each time. For frequent use, global install is faster.

## Quick Start

```bash
# Install
pnpm add -g @unrdf/cli

# Create configuration
cat > unrdf.toml << 'EOF'
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
  ?property rdfs:domain ?entity ; rdfs:label ?propertyName ; rdfs:range ?propertyType .
}
ORDER BY ?entityName ?propertyName
"""
EOF

# Create template
mkdir -p templates
cat > templates/entities.njk << 'EOF'
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
EOF

# Generate code
unrdf sync
```

## unrdf sync

Ontology-driven code generation. Reads RDF ontologies, executes SPARQL queries, and renders results through Nunjucks templates to generate typed code artifacts.

### Configuration (unrdf.toml)

```toml
[project]
name = "my-api"
version = "1.0.0"

[ontology]
source = "ontology/schema.ttl"     # RDF file path
format = "turtle"                   # RDF format (auto-detected)
base_iri = "http://example.org/"    # Base IRI for relative URIs

[generation]
output_dir = "lib"                  # Output directory
parallel = false                    # Run rules in parallel

[[generation.rules]]
name = "zod-schemas"                # Rule identifier
template = "templates/zod.njk"      # Nunjucks template
output_file = "schemas.mjs"         # Output file
mode = "overwrite"                  # overwrite | append | skip_existing
query = """
SELECT ?entityName ?propertyName ?propertyType
WHERE {
  ?entity a rdfs:Class ; rdfs:label ?entityName .
  ?property rdfs:domain ?entity ; rdfs:range ?propertyType .
}
"""
```

### Template Features

**Filters:**

- Case conversion: `camelCase`, `pascalCase`, `snakeCase`, `kebabCase`
- RDF utilities: `localName`, `namespace`
- Type mapping: `zodType`, `jsdocType`, `zodDefault`
- Data manipulation: `groupBy`, `sortBy`, `distinctValues`, `requiredArgs`

**Hygen Directives** (line-based modification):

```yaml
---
to: src/index.ts
inject: true
after: '^// Auto-generated exports$'
---
```

Available directives: `inject`, `before`, `after`, `append`, `prepend`, `lineAt`, `skipIf`

### CLI Usage

```bash
# Generate code from config
unrdf sync

# Preview without writing
unrdf sync --dry-run

# Run specific rule
unrdf sync --rule zod-schemas

# Verbose output
unrdf sync --verbose

# Watch mode (regenerate on ontology changes)
unrdf sync --watch
```

### Documentation

- **Complete guide**: https://github.com/unrdf/unrdf/blob/main/packages/cli/docs/sync-command.md
- **Hygen integration**: Line-based file modification, anchor patterns, conditional skips
- **Template reference**: All filters, context variables, SPARQL results structure
- **Examples**: https://github.com/unrdf/unrdf/tree/main/packages/cli/examples/sync

## Other Commands

### MCP Server (AI Agent Integration)

```bash
unrdf mcp start              # Start MCP server (stdio/SSE)
unrdf mcp status             # Check server status
unrdf mcp inspect            # List tools/resources
unrdf mcp stop               # Stop server
unrdf mcp self-play          # Autonomous tool chaining
```

### Diagnostics

```bash
unrdf doctor                  # Health check
unrdf doctor --fix            # Auto-fix issues
unrdf doctor --watch          # Continuous monitoring
```

Checks: env, system, quality, integration, OTEL, Kubernetes

See https://github.com/unrdf/unrdf/blob/main/packages/cli/docs/doctor-command.md

### Graph Operations

```bash
unrdf graph create --name <name>
unrdf graph load --graph <name> --file <path>
unrdf graph query --graph <name> --query <sparql>
unrdf graph export --graph <name> --format <format>
```

Formats: `turtle`, `ntriples`, `nquads`, `jsonld`, `rdfxml`, `trig`

### Template Generation (Ad-hoc)

```bash
unrdf template generate --template <path> [file]
unrdf template list
unrdf template query
```

### Context Management

```bash
unrdf context create --name <name>
unrdf context add --name <name> --prefix <prefix> --namespace <ns>
unrdf context list
unrdf context delete --name <name>
```

### Hook Evaluation

```bash
unrdf hook eval --policy <path>
unrdf hook validate --policy <path>
```

## Advanced Topics

### CI/CD Integration

```yaml
# .github/workflows/ontology-sync.yml
name: Ontology Sync
on:
  push:
    paths: ['ontology/**', 'unrdf.toml']
jobs:
  sync:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - run: pnpm add -g @unrdf/cli
      - run: unrdf sync
      - run: git add lib/
      - run: git commit -m "chore: sync generated code"
```

### Environment Variables

```bash
export UNRDF_DEFAULT_GRAPH=main
export UNRDF_DEFAULT_FORMAT=turtle
export UNRDF_CONFIG_PATH=./unrdf.toml
export UNRDF_FILE_LOCK_RETRY_MS=10
export UNRDF_FILE_LOCK_MAX_RETRIES=50
```

### Automation Scripts

```javascript
// automation-script.mjs
import { exec } from 'child_process';
import { promisify } from 'util';

const execAsync = promisify(exec);

async function pipeline() {
  await execAsync('unrdf sync');
  await execAsync('unrdf doctor --format json > health.json');
}

pipeline();
```

## Documentation

- **Sync Command**: https://github.com/unrdf/unrdf/blob/main/packages/cli/docs/sync-command.md
- **Sync Tutorial**: https://github.com/unrdf/unrdf/blob/main/packages/cli/docs/sync-tutorial.md
- **Template Command**: https://github.com/unrdf/unrdf/blob/main/packages/cli/docs/template-command.md
- **Doctor Command**: https://github.com/unrdf/unrdf/blob/main/packages/cli/docs/doctor-command.md
- **Daemon CLI**: https://github.com/unrdf/unrdf/blob/main/packages/cli/docs/daemon-cli.md
- **Quick Start**: https://github.com/unrdf/unrdf/blob/main/packages/cli/QUICKSTART-CLI.md

## Examples

- https://github.com/unrdf/unrdf/tree/main/packages/cli/examples/sync/basic.unrdf.toml
- https://github.com/unrdf/unrdf/tree/main/packages/cli/examples/sync/advanced.unrdf.toml
- https://github.com/unrdf/unrdf/tree/main/packages/cli/examples/template-pipeline.mjs

## Support

- **Issues**: [GitHub Issues](https://github.com/unrdf/unrdf/issues)
- **Discussions**: [GitHub Discussions](https://github.com/unrdf/unrdf/discussions)

## License

MIT
