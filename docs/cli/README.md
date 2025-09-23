# UNRDF CLI

The UNRDF CLI is a comprehensive command-line interface that provides access to all the functionality of the UNRDF framework. It follows the same opinionated and composable philosophy as the core framework, providing a clean and consistent interface for RDF operations.

## Installation

Install the UNRDF package globally to use the CLI:

```bash
pnpm install -g unrdf
```

Or use it directly from a project that has UNRDF as a dependency:

```bash
npx unrdf --help
```

## Quick Start

```bash
# Initialize a new UNRDF project
unrdf init my-rdf-project

# Parse RDF data
unrdf parse data.ttl --stats

# Query with SPARQL
unrdf query data.ttl --query "SELECT * WHERE { ?s ?p ?o } LIMIT 10"

# Validate against SHACL shapes
unrdf validate data.ttl shapes.ttl

# Convert between formats
unrdf convert data.ttl --to json-ld
```

## Core Commands

### `parse` - Parse RDF Data

Parse RDF data from various formats into the canonical N3 store representation.

```bash
unrdf parse <input> [options]
```

**Arguments:**
- `input` - Input file path or RDF data string

**Options:**
- `--format <format>` - Input format (turtle, n3, json-ld, nquads) [default: turtle]
- `-o, --output <file>` - Output file path (optional)
- `--stats` - Show detailed parsing statistics

**Examples:**
```bash
# Parse a Turtle file with basic output
unrdf parse data.ttl

# Parse with detailed statistics
unrdf parse data.ttl --stats

# Parse and save to file
unrdf parse data.ttl --output parsed.ttl

# Parse JSON-LD format
unrdf parse data.jsonld --format json-ld
```

### `query` - SPARQL Queries

Execute SPARQL queries against RDF graphs using Comunica.

```bash
unrdf query <input> [options]
```

**Arguments:**
- `input` - Input RDF file path

**Options:**
- `-q, --query <query>` - SPARQL query string
- `-f, --query-file <file>` - SPARQL query file path
- `--format <format>` - Output format (json, table, turtle, csv) [default: table]
- `-l, --limit <number>` - Limit number of results

**Examples:**
```bash
# Simple SELECT query
unrdf query data.ttl --query "SELECT ?name WHERE { ?person foaf:name ?name }"

# Query from file with JSON output
unrdf query data.ttl --query-file queries/people.sparql --format json

# Limit results
unrdf query data.ttl --query "SELECT * WHERE { ?s ?p ?o }" --limit 100

# CSV output for spreadsheet import
unrdf query data.ttl --query-file analytics.sparql --format csv
```

### `validate` - SHACL Validation

Validate RDF data against SHACL shapes for data quality assurance.

```bash
unrdf validate <data> --shape <shape> [options]
```

**Arguments:**
- `data` - RDF data file path
- `shape` - SHACL shape file path (required)

**Options:**
- `-o, --output <file>` - Output file for validation report
- `--format <format>` - Report format (json, turtle, table) [default: table]

**Examples:**
```bash
# Basic validation
unrdf validate data.ttl --shape person-shape.ttl

# Save validation report
unrdf validate data.ttl --shape shapes.ttl --output report.json

# Different report formats
unrdf validate data.ttl --shape shapes.ttl --format json
```

### `reason` - OWL Reasoning

Perform OWL reasoning operations using EYE reasoner.

```bash
unrdf reason <input> [options]
```

**Arguments:**
- `input` - Input RDF file path

**Options:**
- `-r, --rules <file>` - N3 rules file path
- `-o, --output <file>` - Output file for reasoned data
- `--builtin <rules>` - Built-in rule set (owl, rdfs) [multiple]

**Examples:**
```bash
# Basic reasoning with OWL rules
unrdf reason ontology.ttl --builtin owl --output inferred.ttl

# Custom reasoning rules
unrdf reason data.ttl --rules family-rules.n3 --output family-inferred.ttl

# Multiple built-in rule sets
unrdf reason data.ttl --builtin owl --builtin rdfs
```

### `convert` - Format Conversion

Convert between RDF formats and structured data using Zod schemas.

```bash
unrdf convert <input> [options]
```

**Arguments:**
- `input` - Input file path

**Options:**
- `--from <format>` - Source format (turtle, json-ld, nquads, json) [default: turtle]
- `--to <format>` - Target format (turtle, json-ld, nquads, json) [default: json]
- `-s, --schema <file>` - Zod schema file path (for JSON conversion)
- `-o, --output <file>` - Output file path
- `--context <file>` - JSON-LD context file path

**Examples:**
```bash
# RDF to RDF conversion
unrdf convert data.ttl --to json-ld

# JSON to RDF with schema validation
unrdf convert people.json --from json --to turtle --schema person-schema.mjs

# Save converted data
unrdf convert data.ttl --to nquads --output data.nq

# Use custom JSON-LD context
unrdf convert data.ttl --to json-ld --context custom-context.json
```

## Utility Commands

### `store` - Store Operations

Interact with the RDF store for analysis and management.

```bash
unrdf store <subcommand> [options]
```

**Subcommands:**
- `stats <input>` - Show store statistics
- `clear` - Clear store contents

**Examples:**
```bash
# Analyze store statistics
unrdf store stats large-dataset.ttl

# Clear store
unrdf store clear
```

### `turtle` - Turtle Operations

Handle Turtle file operations with enhanced functionality.

```bash
unrdf turtle <subcommand> [options]
```

**Subcommands:**
- `parse <input> [-o output]` - Parse Turtle file
- `serialize <input> --output <output> [--format format]` - Serialize to Turtle

**Examples:**
```bash
# Parse Turtle with file system integration
unrdf turtle parse data.ttl --output cleaned.ttl

# Serialize JSON-LD to Turtle
unrdf turtle serialize data.jsonld --output data.ttl --format json-ld
```

### `canon` - Canonicalization

Perform RDF canonicalization operations using URDNA2015.

```bash
unrdf canon <subcommand> [options]
```

**Subcommands:**
- `check <input>` - Check if data is canonicalized
- `hash <input>` - Generate canonical hash

**Examples:**
```bash
# Check canonicalization
unrdf canon check dataset.ttl

# Generate canonical hash for integrity verification
unrdf canon hash dataset.ttl
```

### `prefix` - Namespace Management

Manage and use namespace prefixes.

```bash
unrdf prefix <subcommand> [options]
```

**Subcommands:**
- `list` - List all known prefixes
- `expand <curie>` - Expand a CURIE to full IRI
- `shrink <iri>` - Shrink a full IRI to CURIE

**Examples:**
```bash
# List all prefixes
unrdf prefix list

# Expand CURIE
unrdf prefix expand foaf:Person

# Shrink IRI
unrdf prefix shrink http://xmlns.com/foaf/0.1/Person
```

### `id` - ID Generation

Generate various types of identifiers.

```bash
unrdf id <subcommand> [options]
```

**Subcommands:**
- `uuid [--count n]` - Generate UUID(s)
- `hash <input>` - Generate hash-based ID
- `generate [--prefix prefix]` - Generate generic ID

**Examples:**
```bash
# Generate single UUID
unrdf id uuid

# Generate multiple UUIDs
unrdf id uuid --count 5

# Hash-based ID
unrdf id hash "unique input string"

# Generic ID with prefix
unrdf id generate --prefix resource
```

### `metrics` - RDF Analytics

Analyze RDF datasets for insights and quality metrics.

```bash
unrdf metrics <input> [options]
```

**Arguments:**
- `input` - RDF file to analyze

**Options:**
- `--detailed` - Show detailed metrics

**Examples:**
```bash
# Basic metrics
unrdf metrics dataset.ttl

# Detailed analysis
unrdf metrics large-dataset.ttl --detailed
```

### `delta` - Dataset Comparison

Compare RDF datasets to identify changes.

```bash
unrdf delta <source> <target> [options]
```

**Arguments:**
- `source` - Source RDF file
- `target` - Target RDF file

**Options:**
- `-o, --output <file>` - Output file for delta report

**Examples:**
```bash
# Compare datasets
unrdf delta old-data.ttl new-data.ttl

# Save comparison report
unrdf delta v1.ttl v2.ttl --output changes.json
```

### `cache` - Cache Management

Manage RDF processing cache for performance optimization.

```bash
unrdf cache <subcommand>
```

**Subcommands:**
- `clear` - Clear cache
- `stats` - Show cache statistics

**Examples:**
```bash
# Clear cache
unrdf cache clear

# Show cache statistics
unrdf cache stats
```

## Project Commands

### `init` - Project Initialization

Initialize a new UNRDF project with configuration and sample files.

```bash
unrdf init <name> [options]
```

**Arguments:**
- `name` - Project name

**Options:**
- `--template <template>` - Project template (basic, advanced) [default: basic]

**Examples:**
```bash
# Initialize basic project
unrdf init my-knowledge-graph

# Initialize with advanced template
unrdf init enterprise-kg --template advanced
```

**Generated Files:**
- `unrdf.config.mjs` - Project configuration
- `package.json` - Node.js package configuration
- `sample.ttl` - Sample RDF data

### `help` - Help System

Show detailed help information for commands.

```bash
unrdf help [command]
```

**Examples:**
```bash
# General help
unrdf help

# Command-specific help
unrdf help parse
unrdf help query
```

## Configuration

### Configuration File

Create `unrdf.config.mjs` in your project root:

```javascript
/**
 * UNRDF Configuration
 */
export default {
  baseIRI: 'http://example.org/my-project/',
  prefixes: {
    'ex': 'http://example.org/my-project/',
    'foaf': 'http://xmlns.com/foaf/0.1/',
    'schema': 'https://schema.org/',
    'custom': 'http://my-domain.com/vocab/'
  },
  validation: {
    strict: true,
    validateOnLoad: true
  },
  reasoning: {
    reasoner: 'eye',
    builtinRules: ['owl', 'rdfs']
  }
};
```

### Environment Variables

Configure UNRDF through environment variables:

- `UNRDF_BASE_IRI` - Default base IRI
- `UNRDF_PREFIXES` - JSON string of prefix mappings
- `UNRDF_DEBUG` - Enable debug mode
- `UNRDF_LOG_LEVEL` - Set log level

**Example:**
```bash
export UNRDF_BASE_IRI="http://my-domain.com/"
export UNRDF_PREFIXES='{"my":"http://my-domain.com/vocab/"}'
export UNRDF_DEBUG=true

unrdf parse data.ttl
```

## Common Workflows

### Data Processing Pipeline

```bash
# 1. Parse and validate input data
unrdf parse raw-data.ttl --stats
unrdf validate raw-data.ttl --shape data-shape.ttl

# 2. Apply reasoning
unrdf reason raw-data.ttl --builtin owl --output enriched-data.ttl

# 3. Convert to target format
unrdf convert enriched-data.ttl --to json-ld --output final-data.jsonld

# 4. Analyze results
unrdf metrics final-data.jsonld --detailed
```

### Data Quality Assurance

```bash
# Check data canonicalization
unrdf canon check dataset.ttl

# Validate against multiple shape files
unrdf validate dataset.ttl --shape person-shapes.ttl
unrdf validate dataset.ttl --shape organization-shapes.ttl

# Generate quality metrics
unrdf metrics dataset.ttl --detailed > quality-report.txt
```

### Dataset Comparison

```bash
# Compare two versions
unrdf delta dataset-v1.ttl dataset-v2.ttl --output changes.json

# Analyze the differences
unrdf metrics dataset-v1.ttl > v1-metrics.txt
unrdf metrics dataset-v2.ttl > v2-metrics.txt
```

### Development Workflow

```bash
# Initialize project
unrdf init my-semantic-app

# Set up data processing
unrdf parse source-data.ttl --output processed.ttl
unrdf query processed.ttl --query-file analytics.sparql --format json

# Validate results
unrdf validate processed.ttl --shape validation-shapes.ttl
```

## Error Handling

The CLI provides comprehensive error handling with helpful messages:

- **Parse errors**: Detailed syntax error reporting with line numbers
- **Query errors**: SPARQL syntax validation and execution errors
- **Validation errors**: SHACL constraint violation details
- **File errors**: Clear file not found and permission messages
- **Configuration errors**: Invalid configuration format warnings

## Performance Tips

1. **Use caching**: The CLI automatically caches parsed data for better performance
2. **Limit query results**: Use `--limit` for large datasets
3. **Stream processing**: Large files are processed in streaming mode
4. **Parallel operations**: Multiple CLI commands can run simultaneously
5. **Memory management**: The CLI automatically manages memory for large datasets

## Integration

### Shell Scripts

```bash
#!/bin/bash
# Process multiple RDF files
for file in *.ttl; do
  echo "Processing $file..."
  unrdf validate "$file" --shape shapes.ttl
  unrdf convert "$file" --to json-ld --output "${file%.ttl}.jsonld"
done
```

### Package.json Scripts

```json
{
  "scripts": {
    "validate": "unrdf validate data.ttl --shape shapes.ttl",
    "convert": "unrdf convert data.ttl --to json-ld --output data.jsonld",
    "analyze": "unrdf metrics data.ttl --detailed",
    "process": "npm run validate && npm run convert && npm run analyze"
  }
}
```

### CI/CD Integration

```yaml
# GitHub Actions example
- name: Validate RDF Data
  run: |
    unrdf validate data/*.ttl --shape schemas/shapes.ttl
    unrdf metrics data/combined.ttl --detailed > quality-report.txt
```

## Troubleshooting

### Common Issues

1. **Command not found**: Ensure UNRDF is installed globally or use `npx unrdf`
2. **Parse errors**: Check file encoding and RDF syntax
3. **Memory issues**: Use streaming mode for large files
4. **Permission errors**: Check file permissions and paths
5. **Configuration issues**: Validate `unrdf.config.mjs` syntax

### Debug Mode

Enable debug mode for detailed operation logs:

```bash
UNRDF_DEBUG=true unrdf parse data.ttl
```

### Getting Help

- Use `unrdf help <command>` for command-specific help
- Check the [GitHub issues](https://github.com/gitvan/unrdf/issues) for known problems
- Review the [API documentation](../api-reference.md) for advanced usage

## Examples

See the [examples directory](../examples/) for comprehensive usage examples:

- [Basic Usage](../examples/basic-usage.md)
- [SPARQL Queries](../examples/sparql.md)
- [Validation Patterns](../examples/validation-reasoning.mjs)
- [CLI Scenarios](../../playground/examples/cli-scenarios.mjs)