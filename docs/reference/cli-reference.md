# CLI Reference

Command-line interface documentation for UNRDF.

## Installation

```bash
# Global installation
pnpm add -g unrdf

# Or use npx
npx unrdf <command>
```

## Synopsis

```
unrdf <command> [options]
```

## Commands

### store

RDF store management commands.

```bash
unrdf store <subcommand> [options]
```

#### store backup

Create a backup of the RDF store.

```bash
unrdf store backup --output <path> [--format <format>]
```

**Options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--output, -o` | `string` | Required | Output file path |
| `--format, -f` | `string` | `turtle` | Output format (turtle, nquads, jsonld) |
| `--compress` | `boolean` | `false` | Compress output with gzip |

**Examples:**

```bash
# Backup to Turtle
unrdf store backup --output backup.ttl

# Backup to N-Quads with compression
unrdf store backup -o backup.nq.gz -f nquads --compress
```

---

#### store restore

Restore the RDF store from a backup.

```bash
unrdf store restore --input <path> [--merge]
```

**Options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--input, -i` | `string` | Required | Input file path |
| `--merge` | `boolean` | `false` | Merge with existing data |
| `--clear` | `boolean` | `false` | Clear store before restore |

**Examples:**

```bash
# Restore from backup
unrdf store restore --input backup.ttl

# Merge backup into existing store
unrdf store restore -i backup.ttl --merge
```

---

#### store import

Import RDF data into the store.

```bash
unrdf store import <file> [--format <format>] [--graph <uri>]
```

**Arguments:**

| Argument | Type | Description |
|----------|------|-------------|
| `file` | `string` | File to import |

**Options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--format, -f` | `string` | Auto-detect | Input format |
| `--graph, -g` | `string` | Default graph | Named graph URI |
| `--validate` | `boolean` | `false` | Validate before import |
| `--shapes` | `string` | None | SHACL shapes file for validation |

**Examples:**

```bash
# Import Turtle file
unrdf store import data.ttl

# Import into named graph
unrdf store import data.ttl --graph http://example.org/graph1

# Import with validation
unrdf store import data.ttl --validate --shapes shapes.ttl
```

---

#### store query

Execute SPARQL queries against the store.

```bash
unrdf store query <sparql> [--format <format>]
```

**Arguments:**

| Argument | Type | Description |
|----------|------|-------------|
| `sparql` | `string` | SPARQL query or file path |

**Options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--format, -f` | `string` | `table` | Output format (table, json, csv) |
| `--file` | `boolean` | `false` | Treat argument as file path |
| `--limit` | `number` | None | Limit results |

**Examples:**

```bash
# Execute inline query
unrdf store query "SELECT * WHERE { ?s ?p ?o } LIMIT 10"

# Execute query from file
unrdf store query queries/find-people.rq --file

# Output as JSON
unrdf store query "SELECT * WHERE { ?s a foaf:Person }" -f json
```

---

#### store validate

Validate store contents against SHACL shapes.

```bash
unrdf store validate --shapes <file> [--format <format>]
```

**Options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--shapes, -s` | `string` | Required | SHACL shapes file |
| `--format, -f` | `string` | `text` | Output format (text, json) |
| `--severity` | `string` | `violation` | Minimum severity to report |

**Examples:**

```bash
# Validate against shapes
unrdf store validate --shapes person-shapes.ttl

# Output validation report as JSON
unrdf store validate -s shapes.ttl -f json

# Include warnings
unrdf store validate -s shapes.ttl --severity warning
```

---

#### store stats

Display store statistics.

```bash
unrdf store stats [--detailed]
```

**Options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--detailed` | `boolean` | `false` | Show detailed statistics |

**Examples:**

```bash
# Basic stats
unrdf store stats

# Detailed stats
unrdf store stats --detailed
```

**Output:**

```
Store Statistics:
  Total triples: 1,234
  Named graphs: 3
  Unique subjects: 456
  Unique predicates: 23
  Unique objects: 789
```

---

## Global Options

These options are available for all commands:

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `--help, -h` | `boolean` | `false` | Show help |
| `--version, -v` | `boolean` | `false` | Show version |
| `--verbose` | `boolean` | `false` | Verbose output |
| `--quiet, -q` | `boolean` | `false` | Suppress non-error output |
| `--config` | `string` | None | Config file path |

## Configuration File

Create `.unrdfrc.json` in your project root:

```json
{
  "store": {
    "path": "./data/store",
    "format": "turtle"
  },
  "validation": {
    "shapesDir": "./shapes"
  },
  "query": {
    "timeout": 30000
  }
}
```

Or use `unrdf.config.mjs`:

```javascript
export default {
  store: {
    path: './data/store',
    format: 'turtle'
  },
  validation: {
    shapesDir: './shapes'
  },
  query: {
    timeout: 30000
  }
};
```

## Environment Variables

| Variable | Description |
|----------|-------------|
| `UNRDF_STORE_PATH` | Default store path |
| `UNRDF_LOG_LEVEL` | Log level (debug, info, warn, error) |
| `UNRDF_CONFIG` | Config file path |

## Exit Codes

| Code | Description |
|------|-------------|
| `0` | Success |
| `1` | General error |
| `2` | Invalid arguments |
| `3` | Validation failed |
| `4` | Query error |
| `5` | I/O error |

## Examples

### Complete Workflow

```bash
# 1. Import data
unrdf store import data/people.ttl

# 2. Validate
unrdf store validate --shapes shapes/person-shape.ttl

# 3. Query
unrdf store query "SELECT ?name WHERE { ?p foaf:name ?name }" -f json

# 4. Backup
unrdf store backup -o backup-$(date +%Y%m%d).ttl
```

### Scripting

```bash
#!/bin/bash
set -e

# Validate before processing
if unrdf store validate -s shapes.ttl --quiet; then
  echo "Validation passed"
  unrdf store query "SELECT * WHERE { ... }" -f csv > output.csv
else
  echo "Validation failed" >&2
  exit 1
fi
```

## Related

- [API Reference](./api-reference.md) - Programmatic API
- [Configuration](./configuration-options.md) - Configuration options
- [Troubleshooting](../guides/troubleshooting.md) - Common issues
