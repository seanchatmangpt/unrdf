# @unrdf/cli - Quick Start Guide

**80/20 Guide**: Command-line RDF operations in 5 minutes.

## Installation

```bash
pnpm add -g @unrdf/cli
# or use with pnpm
pnpm exec unrdf
```

## Quick Commands

### Create Graph

```bash
unrdf graph create --name my-dataset
```

### Load Data

```bash
unrdf graph load --graph my-dataset --file data.ttl
```

### Query with SPARQL

```bash
unrdf graph query --graph my-dataset --query "SELECT ?s WHERE { ?s ?p ?o }"
```

### Export Data

```bash
unrdf graph export --graph my-dataset --format jsonld
```

## Common Workflows

### Workflow 1: Import and Query

```bash
# Create graph
unrdf graph create --name people

# Load Turtle data
unrdf graph load --graph people --file people.ttl

# Query
unrdf graph query --graph people --query "SELECT ?name WHERE { ?p foaf:name ?name }"

# Export as JSON-LD
unrdf graph export --graph people --format jsonld > people.jsonld
```

### Workflow 2: Transform Data

```bash
# Load source data
unrdf graph load --graph source --file input.rdf

# Transform with SPARQL CONSTRUCT
unrdf graph query --graph source --query "CONSTRUCT { ?s schema:name ?n } WHERE { ?s foaf:name ?n }" > transformed.ttl

# Load transformed data
unrdf graph load --graph target --file transformed.ttl
```

### Workflow 3: Automation

```javascript
// automation-script.mjs
import { exec } from 'child_process'
import { promisify } from 'util'

const execAsync = promisify(exec)

async function pipeline() {
  await execAsync('unrdf graph create --name daily-import')
  await execAsync('unrdf graph load --graph daily-import --file daily.ttl')
  await execAsync('unrdf graph query --graph daily-import --query "SELECT * WHERE { ?s ?p ?o }"')
}

pipeline()
```

## Available Commands

- `unrdf graph create` - Create new graph
- `unrdf graph load` - Load RDF data
- `unrdf graph query` - Execute SPARQL
- `unrdf graph export` - Export data
- `unrdf context create` - Create context
- `unrdf hook eval` - Evaluate hook

## Support

- Issues: https://github.com/unrdf/unrdf/issues
- Examples: See [examples/](./examples/)
