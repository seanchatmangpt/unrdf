# @unrdf/cli

**Command-line Tools for Graph Operations**

CLI for working with RDF graphs. Create, query, update, and manage graphs from the terminal.

## Installation

```bash
pnpm add -g @unrdf/cli
# or use with pnpm
pnpm exec unrdf
```

## Quick Start

```bash
# Create a new graph
unrdf graph create --name my-dataset

# Load data
unrdf graph load --graph my-dataset --file data.ttl

# Query with SPARQL
unrdf graph query --graph my-dataset --query "SELECT ?s WHERE { ?s ?p ?o }"

# Export data
unrdf graph export --graph my-dataset --format jsonld

# Create a context
unrdf context create --name my-context --graph my-dataset

# Evaluate a hook
unrdf hook eval --hook my-hook.mjs --quad <quad-json>
```

## Features

- ✅ Graph CRUD operations (create, read, update, delete)
- ✅ Context management (switch between configurations)
- ✅ SPARQL queries from CLI
- ✅ Multiple export formats (TTL, JSON-LD, N-Triples)
- ✅ Batch operations (import multiple files)
- ✅ Hook evaluation and testing

## Commands

- `unrdf graph create` - Create new graph
- `unrdf graph delete` - Delete graph
- `unrdf graph list` - List graphs
- `unrdf graph load` - Load RDF data
- `unrdf graph export` - Export graph
- `unrdf graph query` - Execute SPARQL
- `unrdf context use` - Switch context
- `unrdf context create` - Create context
- `unrdf hook eval` - Test hooks

## Documentation

- **[API Reference](./docs/API.md)** - Command reference
- **[User Guide](./docs/GUIDE.md)** - Usage guide and examples
- **[Examples](./examples/)** - CLI examples
- **[Contributing](./docs/CONTRIBUTING.md)** - How to contribute

## Depends On

- `@unrdf/core` - RDF substrate
- `@unrdf/hooks` - Hook evaluation
- `@unrdf/federation` - Federation commands
- `@unrdf/streaming` - Stream monitoring

## VOC Usage

- VOC-5: Data Engineer (ETL CLI)
- VOC-7: DevOps Operator (graph management)

## License

MIT
