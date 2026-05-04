# UNRDF CLI Reference

## Installation

```bash
npm install -g @unrdf/cli
# or use via pnpm dlx
pnpm dlx @unrdf/cli --help
```

## Commands

### `unrdf query`

Execute a SPARQL query against an RDF file or endpoint.

```bash
unrdf query --file data.ttl --sparql "SELECT * WHERE { ?s ?p ?o } LIMIT 10"
```

### `unrdf validate`

Validate RDF data against SHACL shapes.

```bash
unrdf validate --data data.ttl --shapes shapes.ttl
```

### `unrdf convert`

Convert between RDF serialization formats.

```bash
unrdf convert --input data.ttl --output data.nt --format application/n-triples
```

### `unrdf mcp:list`

List available MCP tools provided by the daemon.

```bash
pnpm --filter @unrdf/daemon start
unrdf mcp:list
```

## Options

| Flag | Description |
|------|-------------|
| `--help` | Show help |
| `--version` | Show version |
| `--verbose` | Enable verbose output |
| `--format` | Output format (json, text, ttl) |

## See Also

- [MCP_INTEGRATION.md](MCP_INTEGRATION.md) — Model Context Protocol guide
- [GETTING_STARTED.md](GETTING_STARTED.md) — Tutorials and quick start
