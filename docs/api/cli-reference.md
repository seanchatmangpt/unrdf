# CLI Reference

Complete command-line interface reference for UNRDF v3.

## Overview

UNRDF CLI provides a kubectl/docker-style noun-verb interface for all RDF operations, knowledge hooks, and policy management.

**Command Structure:**

```bash
unrdf <noun> <verb> [options] [args]
```

## Global Options

Available for all commands:

- `--help, -h`: Show help
- `--version, -v`: Show version
- `--debug`: Enable debug logging
- `--verbose`: Verbose output
- `--output, -o`: Output format (json|yaml|table|tree)
- `--config`: Config file path (default: `~/.unrdf/config.json`)
- `--context, -c`: Context to use

## Commands

### graph - Graph Operations

Manage RDF graphs and datasets.

#### graph validate

Validate RDF graph against SHACL shapes.

```bash
unrdf graph validate <input> --shapes <shapes-file>
```

**Arguments:**

- `input`: Input file path

**Options:**

- `--shapes <file>`: SHACL shapes file (required)
- `--strict`: Throw on validation failure
- `--output, -o`: Output format (json|yaml|table)

**Example:**

```bash
unrdf graph validate data.ttl --shapes shapes.ttl
```

**Output:**

```
✓ Validation passed
  Conforms: true
  Results: 0 violations
```

#### graph export

Export RDF graph to different formats.

```bash
unrdf graph export <input> --format <format>
```

**Options:**

- `--format, -f`: Output format (turtle|nquads|jsonld|rdfxml)
- `--output, -o`: Output file (default: stdout)
- `--prefixes`: Prefix file for compact notation

**Example:**

```bash
unrdf graph export data.ttl --format jsonld > data.jsonld
```

#### graph import

Import RDF data from various formats.

```bash
unrdf graph import <input> [options]
```

**Options:**

- `--format, -f`: Input format (auto-detected if not specified)
- `--base`: Base IRI for relative IRIs
- `--graph`: Target named graph IRI

**Example:**

```bash
unrdf graph import data.jsonld --base http://example.org/
```

#### graph stats

Show statistics about an RDF graph.

```bash
unrdf graph stats <input>
```

**Example:**

```bash
unrdf graph stats data.ttl
```

**Output:**

```
Graph Statistics:
  Total quads: 15,234
  Subjects: 1,456
  Predicates: 89
  Objects: 12,345
  Blank nodes: 234
  Named graphs: 3
```

### hook - Knowledge Hook Management

Manage and execute knowledge hooks.

#### hook list

List all registered knowledge hooks.

```bash
unrdf hook list [options]
```

**Options:**

- `--active`: Show only active hooks
- `--pack <name>`: Filter by policy pack

**Example:**

```bash
unrdf hook list --output table
```

**Output:**

```
┌─────────────────────────┬──────────┬─────────────────────────┬────────┐
│ Name                    │ Type     │ Description             │ Status │
├─────────────────────────┼──────────┼─────────────────────────┼────────┤
│ compliance:large-tx     │ ASK      │ Alert on large trans... │ active │
│ quality:completeness    │ SHACL    │ Check data complete...  │ active │
│ analytics:revenue       │ SELECT   │ Track revenue changes   │ active │
└─────────────────────────┴──────────┴─────────────────────────┴────────┘
```

#### hook eval

Evaluate a knowledge hook.

```bash
unrdf hook eval <hook-file> [options]
```

**Arguments:**

- `hook-file`: Hook definition file (.mjs)

**Options:**

- `--data <file>`: RDF data file
- `--payload <json>`: Custom payload (JSON)
- `--dry-run`: Evaluate without side effects

**Example:**

```bash
unrdf hook eval hooks/health-check.mjs --data data.ttl
```

**Output:**

```json
{
  "success": true,
  "result": {
    "status": "healthy",
    "timestamp": 1696284567890
  },
  "executionTime": 45
}
```

#### hook test

Test a knowledge hook with sample data.

```bash
unrdf hook test <hook-file> [options]
```

**Options:**

- `--data <file>`: Test data file
- `--cases <file>`: Test cases file (JSON)
- `--coverage`: Show coverage metrics

**Example:**

```bash
unrdf hook test hooks/compliance.mjs --cases test-cases.json
```

#### hook describe

Show detailed information about a hook.

```bash
unrdf hook describe <hook-name>
```

**Example:**

```bash
unrdf hook describe compliance:large-tx
```

### policy - Policy Pack Management

Manage policy packs and governance rules.

#### policy list

List all policy packs.

```bash
unrdf policy list [options]
```

**Options:**

- `--active`: Show only active packs
- `--format <format>`: Output format

**Example:**

```bash
unrdf policy list --active
```

#### policy apply

Apply a policy pack.

```bash
unrdf policy apply <pack-file> [options]
```

**Arguments:**

- `pack-file`: Policy pack manifest (.json)

**Options:**

- `--dry-run`: Validate without applying
- `--force`: Override existing pack

**Example:**

```bash
unrdf policy apply compliance-v1.json
```

#### policy rollback

Rollback to a previous policy pack version.

```bash
unrdf policy rollback <pack-name> [options]
```

**Options:**

- `--version <version>`: Target version
- `--confirm`: Skip confirmation prompt

**Example:**

```bash
unrdf policy rollback compliance --version 1.0.0
```

#### policy deactivate

Deactivate a policy pack.

```bash
unrdf policy deactivate <pack-name>
```

### query - SPARQL Queries

Execute SPARQL queries.

#### query select

Execute a SPARQL SELECT query.

```bash
unrdf query select <query> [options]
```

**Arguments:**

- `query`: SPARQL query string or file path

**Options:**

- `--data <file>`: RDF data file
- `--limit <n>`: Result limit
- `--format <format>`: Output format

**Example:**

```bash
unrdf query select "SELECT * WHERE { ?s ?p ?o } LIMIT 10" --data data.ttl
```

#### query ask

Execute a SPARQL ASK query.

```bash
unrdf query ask <query> [options]
```

**Example:**

```bash
unrdf query ask "ASK { ?s a <http://example.org/Person> }" --data data.ttl
```

**Output:**

```
✓ true
```

#### query construct

Execute a SPARQL CONSTRUCT query.

```bash
unrdf query construct <query> [options]
```

**Options:**

- `--output, -o`: Output file
- `--format, -f`: Output format (default: turtle)

**Example:**

```bash
unrdf query construct "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }" --data data.ttl
```

### sidecar - Sidecar Management

Control and monitor the KGC sidecar.

#### sidecar status

Check sidecar health and status.

```bash
unrdf sidecar status
```

**Output:**

```json
{
  "status": "healthy",
  "uptime": 3600,
  "version": "3.0.0",
  "connections": {
    "active": 12,
    "total": 1234
  },
  "performance": {
    "p50": 15,
    "p95": 45,
    "p99": 180
  }
}
```

#### sidecar logs

Stream sidecar logs.

```bash
unrdf sidecar logs [options]
```

**Options:**

- `--follow, -f`: Follow log output
- `--tail <n>`: Number of lines to show
- `--level <level>`: Log level filter (debug|info|warn|error)

**Example:**

```bash
unrdf sidecar logs --follow --level error
```

#### sidecar config get

Get sidecar configuration.

```bash
unrdf sidecar config get [key]
```

**Example:**

```bash
unrdf sidecar config get endpoint.address
```

**Output:**

```
localhost
```

#### sidecar config set

Update sidecar configuration.

```bash
unrdf sidecar config set <key> <value>
```

**Example:**

```bash
unrdf sidecar config set endpoint.port 50051
```

#### sidecar restart

Restart the sidecar process.

```bash
unrdf sidecar restart [options]
```

**Options:**

- `--graceful`: Graceful shutdown
- `--timeout <ms>`: Shutdown timeout

### store - Store Operations

Direct RDF store manipulation.

#### store create

Create a new RDF store.

```bash
unrdf store create <name> [options]
```

**Options:**

- `--base-iri <iri>`: Base IRI for the store
- `--prefixes <file>`: Prefix definitions

**Example:**

```bash
unrdf store create mystore --base-iri http://example.org/
```

#### store load

Load data into a store.

```bash
unrdf store load <name> <file> [options]
```

**Example:**

```bash
unrdf store load mystore data.ttl
```

#### store export

Export store contents.

```bash
unrdf store export <name> [options]
```

**Options:**

- `--format, -f`: Output format
- `--output, -o`: Output file

### context - Context Management

Manage multi-environment contexts (similar to kubeconfig).

#### context list

List all contexts.

```bash
unrdf context list
```

**Output:**

```
CURRENT   NAME        SIDECAR                    STATUS
*         production  kgc.example.com:443       healthy
          staging     kgc-staging.example.com   healthy
          dev         localhost:50051           unhealthy
```

#### context create

Create a new context.

```bash
unrdf context create <name> [options]
```

**Options:**

- `--sidecar <address>`: Sidecar address
- `--base-iri <iri>`: Base IRI
- `--tls`: Enable TLS
- `--ca <file>`: CA certificate

**Example:**

```bash
unrdf context create prod \
  --sidecar kgc.example.com:443 \
  --base-iri http://example.org/ \
  --tls
```

#### context use

Switch to a different context.

```bash
unrdf context use <name>
```

**Example:**

```bash
unrdf context use production
```

#### context delete

Delete a context.

```bash
unrdf context delete <name>
```

#### context current

Show current context.

```bash
unrdf context current
```

**Output:**

```
production
```

## Configuration File

UNRDF uses a configuration file at `~/.unrdf/config.json`:

```json
{
  "currentContext": "production",
  "contexts": [
    {
      "name": "production",
      "endpoint": {
        "address": "kgc.example.com",
        "port": 443,
        "tls": {
          "enabled": true,
          "ca": "/path/to/ca.crt"
        }
      },
      "timeout": 30000,
      "maxRetries": 3
    },
    {
      "name": "dev",
      "endpoint": {
        "address": "localhost",
        "port": 50051,
        "tls": { "enabled": false }
      }
    }
  ],
  "baseIRI": "http://example.org/",
  "prefixes": {
    "ex": "http://example.org/",
    "foaf": "http://xmlns.com/foaf/0.1/"
  }
}
```

## Environment Variables

- `UNRDF_BASE_IRI`: Default base IRI
- `UNRDF_PREFIXES`: JSON-encoded prefix mappings
- `KGC_SIDECAR_ADDRESS`: Sidecar address (e.g., `localhost:50051`)
- `UNRDF_CONFIG`: Config file path
- `DEBUG`: Debug logging (e.g., `unrdf:*`)

## Exit Codes

- `0`: Success
- `1`: General error
- `2`: Validation error
- `3`: Connection error
- `4`: Authentication error
- `5`: Permission error

## Shell Completion

### Bash

```bash
unrdf completion bash > /etc/bash_completion.d/unrdf
```

### Zsh

```bash
unrdf completion zsh > /usr/local/share/zsh/site-functions/_unrdf
```

### Fish

```bash
unrdf completion fish > ~/.config/fish/completions/unrdf.fish
```

## See Also

- [Knowledge Hooks API](/docs/api/knowledge-hooks.md)
- [Composables API](/docs/api/composables.md)
- [Sidecar API](/docs/api/sidecar.md)
- [Quickstart Guide](/docs/quickstart.md)
