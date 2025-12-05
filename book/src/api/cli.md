# CLI API Reference

UNRDF provides a comprehensive command-line interface for RDF graph management, knowledge hooks, policy packs, and SPARQL operations.

## Installation & Setup

```bash
# Install UNRDF
npm install unrdf

# Verify installation
npx unrdf --version

# Generate shell completion
npx unrdf completion bash > ~/.unrdf-completion.bash
source ~/.unrdf-completion.bash
```

---

## Global Options

All commands support these global options:

```bash
--fast           # Fast mode - skip heavy initialization
--help, -h       # Show help
--version, -v    # Show version
```

---

## Graph Commands

Manage RDF graphs with CRUD operations.

### `unrdf graph update [OPTIONS]`

Update a graph with new data.

**Options:**
- `--graph, -g <uri>` - Graph URI (required)
- `--file, -f <path>` - Input file path
- `--format <format>` - Input format (turtle, nquads, jsonld)
- `--merge` - Merge with existing graph (default: replace)

**Examples:**
```bash
# Update graph from Turtle file
unrdf graph update -g http://example.org/graph1 -f data.ttl

# Merge N-Quads data
unrdf graph update -g ex:graph2 -f data.nq --format nquads --merge
```

---

### `unrdf graph delete [OPTIONS]`

Delete a graph or specific triples.

**Options:**
- `--graph, -g <uri>` - Graph URI (required)
- `--pattern <pattern>` - Triple pattern to delete (optional)

**Examples:**
```bash
# Delete entire graph
unrdf graph delete -g http://example.org/graph1

# Delete triples matching pattern
unrdf graph delete -g ex:graph1 --pattern "?s foaf:knows ?o"
```

---

### `unrdf graph describe [OPTIONS]`

Describe a graph or resource.

**Options:**
- `--graph, -g <uri>` - Graph URI
- `--resource, -r <uri>` - Resource URI to describe
- `--format <format>` - Output format (turtle, nquads, json)

**Examples:**
```bash
# Describe a graph
unrdf graph describe -g http://example.org/graph1

# Describe a specific resource
unrdf graph describe -r http://example.org/alice --format turtle
```

---

## Hook Commands

Manage knowledge hooks for graph governance.

### `unrdf hook list [OPTIONS]`

List all registered knowledge hooks.

**Options:**
- `--format <format>` - Output format (table, json, yaml)
- `--filter <pattern>` - Filter by hook name pattern

**Examples:**
```bash
# List all hooks
unrdf hook list

# List hooks as JSON
unrdf hook list --format json

# Filter hooks
unrdf hook list --filter "validate-*"
```

**Output:**
```
📋 Knowledge Hooks
┌────────────────────────┬─────────┬──────────────┐
│ Name                   │ Version │ Status       │
├────────────────────────┼─────────┼──────────────┤
│ validate-schema        │ 1.0.0   │ ✅ Active    │
│ check-permissions      │ 1.2.0   │ ✅ Active    │
│ audit-changes          │ 2.0.0   │ ⏸ Disabled  │
└────────────────────────┴─────────┴──────────────┘
```

---

### `unrdf hook get <NAME>`

Get details about a specific hook.

**Arguments:**
- `<NAME>` - Hook name (required)

**Options:**
- `--format <format>` - Output format (yaml, json)

**Examples:**
```bash
# Get hook details
unrdf hook get validate-schema

# Get hook as JSON
unrdf hook get validate-schema --format json
```

**Output:**
```yaml
name: validate-schema
version: 1.0.0
description: Validate RDF data against SHACL shapes
status: active
when:
  sparql-ask: |
    ASK { ?s ?p ?o }
priority: 80
last_executed: 2024-01-15T10:30:00Z
execution_count: 142
```

---

### `unrdf hook create [OPTIONS]`

Create a new knowledge hook.

**Options:**
- `--name, -n <name>` - Hook name (required)
- `--file, -f <path>` - Hook implementation file
- `--template <template>` - Use template (validation, audit, transform)
- `--interactive, -i` - Interactive mode

**Examples:**
```bash
# Create hook from file
unrdf hook create -n my-validator -f validator.mjs

# Create from template
unrdf hook create -n schema-check --template validation

# Interactive mode
unrdf hook create --interactive
```

---

### `unrdf hook update <NAME> [OPTIONS]`

Update an existing hook.

**Arguments:**
- `<NAME>` - Hook name (required)

**Options:**
- `--file, -f <path>` - New implementation file
- `--enable` - Enable hook
- `--disable` - Disable hook
- `--priority <n>` - Set priority (0-100)

**Examples:**
```bash
# Update hook implementation
unrdf hook update validate-schema -f new-validator.mjs

# Change priority
unrdf hook update validate-schema --priority 90

# Disable hook
unrdf hook update validate-schema --disable
```

---

### `unrdf hook delete <NAME>`

Delete a knowledge hook.

**Arguments:**
- `<NAME>` - Hook name (required)

**Options:**
- `--force, -f` - Skip confirmation

**Examples:**
```bash
# Delete hook (with confirmation)
unrdf hook delete old-validator

# Force delete
unrdf hook delete old-validator --force
```

---

### `unrdf hook history <NAME> [OPTIONS]`

Show hook execution history.

**Arguments:**
- `<NAME>` - Hook name (required)

**Options:**
- `--limit <n>` - Limit results (default: 50)
- `--format <format>` - Output format (table, json)

**Examples:**
```bash
# Show recent executions
unrdf hook history validate-schema

# Show last 10 executions as JSON
unrdf hook history validate-schema --limit 10 --format json
```

**Output:**
```
📊 Execution History: validate-schema
┌─────────────────────┬─────────┬──────────┬──────────┐
│ Timestamp           │ Result  │ Duration │ Actor    │
├─────────────────────┼─────────┼──────────┼──────────┤
│ 2024-01-15 10:30:00 │ ✅ Pass │ 42ms     │ system   │
│ 2024-01-15 10:25:00 │ ✅ Pass │ 38ms     │ user:123 │
│ 2024-01-15 10:20:00 │ ❌ Fail │ 15ms     │ system   │
└─────────────────────┴─────────┴──────────┴──────────┘
```

---

### `unrdf hook describe <NAME>`

Show detailed hook information.

**Arguments:**
- `<NAME>` - Hook name (required)

**Examples:**
```bash
unrdf hook describe validate-schema
```

---

## Policy Commands

Manage policy packs for versioned governance.

### `unrdf policy list [OPTIONS]`

List all policy packs.

**Options:**
- `--active` - Show only active packs
- `--format <format>` - Output format (table, json)

**Examples:**
```bash
# List all policy packs
unrdf policy list

# List active packs only
unrdf policy list --active
```

**Output:**
```
📦 Policy Packs
┌──────────────────┬─────────┬─────────┬────────┐
│ Name             │ Version │ Status  │ Hooks  │
├──────────────────┼─────────┼─────────┼────────┤
│ governance:core  │ 1.0.0   │ ✅ Active │ 5      │
│ security:basic   │ 2.1.0   │ ✅ Active │ 3      │
│ compliance:gdpr  │ 1.5.0   │ ⏸ Inactive │ 8      │
└──────────────────┴─────────┴─────────┴────────┘
```

---

### `unrdf policy get <NAME>`

Get policy pack details.

**Arguments:**
- `<NAME>` - Policy pack name (required)

**Examples:**
```bash
unrdf policy get governance:core
```

---

### `unrdf policy apply <NAME>`

Activate a policy pack.

**Arguments:**
- `<NAME>` - Policy pack name (required)

**Options:**
- `--force, -f` - Force activation even if incompatible

**Examples:**
```bash
# Activate policy pack
unrdf policy apply governance:core

# Force activation
unrdf policy apply governance:core --force
```

---

### `unrdf policy test <NAME> [OPTIONS]`

Test a policy pack against sample data.

**Arguments:**
- `<NAME>` - Policy pack name (required)

**Options:**
- `--data, -d <path>` - Test data file
- `--verbose, -v` - Verbose output

**Examples:**
```bash
# Test policy pack
unrdf policy test governance:core -d test-data.ttl

# Verbose testing
unrdf policy test governance:core -d test-data.ttl --verbose
```

---

### `unrdf policy validate <NAME>`

Validate a policy pack manifest and hooks.

**Arguments:**
- `<NAME>` - Policy pack name or path to manifest (required)

**Examples:**
```bash
# Validate installed pack
unrdf policy validate governance:core

# Validate manifest file
unrdf policy validate ./policy-packs/my-pack/manifest.json
```

**Output:**
```
✅ Policy Pack Validation: governance:core

Manifest:
  ✅ Valid JSON structure
  ✅ All required fields present
  ✅ Version format valid (1.0.0)

Hooks:
  ✅ validate-schema.mjs - valid
  ✅ check-permissions.mjs - valid
  ⚠️  audit-changes.mjs - missing dependency

Resources:
  ✅ All referenced files exist

Overall: ⚠️ Valid with warnings
```

---

### `unrdf policy describe <NAME>`

Show detailed policy pack information.

**Arguments:**
- `<NAME>` - Policy pack name (required)

**Examples:**
```bash
unrdf policy describe governance:core
```

---

## Store Commands

Manage the RDF store directly.

### `unrdf store import <FILE> [OPTIONS]`

Import data into the store.

**Arguments:**
- `<FILE>` - Input file path (required)

**Options:**
- `--format <format>` - Input format (turtle, nquads, jsonld)
- `--graph <uri>` - Target graph URI
- `--clear` - Clear existing data first

**Examples:**
```bash
# Import Turtle data
unrdf store import data.ttl

# Import into specific graph
unrdf store import data.nq --format nquads --graph ex:graph1

# Clear and import
unrdf store import data.ttl --clear
```

---

### `unrdf store export [OPTIONS]`

Export data from the store.

**Options:**
- `--output, -o <file>` - Output file path (default: stdout)
- `--format <format>` - Output format (turtle, nquads, jsonld)
- `--graph <uri>` - Source graph URI
- `--pretty` - Pretty-print output

**Examples:**
```bash
# Export to stdout
unrdf store export --format turtle

# Export to file
unrdf store export -o export.ttl --format turtle --pretty

# Export specific graph
unrdf store export -o graph1.nq --graph ex:graph1 --format nquads
```

---

### `unrdf store query <SPARQL> [OPTIONS]`

Execute a SPARQL query against the store.

**Arguments:**
- `<SPARQL>` - SPARQL query string or file path (required)

**Options:**
- `--file, -f` - Treat argument as file path
- `--format <format>` - Output format (table, json, csv, xml)
- `--limit <n>` - Result limit

**Examples:**
```bash
# Query from command line
unrdf store query "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10"

# Query from file
unrdf store query query.sparql --file

# JSON output
unrdf store query query.sparql -f --format json

# CSV output with limit
unrdf store query "SELECT * WHERE { ?s ?p ?o }" --format csv --limit 100
```

**Output (table format):**
```
┌─────────────────────────────┬──────────────────────┬─────────┐
│ s                           │ p                    │ o       │
├─────────────────────────────┼──────────────────────┼─────────┤
│ http://example.org/alice    │ foaf:name            │ "Alice" │
│ http://example.org/alice    │ foaf:knows           │ ex:bob  │
│ http://example.org/bob      │ foaf:name            │ "Bob"   │
└─────────────────────────────┴──────────────────────┴─────────┘
```

---

### `unrdf store stats [OPTIONS]`

Show store statistics.

**Options:**
- `--format <format>` - Output format (table, json)
- `--verbose, -v` - Include detailed statistics

**Examples:**
```bash
# Basic stats
unrdf store stats

# Detailed stats
unrdf store stats --verbose

# JSON output
unrdf store stats --format json
```

**Output:**
```
📊 Store Statistics

Overview:
  Total Quads:      12,345
  Unique Subjects:  1,234
  Unique Predicates: 45
  Unique Objects:   3,456
  Named Graphs:     8

Performance:
  Last Query:       42ms
  Avg Query Time:   35ms
  Cache Hit Rate:   85%

Disk Usage:
  Store Size:       2.4 MB
  Index Size:       0.8 MB
  Total:            3.2 MB
```

---

## Context Commands

Manage CLI contexts for different environments.

### `unrdf context list`

List all contexts.

**Examples:**
```bash
unrdf context list
```

**Output:**
```
📋 CLI Contexts
┌─────────────┬────────────────────────┬────────┐
│ Name        │ Endpoint               │ Active │
├─────────────┼────────────────────────┼────────┤
│ development │ http://localhost:3030  │ ✅     │
│ staging     │ https://stage.api.com  │        │
│ production  │ https://prod.api.com   │        │
└─────────────┴────────────────────────┴────────┘
```

---

### `unrdf context create <NAME> [OPTIONS]`

Create a new context.

**Arguments:**
- `<NAME>` - Context name (required)

**Options:**
- `--endpoint, -e <url>` - SPARQL endpoint URL
- `--auth <token>` - Authentication token

**Examples:**
```bash
unrdf context create staging --endpoint https://stage.api.com
```

---

### `unrdf context use <NAME>`

Switch to a different context.

**Arguments:**
- `<NAME>` - Context name (required)

**Examples:**
```bash
unrdf context use production
```

---

### `unrdf context current`

Show current context.

**Examples:**
```bash
unrdf context current
```

---

## Plugin Commands

Manage CLI plugins.

### `unrdf plugin list`

List installed plugins.

**Examples:**
```bash
unrdf plugin list
```

---

### `unrdf plugin install <NAME>`

Install a plugin.

**Arguments:**
- `<NAME>` - Plugin name (required)

**Examples:**
```bash
unrdf plugin install unrdf-plugin-visualizer
```

---

## REPL

### `unrdf repl [OPTIONS]`

Start interactive SPARQL REPL.

**Options:**
- `--endpoint, -e <url>` - SPARQL endpoint
- `--timeout <ms>` - Query timeout (default: 30000)

**Examples:**
```bash
# Start REPL
unrdf repl

# Connect to endpoint
unrdf repl -e http://localhost:3030/dataset
```

**REPL Commands:**
```
unrdf> SELECT * WHERE { ?s ?p ?o } LIMIT 5
┌─────────────────────────────┬──────────────┬─────────┐
│ s                           │ p            │ o       │
├─────────────────────────────┼──────────────┼─────────┤
│ http://example.org/alice    │ foaf:name    │ "Alice" │
└─────────────────────────────┴──────────────┴─────────┘

unrdf> .help
Available commands:
  .help          Show this help
  .quit          Exit REPL
  .clear         Clear screen
  .prefixes      Show loaded prefixes
  .load <file>   Load and execute SPARQL file
  .export <file> Export last results

unrdf> .quit
Goodbye!
```

---

## Shell Completion

Generate shell completion scripts:

```bash
# Bash
unrdf completion bash > ~/.unrdf-completion.bash
echo "source ~/.unrdf-completion.bash" >> ~/.bashrc

# Zsh
unrdf completion zsh > ~/.unrdf-completion.zsh
echo "source ~/.unrdf-completion.zsh" >> ~/.zshrc

# Fish
unrdf completion fish > ~/.config/fish/completions/unrdf.fish
```

---

## Configuration File

UNRDF reads configuration from `.unrdfrc.json`:

```json
{
  "defaultContext": "development",
  "contexts": {
    "development": {
      "endpoint": "http://localhost:3030",
      "timeout": 30000
    },
    "production": {
      "endpoint": "https://api.example.com",
      "auth": "${UNRDF_AUTH_TOKEN}"
    }
  },
  "plugins": [
    "unrdf-plugin-visualizer"
  ],
  "hooks": {
    "basePath": "./hooks",
    "autoLoad": true
  }
}
```

---

## Environment Variables

```bash
UNRDF_ENDPOINT       # Default SPARQL endpoint
UNRDF_AUTH_TOKEN     # Authentication token
UNRDF_CONFIG         # Config file path
UNRDF_LOG_LEVEL      # Log level (debug, info, warn, error)
UNRDF_TIMEOUT        # Default timeout (ms)
```

---

## Best Practices

### Scripting

Use JSON output for scripting:

```bash
#!/bin/bash

# Get hook status
status=$(unrdf hook get validate-schema --format json | jq -r '.status')

if [ "$status" = "active" ]; then
  echo "Hook is active"
fi
```

### Batch Operations

Use batch mode for multiple operations:

```bash
# Import multiple files
for file in *.ttl; do
  unrdf store import "$file" --graph "ex:${file%.ttl}"
done
```

### Performance

Use `--fast` mode for quick operations:

```bash
# Fast mode skips heavy initialization
unrdf --fast store stats
unrdf --fast hook list
```
