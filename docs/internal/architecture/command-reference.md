# UNRDF CLI Command Reference

## Executive Summary

This document provides a comprehensive catalog of all UNRDF CLI commands organized by noun with detailed descriptions, examples, options, and output formats.

## Global Options

All commands support these global options:

| Option | Description | Default |
|--------|-------------|---------|
| `--help`, `-h` | Show help for command | - |
| `--version`, `-v` | Show CLI version | - |
| `--format=<fmt>` | Output format (json\|yaml\|table\|tree) | table |
| `--verbose` | Verbose output | false |
| `--quiet`, `-q` | Suppress output | false |
| `--context=<name>` | Use specific context | current |
| `--config=<path>` | Config file path | ~/.unrdf/config |
| `--no-color` | Disable colors | false |
| `--timeout=<ms>` | Command timeout | 30000 |

## Command Categories

### 1. Graph Commands (`unrdf graph`)

Knowledge graph resource management.

#### `unrdf graph list`

List all knowledge graphs.

**Usage:**
```bash
unrdf graph list [options]
```

**Options:**
- `--filter=<pattern>` - Filter graphs by name pattern
- `--limit=<n>` - Limit results (default: 100)
- `--offset=<n>` - Skip results (default: 0)
- `--sort=<field>` - Sort by field (name|size|modified)
- `--watch` - Watch for changes

**Examples:**
```bash
# List all graphs
unrdf graph list

# Filter by pattern
unrdf graph list --filter='prod-*'

# JSON output
unrdf graph list --format=json

# Watch for changes
unrdf graph list --watch
```

**Output:**
```
NAME            SIZE      MODIFIED           DESCRIPTION
prod-graph      45123     2025-10-01 10:30   Production graph
dev-graph       12456     2025-10-01 09:15   Development graph
test-graph      3421      2025-09-30 15:45   Test graph
```

#### `unrdf graph get <name>`

Get details of a specific graph.

**Usage:**
```bash
unrdf graph get <name> [options]
```

**Options:**
- `--show-stats` - Include statistics
- `--show-schema` - Include schema info
- `--show-provenance` - Include provenance data

**Examples:**
```bash
# Get graph details
unrdf graph get prod-graph

# With statistics
unrdf graph get prod-graph --show-stats

# JSON output with schema
unrdf graph get prod-graph --format=json --show-schema
```

**Output:**
```json
{
  "name": "prod-graph",
  "created": "2025-09-01T00:00:00Z",
  "modified": "2025-10-01T10:30:00Z",
  "size": 45123,
  "description": "Production graph",
  "stats": {
    "triples": 45123,
    "subjects": 8234,
    "predicates": 156,
    "objects": 15234
  }
}
```

#### `unrdf graph create <name>`

Create a new knowledge graph.

**Usage:**
```bash
unrdf graph create <name> [options]
```

**Options:**
- `--description=<text>` - Graph description
- `--from-template=<tpl>` - Use template
- `--import=<file>` - Import initial data

**Examples:**
```bash
# Create empty graph
unrdf graph create my-graph

# With description
unrdf graph create my-graph --description="My knowledge graph"

# From template
unrdf graph create my-graph --from-template=healthcare

# With initial data
unrdf graph create my-graph --import=data.ttl
```

#### `unrdf graph delete <name>`

Delete a knowledge graph.

**Usage:**
```bash
unrdf graph delete <name> [options]
```

**Options:**
- `--force`, `-f` - Skip confirmation
- `--backup=<path>` - Create backup before deletion

**Examples:**
```bash
# Delete with confirmation
unrdf graph delete old-graph

# Force delete
unrdf graph delete old-graph --force

# Delete with backup
unrdf graph delete old-graph --backup=./backups/
```

#### `unrdf graph import <file>`

Import data into graph.

**Usage:**
```bash
unrdf graph import <file> [options]
```

**Options:**
- `--graph=<name>` - Target graph (default: default)
- `--format=<fmt>` - Data format (turtle|nquads|jsonld|rdfxml)
- `--base-iri=<iri>` - Base IRI for relative URIs
- `--mode=<mode>` - Import mode (append|replace|merge)
- `--validate` - Validate before import

**Examples:**
```bash
# Import Turtle file
unrdf graph import data.ttl --graph=my-graph

# Import with validation
unrdf graph import data.ttl --validate --graph=my-graph

# Replace existing data
unrdf graph import data.ttl --mode=replace --graph=my-graph

# Auto-detect format
unrdf graph import data.nq --graph=my-graph
```

#### `unrdf graph export <name>`

Export graph data.

**Usage:**
```bash
unrdf graph export <name> [options]
```

**Options:**
- `--output=<file>` - Output file path
- `--format=<fmt>` - Export format (turtle|nquads|jsonld|rdfxml)
- `--compress` - Compress output (gzip)

**Examples:**
```bash
# Export to Turtle
unrdf graph export my-graph --output=export.ttl

# Export to N-Quads
unrdf graph export my-graph --format=nquads --output=export.nq

# Compressed export
unrdf graph export my-graph --output=export.ttl.gz --compress
```

#### `unrdf graph validate <name>`

Validate graph data.

**Usage:**
```bash
unrdf graph validate <name> [options]
```

**Options:**
- `--shapes=<file>` - SHACL shapes file
- `--strict` - Strict validation mode
- `--report=<file>` - Write report to file

**Examples:**
```bash
# Basic validation
unrdf graph validate my-graph

# SHACL validation
unrdf graph validate my-graph --shapes=shapes.ttl

# Strict mode with report
unrdf graph validate my-graph --strict --report=report.json
```

#### `unrdf graph merge <source> <target>`

Merge graphs.

**Usage:**
```bash
unrdf graph merge <source> <target> [options]
```

**Options:**
- `--strategy=<strat>` - Merge strategy (append|union|override)
- `--conflict-resolution=<res>` - Conflict resolution (keep-source|keep-target|fail)

**Examples:**
```bash
# Merge graphs
unrdf graph merge dev-graph prod-graph

# Union merge
unrdf graph merge dev-graph prod-graph --strategy=union

# With conflict resolution
unrdf graph merge dev-graph prod-graph --conflict-resolution=keep-target
```

#### `unrdf graph diff <graph1> <graph2>`

Compare two graphs.

**Usage:**
```bash
unrdf graph diff <graph1> <graph2> [options]
```

**Options:**
- `--show-additions` - Show added triples
- `--show-deletions` - Show deleted triples
- `--output=<file>` - Write diff to file

**Examples:**
```bash
# Compare graphs
unrdf graph diff dev-graph prod-graph

# Show only additions
unrdf graph diff dev-graph prod-graph --show-additions

# Export diff
unrdf graph diff dev-graph prod-graph --output=diff.json
```

#### `unrdf graph stats <name>`

Show graph statistics.

**Usage:**
```bash
unrdf graph stats <name> [options]
```

**Options:**
- `--detailed` - Show detailed statistics
- `--by-predicate` - Group by predicate

**Examples:**
```bash
# Basic stats
unrdf graph stats my-graph

# Detailed stats
unrdf graph stats my-graph --detailed

# By predicate
unrdf graph stats my-graph --by-predicate
```

**Output:**
```
Graph Statistics: my-graph
─────────────────────────────
Total Triples:    45,123
Unique Subjects:  8,234
Unique Predicates: 156
Unique Objects:   15,234
Size:             2.3 MB
Last Modified:    2025-10-01 10:30:00
```

### 2. Hook Commands (`unrdf hook`)

Knowledge hook lifecycle management.

#### `unrdf hook list`

List all knowledge hooks.

**Usage:**
```bash
unrdf hook list [options]
```

**Options:**
- `--kind=<kind>` - Filter by kind (before|after)
- `--enabled` - Show only enabled hooks
- `--policy-pack=<pack>` - Filter by policy pack

**Examples:**
```bash
# List all hooks
unrdf hook list

# List enabled hooks
unrdf hook list --enabled

# List by kind
unrdf hook list --kind=before

# Filter by policy pack
unrdf hook list --policy-pack=compliance-v1
```

#### `unrdf hook get <id>`

Get hook details.

**Usage:**
```bash
unrdf hook get <id> [options]
```

**Options:**
- `--show-definition` - Show full definition
- `--show-history` - Show execution history

**Examples:**
```bash
# Get hook details
unrdf hook get health-check

# With definition
unrdf hook get health-check --show-definition

# With history
unrdf hook get health-check --show-history --limit=10
```

#### `unrdf hook create <name> <type>`

Create new hook from template.

**Usage:**
```bash
unrdf hook create <name> <type> [options]
```

**Types:**
- `sparql-ask` - SPARQL ASK query hook
- `sparql-select` - SPARQL SELECT query hook
- `shacl` - SHACL validation hook
- `delta` - Delta change detection hook
- `threshold` - Threshold monitoring hook
- `count` - Count monitoring hook
- `window` - Time window aggregation hook

**Options:**
- `--description=<text>` - Hook description
- `--query=<sparql>` - SPARQL query (for SPARQL hooks)
- `--shapes=<file>` - Shapes file (for SHACL hooks)
- `--threshold=<value>` - Threshold value
- `--output=<dir>` - Output directory (default: ./hooks/)

**Examples:**
```bash
# Create SPARQL ASK hook
unrdf hook create health-check sparql-ask \
  --query="ASK { ?s a :HealthCheck }" \
  --description="Health check hook"

# Create SHACL hook
unrdf hook create data-quality shacl \
  --shapes=shapes.ttl \
  --description="Data quality validation"

# Create threshold hook
unrdf hook create large-dataset threshold \
  --threshold=10000 \
  --query="SELECT (COUNT(?s) AS ?count) WHERE { ?s ?p ?o }"
```

#### `unrdf hook eval <id>`

Evaluate hook on data.

**Usage:**
```bash
unrdf hook eval <id> [options]
```

**Options:**
- `--data=<file>` - Data file or directory
- `--graph=<name>` - Graph to evaluate
- `--output=<file>` - Write result to file

**Examples:**
```bash
# Evaluate hook on file
unrdf hook eval health-check --data=./graphs/production.ttl

# Evaluate on graph
unrdf hook eval health-check --graph=prod-graph

# Export result
unrdf hook eval health-check --data=data.ttl --output=result.json
```

#### `unrdf hook history <id>`

Show hook execution history.

**Usage:**
```bash
unrdf hook history <id> [options]
```

**Options:**
- `--limit=<n>` - Limit results (default: 100)
- `--since=<date>` - Show since date
- `--fired-only` - Show only fired events

**Examples:**
```bash
# Recent history
unrdf hook history health-check

# Last 20 executions
unrdf hook history health-check --limit=20

# Since date
unrdf hook history health-check --since=2025-09-01

# Only fired events
unrdf hook history health-check --fired-only
```

#### `unrdf hook enable <id>`

Enable hook.

**Usage:**
```bash
unrdf hook enable <id>
```

**Examples:**
```bash
unrdf hook enable health-check
```

#### `unrdf hook disable <id>`

Disable hook.

**Usage:**
```bash
unrdf hook disable <id>
```

**Examples:**
```bash
unrdf hook disable health-check
```

#### `unrdf hook validate <id>`

Validate hook definition.

**Usage:**
```bash
unrdf hook validate <id>
```

**Examples:**
```bash
# Validate hook
unrdf hook validate health-check

# Validate hook file
unrdf hook validate ./hooks/health-check.json
```

#### `unrdf hook test <id>`

Test hook evaluation.

**Usage:**
```bash
unrdf hook test <id> [options]
```

**Options:**
- `--test-data=<file>` - Test data file
- `--dry-run` - Don't execute effects

**Examples:**
```bash
# Test hook
unrdf hook test health-check --test-data=test.ttl

# Dry run
unrdf hook test health-check --dry-run
```

### 3. Policy Commands (`unrdf policy`)

Policy pack governance.

#### `unrdf policy list`

List policy packs.

**Usage:**
```bash
unrdf policy list [options]
```

**Options:**
- `--enabled` - Show only enabled packs
- `--version=<ver>` - Filter by version

**Examples:**
```bash
# List all policies
unrdf policy list

# Enabled only
unrdf policy list --enabled
```

#### `unrdf policy get <id>`

Get policy pack details.

**Usage:**
```bash
unrdf policy get <id> [options]
```

**Examples:**
```bash
# Get policy details
unrdf policy get compliance-v1

# JSON output
unrdf policy get compliance-v1 --format=json
```

#### `unrdf policy apply <file>`

Apply policy pack.

**Usage:**
```bash
unrdf policy apply <file> [options]
```

**Options:**
- `--validate` - Validate before applying
- `--activate` - Auto-activate after apply

**Examples:**
```bash
# Apply policy pack
unrdf policy apply compliance-pack.json

# With validation
unrdf policy apply compliance-pack.json --validate

# Apply and activate
unrdf policy apply compliance-pack.json --activate
```

#### `unrdf policy activate <id>`

Activate policy pack.

**Usage:**
```bash
unrdf policy activate <id>
```

**Examples:**
```bash
unrdf policy activate compliance-v1
```

#### `unrdf policy deactivate <id>`

Deactivate policy pack.

**Usage:**
```bash
unrdf policy deactivate <id>
```

**Examples:**
```bash
unrdf policy deactivate compliance-v1
```

#### `unrdf policy validate <file>`

Validate policy pack.

**Usage:**
```bash
unrdf policy validate <file> [options]
```

**Options:**
- `--strict` - Strict validation mode

**Examples:**
```bash
# Validate policy
unrdf policy validate compliance-pack.json

# Strict mode
unrdf policy validate compliance-pack.json --strict
```

#### `unrdf policy test <file>`

Test policy pack.

**Usage:**
```bash
unrdf policy test <file> [options]
```

**Options:**
- `--test-data=<file>` - Test data
- `--dry-run` - Dry run mode

**Examples:**
```bash
# Test policy
unrdf policy test compliance-pack.json --test-data=test.ttl

# Dry run
unrdf policy test compliance-pack.json --dry-run
```

#### `unrdf policy diff <id1> <id2>`

Compare policy packs.

**Usage:**
```bash
unrdf policy diff <id1> <id2>
```

**Examples:**
```bash
unrdf policy diff compliance-v1 compliance-v2
```

#### `unrdf policy export <id>`

Export policy pack.

**Usage:**
```bash
unrdf policy export <id> [options]
```

**Options:**
- `--output=<file>` - Output file
- `--format=<fmt>` - Export format (json|yaml)

**Examples:**
```bash
# Export policy
unrdf policy export compliance-v1 --output=export.json

# YAML export
unrdf policy export compliance-v1 --format=yaml
```

### 4. Sidecar Commands (`unrdf sidecar`)

KGC sidecar management.

#### `unrdf sidecar status`

Check sidecar status.

**Usage:**
```bash
unrdf sidecar status
```

**Examples:**
```bash
unrdf sidecar status
```

**Output:**
```
Sidecar Status
──────────────
Status:    Running
Uptime:    24h 15m
Version:   2.0.0
Protocol:  gRPC
Endpoint:  localhost:50051
Health:    Healthy
```

#### `unrdf sidecar start`

Start sidecar.

**Usage:**
```bash
unrdf sidecar start [options]
```

**Options:**
- `--detach`, `-d` - Run in background
- `--port=<port>` - Port number (default: 50051)

**Examples:**
```bash
# Start sidecar
unrdf sidecar start

# Background mode
unrdf sidecar start --detach
```

#### `unrdf sidecar stop`

Stop sidecar.

**Usage:**
```bash
unrdf sidecar stop [options]
```

**Options:**
- `--force`, `-f` - Force stop

**Examples:**
```bash
# Graceful stop
unrdf sidecar stop

# Force stop
unrdf sidecar stop --force
```

#### `unrdf sidecar restart`

Restart sidecar.

**Usage:**
```bash
unrdf sidecar restart
```

**Examples:**
```bash
unrdf sidecar restart
```

#### `unrdf sidecar logs`

View sidecar logs.

**Usage:**
```bash
unrdf sidecar logs [options]
```

**Options:**
- `--follow`, `-f` - Follow log output
- `--tail=<n>` - Show last n lines (default: 100)
- `--since=<date>` - Show since date

**Examples:**
```bash
# View logs
unrdf sidecar logs

# Follow logs
unrdf sidecar logs --follow

# Last 50 lines
unrdf sidecar logs --tail=50
```

#### `unrdf sidecar config get [key]`

Get sidecar configuration.

**Usage:**
```bash
unrdf sidecar config get [key]
```

**Examples:**
```bash
# Get all config
unrdf sidecar config get

# Get specific key
unrdf sidecar config get validation.strictMode
```

#### `unrdf sidecar config set <key>=<value>`

Set sidecar configuration.

**Usage:**
```bash
unrdf sidecar config set <key>=<value>
```

**Examples:**
```bash
# Set config value
unrdf sidecar config set validation.strictMode=true

# Set nested value
unrdf sidecar config set performance.maxConcurrency=20
```

#### `unrdf sidecar health`

Run health check.

**Usage:**
```bash
unrdf sidecar health
```

**Examples:**
```bash
unrdf sidecar health
```

#### `unrdf sidecar metrics`

Show sidecar metrics.

**Usage:**
```bash
unrdf sidecar metrics [options]
```

**Options:**
- `--watch` - Watch metrics
- `--interval=<ms>` - Update interval (default: 5000)

**Examples:**
```bash
# Show metrics
unrdf sidecar metrics

# Watch metrics
unrdf sidecar metrics --watch --interval=1000
```

### 5. Store Commands (`unrdf store`)

Triple store operations.

#### `unrdf store stats`

Show store statistics.

**Usage:**
```bash
unrdf store stats
```

**Examples:**
```bash
unrdf store stats
```

#### `unrdf store import <file>`

Import data to store.

**Usage:**
```bash
unrdf store import <file> [options]
```

**Options:**
- `--format=<fmt>` - Data format
- `--graph=<name>` - Target graph

**Examples:**
```bash
# Import file
unrdf store import data.ttl

# Specify format
unrdf store import data.rdf --format=rdfxml
```

#### `unrdf store export <output>`

Export store data.

**Usage:**
```bash
unrdf store export <output> [options]
```

**Options:**
- `--format=<fmt>` - Export format
- `--graph=<name>` - Source graph

**Examples:**
```bash
# Export to file
unrdf store export output.nq

# Export specific graph
unrdf store export output.ttl --graph=prod-graph
```

#### `unrdf store query <sparql>`

Execute SPARQL query.

**Usage:**
```bash
unrdf store query <sparql> [options]
```

**Options:**
- `--file`, `-f` - Read query from file
- `--limit=<n>` - Result limit
- `--offset=<n>` - Result offset

**Examples:**
```bash
# Inline query
unrdf store query "SELECT * WHERE { ?s ?p ?o } LIMIT 10"

# From file
unrdf store query --file=query.sparql

# With pagination
unrdf store query "SELECT * WHERE { ?s ?p ?o }" --limit=100 --offset=0
```

#### `unrdf store clear`

Clear store.

**Usage:**
```bash
unrdf store clear [options]
```

**Options:**
- `--graph=<name>` - Clear specific graph
- `--force`, `-f` - Skip confirmation

**Examples:**
```bash
# Clear all (with confirmation)
unrdf store clear

# Clear specific graph
unrdf store clear --graph=dev-graph --force
```

#### `unrdf store backup <file>`

Create store backup.

**Usage:**
```bash
unrdf store backup <file> [options]
```

**Options:**
- `--compress` - Compress backup

**Examples:**
```bash
# Create backup
unrdf store backup backup.nq

# Compressed backup
unrdf store backup backup.nq.gz --compress
```

#### `unrdf store restore <file>`

Restore store from backup.

**Usage:**
```bash
unrdf store restore <file> [options]
```

**Options:**
- `--mode=<mode>` - Restore mode (append|replace)

**Examples:**
```bash
# Restore backup
unrdf store restore backup.nq

# Replace mode
unrdf store restore backup.nq --mode=replace
```

### 6. Context Commands (`unrdf context`)

CLI context management.

#### `unrdf context list`

List all contexts.

**Usage:**
```bash
unrdf context list
```

**Examples:**
```bash
unrdf context list
```

**Output:**
```
NAME          CURRENT   SIDECAR              STATUS
development   *         localhost:50051      active
production              prod.example.com     active
staging                 staging:50051        inactive
```

#### `unrdf context get <name>`

Get context details.

**Usage:**
```bash
unrdf context get <name>
```

**Examples:**
```bash
unrdf context get production
```

#### `unrdf context use <name>`

Switch to context.

**Usage:**
```bash
unrdf context use <name>
```

**Examples:**
```bash
# Switch context
unrdf context use production

# Verify
unrdf context current
```

#### `unrdf context create <name>`

Create new context.

**Usage:**
```bash
unrdf context create <name> [options]
```

**Options:**
- `--sidecar=<host:port>` - Sidecar endpoint
- `--protocol=<proto>` - Protocol (grpc|http|ipc)
- `--tls` - Enable TLS

**Examples:**
```bash
# Create context
unrdf context create staging --sidecar=staging:50051

# With TLS
unrdf context create production \
  --sidecar=prod:50051 \
  --protocol=grpc \
  --tls
```

#### `unrdf context delete <name>`

Delete context.

**Usage:**
```bash
unrdf context delete <name>
```

**Examples:**
```bash
unrdf context delete staging
```

#### `unrdf context current`

Show current context.

**Usage:**
```bash
unrdf context current
```

**Examples:**
```bash
unrdf context current
```

#### `unrdf context set <key>=<value>`

Set context property.

**Usage:**
```bash
unrdf context set <key>=<value>
```

**Examples:**
```bash
# Set default format
unrdf context set defaults.format=json

# Set timeout
unrdf context set defaults.timeout=60000
```

### 7. Transaction Commands (`unrdf transaction`)

Transaction management.

#### `unrdf transaction list`

List transactions.

**Usage:**
```bash
unrdf transaction list [options]
```

**Options:**
- `--limit=<n>` - Limit results
- `--since=<date>` - Since date
- `--actor=<name>` - Filter by actor

**Examples:**
```bash
# List transactions
unrdf transaction list

# Recent 50
unrdf transaction list --limit=50

# By actor
unrdf transaction list --actor=alice
```

#### `unrdf transaction get <id>`

Get transaction details.

**Usage:**
```bash
unrdf transaction get <id> [options]
```

**Options:**
- `--show-provenance` - Show provenance data

**Examples:**
```bash
# Get transaction
unrdf transaction get tx-12345

# With provenance
unrdf transaction get tx-12345 --show-provenance
```

#### `unrdf transaction history`

Show transaction history.

**Usage:**
```bash
unrdf transaction history [options]
```

**Options:**
- `--limit=<n>` - Limit results
- `--graph=<name>` - Filter by graph

**Examples:**
```bash
# Transaction history
unrdf transaction history

# By graph
unrdf transaction history --graph=prod-graph
```

#### `unrdf transaction verify <id>`

Verify transaction integrity.

**Usage:**
```bash
unrdf transaction verify <id>
```

**Examples:**
```bash
unrdf transaction verify tx-12345
```

#### `unrdf transaction export <id>`

Export transaction receipt.

**Usage:**
```bash
unrdf transaction export <id> [options]
```

**Options:**
- `--output=<file>` - Output file
- `--format=<fmt>` - Export format

**Examples:**
```bash
# Export receipt
unrdf transaction export tx-12345 --output=receipt.json
```

### 8. Lockchain Commands (`unrdf lockchain`)

Audit trail management.

#### `unrdf lockchain list`

List lockchain receipts.

**Usage:**
```bash
unrdf lockchain list [options]
```

**Options:**
- `--limit=<n>` - Limit results
- `--since=<date>` - Since date

**Examples:**
```bash
# List receipts
unrdf lockchain list

# Recent 100
unrdf lockchain list --limit=100
```

#### `unrdf lockchain get <id>`

Get receipt details.

**Usage:**
```bash
unrdf lockchain get <id>
```

**Examples:**
```bash
unrdf lockchain get tx-12345
```

#### `unrdf lockchain verify`

Verify lockchain integrity.

**Usage:**
```bash
unrdf lockchain verify [options]
```

**Options:**
- `--detailed` - Show detailed verification

**Examples:**
```bash
# Verify chain
unrdf lockchain verify

# Detailed verification
unrdf lockchain verify --detailed
```

#### `unrdf lockchain export`

Export lockchain.

**Usage:**
```bash
unrdf lockchain export [options]
```

**Options:**
- `--output=<file>` - Output file
- `--format=<fmt>` - Export format

**Examples:**
```bash
# Export chain
unrdf lockchain export --output=chain.json
```

#### `unrdf lockchain audit`

Generate audit report.

**Usage:**
```bash
unrdf lockchain audit [options]
```

**Options:**
- `--start-date=<date>` - Start date
- `--end-date=<date>` - End date
- `--export=<file>` - Export report

**Examples:**
```bash
# Generate audit report
unrdf lockchain audit \
  --start-date=2025-01-01 \
  --end-date=2025-10-01 \
  --export=audit-report.json
```

### 9. Config Commands (`unrdf config`)

Global configuration.

#### `unrdf config get [key]`

Get configuration value.

**Usage:**
```bash
unrdf config get [key]
```

**Examples:**
```bash
# Get all config
unrdf config get

# Get specific key
unrdf config get defaults.format
```

#### `unrdf config set <key>=<value>`

Set configuration value.

**Usage:**
```bash
unrdf config set <key>=<value>
```

**Examples:**
```bash
# Set value
unrdf config set defaults.format=json

# Set nested value
unrdf config set defaults.timeout=60000
```

#### `unrdf config list`

List all configuration.

**Usage:**
```bash
unrdf config list
```

**Examples:**
```bash
unrdf config list
```

#### `unrdf config unset <key>`

Unset configuration value.

**Usage:**
```bash
unrdf config unset <key>
```

**Examples:**
```bash
unrdf config unset defaults.format
```

#### `unrdf config view`

View configuration file.

**Usage:**
```bash
unrdf config view
```

**Examples:**
```bash
unrdf config view
```

#### `unrdf config reset`

Reset to defaults.

**Usage:**
```bash
unrdf config reset [options]
```

**Options:**
- `--force`, `-f` - Skip confirmation

**Examples:**
```bash
# Reset config
unrdf config reset --force
```

## Conclusion

This command reference provides complete documentation for all UNRDF CLI commands with usage examples, options, and expected output.

**Status**: ✅ COMMAND REFERENCE COMPLETE
