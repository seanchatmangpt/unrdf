# 🎯 Knowledge Hooks CLI

**Command-line interface for managing Knowledge Hooks with cryptographic provenance**

## Overview

The Knowledge Hooks CLI provides a comprehensive command-line interface for creating, evaluating, and managing Knowledge Hooks. It supports all hook types, provides detailed evaluation results, and maintains complete audit trails with cryptographic signatures.

## Installation

### Global Installation
```bash
pnpm install -g unrdf
```

### Local Installation
```bash
pnpm install unrdf
npx unrdf hook --help
```

## Quick Start

### Define a Hook

Create a hook definition file (JSON or YAML):

```json
{
  "id": "ex:ServiceHealthMonitor",
  "name": "Service Health Monitor",
  "description": "Monitors service error rates",
  "select": "SELECT ?service ?errorRate WHERE { ?service ex:errorRate ?errorRate }",
  "predicates": [
    {
      "kind": "THRESHOLD",
      "spec": {
        "var": "errorRate",
        "op": ">",
        "value": 0.02
      }
    }
  ],
  "combine": "OR"
}
```

### Evaluate a Hook

```bash
# Evaluate hook with data file
unrdf hook eval --hook hooks/service-health.json --data data/services.ttl

# Evaluate hook with inline data
unrdf hook eval --hook ex:ServiceHealthMonitor --data "
@prefix ex: <http://example.org/> .
ex:service1 ex:errorRate 0.05 .
"

# Show execution plan without evaluation
unrdf hook plan --hook hooks/service-health.json
```

### Monitor Hook Activity

```bash
# Watch hook evaluations in real-time
unrdf hook watch --hook ex:ServiceHealthMonitor

# Get evaluation history
unrdf hook receipts --hook ex:ServiceHealthMonitor --limit 10

# Verify receipt signatures
unrdf hook receipts --hook ex:ServiceHealthMonitor --verify
```

## Commands

### hook eval

Evaluate a Knowledge Hook and display results.

```bash
unrdf hook eval [options]
```

**Options:**
- `--hook, -h` - Hook ID or file path (required)
- `--data, -d` - RDF data file or string
- `--format, -f` - Data format: turtle, nquads, jsonld (default: turtle)
- `--output, -o` - Output format: json, table, yaml (default: table)
- `--persist, -p` - Persist baseline data for delta comparisons
- `--timeout, -t` - Evaluation timeout in seconds (default: 5)
- `--verify, -v` - Verify cryptographic signatures

**Examples:**
```bash
# Evaluate hook with data file
unrdf hook eval --hook hooks/monitor.json --data services.ttl

# Evaluate with inline Turtle data
unrdf hook eval --hook ex:HealthMonitor --data "
@prefix ex: <http://example.org/> .
ex:service1 ex:errorRate 0.03 .
"

# Get detailed JSON output
unrdf hook eval --hook ex:HealthMonitor --output json --verify

# Use with timeout
unrdf hook eval --hook ex:ComplexMonitor --timeout 10
```

### hook plan

Show the execution plan for a hook without evaluation.

```bash
unrdf hook plan [options]
```

**Options:**
- `--hook, -h` - Hook ID or file path (required)
- `--output, -o` - Output format: json, table, yaml (default: table)

**Examples:**
```bash
# Plan hook from file
unrdf hook plan --hook hooks/monitor.json

# Plan hook by ID
unrdf hook plan --hook ex:HealthMonitor

# Get JSON plan
unrdf hook plan --hook ex:HealthMonitor --output json
```

**Sample Output:**
```
Hook ID: ex:ServiceHealthMonitor
Query Plan:
  Variables: service, errorRate
  Estimated Complexity: low
  Estimated Duration: 15ms

Predicate Plan:
  1. THRESHOLD (errorRate > 0.02)
     Complexity: low
     Dependencies: []

Combination Logic: OR
Total Estimated Duration: 18ms
```

### hook watch

Monitor hook evaluations in real-time.

```bash
unrdf hook watch [options]
```

**Options:**
- `--hook, -h` - Hook ID to watch (required)
- `--data, -d` - RDF data file to load initially
- `--interval, -i` - Polling interval in seconds (default: 5)
- `--format, -f` - Output format: table, json, compact (default: table)
- `--verify, -v` - Verify cryptographic signatures

**Examples:**
```bash
# Watch hook with 10-second intervals
unrdf hook watch --hook ex:ServiceHealthMonitor --interval 10

# Watch with initial data load
unrdf hook watch --hook ex:HealthMonitor --data services.ttl

# Compact output format
unrdf hook watch --hook ex:HealthMonitor --format compact
```

### hook receipts

View hook evaluation history and receipts.

```bash
unrdf hook receipts [options]
```

**Options:**
- `--hook, -h` - Hook ID to get receipts for (required)
- `--limit, -l` - Number of receipts to show (default: 10)
- `--since` - Show receipts since date (ISO format)
- `--until` - Show receipts until date (ISO format)
- `--format, -f` - Output format: json, table, yaml (default: table)
- `--verify, -v` - Verify cryptographic signatures
- `--tail, -t` - Follow new receipts in real-time

**Examples:**
```bash
# Get last 5 receipts
unrdf hook receipts --hook ex:HealthMonitor --limit 5

# Get receipts from last hour
unrdf hook receipts --hook ex:HealthMonitor --since 2024-01-01T10:00:00Z

# Follow new receipts
unrdf hook receipts --hook ex:HealthMonitor --tail

# Verify all receipts
unrdf hook receipts --hook ex:HealthMonitor --verify

# Export as JSON
unrdf hook receipts --hook ex:HealthMonitor --format json
```

### hook create

Create a new hook interactively or from template.

```bash
unrdf hook create [options]
```

**Options:**
- `--template, -t` - Template type: basic, threshold, delta, shacl, ask
- `--output, -o` - Output file path
- `--format, -f` - Output format: json, yaml (default: json)

**Examples:**
```bash
# Create basic hook interactively
unrdf hook create

# Create from template
unrdf hook create --template threshold --output hooks/cpu-monitor.json

# Create SHACL validation hook
unrdf hook create --template shacl --output hooks/compliance.json
```

### hook validate

Validate hook definition syntax and semantics.

```bash
unrdf hook validate [options]
```

**Options:**
- `--hook, -h` - Hook file or ID to validate (required)
- `--data, -d` - Sample data for validation
- `--format, -f` - Output format: json, table (default: table)

**Examples:**
```bash
# Validate hook file
unrdf hook validate --hook hooks/monitor.json

# Validate with sample data
unrdf hook validate --hook hooks/monitor.json --data sample-data.ttl

# Get detailed validation report
unrdf hook validate --hook hooks/complex.json --format json
```

### hook export

Export hook receipts and data for analysis.

```bash
unrdf hook export [options]
```

**Options:**
- `--hook, -h` - Hook ID to export data for (required)
- `--since` - Start date for export (ISO format)
- `--until` - End date for export (ISO format)
- `--format, -f` - Export format: json, csv, parquet (default: json)
- `--output, -o` - Output file path
- `--include-data` - Include RDF data in export

**Examples:**
```bash
# Export hook receipts as JSON
unrdf hook export --hook ex:HealthMonitor --output health-data.json

# Export with date range
unrdf hook export --hook ex:HealthMonitor --since 2024-01-01 --until 2024-01-31

# Export as CSV for analysis
unrdf hook export --hook ex:HealthMonitor --format csv --include-data
```

## Hook Definition Formats

### JSON Format

```json
{
  "id": "ex:ServiceHealthMonitor",
  "name": "Service Health Monitor",
  "description": "Monitors service error rates and latency",
  "select": "SELECT ?service ?errorRate ?latency WHERE { ?service ex:errorRate ?errorRate ; ex:latency ?latency }",
  "predicates": [
    {
      "kind": "THRESHOLD",
      "spec": {
        "var": "errorRate",
        "op": ">",
        "value": 0.02
      }
    },
    {
      "kind": "THRESHOLD",
      "spec": {
        "var": "latency",
        "op": ">",
        "value": 2000
      }
    }
  ],
  "combine": "OR",
  "output": {
    "schema": {
      "type": "object",
      "properties": {
        "service": { "type": "string" },
        "alert": { "type": "string" }
      }
    },
    "format": "json",
    "destination": "console"
  }
}
```

### YAML Format

```yaml
id: 'ex:ServiceHealthMonitor'
name: 'Service Health Monitor'
description: 'Monitors service error rates and latency'
select: |
  SELECT ?service ?errorRate ?latency WHERE {
    ?service ex:errorRate ?errorRate ;
             ex:latency ?latency
  }
predicates:
  - kind: 'THRESHOLD'
    spec:
      var: 'errorRate'
      op: '>'
      value: 0.02
  - kind: 'THRESHOLD'
    spec:
      var: 'latency'
      op: '>'
      value: 2000
combine: 'OR'
output:
  schema:
    type: object
    properties:
      service: { type: string }
      alert: { type: string }
  format: 'json'
  destination: 'console'
```

### Markdown Format with Frontmatter

```markdown
---
hook:
  id: 'ex:ServiceHealthMonitor'
  name: 'Service Health Monitor'
  description: 'Monitors service error rates and latency'
  select: 'SELECT ?service ?errorRate WHERE { ?service ex:errorRate ?errorRate }'
  predicates:
    - kind: 'THRESHOLD'
      spec:
        var: 'errorRate'
        op: '>'
        value: 0.02
  combine: 'OR'
---

# Service Health Monitor

This hook monitors service health metrics and alerts when error rates exceed 2%.

## Usage

Load service data and evaluate the hook:

```bash
unrdf hook eval --hook service-health.md --data services.ttl
```
```

## Data Formats

The CLI supports multiple RDF data formats:

### Turtle Format
```bash
unrdf hook eval --hook ex:HealthMonitor --data services.ttl --format turtle
```

### N-Quads Format
```bash
unrdf hook eval --hook ex:HealthMonitor --data services.nq --format nquads
```

### JSON-LD Format
```bash
unrdf hook eval --hook ex:HealthMonitor --data services.jsonld --format jsonld
```

### Inline Data
```bash
unrdf hook eval --hook ex:HealthMonitor --data "
@prefix ex: <http://example.org/> .
ex:service1 ex:errorRate 0.05 .
" --format turtle
```

## Output Formats

### Table Format (Default)
```bash
unrdf hook eval --hook ex:HealthMonitor --output table
```

**Sample Output:**
```
Hook ID: ex:ServiceHealthMonitor
Fired: ✅ YES

Predicate Results:
  1. THRESHOLD (errorRate > 0.02): ✅ OK (matched: 2, max: 0.05)
  2. THRESHOLD (latency > 2000): ❌ FAIL (max: 1800)

Performance:
  Total Duration: 24ms
  Query Time: 8ms
  Predicate Time: 12ms
  Canonicalization: 4ms

Provenance:
  Hook Hash: sha256:61e0f5217...
  Query Hash: sha256:ec2b13e61...
  Graph Hash: sha256:7b52009b6...
  Receipt Hash: sha256:a1b2c3d4e...

Timestamp: 2024-01-01T10:00:00.000Z
```

### JSON Format
```bash
unrdf hook eval --hook ex:HealthMonitor --output json
```

**Sample Output:**
```json
{
  "id": "ex:ServiceHealthMonitor",
  "fired": true,
  "predicates": [
    {
      "kind": "THRESHOLD",
      "ok": true,
      "meta": {
        "matched": 2,
        "threshold": 0.02,
        "maxValue": 0.05
      },
      "duration": 12
    }
  ],
  "durations": {
    "totalMs": 24,
    "queryMs": 8,
    "predicateMs": 12,
    "canonicalizationMs": 4
  },
  "provenance": {
    "hookHash": "sha256:61e0f5217...",
    "queryHash": "sha256:ec2b13e61...",
    "graphHash": "sha256:7b52009b6...",
    "receiptHash": "sha256:a1b2c3d4e..."
  },
  "at": "2024-01-01T10:00:00.000Z",
  "input": {
    "bindings": 5,
    "variables": ["service", "errorRate"]
  }
}
```

### Compact Format
```bash
unrdf hook eval --hook ex:HealthMonitor --output compact
```

**Sample Output:**
```
🔥 ex:ServiceHealthMonitor | 24ms | 2/2 predicates | sha256:a1b2c3d4e...
```

## Configuration

### Configuration File

Create `~/.unrdf/config.json`:

```json
{
  "defaultFormat": "turtle",
  "defaultOutput": "table",
  "verifySignatures": true,
  "timeout": 10,
  "maxResults": 1000,
  "colorOutput": true,
  "logLevel": "info"
}
```

### Environment Variables

```bash
export UNRDF_DEFAULT_FORMAT=turtle
export UNRDF_VERIFY_SIGNATURES=true
export UNRDF_TIMEOUT=10
export UNRDF_COLOR_OUTPUT=true
```

## Examples

### Service Health Monitoring

```bash
# Create service health hook
unrdf hook create --template threshold --output hooks/service-health.json

# Edit hook to customize
# Modify hooks/service-health.json to monitor specific metrics

# Load service data
cat > services.ttl << 'EOF'
@prefix ex: <http://example.org/> .
ex:service1 ex:errorRate 0.01 ; ex:latency 1500 .
ex:service2 ex:errorRate 0.05 ; ex:latency 2500 .
EOF

# Evaluate hook
unrdf hook eval --hook hooks/service-health.json --data services.ttl
```

### Compliance Monitoring

```bash
# Create compliance hook
unrdf hook create --template shacl --output hooks/compliance.json

# Load shapes and data
cat > shapes.ttl << 'EOF'
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example.org/> .

ex:GDPRShape a sh:NodeShape ;
  sh:targetClass ex:SensitiveData ;
  sh:property [
    sh:path ex:consentGiven ;
    sh:minCount 1 ;
  ] .
EOF

cat > data.ttl << 'EOF'
@prefix ex: <http://example.org/> .
ex:record1 a ex:SensitiveData ;
  ex:consentGiven true .
ex:record2 a ex:SensitiveData .
EOF

# Validate compliance
unrdf hook eval --hook hooks/compliance.json --data data.ttl
```

### Configuration Drift Detection

```bash
# Create drift detection hook
unrdf hook create --template delta --output hooks/drift.json

# Load baseline configuration
cat > baseline.ttl << 'EOF'
@prefix ex: <http://example.org/> .
ex:config1 ex:value 100 .
ex:config2 ex:value 200 .
EOF

# Monitor for changes
unrdf hook watch --hook hooks/drift.json --data current-config.ttl --interval 30
```

## Error Handling

### Common Errors

#### Hook Definition Errors
```bash
# Error: Invalid SPARQL syntax
Error: Parse error in SELECT clause at position 25

# Error: Missing required fields
Error: Hook definition missing required field: 'select'

# Error: Invalid predicate specification
Error: Invalid operator '>>' in THRESHOLD predicate. Valid operators: >, >=, <, <=, ==, !=
```

#### Evaluation Errors
```bash
# Error: Timeout
Error: Hook evaluation timed out after 5000ms

# Error: Data not found
Error: No RDF data available for evaluation

# Error: Invalid data format
Error: Failed to parse data: Expected valid Turtle format
```

#### Cryptographic Errors
```bash
# Error: Signature verification failed
Error: Receipt signature verification failed

# Error: Missing baseline data
Error: DELTA predicate requires baseline data for comparison
```

### Debugging

```bash
# Enable verbose logging
export UNRDF_LOG_LEVEL=debug
unrdf hook eval --hook ex:HealthMonitor

# Show execution plan
unrdf hook plan --hook ex:HealthMonitor

# Validate hook definition
unrdf hook validate --hook ex:HealthMonitor --data sample.ttl

# Test with minimal data
echo "ex:service1 ex:errorRate 0.05 ." | \
unrdf hook eval --hook ex:HealthMonitor --data - --format turtle
```

## Best Practices

### Hook Design
1. **Single Responsibility**: Each hook should monitor one logical condition
2. **Clear Names**: Use descriptive IDs and names for hooks
3. **Efficient Queries**: Optimize SPARQL queries for performance
4. **Appropriate Timeouts**: Set realistic timeouts for your use case

### Performance
1. **Use ASK for existence checks** (fastest predicate type)
2. **Limit query results** when possible
3. **Use COUNT instead of full result sets** for cardinality checks
4. **Cache expensive operations** when appropriate

### Security
1. **Validate input data** before processing
2. **Use HTTPS for webhook endpoints**
3. **Verify all cryptographic signatures**
4. **Implement proper access controls**

### Monitoring
1. **Monitor hook performance** regularly
2. **Set up alerting** for hook failures
3. **Archive old receipts** according to retention policies
4. **Review hook effectiveness** periodically

## Integration

### With CI/CD Pipelines
```yaml
# GitHub Actions example
- name: Run Knowledge Hooks
  run: |
    unrdf hook eval --hook hooks/compliance.json --data src/data.ttl
    unrdf hook eval --hook hooks/security.json --data src/data.ttl
```

### With Monitoring Systems
```bash
# Send alerts to monitoring system
unrdf hook eval --hook ex:HealthMonitor --output json | \
curl -X POST -H "Content-Type: application/json" \
  -d @- https://monitoring.example.com/alerts
```

### With Log Aggregation
```bash
# Send receipts to log aggregation system
unrdf hook receipts --hook ex:HealthMonitor --format json | \
  curl -X POST -H "Content-Type: application/json" \
  -d @- https://logs.example.com/receipts
```

## Troubleshooting

### Hook Not Firing
1. Check SPARQL query syntax and results
2. Verify predicate logic and thresholds
3. Ensure data is loaded into the correct graph
4. Check baseline data for DELTA predicates

### Performance Issues
1. Optimize SPARQL queries (use LIMIT, specific variables)
2. Choose more efficient predicate types (ASK is fastest)
3. Reduce query result set size
4. Consider query result caching

### Receipt Verification Failures
1. Verify cryptographic signatures are enabled
2. Check provenance hashes for consistency
3. Ensure data canonicalization is working properly
4. Validate receipt schema

### Memory Issues
1. Limit query result sets with LIMIT clauses
2. Use streaming for large datasets
3. Monitor memory usage with `--maxResults`
4. Clear unused data periodically

## Getting Help

### Command Help
```bash
unrdf hook --help
unrdf hook eval --help
unrdf hook watch --help
```

### Examples and Templates
```bash
# List available templates
unrdf hook create --help

# Get template examples
unrdf hook create --template basic --output example.json
```

### Community Support
- **GitHub Issues**: Report bugs and request features
- **Discussions**: Ask questions and share ideas
- **Documentation**: Complete API reference and examples

## License

MIT License - see the main project LICENSE for details.
