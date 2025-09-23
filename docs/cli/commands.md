# CLI Commands Reference

This document provides detailed reference for all UNRDF CLI commands, their options, and usage examples.

## Command Structure

```bash
unrdf <command> [options] [arguments]
```

## Global Options

These options are available for all commands:

- `--help, -h` - Show help information
- `--version, -v` - Show version information
- `--verbose` - Enable verbose output
- `--debug` - Enable debug mode
- `--config <file>` - Use custom configuration file

## Commands

### `parse`

Parse RDF data from various formats into a standardized format.

#### Syntax

```bash
unrdf parse <input> [options]
```

#### Arguments

- `<input>` - Input file path or RDF data string (required)

#### Options

- `--format, -f <format>` - Input format (turtle, n3, json-ld) [default: turtle]
- `--output, -o <file>` - Output file path
- `--strict` - Enable strict parsing
- `--prefixes <file>` - Custom prefixes file
- `--base <uri>` - Base URI for relative IRIs

#### Examples

```bash
# Parse Turtle file
unrdf parse data.ttl

# Parse with specific format
unrdf parse data.json --format json-ld

# Parse with output file
unrdf parse data.ttl --output parsed.ttl

# Parse with strict mode
unrdf parse data.ttl --strict

# Parse from stdin
cat data.ttl | unrdf parse --format turtle

# Parse with custom prefixes
unrdf parse data.ttl --prefixes prefixes.json

# Parse with base URI
unrdf parse data.ttl --base "http://example.org/"
```

#### Output

The command outputs:
- Number of triples parsed
- Parse statistics (subjects, predicates, objects)
- Error messages if parsing fails

### `query`

Execute SPARQL queries against RDF data.

#### Syntax

```bash
unrdf query <input> [options]
```

#### Arguments

- `<input>` - Input RDF file path (required)

#### Options

- `--query, -q <query>` - SPARQL query string
- `--query-file, -f <file>` - SPARQL query file path
- `--format <format>` - Output format (json, table, turtle) [default: table]
- `--timeout <ms>` - Query timeout in milliseconds [default: 30000]
- `--limit <n>` - Limit number of results
- `--offset <n>` - Offset for pagination

#### Examples

```bash
# Query with inline SPARQL
unrdf query data.ttl --query "SELECT ?name WHERE { ?person foaf:name ?name }"

# Query with file
unrdf query data.ttl --query-file query.sparql

# Query with JSON output
unrdf query data.ttl --query "SELECT * WHERE { ?s ?p ?o }" --format json

# Query with timeout
unrdf query data.ttl --query "SELECT * WHERE { ?s ?p ?o }" --timeout 60000

# Query with pagination
unrdf query data.ttl --query "SELECT ?name WHERE { ?person foaf:name ?name }" --limit 10 --offset 20
```

#### Output Formats

**Table Format (default):**
```
name
---
John Doe
Jane Smith
```

**JSON Format:**
```json
[
  {"name": "John Doe"},
  {"name": "Jane Smith"}
]
```

**Turtle Format:**
```turtle
@prefix ex: <http://example.org/> .
ex:result1 ex:name "John Doe" .
ex:result2 ex:name "Jane Smith" .
```

### `validate`

Validate RDF data against SHACL shapes.

#### Syntax

```bash
unrdf validate <data> <shape> [options]
```

#### Arguments

- `<data>` - RDF data file path (required)
- `<shape>` - SHACL shape file path (required)

#### Options

- `--output, -o <file>` - Output file for validation report
- `--format <format>` - Report format (json, text) [default: text]
- `--strict` - Enable strict validation
- `--max-errors <n>` - Maximum number of errors to report [default: 100]
- `--include-warnings` - Include warnings in report

#### Examples

```bash
# Basic validation
unrdf validate data.ttl shape.ttl

# Validation with output report
unrdf validate data.ttl shape.ttl --output report.json

# Validation with strict mode
unrdf validate data.ttl shape.ttl --strict

# Validation with custom error limit
unrdf validate data.ttl shape.ttl --max-errors 50

# Validation with warnings
unrdf validate data.ttl shape.ttl --include-warnings
```

#### Output

**Text Format:**
```
Validation Results:
- Conforms: false
- Violations: 3

Validation Violations:
- Focus Node: http://example.org/john
  Message: Person must have a name
  Severity: http://www.w3.org/ns/shacl#Violation
  Path: http://xmlns.com/foaf/0.1/name
```

**JSON Format:**
```json
{
  "conforms": false,
  "violations": 3,
  "results": [
    {
      "focusNode": "http://example.org/john",
      "message": "Person must have a name",
      "severity": "http://www.w3.org/ns/shacl#Violation",
      "path": "http://xmlns.com/foaf/0.1/name"
    }
  ]
}
```

### `reason`

Perform OWL reasoning on RDF data.

#### Syntax

```bash
unrdf reason <input> [options]
```

#### Arguments

- `<input>` - Input RDF file path (required)

#### Options

- `--output, -o <file>` - Output file for reasoned data
- `--format <format>` - Output format (turtle, n3, json-ld) [default: turtle]
- `--rules <file>` - Custom reasoning rules file
- `--timeout <ms>` - Reasoning timeout in milliseconds [default: 60000]
- `--enable-owl` - Enable OWL reasoning [default: true]
- `--transitive-closure` - Enable transitive closure [default: true]

#### Examples

```bash
# Basic reasoning
unrdf reason data.ttl

# Reasoning with output file
unrdf reason data.ttl --output reasoned.ttl

# Reasoning with custom rules
unrdf reason data.ttl --rules custom-rules.json

# Reasoning with timeout
unrdf reason data.ttl --timeout 120000

# Reasoning without OWL
unrdf reason data.ttl --enable-owl false

# Reasoning without transitive closure
unrdf reason data.ttl --transitive-closure false
```

#### Output

The command outputs:
- Number of original triples
- Number of inferred triples
- Total number of triples
- Reasoning statistics

### `convert`

Convert between RDF and structured data formats.

#### Syntax

```bash
unrdf convert <input> [options]
```

#### Arguments

- `<input>` - Input file path (required)

#### Options

- `--from <format>` - Source format (turtle, json, json-ld) [default: auto-detect]
- `--to <format>` - Target format (turtle, json, json-ld) [default: turtle]
- `--output, -o <file>` - Output file path
- `--schema <file>` - Schema file for validation
- `--context <file>` - JSON-LD context file
- `--compact` - Use compact format for JSON-LD

#### Examples

```bash
# Convert Turtle to JSON
unrdf convert data.ttl --to json

# Convert JSON to Turtle
unrdf convert data.json --from json --to turtle

# Convert with schema validation
unrdf convert data.json --schema schema.json --to turtle

# Convert with JSON-LD context
unrdf convert data.json --context context.json --to json-ld

# Convert with compact format
unrdf convert data.json --to json-ld --compact
```

#### Supported Formats

- **Turtle (ttl)** - Human-readable RDF format
- **JSON-LD** - JSON format for RDF
- **N-Triples (nt)** - Simple line-based RDF format
- **JSON** - Structured data format

### `help`

Show help information for commands.

#### Syntax

```bash
unrdf help [command]
```

#### Arguments

- `[command]` - Command to get help for (optional)

#### Examples

```bash
# Show general help
unrdf help

# Show help for specific command
unrdf help parse

# Show help for command option
unrdf help query --format
```

## Configuration

### Configuration File

Create a configuration file at `~/.unrdf/config.json`:

```json
{
  "defaults": {
    "format": "turtle",
    "timeout": 30000,
    "strict": false
  },
  "prefixes": {
    "foaf": "http://xmlns.com/foaf/0.1/",
    "ex": "http://example.org/",
    "schema": "https://schema.org/"
  },
  "validation": {
    "strict": true,
    "reportDetails": true,
    "maxErrors": 100
  },
  "reasoning": {
    "enableOWL": true,
    "transitiveClosure": true,
    "timeout": 60000
  }
}
```

### Environment Variables

```bash
# Set default format
export UNRDF_DEFAULT_FORMAT=turtle

# Set default timeout
export UNRDF_DEFAULT_TIMEOUT=30000

# Enable debug mode
export UNRDF_DEBUG=true

# Set configuration file
export UNRDF_CONFIG=/path/to/config.json
```

## Error Handling

### Common Error Codes

- `0` - Success
- `1` - General error
- `2` - Parse error
- `3` - Query error
- `4` - Validation error
- `5` - Reasoning error
- `6` - Conversion error
- `7` - File not found
- `8` - Permission denied

### Error Messages

**Parse Errors:**
```
Error: Parse error at line 5, column 10
Unexpected token: ';'
```

**Query Errors:**
```
Error: SPARQL syntax error
Unexpected token: 'WHERE'
```

**Validation Errors:**
```
Error: Validation failed
3 violations found
```

**File Errors:**
```
Error: File not found
data.ttl: No such file or directory
```

## Examples

### Complete Workflow

```bash
# 1. Parse RDF data
unrdf parse data.ttl --output parsed.ttl

# 2. Validate the data
unrdf validate parsed.ttl shape.ttl

# 3. Query the data
unrdf query parsed.ttl --query "SELECT ?name WHERE { ?person foaf:name ?name }" --format table

# 4. Perform reasoning
unrdf reason parsed.ttl --output reasoned.ttl

# 5. Convert to different format
unrdf convert reasoned.ttl --to json --output reasoned.json
```

### Batch Processing

```bash
# Process multiple files
for file in *.ttl; do
  echo "Processing $file"
  unrdf parse "$file" --output "processed/$file"
  unrdf validate "processed/$file" shape.ttl
done
```

### Pipeline Processing

```bash
# Chain multiple operations
cat data.ttl | unrdf parse --format turtle | unrdf query --query "SELECT * WHERE { ?s ?p ?o }" --format json
```

## Best Practices

### 1. Use Configuration Files

```bash
# Create .unrdf/config.json
{
  "defaults": {
    "format": "turtle",
    "timeout": 30000
  }
}
```

### 2. Handle Errors Gracefully

```bash
# Check exit codes
unrdf validate data.ttl shape.ttl
if [ $? -eq 0 ]; then
  echo "Validation passed"
else
  echo "Validation failed"
  exit 1
fi
```

### 3. Use Appropriate Output Formats

```bash
# For human reading
unrdf query data.ttl --query "SELECT ?name WHERE { ?person foaf:name ?name }" --format table

# For programmatic use
unrdf query data.ttl --query "SELECT ?name WHERE { ?person foaf:name ?name }" --format json
```

### 4. Optimize for Large Files

```bash
# Use streaming for large files
cat large-data.ttl | unrdf parse --format turtle | unrdf query --query "SELECT * WHERE { ?s ?p ?o }"
```

### 5. Use Meaningful File Names

```bash
# Good
unrdf parse people-data.ttl --output people-processed.ttl

# Avoid
unrdf parse data.ttl --output out.ttl
```

## Troubleshooting

### Common Issues

1. **Command not found**
   ```bash
   # Install globally
   npm install -g unrdf
   
   # Or use npx
   npx unrdf --help
   ```

2. **Permission denied**
   ```bash
   # Fix permissions
   chmod +x $(which unrdf)
   ```

3. **Memory issues**
   ```bash
   # Increase Node.js memory
   export NODE_OPTIONS="--max-old-space-size=4096"
   ```

4. **Timeout errors**
   ```bash
   # Increase timeout
   unrdf query data.ttl --query "..." --timeout 60000
   ```

### Debug Mode

Enable debug mode for detailed information:

```bash
# Set debug environment variable
export UNRDF_DEBUG=true

# Or use debug flag
unrdf parse data.ttl --debug
```

### Getting Help

- Use `--help` flag for command-specific help
- Check the [CLI Overview](./overview.md) for general usage
- Review [CLI Examples](./examples.md) for practical usage
- Open an [issue](https://github.com/gitvan/unrdf/issues) for bugs

## Next Steps

- Explore [CLI Examples](./examples.md) for practical usage
- Learn about [CLI Testing](./testing.md) for development workflows
- Check out the [playground examples](../../playground/) for interactive learning
