# CLI Overview

The UNRDF CLI provides a command-line interface for RDF operations, making it easy to work with RDF data from the terminal.

## Installation

### Global Installation

```bash
npm install -g unrdf
```

### Local Installation

```bash
npm install unrdf
npx unrdf --help
```

### Development Installation

```bash
git clone https://github.com/gitvan/unrdf.git
cd unrdf
npm install
npm run build
npm link
```

## Basic Usage

### Help and Version

```bash
# Show help
unrdf --help

# Show version
unrdf --version

# Show help for specific command
unrdf parse --help
```

### Command Structure

```bash
unrdf <command> [options] [arguments]
```

## Available Commands

### `parse` - Parse RDF Data

Parse RDF data from various formats into a standardized format.

```bash
# Parse Turtle file
unrdf parse data.ttl --format turtle

# Parse with output file
unrdf parse data.ttl --output parsed.ttl

# Parse from stdin
cat data.ttl | unrdf parse --format turtle
```

**Options:**
- `--format, -f` - Input format (turtle, n3, json-ld)
- `--output, -o` - Output file path
- `--strict` - Enable strict parsing
- `--verbose, -v` - Verbose output

### `query` - Execute SPARQL Queries

Execute SPARQL queries against RDF data.

```bash
# Query with inline SPARQL
unrdf query data.ttl --query "SELECT ?name WHERE { ?person foaf:name ?name }"

# Query with file
unrdf query data.ttl --query-file query.sparql

# Query with output format
unrdf query data.ttl --query "SELECT * WHERE { ?s ?p ?o }" --format json
```

**Options:**
- `--query, -q` - SPARQL query string
- `--query-file, -f` - SPARQL query file path
- `--format` - Output format (json, table, turtle)
- `--timeout` - Query timeout in milliseconds

### `validate` - Validate RDF Data

Validate RDF data against SHACL shapes.

```bash
# Validate data against shape
unrdf validate data.ttl shape.ttl

# Validate with output report
unrdf validate data.ttl shape.ttl --output report.json

# Validate with custom constraints
unrdf validate data.ttl --constraints constraints.json
```

**Options:**
- `--output, -o` - Output file for validation report
- `--constraints` - Custom constraints file
- `--strict` - Enable strict validation
- `--verbose, -v` - Verbose output

### `reason` - Perform OWL Reasoning

Perform OWL reasoning on RDF data.

```bash
# Reason with default settings
unrdf reason data.ttl

# Reason with output file
unrdf reason data.ttl --output reasoned.ttl

# Reason with custom rules
unrdf reason data.ttl --rules custom-rules.json
```

**Options:**
- `--output, -o` - Output file for reasoned data
- `--rules` - Custom reasoning rules file
- `--timeout` - Reasoning timeout in milliseconds
- `--verbose, -v` - Verbose output

### `convert` - Convert Between Formats

Convert between RDF and structured data formats.

```bash
# Convert RDF to JSON
unrdf convert data.ttl --to json

# Convert JSON to RDF
unrdf convert data.json --from json --to turtle

# Convert with schema
unrdf convert data.json --schema schema.json --to turtle
```

**Options:**
- `--from` - Source format (turtle, json, json-ld)
- `--to` - Target format (turtle, json, json-ld)
- `--schema` - Schema file for validation
- `--output, -o` - Output file path

### `help` - Show Help Information

Show detailed help information for commands.

```bash
# Show general help
unrdf help

# Show help for specific command
unrdf help parse

# Show help for subcommand
unrdf help query --format
```

## Configuration

### Global Configuration

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
    "ex": "http://example.org/"
  },
  "validation": {
    "strict": true,
    "reportDetails": true
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
```

## Examples

### Basic Workflow

```bash
# 1. Parse RDF data
unrdf parse data.ttl --output parsed.ttl

# 2. Query the data
unrdf query parsed.ttl --query "SELECT ?name WHERE { ?person foaf:name ?name }" --format table

# 3. Validate the data
unrdf validate parsed.ttl shape.ttl

# 4. Perform reasoning
unrdf reason parsed.ttl --output reasoned.ttl
```

### Advanced Queries

```bash
# Complex SPARQL query
unrdf query data.ttl --query "
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX ex: <http://example.org/>

SELECT ?name ?age WHERE {
  ?person a foaf:Person ;
    foaf:name ?name ;
    foaf:age ?age .
  FILTER(?age > 25)
}
" --format json
```

### Validation Workflow

```bash
# Validate with detailed report
unrdf validate data.ttl shape.ttl --output validation-report.json --verbose

# Check validation results
cat validation-report.json | jq '.conforms'
```

### Batch Processing

```bash
# Process multiple files
for file in *.ttl; do
  echo "Processing $file"
  unrdf parse "$file" --output "processed/$file"
done
```

## Error Handling

### Common Errors

1. **Parse Errors**
   ```bash
   Error: Parse error at line 5, column 10
   ```

2. **Query Errors**
   ```bash
   Error: SPARQL syntax error: unexpected token 'WHERE'
   ```

3. **Validation Errors**
   ```bash
   Error: Validation failed: 3 violations found
   ```

4. **File Not Found**
   ```bash
   Error: File not found: data.ttl
   ```

### Debug Mode

Enable debug mode for detailed error information:

```bash
# Set debug environment variable
export UNRDF_DEBUG=true

# Or use debug flag
unrdf parse data.ttl --debug
```

## Performance Tips

### Large Files

For large RDF files, use streaming:

```bash
# Stream processing
cat large-data.ttl | unrdf parse --format turtle | unrdf query --query "SELECT * WHERE { ?s ?p ?o }"
```

### Memory Management

```bash
# Process in chunks
split -l 1000 large-data.ttl chunk-
for chunk in chunk-*; do
  unrdf parse "$chunk" --output "processed/$chunk"
done
```

### Query Optimization

```bash
# Use efficient SPARQL patterns
unrdf query data.ttl --query "SELECT ?s WHERE { ?s a ex:Person }" --timeout 10000
```

## Integration

### Shell Scripts

```bash
#!/bin/bash
# Process RDF data
unrdf parse "$1" --output "processed.ttl"
unrdf validate "processed.ttl" "shape.ttl"
if [ $? -eq 0 ]; then
  echo "Validation passed"
else
  echo "Validation failed"
  exit 1
fi
```

### Makefiles

```makefile
# Process RDF data
%.processed.ttl: %.ttl shape.ttl
	unrdf parse $< --output $@
	unrdf validate $@ shape.ttl

# Query RDF data
%.results.json: %.ttl query.sparql
	unrdf query $< --query-file query.sparql --format json --output $@
```

### CI/CD Pipelines

```yaml
# GitHub Actions example
- name: Validate RDF data
  run: |
    unrdf validate data.ttl shape.ttl
    if [ $? -ne 0 ]; then
      echo "Validation failed"
      exit 1
    fi
```

## Best Practices

### 1. Use Meaningful File Names

```bash
# Good
unrdf parse people-data.ttl --output people-processed.ttl

# Avoid
unrdf parse data.ttl --output out.ttl
```

### 2. Validate Before Processing

```bash
# Always validate first
unrdf validate data.ttl shape.ttl
unrdf parse data.ttl --output processed.ttl
```

### 3. Use Appropriate Output Formats

```bash
# For human reading
unrdf query data.ttl --query "SELECT ?name WHERE { ?person foaf:name ?name }" --format table

# For programmatic use
unrdf query data.ttl --query "SELECT ?name WHERE { ?person foaf:name ?name }" --format json
```

### 4. Handle Errors Gracefully

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

### 5. Use Configuration Files

```bash
# Create .unrdf/config.json
{
  "defaults": {
    "format": "turtle",
    "timeout": 30000
  }
}
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

### Getting Help

- Use `--help` flag for command-specific help
- Check the [CLI Commands documentation](./commands.md)
- Review [CLI Examples](./examples.md)
- Open an [issue](https://github.com/gitvan/unrdf/issues)

## Next Steps

- Learn about [CLI Commands](./commands.md) in detail
- Explore [CLI Examples](./examples.md) for practical usage
- Check out [CLI Testing](./testing.md) for development workflows
- Try the [playground examples](../../playground/) for interactive learning
