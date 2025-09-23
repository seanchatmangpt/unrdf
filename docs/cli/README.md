# unrdf CLI Documentation

The unrdf CLI provides command-line tools for common RDF operations, making it easy to work with RDF data from the terminal.

## Installation

The CLI is included with the main unrdf package:

```bash
pnpm add unrdf
# or
npm install unrdf
```

## Usage

### Basic Commands

```bash
# Show help
npx unrdf --help

# Show version
npx unrdf --version
```

### Data Operations

#### Convert between formats

```bash
# Convert Turtle to N-Quads
npx unrdf convert input.ttl output.nq --from turtle --to nquads

# Convert N-Quads to Turtle
npx unrdf convert input.nq output.ttl --from nquads --to turtle

# Convert JSON-LD to Turtle
npx unrdf convert input.jsonld output.ttl --from jsonld --to turtle
```

#### Validate data

```bash
# Validate Turtle file
npx unrdf validate data.ttl --format turtle

# Validate N-Quads file
npx unrdf validate data.nq --format nquads

# Validate against SHACL shapes
npx unrdf validate data.ttl --shapes shapes.ttl
```

#### Query data

```bash
# Execute SPARQL SELECT query
npx unrdf query data.ttl "SELECT ?s ?p ?o WHERE { ?s ?p ?o }"

# Execute SPARQL ASK query
npx unrdf query data.ttl "ASK WHERE { ?s a <http://xmlns.com/foaf/0.1/Person> }"

# Execute CONSTRUCT query
npx unrdf query data.ttl "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }" --output result.ttl
```

### File System Operations

#### Manage Turtle files

```bash
# Load all Turtle files from directory
npx unrdf load ./data --format turtle

# Save store to Turtle file
npx unrdf save output.ttl --format turtle

# List all Turtle files in directory
npx unrdf list ./data --format turtle
```

#### Batch operations

```bash
# Process multiple files
npx unrdf batch process input/*.ttl --output ./processed/

# Merge multiple files
npx unrdf merge file1.ttl file2.ttl file3.ttl --output merged.ttl

# Split large file
npx unrdf split large.ttl --chunks 1000 --output ./chunks/
```

### Reasoning and Validation

#### EYE reasoning

```bash
# Apply reasoning rules
npx unrdf reason data.ttl --rules rules.n3 --output inferred.ttl

# Reason with built-in rules
npx unrdf reason data.ttl --builtin owl --output inferred.ttl
```

#### SHACL validation

```bash
# Validate against SHACL shapes
npx unrdf validate-shacl data.ttl shapes.ttl

# Generate validation report
npx unrdf validate-shacl data.ttl shapes.ttl --report report.json
```

#### Canonicalization

```bash
# Canonicalize RDF data
npx unrdf canon data.ttl --output canonical.nq

# Check if two files are isomorphic
npx unrdf canon file1.ttl file2.ttl --check-isomorphic
```

### Development and Testing

#### Generate test data

```bash
# Generate sample RDF data
npx unrdf generate --type person --count 100 --output sample.ttl

# Generate with specific patterns
npx unrdf generate --pattern "person with friends" --count 50 --output friends.ttl
```

#### Run tests

```bash
# Run all tests
npx unrdf test

# Run specific test file
npx unrdf test test/composables/useStore.test.mjs

# Run tests with coverage
npx unrdf test --coverage
```

#### Lint and format

```bash
# Lint code
npx unrdf lint

# Fix linting issues
npx unrdf lint --fix

# Format code
npx unrdf format
```

### Configuration

#### Configuration file

Create a `unrdf.config.mjs` file in your project root:

```javascript
export default {
  // Default base IRI
  baseIRI: "http://example.org/",
  
  // Default prefixes
  prefixes: {
    "ex": "http://example.org/",
    "foaf": "http://xmlns.com/foaf/0.1/",
    "dc": "http://purl.org/dc/terms/"
  },
  
  // File processing options
  files: {
    // Default input format
    inputFormat: "turtle",
    
    // Default output format
    outputFormat: "turtle",
    
    // Auto-detect format
    autoDetect: true
  },
  
  // Validation options
  validation: {
    // Strict mode
    strict: false,
    
    // Validate on load
    validateOnLoad: true
  },
  
  // Reasoning options
  reasoning: {
    // Default reasoner
    reasoner: "eye",
    
    // Built-in rules
    builtinRules: ["owl"]
  }
};
```

#### Environment variables

```bash
# Set default base IRI
export UNRDF_BASE_IRI="http://my-org.org/"

# Set default prefixes
export UNRDF_PREFIXES='{"ex":"http://example.org/","foaf":"http://xmlns.com/foaf/0.1/"}'

# Enable debug mode
export UNRDF_DEBUG=true

# Set log level
export UNRDF_LOG_LEVEL=info
```

### Examples

#### Complete workflow example

```bash
# 1. Generate sample data
npx unrdf generate --type person --count 100 --output people.ttl

# 2. Apply reasoning rules
npx unrdf reason people.ttl --rules rules.n3 --output people-inferred.ttl

# 3. Validate against shapes
npx unrdf validate-shacl people-inferred.ttl person-shapes.ttl

# 4. Query for results
npx unrdf query people-inferred.ttl "SELECT ?name WHERE { ?person foaf:name ?name }" --output names.json

# 5. Convert to different format
npx unrdf convert people-inferred.ttl people.nq --from turtle --to nquads
```

#### Batch processing example

```bash
# Process all Turtle files in a directory
npx unrdf batch process ./data/*.ttl --output ./processed/ --operation "reason --rules rules.n3"

# Merge processed files
npx unrdf merge ./processed/*.ttl --output merged.ttl

# Validate merged result
npx unrdf validate-shacl merged.ttl shapes.ttl
```

#### Development workflow example

```bash
# 1. Initialize project
npx unrdf init my-rdf-project

# 2. Add sample data
npx unrdf generate --type person --count 10 --output data/sample.ttl

# 3. Create shapes
npx unrdf create-shapes --type person --output shapes/person.ttl

# 4. Run tests
npx unrdf test

# 5. Build project
npx unrdf build

# 6. Deploy
npx unrdf deploy
```

### Advanced Usage

#### Custom scripts

Create custom CLI scripts using unrdf:

```javascript
#!/usr/bin/env node

import { useStore, useTurtle, useGraph } from 'unrdf';

// Custom CLI script
async function main() {
  const args = process.argv.slice(2);
  const inputFile = args[0];
  const outputFile = args[1];
  
  if (!inputFile || !outputFile) {
    console.error('Usage: node script.mjs <input.ttl> <output.ttl>');
    process.exit(1);
  }
  
  try {
    // Load data
    const turtle = useTurtle();
    const store = await turtle.parseFile(inputFile);
    
    // Process data
    const graph = useGraph(store);
    const results = await graph.select(`
      SELECT ?s ?p ?o WHERE {
        ?s ?p ?o .
      }
    `);
    
    // Save results
    await turtle.saveFile(outputFile, store);
    
    console.log(`Processed ${results.length} results`);
  } catch (error) {
    console.error('Error:', error.message);
    process.exit(1);
  }
}

main();
```

#### Integration with other tools

```bash
# Use with jq for JSON processing
npx unrdf query data.ttl "SELECT ?name WHERE { ?person foaf:name ?name }" | jq '.[].name.value'

# Use with grep for text processing
npx unrdf convert data.ttl - --from turtle --to nquads | grep "foaf:name"

# Use with sort for ordering
npx unrdf query data.ttl "SELECT ?name WHERE { ?person foaf:name ?name }" | jq -r '.[].name.value' | sort
```

### Troubleshooting

#### Common issues

1. **File not found**
   ```bash
   # Check if file exists
   ls -la input.ttl
   
   # Use absolute path
   npx unrdf convert /full/path/to/input.ttl output.ttl
   ```

2. **Format detection issues**
   ```bash
   # Specify format explicitly
   npx unrdf convert input.ttl output.nq --from turtle --to nquads
   ```

3. **Memory issues with large files**
   ```bash
   # Use streaming mode
   npx unrdf convert input.ttl output.nq --stream
   
   # Process in chunks
   npx unrdf split input.ttl --chunks 1000
   ```

#### Debug mode

```bash
# Enable debug output
npx unrdf convert input.ttl output.nq --debug

# Set log level
npx unrdf convert input.ttl output.nq --log-level debug

# Show detailed error information
npx unrdf convert input.ttl output.nq --verbose
```

### Performance Tips

1. **Use appropriate formats**
   - N-Quads for large datasets
   - Turtle for human-readable data
   - JSON-LD for web integration

2. **Optimize queries**
   - Use specific predicates
   - Add LIMIT clauses
   - Use appropriate indexes

3. **Batch operations**
   - Process multiple files together
   - Use streaming for large files
   - Cache intermediate results

### Contributing

To contribute to the CLI:

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Submit a pull request

### Support

- **GitHub Issues**: [Report bugs or request features](https://github.com/gitvan/unrdf/issues)
- **Discussions**: [Ask questions or share ideas](https://github.com/gitvan/unrdf/discussions)
- **Documentation**: [Complete API reference](https://github.com/gitvan/unrdf#readme)
