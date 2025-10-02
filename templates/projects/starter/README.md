# UNRDF Starter Project

A basic UNRDF project template for getting started with knowledge graphs and RDF development.

## Features

- Knowledge Hooks for reactive graph operations
- SPARQL query execution
- SHACL validation
- Sample data and schemas
- Test suite setup

## Project Structure

```
.
├── src/               # Source code
│   ├── index.mjs      # Main entry point
│   └── hooks/         # Knowledge Hooks definitions
├── data/              # RDF data files
│   └── sample.ttl     # Sample Turtle data
├── queries/           # SPARQL queries
│   └── sample.rq      # Sample query
├── shapes/            # SHACL shapes
│   └── sample-shape.ttl
├── test/              # Test files
│   └── hooks.test.mjs
└── unrdf.config.mjs   # UNRDF configuration
```

## Getting Started

1. Install dependencies:
   ```bash
   npm install
   ```

2. Run the example:
   ```bash
   npm run dev
   ```

3. Run tests:
   ```bash
   npm test
   ```

4. Query the data:
   ```bash
   npm run query
   ```

5. Validate the data:
   ```bash
   npm run validate
   ```

## Configuration

Edit `unrdf.config.mjs` to customize:
- Base IRI
- Namespace prefixes
- Validation settings
- Hook configurations

## Next Steps

1. Add your own RDF data in `data/`
2. Create custom Knowledge Hooks in `src/hooks/`
3. Define SHACL shapes in `shapes/`
4. Write SPARQL queries in `queries/`
5. Add tests in `test/`

## Documentation

- [UNRDF Documentation](https://github.com/unrdf/unrdf)
- [Knowledge Hooks Guide](https://github.com/unrdf/unrdf/blob/main/docs/knowledge-hooks.md)
- [SPARQL Tutorial](https://www.w3.org/TR/sparql11-query/)
- [SHACL Specification](https://www.w3.org/TR/shacl/)

## License

MIT
