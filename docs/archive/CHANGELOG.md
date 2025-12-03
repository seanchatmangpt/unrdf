# Changelog

All notable changes to unrdf will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Comprehensive documentation in `docs/` directory
- Getting started guide
- API reference
- Migration guide from other RDF libraries
- Examples and tutorials
- Contributing guidelines

## [1.0.0] - 2024-01-XX

### Added
- Initial release of unrdf
- Core composables: useStore, useGraph, useTurtle, useValidator, useReasoner, useCanon, useZod
- Extended composables: usePrefixes, useTerms, usePointer, useJsonLd, useNQuads, useTurtleFS, useCache, useDelta, useMetrics, useLists
- RdfEngine with N3.js, Comunica, SHACL, EYE, rdf-canonize, jsonld, and Clownface integration
- Comprehensive utility functions for term manipulation, graph operations, validation, ID management, namespace operations, SPARQL building, data transformation, quality assessment, debugging, I/O operations, and quad utilities
- JSDoc + Zod only approach (no TypeScript)
- Opinionated single path through RDF universe
- Comprehensive error handling and edge case coverage
- Performance optimizations and deterministic operations
- Extensive test suite with edge case testing
- Build system with obuild
- ESLint and Prettier configuration
- Vitest testing framework
- Coverage reporting

### Features
- **Store Management**: N3.Store with composable interface
- **SPARQL Operations**: SELECT, ASK, CONSTRUCT, UPDATE queries
- **Turtle I/O**: Loading, parsing, and saving Turtle files
- **SHACL Validation**: Data validation with comprehensive error reporting
- **N3 Reasoning**: Rule-based reasoning with EYE reasoner
- **Canonicalization**: URDNA2015 canonicalization and isomorphism checking
- **Runtime Validation**: Zod integration for data validation
- **Graph Operations**: Union, difference, intersection operations
- **JSON-LD Processing**: Conversion between RDF and JSON-LD
- **Prefix Management**: CURIE expansion and shrinking
- **Term Creation**: Named nodes, literals, blank nodes, quads
- **Graph Traversal**: Clownface-based traversal
- **Caching**: Expensive operation caching
- **Delta Operations**: Change tracking between stores
- **Metrics**: Performance monitoring
- **ID Management**: UUID and blank node ID generation
- **Namespace Operations**: IRI and CURIE manipulation
- **SPARQL Building**: Query construction utilities
- **Data Transformation**: Store transformation operations
- **Quality Assessment**: RDF data quality evaluation
- **Debug Utilities**: RDF data inspection
- **I/O Operations**: File and stream operations
- **Quad Utilities**: Quad manipulation and conversion

### Technical Details
- **Package Manager**: pnpm only
- **File Format**: .mjs modules only
- **Type System**: JSDoc + Zod only (no TypeScript)
- **Build System**: obuild for zero-transpile builds
- **Testing**: Vitest with comprehensive coverage
- **Linting**: ESLint + Prettier
- **Dependencies**: N3, Comunica, SHACL, EYE, rdf-canonize, jsonld, Clownface, Zod
- **Node.js**: 18+ support
- **ES Modules**: Native ES module support
- **Deterministic**: Consistent, reproducible operations
- **Performance**: Optimized for large datasets
- **Error Handling**: Comprehensive error handling and recovery
- **Edge Cases**: Extensive edge case testing and handling

### Documentation
- Complete API reference
- Getting started guide
- Core concepts and philosophy
- Migration guide from other RDF libraries
- Comprehensive examples
- Contributing guidelines
- JSDoc documentation for all functions

### Testing
- Unit tests for all composables and utilities
- Integration tests for composable interactions
- Edge case tests for error conditions
- Performance tests for large datasets
- 100% test coverage for core functionality
- Comprehensive error handling tests

### Performance
- Optimized for large RDF datasets
- Streaming support for memory efficiency
- Caching for expensive operations
- Deterministic operations for consistency
- Timeout management for long-running operations
- Memory management and cleanup

### Security
- Input validation for all operations
- Safe error handling and reporting
- No external code execution
- Secure file operations
- Proper resource cleanup

## [0.1.0] - 2024-01-XX

### Added
- Initial development version
- Basic composable structure
- Core engine implementation
- Basic utility functions
- Initial test suite
- Build configuration
- Package structure