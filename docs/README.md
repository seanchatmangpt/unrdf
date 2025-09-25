# unrdf Documentation

**unrdf** is an opinionated, composable framework for RDF knowledge operations in JavaScript. It provides a "one true path" approach to RDF development, eliminating the "dark matter" of boilerplate glue code that typically plagues RDF workflows.

## Philosophy

unrdf is not a neutral toolkit—it's a **canon** for **reactive knowledge graphs**. When you import unrdf, you're accepting its way. No escape hatches, no alternative backends, no configuration flexibility. This opinionated approach eliminates the 80/20 "dark matter" problem in RDF development.

**🎯 Knowledge Hooks** are the crown jewel of unrdf - enterprise-grade triggers that transform static knowledge graphs into intelligent, reactive systems. They provide deterministic, auditable actions with cryptographic provenance tracking.

### Core Principles

- **🎯 One Hooks**: Knowledge Hooks are the only trigger system
- **One Store**: N3.Store is the only memory model
- **One Terms**: N3 DataFactory is the only term creation method
- **One Prefixes**: Centralized prefix management
- **One Lists**: Standard rdf:List format
- **One Query Engine**: Comunica is the only SPARQL engine
- **One Validator**: SHACL is the only validation method
- **One Reasoner**: EYE is the only reasoning engine
- **One Canonicalization**: URDNA2015 is the only canonicalization method
- **One Serialization**: Turtle and N-Quads are the primary formats
- **One JSON-LD**: Standard JSON-LD format only
- **One Pointer**: Clownface is the only traversal method
- **One Validation**: Zod is the only runtime validation

## Quick Start

### Knowledge Hooks (Primary API)

```javascript
import { initStore, defineHook, evaluateHook } from 'unrdf';

// Initialize store with your RDF data
const runApp = initStore(turtleData);

runApp(async () => {
  // Define a service health monitoring hook
  const healthHook = defineHook({
    id: 'ex:ServiceHealthMonitor',
    select: 'SELECT ?service ?errorRate WHERE { ?service ex:errorRate ?errorRate }',
    predicates: [
      { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.02 } }
    ],
    combine: 'OR'
  });

  // Evaluate with cryptographic receipt
  const receipt = await evaluateHook(healthHook, { persist: true });

  if (receipt.fired) {
    console.log('🚨 Service health issue detected!');
    console.log('Evidence:', receipt.predicates);
    console.log('Provenance:', receipt.provenance);
  }
});
```

### Traditional Composables (Secondary API)

```javascript
import { useStore, useTerms, useGraph, useValidator, useZod } from 'unrdf';

// Create a store
const store = useStore();

// Add some data
const terms = useTerms();
const quad = terms.quad(
  terms.iri("http://example.org/Person"),
  terms.iri("http://xmlns.com/foaf/0.1/name"),
  terms.lit("John Doe")
);
store.add(quad);

// Create a graph interface
const graph = useGraph(store.store);

// Query with SPARQL
const results = await graph.select(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE {
    ?person foaf:name ?name .
  }
`);

// Validate with Zod
const zod = useZod();
const PersonSchema = z.object({
  name: z.string()
});

const validation = await zod.validateResults(results, PersonSchema);
console.log(validation.validated); // [{ name: "John Doe" }]
```

## Documentation Structure

### 🎯 Knowledge Hooks (Primary)
- **[Knowledge Hooks Guide](./guides/knowledge-hooks.md)** - Complete guide to the primary API
- **[Getting Started Guide](./guides/getting-started.md)** - Complete introduction to unrdf
- **[Advanced Patterns](./guides/advanced-patterns.md)** - Best practices and advanced usage

### API Reference
- **[Knowledge Hooks API](./api/knowledge-hooks.md)** - Primary API reference
- **[Composables API](./api/composables.md)** - Secondary composables reference
- **[Utilities API](./api/utilities.md)** - Helper functions for common operations

### Examples and Tutorials
- **[Knowledge Hooks Examples](./examples/knowledge-hooks/)** - Primary API examples
- **[Basic Usage Examples](./examples/basic-usage.mjs)** - Fundamental operations
- **[Validation and Reasoning](./examples/validation-reasoning.mjs)** - Advanced features

### CLI Documentation
- **[CLI Reference](./cli/README.md)** - Command-line interface usage
- **[Knowledge Hooks CLI](./cli/knowledge-hooks.md)** - Hook management commands

## Core APIs

### 🎯 Knowledge Hooks (Primary)
- **`defineHook`** - Define reactive knowledge hooks
- **`evaluateHook`** - Evaluate hooks with cryptographic receipts
- **`initStore`** - Context management for hooks
- **`useKnowledgeHooks`** - Composable hook interface

### Foundation Composables (Secondary)
- **`useStore`** - N3.Store management and operations
- **`useTerms`** - RDF term creation and manipulation
- **`usePrefixes`** - Prefix management and CURIE operations

### Data Operations (Secondary)
- **`useLists`** - RDF list operations
- **`useGraph`** - High-level RDF operations and SPARQL queries
- **`useTurtle`** - Turtle parsing and serialization
- **`useNQuads`** - N-Quads parsing and serialization
- **`useJsonLd`** - JSON-LD operations
- **`useTurtleFS`** - File system operations for Turtle files

### Advanced Features (Secondary)
- **`usePointer`** - Clownface-based graph traversal
- **`useValidator`** - SHACL validation
- **`useReasoner`** - EYE-based reasoning
- **`useCanon`** - Canonicalization and isomorphism checking
- **`useZod`** - Runtime validation for RDF-derived data
- **`useTypes`** - RDF term type checking
- **`useRDFExt`** - Advanced RDF dataset operations

### Utilities (Secondary)
- **`useIRIs`** - IRI management and resolution
- **`useCache`** - Caching operations
- **`useDelta`** - Delta operations for RDF stores
- **`useMetrics`** - Metrics and analytics

## Key Features

- **🎯 Knowledge Hooks**: Enterprise-grade reactive triggers with cryptographic provenance
- **Composable Architecture**: Focused, single-responsibility functions
- **Cryptographic Provenance**: URDNA2015 canonical hashes for all operations
- **Type Safety**: JSDoc + Zod for runtime validation
- **Performance**: Sub-millisecond hook evaluation with optimized SPARQL
- **Developer Experience**: Minimal boilerplate, maximum productivity
- **Testing**: Comprehensive test suite with edge case coverage
- **CLI Tools**: Command-line interface for hook management
- **Error Handling**: Comprehensive error handling with descriptive messages
- **Documentation**: Extensive documentation with examples

## Installation

```bash
# Using pnpm (recommended)
pnpm add unrdf

# Using npm
npm install unrdf

# Using yarn
yarn add unrdf
```

## Why unrdf?

The RDF ecosystem has matured into a diverse set of libraries, but this diversity has created fragmentation. A typical project may mix N3 for parsing, Comunica for SPARQL, rdf-ext for datasets, rdf-validate-shacl for constraints, and eyereasoner for inference. Each library is useful in isolation, but together they form a patchwork of styles, APIs, and stores.

unrdf addresses this by enforcing a single opinionated path. The framework selects a canonical implementation for each layer and wraps them in a composable API pattern. The result is not a new ontology language or reasoner but a reduction of cognitive overhead for practitioners.

## Examples

### Basic Usage
```javascript
import { useStore, useTerms, useGraph } from 'unrdf';

const store = useStore();
const terms = useTerms();

// Add data
const person = terms.iri("http://example.org/person1");
const name = terms.lit("John Doe");
store.add(terms.quad(person, terms.iri("name"), name));

// Query data
const graph = useGraph(store.store);
const results = await graph.select(`
  SELECT ?name WHERE {
    ?person <http://example.org/name> ?name .
  }
`);
```

### Validation
```javascript
import { useValidator, useZod } from 'unrdf';
import { z } from 'zod';

// SHACL validation
const validator = useValidator();
const report = await validator.validate(dataStore, shapesStore);

// Zod validation
const zod = useZod();
const schema = z.object({ name: z.string() });
const validation = await zod.validateResults(results, schema);
```

### Reasoning
```javascript
import { useReasoner } from 'unrdf';

const reasoner = useReasoner();
const inferred = await reasoner.reason(dataStore, rulesStore);
```

## CLI Usage

```bash
# Convert between formats
npx unrdf convert input.ttl output.nq --from turtle --to nquads

# Validate data
npx unrdf validate data.ttl --shapes shapes.ttl

# Query data
npx unrdf query data.ttl "SELECT ?s ?p ?o WHERE { ?s ?p ?o }"

# Apply reasoning
npx unrdf reason data.ttl --rules rules.n3 --output inferred.ttl
```

## License

MIT License - see [LICENSE](../LICENSE) for details.

## Contributing

This project follows the opinionated design philosophy. Contributions should align with the single-path approach and maintain the composable API pattern.

## Support

- **[GitHub Issues](https://github.com/gitvan/unrdf/issues)** - Report bugs or request features
- **[Discussions](https://github.com/gitvan/unrdf/discussions)** - Ask questions or share ideas
- **[Documentation](https://github.com/gitvan/unrdf#readme)** - Complete API reference