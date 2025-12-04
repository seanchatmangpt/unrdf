# @unrdf/knowledge-engine

**Rule Engine, Inference, and Pattern Matching** *(Optional Extension)*

Apply business rules and inference to RDF data. Includes reasoning engine and pattern matching.

## Installation

```bash
pnpm add @unrdf/knowledge-engine
```

## 📚 Examples

See these examples that demonstrate @unrdf/knowledge-engine:

- **[knowledge-engine-example.mjs](../../examples/knowledge-engine-example.mjs)** - Complete AI semantic analysis demo (1 hour)
- **[ai-semantic-example.mjs](../../examples/ai-semantic-example.mjs)** - Embedding-based semantic search
- **[comprehensive-feature-test.mjs](../../examples/comprehensive-feature-test.mjs)** - Knowledge engine integration

**Want AI-powered queries?** Start with [knowledge-engine-example.mjs](../../examples/knowledge-engine-example.mjs).

## Quick Start

```javascript
import { inferPatterns, createRuleSet } from '@unrdf/knowledge-engine'

// Define rules
const rules = createRuleSet([
  {
    name: 'infer-person',
    pattern: '?x rdf:type foaf:Person',
    then: '?x rdfs:label ?name'
  }
])

// Apply inference
const inferred = await inferPatterns(store, rules)
```

## Features

- ✅ Forward chaining inference
- ✅ Rule definition and execution
- ✅ Pattern matching on RDF data
- ✅ Built-in ontology reasoning (RDF, RDFS, OWL)
- ✅ Custom rule sets
- ✅ Performance optimization

## Use Cases

- **Business rules**: Apply domain rules to RDF
- **Data enrichment**: Infer new facts from existing data
- **Ontology reasoning**: RDFS and OWL inference
- **Data quality**: Detect and fix inconsistencies
- **Knowledge extraction**: Mine patterns from data

## Documentation

- **[API Reference](./docs/API.md)** - Complete API documentation
- **[User Guide](./docs/GUIDE.md)** - Rule definition and usage
- **[Examples](./examples/)** - Code examples
- **[Contributing](./docs/CONTRIBUTING.md)** - How to contribute

## Status

**Optional Extension** - Not required for core UNRDF functionality.
Use only if you need rule-based inference.

## Depends On

- `@unrdf/core` - RDF substrate
- `@unrdf/streaming` - Change subscriptions

## VOC Usage

- VOC-3: ML Agent (pattern matching)

## License

MIT
