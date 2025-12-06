# Conceptual Overview

Deep dive into UNRDF architecture, design patterns, and core concepts.

## Architecture

UNRDF is built on a modular architecture with clear separation of concerns:

```
@unrdf/core          → RDF store and SPARQL engine
@unrdf/hooks         → React integration layer
@unrdf/browser       → Browser-specific optimizations
@unrdf/kgc-4d        → Knowledge construction patterns
@unrdf/streaming     → Real-time data processing
@unrdf/federation    → Multi-source integration
```

## Design Philosophy

### 1. Pure Functions First
All core operations are pure functions with no side effects:
- Predictable behavior
- Easy testing
- Better performance

### 2. TypeScript Native
Full TypeScript support from the ground up:
- 100% type coverage
- Automatic inference
- IDE autocomplete

### 3. React-Friendly
Designed for React applications:
- 40+ specialized hooks
- Context providers
- Suspense support

### 4. Standards-Compliant
Adheres to W3C standards:
- RDF 1.1 specification
- SPARQL 1.1 query language
- Turtle syntax support

## Core Concepts

### RDF Triples
Everything in UNRDF is a triple: `<subject> <predicate> <object>`

### Knowledge Graphs
Interconnected data represented as graph structures.

### SPARQL Queries
SQL-like query language for RDF data.

### Reasoning
Automatic inference of new knowledge from existing data.

## Next Steps

- [Architecture Details](/docs/explanation/architecture/overview)
- [KGC-4D Patterns](/docs/explanation/kgc-4d/overview)
- [Design Patterns](/docs/explanation/architecture/design-patterns)
