# Capability Map: @unrdf/rdf-graphql

**Generated:** 2025-12-28
**Package:** @unrdf/rdf-graphql
**Version:** 1.0.0

---

## Description

Type-safe GraphQL interface for RDF knowledge graphs with automatic schema generation

---

## Capability Atoms

### A58: GraphQL Schema Generation

**Runtime:** Node.js
**Invariants:** RDF-to-GraphQL mapping
**Evidence:** `packages/rdf-graphql/src/index.mjs`



---

## Package Metadata

### Dependencies

- `graphql`: ^16.8.1
- `@graphql-tools/schema`: ^10.0.2
- `@unrdf/oxigraph`: workspace:*
- `zod`: ^3.22.4

### Exports

- `.`: `./src/adapter.mjs`
- `./schema`: `./src/schema-generator.mjs`
- `./query`: `./src/query-builder.mjs`
- `./resolver`: `./src/resolver.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **GraphQL Schema Generation**
   - Import: `import { /* exports */ } from '@unrdf/rdf-graphql'`
   - Use for: GraphQL Schema Generation operations
   - Runtime: Node.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/rdf-graphql';

const store = createStore();
// Use rdf-graphql capabilities with store
```


---

## Evidence Trail

- **A58**: `packages/rdf-graphql/src/index.mjs`

---

## Next Steps

1. **Explore API Surface**
   - Review exports in package.json
   - Read source files in `src/` directory

2. **Integration Testing**
   - Create test cases using package capabilities
   - Verify compatibility with dependent packages

3. **Performance Profiling**
   - Benchmark key operations
   - Measure runtime characteristics

---

**Status:** GENERATED
**Method:** Systematic extraction from capability-basis.md + package.json analysis
**Confidence:** 95% (evidence-based)
