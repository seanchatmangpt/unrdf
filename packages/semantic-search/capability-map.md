# Capability Map: @unrdf/semantic-search

**Generated:** 2025-12-28
**Package:** @unrdf/semantic-search
**Version:** 1.0.0

---

## Description

AI-powered semantic search over RDF knowledge graphs using vector embeddings

---

## Capability Atoms

### A46: Vector Embedding Search

**Runtime:** Node.js
**Invariants:** cosine-similarity, transformer-based
**Evidence:** `packages/semantic-search/src/search/index.mjs:10`



---

## Package Metadata

### Dependencies

- `@unrdf/oxigraph`: workspace:*
- `@xenova/transformers`: ^2.17.2
- `vectra`: ^0.11.1
- `zod`: ^4.1.13

### Exports

- `.`: `./src/index.mjs`
- `./embeddings`: `./src/embeddings/index.mjs`
- `./search`: `./src/search/index.mjs`
- `./discovery`: `./src/discovery/index.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **Vector Embedding Search**
   - Import: `import { /* exports */ } from '@unrdf/semantic-search'`
   - Use for: Vector Embedding Search operations
   - Runtime: Node.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/semantic-search';

const store = createStore();
// Use semantic-search capabilities with store
```


---

## Evidence Trail

- **A46**: `packages/semantic-search/src/search/index.mjs:10`

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
