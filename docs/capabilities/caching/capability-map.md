# Capability Map: @unrdf/caching

**Generated:** 2025-12-28
**Package:** @unrdf/caching
**Version:** latest

---

## Description

Multi-layer caching system for RDF queries with Redis and LRU

---

## Capability Atoms

### A44: Multi-Layer Cache

**Runtime:** Node.js
**Invariants:** LRU+Redis, invalidation-tracked
**Evidence:** `packages/caching/src/layers/multi-layer-cache.mjs:10`


### A45: SPARQL Cache

**Runtime:** Node.js
**Invariants:** query-fingerprint, dependency-graph
**Evidence:** `packages/caching/src/query/sparql-cache.mjs:12`



---

## Package Metadata

### Dependencies

- `@unrdf/oxigraph`: workspace:*
- `ioredis`: ^latest
- `lru-cache`: ^latest
- `msgpackr`: ^latest
- `zod`: ^latest

### Exports

- `.`: `./src/index.mjs`
- `./layers`: `./src/layers/multi-layer-cache.mjs`
- `./invalidation`: `./src/invalidation/dependency-tracker.mjs`
- `./query`: `./src/query/sparql-cache.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **Multi-Layer Cache**
   - Import: `import { /* exports */ } from '@unrdf/caching'`
   - Use for: Multi-Layer Cache operations
   - Runtime: Node.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/caching';

const store = createStore();
// Use caching capabilities with store
```


---

## Evidence Trail

- **A44**: `packages/caching/src/layers/multi-layer-cache.mjs:10`
- **A45**: `packages/caching/src/query/sparql-cache.mjs:12`

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
