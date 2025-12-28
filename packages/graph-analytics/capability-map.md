# Capability Map: @unrdf/graph-analytics

**Generated:** 2025-12-28
**Package:** @unrdf/graph-analytics
**Version:** 1.0.0

---

## Description

Advanced graph analytics for RDF knowledge graphs using graphlib

---

## Capability Atoms

### A55: Graph Analysis

**Runtime:** Node.js
**Invariants:** algorithmic, centrality-metrics
**Evidence:** `packages/graph-analytics/src/index.mjs`



---

## Package Metadata

### Dependencies

- `@dagrejs/graphlib`: ^2.2.4
- `graphlib`: ^2.1.8
- `zod`: ^4.1.13

### Exports

- `.`: `./src/index.mjs`
- `./converter`: `./src/converter/rdf-to-graph.mjs`
- `./centrality`: `./src/centrality/pagerank-analyzer.mjs`
- `./paths`: `./src/paths/relationship-finder.mjs`
- `./clustering`: `./src/clustering/community-detector.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **Graph Analysis**
   - Import: `import { /* exports */ } from '@unrdf/graph-analytics'`
   - Use for: Graph Analysis operations
   - Runtime: Node.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/graph-analytics';

const store = createStore();
// Use graph-analytics capabilities with store
```


---

## Evidence Trail

- **A55**: `packages/graph-analytics/src/index.mjs`

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
