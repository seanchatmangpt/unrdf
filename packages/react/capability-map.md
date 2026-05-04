# Capability Map: @unrdf/react

**Generated:** 2025-12-28
**Package:** @unrdf/react
**Version:** 5.0.0

---

## Description

UNRDF React - AI Semantic Analysis Tools for RDF Knowledge Graphs (Optional Extension)

---

## Capability Atoms

### A59: React Hooks for RDF

**Runtime:** Browser
**Invariants:** reactive, hook-based
**Evidence:** `packages/react/src/index.mjs`



---

## Package Metadata

### Dependencies

- `@opentelemetry/api`: ^1.9.0
- `@unrdf/core`: workspace:*
- `@unrdf/oxigraph`: workspace:*
- `lru-cache`: ^11.1.0
- `zod`: ^4.1.13

### Exports

- `.`: `./src/index.mjs`
- `./ai-semantic`: `./src/ai-semantic/index.mjs`
- `./semantic-analyzer`: `./src/ai-semantic/semantic-analyzer.mjs`
- `./embeddings-manager`: `./src/ai-semantic/embeddings-manager.mjs`
- `./nlp-query-builder`: `./src/ai-semantic/nlp-query-builder.mjs`
- `./anomaly-detector`: `./src/ai-semantic/anomaly-detector.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **React Hooks for RDF**
   - Import: `import { /* exports */ } from '@unrdf/react'`
   - Use for: React Hooks for RDF operations
   - Runtime: Browser


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/react';

const store = createStore();
// Use react capabilities with store
```


---

## Evidence Trail

- **A59**: `packages/react/src/index.mjs`

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
