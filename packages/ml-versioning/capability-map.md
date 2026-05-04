# Capability Map: @unrdf/ml-versioning

**Generated:** 2025-12-28
**Package:** @unrdf/ml-versioning
**Version:** 1.0.0

---

## Description

ML Model Versioning System using TensorFlow.js and UNRDF KGC-4D time-travel capabilities

---

## Capability Atoms

### A56: Model Version Control

**Runtime:** Node.js
**Invariants:** git-based, reproducible
**Evidence:** `packages/ml-versioning/src/index.mjs`



---

## Package Metadata

### Dependencies

- `@tensorflow/tfjs-node`: ^4.22.0
- `@unrdf/kgc-4d`: workspace:*
- `@unrdf/oxigraph`: workspace:*
- `@unrdf/core`: workspace:*
- `hash-wasm`: ^4.12.0
- `zod`: ^4.1.13

### Exports

- `.`: `./src/index.mjs`
- `./version-store`: `./src/version-store.mjs`
- `./examples/image-classifier`: `./src/examples/image-classifier.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **Model Version Control**
   - Import: `import { /* exports */ } from '@unrdf/ml-versioning'`
   - Use for: Model Version Control operations
   - Runtime: Node.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/ml-versioning';

const store = createStore();
// Use ml-versioning capabilities with store
```


---

## Evidence Trail

- **A56**: `packages/ml-versioning/src/index.mjs`

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
