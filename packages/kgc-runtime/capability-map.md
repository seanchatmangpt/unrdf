# Capability Map: @unrdf/kgc-runtime

**Generated:** 2025-12-28
**Package:** @unrdf/kgc-runtime
**Version:** 1.0.0

---

## Description

KGC governance runtime with comprehensive Zod schemas and work item system

---

## Capability Atoms

### A75: KGC Runtime Environment

**Runtime:** Node.js
**Invariants:** isolated, deterministic-execution
**Evidence:** `packages/kgc-runtime/src/index.mjs`



---

## Package Metadata

### Dependencies

- `@unrdf/oxigraph`: workspace:*
- `hash-wasm`: ^4.11.0
- `zod`: ^4.1.13

### Exports

- `.`: `./src/index.mjs`
- `./schemas`: `./src/schemas.mjs`
- `./work-item`: `./src/work-item.mjs`
- `./plugin-manager`: `./src/plugin-manager.mjs`
- `./plugin-isolation`: `./src/plugin-isolation.mjs`
- `./api-version`: `./src/api-version.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **KGC Runtime Environment**
   - Import: `import { /* exports */ } from '@unrdf/kgc-runtime'`
   - Use for: KGC Runtime Environment operations
   - Runtime: Node.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/kgc-runtime';

const store = createStore();
// Use kgc-runtime capabilities with store
```


---

## Evidence Trail

- **A75**: `packages/kgc-runtime/src/index.mjs`

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
