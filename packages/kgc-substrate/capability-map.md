# Capability Map: @unrdf/kgc-substrate

**Generated:** 2025-12-28
**Package:** @unrdf/kgc-substrate
**Version:** 1.0.0

---

## Description

KGC Substrate - Deterministic, hash-stable KnowledgeStore with immutable append-only log

---

## Capability Atoms

### A76: KGC Substrate Layer

**Runtime:** Node.js
**Invariants:** foundation, low-level-primitives
**Evidence:** `packages/kgc-substrate/src/index.mjs`



---

## Package Metadata

### Dependencies

- `@unrdf/kgc-4d`: workspace:*
- `@unrdf/oxigraph`: workspace:*
- `@unrdf/core`: workspace:*
- `hash-wasm`: ^4.12.0
- `zod`: ^4.1.13

### Exports

- `.`: `./src/index.mjs`
- `./types`: `./src/types.mjs`
- `./KnowledgeStore`: `./src/KnowledgeStore.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **KGC Substrate Layer**
   - Import: `import { /* exports */ } from '@unrdf/kgc-substrate'`
   - Use for: KGC Substrate Layer operations
   - Runtime: Node.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/kgc-substrate';

const store = createStore();
// Use kgc-substrate capabilities with store
```


---

## Evidence Trail

- **A76**: `packages/kgc-substrate/src/index.mjs`

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
