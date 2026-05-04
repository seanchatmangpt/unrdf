# Capability Map: @unrdf/kgc-tools

**Generated:** 2025-12-28
**Package:** @unrdf/kgc-tools
**Version:** 1.0.0

---

## Description

KGC Tools - Verification, freeze, and replay utilities for KGC capsules

---

## Capability Atoms

### A78: KGC Development Tools

**Runtime:** Node.js
**Invariants:** debugging, profiling, analysis
**Evidence:** `packages/kgc-tools/src/index.mjs`



---

## Package Metadata

### Dependencies

- `@unrdf/kgc-4d`: workspace:*
- `@unrdf/kgc-runtime`: workspace:*
- `@unrdf/core`: workspace:*
- `hash-wasm`: ^4.12.0
- `zod`: ^4.1.13

### Exports

- `.`: `./src/index.mjs`
- `./verify`: `./src/verify.mjs`
- `./freeze`: `./src/freeze.mjs`
- `./replay`: `./src/replay.mjs`
- `./list`: `./src/list.mjs`
- `./tool-wrapper`: `./src/tool-wrapper.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **KGC Development Tools**
   - Import: `import { /* exports */ } from '@unrdf/kgc-tools'`
   - Use for: KGC Development Tools operations
   - Runtime: Node.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/kgc-tools';

const store = createStore();
// Use kgc-tools capabilities with store
```


---

## Evidence Trail

- **A78**: `packages/kgc-tools/src/index.mjs`

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
