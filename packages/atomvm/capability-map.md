# Capability Map: @unrdf/atomvm

**Generated:** 2025-12-28
**Package:** @unrdf/atomvm
**Version:** 5.0.1

---

## Description

Run AtomVM (Erlang/BEAM VM) in browser and Node.js using WebAssembly

---

## Capability Atoms

### A41: BEAM/Erlang WASM Execution

**Runtime:** Node.js/Browser
**Invariants:** sandboxed, bytecode-interpreted
**Evidence:** `packages/atomvm/src/index.mjs:9-11`


### A42: Service Worker Manager

**Runtime:** Browser
**Invariants:** isolated, message-passing
**Evidence:** `packages/atomvm/src/service-worker-manager.mjs:10`



---

## Package Metadata

### Dependencies

- `@opentelemetry/api`: ^1.8.0
- `@unrdf/core`: workspace:*
- `@unrdf/oxigraph`: workspace:*
- `@unrdf/streaming`: workspace:*
- `coi-serviceworker`: ^0.1.7

### Exports

- `.`: `./src/index.mjs`
- `./service-worker-manager`: `./src/service-worker-manager.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **BEAM/Erlang WASM Execution**
   - Import: `import { /* exports */ } from '@unrdf/atomvm'`
   - Use for: BEAM/Erlang WASM Execution operations
   - Runtime: Node.js/Browser


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/atomvm';

const store = createStore();
// Use atomvm capabilities with store
```


---

## Evidence Trail

- **A41**: `packages/atomvm/src/index.mjs:9-11`
- **A42**: `packages/atomvm/src/service-worker-manager.mjs:10`

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
