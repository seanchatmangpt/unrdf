# Capability Map: @unrdf/yawl-durable

**Generated:** 2025-12-28
**Package:** @unrdf/yawl-durable
**Version:** 0.1.0

---

## Description

Durable execution framework inspired by Temporal.io using YAWL and KGC-4D

---

## Capability Atoms

### A65: Durable Workflow Execution

**Runtime:** Node.js
**Invariants:** persistent, crash-recovery
**Evidence:** `packages/yawl-durable/src/index.mjs`



---

## Package Metadata

### Dependencies

- `@unrdf/yawl`: workspace:*
- `@unrdf/kgc-4d`: workspace:*
- `hash-wasm`: ^4.11.0
- `zod`: ^3.22.4

### Exports

- `.`: `./src/engine.mjs`
- `./saga`: `./src/saga.mjs`
- `./activity`: `./src/activity.mjs`
- `./replay`: `./src/replay.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **Durable Workflow Execution**
   - Import: `import { /* exports */ } from '@unrdf/yawl-durable'`
   - Use for: Durable Workflow Execution operations
   - Runtime: Node.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/yawl-durable';

const store = createStore();
// Use yawl-durable capabilities with store
```


---

## Evidence Trail

- **A65**: `packages/yawl-durable/src/index.mjs`

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
