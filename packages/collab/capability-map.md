# Capability Map: @unrdf/collab

**Generated:** 2025-12-28
**Package:** @unrdf/collab
**Version:** 1.0.0

---

## Description

Real-time collaborative RDF editing using CRDTs (Yjs) with offline-first architecture

---

## Capability Atoms

### A81: Collaborative Editing

**Runtime:** Node.js/Browser
**Invariants:** CRDT-based, conflict-resolution
**Evidence:** `packages/collab/src/index.mjs`



---

## Package Metadata

### Dependencies

- `@unrdf/core`: workspace:*
- `yjs`: ^13.6.18
- `y-websocket`: ^2.0.4
- `y-indexeddb`: ^9.0.12
- `lib0`: ^0.2.97
- `zod`: ^4.1.13
- `ws`: ^8.18.0

### Exports

- `.`: `./src/index.mjs`
- `./crdt`: `./src/crdt/index.mjs`
- `./sync`: `./src/sync/index.mjs`
- `./composables`: `./src/composables/index.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **Collaborative Editing**
   - Import: `import { /* exports */ } from '@unrdf/collab'`
   - Use for: Collaborative Editing operations
   - Runtime: Node.js/Browser


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/collab';

const store = createStore();
// Use collab capabilities with store
```


---

## Evidence Trail

- **A81**: `packages/collab/src/index.mjs`

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
