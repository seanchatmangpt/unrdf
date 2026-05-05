# Capability Map: @unrdf/v6-core

**Generated:** 2025-12-28
**Package:** @unrdf/v6-core
**Version:** 6.0.0-rc.1

---

## Description

UNRDF v6 Core - ΔGate control plane, unified receipts, and delta contracts

---

## Capability Atoms

### A62: N3.js v6 Core APIs

**Runtime:** Node.js
**Invariants:** legacy-support, deprecated
**Evidence:** `packages/v6-core/src/index.mjs`



---

## Package Metadata

### Dependencies

- `@unrdf/kgc-substrate`: workspace:*
- `@unrdf/yawl`: workspace:*
- `@unrdf/kgc-cli`: workspace:*
- `@unrdf/kgc-4d`: workspace:*
- `@unrdf/hooks`: workspace:*
- `@unrdf/oxigraph`: workspace:*
- `@unrdf/blockchain`: workspace:*
- `citty`: ^0.1.5
- `zod`: ^3.22.4
- `hash-wasm`: ^4.11.0

### Exports

- `.`: `./src/index.mjs`
- `./browser`: `./src/browser.mjs`
- `./browser/receipt-store`: `./src/browser/receipt-store.mjs`
- `./deltagate`: `./src/deltagate.mjs`
- `./schemas`: `./src/schemas.mjs`
- `./receipts`: `./src/receipts.mjs`
- `./receipts/base-receipt`: `./src/receipts/base-receipt.mjs`
- `./receipts/merkle`: `./src/receipts/merkle/tree.mjs`
- `./delta`: `./src/delta/index.mjs`
- `./delta/schema`: `./src/delta/schema.mjs`
- `./delta/gate`: `./src/delta/gate.mjs`
- `./grammar`: `./src/grammar/index.mjs`
- `./cli`: `./src/cli/index.mjs`
- `./cli/nouns`: `./src/cli/nouns.mjs`
- `./cli/verbs`: `./src/cli/verbs.mjs`
- `./cli/spine`: `./src/cli/spine.mjs`
- `./cli/commands/receipt`: `./src/cli/commands/receipt.mjs`
- `./cli/commands/delta`: `./src/cli/commands/delta.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **N3.js v6 Core APIs**
   - Import: `import { /* exports */ } from '@unrdf/v6-core'`
   - Use for: N3.js v6 Core APIs operations
   - Runtime: Node.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/v6-core';

const store = createStore();
// Use v6-core capabilities with store
```


---

## Evidence Trail

- **A62**: `packages/v6-core/src/index.mjs`

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
