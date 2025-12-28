# Capability Map: @unrdf/kgc-cli

**Generated:** 2025-12-28
**Package:** @unrdf/kgc-cli
**Version:** 5.0.1

---

## Description

KGC CLI - Deterministic extension registry for ~40 workspace packages

---

## Capability Atoms

### A72: KGC Command-Line Interface

**Runtime:** Node.js
**Invariants:** CLI-commands, receipt-operations
**Evidence:** `packages/kgc-cli/src/index.mjs`



---

## Package Metadata

### Dependencies

- `citty`: ^0.1.6
- `zod`: ^3.23.0

### Exports

- `.`: `./src/index.mjs`
- `./registry`: `./src/lib/registry.mjs`
- `./manifest`: `./src/manifest/extensions.mjs`
- `./latex`: `./src/lib/latex/index.mjs`
- `./latex/schemas`: `./src/lib/latex/schemas.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **KGC Command-Line Interface**
   - Import: `import { /* exports */ } from '@unrdf/kgc-cli'`
   - Use for: KGC Command-Line Interface operations
   - Runtime: Node.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/kgc-cli';

const store = createStore();
// Use kgc-cli capabilities with store
```


---

## Evidence Trail

- **A72**: `packages/kgc-cli/src/index.mjs`

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
