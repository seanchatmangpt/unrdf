# Capability Map: @unrdf/nextra

**Generated:** 2025-12-28
**Package:** @unrdf/nextra
**Version:** 5.0.1

---

## Description

UNRDF documentation with Nextra 4 - Developer-focused Next.js documentation

---

## Capability Atoms

### A87: Nextra Documentation

**Runtime:** Next.js
**Invariants:** static-site-generation
**Evidence:** `packages/nextra/src/index.mjs`



---

## Package Metadata

### Dependencies

- `katex`: ^0.16.25
- `next`: ^16.1.1
- `nextra`: ^4.6.1
- `nextra-theme-docs`: ^4.6.1
- `react`: ^19.2.1
- `react-dom`: ^19.2.1
- `zod`: ^4.1.13

### Exports

- `.`: `./src/index.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **Nextra Documentation**
   - Import: `import { /* exports */ } from '@unrdf/nextra'`
   - Use for: Nextra Documentation operations
   - Runtime: Next.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/nextra';

const store = createStore();
// Use nextra capabilities with store
```


---

## Evidence Trail

- **A87**: `packages/nextra/src/index.mjs`

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
