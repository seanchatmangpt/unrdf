# Capability Map: @unrdf/integration-tests

**Generated:** 2025-12-28
**Package:** @unrdf/integration-tests
**Version:** 5.0.0

---

## Description

Comprehensive integration tests for UNRDF multi-package workflows

---

## Capability Atoms

### A88: End-to-End Integration Tests

**Runtime:** Node.js
**Invariants:** cross-package, system-level
**Evidence:** `packages/integration-tests/test/index.mjs`



---

## Package Metadata

### Dependencies

- `@unrdf/yawl`: workspace:*
- `@unrdf/hooks`: workspace:*
- `@unrdf/kgc-4d`: workspace:*
- `@unrdf/federation`: workspace:*
- `@unrdf/streaming`: workspace:*
- `@unrdf/oxigraph`: workspace:*
- `@unrdf/core`: workspace:*
- `zod`: ^4.1.13

### Exports

- `.`: `./src/index.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **End-to-End Integration Tests**
   - Import: `import { /* exports */ } from '@unrdf/integration-tests'`
   - Use for: End-to-End Integration Tests operations
   - Runtime: Node.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/integration-tests';

const store = createStore();
// Use integration-tests capabilities with store
```


---

## Evidence Trail

- **A88**: `packages/integration-tests/test/index.mjs`

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
