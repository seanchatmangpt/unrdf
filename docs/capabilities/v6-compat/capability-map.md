# Capability Map: @unrdf/v6-compat

**Generated:** 2025-12-28
**Package:** @unrdf/v6-compat
**Version:** 6.0.0-rc.1

---

## Description

UNRDF v6 Compatibility Layer - v5 to v6 migration bridge with adapters and lint rules

---

## Capability Atoms

### A61: N3.js v6 Compatibility Layer

**Runtime:** Node.js
**Invariants:** backward-compatible, migration-helper
**Evidence:** `packages/v6-compat/src/index.mjs`



---

## Package Metadata

### Dependencies

- `@unrdf/core`: workspace:*
- `@unrdf/kgc-4d`: workspace:*
- `@unrdf/oxigraph`: workspace:*
- `zod`: ^4.1.13

### Exports

- `.`: `./src/index.mjs`
- `./adapters`: `./src/adapters.mjs`
- `./lint-rules`: `./src/lint-rules.mjs`
- `./schema-generator`: `./src/schema-generator.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **N3.js v6 Compatibility Layer**
   - Import: `import { /* exports */ } from '@unrdf/v6-compat'`
   - Use for: N3.js v6 Compatibility Layer operations
   - Runtime: Node.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/v6-compat';

const store = createStore();
// Use v6-compat capabilities with store
```


---

## Evidence Trail

- **A61**: `packages/v6-compat/src/index.mjs`

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
