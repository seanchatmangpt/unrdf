# Capability Map: @unrdf/validation

**Generated:** 2025-12-28
**Package:** @unrdf/validation
**Version:** 5.0.1

---

## Description

OTEL validation framework for UNRDF development

---

## Capability Atoms

### A83: Schema Validation

**Runtime:** Node.js
**Invariants:** SHACL, Zod-based
**Evidence:** `packages/validation/src/index.mjs`



---

## Package Metadata

### Dependencies

- `@unrdf/knowledge-engine`: workspace:*

### Exports

- `.`: `./src/index.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **Schema Validation**
   - Import: `import { /* exports */ } from '@unrdf/validation'`
   - Use for: Schema Validation operations
   - Runtime: Node.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/validation';

const store = createStore();
// Use validation capabilities with store
```


---

## Evidence Trail

- **A83**: `packages/validation/src/index.mjs`

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
