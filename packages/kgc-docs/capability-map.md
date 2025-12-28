# Capability Map: @unrdf/kgc-docs

**Generated:** 2025-12-28
**Package:** @unrdf/kgc-docs
**Version:** 1.0.0

---

## Description

KGC Markdown parser and dynamic documentation generator with proof anchoring

---

## Capability Atoms

### A73: KGC Documentation Generator

**Runtime:** Node.js
**Invariants:** receipt-driven, markdown-output
**Evidence:** `packages/kgc-docs/src/index.mjs`



---

## Package Metadata

### Dependencies

- `zod`: ^4.1.13

### Exports

- `.`: `./src/kgc-markdown.mjs`
- `./parser`: `./src/parser.mjs`
- `./renderer`: `./src/renderer.mjs`
- `./proof`: `./src/proof.mjs`
- `./reference-validator`: `./src/reference-validator.mjs`
- `./changelog-generator`: `./src/changelog-generator.mjs`
- `./executor`: `./src/executor.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **KGC Documentation Generator**
   - Import: `import { /* exports */ } from '@unrdf/kgc-docs'`
   - Use for: KGC Documentation Generator operations
   - Runtime: Node.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/kgc-docs';

const store = createStore();
// Use kgc-docs capabilities with store
```


---

## Evidence Trail

- **A73**: `packages/kgc-docs/src/index.mjs`

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
