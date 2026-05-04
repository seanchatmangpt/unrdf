# Capability Map: @unrdf/kgn

**Generated:** 2025-12-28
**Package:** @unrdf/kgn
**Version:** 5.0.1

---

## Description

Deterministic Nunjucks template system with custom filters and frontmatter support

---

## Capability Atoms

### A79: Knowledge Graph Navigator

**Runtime:** Node.js/Browser
**Invariants:** interactive, graph-exploration
**Evidence:** `packages/kgn/src/index.mjs`



---

## Package Metadata

### Dependencies

- `@unrdf/core`: workspace:*
- `@unrdf/test-utils`: workspace:*
- `fs-extra`: ^11.3.1
- `gray-matter`: ^4.0.3
- `nunjucks`: ^3.2.4
- `yaml`: ^2.8.1

### Exports

- `.`: `[object Object]`
- `./engine`: `[object Object]`
- `./filters`: `[object Object]`
- `./renderer`: `[object Object]`
- `./linter`: `[object Object]`
- `./templates/*`: `./src/templates/*`

---

## Integration Patterns

### Primary Use Cases

1. **Knowledge Graph Navigator**
   - Import: `import { /* exports */ } from '@unrdf/kgn'`
   - Use for: Knowledge Graph Navigator operations
   - Runtime: Node.js/Browser


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/kgn';

const store = createStore();
// Use kgn capabilities with store
```


---

## Evidence Trail

- **A79**: `packages/kgn/src/index.mjs`

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
