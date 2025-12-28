# Capability Map: @unrdf/yawl-viz

**Generated:** 2025-12-28
**Package:** @unrdf/yawl-viz
**Version:** 1.0.0

---

## Description

Real-time D3.js visualization for YAWL workflows with Van der Aalst pattern rendering

---

## Capability Atoms

### A71: Workflow Visualization

**Runtime:** Browser
**Invariants:** D3-based, interactive-graph
**Evidence:** `packages/yawl-viz/src/index.mjs`



---

## Package Metadata

### Dependencies

- `@unrdf/yawl`: workspace:*
- `d3`: ^7.9.0
- `d3-graphviz`: ^5.6.0
- `d3-selection`: ^3.0.0
- `d3-zoom`: ^3.0.0
- `d3-drag`: ^3.0.0
- `d3-force`: ^3.0.0
- `d3-hierarchy`: ^3.1.2

### Exports

- `.`: `./src/visualizer.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **Workflow Visualization**
   - Import: `import { /* exports */ } from '@unrdf/yawl-viz'`
   - Use for: Workflow Visualization operations
   - Runtime: Browser


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/yawl-viz';

const store = createStore();
// Use yawl-viz capabilities with store
```


---

## Evidence Trail

- **A71**: `packages/yawl-viz/src/index.mjs`

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
