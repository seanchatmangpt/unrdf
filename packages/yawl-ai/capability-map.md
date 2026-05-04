# Capability Map: @unrdf/yawl-ai

**Generated:** 2025-12-28
**Package:** @unrdf/yawl-ai
**Version:** 1.0.0

---

## Description

AI-powered workflow optimization using TensorFlow.js and YAWL patterns

---

## Capability Atoms

### A63: AI-Powered Workflow Routing

**Runtime:** Node.js
**Invariants:** LLM-based, semantic-task-allocation
**Evidence:** `packages/yawl-ai/src/index.mjs`



---

## Package Metadata

### Dependencies

- `@tensorflow/tfjs-node`: ^4.22.0
- `@tensorflow/tfjs-layers`: ^4.22.0
- `ml-matrix`: ^6.11.1
- `zod`: ^4.1.13

### Exports

- `.`: `./src/index.mjs`
- `./predictor`: `./src/ml/workflow-predictor.mjs`
- `./optimizer`: `./src/ml/performance-optimizer.mjs`
- `./anomaly`: `./src/ml/anomaly-detector.mjs`
- `./adapter`: `./src/integration/yawl-adapter.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **AI-Powered Workflow Routing**
   - Import: `import { /* exports */ } from '@unrdf/yawl-ai'`
   - Use for: AI-Powered Workflow Routing operations
   - Runtime: Node.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/yawl-ai';

const store = createStore();
// Use yawl-ai capabilities with store
```


---

## Evidence Trail

- **A63**: `packages/yawl-ai/src/index.mjs`

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
