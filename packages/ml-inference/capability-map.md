# Capability Map: @unrdf/ml-inference

**Generated:** 2025-12-28
**Package:** @unrdf/ml-inference
**Version:** 5.0.1

---

## Description

UNRDF ML Inference - High-performance ONNX model inference pipeline for RDF streams

---

## Capability Atoms

### A47: ONNX Inference Pipeline

**Runtime:** Node.js
**Invariants:** streaming, batched
**Evidence:** `packages/ml-inference/src/pipeline/streaming-inference.mjs:10`



---

## Package Metadata

### Dependencies

- `@opentelemetry/api`: ^1.9.0
- `@unrdf/core`: workspace:*
- `@unrdf/streaming`: workspace:*
- `@unrdf/oxigraph`: workspace:*
- `onnxruntime-node`: ^1.20.1
- `zod`: ^4.1.13

### Exports

- `.`: `./src/index.mjs`
- `./runtime`: `./src/runtime/onnx-runner.mjs`
- `./pipeline`: `./src/pipeline/streaming-inference.mjs`
- `./registry`: `./src/registry/model-registry.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **ONNX Inference Pipeline**
   - Import: `import { /* exports */ } from '@unrdf/ml-inference'`
   - Use for: ONNX Inference Pipeline operations
   - Runtime: Node.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/ml-inference';

const store = createStore();
// Use ml-inference capabilities with store
```


---

## Evidence Trail

- **A47**: `packages/ml-inference/src/pipeline/streaming-inference.mjs:10`

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
