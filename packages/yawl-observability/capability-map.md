# Capability Map: @unrdf/yawl-observability

**Generated:** 2025-12-28
**Package:** @unrdf/yawl-observability
**Version:** 1.0.0

---

## Description

Workflow observability framework with Prometheus metrics and OpenTelemetry tracing for YAWL

---

## Capability Atoms

### A68: Workflow Observability

**Runtime:** Node.js
**Invariants:** OTEL-instrumented, trace-propagation
**Evidence:** `packages/yawl-observability/src/index.mjs`



---

## Package Metadata

### Dependencies

- `@opentelemetry/api`: ^1.9.0
- `@opentelemetry/sdk-node`: ^0.208.0
- `@opentelemetry/sdk-metrics`: ^2.2.0
- `@unrdf/yawl`: workspace:*
- `prom-client`: ^15.1.3
- `zod`: ^4.1.13

### Exports

- `.`: `./src/index.mjs`
- `./metrics`: `./src/metrics.mjs`
- `./tracing`: `./src/tracing.mjs`
- `./sli`: `./src/sli.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **Workflow Observability**
   - Import: `import { /* exports */ } from '@unrdf/yawl-observability'`
   - Use for: Workflow Observability operations
   - Runtime: Node.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/yawl-observability';

const store = createStore();
// Use yawl-observability capabilities with store
```


---

## Evidence Trail

- **A68**: `packages/yawl-observability/src/index.mjs`

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
