# Capability Map: @unrdf/yawl-api

**Generated:** 2025-12-28
**Package:** @unrdf/yawl-api
**Version:** 1.0.0

---

## Description

High-performance REST API framework that exposes YAWL workflows as RESTful APIs with OpenAPI documentation

---

## Capability Atoms

### A64: YAWL REST API

**Runtime:** Node.js
**Invariants:** RESTful, workflow-operations
**Evidence:** `packages/yawl-api/src/index.mjs`



---

## Package Metadata

### Dependencies

- `@unrdf/yawl`: workspace:*
- `@unrdf/kgc-4d`: workspace:*
- `fastify`: ^5.2.0
- `@fastify/swagger`: ^9.3.0
- `@fastify/swagger-ui`: ^5.2.0
- `@fastify/cors`: ^10.0.1
- `zod`: ^4.1.13
- `zod-to-json-schema`: ^3.24.1

### Exports

- `.`: `./src/server.mjs`
- `./server`: `./src/server.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **YAWL REST API**
   - Import: `import { /* exports */ } from '@unrdf/yawl-api'`
   - Use for: YAWL REST API operations
   - Runtime: Node.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/yawl-api';

const store = createStore();
// Use yawl-api capabilities with store
```


---

## Evidence Trail

- **A64**: `packages/yawl-api/src/index.mjs`

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
