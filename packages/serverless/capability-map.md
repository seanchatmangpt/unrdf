# Capability Map: @unrdf/serverless

**Generated:** 2025-12-28
**Package:** @unrdf/serverless
**Version:** 1.0.0

---

## Description

UNRDF Serverless - One-click AWS deployment for RDF applications

---

## Capability Atoms

### A60: Serverless Deployment

**Runtime:** Cloud
**Invariants:** lambda-optimized, cold-start-aware
**Evidence:** `packages/serverless/src/index.mjs`



---

## Package Metadata

### Dependencies

- `@unrdf/core`: workspace:*
- `@unrdf/oxigraph`: workspace:*
- `aws-cdk-lib`: ^2.165.0
- `constructs`: ^10.4.2
- `esbuild`: ^0.24.2
- `zod`: ^4.1.13

### Exports

- `.`: `./src/index.mjs`
- `./cdk`: `./src/cdk/index.mjs`
- `./deploy`: `./src/deploy/index.mjs`
- `./api`: `./src/api/index.mjs`
- `./storage`: `./src/storage/index.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **Serverless Deployment**
   - Import: `import { /* exports */ } from '@unrdf/serverless'`
   - Use for: Serverless Deployment operations
   - Runtime: Cloud


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/serverless';

const store = createStore();
// Use serverless capabilities with store
```


---

## Evidence Trail

- **A60**: `packages/serverless/src/index.mjs`

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
