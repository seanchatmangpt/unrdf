# Capability Map: @unrdf/kgc-probe

**Generated:** 2025-12-28
**Package:** @unrdf/kgc-probe
**Version:** 1.0.0

---

## Description

KGC Probe - Automated knowledge graph integrity scanning with 10 agents and artifact validation

---

## Capability Atoms

### A74: KGC Receipt Verification

**Runtime:** Node.js
**Invariants:** cryptographic-validation, audit-trail
**Evidence:** `packages/kgc-probe/src/index.mjs`



---

## Package Metadata

### Dependencies

- `@unrdf/kgc-substrate`: workspace:*
- `@unrdf/kgc-4d`: workspace:*
- `@unrdf/v6-core`: workspace:*
- `@unrdf/oxigraph`: workspace:*
- `@unrdf/hooks`: workspace:*
- `@unrdf/yawl`: workspace:*
- `hash-wasm`: ^4.12.0
- `zod`: ^4.1.13

### Exports

- `.`: `./src/index.mjs`
- `./orchestrator`: `./src/orchestrator.mjs`
- `./guards`: `./src/guards.mjs`
- `./agents`: `./src/agents/index.mjs`
- `./storage`: `./src/storage/index.mjs`
- `./types`: `./src/types.mjs`
- `./artifact`: `./src/artifact.mjs`
- `./cli`: `./src/cli.mjs`
- `./utils`: `./src/utils/index.mjs`
- `./utils/logger`: `./src/utils/logger.mjs`
- `./utils/errors`: `./src/utils/errors.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **KGC Receipt Verification**
   - Import: `import { /* exports */ } from '@unrdf/kgc-probe'`
   - Use for: KGC Receipt Verification operations
   - Runtime: Node.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/kgc-probe';

const store = createStore();
// Use kgc-probe capabilities with store
```


---

## Evidence Trail

- **A74**: `packages/kgc-probe/src/index.mjs`

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
