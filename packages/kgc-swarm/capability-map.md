# Capability Map: @unrdf/kgc-swarm

**Generated:** 2025-12-28
**Package:** @unrdf/kgc-swarm
**Version:** 1.0.0

---

## Description

Multi-agent template orchestration with cryptographic receipts - KGC planning meets kgn rendering

---

## Capability Atoms

### A77: Multi-Agent Swarm Coordination

**Runtime:** Node.js
**Invariants:** distributed, consensus-based
**Evidence:** `packages/kgc-swarm/src/index.mjs`



---

## Package Metadata

### Dependencies

- `@unrdf/core`: workspace:*
- `@unrdf/oxigraph`: workspace:*
- `@unrdf/kgc-substrate`: workspace:*
- `@unrdf/kgn`: workspace:*
- `@unrdf/knowledge-engine`: workspace:*
- `@unrdf/kgc-4d`: workspace:*
- `hash-wasm`: ^4.11.0
- `zod`: ^4.1.13

### Exports

- `.`: `./src/index.mjs`
- `./guards`: `./src/guards.mjs`
- `./orchestrator`: `./src/orchestrator.mjs`
- `./token-generator`: `./src/token-generator.mjs`
- `./compressor`: `./src/compressor.mjs`
- `./tracker`: `./src/tracker.mjs`
- `./guardian`: `./src/guardian.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **Multi-Agent Swarm Coordination**
   - Import: `import { /* exports */ } from '@unrdf/kgc-swarm'`
   - Use for: Multi-Agent Swarm Coordination operations
   - Runtime: Node.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/kgc-swarm';

const store = createStore();
// Use kgc-swarm capabilities with store
```


---

## Evidence Trail

- **A77**: `packages/kgc-swarm/src/index.mjs`

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
