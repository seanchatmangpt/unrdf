# Capability Map: @unrdf/decision-fabric

**Generated:** 2025-12-28
**Package:** @unrdf/decision-fabric
**Version:** 0.1.0

---

## Description

Hyperdimensional Decision Fabric - Intent-to-Outcome transformation engine using μ-operators

---

## Capability Atoms

### A51: Decision Management

**Runtime:** Node.js
**Invariants:** rule-based, traceable
**Evidence:** `packages/decision-fabric/src/index.mjs`



---

## Package Metadata

### Dependencies

- `@unrdf/core`: workspace:*
- `@unrdf/hooks`: workspace:*
- `@unrdf/kgc-4d`: workspace:*
- `@unrdf/knowledge-engine`: workspace:*
- `@unrdf/oxigraph`: workspace:*
- `@unrdf/streaming`: workspace:*
- `@unrdf/validation`: workspace:*

### Exports

- `.`: `./src/index.mjs`
- `./engine`: `./src/engine.mjs`
- `./operators`: `./src/operators.mjs`
- `./socratic`: `./src/socratic-agent.mjs`
- `./pareto`: `./src/pareto-analyzer.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **Decision Management**
   - Import: `import { /* exports */ } from '@unrdf/decision-fabric'`
   - Use for: Decision Management operations
   - Runtime: Node.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/decision-fabric';

const store = createStore();
// Use decision-fabric capabilities with store
```


---

## Evidence Trail

- **A51**: `packages/decision-fabric/src/index.mjs`

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
