# Capability Map: @unrdf/yawl-langchain

**Generated:** 2025-12-28
**Package:** @unrdf/yawl-langchain
**Version:** 1.0.0

---

## Description

LangChain integration for YAWL workflow engine - AI-powered workflow orchestration with RDF context

---

## Capability Atoms

### A67: LangChain Integration

**Runtime:** Node.js
**Invariants:** chain-of-thought, agent-orchestration
**Evidence:** `packages/yawl-langchain/src/index.mjs`



---

## Package Metadata

### Dependencies

- `@langchain/core`: ^0.3.29
- `@langchain/openai`: ^0.3.17
- `@unrdf/kgc-4d`: workspace:*
- `@unrdf/oxigraph`: workspace:*
- `@unrdf/yawl`: workspace:*
- `langchain`: ^0.3.7
- `zod`: ^4.1.13

### Exports

- `.`: `./src/index.mjs`
- `./adapter`: `./src/adapter.mjs`
- `./examples`: `./examples/code-review-workflow.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **LangChain Integration**
   - Import: `import { /* exports */ } from '@unrdf/yawl-langchain'`
   - Use for: LangChain Integration operations
   - Runtime: Node.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/yawl-langchain';

const store = createStore();
// Use yawl-langchain capabilities with store
```


---

## Evidence Trail

- **A67**: `packages/yawl-langchain/src/index.mjs`

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
