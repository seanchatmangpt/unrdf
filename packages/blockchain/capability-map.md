# Capability Map: @unrdf/blockchain

**Generated:** 2025-12-28
**Package:** @unrdf/blockchain
**Version:** 1.0.0

---

## Description

Blockchain integration for UNRDF - Cryptographic receipt anchoring and audit trails

---

## Capability Atoms

### A43: Blockchain Receipt Anchoring

**Runtime:** Node.js
**Invariants:** merkle-tree, ethereum
**Evidence:** `packages/blockchain/src/anchoring/receipt-anchorer.mjs:9`



---

## Package Metadata

### Dependencies

- `@noble/hashes`: ^1.3.3
- `@unrdf/kgc-4d`: workspace:*
- `@unrdf/yawl`: workspace:*
- `ethers`: ^6.10.0
- `merkletreejs`: ^0.3.11
- `zod`: ^3.22.4

### Exports

- `.`: `./src/index.mjs`
- `./anchoring`: `./src/anchoring/receipt-anchorer.mjs`
- `./contracts`: `./src/contracts/workflow-verifier.mjs`
- `./merkle`: `./src/merkle/merkle-proof-generator.mjs`

---

## Integration Patterns

### Primary Use Cases

1. **Blockchain Receipt Anchoring**
   - Import: `import { /* exports */ } from '@unrdf/blockchain'`
   - Use for: Blockchain Receipt Anchoring operations
   - Runtime: Node.js


### Composition Examples

```javascript
import { createStore } from '@unrdf/oxigraph';
import { /* functions */ } from '@unrdf/blockchain';

const store = createStore();
// Use blockchain capabilities with store
```


---

## Evidence Trail

- **A43**: `packages/blockchain/src/anchoring/receipt-anchorer.mjs:9`

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
