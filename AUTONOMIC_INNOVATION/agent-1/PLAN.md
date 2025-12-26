# Agent 1: Orchestrator / Integrator

**Role**: Coordinate all 10 agents, compose outputs into unified public API, ensure determinism

## Files to Create

### Integration Modules
- `./AUTONOMIC_INNOVATION/src/index.mjs` - Main public API export
- `./AUTONOMIC_INNOVATION/RUNBOOK.md` - Runnable commands guide
- `./AUTONOMIC_INNOVATION/demo.mjs` - Full end-to-end demo

### Integration Supervision
- `./AUTONOMIC_INNOVATION/test/integration.test.mjs` - Cross-agent integration tests
- `./AUTONOMIC_INNOVATION/examples/facade-demo.mjs` - Facade generation example
- `./AUTONOMIC_INNOVATION/examples/capsule-workflow.mjs` - Capsule planning/apply workflow

### Shared Utilities
- `./AUTONOMIC_INNOVATION/src/shared/determinism.mjs` - Determinism enforcement utilities
- `./AUTONOMIC_INNOVATION/src/shared/canonical-order.mjs` - Canonical ordering functions
- `./AUTONOMIC_INNOVATION/src/shared/hash-receipt.mjs` - Receipt formatting

## Exports to Provide

```javascript
export { planCapsule, verifyCapsule } from '../agent-2/src/index.mjs';
export { compileLens, defineLens } from '../agent-3/src/index.mjs';
export { computeImpactSet } from '../agent-4/src/index.mjs';
export { canReorder, conflictCertificate } from '../agent-5/src/index.mjs';
export { compileProfile } from '../agent-6/src/index.mjs';
export { generateFacade } from '../agent-7/src/index.mjs';
export { applyCapsule, createAtomicStore } from '../agent-8/src/index.mjs';
export { shadowWrite, shadowRead, partialServe } from '../agent-9/src/index.mjs';
export { runQualityGates, validateDeterminism } from '../agent-10/src/index.mjs';
```

## Tests to Add

1. **Integration Test**: All agents working together
2. **Determinism Audit**: Run demo twice, compare hashes
3. **Import Verification**: All exports correctly wired
4. **Cross-Module Types**: JSDoc consistency

## Determinism Enforcement

- All file writes use `canonicalSort()` for stable ordering
- All hashes use `hash-wasm` with deterministic serialization
- Demo runs twice with output comparison
- RUNBOOK includes `--deterministic` flag for all tests

## Dependencies

- Re-exports from agents 2-10
- `hash-wasm` for deterministic hashing
- `@unrdf/kgc-4d` for receipt generation
- `@unrdf/oxigraph` for store operations

## Demo Hook Points

1. Generate Conventions Profile
2. Create Customer Lens
3. Define intent operation
4. Plan Capsule
5. Apply to store
6. Compute impact set
7. Check commutativity with second capsule
8. Generate façade
9. Run shadow mode
10. Verify determinism by re-running
11. Print all hashes and receipts

## Success Criteria

- [ ] All 9 agents submit implementations
- [ ] Main `index.mjs` exports all public APIs
- [ ] Demo runs end-to-end with stable hashes
- [ ] RUNBOOK has exact commands for tests + demo
- [ ] Two identical runs produce identical hashes
