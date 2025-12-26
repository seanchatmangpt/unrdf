# Agent 1: Orchestrator & Integrator - Implementation Plan

## Mission
Create the integration framework that orchestrates Agents 2-10, providing a unified public API for all AUTONOMIC_INNOVATION primitives.

## Files to Create

### 1. Core Integration Files

#### `./agent-1/index.mjs`
**Purpose**: Central orchestration module - imports and re-exports all 9 agent modules
**Exports**:
- `AGENT_STATUS` - Integration status enum
- `VERSION` - Framework version string
- `getAgentExports(agentId)` - Dynamic agent export retrieval
- `validateIntegration()` - Check all agents are available
- Re-exports from all agents (Agent 2-10)

**Integration Hooks**:
- Dynamic imports with fallback for missing agents
- Status tracking (AVAILABLE, STUB, ERROR)
- Circular dependency detection

#### `./agent-1/constants.mjs`
**Purpose**: Shared constants across all agents
**Exports**:
- `AGENT_IDS` - List of all agent identifiers (AGENT_2...AGENT_10)
- `AGENT_NAMES` - Human-readable agent names
- `REQUIRED_EXPORTS` - Map of agentId → required export names
- `VERSION_INFO` - Semantic version tracking

#### `./agent-1/types.mjs`
**Purpose**: JSDoc type definitions for integration contracts
**Exports**:
- Type definitions for all agent interfaces
- Integration status types
- Common data structures (Capsule, Lens, ImpactSet, etc.)

#### `./agent-1/test.mjs`
**Purpose**: Integration validation tests
**Tests**:
- All 9 agent exports are importable
- Public API completeness
- No circular dependencies
- Determinism proof (demo runs twice with hash equality)

### 2. Public API

#### `./src/index.mjs`
**Purpose**: Single import path for all innovations
**Exports**: (Organized by agent)
- **Agent 2 (Capsules)**: `planCapsule`, `applyCapsule`, `verifyCapsule`, `canonicalize`, `hashCapsule`
- **Agent 3 (Lenses)**: `defineLens`, `compileLens`, `executeLensToGraph`, `executeLensFromGraph`
- **Agent 4 (Impact Sets)**: `computeImpactSet`
- **Agent 5 (Commutativity)**: `canReorder`, `conflictCertificate`
- **Agent 6 (Conventions)**: `compileProfile`, `validateAgainstProfile`, `diagnosticReport`
- **Agent 7 (Generator)**: `generateFacade`
- **Agent 8 (Store)**: `atomicApply`, `replayFromReceipt`
- **Agent 9 (Shadow)**: `shadowWrite`, `shadowRead`, `partialServe`, `mismatchReport`
- **Agent 10 (Quality)**: `runQualityGates`, `e2eValidate`

### 3. Execution & Documentation

#### `./package.json`
**Purpose**: Package configuration and scripts
**Configuration**:
- `"type": "module"` - ES modules
- No new dependencies (use workspace packages only)
**Scripts**:
- `test` - Run full test suite
- `test:fast` - Quick integration check
- `test:determinism` - Prove determinism (two runs → same hash)
- `demo` - Run master demonstration
**Dependencies** (from workspace):
- `@unrdf/core`
- `@unrdf/oxigraph`
- `@unrdf/kgc-4d`

#### `./RUNBOOK.md`
**Purpose**: Copy/paste ready execution commands
**Sections**:
- Quick Start (3 commands)
- Test Suite (`pnpm test:autonomic`)
- Demo (`pnpm demo:autonomic`)
- Determinism Validation (`pnpm test:determinism`)
- Each command with expected output

#### `./demo.mjs`
**Purpose**: Master demonstration exercising ALL 10 agent primitives
**Requirements**:
- Zero external dependencies
- Runs locally
- Deterministic output (two runs → identical)
**Output Sections** (clearly labeled):
1. Capsule planning & hashing
2. Lens compilation & application
3. Diff impact sets
4. Commutativity checks with witness
5. Conventions profile validation
6. Generated façade code
7. Store atomic apply
8. Shadow mode mismatches
9. OTEL-style receipt hashes
10. Quality gates validation

## Integration Contract

### Agent Exports (Expected from Agents 2-10)

```javascript
// Agent 2 (Capsules)
export { planCapsule, applyCapsule, verifyCapsule, canonicalize, hashCapsule }

// Agent 3 (Lenses)
export { defineLens, compileLens, executeLensToGraph, executeLensFromGraph }

// Agent 4 (Impact Sets)
export { computeImpactSet }

// Agent 5 (Commutativity)
export { canReorder, conflictCertificate }

// Agent 6 (Conventions)
export { compileProfile, validateAgainstProfile, diagnosticReport }

// Agent 7 (Generator)
export { generateFacade }

// Agent 8 (Store)
export { atomicApply, replayFromReceipt }

// Agent 9 (Shadow)
export { shadowWrite, shadowRead, partialServe, mismatchReport }

// Agent 10 (Quality)
export { runQualityGates, e2eValidate }
```

### Integration Hooks

#### Dynamic Import Pattern
```javascript
async function loadAgent(agentId) {
  try {
    const module = await import(`../agent-${agentId}/index.mjs`);
    return { status: 'AVAILABLE', exports: module };
  } catch (err) {
    return { status: 'STUB', exports: createStub(agentId), error: err.message };
  }
}
```

#### Validation Pattern
```javascript
function validateIntegration() {
  const results = [];
  for (const agentId of AGENT_IDS) {
    const agent = getAgentExports(agentId);
    const requiredExports = REQUIRED_EXPORTS[agentId];
    const missing = requiredExports.filter(name => !agent[name]);
    results.push({ agentId, status: agent.status, missing });
  }
  return results;
}
```

## Run Commands Structure

### Test Suite
```bash
# Full integration test (timeout: 5s)
timeout 5s pnpm test:autonomic

# Expected output:
# ✅ Agent 2-10 imports successful
# ✅ Public API complete (45 primitives)
# ✅ No circular dependencies
# ✅ Integration validation passed
```

### Demo
```bash
# Master demonstration (timeout: 5s)
timeout 5s pnpm demo:autonomic

# Expected output:
# [CAPSULE] Planning: {...}, Hash: abc123...
# [LENS] Compiled: {...}, Applied: 5 triples
# [IMPACT] Affected: [uri1, uri2, uri3]
# [COMMUTE] Can reorder: true, Witness: {...}
# [CONVENTIONS] Valid: true, Diagnostics: []
# [GENERATOR] Generated: 150 lines
# [STORE] Applied: 10 ops, Receipt: def456...
# [SHADOW] Mismatches: 0
# [QUALITY] Gates passed: 8/8
```

### Determinism
```bash
# Prove determinism (timeout: 10s)
timeout 10s pnpm test:determinism

# Expected output:
# Run 1 hash: abc123...
# Run 2 hash: abc123...
# ✅ Determinism verified
```

## Integration Checklist

- [ ] All 9 agents export an `index.mjs` with clear exports
- [ ] No file overlap between agents except in `src/` and `test/`
- [ ] Public API in `./src/index.mjs` is single import path
- [ ] `RUNBOOK.md` has exact commands (copy/paste ready)
- [ ] `demo.mjs` covers ALL 10 agent outputs
- [ ] `package.json` uses only existing workspace deps
- [ ] Determinism is provable (two runs → same hashes)
- [ ] All imports use `.mjs` extension
- [ ] JSDoc type coverage 100%
- [ ] Zero external network calls in demo

## Success Criteria

1. **Importability**: `import * as autonomic from './src/index.mjs'` succeeds
2. **Completeness**: All 45+ primitives accessible from public API
3. **Determinism**: `node demo.mjs | sha256sum` produces same hash twice
4. **Performance**: All tests complete in <5s
5. **Independence**: Demo runs with zero external dependencies

## Dependency Graph

```
agent-1/index.mjs
  ├─> agent-2/index.mjs (Capsules)
  ├─> agent-3/index.mjs (Lenses)
  ├─> agent-4/index.mjs (Impact Sets)
  ├─> agent-5/index.mjs (Commutativity)
  ├─> agent-6/index.mjs (Conventions)
  ├─> agent-7/index.mjs (Generator)
  ├─> agent-8/index.mjs (Store)
  ├─> agent-9/index.mjs (Shadow)
  └─> agent-10/index.mjs (Quality)

src/index.mjs
  └─> agent-1/index.mjs (re-exports all)

demo.mjs
  └─> src/index.mjs (uses public API)

test.mjs
  ├─> agent-1/index.mjs (integration validation)
  └─> demo.mjs (determinism check)
```

## Notes

- All agents operate independently (no cross-dependencies)
- Agent 1 is the ONLY integration point
- Dynamic imports allow partial availability
- Stubs prevent cascade failures
- Hash-based determinism proof is non-negotiable
