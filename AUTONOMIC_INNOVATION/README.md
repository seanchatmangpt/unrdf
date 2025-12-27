# AUTONOMIC_INNOVATION

**10 Swarm Agents Building Graph Innovations in Parallel**

## Overview

AUTONOMIC_INNOVATION is a fully autonomous, multi-agent system where 10 specialized agents work in parallel to build innovative RDF/graph primitives. Agent 1 (Orchestrator) provides the integration framework, while Agents 2-10 implement specific innovations.

## Architecture

```
Agent 1: Orchestrator & Integrator (this framework)
Agent 2: Capsules (content-addressed RDF operations)
Agent 3: Lenses (bidirectional graph transformations)
Agent 4: Impact Sets (dependency tracking)
Agent 5: Commutativity (operation reordering with witnesses)
Agent 6: Conventions (profile validation)
Agent 7: Generator (façade code generation)
Agent 8: Store (atomic operations with receipts)
Agent 9: Shadow (partial serving with mismatch detection)
Agent 10: Quality (gates & E2E validation)
```

## Quick Start

```bash
# Run master demonstration
pnpm demo:autonomic

# Run integration tests
pnpm test:autonomic

# Verify determinism
pnpm test:determinism
```

## Public API

Single import path for all innovations:

```javascript
import {
  // Agent 2: Capsules
  planCapsule, applyCapsule, verifyCapsule, canonicalize, hashCapsule,

  // Agent 3: Lenses
  defineLens, compileLens, executeLensToGraph, executeLensFromGraph,

  // Agent 4: Impact Sets
  computeImpactSet,

  // Agent 5: Commutativity
  canReorder, conflictCertificate,

  // Agent 6: Conventions
  compileProfile, validateAgainstProfile, diagnosticReport,

  // Agent 7: Generator
  generateFacade,

  // Agent 8: Store
  atomicApply, replayFromReceipt,

  // Agent 9: Shadow
  shadowWrite, shadowRead, partialServe, mismatchReport,

  // Agent 10: Quality
  runQualityGates, e2eValidate,

  // Integration utilities
  validateIntegration, getIntegrationStatus,
} from '@unrdf/autonomic-innovation';
```

## Features

- ✅ **Fully Autonomous**: No questions, complete execution
- ✅ **Parallel Development**: 10 agents work independently
- ✅ **Graceful Degradation**: Stubs for missing agents
- ✅ **Deterministic**: Same input → same output (provable)
- ✅ **Zero External Deps**: Self-contained demonstrations
- ✅ **Integration Framework**: Clean contracts between agents

## Documentation

- **[RUNBOOK.md](./RUNBOOK.md)**: Commands and usage
- **[agent-1/PLAN.md](./agent-1/PLAN.md)**: Integration plan & contracts
- **[agent-1/types.mjs](./agent-1/types.mjs)**: Type definitions

## Development

Each agent (2-10) must:

1. Create `./agent-X/index.mjs` with required exports
2. Follow contracts defined in `agent-1/PLAN.md`
3. Provide JSDoc types for all exports
4. Maintain <5s execution time
5. Ensure deterministic behavior

## Performance Targets

| Command | Target | Timeout |
|---------|--------|---------|
| `pnpm demo` | <3s | 5s |
| `pnpm test` | <2s | 5s |
| `pnpm test:determinism` | <6s | 10s |

## Integration Status

Check current agent availability:

```javascript
import { getIntegrationStatus } from '@unrdf/autonomic-innovation';
const status = await getIntegrationStatus();
console.log(`Available: ${status.available}/${status.total} agents`);
```

## Testing

```bash
# Full test suite
pnpm test

# Fast tests (development)
pnpm test:fast

# Determinism proof
pnpm test:determinism
```

## License

MIT

## Version

0.1.0
