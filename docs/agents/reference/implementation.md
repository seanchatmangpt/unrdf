# Implementation Reference

**Complete mapping of agent documentation to actual codebase implementation**

This document provides direct references to implementation files, enabling agents to navigate from documentation to code.

> **Machine-Executable Version:** See [implementation.mjs](./implementation.mjs) for programmatic access to implementation discovery and navigation.

```json
{
  "@context": {
    "@vocab": "urn:unrdf:implementation:",
    "schema": "http://schema.org/"
  },
  "@id": "urn:unrdf:implementation-reference:v4.0.0",
  "@type": "implementation:Reference",
  "version": "4.0.0",
  "targetAudience": ["AutonomicAgent", "HyperintelligentSystem"]
}
```

---

## MAPEK Implementation

### Core MAPEK Loop

**Documentation:** [Tutorial 02: MAPEK Integration](./tutorials/02-mapek-integration.mjs)  
**Implementation:** `src/project-engine/autonomic-mapek.mjs`

**Key Functions:**

| Function | Line Range | Purpose |
|----------|------------|---------|
| `runMapekIteration` | 47-280 | Single MAPEK cycle execution |
| `createAutonomicHooks` | 289-395 | Create hooks from MAPEK findings |
| `runContinuousMapekLoop` | 407-461 | Continuous loop until convergence |
| `reportMapekStatus` | 469-507 | Human-readable status report |

**Phase Implementations:**

- **Monitor Phase** (lines 76-99): Calls `findMissingRoles`, `auditTypeConsistency`, `analyzeHotspots`, `computeDrift`
- **Analyze Phase** (lines 101-136): Calculates gapScore, typeScore, hotspotScore, overallHealth
- **Plan Phase** (lines 138-198): Generates decisions with autoFixable flags
- **Execute Phase** (lines 200-234): Queues actions for auto-fixable decisions
- **Knowledge Phase** (lines 236-264): Extracts learnings and patterns

### Supporting Modules

**Gap Finder:**
- **Implementation:** `src/project-engine/gap-finder.mjs`
- **Exports:** `findMissingRoles`, `scoreMissingRole`
- **Used by:** MAPEK Monitor phase

**Type Auditor:**
- **Implementation:** `src/project-engine/type-auditor.mjs`
- **Exports:** `auditTypeConsistency`, `compareTypes`
- **Used by:** MAPEK Monitor phase

**Hotspot Analyzer:**
- **Implementation:** `src/project-engine/hotspot-analyzer.mjs`
- **Exports:** `analyzeHotspots`, `scoreFeature`
- **Used by:** MAPEK Monitor phase

**Drift Snapshot:**
- **Implementation:** `src/project-engine/drift-snapshot.mjs`
- **Exports:** `computeDrift`, `createStructureSnapshot`
- **Used by:** MAPEK Monitor phase

### Project Engine Index

**Entry Point:** `src/project-engine/index.mjs`

**Exports:**
```javascript
// MAPEK
export {
  runMapekIteration,
  createAutonomicHooks,
  runContinuousMapekLoop,
  reportMapekStatus,
} from './autonomic-mapek.mjs';

// Supporting tools
export { findMissingRoles, scoreMissingRole } from './gap-finder.mjs';
export { auditTypeConsistency, compareTypes } from './type-auditor.mjs';
export { analyzeHotspots, scoreFeature } from './hotspot-analyzer.mjs';
```

---

## Knowledge Hooks Implementation

### Hook Definition

**Documentation:** [Tutorial 03: Knowledge Hook Creation](./tutorials/03-knowledge-hook-creation.mjs)  
**Implementation:** `src/knowledge-engine/define-hook.mjs`

**Key Functions:**

| Function | Line Range | Purpose |
|----------|------------|---------|
| `defineHook` | 126-127 | Main hook definition function |
| `createKnowledgeHook` | (from schemas.mjs) | Validation and normalization |

**Validation:**
- **Schemas:** `src/knowledge-engine/schemas.mjs`
- **Security:** `src/knowledge-engine/security-validator.mjs`

### Hook Management

**Documentation:** [Reference: API Contracts](./reference/api-contracts.json)  
**Implementation:** `src/knowledge-engine/knowledge-hook-manager.mjs`

**Key Methods:**

| Method | Line Range | Purpose |
|--------|------------|---------|
| `registerHook` | ~100-150 | Register hook with manager |
| `removeHook` | ~150-160 | Remove hook by name |
| `executeKnowledgeHook` | 171-187 | Execute specific hook |
| `executeAllHooks` | ~190-220 | Execute all matching hooks |

**Base Class:** `TransactionManager` (extends for transaction integration)

### Hook Execution

**Implementation:** `src/knowledge-engine/hook-executor.mjs`

**Key Functions:**

| Function | Line Range | Purpose |
|----------|------------|---------|
| `executeHook` | 28-84 | Main execution entry point |
| `_executeHookPhases` | 156-373 | Execute before/run/after phases |
| `evaluateCondition` | (internal) | Evaluate SPARQL/SHACL conditions |

**Lifecycle Flow:**
1. **Before Phase** (lines 240-270): Pre-condition gate, can cancel
2. **Condition Evaluation** (internal): SPARQL ASK/SELECT or SHACL
3. **Run Phase** (lines 272-300): Core effect execution
4. **After Phase** (lines 302-335): Post-execution cleanup/auditing

### Hook Schemas

**Implementation:** `src/knowledge-engine/schemas.mjs`

**Exported Schemas:**
- `HookMetaSchema` - Hook metadata validation
- `FileRefSchema` - Content-addressed file references
- `ConditionSchema` - Condition validation (discriminated union)
- `HookEventSchema` - Event validation
- `HookResultSchema` - Result validation

**Validation Library:** Zod (`z`)

---

## System Core Implementation

### Dark Matter Core

**Documentation:** [How-To: Discover Capabilities](./how-to/01-discover-capabilities.mjs)  
**Implementation:** `src/knowledge-engine/dark-matter-core.mjs`

**Key Class:** `KnowledgeSubstrateCore`

**Key Methods:**

| Method | Purpose |
|--------|---------|
| `query` | Execute SPARQL queries |
| `executeTransaction` | Execute ACID transactions |
| `executeHook` | Execute knowledge hooks |
| `registerHook` | Register hooks with system |
| `getComponent` | Get system component (e.g., 'knowledgeHookManager') |

**Initialization:**
- **Entry Point:** `createDarkMatterCore()` function
- **Location:** `src/knowledge-engine/index.mjs` or `src/knowledge-engine/dark-matter-core.mjs`

### Transaction Manager

**Implementation:** `src/knowledge-engine/transaction.mjs`

**Key Class:** `TransactionManager`

**Transaction States:**
```
PENDING → ACTIVE → HOOKS_PASSED → APPLYING → POST_HOOKS → COMMITTED
           ↓                              ↓
        ROLLING_BACK ←───────────────────┘
```

**Hook Integration:**
- Pre-hooks execute in `ACTIVE` state
- Post-hooks execute in `POST_HOOKS` state
- Delta hooks execute on delta graph

---

## Project Initialization

### Initialization Pipeline

**Implementation:** `src/project-engine/initialize.mjs`

**Key Function:** `createProjectInitializationPipeline`

**Phases:**

| Phase | Line Range | Purpose |
|-------|------------|---------|
| Phase 1: Scan | ~100-150 | Scan filesystem to RDF |
| Phase 2: Stack Detection | ~150-200 | Detect framework stack |
| Phase 3: Project Model | ~200-250 | Build project structure |
| Phase 4: File Roles | ~250-300 | Classify file roles |
| Phase 5: Domain Inference | ~300-350 | Infer domain model |
| Phase 6.5: Code Complexity | ~570-615 | Analyze JS/TS complexity |
| Phase 6: Template Inference | ~350-400 | Infer templates |
| Phase 7: Hooks | ~400-450 | Register hooks |
| Phase 8: Snapshot | ~450-500 | Create baseline |

**Code Complexity Integration:**
- **Implementation:** `src/project-engine/code-complexity-js.mjs`
- **Function:** `analyzeJsComplexity`
- **Phase:** 6.5 (after domain inference, before template inference)
- **Capability:** `CODE_COMPLEXITY_JS` (from `capabilities-manifest.mjs`)

### Capabilities Manifest

**Implementation:** `src/project-engine/capabilities-manifest.mjs`

**Exports:**
- `CODE_COMPLEXITY_JS` - Code complexity capability metadata
- `CAPABILITIES` - Array of all capabilities
- `FEATURE_FLAGS` - Feature flag registry
- `isCapabilityEnabled()` - Check if capability enabled
- `getCapabilityMetadata()` - Get capability metadata
- `getEnabledCapabilities()` - Get all enabled capabilities
- `setCapabilityEnabled()` - Enable/disable capability

---

## Entry Points

### Main Entry

**Path:** `src/index.mjs`  
**Export:** `unrdf` (package.json `"."`)

**Exports:**
- Core composables (`useGraph`, `useTurtle`, etc.)
- Knowledge engine components
- Utilities

### Knowledge Engine Entry

**Path:** `src/knowledge-engine/index.mjs`  
**Export:** `unrdf/knowledge-engine`

**Exports:**
- `KnowledgeHookManager`
- `TransactionManager`
- `defineHook` (via hook-management.mjs)
- `createDarkMatterCore`
- Schemas and validators

### Project Engine Entry

**Path:** `src/project-engine/index.mjs`  
**Export:** `unrdf/project-engine`

**Exports:**
- MAPEK functions (`runMapekIteration`, etc.)
- Supporting tools (gap finder, type auditor, etc.)
- Initialization pipeline
- Capabilities manifest

### CLI Entry

**Path:** `src/cli/index.mjs`  
**Export:** `unrdf/cli`

**Commands:**
- `init` - Initialize project
- `autonomic` - Run MAPEK loop
- `store` - Store operations

---

## Test Files

### MAPEK Tests

**Location:** `test/project-engine/`

**Key Files:**
- `mapek-consolidated.test.mjs` - MAPEK integration tests
- `autonomic-mapek.test.mjs` - Autonomic loop tests

### Knowledge Hooks Tests

**Location:** `test/knowledge-engine/`

**Key Files:**
- `define-hook.test.mjs` - Hook definition tests
- `knowledge-hook-manager.test.mjs` - Manager tests
- `hook-executor.test.mjs` - Execution tests

### Project Initialization Tests

**Location:** `test/project-engine/`

**Key Files:**
- `initialize.test.mjs` - Pipeline tests
- `initialize-with-complexity.test.mjs` - Complexity integration tests
- `capabilities-manifest.test.mjs` - Capability tests

---

## Schema and Validation

### Zod Schemas

**Location:** `src/knowledge-engine/schemas.mjs`

**Schemas:**
- `HookMetaSchema` - Hook metadata
- `FileRefSchema` - Content-addressed references
- `ConditionSchema` - Condition types (discriminated union)
- `HookEventSchema` - Event structure
- `HookResultSchema` - Result structure

### SHACL Validation

**Location:** `src/knowledge-engine/` (via `rdf-validate-shacl`)

**Usage:** SHACL shapes for hook validation

### Ontologies

**Location:** `src/ontologies/`

**Files:**
- `unmetric-ontology.mjs` - Code metrics ontology (named nodes)
- `unmetric-ontology.ttl` - Code metrics ontology (Turtle)

---

## Error Handling

### Error Codes

**Documentation:** [Reference: Error Codes](./reference/error-codes.json)  
**Implementation:** Error codes defined in:
- `src/knowledge-engine/schemas.mjs` - Validation errors
- `src/project-engine/autonomic-mapek.mjs` - MAPEK errors
- `src/knowledge-engine/knowledge-hook-manager.mjs` - Hook manager errors

### Error Recovery

**Implementation:** Error recovery logic in:
- `src/knowledge-engine/hook-executor.mjs` - Hook execution errors
- `src/knowledge-engine/transaction.mjs` - Transaction rollback
- `src/knowledge-engine/security-validator.mjs` - Security validation errors

---

## Performance Monitoring

### OpenTelemetry Integration

**Implementation:** `src/knowledge-engine/observability.mjs`

**Spans Created:**
- `unrdf.transaction.execute`
- `unrdf.hooks.pre.evaluate`
- `unrdf.hooks.post.execute`
- `unrdf.hook.{hookName}`

### Metrics

**SLOs Defined:**
- Hook registration: p99 < 1ms
- Condition evaluation: p99 < 2ms
- Hook execution: p99 < 10ms
- MAPEK iteration: p99 < 200ms

**Measurement:** Via OpenTelemetry spans

---

## Quick Reference Map

### From Documentation to Code

| Documentation | Implementation File | Key Function/Class |
|---------------|---------------------|-------------------|
| [Tutorial 02: MAPEK](./tutorials/02-mapek-integration.mjs) | `src/project-engine/autonomic-mapek.mjs` | `runMapekIteration` |
| [Tutorial 03: Hooks](./tutorials/03-knowledge-hook-creation.mjs) | `src/knowledge-engine/define-hook.mjs` | `defineHook` |
| [How-To: Discover](./how-to/01-discover-capabilities.mjs) | `src/knowledge-engine/dark-matter-core.mjs` | `KnowledgeSubstrateCore` |
| [Reference: API](./reference/api-contracts.json) | `src/knowledge-engine/knowledge-hook-manager.mjs` | `KnowledgeHookManager` |
| [Explanation: Architecture](./explanation/architecture.md) | `src/project-engine/autonomic-mapek.mjs` | All phases |

### From Code to Documentation

| Implementation File | Documentation | Section |
|-------------------|---------------|---------|
| `src/project-engine/autonomic-mapek.mjs` | [Tutorial 02](./tutorials/02-mapek-integration.mjs) | MAPEK Integration |
| `src/knowledge-engine/define-hook.mjs` | [Tutorial 03](./tutorials/03-knowledge-hook-creation.mjs) | Hook Creation |
| `src/knowledge-engine/knowledge-hook-manager.mjs` | [Reference: API](./reference/api-contracts.json) | Hook Management |
| `src/knowledge-engine/hook-executor.mjs` | [Explanation: Architecture](./explanation/architecture.md) | Hook Lifecycle |
| `src/project-engine/code-complexity-js.mjs` | [Reference: Capabilities](./reference/capabilities.jsonld) | Code Complexity |

---

## Implementation Patterns

### Content-Addressed Conditions

**Pattern:** Store conditions as files with SHA-256 hashes

**Implementation:**
- **Definition:** `src/knowledge-engine/define-hook.mjs` (lines 31-40)
- **Validation:** `src/knowledge-engine/schemas.mjs` (`FileRefSchema`)
- **Usage:** `src/knowledge-engine/hook-executor.mjs` (condition evaluation)

### MAPEK → Hooks Integration

**Pattern:** MAPEK creates hooks, hooks execute fixes

**Implementation:**
- **MAPEK:** `src/project-engine/autonomic-mapek.mjs` (`createAutonomicHooks`, lines 289-395)
- **Registration:** `src/knowledge-engine/knowledge-hook-manager.mjs` (`registerHook`)
- **Execution:** `src/knowledge-engine/hook-executor.mjs` (`executeHook`)

### Transaction Integration

**Pattern:** Hooks execute during ACID transactions

**Implementation:**
- **Transaction:** `src/knowledge-engine/transaction.mjs` (`TransactionManager`)
- **Hook Manager:** `src/knowledge-engine/knowledge-hook-manager.mjs` (extends `TransactionManager`)
- **Execution:** `src/knowledge-engine/hook-executor.mjs` (called from transaction lifecycle)

---

## Version Information

- **UNRDF Version:** 4.0.0
- **Documentation Version:** 4.0.0
- **Last Updated:** 2024-01-01
- **Implementation Language:** JavaScript (ESM `.mjs`)
- **Validation:** Zod schemas
- **RDF Library:** N3.js

---

## Machine-Executable Reference

**File:** [implementation.mjs](./implementation.mjs)

The machine-executable reference provides programmatic access to implementation discovery:

```javascript
import {
  getImplementation,
  getImplementationPath,
  readImplementation,
  getFunctionImplementation,
  discoverImplementations
} from './reference/implementation.mjs';

// Get implementation details
const mapekImpl = getImplementation('mapek', 'core');

// Read actual code
const code = await readImplementation('mapek', 'core', {
  lineStart: 47,
  lineEnd: 100
});

// Get function implementation
const func = await getFunctionImplementation('mapek', 'core', 'runMapekIteration');

// Discover all implementations
const discovery = await discoverImplementations();
```

**Available Functions:**
- `getImplementation(concept, subconcept)` - Get implementation registry entry
- `getImplementationPath(concept, subconcept)` - Get absolute file path
- `readImplementation(concept, subconcept, options)` - Read file content (optionally line range)
- `getFunctionImplementation(concept, subconcept, functionName)` - Get function code with metadata
- `listImplementations()` - List complete registry
- `searchImplementations(keyword)` - Search by keyword
- `verifyImplementation(concept, subconcept)` - Verify file exists
- `getImplementationMetadata(concept, subconcept)` - Get file stats and metadata
- `discoverImplementations(concept)` - Complete discovery with verification

## Next Steps for Agents

1. **Use Machine-Executable Reference:** Import `implementation.mjs` for programmatic access
2. **Read Implementation Files:** Start with entry points (`index.mjs` files)
3. **Follow Function Calls:** Trace from documentation examples to implementation
4. **Check Tests:** Test files show usage patterns
5. **Reference Schemas:** Zod schemas define exact contracts
6. **Query Graph:** Use SPARQL to discover capabilities at runtime

