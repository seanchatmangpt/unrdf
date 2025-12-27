# KGC Probe Initialization Sequence

## Document Purpose

This document defines the exact order in which KGC Probe components must be created to avoid circular dependencies and ensure deterministic system startup. It serves as the authoritative reference for system bootstrapping.

---

## 1. Initialization Phases Overview

The system initializes in 7 distinct phases, each completing before the next begins:

```
Phase 1: Type Registration      [0ms - 5ms]      No dependencies
Phase 2: Core Utilities         [5ms - 10ms]    No dependencies
Phase 3: Storage Layer          [10ms - 50ms]   Config only
Phase 4: Guard Registry         [50ms - 60ms]   Types only
Phase 5: Agent Registry         [60ms - 70ms]   Types only
Phase 6: Orchestrator           [70ms - 80ms]   Phases 3-5
Phase 7: CLI Commands           [80ms - 100ms]  Phase 6
```

---

## 2. Phase 1: Type Registration

### 2.1 Purpose
Load and initialize all Zod schemas for runtime validation. These are stateless and have no internal dependencies.

### 2.2 Initialization Code

```javascript
// types.mjs - loaded at module import time
import { z } from 'zod';

// All schemas defined and exported synchronously
export const ObservationSchema = z.object({ ... });
export const ArtifactSchema = z.object({ ... });
export const ProbeConfigSchema = z.object({ ... });
// ... etc
```

### 2.3 Dependencies
- External: `zod` package
- Internal: None

### 2.4 Success Criteria
- All schemas parseable (no Zod definition errors)
- Validation functions exported
- No runtime side effects during import

---

## 3. Phase 2: Core Utilities

### 3.1 Purpose
Initialize core utility functions (UUID generation, timestamps, constants).

### 3.2 Initialization Code

```javascript
// Core utilities are inline or imported from Node.js
import { randomUUID } from 'crypto';

// Constants
const PROBE_VERSION = '1.0.0';
const DEFAULT_TIMEOUT_MS = 300000;
const DEFAULT_BATCH_SIZE = 100;

// Utility functions
function generateRunId() {
  return randomUUID();
}

function getTimestamp() {
  return new Date().toISOString();
}
```

### 3.3 Dependencies
- External: Node.js `crypto` module
- Internal: None

### 3.4 Success Criteria
- UUID generation works
- Timestamps in ISO8601 format
- Constants accessible

---

## 4. Phase 3: Storage Layer

### 4.1 Purpose
Create the storage backend based on configuration. This is the first phase that requires runtime configuration.

### 4.2 Initialization Order

```javascript
// Step 3.1: Determine storage type from config
const storageType = config.storageType || 'memory';

// Step 3.2: Create appropriate backend
let storage;
switch (storageType) {
  case 'memory':
    storage = createMemoryStorage();  // No external deps
    break;
  case 'file':
    storage = createFileStorage(config.artifactDir || './artifacts');  // fs dep
    break;
  case 'database':
    // Requires KGC Substrate store
    if (!config.store) {
      throw new Error('Database storage requires config.store');
    }
    storage = createDatabaseStorage({ store: config.store });  // substrate dep
    break;
}

// Step 3.3: Verify storage is functional
await storage.listArtifacts();  // Smoke test
```

### 4.3 Dependencies

| Storage Type | External Dependencies | Internal Dependencies |
|--------------|----------------------|----------------------|
| memory | None | None |
| file | `fs`, `path` | None |
| database | `@unrdf/kgc-substrate`, `@unrdf/oxigraph` | None |

### 4.4 Success Criteria
- Storage instance created without errors
- `listArtifacts()` returns empty array or existing artifacts
- Storage `type` property is set correctly

---

## 5. Phase 4: Guard Registry

### 5.1 Purpose
Create the guard registry and register all 5 default guards.

### 5.2 Initialization Order

```javascript
// Step 4.1: Create empty registry
const guardRegistry = new GuardRegistry();

// Step 4.2: Register default guards (happens in constructor)
// - quality_check
// - completeness_check
// - severity_limit
// - integrity_check
// - agent_coverage

// Step 4.3: Optionally register custom guards
if (config.customGuards) {
  for (const guard of config.customGuards) {
    guardRegistry.register(guard.id, guard);
  }
}

// Step 4.4: Verify all guards registered
const guardIds = guardRegistry.list();
assert(guardIds.length >= 5, 'Default guards must be registered');
```

### 5.3 Dependencies
- Internal: `types.mjs` (for schema validation)
- External: None

### 5.4 Success Criteria
- 5 default guards registered
- `list()` returns all guard IDs
- Each guard's `validate()` function is callable

---

## 6. Phase 5: Agent Registry

### 6.1 Purpose
Create the agent registry and register all 10 default agents.

### 6.2 Initialization Order

```javascript
// Step 5.1: Create empty registry
const agentRegistry = new AgentRegistry();

// Step 5.2: Register default agents (happens in constructor)
// - completion
// - consistency
// - conformance
// - coverage
// - caching
// - completeness
// - coherence
// - clustering
// - classification
// - collaboration

// Step 5.3: Optionally register custom agents
if (config.customAgents) {
  for (const agent of config.customAgents) {
    agentRegistry.register(agent.id, agent);
  }
}

// Step 5.4: Verify all agents registered
const agentIds = agentRegistry.list();
assert(agentIds.length >= 10, 'Default agents must be registered');
```

### 6.3 Dependencies
- Internal: None (agents are self-contained)
- External: `crypto` for UUID generation in observations

### 6.4 Success Criteria
- 10 default agents registered
- `list()` returns all agent IDs
- Each agent's `scan()` function is callable

---

## 7. Phase 6: Orchestrator

### 7.1 Purpose
Create the ProbeOrchestrator, wiring together storage, guards, and agents.

### 7.2 Initialization Order

```javascript
// Step 6.1: Validate all dependencies are ready
assert(storage, 'Storage must be initialized');
assert(guardRegistry, 'GuardRegistry must be initialized');
assert(agentRegistry, 'AgentRegistry must be initialized');

// Step 6.2: Create orchestrator
const orchestrator = new ProbeOrchestrator({
  storage,
  guards: guardRegistry,
  agents: agentRegistry
});

// Step 6.3: Register event listeners (optional)
if (config.eventHandlers) {
  for (const [event, handler] of Object.entries(config.eventHandlers)) {
    orchestrator.on(event, handler);
  }
}

// Step 6.4: Verify orchestrator is ready
assert(orchestrator.storage === storage, 'Storage wired correctly');
assert(orchestrator.guards === guardRegistry, 'Guards wired correctly');
assert(orchestrator.agents === agentRegistry, 'Agents wired correctly');
```

### 7.3 Dependencies
- Internal: Phases 3, 4, 5 (storage, guards, agents)
- External: None

### 7.4 Success Criteria
- Orchestrator created without errors
- All registries correctly wired
- Event emission functional

---

## 8. Phase 7: CLI Commands (Optional)

### 8.1 Purpose
Register probe-specific CLI commands with the KGC CLI registry.

### 8.2 Initialization Order

```javascript
// Step 7.1: Import CLI registry
import { Registry, createEnvelope } from '@unrdf/kgc-cli';

// Step 7.2: Create command envelopes
const probeCommand = createEnvelope({
  name: 'probe',
  description: 'Run integrity scan',
  run: async (args) => {
    const result = await orchestrator.scan({
      universe_id: args.universe,
      agents: args.agents?.split(',')
    });
    return result;
  }
});

// Step 7.3: Register with global registry
Registry.register('probe', probeCommand);
Registry.register('probe:scan', probeCommand);
Registry.register('probe:validate', validateCommand);
Registry.register('probe:diff', diffCommand);

// Step 7.4: Verify commands registered
const commands = Registry.list();
assert(commands.includes('probe'), 'Probe command registered');
```

### 8.3 Dependencies
- Internal: Phase 6 (orchestrator)
- External: `@unrdf/kgc-cli`

### 8.4 Success Criteria
- All probe commands registered
- Commands executable via CLI
- Help text available

---

## 9. Complete Initialization Sequence

### 9.1 Full Bootstrap Function

```javascript
/**
 * Bootstrap the complete KGC Probe system
 *
 * @param {Object} config - Initialization configuration
 * @returns {Promise<{ orchestrator, storage, agents, guards }>}
 */
export async function bootstrap(config = {}) {
  // Phase 1: Types (implicit - loaded at import time)

  // Phase 2: Core Utilities (implicit - loaded at import time)

  // Phase 3: Storage Layer
  const storage = await createStorage(config);

  // Phase 4: Guard Registry
  const guards = createGuardRegistry(config.guardConfig);

  // Phase 5: Agent Registry
  const agents = createAgentRegistry();

  // Phase 6: Orchestrator
  const orchestrator = createProbeOrchestrator({
    storage,
    guards,
    agents
  });

  // Phase 7: CLI (optional)
  if (config.registerCli !== false) {
    await registerCliCommands(orchestrator);
  }

  return { orchestrator, storage, agents, guards };
}

/**
 * Create storage based on config
 */
async function createStorage(config) {
  const type = config.storageType || 'memory';

  switch (type) {
    case 'file':
      return createFileStorage(config.artifactDir);
    case 'database':
      return createDatabaseStorage({ store: config.store });
    default:
      return createMemoryStorage();
  }
}

/**
 * Register CLI commands
 */
async function registerCliCommands(orchestrator) {
  const { Registry } = await import('@unrdf/kgc-cli');
  // Register commands...
}
```

### 9.2 Minimal Initialization (Testing)

```javascript
/**
 * Minimal bootstrap for testing
 */
export function bootstrapMinimal() {
  const storage = createMemoryStorage();
  return createProbeOrchestrator({ storage });
}
```

---

## 10. Initialization Validation

### 10.1 Validation Checks

```javascript
/**
 * Validate system is correctly initialized
 */
function validateInitialization({ orchestrator, storage, agents, guards }) {
  const errors = [];

  // Check storage
  if (!storage || typeof storage.saveArtifact !== 'function') {
    errors.push('Storage not properly initialized');
  }

  // Check guards
  if (!guards || guards.list().length < 5) {
    errors.push('Default guards not registered');
  }

  // Check agents
  if (!agents || agents.list().length < 10) {
    errors.push('Default agents not registered');
  }

  // Check orchestrator wiring
  if (!orchestrator.storage || !orchestrator.guards || !orchestrator.agents) {
    errors.push('Orchestrator not properly wired');
  }

  return {
    valid: errors.length === 0,
    errors
  };
}
```

### 10.2 Smoke Test

```javascript
/**
 * Run smoke test to verify system works
 */
async function smokeTest(orchestrator) {
  try {
    const result = await orchestrator.scan({
      universe_id: 'smoke-test',
      persist: false
    });

    return {
      passed: result.status === 'success' || result.status === 'partial',
      observationCount: result.artifact.observations.length
    };
  } catch (err) {
    return {
      passed: false,
      error: err.message
    };
  }
}
```

---

## 11. Error Recovery

### 11.1 Phase Failure Recovery

| Phase | Failure Mode | Recovery Strategy |
|-------|--------------|-------------------|
| 1 (Types) | Zod parse error | Fatal - fix schema definition |
| 2 (Utils) | crypto unavailable | Use polyfill or fallback |
| 3 (Storage) | File system error | Fall back to memory storage |
| 3 (Storage) | Database connection error | Fall back to memory storage |
| 4 (Guards) | Registration error | Log and continue without guard |
| 5 (Agents) | Registration error | Log and continue without agent |
| 6 (Orchestrator) | Wiring error | Fatal - dependencies missing |
| 7 (CLI) | Registry error | Log and continue without CLI |

### 11.2 Recovery Code Example

```javascript
async function bootstrapWithRecovery(config) {
  let storage;

  // Phase 3 with fallback
  try {
    storage = await createStorage(config);
  } catch (err) {
    console.warn(`Storage creation failed: ${err.message}, using memory`);
    storage = createMemoryStorage();
  }

  // Phase 4 with error logging
  const guards = createGuardRegistry();
  if (guards.list().length < 5) {
    console.warn('Some default guards failed to register');
  }

  // Phase 5 with error logging
  const agents = createAgentRegistry();
  if (agents.list().length < 10) {
    console.warn('Some default agents failed to register');
  }

  // Phase 6 - no recovery, will throw
  const orchestrator = createProbeOrchestrator({ storage, guards, agents });

  // Phase 7 with skip on error
  try {
    await registerCliCommands(orchestrator);
  } catch (err) {
    console.warn(`CLI registration failed: ${err.message}`);
  }

  return { orchestrator, storage, agents, guards };
}
```

---

## 12. Timing Guarantees

### 12.1 Expected Durations

| Phase | Min | Typical | Max | Notes |
|-------|-----|---------|-----|-------|
| 1 | 1ms | 3ms | 10ms | Zod schema compilation |
| 2 | 0ms | 1ms | 5ms | Native functions |
| 3 (memory) | 0ms | 1ms | 5ms | No I/O |
| 3 (file) | 5ms | 20ms | 100ms | Directory creation |
| 3 (database) | 50ms | 200ms | 1000ms | Connection establishment |
| 4 | 1ms | 5ms | 20ms | Guard registration |
| 5 | 1ms | 5ms | 20ms | Agent registration |
| 6 | 0ms | 2ms | 10ms | Object creation |
| 7 | 10ms | 50ms | 200ms | Module import |

### 12.2 Total Initialization Time

| Scenario | Typical | Max |
|----------|---------|-----|
| Memory storage, no CLI | 15ms | 70ms |
| File storage, no CLI | 35ms | 170ms |
| Database storage, with CLI | 260ms | 1280ms |

---

## Document Metadata

| Property | Value |
|----------|-------|
| Version | 1.0.0 |
| Created | 2025-12-27 |
| Status | Authoritative |
| Maintainer | Agent-3 (System Architect) |
