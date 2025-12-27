# KGC Probe System Structure

## Document Purpose

This document defines the complete system structure for the `@unrdf/kgc-probe` package, including the component layout, dependency injection (DI) container design, module initialization patterns, and runtime architecture. It serves as the authoritative reference for all 10 parallel implementation agents.

---

## 1. Architecture Overview

### 1.1 Architectural Layers (9 Layers)

The KGC Probe package follows a strict 9-layer architecture ensuring separation of concerns and testability:

```
Layer 9: CLI Integration     (@unrdf/kgc-cli commands)
Layer 8: API Gateway         (Public exports in index.mjs)
Layer 7: Orchestration       (ProbeOrchestrator, runProbe)
Layer 6: Agents              (10 specialized probe agents)
Layer 5: Guards              (5 validation guards)
Layer 4: Artifact            (Hash, merge, diff, verify)
Layer 3: Storage             (Memory, File, Database)
Layer 2: Types               (Zod schemas, validators)
Layer 1: Core Utilities      (UUID, timestamps, constants)
```

### 1.2 Component Count Summary

| Category | Count | Description |
|----------|-------|-------------|
| Agents | 10 | Specialized integrity scanning agents |
| Guards | 5 | Post-scan validation guards |
| Storage Backends | 3 | Memory, File, Database |
| Artifact Operations | 6 | hash, merge, diff, verify, serialize, deserialize |
| Event Types | 12 | Observable scan lifecycle events |
| Zod Schemas | 10 | Runtime-validated type definitions |

---

## 2. Dependency Injection Container

### 2.1 Container Design Pattern

The DI container uses a **Factory Registry Pattern** with lazy initialization. This ensures:
- No circular dependencies (factories are registered, not instances)
- Testability (mock any factory)
- Configurability (swap implementations at runtime)

```javascript
/**
 * ProbeContainer - Central DI container for all probe components
 *
 * Design: Factory Registry with Lazy Instantiation
 * - Factories registered at module load time
 * - Instances created on first access
 * - Singletons cached for reuse
 */
class ProbeContainer {
  constructor() {
    this.factories = new Map();
    this.singletons = new Map();
    this.config = {};
  }

  // Register a factory function
  register(name, factory, { singleton = false } = {}) {
    this.factories.set(name, { factory, singleton });
  }

  // Resolve a component (creates or returns cached)
  resolve(name) {
    const entry = this.factories.get(name);
    if (!entry) throw new Error(`Unknown component: ${name}`);

    if (entry.singleton && this.singletons.has(name)) {
      return this.singletons.get(name);
    }

    const instance = entry.factory(this);
    if (entry.singleton) {
      this.singletons.set(name, instance);
    }
    return instance;
  }

  // Configure container
  configure(config) {
    this.config = { ...this.config, ...config };
  }
}
```

### 2.2 Standard Registrations

```javascript
// Default container configuration
function createDefaultContainer() {
  const container = new ProbeContainer();

  // Layer 1: Core Utilities
  container.register('uuid', () => crypto.randomUUID);
  container.register('timestamp', () => () => new Date().toISOString());

  // Layer 2: Types (stateless, no registration needed)

  // Layer 3: Storage
  container.register('storage:memory', () => createMemoryStorage(), { singleton: true });
  container.register('storage:file', (c) => createFileStorage(c.config.artifactDir || './artifacts'));
  container.register('storage:database', (c) => createDatabaseStorage({ store: c.config.store }));

  // Layer 4: Artifact Operations (stateless functions, no registration)

  // Layer 5: Guards
  container.register('guardRegistry', () => createGuardRegistry(), { singleton: true });

  // Layer 6: Agents
  container.register('agentRegistry', () => createAgentRegistry(), { singleton: true });

  // Layer 7: Orchestration
  container.register('orchestrator', (c) => createProbeOrchestrator({
    storage: c.resolve(`storage:${c.config.storageType || 'memory'}`),
    guards: c.resolve('guardRegistry'),
    agents: c.resolve('agentRegistry')
  }));

  return container;
}
```

---

## 3. Module Layout

### 3.1 Directory Structure

```
packages/kgc-probe/
  src/
    index.mjs              # Public API (Layer 8)
    probe.mjs              # runProbe convenience function (Layer 7)
    orchestrator.mjs       # ProbeOrchestrator class (Layer 7)
    guards.mjs             # GuardRegistry + 5 guards (Layer 5)
    artifact.mjs           # Hash, merge, diff, verify (Layer 4)
    types.mjs              # Zod schemas (Layer 2)
    agents/
      index.mjs            # AgentRegistry + 10 agents (Layer 6)
    storage/
      index.mjs            # Storage factories (Layer 3)
      memory.mjs           # MemoryStorage implementation
      file.mjs             # FileStorage implementation
      database.mjs         # DatabaseStorage implementation
    cli/
      probe.mjs            # CLI command registration (Layer 9)
      agent.mjs            # Agent subcommands
      guard.mjs            # Guard subcommands
      shard.mjs            # Shard subcommands
```

### 3.2 Module Responsibilities

| Module | Responsibility | Dependencies |
|--------|---------------|--------------|
| `index.mjs` | Public exports, package metadata | All modules |
| `types.mjs` | Zod schemas, validation functions | zod |
| `probe.mjs` | High-level `runProbe()` | orchestrator, storage |
| `orchestrator.mjs` | Scan coordination, events | agents, guards, storage |
| `agents/index.mjs` | Agent registry, 10 agent classes | types |
| `guards.mjs` | Guard registry, 5 guard implementations | types |
| `artifact.mjs` | Deterministic hashing, merge, diff | types, hash-wasm |
| `storage/index.mjs` | Storage factory exports | memory, file, database |

---

## 4. Agent Architecture

### 4.1 Agent Base Class

All 10 agents extend a common base class:

```javascript
/**
 * BaseAgent - Abstract base for all probe agents
 */
class BaseAgent {
  constructor(id, kind, description) {
    this.id = id;
    this.kind = kind;
    this.description = description;
  }

  /**
   * Scan knowledge store and produce observations
   * @param {Object} context - Scan context
   * @param {KnowledgeStore} context.store - KGC Substrate store
   * @param {Object} context.config - Probe configuration
   * @returns {Promise<Observation[]>} Observations found
   */
  async scan(context) {
    throw new Error('BaseAgent.scan must be overridden');
  }

  /**
   * Validate agent is properly configured
   * @returns {{ valid: boolean, errors: string[] }}
   */
  validate() {
    return { valid: true, errors: [] };
  }
}
```

### 4.2 Agent Registry Pattern

```javascript
/**
 * AgentRegistry - Manages agent registration and retrieval
 *
 * Pattern: Registry with default auto-registration
 * - All 10 default agents registered on construction
 * - Custom agents can be added dynamically
 * - Agents retrieved by ID
 */
class AgentRegistry {
  constructor() {
    this.agents = new Map();
    this.registerDefaultAgents();
  }

  registerDefaultAgents() {
    this.register('completion', new CompletionAgent());
    this.register('consistency', new ConsistencyAgent());
    this.register('conformance', new ConformanceAgent());
    this.register('coverage', new CoverageAgent());
    this.register('caching', new CachingAgent());
    this.register('completeness', new CompletenessAgent());
    this.register('coherence', new CoherenceAgent());
    this.register('clustering', new ClusteringAgent());
    this.register('classification', new ClassificationAgent());
    this.register('collaboration', new CollaborationAgent());
  }

  register(id, agent) {
    this.agents.set(id, agent);
  }

  get(id) {
    return this.agents.get(id);
  }

  list() {
    return Array.from(this.agents.keys());
  }
}
```

---

## 5. Storage Layer Abstraction

### 5.1 Storage Interface

All storage backends implement a common interface:

```javascript
/**
 * Storage Interface (implemented by Memory, File, Database)
 * @interface Storage
 */
const StorageInterface = {
  type: 'string',                          // Backend identifier
  saveArtifact: 'async (artifact) => void',
  loadArtifact: 'async (id) => Artifact',
  fetchShards: 'async () => Artifact[]',
  listArtifacts: 'async () => string[]',
  deleteArtifact: 'async (id) => boolean',
};
```

### 5.2 Backend Selection Strategy

```javascript
/**
 * Storage selection strategy (Strategy Pattern)
 *
 * Selection priority:
 * 1. Explicit config: config.storageType
 * 2. Environment: NODE_ENV === 'production' ? 'database' : 'memory'
 * 3. Default: 'memory'
 */
function selectStorage(config) {
  if (config.storage) return config.storage;

  const storageType = config.storageType
    || (process.env.NODE_ENV === 'production' ? 'database' : 'memory');

  switch (storageType) {
    case 'file':
      return createFileStorage(config.artifactDir || './artifacts');
    case 'database':
      return createDatabaseStorage({ store: config.store });
    case 'memory':
    default:
      return createMemoryStorage();
  }
}
```

---

## 6. Event Emission Pattern

### 6.1 Event Emitter Design

The orchestrator implements an Observer pattern for scan lifecycle events:

```javascript
/**
 * Event emission pattern for ProbeOrchestrator
 *
 * Design: Synchronous callbacks with error isolation
 * - Events emitted at key lifecycle points
 * - Errors in listeners logged but don't halt scan
 * - All listeners receive same event data
 */
class EventEmitter {
  constructor() {
    this.listeners = new Map();
  }

  on(event, callback) {
    if (!this.listeners.has(event)) {
      this.listeners.set(event, []);
    }
    this.listeners.get(event).push(callback);
    return () => this.off(event, callback); // Return unsubscribe function
  }

  off(event, callback) {
    const callbacks = this.listeners.get(event);
    if (callbacks) {
      const idx = callbacks.indexOf(callback);
      if (idx !== -1) callbacks.splice(idx, 1);
    }
  }

  emit(event, data) {
    const callbacks = this.listeners.get(event) || [];
    for (const cb of callbacks) {
      try {
        cb(data);
      } catch (err) {
        console.error(`[EventEmitter] Error in ${event} listener:`, err);
      }
    }
  }
}
```

### 6.2 Standard Event Types

See `event-schema.json` for complete event type definitions.

---

## 7. Initialization Order

### 7.1 Boot Sequence

The system follows a strict initialization order to avoid circular dependencies:

```
Phase 1: Type Registration
  - Load Zod schemas (types.mjs)
  - No dependencies

Phase 2: Core Utilities
  - UUID generator
  - Timestamp functions
  - No dependencies

Phase 3: Storage Layer
  - Create storage backend based on config
  - Depends on: config only

Phase 4: Guard Registry
  - Create guard registry
  - Register 5 default guards
  - Depends on: types

Phase 5: Agent Registry
  - Create agent registry
  - Register 10 default agents
  - Depends on: types

Phase 6: Orchestrator
  - Create ProbeOrchestrator
  - Wire storage, guards, agents
  - Depends on: Phases 3-5

Phase 7: CLI (if enabled)
  - Register CLI commands
  - Depends on: Phase 6
```

### 7.2 Lazy vs Eager Initialization

| Component | Strategy | Reason |
|-----------|----------|--------|
| Types/Schemas | Eager | No side effects, needed everywhere |
| Storage | Lazy | May require config/connections |
| Guards | Eager | Lightweight, always needed |
| Agents | Eager | Lightweight, always needed |
| Orchestrator | Lazy | Heavy, may not be used |
| CLI | Lazy | Only needed in CLI context |

---

## 8. Error Handling Strategy

### 8.1 Error Propagation

```
Agent Error → Logged + Added to errors array → Scan continues
Guard Error → Logged + Added to errors array → Scan continues
Storage Error → Logged + Added to errors array → Artifact may not persist
Orchestrator Error → Thrown → Scan terminates
Schema Validation Error → Thrown → Operation fails
```

### 8.2 Error Categories

| Category | Action | Recovery |
|----------|--------|----------|
| Validation | Throw ZodError | Caller must fix input |
| Agent Failure | Log + continue | Other agents complete |
| Guard Failure | Log + continue | Other guards complete |
| Storage Failure | Log + continue | In-memory artifact returned |
| Hash Failure | Throw | Cannot proceed without integrity |

---

## 9. Integration Points

### 9.1 Package Dependencies

```mermaid
graph TD
  KGC_PROBE[@unrdf/kgc-probe] --> V6_CORE[@unrdf/v6-core]
  KGC_PROBE --> KGC_SUBSTRATE[@unrdf/kgc-substrate]
  KGC_PROBE --> OXIGRAPH[@unrdf/oxigraph]
  KGC_PROBE --> HOOKS[@unrdf/hooks]
  KGC_PROBE --> KGC_CLI[@unrdf/kgc-cli]
  KGC_PROBE --> YAWL[@unrdf/yawl]
```

### 9.2 Integration Contracts

See `integration-contracts.md` for detailed API contracts with each dependency.

---

## 10. Thread Safety and Concurrency

### 10.1 Parallel Agent Execution

Agents execute in parallel using `Promise.allSettled()`:

```javascript
// Parallel execution with failure isolation
const agentPromises = agentIds.map(id =>
  this.executeAgent(id, config, observations, errors)
);
await Promise.allSettled(agentPromises);
```

### 10.2 Shared State

| State | Access Pattern | Thread Safety |
|-------|---------------|---------------|
| observations[] | Append-only | Array.push is atomic |
| errors[] | Append-only | Array.push is atomic |
| Storage | Sequential writes | No concurrent mutations |
| Registries | Read-only at runtime | Immutable after init |

---

## 11. Configuration Schema

```javascript
/**
 * Complete configuration schema for KGC Probe
 */
const ProbeConfigSchema = z.object({
  // Required
  universe_id: z.string().describe('Universe to scan'),

  // Optional - Scan scope
  snapshot_id: z.string().optional(),
  agents: z.array(z.string()).optional(),
  guards: z.array(z.string()).optional(),

  // Optional - Behavior
  distributed: z.boolean().default(false),
  persist: z.boolean().default(true),
  timeout_ms: z.number().positive().default(300000),
  batch_size: z.number().positive().default(100),

  // Optional - Storage
  storageType: z.enum(['memory', 'file', 'database']).optional(),
  artifactDir: z.string().optional(),
  store: z.unknown().optional(),  // KGC Substrate store instance
});
```

---

## 12. Testing Strategy

### 12.1 Unit Test Isolation

Each layer is independently testable:

```javascript
// Test agent in isolation
const agent = new CompletionAgent();
const mockContext = { store: mockStore, config: {} };
const observations = await agent.scan(mockContext);
expect(observations).toBeArray();

// Test orchestrator with mock registries
const orchestrator = new ProbeOrchestrator({
  storage: createMemoryStorage(),
  agents: mockAgentRegistry,
  guards: mockGuardRegistry,
});
```

### 12.2 Integration Test Points

| Test | Components | Purpose |
|------|------------|---------|
| Full scan | All | End-to-end validation |
| Agent isolation | Single agent | Agent-specific logic |
| Guard isolation | Single guard | Guard-specific logic |
| Storage round-trip | Storage + artifact | Persistence correctness |
| Event emission | Orchestrator | Observability correctness |

---

## Document Metadata

| Property | Value |
|----------|-------|
| Version | 1.0.0 |
| Created | 2025-12-27 |
| Status | Authoritative |
| Maintainer | Agent-3 (System Architect) |
