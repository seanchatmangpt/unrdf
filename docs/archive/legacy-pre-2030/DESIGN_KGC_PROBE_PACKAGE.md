# KGC Probe Package - SPARC Pseudocode Design

## Executive Summary

@unrdf/kgc-probe provides automated knowledge graph integrity scanning through 10 specialized agents with deterministic artifact generation, guard-based validation, and pluggable storage backends.

**Design Principles**:
- Pure functions with immutable data structures
- Deterministic shard merging via cryptographic hashing
- Zod schema validation at boundaries
- ESM-only distribution with tree-shaking optimization
- No external runtime dependencies (only @unrdf packages)

---

## PHASE 1: Package Configuration

### Package.json Specification

```json
{
  "name": "@unrdf/kgc-probe",
  "version": "1.0.0",
  "description": "KGC Probe - Automated knowledge graph integrity scanning with 10 agents and artifact validation",
  "type": "module",
  "main": "./src/index.mjs",
  "exports": {
    ".": "./src/index.mjs",
    "./orchestrator": "./src/orchestrator.mjs",
    "./guards": "./src/guards.mjs",
    "./agents": "./src/agents/index.mjs",
    "./storage": "./src/storage/index.mjs",
    "./types": "./src/types.mjs",
    "./artifact": "./src/artifact.mjs"
  },
  "sideEffects": false,
  "files": [
    "src/",
    "README.md",
    "LICENSE"
  ],
  "scripts": {
    "test": "vitest run --coverage",
    "test:fast": "vitest run --coverage",
    "test:watch": "vitest --coverage",
    "lint": "eslint src/ test/ --max-warnings=0",
    "lint:fix": "eslint src/ test/ --fix",
    "format": "prettier --write src/ test/",
    "format:check": "prettier --check src/ test/",
    "build": "echo 'Build complete (pure ESM, no compilation needed)'",
    "validate": "npm run lint && npm run test"
  },
  "keywords": [
    "rdf",
    "knowledge-graph",
    "probe",
    "integrity",
    "agents",
    "validation",
    "kgc"
  ],
  "dependencies": {
    "@unrdf/kgc-substrate": "workspace:*",
    "@unrdf/kgc-4d": "workspace:*",
    "@unrdf/v6-core": "workspace:*",
    "@unrdf/oxigraph": "workspace:*",
    "@unrdf/hooks": "workspace:*",
    "@unrdf/yawl": "workspace:*",
    "hash-wasm": "^4.12.0",
    "zod": "^4.1.13"
  },
  "devDependencies": {
    "@types/node": "^24.10.1",
    "vitest": "^4.0.15",
    "@vitest/coverage-v8": "^4.0.15"
  },
  "engines": {
    "node": ">=18.0.0",
    "pnpm": ">=7.0.0"
  },
  "repository": {
    "type": "git",
    "url": "https://github.com/unrdf/unrdf.git",
    "directory": "packages/kgc-probe"
  },
  "bugs": {
    "url": "https://github.com/unrdf/unrdf/issues"
  },
  "homepage": "https://github.com/unrdf/unrdf#readme",
  "license": "MIT",
  "publishConfig": {
    "access": "public"
  }
}
```

**Rationale**:
- Workspace dependencies (`workspace:*`) ensure monorepo consistency
- No CJS compatibility (ESM-only following workspace standards)
- Minimal DevDeps: only vitest, no TypeScript in source
- Hash-wasm for deterministic artifact hashing
- Zod for runtime schema validation

---

## PHASE 2: Export Structure (index.mjs)

### Root Exports

```javascript
/**
 * @fileoverview KGC Probe - Public API
 *
 * High-level entry points for:
 * - Creating probe orchestrator instances
 * - Registering guards and agents
 * - Running scans and merging results
 * - Validating artifacts
 */

// Orchestrator
export { createProbeOrchestrator } from './orchestrator.mjs';
export { ProbeOrchestrator } from './orchestrator.mjs';

// Guards Registry
export { createGuardRegistry } from './guards.mjs';
export { GuardRegistry } from './guards.mjs';

// Observation Validator
export { createObservationValidator } from './artifact.mjs';
export { ObservationValidator } from './artifact.mjs';

// Agents (Factory Functions)
export {
  createAgentRegistry,
  createCompletionAgent,
  createConsistencyAgent,
  createConformanceAgent,
  createCoverageAgent,
  createCachingAgent,
  createCompletenessAgent,
  createCoherenceAgent,
  createClusteringAgent,
  createClassificationAgent,
  createCollaborationAgent
} from './agents/index.mjs';

// Storage Backends
export {
  createMemoryStorage,
  createFileStorage,
  createDatabaseStorage
} from './storage/index.mjs';

// Artifact Operations
export {
  mergeShards,
  diffArtifacts,
  verifyArtifact,
  serializeArtifact,
  deserializeArtifact
} from './artifact.mjs';

// Convenience: Full Scan
export { runProbe } from './probe.mjs';

// Types
export * from './types.mjs';

// Default export (package metadata)
export default {
  name: '@unrdf/kgc-probe',
  version: '1.0.0',
  description: 'KGC Probe - Automated integrity scanning'
};
```

---

## PHASE 3: Data Structure Specification

### ObservationSchema (Zod)

```
SCHEMA: Observation
  - id: string (UUID, globally unique)
  - agent: string (agent identifier)
  - timestamp: ISO8601 (when observation made)
  - kind: enum('completeness' | 'consistency' | 'conformance' | ...)
  - severity: enum('critical' | 'warning' | 'info')
  - subject: string (RDF node under observation)
  - predicate: string (optional, RDF property)
  - object: string (optional, RDF value)
  - evidence: object (proof/context)
    - query: string (SPARQL or algorithm used)
    - result: any (computation result)
    - witnesses: string[] (confirming triples/nodes)
  - metrics: object
    - confidence: float [0, 1]
    - coverage: float [0, 1]
    - latency_ms: number
  - tags: string[] (searchable tags)
  - xid: string (correlation ID for tracing)
```

### ArtifactSchema

```
SCHEMA: Artifact
  - version: "1.0"
  - universe_id: string (4D universe reference)
  - snapshot_id: string (KGC-4D snapshot hash)
  - generated_at: ISO8601
  - probe_run_id: UUID
  - shard_count: number (how many shards merged)
  - shard_hash: hex string (Blake3 hash of shards)

  - observations: Observation[]

  - summary: object
    - total: number (all observations)
    - by_kind: object
      - completeness: number
      - consistency: number
      - conformance: number
      - ... (one per agent)
    - by_severity: object
      - critical: number
      - warning: number
      - info: number
    - confidence_mean: float
    - coverage_mean: float

  - metadata: object
    - agents_run: string[]
    - guards_applied: string[]
    - execution_time_ms: number
    - storage_backend: string
    - config: object (scan parameters)

  - integrity: object
    - checksum: hex string (Blake3(observations))
    - signature: string (optional, if signed)
    - verified_at: ISO8601 (if verified)
```

---

## PHASE 4: Core Algorithm Design

### ALGORITHM: ProbeOrchestrator.scan()

```
ALGORITHM: OrchestrateScan
INPUT:
  - config: ProbeConfig
  - store: Store
OUTPUT:
  - artifact: Artifact
  - status: 'success' | 'partial' | 'failed'

BEGIN
  // Phase 1: Initialization
  run_id ← GenerateUUID()
  start_time ← CurrentTime()
  observations ← []
  errors ← []

  // Phase 2: Parallel Agent Execution
  // Each agent runs concurrently, collects observations
  agent_tasks ← []
  FOR EACH agent IN config.agents DO
    task ← async {
      TRY
        results ← agent.scan(store, config)
        observations ← observations CONCAT results
      CATCH error
        errors.append({agent: agent.id, error: error.message})
      END TRY
    }
    agent_tasks.append(task)
  END FOR

  await Promise.all(agent_tasks)

  // Phase 3: Guard Validation
  FOR EACH guard IN config.guards DO
    TRY
      violations ← guard.validate(observations)
      FOR EACH violation IN violations DO
        observations.append({
          agent: 'guard:' + guard.id,
          kind: 'guard_violation',
          severity: violation.severity,
          evidence: violation.details
        })
      END FOR
    CATCH error
      errors.append({guard: guard.id, error: error.message})
    END TRY
  END FOR

  // Phase 4: Shard Merging (if distributed)
  IF config.distributed THEN
    shards ← await store.fetchShards()
    shard_hash ← HashWasm.blake3(JSON.stringify(shards))
    observations ← MergeShards(shards, observations)
  ELSE
    shard_hash ← HashWasm.blake3('[]')
  END IF

  // Phase 5: Artifact Generation
  end_time ← CurrentTime()
  execution_time ← end_time - start_time

  artifact ← {
    version: '1.0',
    universe_id: config.universe_id,
    snapshot_id: config.snapshot_id,
    generated_at: end_time.toISO(),
    probe_run_id: run_id,
    shard_count: config.distributed ? shards.length : 1,
    shard_hash: shard_hash,

    observations: observations,

    summary: ComputeSummary(observations),

    metadata: {
      agents_run: config.agents.map(a → a.id),
      guards_applied: config.guards.map(g → g.id),
      execution_time_ms: execution_time,
      storage_backend: store.type,
      config: config
    },

    integrity: {
      checksum: HashWasm.blake3(JSON.stringify(observations)),
      verified_at: null
    }
  }

  // Phase 6: Persistence
  IF config.persist THEN
    await store.saveArtifact(artifact)
  END IF

  RETURN {
    artifact: artifact,
    status: (errors.length == 0) ? 'success' : 'partial',
    errors: errors
  }
END
```

### ALGORITHM: GuardRegistry.validate()

```
ALGORITHM: ValidateObservations
INPUT:
  - observations: Observation[]
  - guards: Guard[]
OUTPUT:
  - violations: {guard_id, severity, details}[]

CONSTANTS:
  THRESHOLDS = {
    critical_observations: 50,
    avg_confidence_min: 0.8,
    coverage_min: 0.7
  }

BEGIN
  violations ← []

  // Guard 1: Observation Quality
  guard_id ← 'quality_check'
  low_confidence ← FILTER observations WHERE confidence < 0.6

  IF length(low_confidence) > THRESHOLDS.critical_observations THEN
    violations.append({
      guard_id: guard_id,
      severity: 'warning',
      details: {
        message: 'High count of low-confidence observations',
        count: length(low_confidence),
        threshold: THRESHOLDS.critical_observations
      }
    })
  END IF

  // Guard 2: Completeness Check
  guard_id ← 'completeness_check'
  completeness_obs ← FILTER observations WHERE kind == 'completeness'

  IF length(completeness_obs) > 0 THEN
    avg_coverage ← MEAN(completeness_obs.coverage)

    IF avg_coverage < THRESHOLDS.coverage_min THEN
      violations.append({
        guard_id: guard_id,
        severity: 'warning',
        details: {
          message: 'Coverage below threshold',
          coverage: avg_coverage,
          threshold: THRESHOLDS.coverage_min
        }
      })
    END IF
  END IF

  // Guard 3: Critical Severity Limit
  guard_id ← 'severity_limit'
  critical_count ← LENGTH(FILTER observations WHERE severity == 'critical')

  IF critical_count > 10 THEN
    violations.append({
      guard_id: guard_id,
      severity: 'critical',
      details: {
        message: 'Critical violations exceed limit',
        count: critical_count,
        limit: 10
      }
    })
  END IF

  RETURN violations
END
```

### ALGORITHM: MergeShards()

```
ALGORITHM: MergeShards
INPUT:
  - shards: Artifact[]
  - observations: Observation[]
OUTPUT:
  - merged_observations: Observation[]

CONSTANTS:
  DEDUP_ALGORITHM = 'observation_hash'

BEGIN
  // Phase 1: Collect all observations
  all_observations ← []
  FOR EACH shard IN shards DO
    all_observations ← all_observations CONCAT shard.observations
  END FOR

  // Phase 2: Add new observations
  all_observations ← all_observations CONCAT observations

  // Phase 3: Deduplication by content hash
  seen_hashes ← Set()
  deduplicated ← []

  FOR EACH obs IN all_observations DO
    hash ← HashWasm.blake3(JSON.stringify({
      agent: obs.agent,
      kind: obs.kind,
      subject: obs.subject,
      evidence: obs.evidence
    }))

    IF NOT seen_hashes.contains(hash) THEN
      deduplicated.append(obs)
      seen_hashes.add(hash)
    END IF
  END FOR

  // Phase 4: Sort by timestamp (deterministic order)
  deduplicated.sortBy((a, b) → a.timestamp.localeCompare(b.timestamp))

  RETURN deduplicated
END
```

### ALGORITHM: Agent.scan() (Base Pattern)

```
ALGORITHM: AgentScan (Base Template)
INPUT:
  - store: Store (RDF store with query capability)
  - config: ProbeConfig
OUTPUT:
  - observations: Observation[]

SUBROUTINE: QueryStore
INPUT: sparql_query (string)
OUTPUT: results (bindings[])
BEGIN
  results ← store.query(sparql_query)
  RETURN results
END

SUBROUTINE: ScoreObservation
INPUT: observation (dict), context (dict)
OUTPUT: scored_observation (dict)
BEGIN
  // Calculate confidence based on evidence quality
  evidence_quality ← ScoreEvidence(observation.evidence)
  witness_count ← length(observation.evidence.witnesses)

  confidence ← MIN(1.0, evidence_quality * (1 + witness_count * 0.1))

  observation.metrics.confidence ← confidence
  RETURN observation
END

BEGIN
  observations ← []
  start_time ← CurrentTime()

  // Phase 1: Discover Scope
  scope_query ← BuildScopeQuery(config)
  scope_results ← QueryStore(scope_query)

  IF length(scope_results) == 0 THEN
    RETURN [] // Early exit: nothing to scan
  END IF

  // Phase 2: Main Scan Loop
  FOR EACH item IN scope_results DO
    TRY
      // Execute agent-specific logic
      result ← AgentSpecificAnalysis(item, store, config)

      IF result.observations THEN
        FOR EACH obs IN result.observations DO
          // Normalize and validate
          normalized ← NormalizeObservation(obs, config)
          scored ← ScoreObservation(normalized, {item: item})

          observations.append(scored)
        END FOR
      END IF
    CATCH error
      // Log but continue
      LOG("Agent error for " + item.id + ": " + error.message)
    END TRY
  END FOR

  // Phase 3: Post-process
  end_time ← CurrentTime()
  latency ← end_time - start_time

  FOR EACH obs IN observations DO
    obs.metrics.latency_ms ← latency / length(observations)
  END FOR

  RETURN observations
END
```

---

## PHASE 5: Agent Implementations

### 10 Agent Specifications

| Agent ID | Kind | Purpose | Query Strategy |
|----------|------|---------|-----------------|
| `completion` | Completeness | Missing required properties | SPARQL OPTIONAL matching |
| `consistency` | Consistency | Value conflicts/contradictions | Cross-property validation |
| `conformance` | Conformance | Schema/SHACL violations | Shape graph queries |
| `coverage` | Coverage | Triple density and reachability | Graph traversal metrics |
| `caching` | Performance | Cache efficiency analysis | TTL/staleness detection |
| `completeness` | Completeness | Data population levels | NULL/UNBOUND patterns |
| `coherence` | Coherence | Semantic inconsistencies | Ontology reasoning |
| `clustering` | Structural | Entity grouping patterns | Community detection |
| `classification` | Semantic | Type hierarchy issues | Class inheritance |
| `collaboration` | Temporal | Cross-agent findings fusion | Observation correlation |

### Agent Registration Pattern

```javascript
INTERFACE: Agent
  id: string
  kind: string
  description: string

  scan(store, config): Promise<Observation[]>

CLASS: CompletionAgent IMPLEMENTS Agent
  id = 'completion'
  kind = 'completeness'

  scan(store, config):
    // Query: Find all (s, p, ?) with OPTIONAL (?o)
    // If ?o is NULL, emit completeness observation

CLASS: ConsistencyAgent IMPLEMENTS Agent
  id = 'consistency'
  kind = 'consistency'

  scan(store, config):
    // Query: Find (s, p1, o1) and (s, p1, o2) WHERE o1 ≠ o2
    // Emit conflict observation

CLASS: ConformanceAgent IMPLEMENTS Agent
  id = 'conformance'
  kind = 'conformance'

  scan(store, config):
    // Use SHACL shapes from store
    // Emit violation observations

// ... (8 more agents following same pattern)
```

---

## PHASE 6: Storage Backend Interface

### INTERFACE: Storage

```
INTERFACE: Storage
  type: string ('memory' | 'file' | 'database')

  METHODS:
    - saveArtifact(artifact): Promise<void>
    - loadArtifact(id): Promise<Artifact>
    - fetchShards(): Promise<Artifact[]>
    - listArtifacts(): Promise<Artifact[]>
    - deleteArtifact(id): Promise<void>

IMPLEMENTATION: MemoryStorage
  store: Map<string, Artifact>

  saveArtifact(artifact):
    store.set(artifact.probe_run_id, artifact)
    RETURN Promise.resolve()

IMPLEMENTATION: FileStorage
  root_dir: string

  saveArtifact(artifact):
    path ← root_dir + '/' + artifact.probe_run_id + '.json'
    write(path, JSON.stringify(artifact))
    RETURN Promise.resolve()

IMPLEMENTATION: DatabaseStorage
  db: @unrdf/kgc-substrate or similar

  saveArtifact(artifact):
    quad ← CreateQuad(
      subject: artifact.probe_run_id,
      predicate: 'rdf:type',
      object: 'probe:Artifact'
    )
    db.add(quad)
    // Add all artifact data as quads
    RETURN db.commit()
```

---

## PHASE 7: CLI Integration

### CLI Extension Pattern (kgc-probe.mjs)

```javascript
/**
 * @fileoverview KGC CLI extension for probe commands
 */

import { z } from 'zod';
import { createProbeOrchestrator, createMemoryStorage } from '@unrdf/kgc-probe';

// Schemas
const ScanSchema = z.object({
  universe: z.string().describe('Universe ID'),
  snapshot: z.string().optional().describe('Snapshot ID'),
  agents: z.array(z.string()).optional().describe('Agent IDs to run'),
  persist: z.boolean().optional().default(true).describe('Save artifact')
});

const ValidateSchema = z.object({
  artifact_id: z.string().describe('Artifact ID to validate'),
  strict: z.boolean().optional().default(false).describe('Fail on warnings')
});

const DiffSchema = z.object({
  artifact1_id: z.string(),
  artifact2_id: z.string(),
  kind: z.enum(['observations', 'summary']).optional().default('summary')
});

// Extension definition
const extension = {
  id: '@unrdf/kgc-probe',
  description: 'KGC Probe - Automated integrity scanning',

  nouns: {
    probe: {
      description: 'Knowledge graph integrity scanning',
      verbs: {
        scan: {
          description: 'Run full integrity scan with all agents',
          argsSchema: ScanSchema,
          handler: async (args) => {
            const orchestrator = createProbeOrchestrator({
              storage: createMemoryStorage()
            });

            const result = await orchestrator.scan({
              universe_id: args.universe,
              snapshot_id: args.snapshot,
              agents: args.agents || undefined,
              persist: args.persist
            });

            return {
              probe_run_id: result.artifact.probe_run_id,
              status: result.status,
              summary: result.artifact.summary,
              errors: result.errors
            };
          }
        },
        validate: {
          description: 'Validate an artifact against guards',
          argsSchema: ValidateSchema,
          handler: async (args) => {
            // Load artifact and validate
            return { valid: true, violations: [] };
          }
        },
        diff: {
          description: 'Compare two artifacts',
          argsSchema: DiffSchema,
          handler: async (args) => {
            // Diff artifacts
            return { differences: [] };
          }
        }
      }
    },

    shard: {
      description: 'Distributed shard management',
      verbs: {
        merge: {
          description: 'Merge probe shards',
          handler: async () => {
            return { merged: true, count: 0 };
          }
        }
      }
    }
  }
};

export default extension;
```

### Manifest Entry (extensions.mjs)

```javascript
// Add to manifest after line 103 (after yawl-observability, before standard packages):
{
  id: '@unrdf/kgc-probe',
  path: '../extensions/kgc-probe.mjs',
  loadOrder: 13,  // Right after hooks
  enabled: true
}
```

---

## PHASE 8: Dependency Audit

### Dependency Tree Analysis

```
@unrdf/kgc-probe
├── @unrdf/kgc-substrate (^1.0.0)
│   ├── @unrdf/kgc-4d
│   ├── @unrdf/oxigraph
│   └── @unrdf/core
├── @unrdf/kgc-4d (^5.0.1)
│   ├── @unrdf/core
│   ├── @unrdf/oxigraph
│   └── isomorphic-git
├── @unrdf/v6-core (^6.0.0-alpha.1)
│   ├── @unrdf/kgc-substrate
│   ├── @unrdf/yawl
│   ├── @unrdf/kgc-cli
│   ├── @unrdf/kgc-4d
│   ├── @unrdf/hooks
│   └── @unrdf/oxigraph
├── @unrdf/oxigraph (^5.0.1)
│   └── oxigraph (^0.5.2) [native]
├── @unrdf/hooks (^5.0.1)
│   ├── @unrdf/core
│   └── @unrdf/oxigraph
└── @unrdf/yawl (^5.0.0)
    ├── @unrdf/hooks
    ├── @unrdf/kgc-4d
    └── @unrdf/oxigraph
```

### Conflict Analysis

| Package | Version | Status | Notes |
|---------|---------|--------|-------|
| zod | ^4.1.13 | OK | Already used by kgc-substrate, kgc-4d, hooks, yawl |
| hash-wasm | ^4.12.0 | OK | Used by kgc-4d, yawl, kgc-substrate |
| @unrdf/core | workspace:* | OK | Core foundation, no conflicts |
| @unrdf/oxigraph | workspace:* | OK | SPARQL engine, required for queries |
| isomorphic-git | (via kgc-4d) | OK | Transitive, no direct use needed |

### Circular Dependency Check

**Result**: NO CIRCULAR DEPENDENCIES

- kgc-probe → kgc-substrate ✓
- kgc-probe → kgc-4d ✓
- kgc-probe → v6-core ✓ (v6-core imports from substrate, not probe)
- kgc-probe → hooks ✓
- kgc-probe → yawl ✓

**Justification**: kgc-probe is pure agent/guard library. CLI uses it, not vice versa.

---

## PHASE 9: Build Configuration

### ESM-Only Build Strategy

```javascript
// build.config.mjs (unbuild)
export default {
  entries: [
    'src/index.mjs',
    { input: 'src/orchestrator.mjs', name: 'orchestrator' },
    { input: 'src/guards.mjs', name: 'guards' },
    { input: 'src/agents/index.mjs', name: 'agents' },
    { input: 'src/storage/index.mjs', name: 'storage' },
    { input: 'src/artifact.mjs', name: 'artifact' },
    { input: 'src/types.mjs', name: 'types' }
  ],

  declaration: true,

  rollup: {
    emitCJS: false,  // ESM only

    output: {
      format: 'es',
      exports: 'named'
    }
  }
}
```

### Tree-Shaking Optimization

```javascript
// Every module must be pure:
// ✓ No side effects at module scope
// ✓ Export named functions/classes
// ✓ Use static imports only

// ✓ CORRECT
export function createAgent() { ... }
export const AgentRegistry = class { ... }

// ✗ WRONG (side effect)
globalThis.probeConfig = {}
registerAgent('default', new Agent())

// Package.json sideEffects: false enables tree-shaking
```

### No External Runtime Dependencies Policy

```
Allowed:
  ✓ @unrdf/* (monorepo)
  ✓ zod (validation)
  ✓ hash-wasm (hashing)
  ✓ Node built-ins (fs, path, etc.)

Forbidden:
  ✗ axios, node-fetch (use native fetch)
  ✗ lodash (use native iterables)
  ✗ winston (use console)
  ✗ Any external observability (OTEL is workspace concern)
```

---

## PHASE 10: Complexity Analysis

### Time Complexity

| Operation | Complexity | Notes |
|-----------|-----------|-------|
| scan() | O(n × m) | n=triples, m=agents (parallel) |
| mergeShards() | O(s × log s) | s=total observations, sorting + dedup |
| validateObservations() | O(o × g) | o=observations, g=guards |
| hashObservation() | O(1) | Fixed-size Blake3 hash |
| findKNearest(k) | O(n × log k) | Agent clustering |

### Space Complexity

| Structure | Complexity | Notes |
|-----------|-----------|-------|
| Artifact | O(o) | o=total observations |
| Observations cache | O(o) | In-memory during scan |
| Shard dedup set | O(s) | s=all shard observations |
| Guard violations | O(o) | Worst case: all invalid |

### Optimization Opportunities

1. **Streaming Agents**: Process observations in batches to reduce memory
2. **Lazy Evaluation**: Load shards on-demand vs. upfront
3. **Cache Observations**: LRU cache for repeated queries
4. **Parallel Guards**: Run guards concurrently with agents
5. **Bloom Filters**: Dedupe candidates before full hashing

---

## PHASE 11: Design Patterns

### 1. Factory Pattern (All Exports)

```javascript
// ✓ CORRECT: Factory returns configured instance
export function createProbeOrchestrator(config) {
  return new ProbeOrchestrator(config);
}

// ✗ WRONG: Direct class export
export class ProbeOrchestrator { ... }  // Then: new ProbeOrchestrator()
```

### 2. Registry Pattern (Guards, Agents)

```javascript
CLASS: GuardRegistry
  guards: Map<string, Guard>

  register(id: string, guard: Guard): void
  getGuard(id: string): Guard
  validateAll(observations): Violation[]

// Usage:
const registry = createGuardRegistry();
registry.register('quality_check', new QualityGuard());
const violations = registry.validateAll(observations);
```

### 3. Strategy Pattern (Storage)

```javascript
INTERFACE: Storage
  type: string
  saveArtifact(artifact): Promise<void>

// Multiple implementations:
MemoryStorage, FileStorage, DatabaseStorage
// Client code is agnostic
```

### 4. Observer Pattern (Agent Results)

```javascript
CLASS: ProbeOrchestrator
  listeners: Map<string, Callback[]>

  on(event: 'agent_complete' | 'guard_violation', callback): void
  emit(event: string, data): void

// Usage:
orchestrator.on('agent_complete', (result) => {
  console.log(`Agent ${result.agentId} completed`);
});
```

---

## PHASE 12: Testing Strategy

### Test Suites (Vitest)

```javascript
// test/orchestrator.test.mjs
describe('ProbeOrchestrator', () => {
  it('should run scan with all agents', async () => {
    const orch = createProbeOrchestrator({ ... });
    const result = await orch.scan(config);

    expect(result.artifact).toBeDefined();
    expect(result.artifact.observations.length).toBeGreaterThan(0);
  });
});

// test/guards.test.mjs
describe('GuardRegistry', () => {
  it('should detect quality violations', () => {
    const registry = createGuardRegistry();
    const violations = registry.validateAll(observations);

    expect(violations.some(v => v.guard_id === 'quality_check')).toBe(true);
  });
});

// test/agents.test.mjs
describe('Agents', () => {
  it('completion agent should find missing properties', async () => {
    const agent = createCompletionAgent();
    const results = await agent.scan(store, config);

    expect(results.length).toBeGreaterThan(0);
    expect(results[0].kind).toBe('completeness');
  });
});

// test/artifact.test.mjs
describe('Artifact Operations', () => {
  it('mergeShards should deduplicate', () => {
    const merged = mergeShards([shard1, shard2], newObs);

    const hashes = new Set(merged.map(o => o.hash));
    expect(hashes.size).toBe(merged.length); // No dups
  });
});
```

### Coverage Target
- **Lines**: 80%+
- **Branches**: 75%+
- **Functions**: 85%+

---

## Summary: File Structure

```
packages/kgc-probe/
├── src/
│   ├── index.mjs                    # Main exports
│   ├── types.mjs                    # Zod schemas + TypeScript types
│   ├── orchestrator.mjs             # ProbeOrchestrator class
│   ├── guards.mjs                   # GuardRegistry + implementations
│   ├── artifact.mjs                 # Artifact operations (merge, diff, verify)
│   ├── probe.mjs                    # Convenience runProbe() function
│   ├── agents/
│   │   ├── index.mjs                # Agent registry + factories
│   │   ├── base.mjs                 # Agent base class
│   │   ├── completion.mjs           # CompletionAgent
│   │   ├── consistency.mjs          # ConsistencyAgent
│   │   ├── conformance.mjs          # ConformanceAgent
│   │   ├── coverage.mjs             # CoverageAgent
│   │   ├── caching.mjs              # CachingAgent
│   │   ├── completeness.mjs         # CompletenessAgent
│   │   ├── coherence.mjs            # CoherenceAgent
│   │   ├── clustering.mjs           # ClusteringAgent
│   │   ├── classification.mjs       # ClassificationAgent
│   │   └── collaboration.mjs        # CollaborationAgent
│   └── storage/
│       ├── index.mjs                # Storage factories
│       ├── memory.mjs               # MemoryStorage
│       ├── file.mjs                 # FileStorage
│       └── database.mjs             # DatabaseStorage
├── test/
│   ├── orchestrator.test.mjs
│   ├── guards.test.mjs
│   ├── agents.test.mjs
│   ├── artifact.test.mjs
│   └── integration.test.mjs
├── package.json
├── README.md
└── LICENSE
```

---

## Pseudocode: Implementation Order (80/20)

### Phase 1: Core (Days 1-2)
1. ✓ package.json + exports
2. ✓ types.mjs (schemas)
3. ✓ orchestrator.mjs (scan algorithm)
4. ✓ guards.mjs (validation)

### Phase 2: Agents (Days 3-5)
5. agents/base.mjs (template)
6. agents/{completion,consistency,conformance}.mjs (high-value)
7. agents/{coverage,clustering}.mjs (structural)
8. agents/{caching,coherence,classification,collaboration}.mjs (enhanced)

### Phase 3: Infrastructure (Days 6-7)
9. storage/*.mjs (all backends)
10. artifact.mjs (merge/diff/verify)
11. probe.mjs (convenience)

### Phase 4: Integration (Days 8)
12. CLI extension (kgc-probe.mjs)
13. Manifest entry (extensions.mjs)
14. Tests + documentation

---

## Verification Checklist

- [ ] No circular dependencies (verified with `pnpm ls`)
- [ ] All exports are named + static (tree-shakeable)
- [ ] Zod schemas validate at runtime
- [ ] Tests pass with >80% coverage
- [ ] CLI command `kgc probe scan` works
- [ ] Artifact hash is deterministic (same input = same hash)
- [ ] No external dependencies outside @unrdf/*
- [ ] ESM-only (no CommonJS)
