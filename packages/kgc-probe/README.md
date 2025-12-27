# @unrdf/kgc-probe

Automated knowledge graph integrity scanning with 10 specialized agents and deterministic artifact validation.

## Overview

KGC Probe provides a comprehensive integrity scanning framework for RDF knowledge graphs with:

- **10 Specialized Agents**: Completeness, consistency, conformance, coverage, caching, coherence, clustering, classification, and collaboration analysis
- **Guard-Based Validation**: Quality checks, severity limits, and completeness verification
- **Deterministic Artifacts**: Blake3 hashed observations for reproducible scans
- **Distributed Shard Merging**: Multi-node probe result aggregation
- **Multiple Storage Backends**: Memory, file system, or database-backed
- **CLI Integration**: Full `kgc probe` command suite

## Installation

```bash
pnpm add @unrdf/kgc-probe
```

## Quick Start

### Basic Scan

```javascript
import { runProbe } from '@unrdf/kgc-probe';

const artifact = await runProbe({
  universe_id: 'my-universe',
  snapshot_id: 'snap_123'
});

console.log(artifact.summary);
// {
//   total: 42,
//   by_kind: { completeness: 15, consistency: 12, ... },
//   by_severity: { critical: 2, warning: 8, info: 32 },
//   confidence_mean: 0.89,
//   coverage_mean: 0.92
// }
```

### Advanced Orchestration

```javascript
import { createProbeOrchestrator, createMemoryStorage } from '@unrdf/kgc-probe';

const orchestrator = createProbeOrchestrator({
  storage: createMemoryStorage()
});

// Listen to events
orchestrator.on('agent_complete', (result) => {
  console.log(`Agent ${result.agentId} found ${result.observationCount} observations`);
});

// Run scan
const result = await orchestrator.scan({
  universe_id: 'my-universe',
  agents: ['completion', 'consistency', 'conformance'], // Specific agents
  distributed: true // Enable shard merging
});

console.log(result.status); // 'success' | 'partial' | 'failed'
```

## CLI Usage

```bash
# Run full scan
kgc probe scan --args '{"universe":"my-universe"}' --json

# List agents
kgc agent list

# List guards
kgc guard list

# Validate artifact
kgc probe validate --args '{"artifact_id":"run-123"}'

# Diff two artifacts
kgc probe diff --args '{"artifact1_id":"run-1","artifact2_id":"run-2"}'

# Merge shards
kgc shard merge --args '{"universe":"my-universe"}'
```

## Architecture

### 10 Agents

| Agent | Kind | Purpose |
|-------|------|---------|
| **Completion** | completeness | Missing required properties |
| **Consistency** | consistency | Value conflicts and contradictions |
| **Conformance** | conformance | SHACL shape violations |
| **Coverage** | coverage | Triple density and reachability |
| **Caching** | caching | Cache staleness and efficiency |
| **Completeness** | completeness_level | Data population ratios |
| **Coherence** | coherence | Semantic inconsistencies |
| **Clustering** | clustering | Entity grouping patterns |
| **Classification** | classification | Type hierarchy issues |
| **Collaboration** | collaboration | Cross-agent finding fusion |

### 5 Guards

| Guard | Purpose |
|-------|---------|
| **quality_check** | Validates confidence and observation count |
| **completeness_check** | Verifies coverage thresholds |
| **severity_limit** | Enforces maximum critical observations |
| **integrity_check** | Validates observation structure |
| **agent_coverage** | Ensures agent diversity |

### Observation Schema

```typescript
interface Observation {
  id: string; // UUID
  agent: string; // Agent identifier
  timestamp: ISO8601; // When observed
  kind: string; // Observation type
  severity: 'critical' | 'warning' | 'info';
  subject: string; // RDF node
  predicate?: string; // RDF property
  object?: string; // RDF value
  evidence: {
    query: string; // SPARQL or algorithm
    result: unknown; // Computation result
    witnesses: string[]; // Confirming references
  };
  metrics: {
    confidence: number; // [0, 1]
    coverage: number; // [0, 1]
    latency_ms: number;
  };
  tags?: string[];
  xid?: string; // Correlation ID
}
```

### Artifact Schema

```typescript
interface Artifact {
  version: '1.0';
  universe_id: string;
  snapshot_id: string;
  generated_at: ISO8601;
  probe_run_id: UUID;
  shard_count: number;
  shard_hash: hex64;
  observations: Observation[];
  summary: {
    total: number;
    by_kind: Record<string, number>;
    by_severity: Record<'critical' | 'warning' | 'info', number>;
    confidence_mean: number;
    coverage_mean: number;
  };
  metadata: {
    agents_run: string[];
    guards_applied: string[];
    execution_time_ms: number;
    storage_backend: string;
    config: ProbeConfig;
  };
  integrity: {
    checksum: hex64;
    signature?: string;
    verified_at?: ISO8601;
  };
}
```

## Storage Backends

### Memory Storage

In-process Map-based storage. Best for testing and single-process deployments.

```javascript
import { createMemoryStorage } from '@unrdf/kgc-probe';

const storage = createMemoryStorage();
await storage.saveArtifact(artifact);
const loaded = await storage.loadArtifact(artifact.probe_run_id);
```

### File Storage

Filesystem-based storage with JSON serialization.

```javascript
import { createFileStorage } from '@unrdf/kgc-probe';

const storage = createFileStorage('./artifacts');
await storage.saveArtifact(artifact);
const shards = await storage.fetchShards(); // Merge-ready artifacts
```

### Database Storage

KGC Substrate-backed storage using RDF quads (production).

```javascript
import { createDatabaseStorage } from '@unrdf/kgc-probe';

const storage = createDatabaseStorage({
  store: kgcSubstrateStore,
  namespace: 'https://probe.unrdf.org/'
});
```

## Operations

### Merge Shards

Deterministic merge of distributed probe results with deduplication:

```javascript
import { mergeShards } from '@unrdf/kgc-probe';

const artifacts = await storage.fetchShards();
const merged = await mergeShards(artifacts, newObservations);
// Merged observations are deduplicated and sorted deterministically
```

### Diff Artifacts

Compare two artifacts to identify changes:

```javascript
import { diffArtifacts } from '@unrdf/kgc-probe';

const diff = diffArtifacts(artifact1, artifact2);
// {
//   added: Observation[],
//   removed: Observation[],
//   modified: { subject, predicate, before, after }[],
//   summary: { total_changes, similarity_ratio }
// }
```

### Verify Artifact

Validate artifact integrity and schema compliance:

```javascript
import { verifyArtifact } from '@unrdf/kgc-probe';

const result = await verifyArtifact(artifact);
// {
//   valid: boolean,
//   errors: string[],
//   verified_at: ISO8601
// }
```

## Complexity Analysis

| Operation | Time | Space |
|-----------|------|-------|
| scan() | O(n × m) | O(o) |
| mergeShards() | O(s log s) | O(s) |
| diffArtifacts() | O(o) | O(o) |
| hashObservations() | O(o log o) | O(o) |

- n = triples in graph
- m = agents (parallel)
- o = observations
- s = shard observations

## Configuration

### ProbeConfig

```typescript
interface ProbeConfig {
  universe_id: string; // Required
  snapshot_id?: string; // Optional snapshot reference
  agents?: string[]; // Specific agent IDs (all if omitted)
  guards?: string[]; // Specific guard IDs
  distributed?: boolean; // Enable shard merging (default: false)
  persist?: boolean; // Save to storage (default: true)
  timeout_ms?: number; // Scan timeout (default: 300000)
  batch_size?: number; // Observation batch size (default: 100)
}
```

## Examples

### Full Workflow

```javascript
import {
  createProbeOrchestrator,
  createFileStorage,
  createGuardRegistry,
  diffArtifacts,
  verifyArtifact
} from '@unrdf/kgc-probe';

// Setup
const storage = createFileStorage('./probes');
const orchestrator = createProbeOrchestrator({ storage });

// Scan 1
const result1 = await orchestrator.scan({
  universe_id: 'my-universe',
  persist: true
});
const artifact1 = result1.artifact;

// Later: Scan 2
const result2 = await orchestrator.scan({
  universe_id: 'my-universe',
  persist: true
});
const artifact2 = result2.artifact;

// Compare
const diff = diffArtifacts(artifact1, artifact2);
console.log(`Changes: ${diff.summary.total_changes}`);
console.log(`Similarity: ${(diff.summary.similarity_ratio * 100).toFixed(1)}%`);

// Verify
const verification = await verifyArtifact(artifact2);
console.log(`Valid: ${verification.valid}`);
```

## Design Patterns

### Factory Pattern

All exports use factory functions for configuration flexibility:

```javascript
const orchestrator = createProbeOrchestrator({ storage });
const registry = createGuardRegistry();
const storage = createMemoryStorage();
```

### Registry Pattern

Agents and guards use registry for dynamic registration:

```javascript
const agents = createAgentRegistry();
agents.register('custom-agent', new MyAgent());
const agent = agents.get('custom-agent');
```

### Observer Pattern

Orchestrator emits events for monitoring:

```javascript
orchestrator.on('agent_complete', handler);
orchestrator.on('guard_violation', handler);
orchestrator.on('scan_complete', handler);
```

### Strategy Pattern

Storage is pluggable with multiple implementations:

```javascript
// Swap backends without changing code
const storage = process.env.NODE_ENV === 'production'
  ? createDatabaseStorage(opts)
  : createMemoryStorage();
```

## Performance Characteristics

- **Scan time**: Dominated by agent SPARQL queries (varies by graph size)
- **Memory**: O(observations) - typically 10-100KB per 1000 observations
- **Determinism**: Blake3 hashing ensures identical results for same input

## Testing

```bash
pnpm test                  # Run all tests
pnpm test:watch            # Watch mode
pnpm test coverage         # Coverage report
```

## Dependencies

- `@unrdf/kgc-substrate` - Knowledge store interface
- `@unrdf/kgc-4d` - 4D snapshot and universe management
- `@unrdf/v6-core` - Core UNRDF types and utilities
- `@unrdf/oxigraph` - SPARQL query engine
- `@unrdf/hooks` - Hook and policy framework
- `@unrdf/yawl` - Workflow orchestration
- `hash-wasm` - Blake3 hashing
- `zod` - Runtime validation schemas

## License

MIT

## Contributing

See [CONTRIBUTING.md](../../CONTRIBUTING.md)

## References

- [KGC Probe Design](../../DESIGN_KGC_PROBE_PACKAGE.md)
- [SPARC Methodology](../../docs/sparc-methodology.md)
- [UNRDF Documentation](https://unrdf.org)
