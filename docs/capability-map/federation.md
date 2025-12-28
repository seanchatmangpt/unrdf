# @unrdf/federation Capability Map

**Version**: 6.0.0
**Status**: Production Ready
**Runtime**: Node.js ≥18.0.0
**Last Updated**: 2025-12-28

---

## Overview

Distributed RDF Query with RAFT Consensus and Multi-Master Replication. Provides federated SPARQL execution across multiple peers with Comunica integration, strong consistency guarantees via RAFT, and multi-topology replication.

**Key Capabilities**:
- **Federated SPARQL**: Distribute queries across multiple RDF stores
- **RAFT Consensus**: Leader election, log replication, and fault tolerance
- **Data Replication**: Multi-master, primary-backup, and ring topologies
- **Advanced Federation**: Comunica-powered SPARQL 1.1 Federation with streaming

**Package Exports**:
```javascript
import {
  createCoordinator,
  executeFederatedQuery,
  createConsensusManager,
  createDataReplicationManager
} from '@unrdf/federation';
```

**Dependencies**:
- Required: `@unrdf/core` (workspace), `@unrdf/hooks` (workspace), `@comunica/query-sparql` (^3.2.4), `zod` (^4.1.13)
- Optional: `prom-client` (^15.0.0) for Prometheus metrics

**Evidence**:
- Test Coverage: Not specified
- Test Files: vitest-based test suite
- OTEL Validation: Not specified
- Example Files: Federation examples

---

## Capability Atoms

### Core Capabilities (Tier 1)

| Atom | Type | Runtime | Evidence | Compositions |
|------|------|---------|----------|--------------|
| `createCoordinator()` | Function | Node | [src/federation/coordinator.mjs](file:///home/user/unrdf/packages/federation/src/federation/coordinator.mjs) | C1 |
| `createPeerManager()` | Function | Node | [src/federation/peer-manager.mjs](file:///home/user/unrdf/packages/federation/src/federation/peer-manager.mjs) | C1 |
| `executeFederatedQuery()` | Function | Node | [src/federation/distributed-query.mjs](file:///home/user/unrdf/packages/federation/src/federation/distributed-query.mjs) | C2 |
| `routeQuery()` | Function | Node | [src/federation/distributed-query.mjs](file:///home/user/unrdf/packages/federation/src/federation/distributed-query.mjs) | C2 |

**Verification**:
```bash
timeout 5s pnpm --filter @unrdf/federation test
```

### Advanced Capabilities (Tier 2 - V6 Features)

| Atom | Type | Runtime | Evidence | Compositions |
|------|------|---------|----------|--------------|
| `FederationCoordinator` | Class | Node | [src/federation/federation-coordinator.mjs](file:///home/user/unrdf/packages/federation/src/federation/federation-coordinator.mjs) | C3 |
| `DistributedQueryEngine` | Class | Node | [src/federation/distributed-query-engine.mjs](file:///home/user/unrdf/packages/federation/src/federation/distributed-query-engine.mjs) | C4 |
| `ConsensusManager` | Class | Node | [src/federation/consensus-manager.mjs](file:///home/user/unrdf/packages/federation/src/federation/consensus-manager.mjs) | C5 |
| `DataReplicationManager` | Class | Node | [src/federation/data-replication.mjs](file:///home/user/unrdf/packages/federation/src/federation/data-replication.mjs) | C6 |

### Experimental Capabilities (Tier 3 - Comunica)

| Atom | Type | Runtime | Evidence | Compositions |
|------|------|---------|----------|--------------|
| `createAdvancedFederationEngine()` | Function | Node | [src/advanced-sparql-federation.mjs](file:///home/user/unrdf/packages/federation/src/advanced-sparql-federation.mjs) | C7 |
| `federatedQuery()` | Function | Node | [src/advanced-sparql-federation.mjs](file:///home/user/unrdf/packages/federation/src/advanced-sparql-federation.mjs) | C7 |
| `streamFederatedQuery()` | Function | Node | [src/advanced-sparql-federation.mjs](file:///home/user/unrdf/packages/federation/src/advanced-sparql-federation.mjs) | C8 |

---

## Composition Patterns

**C1**: **Basic Federation** - Create coordinator → Add peers → Route queries
```javascript
import { createCoordinator, createPeerManager } from '@unrdf/federation';

const coordinator = createCoordinator({ nodeId: 'node-1' });
const peerManager = createPeerManager();
await peerManager.addPeer({ id: 'peer-1', endpoint: 'http://peer1:8080' });
```

**C2**: **Federated Query** - Execute SPARQL across multiple stores
```javascript
import { executeFederatedQuery } from '@unrdf/federation';

const results = await executeFederatedQuery(
  'SELECT ?s ?p ?o WHERE { ?s ?p ?o }',
  { peers: [peer1, peer2, peer3], strategy: 'parallel' }
);
```

**C3**: **Store Management** - Coordinator manages multiple stores
```javascript
import { createFederationCoordinator } from '@unrdf/federation';

const coordinator = createFederationCoordinator({
  stores: [store1, store2, store3],
  healthCheckInterval: 5000
});
await coordinator.start();
```

**C4**: **Query Optimization** - Analyze query → Generate plan → Execute
```javascript
import { createDistributedQueryEngine } from '@unrdf/federation';

const engine = createDistributedQueryEngine({
  strategy: 'cost-based', // or 'round-robin', 'hash'
  enableCaching: true
});
const plan = engine.analyzePlan(sparqlQuery);
const results = await engine.execute(plan);
```

**C5**: **RAFT Consensus** - Leader election → Log replication → Commit
```javascript
import { createConsensusManager } from '@unrdf/federation';

const consensus = createConsensusManager({
  nodeId: 'node-1',
  peers: ['node-2', 'node-3'],
  electionTimeout: 300
});
await consensus.start();
// Automatic leader election and log replication
```

**C6**: **Data Replication** - Configure topology → Replicate deltas
```javascript
import { createDataReplicationManager } from '@unrdf/federation';

const replication = createDataReplicationManager({
  topology: 'multi-master', // or 'primary-backup', 'ring'
  conflictResolution: 'last-write-wins',
  replicationMode: 'async'
});
await replication.replicate(delta, targetNodes);
```

**C7**: **Comunica Federation** - Advanced SPARQL 1.1 Federation
```javascript
import { createAdvancedFederationEngine, federatedQuery } from '@unrdf/federation';

const engine = createAdvancedFederationEngine({
  sources: [
    { type: 'sparql', value: 'http://dbpedia.org/sparql' },
    { type: 'sparql', value: 'http://localhost:3030/dataset' }
  ]
});
const results = await federatedQuery(engine, 'SELECT * WHERE { ?s ?p ?o }');
```

**C8**: **Streaming Federation** - Stream results for large datasets
```javascript
import { streamFederatedQuery } from '@unrdf/federation';

const stream = await streamFederatedQuery(engine, query);
for await (const binding of stream) {
  console.log(binding);
}
```

---

## Performance Model

**Theoretical Performance**:

Based on distributed systems architecture:
- Time Complexity: O(p·n) for federated queries (p=peers, n=triples per peer)
- Space Complexity: O(k) for result sets (k=result count)
- Scalability: Network-latency bound

**Empirical Benchmarks** (from performance-analysis.md):

| Operation | Dataset Size | Execution Time | Notes |
|-----------|--------------|----------------|-------|
| Federated SPARQL | 3 peers, 1K triples each | 100ms + network latency | Network-bound |
| Leader election | 3-node cluster | 100-500ms | Depends on RTT |
| Log replication | Per entry | 50-200ms | Quorum-dependent |
| Consensus throughput | Leader capacity | 100-500 commands/sec | Leader-limited |

**Performance Characteristics**:
- Network latency dominates query time
- RAFT requires >50% quorum availability
- Multi-master topology increases write throughput
- Conflict resolution adds overhead

**Optimization Strategies**:
1. **Query Planning**: Push filters to source stores
2. **Caching**: Cache query results per peer
3. **Sharding**: Partition data across peers

**Verification**:
```bash
timeout 30s pnpm --filter @unrdf/federation run benchmark
```

---

## Runtime Compatibility Matrix

| Capability | Node.js | Browser | BEAM/WASM | Notes |
|------------|---------|---------|-----------|-------|
| Federation Core | ✅ ≥18.0 | ❌ Not supported | ⏳ Planned | Requires network |
| RAFT Consensus | ✅ ≥18.0 | ❌ Not supported | ❌ Not supported | Server-only |
| Data Replication | ✅ ≥18.0 | ❌ Not supported | ❌ Not supported | Server-only |
| Comunica Engine | ✅ ≥18.0 | ✅ ES2020+ | ⏳ Planned | Universal |
| Streaming Queries | ✅ ≥18.0 | ✅ ES2020+ | ⏳ Planned | AsyncIterator support |

**Legend**:
- ✅ Fully supported
- ⏳ Planned/In progress
- ❌ Not supported

**Node.js Considerations**:
- Requires stable network for RAFT
- ESM-only: Requires `"type": "module"`

---

## Evidence & Verification

### Source Code References

All capability atoms are traceable to source:
- Core: [src/federation/coordinator.mjs](file:///home/user/unrdf/packages/federation/src/federation/coordinator.mjs)
- Consensus: [src/federation/consensus-manager.mjs](file:///home/user/unrdf/packages/federation/src/federation/consensus-manager.mjs)
- Comunica: [src/advanced-sparql-federation.mjs](file:///home/user/unrdf/packages/federation/src/advanced-sparql-federation.mjs)

### Verification Commands

**Quick Verification** (< 5 seconds):
```bash
timeout 5s pnpm --filter @unrdf/federation test
```

---

## Cross-References

### Related Packages
- **@unrdf/consensus**: Full RAFT implementation
- **@unrdf/oxigraph**: Local RDF store
- **@comunica/query-sparql**: SPARQL 1.1 engine

---

**Document Metadata**:
- **Template Version**: 1.0.0
- **Generated**: 2025-12-28
- **Maintainer**: @unrdf/core-team
- **Last Review**: 2025-12-28
- **Next Review**: 2026-03-28
