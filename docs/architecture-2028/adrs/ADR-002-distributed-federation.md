# ADR-002: Distributed Knowledge Federation

**Status:** Proposed
**Date:** 2025-11-18
**Deciders:** System Architecture Team
**Technical Story:** Enable distributed knowledge graphs across multiple stores and networks

## Context and Problem Statement

Knowledge graphs often span multiple data sources, organizations, and geographic locations. How do we enable federated queries, distributed synchronization, and consistency across heterogeneous RDF stores while maintaining performance and reliability?

## Decision Drivers

- **Scalability**: Support millions of triples across distributed nodes
- **Performance**: Sub-200ms federated queries (P95)
- **Consistency**: Configurable consistency models (eventual, causal, strong)
- **Fault Tolerance**: Byzantine fault tolerance for untrusted networks
- **Interoperability**: Work with SPARQL endpoints, GraphQL, REST APIs
- **Network Efficiency**: Minimize data transfer and round trips

## Considered Options

### Option 1: SPARQL Federation Only (W3C SPARQL 1.1 Federation)
- **Pros**: Standard compliant, simple, works with existing endpoints
- **Cons**: Limited optimization, no consistency guarantees, high latency

### Option 2: Custom P2P Protocol
- **Pros**: Full control, optimized for RDF, advanced features
- **Cons**: Not standard compliant, complex implementation

### Option 3: Hybrid Approach (Recommended)
- **Pros**: Standard compliance + advanced features, flexibility
- **Cons**: More complex, multiple code paths

## Decision Outcome

**Chosen option:** Option 3 - Hybrid Approach

Implement a federated knowledge architecture supporting:
1. Standard SPARQL 1.1 Federation for compatibility
2. Custom protocol for advanced features (P2P sync, CRDTs, consistency)
3. GraphQL Federation for modern APIs
4. gRPC for high-performance node-to-node communication

### Architecture Design

#### Component Structure

```
src/federation/
├── core/
│   ├── federation-manager.mjs      # Main orchestrator
│   ├── node-registry.mjs           # Peer/endpoint registry
│   ├── topology-manager.mjs        # Network topology
│   └── schemas.mjs
│
├── protocol/
│   ├── federation-protocol.mjs     # Core protocol definition
│   ├── query-planner.mjs           # Distributed query planning
│   ├── query-executor.mjs          # Distributed execution
│   ├── result-merger.mjs           # Result aggregation
│   └── cost-estimator.mjs          # Query cost estimation
│
├── adapters/
│   ├── sparql-adapter.mjs          # SPARQL 1.1 Federation
│   ├── graphql-adapter.mjs         # GraphQL Federation
│   ├── grpc-adapter.mjs            # gRPC communication
│   └── rest-adapter.mjs            # REST API adapter
│
├── sync/
│   ├── p2p-sync.mjs                # Peer-to-peer synchronization
│   ├── crdt-resolver.mjs           # CRDT-based conflict resolution
│   ├── vector-clock.mjs            # Vector clock implementation
│   ├── conflict-resolver.mjs       # Conflict resolution strategies
│   └── sync-protocol.mjs           # Synchronization protocol
│
├── consistency/
│   ├── consistency-manager.mjs     # Consistency model manager
│   ├── eventual.mjs                # Eventual consistency
│   ├── causal.mjs                  # Causal consistency
│   ├── strong.mjs                  # Strong consistency (Paxos/Raft)
│   └── quorum.mjs                  # Quorum-based reads/writes
│
├── discovery/
│   ├── service-discovery.mjs       # Node discovery
│   ├── dns-discovery.mjs           # DNS-based discovery
│   ├── mdns-discovery.mjs          # mDNS for local networks
│   └── dht-discovery.mjs           # DHT-based discovery (IPFS)
│
└── index.mjs
```

#### Core Interfaces

```javascript
/**
 * @typedef {Object} FederationConfig
 * @property {'sparql'|'graphql'|'grpc'|'p2p'} protocol
 * @property {'eventual'|'causal'|'strong'} consistency
 * @property {Array<string>} peers - Peer node addresses
 * @property {Object} topology - Network topology configuration
 */

/**
 * Federation Manager for distributed knowledge graphs
 */
export class FederationManager {
  constructor(config = {}) {
    this.config = {
      protocol: config.protocol || 'p2p',
      consistency: config.consistency || 'eventual',
      peers: config.peers || [],
      topology: config.topology || { type: 'mesh' }
    };

    this.nodeRegistry = new NodeRegistry();
    this.topologyManager = new TopologyManager(this.config.topology);
    this.queryPlanner = new QueryPlanner(this.nodeRegistry);
    this.syncManager = new P2PSyncManager(this.config);
  }

  /**
   * Execute a federated query across multiple nodes
   * @param {string} sparqlQuery - SPARQL query
   * @param {Object} options - Query options
   * @returns {Promise<Array<Object>>} Query results
   */
  async query(sparqlQuery, options = {}) {
    const span = tracer.startSpan('federation.query');

    try {
      span.setAttribute('query.type', 'federated');
      span.setAttribute('peers.count', this.nodeRegistry.size());

      // Parse query and create query plan
      const plan = await this.queryPlanner.plan(sparqlQuery, options);

      span.setAttribute('plan.subqueries', plan.subqueries.length);
      span.setAttribute('plan.estimated_cost', plan.estimatedCost);

      // Execute query plan across nodes
      const executor = new QueryExecutor(this.nodeRegistry);
      const results = await executor.execute(plan);

      span.setAttribute('results.count', results.length);
      span.setStatus({ code: SpanStatusCode.OK });

      return results;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Register a peer node
   * @param {Object} peer - Peer configuration
   */
  async registerPeer(peer) {
    const span = tracer.startSpan('federation.peer.register');

    try {
      span.setAttribute('peer.id', peer.id);
      span.setAttribute('peer.endpoint', peer.endpoint);

      // Validate peer capabilities
      const capabilities = await this._probePeerCapabilities(peer);

      // Register in node registry
      this.nodeRegistry.register({
        id: peer.id,
        endpoint: peer.endpoint,
        protocol: peer.protocol || 'grpc',
        capabilities,
        metadata: peer.metadata || {}
      });

      // Update network topology
      await this.topologyManager.addNode(peer.id);

      span.setStatus({ code: SpanStatusCode.OK });

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Synchronize local store with peers
   * @param {Store} store - Local N3 store
   * @param {Object} options - Sync options
   */
  async sync(store, options = {}) {
    const span = tracer.startSpan('federation.sync');

    try {
      span.setAttribute('store.size', store.size);
      span.setAttribute('sync.mode', this.config.consistency);

      // Execute synchronization based on consistency model
      const result = await this.syncManager.sync(store, options);

      span.setAttribute('sync.changes.sent', result.changesSent);
      span.setAttribute('sync.changes.received', result.changesReceived);
      span.setAttribute('sync.conflicts', result.conflicts);

      span.setStatus({ code: SpanStatusCode.OK });

      return result;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }
}
```

#### Distributed Query Planning

```javascript
/**
 * Query planner for distributed SPARQL queries
 */
export class QueryPlanner {
  constructor(nodeRegistry) {
    this.nodeRegistry = nodeRegistry;
    this.costEstimator = new CostEstimator();
  }

  /**
   * Create distributed query plan
   * @param {string} sparqlQuery - SPARQL query
   * @param {Object} options - Planning options
   * @returns {Promise<Object>} Query plan
   */
  async plan(sparqlQuery, options = {}) {
    const span = tracer.startSpan('federation.query.plan');

    try {
      // Parse SPARQL query
      const parsed = this._parseSparql(sparqlQuery);

      // Analyze query structure
      const analysis = this._analyzeQuery(parsed);

      span.setAttribute('query.patterns', analysis.patterns.length);
      span.setAttribute('query.filters', analysis.filters.length);

      // Determine data sources for each pattern
      const sourceMap = await this._mapPatternsToSources(analysis.patterns);

      // Generate optimal execution plan
      const plan = await this._generatePlan(sourceMap, analysis);

      // Estimate query cost
      plan.estimatedCost = await this.costEstimator.estimate(plan);

      span.setAttribute('plan.strategy', plan.strategy);
      span.setStatus({ code: SpanStatusCode.OK });

      return plan;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  async _mapPatternsToSources(patterns) {
    const sourceMap = new Map();

    for (const pattern of patterns) {
      // Query capabilities from all nodes
      const capableNodes = [];

      for (const node of this.nodeRegistry.getAll()) {
        // Check if node can answer this pattern
        const canAnswer = await this._checkPatternCapability(node, pattern);
        if (canAnswer) {
          capableNodes.push({
            nodeId: node.id,
            estimatedCost: await this.costEstimator.estimatePattern(node, pattern)
          });
        }
      }

      // Sort by estimated cost
      capableNodes.sort((a, b) => a.estimatedCost - b.estimatedCost);

      sourceMap.set(pattern, capableNodes);
    }

    return sourceMap;
  }

  async _generatePlan(sourceMap, analysis) {
    // Determine execution strategy
    const strategy = this._selectStrategy(sourceMap, analysis);

    switch (strategy) {
      case 'parallel':
        return this._generateParallelPlan(sourceMap, analysis);
      case 'pipelined':
        return this._generatePipelinedPlan(sourceMap, analysis);
      case 'sequential':
        return this._generateSequentialPlan(sourceMap, analysis);
      default:
        throw new Error(`Unknown strategy: ${strategy}`);
    }
  }

  _generateParallelPlan(sourceMap, analysis) {
    // Execute all independent patterns in parallel
    const subqueries = [];

    for (const [pattern, nodes] of sourceMap.entries()) {
      if (nodes.length === 0) {
        throw new Error(`No capable nodes for pattern: ${pattern}`);
      }

      subqueries.push({
        pattern,
        node: nodes[0].nodeId,  // Best node
        type: 'parallel',
        dependencies: []
      });
    }

    return {
      strategy: 'parallel',
      subqueries,
      mergeStrategy: 'join'
    };
  }
}
```

#### P2P Synchronization with CRDTs

```javascript
/**
 * Peer-to-peer synchronization using CRDTs
 */
export class P2PSyncManager {
  constructor(config) {
    this.config = config;
    this.vectorClock = new VectorClock();
    this.crdtResolver = new CRDTResolver();
  }

  /**
   * Synchronize with peers using CRDT-based conflict resolution
   * @param {Store} store - Local N3 store
   * @param {Object} options - Sync options
   * @returns {Promise<Object>} Sync result
   */
  async sync(store, options = {}) {
    const span = tracer.startSpan('federation.sync.p2p');

    try {
      const localVersion = this.vectorClock.getLocalVersion();

      span.setAttribute('sync.local_version', localVersion);

      // Get changes since last sync
      const localChanges = await this._getLocalChanges(store, localVersion);

      span.setAttribute('sync.local_changes', localChanges.length);

      // Exchange changes with peers
      const remoteChanges = await this._exchangeChanges(localChanges);

      span.setAttribute('sync.remote_changes', remoteChanges.length);

      // Resolve conflicts using CRDTs
      const resolved = await this.crdtResolver.resolve(
        store,
        localChanges,
        remoteChanges
      );

      span.setAttribute('sync.conflicts', resolved.conflicts.length);

      // Apply resolved changes
      await this._applyChanges(store, resolved.changes);

      // Update vector clock
      this.vectorClock.increment();

      span.setStatus({ code: SpanStatusCode.OK });

      return {
        changesSent: localChanges.length,
        changesReceived: remoteChanges.length,
        conflicts: resolved.conflicts.length,
        version: this.vectorClock.getLocalVersion()
      };

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  async _exchangeChanges(localChanges) {
    const allRemoteChanges = [];

    // Connect to all peers in parallel
    const peerPromises = this.config.peers.map(async (peer) => {
      try {
        const client = await this._connectToPeer(peer);
        const remoteChanges = await client.sync({
          changes: localChanges,
          version: this.vectorClock.getLocalVersion()
        });
        return remoteChanges;
      } catch (error) {
        console.error(`Failed to sync with peer ${peer}:`, error);
        return [];
      }
    });

    const results = await Promise.all(peerPromises);

    for (const changes of results) {
      allRemoteChanges.push(...changes);
    }

    return allRemoteChanges;
  }
}
```

#### CRDT Conflict Resolution

```javascript
/**
 * CRDT-based conflict resolution for RDF quads
 */
export class CRDTResolver {
  constructor() {
    this.strategies = {
      'lww': this._lastWriteWins.bind(this),
      'addWins': this._addWins.bind(this),
      'removeWins': this._removeWins.bind(this),
      'custom': this._customResolver.bind(this)
    };
  }

  /**
   * Resolve conflicts between local and remote changes
   * @param {Store} store - Local store
   * @param {Array} localChanges - Local changes
   * @param {Array} remoteChanges - Remote changes
   * @returns {Promise<Object>} Resolved changes
   */
  async resolve(store, localChanges, remoteChanges) {
    const conflicts = [];
    const resolvedChanges = [];

    // Build conflict set
    const conflictSet = this._findConflicts(localChanges, remoteChanges);

    for (const conflict of conflictSet) {
      // Apply resolution strategy
      const strategy = conflict.strategy || 'lww';
      const resolved = await this.strategies[strategy](conflict);

      if (resolved) {
        resolvedChanges.push(resolved);
      } else {
        conflicts.push(conflict);
      }
    }

    // Add non-conflicting changes
    const nonConflicting = this._getNonConflicting(
      localChanges,
      remoteChanges,
      conflictSet
    );
    resolvedChanges.push(...nonConflicting);

    return {
      changes: resolvedChanges,
      conflicts
    };
  }

  _lastWriteWins(conflict) {
    // Timestamp-based resolution
    const localTime = conflict.local.timestamp;
    const remoteTime = conflict.remote.timestamp;

    return localTime > remoteTime ? conflict.local : conflict.remote;
  }

  _addWins(conflict) {
    // Add operations always win over remove
    if (conflict.local.type === 'add') return conflict.local;
    if (conflict.remote.type === 'add') return conflict.remote;
    return null;
  }

  _removeWins(conflict) {
    // Remove operations always win over add
    if (conflict.local.type === 'remove') return conflict.local;
    if (conflict.remote.type === 'remove') return conflict.remote;
    return null;
  }
}
```

### Integration with Knowledge Hooks

```javascript
// Federation hooks
defineHook('federation.query.distributed', {
  phase: 'pre',
  condition: async ({ query }) => query.includes('SERVICE'),
  effect: async ({ query }) => {
    const optimized = await federationManager.optimizeQuery(query);
    return { query: optimized };
  }
});

defineHook('federation.sync.completed', {
  phase: 'post',
  condition: async ({ result }) => result.conflicts > 0,
  effect: async ({ result }) => {
    // Log conflicts for manual resolution
    await conflictLogger.log(result.conflicts);
  }
});

defineHook('federation.peer.joined', {
  phase: 'post',
  effect: async ({ peer }) => {
    // Notify other subsystems
    await eventBus.emit('peer:joined', peer);

    // Trigger initial sync
    await federationManager.sync(store);
  }
});
```

### Usage Examples

```javascript
// Example 1: Federated query across multiple SPARQL endpoints
import { FederationManager } from 'unrdf/federation';

const federation = new FederationManager({
  protocol: 'sparql',
  peers: [
    'https://dbpedia.org/sparql',
    'https://query.wikidata.org/sparql',
    'https://data.gov/sparql'
  ]
});

const query = `
  PREFIX dbo: <http://dbpedia.org/ontology/>
  PREFIX wd: <http://www.wikidata.org/entity/>

  SELECT ?person ?name ?birthDate WHERE {
    SERVICE <https://dbpedia.org/sparql> {
      ?person dbo:occupation wd:Q82594 .
      ?person dbo:name ?name .
    }
    SERVICE <https://query.wikidata.org/sparql> {
      ?person wdt:P569 ?birthDate .
    }
  }
`;

const results = await federation.query(query);

// Example 2: P2P synchronization with eventual consistency
const p2pFederation = new FederationManager({
  protocol: 'p2p',
  consistency: 'eventual',
  peers: ['node1.local:8080', 'node2.local:8080'],
  topology: { type: 'mesh' }
});

// Periodic sync every 30 seconds
setInterval(async () => {
  const result = await p2pFederation.sync(store);
  console.log(`Synced: sent ${result.changesSent}, received ${result.changesReceived}`);
}, 30000);
```

## Consequences

### Positive

- **Scalability**: Distribute load across multiple nodes
- **Resilience**: Fault tolerance through replication
- **Performance**: Parallel query execution
- **Flexibility**: Support multiple protocols and consistency models
- **Interoperability**: Standard SPARQL federation support

### Negative

- **Complexity**: Significantly more complex than single-node
- **Network Dependency**: Requires reliable network connectivity
- **Consistency Trade-offs**: Eventual consistency may have stale reads
- **Operational Overhead**: Managing distributed systems is hard

## Performance Targets

| Operation | Target Latency (P95) |
|-----------|---------------------|
| Federated Query (2 nodes) | <200ms |
| Federated Query (10 nodes) | <500ms |
| P2P Sync (1000 changes) | <1s |
| Conflict Resolution | <100ms |

## References

- [SPARQL 1.1 Federation](https://www.w3.org/TR/sparql11-federated-query/)
- [CRDTs: Consistency without concurrency control](https://arxiv.org/abs/0907.0929)
- [Apollo GraphQL Federation](https://www.apollographql.com/docs/federation/)
