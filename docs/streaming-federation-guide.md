# Streaming and Federation Integration Guide

Comprehensive guide for building real-time, distributed knowledge graph applications with UNRDF's streaming and federation hooks.

## Table of Contents

- [Overview](#overview)
- [Real-time Federated Queries](#real-time-federated-queries)
- [State Synchronization](#state-synchronization)
- [Error Handling and Recovery](#error-handling-and-recovery)
- [Performance Tuning](#performance-tuning)
- [Architecture Patterns](#architecture-patterns)
- [Production Deployment](#production-deployment)

---

## Overview

UNRDF provides a powerful combination of streaming and federation hooks that enable building distributed, real-time knowledge graph applications. This guide covers integration patterns, best practices, and production considerations.

### Key Concepts

| Concept | Description | Primary Hooks |
|---------|-------------|---------------|
| **Streaming** | Real-time change propagation | `useChangeFeed`, `useStreamProcessor` |
| **Federation** | Distributed query execution | `useFederatedSystem`, `useDistributedQuery` |
| **Consensus** | Distributed agreement | `useConsensusManager` |
| **Replication** | Data synchronization | `useDataReplication` |

### Architecture Overview

```
+------------------+      +------------------+      +------------------+
|   React App      |      |   React App      |      |   React App      |
|  (useChangeFeed) |      | (useStreamProc)  |      | (useFederated)   |
+--------+---------+      +--------+---------+      +--------+---------+
         |                         |                         |
         v                         v                         v
+--------+---------+      +--------+---------+      +--------+---------+
|   Change Feed    |<---->|  Stream Window   |<---->|  Federation      |
|   (Real-time)    |      |  (Aggregation)   |      |  Coordinator     |
+--------+---------+      +--------+---------+      +--------+---------+
         |                         |                         |
         +-------------------------+-------------------------+
                                   |
                    +-------------+v+-------------+
                    |                             |
              +-----v-----+               +-------v------+
              |  Store 1  |               |   Store 2    |
              | (Primary) |<--Consensus-->|  (Replica)   |
              +-----------+               +--------------+
```

---

## Real-time Federated Queries

### Basic Integration

Combine `useDistributedQuery` with `useChangeFeed` for real-time federated dashboards:

```jsx
import { useDistributedQuery, useChangeFeed } from 'unrdf/react-hooks';

function RealTimeFederatedDashboard() {
  // Distributed query across multiple stores
  const { data, loading, execute, executionStats } = useDistributedQuery(`
    SELECT ?product ?price ?store WHERE {
      ?product a :Product ;
               :price ?price ;
               :soldAt ?store .
    }
  `, {
    strategy: 'quorum',      // Query 51% of stores
    aggregation: 'union',    // Combine all results
    timeout: 30000           // 30s timeout
  });

  // Real-time change feed for updates
  const { changes, start, stop, isRunning, stats } = useChangeFeed({
    filter: (change) => {
      // Only track price changes
      return change.quads.some(q =>
        q.predicate.value.includes('price')
      );
    },
    operations: ['insert', 'delete'],
    batchSize: 10,
    batchInterval: 1000
  });

  // Refetch when changes detected
  useEffect(() => {
    if (changes.length > 0) {
      execute(); // Refetch distributed query
    }
  }, [changes]);

  return (
    <div>
      <header>
        <h1>Real-time Product Dashboard</h1>
        <button onClick={isRunning ? stop : start}>
          {isRunning ? 'Pause Updates' : 'Start Updates'}
        </button>
      </header>

      <aside>
        <h3>Query Stats</h3>
        <p>Duration: {executionStats?.duration}ms</p>
        <p>Stores queried: {executionStats?.storesQueried?.length}</p>
        <p>Cache hit: {executionStats?.cacheHit ? 'Yes' : 'No'}</p>
      </aside>

      <aside>
        <h3>Change Feed Stats</h3>
        <p>Total changes: {stats.totalChanges}</p>
        <p>Inserts: {stats.inserts}</p>
        <p>Deletes: {stats.deletes}</p>
      </aside>

      <main>
        {loading ? (
          <p>Loading from distributed stores...</p>
        ) : (
          <ProductTable data={data} />
        )}
      </main>

      <footer>
        <h3>Recent Changes</h3>
        <ul>
          {changes.slice(-5).map((change, i) => (
            <li key={i}>
              {change.operation}: {change.quads.length} quads
            </li>
          ))}
        </ul>
      </footer>
    </div>
  );
}
```

### Windowed Aggregation with Federated Data

Use `useStreamProcessor` for time-window aggregations on federated data:

```jsx
import {
  useFederatedSystem,
  useChangeFeed,
  useStreamProcessor
} from 'unrdf/react-hooks';

function FederatedAnalyticsDashboard() {
  // Federation for distributed queries
  const federation = useFederatedSystem({
    stores: ['store-us', 'store-eu', 'store-asia'],
    consensusProtocol: 'raft',
    replicationFactor: 2
  });

  // Stream processor for windowed aggregation
  const { windows, currentWindow, start, stop, stats } = useStreamProcessor({
    windowType: 'tumbling',
    windowSize: 60000, // 1-minute windows
    aggregator: (events) => {
      // Aggregate events per window
      const regions = {};
      events.forEach(event => {
        event.quads.forEach(quad => {
          if (quad.predicate.value.includes('region')) {
            const region = quad.object.value;
            regions[region] = (regions[region] || 0) + 1;
          }
        });
      });

      return {
        count: events.length,
        regionBreakdown: regions,
        timestamp: new Date().toISOString(),
        operations: {
          inserts: events.filter(e => e.operation === 'insert').length,
          deletes: events.filter(e => e.operation === 'delete').length
        }
      };
    }
  });

  // Query across all regions
  const queryAllRegions = async () => {
    const results = await federation.query(`
      SELECT ?region (COUNT(?sale) as ?sales) WHERE {
        ?sale a :Sale ;
              :region ?region ;
              :timestamp ?ts .
        FILTER(?ts > NOW() - "PT1H"^^xsd:duration)
      }
      GROUP BY ?region
    `, {
      storeSelection: 'all',
      aggregation: 'union'
    });
    return results;
  };

  return (
    <div>
      <h1>Global Analytics Dashboard</h1>

      <section>
        <h2>Federation Health</h2>
        <p>Status: {federation.health.status}</p>
        <ul>
          {Object.entries(federation.health.stores).map(([id, status]) => (
            <li key={id}>{id}: {status.status}</li>
          ))}
        </ul>
      </section>

      <section>
        <h2>Stream Processing</h2>
        <button onClick={start}>Start</button>
        <button onClick={stop}>Stop</button>
        <p>Windows processed: {stats.windowsProcessed}</p>
        <p>Events processed: {stats.eventsProcessed}</p>
        <p>Avg window size: {stats.avgWindowSize}</p>
      </section>

      <section>
        <h2>Current Window</h2>
        {currentWindow && (
          <div>
            <p>Events: {currentWindow.size}</p>
            <p>Start: {currentWindow.startTime}</p>
          </div>
        )}
      </section>

      <section>
        <h2>Historical Windows</h2>
        {windows.slice(-10).map((window, i) => (
          <div key={window.id}>
            <h4>Window {i + 1}</h4>
            <p>Count: {window.aggregated.count}</p>
            <p>Regions: {JSON.stringify(window.aggregated.regionBreakdown)}</p>
          </div>
        ))}
      </section>
    </div>
  );
}
```

---

## State Synchronization

### Leader-Based Synchronization

Use consensus for leader election and consistent state updates:

```jsx
import {
  useFederatedSystem,
  useConsensusManager,
  useChangeFeed
} from 'unrdf/react-hooks';

function ConsensusBasedSync() {
  const federation = useFederatedSystem({
    stores: ['node-1', 'node-2', 'node-3'],
    consensusProtocol: 'raft'
  });

  const consensus = useConsensusManager({
    protocol: 'raft',
    nodeId: 'node-1', // This node's ID
    peers: ['node-2', 'node-3'],
    electionTimeout: 5000,
    heartbeatInterval: 1000
  });

  const changeFeed = useChangeFeed({
    operations: ['insert', 'delete'],
    includeMetadata: true
  });

  // Only leader can propose changes
  const proposeChange = async (quads, operation) => {
    if (consensus.state !== 'leader') {
      throw new Error(`Cannot propose - current leader is ${consensus.leader}`);
    }

    // Propose through consensus
    const result = await consensus.propose({
      operation,
      quads,
      timestamp: Date.now()
    }, {
      timeout: 10000,
      quorum: 2 // Need 2 of 3 to agree
    });

    if (result.committed) {
      // Replicate to all stores
      await federation.replicate({
        operation,
        quads
      }, {
        strategy: 'immediate',
        stores: consensus.peers
      });
    }

    return result;
  };

  // Handle incoming proposals (followers)
  useEffect(() => {
    if (consensus.state === 'follower') {
      changeFeed.changes.forEach(change => {
        // Apply changes from leader
        console.log('Applying change from leader:', change);
      });
    }
  }, [changeFeed.changes, consensus.state]);

  return (
    <div>
      <h1>Consensus-Based State Sync</h1>

      <section>
        <h2>Consensus Status</h2>
        <p>Role: <strong>{consensus.state}</strong></p>
        <p>Term: {consensus.term}</p>
        <p>Leader: {consensus.leader || 'Election in progress'}</p>
        <p>Commit Index: {consensus.commitIndex}</p>
      </section>

      {consensus.state === 'leader' && (
        <section>
          <h2>Leader Actions</h2>
          <button onClick={() => proposeChange([
            { subject: 'ex:item', predicate: 'ex:status', object: 'active' }
          ], 'insert')}>
            Propose Insert
          </button>
          <button onClick={() => consensus.stepDown()}>
            Step Down
          </button>
        </section>
      )}

      <section>
        <h2>Replication Log</h2>
        <ul>
          {consensus.log.slice(-10).map((entry, i) => (
            <li key={i}>
              Term {entry.term}: {entry.value.operation}
              ({entry.value.quads.length} quads)
            </li>
          ))}
        </ul>
      </section>
    </div>
  );
}
```

### Eventual Consistency with Conflict Resolution

For systems that can tolerate eventual consistency:

```jsx
import {
  useFederatedSystem,
  useChangeFeed,
  useOfflineStore
} from 'unrdf/react-hooks';

function EventuallyConsistentApp() {
  const federation = useFederatedSystem({
    stores: ['primary', 'replica-1', 'replica-2'],
    consensusProtocol: 'gossip', // Eventually consistent
    replicationFactor: 3
  });

  const offline = useOfflineStore({
    dbName: 'my-app-offline',
    autoSync: true,
    syncInterval: 30000,
    onConflict: async (localChange, serverData) => {
      // Last-write-wins conflict resolution
      const localTime = localChange.timestamp;
      const serverTime = serverData.timestamp;

      if (localTime > serverTime) {
        return 'local'; // Keep local version
      } else {
        return 'server'; // Accept server version
      }
    },
    onSync: (result) => {
      console.log(`Synced: ${result.synced}, Conflicts: ${result.conflicts}`);
    }
  });

  const changeFeed = useChangeFeed({
    filter: (change) => !change.metadata?.localOnly,
    operations: ['insert', 'delete', 'update']
  });

  // Optimistic local update
  const updateEntity = async (entityId, predicate, value) => {
    // 1. Update locally first (optimistic)
    await offline.insert([{
      subject: entityId,
      predicate: predicate,
      object: value,
      _localOnly: true,
      _timestamp: Date.now()
    }]);

    // 2. Replicate to federation (eventually)
    try {
      await federation.replicate({
        operation: 'insert',
        quads: [{
          subject: entityId,
          predicate: predicate,
          object: value
        }]
      }, {
        strategy: 'eventual',
        timeout: 5000
      });
    } catch (err) {
      // Will sync later via offline store
      console.log('Queued for later sync:', err.message);
    }
  };

  return (
    <div>
      <h1>Eventually Consistent App</h1>

      <section>
        <h2>Sync Status</h2>
        <p>Online: {offline.isOnline ? 'Yes' : 'No'}</p>
        <p>Pending sync: {offline.pendingCount}</p>
        <p>Last synced: {offline.lastSynced?.toLocaleString()}</p>
        <button onClick={offline.sync} disabled={!offline.isOnline}>
          Force Sync
        </button>
      </section>

      <section>
        <h2>Federation Health</h2>
        {Object.entries(federation.health.stores).map(([id, info]) => (
          <div key={id}>
            <span>{id}: {info.status}</span>
            <span> (lag: {info.replicationLag}ms)</span>
          </div>
        ))}
      </section>

      <section>
        <h2>Local Data ({offline.localOnlyCount} unsynced)</h2>
        <ul>
          {offline.quads.slice(0, 10).map((quad, i) => (
            <li key={i} style={{
              color: quad._localOnly ? 'orange' : 'inherit'
            }}>
              {quad.subject} - {quad.predicate} - {quad.object}
              {quad._localOnly && ' (pending sync)'}
            </li>
          ))}
        </ul>
      </section>
    </div>
  );
}
```

---

## Error Handling and Recovery

### Multi-Layer Error Handling

Implement comprehensive error handling for distributed systems:

```jsx
import {
  useFederatedSystem,
  useDistributedQuery,
  useErrorBoundary,
  useRecovery,
  useChangeFeed
} from 'unrdf/react-hooks';

function ResilientDistributedApp() {
  // Error boundary for UI errors
  const { hasError, error, resetError, captureError } = useErrorBoundary({
    onError: (err, info) => {
      // Log to monitoring service
      console.error('UI Error:', err, info);
    }
  });

  // Recovery with retry logic
  const recovery = useRecovery({
    maxRetries: 5,
    retryDelay: 2000 // Exponential backoff
  });

  // Federation with health monitoring
  const federation = useFederatedSystem({
    stores: ['primary', 'secondary', 'tertiary'],
    consensusProtocol: 'raft'
  });

  // Distributed query with timeout
  const { data, loading, error: queryError, execute } = useDistributedQuery(null);

  // Change feed with error handling
  const changeFeed = useChangeFeed({
    operations: ['insert', 'delete'],
    batchSize: 10
  });

  // Resilient query execution
  const executeResilientQuery = async (sparql) => {
    try {
      return await recovery.executeWithRecovery(async () => {
        // Check federation health first
        const health = await federation.refreshHealth();
        const healthyStores = Object.entries(health.stores)
          .filter(([_, info]) => info.status === 'healthy')
          .map(([id]) => id);

        if (healthyStores.length < 2) {
          throw new Error('Insufficient healthy stores');
        }

        // Execute query only on healthy stores
        return await execute(sparql, {
          stores: healthyStores,
          timeout: 10000
        });
      });
    } catch (err) {
      captureError(err, { component: 'executeResilientQuery' });
      throw err;
    }
  };

  // Network partition recovery
  const handleNetworkPartition = async () => {
    console.log('Network partition detected, attempting recovery...');

    // 1. Stop change feed
    await changeFeed.stop();

    // 2. Wait for network stability
    await new Promise(resolve => setTimeout(resolve, 5000));

    // 3. Refresh federation health
    await federation.refreshHealth();

    // 4. Re-sync any missed changes
    const { synced } = await changeFeed.replay(
      Date.now() - 60000, // Last minute
      Date.now()
    );
    console.log(`Re-synced ${synced} changes`);

    // 5. Restart change feed
    await changeFeed.start();
  };

  // Monitor for unhealthy stores
  useEffect(() => {
    const unhealthyStores = Object.entries(federation.health.stores)
      .filter(([_, info]) => info.status !== 'healthy');

    if (unhealthyStores.length > 0) {
      console.warn('Unhealthy stores detected:', unhealthyStores);

      if (unhealthyStores.length >= 2) {
        handleNetworkPartition();
      }
    }
  }, [federation.health]);

  // UI error recovery
  if (hasError) {
    return (
      <div className="error-container">
        <h1>Something went wrong</h1>
        <p>{error?.message}</p>
        <button onClick={resetError}>Try Again</button>
        <button onClick={() => window.location.reload()}>Reload Page</button>
      </div>
    );
  }

  return (
    <div>
      <h1>Resilient Distributed Application</h1>

      {/* Recovery status */}
      {recovery.isRecovering && (
        <div className="recovery-banner">
          Recovering... Attempt {recovery.retryCount}/5
        </div>
      )}

      {/* Federation health */}
      <section>
        <h2>System Health</h2>
        <table>
          <thead>
            <tr>
              <th>Store</th>
              <th>Status</th>
              <th>Latency</th>
            </tr>
          </thead>
          <tbody>
            {Object.entries(federation.health.stores).map(([id, info]) => (
              <tr key={id} className={info.status !== 'healthy' ? 'warning' : ''}>
                <td>{id}</td>
                <td>{info.status}</td>
                <td>{info.latency}ms</td>
              </tr>
            ))}
          </tbody>
        </table>
      </section>

      {/* Query interface */}
      <section>
        <h2>Query</h2>
        <button
          onClick={() => executeResilientQuery('SELECT * WHERE { ?s ?p ?o } LIMIT 10')}
          disabled={loading || recovery.isRecovering}
        >
          {loading ? 'Querying...' : 'Execute Query'}
        </button>

        {queryError && (
          <div className="error">Query failed: {queryError.message}</div>
        )}

        {data && (
          <pre>{JSON.stringify(data, null, 2)}</pre>
        )}
      </section>
    </div>
  );
}
```

### Graceful Degradation Strategies

```jsx
function GracefulDegradationApp() {
  const federation = useFederatedSystem({
    stores: ['primary', 'secondary', 'cache']
  });

  const [degradationLevel, setDegradationLevel] = useState('full');

  // Monitor health and adjust degradation level
  useEffect(() => {
    const healthyCount = Object.values(federation.health.stores)
      .filter(s => s.status === 'healthy').length;

    if (healthyCount >= 3) {
      setDegradationLevel('full');
    } else if (healthyCount >= 2) {
      setDegradationLevel('reduced');
    } else if (healthyCount >= 1) {
      setDegradationLevel('minimal');
    } else {
      setDegradationLevel('offline');
    }
  }, [federation.health]);

  // Adjust features based on degradation level
  const features = {
    full: {
      realtime: true,
      analytics: true,
      write: true,
      consistency: 'strong'
    },
    reduced: {
      realtime: true,
      analytics: false, // Disable heavy analytics
      write: true,
      consistency: 'eventual'
    },
    minimal: {
      realtime: false, // Disable real-time
      analytics: false,
      write: true,
      consistency: 'eventual'
    },
    offline: {
      realtime: false,
      analytics: false,
      write: false, // Read-only from cache
      consistency: 'none'
    }
  };

  const currentFeatures = features[degradationLevel];

  return (
    <div>
      <header>
        <h1>Application</h1>
        {degradationLevel !== 'full' && (
          <div className={`degradation-banner ${degradationLevel}`}>
            Running in {degradationLevel} mode due to system issues
          </div>
        )}
      </header>

      <main>
        {/* Conditionally render features */}
        {currentFeatures.realtime && <RealTimeDashboard />}
        {currentFeatures.analytics && <AnalyticsDashboard />}

        <DataView
          readOnly={!currentFeatures.write}
          consistency={currentFeatures.consistency}
        />
      </main>
    </div>
  );
}
```

---

## Performance Tuning

### Query Strategy Selection

Choose the right query strategy based on your requirements:

```jsx
import { useDistributedQuery } from 'unrdf/react-hooks';

function OptimizedQueries() {
  // Strategy 1: Fastest - Returns first response
  // Best for: Low latency, eventual consistency acceptable
  const fastestQuery = useDistributedQuery(`
    SELECT * WHERE { ?s a :Product } LIMIT 10
  `, {
    strategy: 'fastest',
    timeout: 5000
  });

  // Strategy 2: Quorum - Waits for majority
  // Best for: Strong consistency, read-heavy workloads
  const quorumQuery = useDistributedQuery(`
    SELECT * WHERE { ?s a :Product } LIMIT 10
  `, {
    strategy: 'quorum',
    timeout: 10000
  });

  // Strategy 3: All - Queries all stores
  // Best for: Analytics, data validation
  const allQuery = useDistributedQuery(`
    SELECT * WHERE { ?s a :Product } LIMIT 10
  `, {
    strategy: 'all',
    aggregation: 'intersection', // Only results from ALL stores
    timeout: 30000
  });

  // Strategy 4: Leader - Query only leader
  // Best for: Strong consistency writes
  const leaderQuery = useDistributedQuery(`
    SELECT * WHERE { ?s a :Product } LIMIT 10
  `, {
    strategy: 'leader',
    timeout: 10000
  });

  return (
    <div>
      <section>
        <h2>Fastest Strategy ({fastestQuery.executionStats?.duration}ms)</h2>
        <p>Results: {fastestQuery.data?.length}</p>
      </section>

      <section>
        <h2>Quorum Strategy ({quorumQuery.executionStats?.duration}ms)</h2>
        <p>Results: {quorumQuery.data?.length}</p>
        <p>Stores: {quorumQuery.executionStats?.storesQueried?.join(', ')}</p>
      </section>
    </div>
  );
}
```

### Change Feed Optimization

Optimize change feed for different use cases:

```jsx
import { useChangeFeed, useStreamProcessor } from 'unrdf/react-hooks';

function OptimizedChangeFeeds() {
  // High-frequency updates (trading, monitoring)
  const highFrequency = useChangeFeed({
    batchSize: 1,        // Process immediately
    batchInterval: 100,  // 100ms batches
    filter: (c) => c.quads.some(q => q.predicate.value.includes('price'))
  });

  // Medium-frequency updates (dashboards)
  const mediumFrequency = useChangeFeed({
    batchSize: 10,       // Batch 10 changes
    batchInterval: 1000, // 1 second batches
    operations: ['insert', 'delete']
  });

  // Low-frequency updates (analytics)
  const lowFrequency = useChangeFeed({
    batchSize: 100,      // Large batches
    batchInterval: 5000, // 5 second batches
    includeMetadata: true
  });

  // Windowed processing for analytics
  const windowedProcessor = useStreamProcessor({
    windowType: 'tumbling',
    windowSize: 60000, // 1-minute windows
    aggregator: (events) => ({
      count: events.length,
      uniqueSubjects: new Set(
        events.flatMap(e => e.quads.map(q => q.subject.value))
      ).size,
      operationBreakdown: {
        inserts: events.filter(e => e.operation === 'insert').length,
        deletes: events.filter(e => e.operation === 'delete').length
      }
    })
  });

  // Sliding window for moving averages
  const slidingProcessor = useStreamProcessor({
    windowType: 'sliding',
    windowSize: 300000,  // 5-minute window
    windowSlide: 60000,  // Slide every minute
    aggregator: (events) => {
      const prices = events
        .flatMap(e => e.quads)
        .filter(q => q.predicate.value.includes('price'))
        .map(q => parseFloat(q.object.value));

      return {
        avgPrice: prices.reduce((a, b) => a + b, 0) / prices.length,
        minPrice: Math.min(...prices),
        maxPrice: Math.max(...prices),
        count: prices.length
      };
    }
  });

  return (
    <div>
      <section>
        <h2>High-Frequency Feed</h2>
        <p>Latency: ~100ms</p>
        <p>Changes/sec: {highFrequency.stats.totalChanges}</p>
      </section>

      <section>
        <h2>Windowed Analytics</h2>
        <p>Windows: {windowedProcessor.windows.length}</p>
        <p>Events processed: {windowedProcessor.stats.eventsProcessed}</p>
      </section>

      <section>
        <h2>Moving Average (5-min sliding window)</h2>
        {slidingProcessor.windows.slice(-5).map((w, i) => (
          <div key={i}>
            Avg: ${w.aggregated.avgPrice?.toFixed(2)}
            (min: ${w.aggregated.minPrice}, max: ${w.aggregated.maxPrice})
          </div>
        ))}
      </section>
    </div>
  );
}
```

### Memory Management

```jsx
import { useChangeFeed, useStreamProcessor } from 'unrdf/react-hooks';
import { useEffect, useRef } from 'react';

function MemoryOptimizedStreaming() {
  const changeFeed = useChangeFeed({
    batchSize: 50,
    batchInterval: 2000
  });

  const processor = useStreamProcessor({
    windowType: 'tumbling',
    windowSize: 60000
  });

  // Limit change history to prevent memory bloat
  const maxChanges = 1000;
  useEffect(() => {
    if (changeFeed.changes.length > maxChanges) {
      // Clear old changes periodically
      changeFeed.clear();
    }
  }, [changeFeed.changes.length]);

  // Limit windows retained
  const maxWindows = 60; // Keep 1 hour at 1-min windows
  useEffect(() => {
    if (processor.windows.length > maxWindows) {
      // Export old windows to storage before clearing
      const oldWindows = processor.windows.slice(0, -maxWindows);
      exportToStorage(oldWindows);
      processor.clear();
    }
  }, [processor.windows.length]);

  // Memory monitoring
  const memoryUsage = useRef({ changes: 0, windows: 0 });
  useEffect(() => {
    memoryUsage.current = {
      changes: JSON.stringify(changeFeed.changes).length,
      windows: JSON.stringify(processor.windows).length
    };
  }, [changeFeed.changes, processor.windows]);

  return (
    <div>
      <h2>Memory Usage</h2>
      <p>Changes buffer: {(memoryUsage.current.changes / 1024).toFixed(1)} KB</p>
      <p>Windows buffer: {(memoryUsage.current.windows / 1024).toFixed(1)} KB</p>
      <p>Total changes: {changeFeed.stats.totalChanges}</p>
      <p>Retained changes: {changeFeed.changes.length}</p>
    </div>
  );
}

async function exportToStorage(windows) {
  // Export to IndexedDB, file, or remote storage
  console.log('Exporting', windows.length, 'windows to storage');
}
```

---

## Architecture Patterns

### Pattern 1: CQRS with Streaming

Command Query Responsibility Segregation with real-time updates:

```jsx
// Commands go through consensus
function useCommands() {
  const consensus = useConsensusManager({
    protocol: 'raft',
    nodeId: process.env.NODE_ID,
    peers: process.env.PEERS.split(',')
  });

  const command = async (operation, data) => {
    if (consensus.state !== 'leader') {
      throw new Error(`Route command to leader: ${consensus.leader}`);
    }
    return consensus.propose({ operation, data });
  };

  return { command, isLeader: consensus.state === 'leader' };
}

// Queries use streaming projections
function useQueries() {
  const changeFeed = useChangeFeed({
    operations: ['insert', 'delete']
  });

  const [projection, setProjection] = useState({});

  useEffect(() => {
    changeFeed.changes.forEach(change => {
      // Update local projection
      setProjection(prev => applyChange(prev, change));
    });
  }, [changeFeed.changes]);

  return { projection, changes: changeFeed.changes };
}

// CQRS Application
function CQRSApp() {
  const { command, isLeader } = useCommands();
  const { projection, changes } = useQueries();

  return (
    <div>
      <WritePanel
        onCommand={command}
        disabled={!isLeader}
        leaderWarning={!isLeader && 'Commands routed to leader'}
      />
      <ReadPanel projection={projection} />
      <ActivityFeed changes={changes} />
    </div>
  );
}
```

### Pattern 2: Event Sourcing with Streaming

```jsx
function EventSourcedApp() {
  const changeFeed = useChangeFeed({
    operations: ['insert'],
    includeMetadata: true
  });

  const [state, dispatch] = useReducer(eventReducer, initialState);
  const [eventLog, setEventLog] = useState([]);

  // Process events
  useEffect(() => {
    changeFeed.changes.forEach(change => {
      const event = {
        id: change.id,
        type: inferEventType(change),
        data: change.quads,
        timestamp: change.timestamp,
        metadata: change.metadata
      };

      // Persist to event log
      setEventLog(prev => [...prev, event]);

      // Update state through reducer
      dispatch(event);
    });
  }, [changeFeed.changes]);

  // Replay events for new projections
  const createProjection = async (projectionFn) => {
    return eventLog.reduce((acc, event) => projectionFn(acc, event), {});
  };

  // Snapshot for faster recovery
  const createSnapshot = () => ({
    state,
    lastEventId: eventLog[eventLog.length - 1]?.id,
    timestamp: Date.now()
  });

  return (
    <div>
      <StateView state={state} />
      <EventLog events={eventLog} />
      <ProjectionBuilder onCreate={createProjection} />
    </div>
  );
}

function eventReducer(state, event) {
  switch (event.type) {
    case 'PRODUCT_CREATED':
      return { ...state, products: [...state.products, event.data] };
    case 'PRODUCT_UPDATED':
      return {
        ...state,
        products: state.products.map(p =>
          p.id === event.data.id ? { ...p, ...event.data } : p
        )
      };
    default:
      return state;
  }
}
```

### Pattern 3: Saga Pattern for Distributed Transactions

```jsx
function SagaOrchestrator() {
  const federation = useFederatedSystem({
    stores: ['inventory', 'orders', 'payments']
  });

  const changeFeed = useChangeFeed({
    operations: ['insert', 'delete']
  });

  const executeSaga = async (sagaSteps) => {
    const completedSteps = [];

    try {
      for (const step of sagaSteps) {
        await federation.query(step.action, { stores: [step.store] });
        completedSteps.push(step);
      }
      return { success: true, steps: completedSteps };
    } catch (error) {
      // Compensate completed steps in reverse
      for (const step of completedSteps.reverse()) {
        if (step.compensation) {
          try {
            await federation.query(step.compensation, { stores: [step.store] });
          } catch (compError) {
            console.error('Compensation failed:', compError);
            // Manual intervention needed
          }
        }
      }
      return { success: false, error, compensated: completedSteps };
    }
  };

  // Example: Order saga
  const placeOrder = async (order) => {
    return executeSaga([
      {
        name: 'reserveInventory',
        store: 'inventory',
        action: `INSERT DATA { <${order.id}> :reserved "true" }`,
        compensation: `DELETE DATA { <${order.id}> :reserved "true" }`
      },
      {
        name: 'createOrder',
        store: 'orders',
        action: `INSERT DATA { <${order.id}> a :Order ; :status "pending" }`,
        compensation: `DELETE { <${order.id}> ?p ?o } WHERE { <${order.id}> ?p ?o }`
      },
      {
        name: 'processPayment',
        store: 'payments',
        action: `INSERT DATA { <${order.id}> :paid "true" }`,
        compensation: `INSERT DATA { <${order.id}> :refunded "true" }`
      }
    ]);
  };

  return { executeSaga, placeOrder };
}
```

---

## Production Deployment

### Health Monitoring

```jsx
import { useFederatedSystem, useChangeFeed } from 'unrdf/react-hooks';

function ProductionHealthMonitor() {
  const federation = useFederatedSystem({
    stores: process.env.STORES.split(',')
  });

  const changeFeed = useChangeFeed({
    batchInterval: 1000
  });

  const [metrics, setMetrics] = useState({
    throughput: 0,
    latency: { p50: 0, p95: 0, p99: 0 },
    errorRate: 0
  });

  // Calculate metrics every second
  useEffect(() => {
    const interval = setInterval(() => {
      const recentChanges = changeFeed.changes.filter(
        c => Date.now() - new Date(c.timestamp).getTime() < 1000
      );

      setMetrics({
        throughput: recentChanges.length,
        latency: calculateLatencyPercentiles(recentChanges),
        errorRate: calculateErrorRate(recentChanges)
      });
    }, 1000);

    return () => clearInterval(interval);
  }, [changeFeed.changes]);

  // Alert on degradation
  useEffect(() => {
    if (metrics.latency.p99 > 1000) {
      alertOps('High P99 latency', metrics.latency.p99);
    }
    if (metrics.errorRate > 0.01) {
      alertOps('Error rate above 1%', metrics.errorRate);
    }
  }, [metrics]);

  return (
    <div className="health-dashboard">
      <h1>Production Health</h1>

      <section>
        <h2>Federation</h2>
        <table>
          <thead>
            <tr>
              <th>Store</th>
              <th>Status</th>
              <th>Replication Lag</th>
            </tr>
          </thead>
          <tbody>
            {Object.entries(federation.health.stores).map(([id, info]) => (
              <tr key={id} className={info.status}>
                <td>{id}</td>
                <td>{info.status}</td>
                <td>{info.replicationLag}ms</td>
              </tr>
            ))}
          </tbody>
        </table>
      </section>

      <section>
        <h2>Streaming</h2>
        <p>Throughput: {metrics.throughput} changes/sec</p>
        <p>P50 Latency: {metrics.latency.p50}ms</p>
        <p>P95 Latency: {metrics.latency.p95}ms</p>
        <p>P99 Latency: {metrics.latency.p99}ms</p>
        <p>Error Rate: {(metrics.errorRate * 100).toFixed(2)}%</p>
      </section>
    </div>
  );
}

function alertOps(message, value) {
  // Send to monitoring system (PagerDuty, Slack, etc.)
  console.error(`ALERT: ${message} - ${value}`);
}
```

### Configuration Best Practices

```javascript
// config/streaming.js
export const streamingConfig = {
  development: {
    batchSize: 1,
    batchInterval: 100,
    maxRetries: 1,
    timeout: 5000
  },
  staging: {
    batchSize: 10,
    batchInterval: 500,
    maxRetries: 3,
    timeout: 15000
  },
  production: {
    batchSize: 50,
    batchInterval: 1000,
    maxRetries: 5,
    timeout: 30000
  }
};

// config/federation.js
export const federationConfig = {
  development: {
    stores: ['local'],
    consensusProtocol: 'raft',
    replicationFactor: 1
  },
  staging: {
    stores: ['staging-1', 'staging-2'],
    consensusProtocol: 'raft',
    replicationFactor: 2
  },
  production: {
    stores: process.env.FEDERATION_STORES.split(','),
    consensusProtocol: 'raft',
    replicationFactor: 3,
    electionTimeout: 5000,
    heartbeatInterval: 1000
  }
};
```

### Deployment Checklist

- [ ] **Federation**
  - [ ] Minimum 3 stores for quorum
  - [ ] Cross-region deployment for availability
  - [ ] Health check endpoints configured
  - [ ] Replication lag alerting set up

- [ ] **Streaming**
  - [ ] Batch size tuned for throughput vs latency
  - [ ] Memory limits configured
  - [ ] Backpressure handling implemented
  - [ ] Dead letter queue for failed events

- [ ] **Consensus**
  - [ ] Election timeout > 2x network latency
  - [ ] Heartbeat interval < election timeout / 3
  - [ ] Log compaction configured
  - [ ] Snapshot interval set

- [ ] **Recovery**
  - [ ] Retry policies configured
  - [ ] Circuit breakers in place
  - [ ] Graceful degradation implemented
  - [ ] Manual failover procedures documented

- [ ] **Monitoring**
  - [ ] Throughput metrics
  - [ ] Latency percentiles (P50, P95, P99)
  - [ ] Error rate tracking
  - [ ] Replication lag alerts
  - [ ] Memory usage monitoring
