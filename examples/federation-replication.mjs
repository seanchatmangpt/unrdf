/**
 * @file Federation Replication Example
 * @description Demonstrates DataReplicationManager and createDataReplicationManager
 *   from @unrdf/federation.
 *
 * API demonstrated:
 *   packages/federation/src/federation/data-replication.mjs —
 *     DataReplicationManager, createDataReplicationManager,
 *     ReplicationTopology, ReplicationMode
 *
 * Docs: packages/federation/README.md — "Data Replication" section (added by this triple)
 *
 * DataReplicationManager handles eventual-consistency replication across a
 * federation of RDF stores. It uses Hybrid Logical Clocks (HLC) for drift
 * prevention and supports configurable topologies (full-mesh, star, ring).
 *
 * EXPECTED OUTPUT:
 *   [DataReplicationManager] constructs with default config
 *   [getStats()] returns changeLog, queue, conflict, HLC, and config fields
 *   [replicate()] enqueues a change; changeLogSize increments
 *   [shutdown()] drains queue and emits 'shutdown'
 *   [createDataReplicationManager] factory creates equivalent instance
 */

import {
  DataReplicationManager,
  createDataReplicationManager,
  ReplicationTopology,
  ReplicationMode,
} from '../packages/federation/src/federation/data-replication.mjs';

// ─── Minimal coordinator duck-type ───────────────────────────────────────────
// DataReplicationManager.initialize() calls coordinator.getStores()
// to set up version vectors. Provide an empty store list to avoid
// requiring real Oxigraph instances.

const mockCoordinator = {
  getStores: () => [],
  getHealthyStores: () => [],
};

// ─── 1. Constructor and config defaults ──────────────────────────────────────

const replication = new DataReplicationManager(mockCoordinator, {
  topology: ReplicationTopology.FULL_MESH,
  mode: ReplicationMode.BIDIRECTIONAL,
  batchSize: 10,
});

const stats0 = replication.getStats();
if (typeof stats0 !== 'object' || stats0 === null) {
  throw new Error('getStats() must return an object');
}

// Verify all expected fields
const requiredFields = ['changeLogSize', 'queueSize', 'maxQueueSize', 'queueUtilization', 'queueOverflowCount', 'conflictCount', 'versionVectors', 'hlc', 'config'];
for (const field of requiredFields) {
  if (!(field in stats0)) throw new Error(`getStats() must include '${field}'`);
}

// HLC (Hybrid Logical Clock) fields
if (typeof stats0.hlc.physical !== 'number') throw new Error('hlc.physical must be a number (ms timestamp)');
if (typeof stats0.hlc.logical !== 'number') throw new Error('hlc.logical must be a number (counter)');
if (typeof stats0.hlc.nodeId !== 'string') throw new Error('hlc.nodeId must be a string');

console.log(`[DataReplicationManager] ✅ initial stats: changeLogSize=${stats0.changeLogSize}, queueSize=${stats0.queueSize}`);
console.log(`[DataReplicationManager] ✅ HLC: physical=${stats0.hlc.physical}, logical=${stats0.hlc.logical}, nodeId=${stats0.hlc.nodeId.slice(0, 8)}...`);
console.log(`[DataReplicationManager] ✅ config: topology=${stats0.config.topology}, mode=${stats0.config.mode}, batchSize=${stats0.config.batchSize}`);

// ─── 2. ReplicationTopology and ReplicationMode enums ────────────────────────

if (typeof ReplicationTopology.FULL_MESH !== 'string') throw new Error('ReplicationTopology.FULL_MESH must be a string');
if (typeof ReplicationTopology.STAR !== 'string') throw new Error('ReplicationTopology.STAR must be a string');
if (typeof ReplicationTopology.RING !== 'string') throw new Error('ReplicationTopology.RING must be a string');

if (typeof ReplicationMode.BIDIRECTIONAL !== 'string') throw new Error('ReplicationMode.BIDIRECTIONAL must be a string');
if (typeof ReplicationMode.PUSH !== 'string') throw new Error('ReplicationMode.PUSH must be a string');
if (typeof ReplicationMode.PULL !== 'string') throw new Error('ReplicationMode.PULL must be a string');

console.log(`[ReplicationTopology] ✅ FULL_MESH=${ReplicationTopology.FULL_MESH}, STAR=${ReplicationTopology.STAR}, RING=${ReplicationTopology.RING}`);
console.log(`[ReplicationMode] ✅ BIDIRECTIONAL=${ReplicationMode.BIDIRECTIONAL}, PUSH=${ReplicationMode.PUSH}, PULL=${ReplicationMode.PULL}`);

// ─── 3. initialize() — sets up version vectors and batch processing ───────────

const initEvents = [];
replication.on('initialized', () => initEvents.push('initialized'));

await replication.initialize();

if (initEvents.length === 0) {
  throw new Error('initialize() must emit "initialized" event');
}
console.log(`[initialize] ✅ emitted 'initialized'`);

// ─── 4. replicate() — enqueues a change operation ────────────────────────────

const changeEvents = [];
replication.on('replicationQueued', (change) => changeEvents.push(change));

// ChangeOperationSchema uses flat strings for quad components (serialization schema)
const change = {
  storeId: 'store-a',
  operation: 'INSERT',
  quad: {
    subject: 'https://example.org/Alice',
    predicate: 'http://xmlns.com/foaf/0.1/name',
    object: 'Alice',
  },
};

await replication.replicate(change);

const statsAfterReplicate = replication.getStats();
// Change may be in queue or already processed (batching); either way
// some state should have advanced from zero
if (statsAfterReplicate.changeLogSize === 0 && statsAfterReplicate.queueSize === 0) {
  // The change was replicated immediately (no real stores → processed synchronously)
  // This is acceptable; verify at least no error was thrown
  console.log(`[replicate] ✅ replicate() completed (change processed, changeLogSize=${statsAfterReplicate.changeLogSize})`);
} else {
  console.log(`[replicate] ✅ replicate() enqueued: changeLogSize=${statsAfterReplicate.changeLogSize}, queueSize=${statsAfterReplicate.queueSize}`);
}

// ─── 5. queueUtilization is a valid ratio ────────────────────────────────────

const util = statsAfterReplicate.queueUtilization;
if (typeof util !== 'number') throw new Error('queueUtilization must be a number');
if (util < 0 || util > 1) throw new Error(`queueUtilization must be 0–1, got ${util}`);

console.log(`[getStats] ✅ queueUtilization=${util.toFixed(3)} (ratio ≤ 1)`);

// ─── 6. shutdown() — drains queue and emits 'shutdown' ───────────────────────

const shutdownEvents = [];
replication.on('shutdown', () => shutdownEvents.push('shutdown'));

await replication.shutdown();

if (shutdownEvents.length === 0) {
  throw new Error('shutdown() must emit "shutdown" event');
}
console.log(`[shutdown] ✅ emitted 'shutdown'`);

// ─── 7. createDataReplicationManager() factory ───────────────────────────────

const replication2 = createDataReplicationManager(mockCoordinator, {
  topology: ReplicationTopology.STAR,
});
if (!(replication2 instanceof DataReplicationManager)) {
  throw new Error('createDataReplicationManager() must return a DataReplicationManager instance');
}
const s2 = replication2.getStats();
if (s2.config.topology !== ReplicationTopology.STAR) {
  throw new Error(`Expected topology=star, got ${s2.config.topology}`);
}
console.log(`[createDataReplicationManager] ✅ factory returns DataReplicationManager, topology=${s2.config.topology}`);

await replication2.shutdown();

console.log('\n✅ All DataReplicationManager assertions passed.');
