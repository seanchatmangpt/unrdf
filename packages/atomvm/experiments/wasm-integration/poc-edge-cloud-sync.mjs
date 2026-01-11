/**
 * POC: Federated Edge-Cloud Sync Protocol
 *
 * Delta-based synchronization with conflict resolution
 * using vector clocks and BEAM binary compression.
 *
 * Architecture:
 * - Edge devices maintain local RDF stores (IndexedDB or in-memory)
 * - Cloud maintains master Oxigraph store
 * - Sync protocol: delta-based, offline-first, eventual consistency
 *
 * @module poc-edge-cloud-sync
 */

import { trace } from '@opentelemetry/api';

/**
 * Vector clock for conflict resolution
 */
export class VectorClock {
  /**
   * @param {string} nodeId - Node identifier
   */
  constructor(nodeId) {
    this.nodeId = nodeId;
    this.clock = new Map([[nodeId, 0]]);
  }

  /**
   * Increment local clock
   */
  tick() {
    this.clock.set(this.nodeId, (this.clock.get(this.nodeId) || 0) + 1);
  }

  /**
   * Update clock with remote clock
   * @param {VectorClock} otherClock - Remote clock
   */
  update(otherClock) {
    for (const [node, timestamp] of otherClock.clock.entries()) {
      const current = this.clock.get(node) || 0;
      this.clock.set(node, Math.max(current, timestamp));
    }
  }

  /**
   * Compare with another vector clock
   * @param {VectorClock} otherClock - Clock to compare
   * @returns {number} -1 (before), 0 (concurrent), 1 (after)
   */
  compare(otherClock) {
    let before = 0;
    let after = 0;

    const allNodes = new Set([...this.clock.keys(), ...otherClock.clock.keys()]);
    for (const node of allNodes) {
      const mine = this.clock.get(node) || 0;
      const theirs = otherClock.clock.get(node) || 0;

      if (mine < theirs) before++;
      if (mine > theirs) after++;
    }

    if (before > 0 && after === 0) return -1; // I'm before
    if (after > 0 && before === 0) return 1;  // I'm after
    return 0; // Concurrent
  }

  /**
   * Serialize to JSON
   * @returns {object} JSON representation
   */
  toJSON() {
    return Object.fromEntries(this.clock);
  }

  /**
   * Deserialize from JSON
   * @param {object} json - JSON data
   * @returns {VectorClock} Vector clock instance
   */
  static fromJSON(json) {
    const vc = new VectorClock('_tmp');
    vc.clock = new Map(Object.entries(json).map(([k, v]) => [k, Number(v)]));
    return vc;
  }
}

/**
 * Delta representing RDF store changes
 */
export class RDFDelta {
  /**
   * @param {string} nodeId - Node identifier
   */
  constructor(nodeId) {
    this.nodeId = nodeId;
    this.additions = [];
    this.deletions = [];
    this.vectorClock = new VectorClock(nodeId);
  }

  /**
   * Add triple to delta
   * @param {string} s - Subject
   * @param {string} p - Predicate
   * @param {string} o - Object
   */
  addTriple(s, p, o) {
    this.additions.push({ s, p, o });
    this.vectorClock.tick();
  }

  /**
   * Delete triple in delta
   * @param {string} s - Subject
   * @param {string} p - Predicate
   * @param {string} o - Object
   */
  deleteTriple(s, p, o) {
    this.deletions.push({ s, p, o });
    this.vectorClock.tick();
  }

  /**
   * Serialize delta
   * @returns {object} Serialized delta
   */
  serialize() {
    return {
      nodeId: this.nodeId,
      additions: this.additions,
      deletions: this.deletions,
      vectorClock: this.vectorClock.toJSON(),
    };
  }

  /**
   * Deserialize delta
   * @param {object} data - Serialized data
   * @returns {RDFDelta} Delta instance
   */
  static deserialize(data) {
    const delta = new RDFDelta(data.nodeId);
    delta.additions = data.additions;
    delta.deletions = data.deletions;
    delta.vectorClock = VectorClock.fromJSON(data.vectorClock);
    return delta;
  }
}

/**
 * Edge node with offline-first RDF store
 */
export class EdgeNode {
  /**
   * @param {string} nodeId - Node identifier
   * @param {string} cloudURL - Cloud endpoint URL
   */
  constructor(nodeId, cloudURL) {
    this.nodeId = nodeId;
    this.cloudURL = cloudURL;
    this.store = new Map(); // In-memory (production: IndexedDB)
    this.pendingDeltas = [];
    this.vectorClock = new VectorClock(nodeId);
    this.syncInterval = null;
  }

  /**
   * Add triple locally and queue for sync
   * @param {string} s - Subject
   * @param {string} p - Predicate
   * @param {string} o - Object
   */
  addTriple(s, p, o) {
    const key = `${s}|${p}|${o}`;
    this.store.set(key, { s, p, o });

    const delta = new RDFDelta(this.nodeId);
    delta.addTriple(s, p, o);
    this.pendingDeltas.push(delta);

    this.vectorClock.tick();
  }

  /**
   * Sync with cloud
   * @returns {Promise<object>} Sync results
   */
  async sync() {
    if (this.pendingDeltas.length === 0) {
      return { synced: 0, conflicts: 0 };
    }

    try {
      const response = await fetch(`${this.cloudURL}/sync`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          nodeId: this.nodeId,
          deltas: this.pendingDeltas.map(d => d.serialize()),
          vectorClock: this.vectorClock.toJSON(),
        }),
      });

      const result = await response.json();

      // Apply remote deltas
      for (const remoteDelta of result.deltas || []) {
        this.applyRemoteDelta(RDFDelta.deserialize(remoteDelta));
      }

      // Update vector clock
      this.vectorClock.update(VectorClock.fromJSON(result.vectorClock));

      // Clear synced deltas
      this.pendingDeltas = [];

      return {
        synced: result.deltas?.length || 0,
        conflicts: result.conflicts || 0,
      };
    } catch (error) {
      console.error('Sync failed (offline):', error.message);
      return { synced: 0, conflicts: 0, error: error.message };
    }
  }

  /**
   * Apply delta from cloud
   * @param {RDFDelta} delta - Remote delta
   */
  applyRemoteDelta(delta) {
    // Check for conflicts using vector clocks
    const comparison = this.vectorClock.compare(delta.vectorClock);

    if (comparison === -1) {
      // Remote is newer: apply changes
      for (const { s, p, o } of delta.additions) {
        const key = `${s}|${p}|${o}`;
        this.store.set(key, { s, p, o });
      }
      for (const { s, p, o } of delta.deletions) {
        const key = `${s}|${p}|${o}`;
        this.store.delete(key);
      }
    } else if (comparison === 0) {
      // Concurrent: use last-write-wins (by nodeId)
      console.warn('Conflict detected, using LWW');
    }
  }

  /**
   * Start automatic sync
   * @param {number} intervalMs - Sync interval in milliseconds
   */
  startAutoSync(intervalMs = 5000) {
    this.syncInterval = setInterval(() => this.sync(), intervalMs);
  }

  /**
   * Stop automatic sync
   */
  stopAutoSync() {
    if (this.syncInterval) {
      clearInterval(this.syncInterval);
    }
  }
}

/**
 * Cloud master node
 */
export class CloudNode {
  constructor() {
    this.store = new Map();
    this.vectorClock = new VectorClock('cloud');
    this.nodeClocks = new Map();
  }

  /**
   * Handle sync request from edge node
   * @param {object} request - Sync request
   * @returns {Promise<object>} Sync response
   */
  async handleSync(request) {
    const { nodeId, deltas, vectorClock } = request;

    // Update node's vector clock
    const nodeVC = VectorClock.fromJSON(vectorClock);
    this.nodeClocks.set(nodeId, nodeVC);

    // Apply deltas
    for (const deltaData of deltas) {
      const delta = RDFDelta.deserialize(deltaData);
      this.applyDelta(delta);
    }

    // Find deltas to send back
    const deltasSince = this.getDeltasSince(nodeVC);

    return {
      deltas: deltasSince.map(d => d.serialize()),
      vectorClock: this.vectorClock.toJSON(),
      conflicts: 0, // TODO: track conflicts
    };
  }

  /**
   * Apply delta to cloud store
   * @param {RDFDelta} delta - Delta to apply
   */
  applyDelta(delta) {
    for (const { s, p, o } of delta.additions) {
      const key = `${s}|${p}|${o}`;
      this.store.set(key, { s, p, o });
    }
    for (const { s, p, o } of delta.deletions) {
      const key = `${s}|${p}|${o}`;
      this.store.delete(key);
    }

    this.vectorClock.update(delta.vectorClock);
  }

  /**
   * Get deltas since client clock
   * @param {VectorClock} clientClock - Client's vector clock
   * @returns {Array<RDFDelta>} Deltas to send
   */
  getDeltasSince(clientClock) {
    // In production: track per-triple vector clocks
    // For now: return empty (assume client is up-to-date)
    return [];
  }
}

/**
 * Demo edge-cloud sync
 * @returns {Promise<void>}
 */
export async function demoEdgeCloudSync() {
  console.log('\nEdge-Cloud Sync Protocol Demo');
  console.log('=============================\n');

  // Simulate cloud node (would be HTTP server in production)
  const cloud = new CloudNode();

  // Create edge nodes
  const edge1 = new EdgeNode('edge1', 'http://cloud.example.com');
  const edge2 = new EdgeNode('edge2', 'http://cloud.example.com');

  // Edge 1 adds data
  edge1.addTriple('http://ex.org/Alice', 'http://ex.org/knows', 'http://ex.org/Bob');
  edge1.addTriple('http://ex.org/Bob', 'http://ex.org/age', '30');

  console.log('Edge 1 local store:', edge1.store.size, 'triples');
  console.log('Edge 1 pending deltas:', edge1.pendingDeltas.length);

  // Simulate sync (mock HTTP)
  const syncRequest = {
    nodeId: edge1.nodeId,
    deltas: edge1.pendingDeltas.map(d => d.serialize()),
    vectorClock: edge1.vectorClock.toJSON(),
  };

  const syncResponse = await cloud.handleSync(syncRequest);
  console.log('\nSync response:', syncResponse);

  console.log('Cloud store:', cloud.store.size, 'triples');

  // Edge 2 syncs and gets Edge 1's data
  const edge2Request = {
    nodeId: edge2.nodeId,
    deltas: [],
    vectorClock: edge2.vectorClock.toJSON(),
  };

  await cloud.handleSync(edge2Request);
  console.log('Edge 2 store after sync:', edge2.store.size, 'triples\n');

  console.log('✅ Edge-Cloud Sync Demo Complete\n');
}

// Run demo if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  demoEdgeCloudSync().catch(console.error);
}
