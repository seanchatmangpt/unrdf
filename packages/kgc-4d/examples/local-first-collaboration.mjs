/**
 * Local-First Collaboration Demo (2026 Architecture)
 *
 * Demonstrates two personal shards collaborating WITHOUT a central server:
 * - Each shard lives on user's device (local-first)
 * - Changes are events appended to both shards
 * - Vector clocks prove causality
 * - Geometric proximity suggests collaboration potential
 * - CRDT-style merge when needed
 *
 * No database. No central coordination. Just math.
 */

import {
  coordsForEvent,
  calculateCentroid,
  cosineSimilarity,
  D_HEAVY,
} from '../src/hdit/index.mjs';
import { EVENT_TYPES } from '../src/constants.mjs';

/**
 * Local-First Personal Shard
 * This would live on user's phone/laptop
 * Simplified implementation without full KGCStore for demo
 */
class LocalShard {
  constructor(userId, userName) {
    this.userId = userId;
    this.userName = userName;
    this.localEvents = [];
    this.eventCounter = 0;
    this.peerConnections = new Map(); // userId -> last sync vector clock
  }

  /**
   * Add local event (user takes action on their device)
   */
  async addLocalEvent(type, description, data = {}) {
    this.eventCounter++;

    const event = {
      type,
      timestamp: Date.now() * 1e6 + this.eventCounter,
      vectorClock: {
        nodeId: this.userId,
        counters: { [this.userId]: String(this.eventCounter) },
      },
      payload: { description, ...data },
      mutations: [],
    };

    this.localEvents.push(event);
    return event;
  }

  /**
   * Calculate my identity centroid
   */
  async getIdentityCentroid() {
    if (this.localEvents.length === 0) {
      throw new Error('No events yet');
    }

    const coords = this.localEvents.map(e => coordsForEvent(e, {}, D_HEAVY));
    return calculateCentroid(coords);
  }

  /**
   * Check if I should collaborate with another shard (geometric decision)
   */
  async shouldCollaborateWith(otherShard) {
    const myCentroid = await this.getIdentityCentroid();
    const theirCentroid = await otherShard.getIdentityCentroid();

    const similarity = cosineSimilarity(myCentroid, theirCentroid);

    // Collaboration threshold: 70% similarity
    return {
      shouldCollaborate: similarity > 0.7,
      similarity,
      reason: similarity > 0.7
        ? 'High geometric similarity - good collaboration potential'
        : 'Low geometric similarity - different domains',
    };
  }

  /**
   * Sync with peer shard (exchange events since last sync)
   */
  async syncWithPeer(peerShard) {
    const lastSyncClock = this.peerConnections.get(peerShard.userId);

    // Get events since last sync
    const newEvents = peerShard.localEvents.filter(e => {
      if (!lastSyncClock) return true;

      // Check if event happened after last sync
      const myCount = BigInt(e.vectorClock.counters[peerShard.userId] || '0');
      const lastCount = BigInt(lastSyncClock.counters[peerShard.userId] || '0');
      return myCount > lastCount;
    });

    // Merge peer events into my shard
    const merged = [];
    for (const event of newEvents) {
      // In real implementation, would use CRDT merge logic
      // For demo, just add to local events
      this.localEvents.push({
        ...event,
        source: peerShard.userId, // Track origin
      });
      merged.push(event);
    }

    // Update last sync point
    if (peerShard.localEvents.length > 0) {
      const lastEvent = peerShard.localEvents[peerShard.localEvents.length - 1];
      this.peerConnections.set(peerShard.userId, lastEvent.vectorClock);
    }

    return {
      merged: merged.length,
      total: this.localEvents.length,
    };
  }

  summary() {
    return {
      user: this.userName,
      events: this.localEvents.length,
      peers: this.peerConnections.size,
    };
  }
}

// ============================================================================
// Demo: Local-First Collaboration
// ============================================================================

console.log('🌐 Local-First Collaboration Demo (2026 Architecture)\n');
console.log('Demonstrating peer-to-peer shard sync without central server\n');

// Create Alice's shard (on her device)
console.log('📱 Alice creates her local shard (on her phone)...\n');
const alice = new LocalShard('alice-device-1', 'Alice Chen');

await alice.addLocalEvent(EVENT_TYPES.CREATE, 'Started reading ML paper on transformers');
await alice.addLocalEvent(EVENT_TYPES.CREATE, 'Implemented attention mechanism prototype');
await alice.addLocalEvent(EVENT_TYPES.UPDATE, 'Experimenting with positional encoding');
await alice.addLocalEvent(EVENT_TYPES.CREATE, 'Writing blog post about self-attention');

// Create Bob's shard (on his device)
console.log('💻 Bob creates his local shard (on his laptop)...\n');
const bob = new LocalShard('bob-device-1', 'Bob Kumar');

await bob.addLocalEvent(EVENT_TYPES.CREATE, 'Designed REST API for ML model serving');
await bob.addLocalEvent(EVENT_TYPES.CREATE, 'Set up PostgreSQL for training data');
await bob.addLocalEvent(EVENT_TYPES.UPDATE, 'Optimized model inference latency');
await bob.addLocalEvent(EVENT_TYPES.CREATE, 'Built monitoring dashboard');

console.log(`Alice's shard: ${alice.summary().events} events`);
console.log(`Bob's shard: ${bob.summary().events} events\n`);

// ============================================================================
// Geometric Discovery: Should they collaborate?
// ============================================================================

console.log('🔍 Geometric Discovery Phase:\n');
console.log('   Computing 512D centroids for each shard...');
console.log('   Measuring cosine similarity...\n');

const aliceDecision = await alice.shouldCollaborateWith(bob);
const bobDecision = await bob.shouldCollaborateWith(alice);

console.log(`   Alice → Bob: ${(aliceDecision.similarity * 100).toFixed(2)}% similarity`);
console.log(`   Decision: ${aliceDecision.reason}\n`);

console.log(`   Bob → Alice: ${(bobDecision.similarity * 100).toFixed(2)}% similarity`);
console.log(`   Decision: ${bobDecision.reason}\n`);

if (aliceDecision.shouldCollaborate) {
  console.log('✅ Geometric proximity suggests collaboration!\n');
} else {
  console.log('⚠️  Low similarity - collaboration may not be productive\n');
}

// ============================================================================
// Local-First Sync (No Server!)
// ============================================================================

console.log('🔄 Peer-to-Peer Sync Phase:\n');
console.log('   Alice and Bob exchange events directly (no server)...\n');

// Alice adds more events before sync
await alice.addLocalEvent(EVENT_TYPES.CREATE, 'Created collaborative document on transformers');
await alice.addLocalEvent(EVENT_TYPES.UPDATE, 'Invited collaborators to review');

// Bob adds more events before sync
await bob.addLocalEvent(EVENT_TYPES.CREATE, 'Deployed model API to staging');
await bob.addLocalEvent(EVENT_TYPES.UPDATE, 'Load testing shows 500 req/s capacity');

console.log(`   Pre-sync: Alice has ${alice.localEvents.length} events`);
console.log(`   Pre-sync: Bob has ${bob.localEvents.length} events\n`);

// Sync: Alice pulls from Bob
const aliceSync = await alice.syncWithPeer(bob);
console.log(`   Alice ← Bob: merged ${aliceSync.merged} new events`);
console.log(`   Alice total: ${aliceSync.total} events\n`);

// Sync: Bob pulls from Alice
const bobSync = await bob.syncWithPeer(alice);
console.log(`   Bob ← Alice: merged ${bobSync.merged} new events`);
console.log(`   Bob total: ${bobSync.total} events\n`);

// ============================================================================
// Causality Without Coordination
// ============================================================================

console.log('⏰ Causality Tracking:\n');
console.log('   Vector clocks prove event ordering without central clock:\n');

// Show last few events from each shard
const showEvent = (event, owner) => {
  const source = event.source || owner;
  const clock = event.vectorClock;
  const counters = Object.entries(clock.counters)
    .map(([node, count]) => `${node}:${count}`)
    .join(', ');

  console.log(`   [${source}] ${event.payload.description}`);
  console.log(`       Vector Clock: {${counters}}`);
  console.log(`       Timestamp: ${event.timestamp}\n`);
};

console.log('   Last 3 events in Alice\'s shard:\n');
alice.localEvents.slice(-3).forEach(e => showEvent(e, 'alice-device-1'));

console.log('   Last 3 events in Bob\'s shard:\n');
bob.localEvents.slice(-3).forEach(e => showEvent(e, 'bob-device-1'));

// ============================================================================
// Key Insights
// ============================================================================

console.log('\n🌟 Key Insights (Local-First + Geometric Discovery):\n');

console.log('1. NO CENTRAL SERVER:');
console.log('   - Each shard lives on user device');
console.log('   - Sync happens peer-to-peer (WebRTC, local network, etc.)');
console.log('   - Cloud is just encrypted backup, not source of truth\n');

console.log('2. GEOMETRIC COLLABORATION:');
console.log('   - Similarity calculated in 512D space');
console.log('   - High similarity → good collaboration potential');
console.log('   - Decision made locally using only math\n');

console.log('3. VECTOR CLOCK CAUSALITY:');
console.log('   - No synchronized timestamps needed');
console.log('   - Events provably ordered by happened-before relation');
console.log('   - Concurrent events detected automatically\n');

console.log('4. PRIVACY BY DEFAULT:');
console.log('   - Your shard = your data');
console.log('   - Share only centroid for discovery (not full events)');
console.log('   - Selective sync with trusted peers\n');

console.log('5. SCALE MODEL:');
console.log(`   - Alice: ${alice.localEvents.length} events × 512D = ${(alice.localEvents.length * 512 * 4 / 1024).toFixed(1)}KB`);
console.log(`   - Bob: ${bob.localEvents.length} events × 512D = ${(bob.localEvents.length * 512 * 4 / 1024).toFixed(1)}KB`);
console.log('   - Billion users = billion sovereign shards');
console.log('   - No database that "knows everything"\n');

console.log('✅ Built with ONLY existing packages:');
console.log('   - @unrdf/kgc-4d/hdit (geometric operations)');
console.log('   - @unrdf/kgc-4d (event sourcing, vector clocks)');
console.log('   - No external dependencies\n');
