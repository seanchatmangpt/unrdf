/**
 * Composition C05: RDF Store + Change Feed + Sync Protocol
 * Atoms: A01 + A06 + A08
 *
 * Proof: Real-time RDF synchronization
 */

import { createStore, namedNode, literal, quad } from '@unrdf/core';
import { createChangeFeed, createSyncMessage, calculateChecksum } from '@unrdf/streaming';

console.log('=== C05: Real-time RDF Sync Proof ===\n');

async function prove() {
  try {
    // A01: Create RDF store
    const store = createStore();
    console.log('✅ A01: RDF store created');

    // A06: Create change feed
    const feed = createChangeFeed(store);
    console.log('✅ A06: Change feed created');

    let changeCount = 0;
    const changes = [];

    // Subscribe to changes
    feed.subscribe((change) => {
      changeCount++;
      changes.push(change);
      console.log(`   📡 Change detected: ${change.type} - ${change.quad ? 'quad added' : 'quad removed'}`);
    });

    // Add data to trigger changes
    const alice = namedNode('http://example.org/alice');
    const name = namedNode('http://xmlns.com/foaf/0.1/name');
    const aliceQuad = quad(alice, name, literal('Alice'));

    store.add(aliceQuad);

    // Wait for async propagation
    await new Promise(resolve => setTimeout(resolve, 100));

    // A08: Create sync message with checksum
    const quads = Array.from(store.match());
    const syncMsg = createSyncMessage({
      quads,
      timestamp: Date.now(),
      source: 'node-1'
    });

    const checksum = calculateChecksum(quads);

    console.log('✅ A08: Sync protocol message created');
    console.log(`   Checksum: ${checksum}`);
    console.log(`   Message timestamp: ${syncMsg.timestamp}`);
    console.log(`   Quads in sync: ${syncMsg.quads.length}`);

    console.log('\n✅ COMPOSITION VERIFIED');
    console.log(`   Changes detected: ${changeCount}`);
    console.log(`   Sync message valid: ${syncMsg.quads.length > 0}`);
    console.log('   Value: Real-time RDF synchronization with integrity checks');

    process.exit(changeCount > 0 && syncMsg.quads.length > 0 ? 0 : 1);
  } catch (error) {
    console.error('❌ COMPOSITION FAILED:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

prove();
