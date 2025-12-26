#!/usr/bin/env node
/**
 * @fileoverview Multi-user collaborative RDF editing demo
 *
 * Demonstrates conflict-free collaborative editing with:
 * - 2+ concurrent clients
 * - Automatic conflict resolution
 * - Real-time synchronization
 * - Offline editing + merge
 *
 * Usage:
 *   # Terminal 1: Start server
 *   node examples/collab-server.mjs
 *
 *   # Terminal 2: Client Alice
 *   node examples/collab-demo.mjs alice
 *
 *   # Terminal 3: Client Bob
 *   node examples/collab-demo.mjs bob
 */

import { CollaborativeRDFGraph } from '../src/crdt/rdf-crdt.mjs';
import { WebSocketSync } from '../src/sync/websocket-sync.mjs';

const CLIENT_NAME = process.argv[2] || 'anonymous';
const WS_URL = process.env.WS_URL || 'ws://localhost:1234';
const ROOM_NAME = process.env.ROOM || 'demo-graph';

// Client colors for visual identification
const COLORS = {
  alice: '#ff6b6b',
  bob: '#4ecdc4',
  charlie: '#ffe66d',
  anonymous: '#95a5a6',
};

console.log(`\n🎨 ${CLIENT_NAME.toUpperCase()} - Collaborative RDF Demo`);
console.log(`   Connecting to: ${WS_URL}/${ROOM_NAME}\n`);

// Create collaborative graph
const graph = new CollaborativeRDFGraph();

// Setup sync
const sync = new WebSocketSync(graph, {
  url: WS_URL,
  roomName: ROOM_NAME,
  awareness: {
    user: {
      name: CLIENT_NAME,
      color: COLORS[CLIENT_NAME.toLowerCase()] || COLORS.anonymous,
    },
  },
});

// Track connection state
sync.on('status', (event) => {
  if (event.status === 'connected') {
    console.log('✅ Connected to server');
  } else if (event.status === 'disconnected') {
    console.log('❌ Disconnected from server');
  }
});

// Track sync state
sync.on('synced', (event) => {
  if (event.isSynced) {
    console.log('🔄 Synced with server');
    showStats();
  }
});

// Track other users (presence awareness)
sync.on('awareness', (state) => {
  const otherUsers = state.states.filter(
    (s) => s.clientID !== graph.clientID && s.user
  );

  if (state.added.length > 0 || state.removed.length > 0) {
    console.log(
      `\n👥 Users online: ${otherUsers.map((u) => u.user.name).join(', ') || 'none'}`
    );
  }
});

// Track graph changes
graph.onChange((changes) => {
  if (changes.added.length > 0) {
    console.log(`\n➕ Added ${changes.added.length} triple(s):`);
    changes.added.forEach((t) => {
      console.log(`   ${formatTriple(t)}`);
    });
  }

  if (changes.removed.length > 0) {
    console.log(`\n➖ Removed ${changes.removed.length} triple(s):`);
    changes.removed.forEach((t) => {
      console.log(`   ${formatTriple(t)}`);
    });
  }

  if (changes.modified.length > 0) {
    console.log(`\n✏️  Modified ${changes.modified.length} triple(s)`);
  }

  showStats();
});

// Helper: Format triple for display
function formatTriple(triple) {
  const s = shorten(triple.subject);
  const p = shorten(triple.predicate);
  const o = shorten(triple.object);
  return `${s} ${p} ${o}`;
}

// Helper: Shorten URIs for readability
function shorten(uri) {
  return uri
    .replace('http://example.org/', 'ex:')
    .replace('http://xmlns.com/foaf/0.1/', 'foaf:')
    .replace('http://www.w3.org/1999/02/22-rdf-syntax-ns#', 'rdf:');
}

// Helper: Show graph stats
function showStats() {
  const stats = graph.getStats();
  console.log(
    `📊 Stats: ${stats.active} triples, ${stats.tombstones} tombstones`
  );
}

// Demo: Simulate concurrent editing based on client
async function runDemo() {
  // Wait for initial sync
  await new Promise((resolve) => {
    const check = setInterval(() => {
      if (sync.isSynced()) {
        clearInterval(check);
        resolve();
      }
    }, 100);
  });

  console.log('\n🎬 Starting demo...\n');

  // Each client adds different triples to test conflict resolution
  if (CLIENT_NAME.toLowerCase() === 'alice') {
    console.log('Adding Alice\'s data...');

    graph.addTriple({
      subject: 'http://example.org/alice',
      predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
      object: 'http://xmlns.com/foaf/0.1/Person',
      objectType: 'uri',
    });

    await sleep(1000);

    graph.addTriple({
      subject: 'http://example.org/alice',
      predicate: 'http://xmlns.com/foaf/0.1/name',
      object: 'Alice',
      objectType: 'literal',
    });

    await sleep(2000);

    graph.addTriple({
      subject: 'http://example.org/alice',
      predicate: 'http://xmlns.com/foaf/0.1/knows',
      object: 'http://example.org/bob',
      objectType: 'uri',
    });
  } else if (CLIENT_NAME.toLowerCase() === 'bob') {
    console.log('Adding Bob\'s data...');

    await sleep(500); // Offset from Alice

    graph.addTriple({
      subject: 'http://example.org/bob',
      predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
      object: 'http://xmlns.com/foaf/0.1/Person',
      objectType: 'uri',
    });

    await sleep(1000);

    graph.addTriple({
      subject: 'http://example.org/bob',
      predicate: 'http://xmlns.com/foaf/0.1/name',
      object: 'Bob',
      objectType: 'literal',
    });

    await sleep(2000);

    // Bob also adds relationship (tests concurrent modification)
    graph.addTriple({
      subject: 'http://example.org/bob',
      predicate: 'http://xmlns.com/foaf/0.1/knows',
      object: 'http://example.org/alice',
      objectType: 'uri',
    });

    await sleep(1000);

    // Test conflict: Both Alice and Bob might add same triple
    graph.addTriple({
      subject: 'http://example.org/alice',
      predicate: 'http://xmlns.com/foaf/0.1/knows',
      object: 'http://example.org/bob',
      objectType: 'uri',
    });
  } else {
    // Observer client - just watch
    console.log('Watching for changes from other clients...');
  }

  console.log('\n✨ Demo complete. Graph will continue syncing in real-time.');
  console.log('   Try adding data from other clients!\n');

  // Show final state after settling
  await sleep(3000);
  console.log('\n📋 Final graph state:');
  graph.getTriples().forEach((triple) => {
    console.log(`   ${formatTriple(triple)}`);
  });
  console.log('');
}

// Helper: Sleep
function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

// Run demo
runDemo().catch((error) => {
  console.error('❌ Demo error:', error);
  process.exit(1);
});

// Keep process alive
process.on('SIGINT', () => {
  console.log('\n👋 Disconnecting...');
  sync.disconnect();
  setTimeout(() => process.exit(0), 500);
});
