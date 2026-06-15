/**
 * @file Federation Peer Query Example
 * @description Demonstrates peer management, query routing, result aggregation, and federated query
 *
 * API demonstrated: packages/federation/src/federation/peer-manager.mjs
 *                   packages/federation/src/federation/distributed-query.mjs
 * Docs: packages/federation/README.md — "Peer Manager" and "Distributed Query" sections
 *
 * This example is self-contained — no remote SPARQL endpoints required.
 * executeFederatedQuery is exercised via its failure path (unreachable endpoint,
 * short timeout) which returns { success: false, error, duration, peerId }.
 * The pure functions (aggregateResults, routeQuery) are tested with mock results.
 *
 * EXPECTED OUTPUT:
 *   [createPeerManager] register/update/remove/list lifecycle
 *   [PeerConfigSchema] rejects invalid peer config
 *   [routeQuery] broadcast/selective/first-available strategies
 *   [aggregateResults] deduplicates SPARQL JSON bindings across peers
 *   [executeFederatedQuery] returns { success: false } for unreachable endpoint
 */

import {
  createPeerManager,
  PeerConfigSchema,
  routeQuery,
  aggregateResults,
  executeFederatedQuery,
} from '../packages/federation/src/index.mjs';

// ─── 1. createPeerManager — register/update/list/remove ──────────────────────

const manager = createPeerManager();

const alice = manager.registerPeer('alice', 'https://sparql.alice.example.org', { region: 'us-east' });
manager.registerPeer('bob', 'https://sparql.bob.example.org');
manager.registerPeer('carol', 'https://sparql.carol.example.org', { region: 'eu-west' });

if (alice.id !== 'alice') throw new Error(`Expected peer id 'alice', got ${alice.id}`);
if (alice.status !== 'healthy') throw new Error(`Expected status 'healthy', got ${alice.status}`);
if (typeof alice.registeredAt !== 'number') throw new Error('Expected registeredAt: number');

const peers = manager.listPeers();
if (peers.length !== 3) throw new Error(`Expected 3 peers, got ${peers.length}`);

console.log(`[createPeerManager] ✅ registered ${peers.length} peers`);
console.log(`[createPeerManager]   alice: ${alice.endpoint} (${alice.status})`);

// Update: registering same id overwrites endpoint but preserves registeredAt
const aliceUpdated = manager.registerPeer('alice', 'https://sparql-v2.alice.example.org');
if (aliceUpdated.registeredAt !== alice.registeredAt) {
  throw new Error('Re-register must preserve original registeredAt');
}
if (aliceUpdated.endpoint !== 'https://sparql-v2.alice.example.org') {
  throw new Error('Re-register must update endpoint');
}
console.log(`[createPeerManager] ✅ re-register updates endpoint, preserves registeredAt`);

// Remove
manager.unregisterPeer('carol');
const peersAfterRemove = manager.listPeers();
if (peersAfterRemove.length !== 2) throw new Error(`Expected 2 peers after remove, got ${peersAfterRemove.length}`);
if (peersAfterRemove.some(p => p.id === 'carol')) throw new Error('carol should be unregistered');
console.log(`[createPeerManager] ✅ unregisterPeer works: ${peersAfterRemove.length} peers remain`);

// ─── 2. PeerConfigSchema — rejects invalid input ─────────────────────────────

const badPeer = PeerConfigSchema.safeParse({ id: '', endpoint: 'not-a-url' });
if (badPeer.success) throw new Error('Expected rejection of empty id and non-URL endpoint');
// Zod v4: issues are in error.issues (not error.errors)
const issues = badPeer.error.issues || [];
const messages = issues.map(e => e.message);
if (!messages.some(m => m.includes('Peer ID must not be empty'))) {
  throw new Error(`Expected 'Peer ID must not be empty' in issues: ${messages}`);
}
console.log(`[PeerConfigSchema] ✅ rejects empty id and non-URL endpoint (${issues.length} issues)`);

// ─── 3. routeQuery — routing strategies ──────────────────────────────────────

const allPeers = manager.listPeers(); // alice, bob
const query = 'SELECT * WHERE { ?s ?p ?o } LIMIT 10';

const broadcast = routeQuery(allPeers, query, 'broadcast');
if (broadcast.length !== allPeers.length) throw new Error(`broadcast should return all ${allPeers.length} peers`);

const selective = routeQuery(allPeers, query, 'selective');
if (selective.length !== allPeers.length) throw new Error('selective should return all peers (no metadata filter yet)');

const firstOnly = routeQuery(allPeers, query, 'first-available');
if (firstOnly.length !== 1) throw new Error(`first-available should return 1 peer, got ${firstOnly.length}`);

console.log(`[routeQuery] ✅ broadcast=${broadcast.length}, selective=${selective.length}, first-available=${firstOnly.length}`);

// ─── 4. aggregateResults — deduplicates SPARQL JSON bindings ─────────────────

const sparqlJsonResult = bindings => ({
  success: true,
  data: { results: { bindings } },
  duration: 10,
  peerId: 'test',
});

const peerResults = [
  sparqlJsonResult([
    { s: { value: 'Alice' }, p: { value: 'type' }, o: { value: 'Person' } },
    { s: { value: 'Bob' }, p: { value: 'type' }, o: { value: 'Person' } },
  ]),
  sparqlJsonResult([
    { s: { value: 'Bob' }, p: { value: 'type' }, o: { value: 'Person' } }, // duplicate
    { s: { value: 'Carol' }, p: { value: 'type' }, o: { value: 'Person' } },
  ]),
  { success: false, data: null, error: 'timeout', duration: 5000, peerId: 'dead-peer' },
];

const aggregated = aggregateResults(peerResults);
if (aggregated.length !== 3) {
  throw new Error(`Expected 3 deduplicated bindings, got ${aggregated.length} — Bob should appear once`);
}

console.log(`[aggregateResults] ✅ deduplicated 4 bindings (2+2 with 1 duplicate) → ${aggregated.length} unique`);
console.log(`[aggregateResults] ✅ ignores failed peer results (1 failure in input)`);

// ─── 5. executeFederatedQuery — failure path (unreachable endpoint) ───────────

const result = await executeFederatedQuery(
  'test-peer',
  'http://localhost:19999/sparql', // nothing listening here
  query,
  { timeout: 200 }, // short timeout for fast failure
);

// Contract: on failure, returns { success: false, error, duration, peerId } — never throws
if (result.success !== false) throw new Error('Expected success: false for unreachable endpoint');
if (typeof result.error !== 'string' || !result.error) throw new Error('Expected error message string');
if (typeof result.duration !== 'number') throw new Error('Expected duration: number');
if (result.peerId !== 'test-peer') throw new Error(`Expected peerId 'test-peer', got ${result.peerId}`);

console.log(`[executeFederatedQuery] ✅ returns { success: false } for unreachable endpoint`);
console.log(`[executeFederatedQuery]   error: "${result.error.slice(0, 60)}", duration: ${result.duration}ms`);

console.log('\n✅ All federation peer query assertions passed.');
