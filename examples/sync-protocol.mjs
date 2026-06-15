/**
 * @file Sync Protocol Example
 * @description Demonstrates the streaming sync protocol for distributing RDF changes
 *
 * API demonstrated: packages/streaming/src/sync-protocol.mjs
 * Docs: packages/streaming/README.md — "Sync Protocol" section
 *
 * The sync protocol serializes RDF changes into tamper-detectable messages
 * that can be shipped over any transport (WebSocket, HTTP, queue) and
 * applied at the receiving end. Checksum verification catches corruption
 * and replay attacks before any changes touch the local store.
 *
 * EXPECTED OUTPUT:
 *   [createSyncMessage] message with N changes, version 1.0, sha256 checksum
 *   [parseSyncMessage] round-trip succeeds
 *   [parseSyncMessage] tampered checksum throws: "Checksum mismatch..."
 *   [calculateChecksum] deterministic: same changes = same hash
 *   [mergeSyncMessages] merged message contains all changes from both inputs
 *   [applySyncMessage] applied N changes to feed
 *   [createSyncMessageFromFeed] reconstructed message from feed history
 */

import {
  createSyncMessage,
  parseSyncMessage,
  calculateChecksum,
  mergeSyncMessages,
  applySyncMessage,
  createSyncMessageFromFeed,
} from '../packages/streaming/src/sync-protocol.mjs';

// Minimal quad shape matching what the protocol expects
function quad(s, p, o) {
  return {
    subject: { value: s },
    predicate: { value: p },
    object: { value: o },
  };
}

function change(type, q, ts = Date.now()) {
  return { type, quad: q, timestamp: ts };
}

// ─── 1. createSyncMessage ─────────────────────────────────────────────────────

const changes1 = [
  change('add', quad('https://example.org/Alice', 'rdf:type', 'foaf:Person'), 1000),
  change('add', quad('https://example.org/Alice', 'foaf:name', '"Alice"'), 1001),
];

const msg1 = createSyncMessage(changes1, { source: 'node-A', sequence: 1 });

if (msg1.version !== '1.0') throw new Error(`Expected version 1.0, got ${msg1.version}`);
if (msg1.changes.length !== 2) throw new Error(`Expected 2 changes, got ${msg1.changes.length}`);
if (!msg1.checksum || msg1.checksum.length !== 64) throw new Error(`Expected 64-char sha256 checksum, got: ${msg1.checksum}`);
if (msg1.source !== 'node-A') throw new Error(`Expected source node-A, got ${msg1.source}`);

console.log(`[createSyncMessage] ✅ message with ${msg1.changes.length} changes, version ${msg1.version}, checksum ${msg1.checksum.slice(0, 16)}…`);

// ─── 2. parseSyncMessage — round-trip ─────────────────────────────────────────

const parsed = parseSyncMessage(msg1);
if (parsed.changes.length !== 2) throw new Error('Round-trip failed: change count mismatch');

console.log(`[parseSyncMessage] ✅ round-trip succeeds, ${parsed.changes.length} changes validated`);

// ─── 3. parseSyncMessage — tampered checksum throws (the failure case) ────────

const tampered = { ...msg1, checksum: 'deadbeef'.repeat(8) };
let caught = null;
try {
  parseSyncMessage(tampered);
} catch (e) {
  caught = e;
}
if (!caught || !caught.message.includes('Checksum mismatch')) {
  throw new Error(`Expected checksum mismatch error, got: ${caught?.message ?? 'no error'}`);
}

console.log(`[parseSyncMessage] ✅ tampered checksum throws: "${caught.message}"`);

// ─── 4. calculateChecksum — deterministic ─────────────────────────────────────

const hash1 = calculateChecksum(changes1);
const hash2 = calculateChecksum([...changes1]); // different array, same contents
if (hash1 !== hash2) throw new Error('Checksum is not deterministic');
if (hash1.length !== 64) throw new Error(`Expected 64-char hex, got length ${hash1.length}`);

console.log(`[calculateChecksum] ✅ deterministic: ${hash1.slice(0, 16)}… (same for identical changes)`);

// ─── 5. mergeSyncMessages ─────────────────────────────────────────────────────

const changes2 = [
  change('add', quad('https://example.org/Bob', 'rdf:type', 'foaf:Person'), 2000),
];
const msg2 = createSyncMessage(changes2, { source: 'node-B', sequence: 1 });

const merged = mergeSyncMessages([msg1, msg2]);
if (merged.changes.length !== 3) {
  throw new Error(`Expected 3 merged changes, got ${merged.changes.length}`);
}

console.log(`[mergeSyncMessages] ✅ merged 2 messages → ${merged.changes.length} changes total`);

// ─── 6. applySyncMessage — feed duck-type ─────────────────────────────────────

const appliedChanges = [];
const mockFeed = {
  emitChange(change) {
    appliedChanges.push(change);
  },
};

const applied = applySyncMessage(mockFeed, msg1);
if (applied !== 2) throw new Error(`Expected 2 applied, got ${applied}`);
if (appliedChanges.length !== 2) throw new Error(`Feed received ${appliedChanges.length} changes, expected 2`);

console.log(`[applySyncMessage] ✅ applied ${applied} changes to feed`);

// ─── 7. createSyncMessageFromFeed ─────────────────────────────────────────────

const history = [
  change('add', quad('https://example.org/Carol', 'rdf:type', 'foaf:Person'), 3000),
  change('remove', quad('https://example.org/Carol', 'foaf:name', '"Carol"'), 3001),
];
const mockFeedWithHistory = {
  getHistory(options = {}) {
    const { since, limit } = options;
    let result = history;
    if (since !== undefined) result = result.filter(c => c.timestamp > since);
    if (limit !== undefined) result = result.slice(0, limit);
    return result;
  },
};

const feedMsg = createSyncMessageFromFeed(mockFeedWithHistory);
if (feedMsg.changes.length !== 2) throw new Error(`Expected 2 changes from feed, got ${feedMsg.changes.length}`);
if (feedMsg.source !== 'feed') throw new Error(`Expected source 'feed', got ${feedMsg.source}`);

// Also test the 'since' option
const recentMsg = createSyncMessageFromFeed(mockFeedWithHistory, { since: 3000 });
if (recentMsg.changes.length !== 1) {
  throw new Error(`Expected 1 change after since:3000, got ${recentMsg.changes.length}`);
}

console.log(`[createSyncMessageFromFeed] ✅ reconstructed ${feedMsg.changes.length} changes from feed history`);
console.log(`[createSyncMessageFromFeed] ✅ since filter: ${recentMsg.changes.length} change after timestamp 3000`);

console.log('\n✅ All sync protocol assertions passed.');
