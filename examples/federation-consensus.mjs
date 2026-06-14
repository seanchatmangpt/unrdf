/**
 * @file Federation Consensus Example
 * @description Demonstrates ConsensusManager (RAFT) and createConsensusManager
 *   from @unrdf/federation.
 *
 * API demonstrated:
 *   packages/federation/src/federation/consensus-manager.mjs —
 *     ConsensusManager, createConsensusManager, NodeState
 *
 * Docs: packages/federation/README.md — "Consensus" section (added by this triple)
 *
 * ConsensusManager implements the RAFT consensus protocol — a distributed
 * algorithm for electing a leader and replicating a log across a cluster.
 * Each node starts as FOLLOWER; an election promotes one node to LEADER.
 *
 * EXPECTED OUTPUT:
 *   [ConsensusManager] initializes in FOLLOWER state
 *   [addPeer/removePeer] peer map size changes correctly
 *   [getState()] returns full RAFT node state object
 *   [createConsensusManager] factory creates equivalent instance
 *   [destroy()] cleans up timers without throwing
 */

import {
  ConsensusManager,
  createConsensusManager,
  NodeState,
} from '../packages/federation/src/federation/consensus-manager.mjs';

// ─── 1. Constructor and initial state ────────────────────────────────────────

const node = new ConsensusManager({
  nodeId: 'node-1',
  electionTimeoutMin: 150,
  electionTimeoutMax: 300,
  heartbeatInterval: 50,
});

// State before initialize(): FOLLOWER, term=0, no peers
const initialState = node.getState();
if (typeof initialState !== 'object' || initialState === null) {
  throw new Error('getState() must return an object');
}
if (initialState.nodeId !== 'node-1') {
  throw new Error(`Expected nodeId=node-1, got ${initialState.nodeId}`);
}
if (initialState.state !== NodeState.FOLLOWER) {
  throw new Error(`Expected initial state FOLLOWER, got ${initialState.state}`);
}
if (initialState.currentTerm !== 0) {
  throw new Error(`Expected currentTerm=0, got ${initialState.currentTerm}`);
}
if (typeof initialState.logLength !== 'number') {
  throw new Error('getState() must include logLength');
}
if (typeof initialState.peerCount !== 'number') {
  throw new Error('getState() must include peerCount');
}
if (typeof initialState.stats !== 'object') {
  throw new Error('getState() must include stats object');
}

console.log(`[ConsensusManager] ✅ initial state: nodeId=${initialState.nodeId}, state=${initialState.state}, term=${initialState.currentTerm}`);

// ─── 2. addPeer() / removePeer() ─────────────────────────────────────────────

node.addPeer('node-2', 'http://node2:8080');
node.addPeer('node-3', 'http://node3:8080');

const stateWithPeers = node.getState();
if (stateWithPeers.peerCount !== 2) {
  throw new Error(`Expected peerCount=2 after addPeer x2, got ${stateWithPeers.peerCount}`);
}
console.log(`[addPeer] ✅ peerCount=${stateWithPeers.peerCount} after adding 2 peers`);

node.removePeer('node-3');
const stateAfterRemove = node.getState();
if (stateAfterRemove.peerCount !== 1) {
  throw new Error(`Expected peerCount=1 after removePeer, got ${stateAfterRemove.peerCount}`);
}
console.log(`[removePeer] ✅ peerCount=${stateAfterRemove.peerCount} after removing 1 peer`);

// ─── 3. initialize() — starts election timer, emits 'initialized' ────────────

const events = [];
node.on('initialized', (data) => events.push({ type: 'initialized', data }));
node.on('stateChange', (newState) => events.push({ type: 'stateChange', newState }));

await node.initialize();

if (events.length === 0 || events[0].type !== 'initialized') {
  throw new Error('initialize() must emit "initialized" event');
}
if (events[0].data.nodeId !== 'node-1') {
  throw new Error(`initialized event must carry nodeId, got ${JSON.stringify(events[0].data)}`);
}

console.log(`[initialize] ✅ emitted 'initialized' with nodeId=${events[0].data.nodeId}`);

// ─── 4. getState() shape — full RAFT state fields ────────────────────────────

const fullState = node.getState();
const requiredFields = ['nodeId', 'state', 'currentTerm', 'leaderId', 'logLength', 'commitIndex', 'lastApplied', 'peerCount', 'stats'];
for (const field of requiredFields) {
  if (!(field in fullState)) throw new Error(`getState() must include '${field}' field`);
}

const requiredStatFields = ['electionsStarted', 'electionsWon', 'heartbeatsSent', 'heartbeatsReceived', 'logEntriesReplicated'];
for (const field of requiredStatFields) {
  if (!(field in fullState.stats)) throw new Error(`getState().stats must include '${field}'`);
}

console.log(`[getState] ✅ all RAFT state fields present: ${requiredFields.join(', ')}`);
console.log(`[getState] ✅ stats fields: ${requiredStatFields.join(', ')}`);

// ─── 5. NodeState enum ────────────────────────────────────────────────────────

if (typeof NodeState.FOLLOWER !== 'string') throw new Error('NodeState.FOLLOWER must be a string');
if (typeof NodeState.CANDIDATE !== 'string') throw new Error('NodeState.CANDIDATE must be a string');
if (typeof NodeState.LEADER !== 'string') throw new Error('NodeState.LEADER must be a string');

console.log(`[NodeState] ✅ FOLLOWER=${NodeState.FOLLOWER}, CANDIDATE=${NodeState.CANDIDATE}, LEADER=${NodeState.LEADER}`);

// ─── 6. createConsensusManager() factory ─────────────────────────────────────

const node2 = createConsensusManager({
  nodeId: 'factory-node',
});
if (!(node2 instanceof ConsensusManager)) {
  throw new Error('createConsensusManager() must return a ConsensusManager instance');
}
const n2state = node2.getState();
if (n2state.nodeId !== 'factory-node') {
  throw new Error(`Expected nodeId=factory-node, got ${n2state.nodeId}`);
}
console.log(`[createConsensusManager] ✅ factory returns ConsensusManager, nodeId=${n2state.nodeId}`);

// ─── 7. destroy() — clean teardown ───────────────────────────────────────────

// destroy() is synchronous and clears all timers/structures
node.destroy();
node2.destroy();

// After destroy, state should still be readable (object fields remain)
// Calling destroy() twice must not throw
node.destroy();

console.log(`[destroy] ✅ destroy() completes without error; double-destroy safe`);

console.log('\n✅ All ConsensusManager assertions passed.');
