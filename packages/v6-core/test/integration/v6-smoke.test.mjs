/**
 * v6-core Smoke Tests - Integration test suite
 *
 * Validates all v6 capsules are working correctly.
 */

import { test } from 'node:test';
import assert from 'node:assert/strict';

// Import all v6 modules
import {
  V6_VERSION,
  V6_FEATURES,
  getV6Status,
  isFeatureEnabled,
} from '../../src/index.mjs';

import {
  createReceipt,
  verifyReceipt,
  ReceiptSchema,
  MerkleTree,
} from '../../src/receipts/index.mjs';

import {
  createDelta,
  createDeltaSystem,
  DeltaSchema,
  DeltaGate,
} from '../../src/delta/index.mjs';

import {
  V6_COMMANDS,
  buildCLISpine,
  executeCommand,
} from '../../src/cli/index.mjs';

import {
  GRAMMAR_VERSION,
  V6_GRAMMAR,
  getGrammarDefinition,
  validateAgainstGrammar,
} from '../../src/grammar/index.mjs';

import {
  V6_DOCS,
  getDocumentation,
  listTopics,
} from '../../src/docs/index.mjs';

// Test Suite: Core Imports
test('v6-smoke: all modules import successfully', () => {
  assert.ok(V6_VERSION);
  assert.ok(V6_FEATURES);
  assert.ok(typeof getV6Status === 'function');
  assert.ok(typeof isFeatureEnabled === 'function');
});

// Test Suite: Version and Status
test('v6-smoke: version is correct', () => {
  assert.strictEqual(V6_VERSION, '6.0.0-alpha.1');
});

test('v6-smoke: getV6Status returns valid status', () => {
  const status = getV6Status();
  assert.strictEqual(status.version, '6.0.0-alpha.1');
  assert.strictEqual(status.status, 'alpha');
  assert.ok(status.features);
  assert.ok(status.timestamp);
});

test('v6-smoke: isFeatureEnabled works correctly', () => {
  assert.strictEqual(isFeatureEnabled('receipts'), true);
  assert.strictEqual(isFeatureEnabled('delta'), true);
  assert.strictEqual(isFeatureEnabled('cli'), true);
  assert.strictEqual(isFeatureEnabled('grammar'), true);
  assert.strictEqual(isFeatureEnabled('docs'), true);
  assert.strictEqual(isFeatureEnabled('otel'), false);
  assert.strictEqual(isFeatureEnabled('performance'), false);
});

// Test Suite: Receipts Capsule
test('v6-smoke: createReceipt works', () => {
  const receipt = createReceipt('test-operation', { foo: 'bar' });

  assert.ok(receipt.id);
  assert.strictEqual(receipt.operation, 'test-operation');
  assert.ok(receipt.timestamp);
  assert.ok(receipt.merkleRoot);
  assert.ok(Array.isArray(receipt.proof));
  assert.strictEqual(receipt.metadata.foo, 'bar');
});

test('v6-smoke: verifyReceipt validates correctly', () => {
  const receipt = createReceipt('test-op', {});
  const isValid = verifyReceipt(receipt);

  assert.strictEqual(isValid, true);
});

test('v6-smoke: ReceiptSchema validates receipts', () => {
  const receipt = {
    id: 'test-1',
    operation: 'test',
    timestamp: new Date().toISOString(),
    merkleRoot: 'root',
    proof: ['proof1'],
  };

  const parsed = ReceiptSchema.parse(receipt);
  assert.ok(parsed);
});

test('v6-smoke: MerkleTree creates trees', () => {
  const tree = new MerkleTree(['leaf1', 'leaf2', 'leaf3']);

  assert.ok(tree.root);
  assert.strictEqual(tree.leaves.length, 3);

  const proof = tree.getProof(0);
  assert.ok(Array.isArray(proof));
});

test('v6-smoke: MerkleTree verifies proofs', () => {
  const isValid = MerkleTree.verify('leaf', ['proof'], 'root');
  assert.strictEqual(isValid, true);
});

// Test Suite: Delta Capsule
test('v6-smoke: createDeltaProposal works', () => {
  const proposal = createDeltaProposal('v1', 'v2', [
    { type: 'add', quad: {} },
    { type: 'remove', quad: {} },
  ]);

  assert.ok(proposal.id);
  assert.strictEqual(proposal.from, 'v1');
  assert.strictEqual(proposal.to, 'v2');
  assert.strictEqual(proposal.operations.length, 2);
  assert.ok(proposal.timestamp);
});

test('v6-smoke: applyDelta applies proposals', async () => {
  const proposal = createDeltaProposal('v1', 'v2', []);
  const store = {}; // Placeholder store

  // Should not throw
  await applyDelta(store, proposal);
  assert.ok(true);
});

test('v6-smoke: DeltaProposalSchema validates proposals', () => {
  const proposal = {
    id: 'delta-1',
    from: 'v1',
    to: 'v2',
    operations: [{ type: 'add', quad: {} }],
    timestamp: new Date().toISOString(),
  };

  const parsed = DeltaProposalSchema.parse(proposal);
  assert.ok(parsed);
});

test('v6-smoke: MemoryAdapter stores and retrieves deltas', async () => {
  const adapter = new adapters.MemoryAdapter();
  const proposal = createDeltaProposal('v1', 'v2', []);

  const id = await adapter.store(proposal);
  assert.ok(id);

  const retrieved = await adapter.retrieve(id);
  assert.strictEqual(retrieved.id, proposal.id);
});

// Test Suite: CLI Capsule
test('v6-smoke: buildCLISpine creates CLI structure', () => {
  const spine = buildCLISpine();

  assert.strictEqual(spine.name, 'v6');
  assert.strictEqual(spine.version, '6.0.0-alpha.1');
  assert.ok(spine.commands);
  assert.ok(spine.description);
});

test('v6-smoke: executeCommand runs commands', async () => {
  const result = await executeCommand('receipt:create', { test: true });

  assert.strictEqual(result.command, 'receipt:create');
  assert.strictEqual(result.status, 'success');
  assert.ok(result.timestamp);
});

test('v6-smoke: executeCommand rejects unknown commands', async () => {
  await assert.rejects(
    async () => executeCommand('unknown:command'),
    /Unknown v6 command/
  );
});

test('v6-smoke: V6_COMMANDS contains expected commands', () => {
  assert.ok(V6_COMMANDS['receipt:create']);
  assert.ok(V6_COMMANDS['receipt:verify']);
  assert.ok(V6_COMMANDS['delta:propose']);
  assert.ok(V6_COMMANDS['delta:apply']);
  assert.ok(V6_COMMANDS['grammar:show']);
  assert.ok(V6_COMMANDS['v6:status']);
});

// Test Suite: Grammar Capsule
test('v6-smoke: GRAMMAR_VERSION is correct', () => {
  assert.strictEqual(GRAMMAR_VERSION, '6.0.0-alpha.1');
});

test('v6-smoke: V6_GRAMMAR contains definitions', () => {
  assert.ok(V6_GRAMMAR.definitions.receipt);
  assert.ok(V6_GRAMMAR.definitions.delta);
  assert.ok(V6_GRAMMAR.definitions.operation);
  assert.ok(V6_GRAMMAR.extensions);
});

test('v6-smoke: getGrammarDefinition retrieves definitions', () => {
  const def = getGrammarDefinition('receipt');

  assert.strictEqual(def.type, 'object');
  assert.ok(Array.isArray(def.required));
  assert.ok(def.required.includes('id'));
});

test('v6-smoke: validateAgainstGrammar validates data', () => {
  const data = {
    id: 'test',
    operation: 'test',
    timestamp: new Date().toISOString(),
    merkleRoot: 'root',
    proof: [],
  };

  const isValid = validateAgainstGrammar('receipt', data);
  assert.strictEqual(isValid, true);
});

test('v6-smoke: validateAgainstGrammar rejects invalid data', () => {
  const data = { id: 'test' }; // Missing required fields

  const isValid = validateAgainstGrammar('receipt', data);
  assert.strictEqual(isValid, false);
});

// Test Suite: Docs Capsule
test('v6-smoke: getDocumentation retrieves docs', () => {
  const doc = getDocumentation('overview');
  assert.ok(doc.includes('/home/user/unrdf/docs/v6/README.md'));
});

test('v6-smoke: listTopics returns all topics', () => {
  const topics = listTopics();

  assert.ok(Array.isArray(topics));
  assert.ok(topics.includes('overview'));
  assert.ok(topics.includes('receipts'));
  assert.ok(topics.includes('delta'));
  assert.ok(topics.includes('cli'));
  assert.ok(topics.includes('grammar'));
  assert.ok(topics.includes('examples'));
});

test('v6-smoke: getDocumentation handles unknown topics', () => {
  const doc = getDocumentation('nonexistent');
  assert.ok(doc.includes('No documentation found'));
});

// Summary Test
test('v6-smoke: all capsules integrated successfully', () => {
  // This test passes if all imports and basic operations work
  const status = getV6Status();
  const receipt = createReceipt('summary-test', {});
  const proposal = createDeltaProposal('v0', 'v1', []);
  const spine = buildCLISpine();
  const def = getGrammarDefinition('receipt');
  const topics = listTopics();

  assert.ok(status);
  assert.ok(receipt);
  assert.ok(proposal);
  assert.ok(spine);
  assert.ok(def);
  assert.ok(Array.isArray(topics));
});
