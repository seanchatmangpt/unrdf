/**
 * Delta CLI Integration Tests - v6-core
 *
 * Tests covering:
 * - End-to-end delta CLI command workflow
 * - propose -> apply -> verify -> export
 * - File-based delta proposal
 * - Store persistence across commands
 *
 * @module @unrdf/v6-core/test/delta/cli-integration
 */

import { test } from 'node:test';
import assert from 'node:assert/strict';
import { writeFile, unlink } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { deltaExtension } from '../../src/cli/commands/delta.mjs';
import { resetDefaultStore } from '../../src/delta/store.mjs';

// ============================================================================
// Helper Functions
// ============================================================================

/**
 * Execute a delta command
 */
async function executeDeltaCommand(verb, args, context = {}) {
  const verbHandler = deltaExtension.nouns.delta.verbs[verb];
  if (!verbHandler) {
    throw new Error(`Unknown delta verb: ${verb}`);
  }

  const validated = verbHandler.argsSchema.parse(args);
  return verbHandler.handler(validated, context);
}

// ============================================================================
// Test Suite 1: Basic Workflow (4 tests)
// ============================================================================

test('CLI Integration - propose delta with inline JSON', async () => {
  resetDefaultStore();

  const deltaJson = JSON.stringify({
    from: 'state-hash-1',
    to: 'state-hash-2',
    operations: [
      { type: 'add', subject: 'ex:Subject', predicate: 'ex:prop', object: 'value' }
    ]
  });

  const result = await executeDeltaCommand('propose', {
    delta: deltaJson,
    description: 'Add test property'
  });

  assert.strictEqual(result.proposed, true);
  assert.ok(result.delta.id);
  assert.strictEqual(result.delta.operations.length, 1);
  assert.strictEqual(result.delta.metadata.description, 'Add test property');
  assert.strictEqual(result.delta.metadata.status, 'proposed');
  assert.ok(result.admissibility);
});

test('CLI Integration - propose delta from file', async () => {
  resetDefaultStore();

  const tmpFile = join(tmpdir(), `delta-cli-test-${Date.now()}.json`);

  const deltaData = {
    from: 'state-abc',
    to: 'state-def',
    operations: [
      { type: 'add', subject: 'ex:S1', predicate: 'ex:p1', object: 'o1' },
      { type: 'modify', subject: 'ex:S2', predicate: 'ex:p2', object: 'new', oldValue: 'old' }
    ]
  };

  await writeFile(tmpFile, JSON.stringify(deltaData, null, 2), 'utf-8');

  try {
    const result = await executeDeltaCommand('propose', {
      file: tmpFile,
      description: 'File-based delta'
    });

    assert.strictEqual(result.proposed, true);
    assert.strictEqual(result.delta.operations.length, 2);
    assert.strictEqual(result.delta.from, 'state-abc');
    assert.strictEqual(result.delta.to, 'state-def');
  } finally {
    await unlink(tmpFile);
  }
});

test('CLI Integration - apply delta', async () => {
  resetDefaultStore();

  // First propose a delta
  const proposeResult = await executeDeltaCommand('propose', {
    delta: JSON.stringify({
      from: 'state-1',
      to: 'state-2',
      operations: [
        { type: 'add', subject: 'ex:Subject', predicate: 'ex:prop', object: 'value' }
      ]
    })
  });

  const deltaId = proposeResult.delta.id;

  // Then apply it
  const applyResult = await executeDeltaCommand('apply', {
    id: deltaId,
    force: false,
    dryRun: false
  });

  assert.strictEqual(applyResult.applied, true);
  assert.strictEqual(applyResult.delta.id, deltaId);
  assert.strictEqual(applyResult.delta.metadata.status, 'applied');
  assert.ok(applyResult.delta.metadata.appliedAt);
  assert.ok(applyResult.operations);
  assert.strictEqual(applyResult.operations.length, 1);
});

test('CLI Integration - dry run apply', async () => {
  resetDefaultStore();

  // Propose delta
  const proposeResult = await executeDeltaCommand('propose', {
    delta: JSON.stringify({
      from: 'state-1',
      to: 'state-2',
      operations: [
        { type: 'add', subject: 'ex:Test', predicate: 'ex:test', object: 'test' }
      ]
    })
  });

  const deltaId = proposeResult.delta.id;

  // Dry run apply
  const dryRunResult = await executeDeltaCommand('apply', {
    id: deltaId,
    dryRun: true
  });

  assert.strictEqual(dryRunResult.applied, false);
  assert.strictEqual(dryRunResult.dryRun, true);
  assert.strictEqual(dryRunResult.wouldApply, true);
});

// ============================================================================
// Test Suite 2: Verify and Export (3 tests)
// ============================================================================

test('CLI Integration - verify delta', async () => {
  resetDefaultStore();

  // Propose and apply
  const proposeResult = await executeDeltaCommand('propose', {
    delta: JSON.stringify({
      from: 'state-1',
      to: 'state-2',
      operations: [
        { type: 'add', subject: 'ex:S', predicate: 'ex:p', object: 'o' }
      ]
    })
  });

  await executeDeltaCommand('apply', {
    id: proposeResult.delta.id,
    force: true
  });

  // Verify
  const verifyResult = await executeDeltaCommand('verify', {
    id: proposeResult.delta.id
  });

  assert.strictEqual(verifyResult.verified, true);
  assert.strictEqual(verifyResult.operations.length, 1);
  assert.strictEqual(verifyResult.operations[0].verified, true);
});

test('CLI Integration - export delta as JSON', async () => {
  resetDefaultStore();

  const proposeResult = await executeDeltaCommand('propose', {
    delta: JSON.stringify({
      from: 'state-1',
      to: 'state-2',
      operations: [
        { type: 'add', subject: 'ex:S', predicate: 'ex:p', object: 'o' }
      ]
    })
  });

  const exportResult = await executeDeltaCommand('export', {
    id: proposeResult.delta.id,
    format: 'json'
  });

  assert.strictEqual(exportResult.exported, true);
  assert.strictEqual(exportResult.format, 'json');
  assert.ok(exportResult.data);
  assert.strictEqual(exportResult.data.id, proposeResult.delta.id);
});

test('CLI Integration - export delta as patch format', async () => {
  resetDefaultStore();

  const proposeResult = await executeDeltaCommand('propose', {
    delta: JSON.stringify({
      from: 'state-1',
      to: 'state-2',
      operations: [
        { type: 'add', subject: 'ex:S1', predicate: 'ex:p1', object: 'o1' },
        { type: 'remove', subject: 'ex:S2', predicate: 'ex:p2', object: 'o2' },
        { type: 'modify', subject: 'ex:S3', predicate: 'ex:p3', object: 'new', oldValue: 'old' }
      ]
    })
  });

  const exportResult = await executeDeltaCommand('export', {
    id: proposeResult.delta.id,
    format: 'patch'
  });

  assert.strictEqual(exportResult.exported, true);
  assert.strictEqual(exportResult.format, 'patch');
  assert.strictEqual(exportResult.data._format, 'patch');
  assert.ok(exportResult.data.patch.includes('+ ex:S1'));
  assert.ok(exportResult.data.patch.includes('- ex:S2'));
  assert.ok(exportResult.data.patch.includes('! ex:S3'));
});

// ============================================================================
// Test Suite 3: Error Handling (3 tests)
// ============================================================================

test('CLI Integration - apply nonexistent delta throws', async () => {
  resetDefaultStore();

  await assert.rejects(
    async () => executeDeltaCommand('apply', { id: 'nonexistent-delta' }),
    /Delta not found/
  );
});

test('CLI Integration - verify nonexistent delta throws', async () => {
  resetDefaultStore();

  await assert.rejects(
    async () => executeDeltaCommand('verify', { id: 'nonexistent-delta' }),
    /Delta not found/
  );
});

test('CLI Integration - export nonexistent delta throws', async () => {
  resetDefaultStore();

  await assert.rejects(
    async () => executeDeltaCommand('export', { id: 'nonexistent-delta' }),
    /Delta not found/
  );
});

// ============================================================================
// Test Suite 4: Complete Workflow (1 test)
// ============================================================================

test('CLI Integration - complete propose->apply->verify->export workflow', async () => {
  resetDefaultStore();

  // 1. Propose delta
  const proposeResult = await executeDeltaCommand('propose', {
    delta: JSON.stringify({
      from: 'state-initial',
      to: 'state-final',
      operations: [
        { type: 'add', subject: 'ex:Resource1', predicate: 'ex:name', object: 'Alice' },
        { type: 'add', subject: 'ex:Resource2', predicate: 'ex:name', object: 'Bob' }
      ]
    }),
    description: 'Add user resources'
  });

  assert.strictEqual(proposeResult.proposed, true);
  const deltaId = proposeResult.delta.id;

  // 2. Apply delta
  const applyResult = await executeDeltaCommand('apply', {
    id: deltaId,
    force: true
  });

  assert.strictEqual(applyResult.applied, true);

  // 3. Verify delta
  const verifyResult = await executeDeltaCommand('verify', {
    id: deltaId
  });

  assert.strictEqual(verifyResult.verified, true);

  // 4. Export as JSON
  const exportJsonResult = await executeDeltaCommand('export', {
    id: deltaId,
    format: 'json'
  });

  assert.strictEqual(exportJsonResult.exported, true);

  // 5. Export as patch
  const exportPatchResult = await executeDeltaCommand('export', {
    id: deltaId,
    format: 'patch'
  });

  assert.strictEqual(exportPatchResult.exported, true);
  assert.ok(exportPatchResult.data.patch.includes('ex:Resource1'));
  assert.ok(exportPatchResult.data.patch.includes('ex:Resource2'));
});

console.log('\n✅ All delta CLI integration tests passed');
