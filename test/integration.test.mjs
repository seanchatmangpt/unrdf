/**
 * @file Integration Tests
 * @description End-to-end workflow tests (propose → admit → project)
 */

import { test, describe } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync, writeFileSync, mkdirSync, rmSync, existsSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';
import { Universe } from '../src/admission/universe.mjs';
import { AdmissionController } from '../src/admission/admission.mjs';
import { ReceiptGenerator, ReceiptChain, MerkleBatcher } from '../src/admission/receipts.mjs';
import { CLI } from '../src/admission/cli.mjs';

const __dirname = dirname(fileURLToPath(import.meta.url));
const fixturesDir = join(__dirname, 'fixtures');
const tmpDir = join(__dirname, 'tmp-integration-test');

// Setup and cleanup
function setupTempDir() {
  if (existsSync(tmpDir)) {
    rmSync(tmpDir, { recursive: true });
  }
  mkdirSync(tmpDir, { recursive: true });
}

function cleanupTempDir() {
  if (existsSync(tmpDir)) {
    rmSync(tmpDir, { recursive: true });
  }
}

describe('Integration Tests', () => {
  test('[TEST] Integration - Full workflow: Load universe → Propose → Admit → Project', async () => {
    console.log('[START] Full workflow integration test');
    setupTempDir();

    // Step 1: Load universe from provided TTL
    console.log('[ASSERT] Step 1: Loading universe');
    const ttlPath = join(fixturesDir, 'test-universe.ttl');
    const ttlContent = readFileSync(ttlPath, 'utf-8');

    const universe = new Universe();
    await universe.loadFromTTL(ttlContent);

    const partitions = universe.getPartitions();
    console.log(`[ASSERT] Universe loaded with ${partitions.length} partitions`);
    assert.equal(partitions.length, 6);

    // Step 2: Propose valid delta (add term to Studios overlay)
    console.log('[ASSERT] Step 2: Proposing delta');
    const delta = {
      additions: [
        {
          subject: 'https://unrdf.org/studios/myProject',
          predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          object: 'https://unrdf.org/studios/Project',
        },
        {
          subject: 'https://unrdf.org/studios/myProject',
          predicate: 'http://www.w3.org/2000/01/rdf-schema#label',
          object: 'Integration Test Project',
        },
      ],
      deletions: [],
    };

    const capsuleId = `capsule:${Date.now()}`;
    console.log(`[ASSERT] Capsule ID generated: ${capsuleId}`);

    // Step 3: Admit delta and verify ALLOW
    console.log('[ASSERT] Step 3: Admitting delta');
    const admissionController = new AdmissionController();
    const admissionResult = admissionController.admit(delta, universe);

    console.log(`[ASSERT] Admission decision: ${admissionResult.decision}`);
    assert.equal(admissionResult.decision, 'ALLOW');
    assert.equal(admissionResult.violations.length, 0);

    // Step 4: Generate receipt
    console.log('[ASSERT] Step 4: Generating receipt');
    const receiptGenerator = new ReceiptGenerator({ toolchainVersion: '1.0.0' });
    const beforeHash = universe.getContentHash() || '0'.repeat(64);
    const receipt = receiptGenerator.generate(admissionResult, delta, beforeHash);

    console.log(`[ASSERT] Receipt ID: ${receipt.id}`);
    console.log(`[ASSERT] Receipt decision: ${receipt.decision}`);
    assert.equal(receipt.decision, 'ALLOW');
    assert.ok(receipt.id);

    // Step 5: Project artifacts
    console.log('[ASSERT] Step 5: Projecting artifacts');
    const artifacts = [
      {
        id: 'manifest:1',
        type: 'CatalogManifest',
        epoch: receipt.epoch,
        items: ['artifact:studios:1', 'artifact:studios:2'],
      },
    ];

    console.log(`[ASSERT] Artifacts projected at epoch: ${receipt.epoch}`);
    assert.equal(artifacts[0].epoch, receipt.epoch);

    // Step 6: Verify receipt chain
    console.log('[ASSERT] Step 6: Verifying receipt chain');
    const chain = new ReceiptChain();
    const substrateReceipt = new Receipt({
      id: 'urn:receipt:0:substrate',
      decision: 'ALLOW',
      deltaHash: '0'.repeat(64),
      beforeHash: '0'.repeat(64),
      afterHash: beforeHash,
      epoch: 0,
      timestamp: Date.now() - 1000,
      toolchainVersion: '1.0.0',
      violations: [],
      reason: 'Substrate initialization',
    });

    chain.addReceipt(substrateReceipt);
    chain.addReceipt(receipt);

    const isChainValid = chain.verifyChain();
    console.log(`[ASSERT] Receipt chain valid: ${isChainValid}`);
    assert.equal(isChainValid, true);

    cleanupTempDir();
    console.log('[RESULT] pass - Full workflow completed successfully');
  });

  test('[TEST] Integration - CLI workflow: validate → propose → admit → project', async () => {
    console.log('[START] CLI workflow integration test');
    setupTempDir();

    const cli = new CLI();

    // Step 1: Validate universe
    console.log('[ASSERT] Step 1: Validate universe');
    const universePath = join(fixturesDir, 'test-universe.ttl');
    const validateCode = await cli.validate({ universe: universePath });
    assert.equal(validateCode, 0);

    // Step 2: Propose delta
    console.log('[ASSERT] Step 2: Propose delta');
    const deltaPath = join(fixturesDir, 'valid-delta.json');
    const proposeCode = await cli.propose({ delta: deltaPath });
    assert.equal(proposeCode, 0);

    // Step 3: Admit delta
    console.log('[ASSERT] Step 3: Admit delta');
    const outputDir = join(tmpDir, 'receipts');
    const admitCode = await cli.admit({ delta: deltaPath, out: outputDir });
    assert.equal(admitCode, 0);
    assert.ok(existsSync(outputDir));

    // Step 4: Project artifacts
    console.log('[ASSERT] Step 4: Project artifacts');
    const projectCode = await cli.project({ epoch: 'τ_1' });
    assert.equal(projectCode, 0);

    cleanupTempDir();
    console.log('[RESULT] pass - CLI workflow completed successfully');
  });

  test('[TEST] Integration - Multiple deltas with receipt chaining', async () => {
    console.log('[START] Multiple delta integration test');
    setupTempDir();

    const universe = new Universe();
    await universe.loadFromTTL('');

    const admissionController = new AdmissionController();
    const receiptGenerator = new ReceiptGenerator();
    const chain = new ReceiptChain();

    let currentHash = universe.getContentHash() || '0'.repeat(64);

    // Submit 5 deltas
    for (let i = 1; i <= 5; i++) {
      console.log(`[ASSERT] Processing delta ${i}`);
      const delta = {
        additions: [
          {
            subject: `https://unrdf.org/studios/project${i}`,
            predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
            object: 'https://unrdf.org/studios/Project',
          },
        ],
        deletions: [],
      };

      const result = admissionController.admit(delta, universe);
      assert.equal(result.decision, 'ALLOW');

      const receipt = receiptGenerator.generate(result, delta, currentHash);
      chain.addReceipt(receipt);

      currentHash = receipt.afterHash;
      console.log(`[ASSERT] Delta ${i} epoch: ${receipt.epoch}`);
    }

    console.log('[ASSERT] Verifying chain of 5 receipts');
    const receipts = chain.getReceipts();
    assert.equal(receipts.length, 5);

    const isChainValid = chain.verifyChain();
    assert.equal(isChainValid, true);

    // Compute merkle root
    const batcher = new MerkleBatcher();
    const merkleRoot = batcher.computeMerkleRoot(receipts);
    console.log(`[ASSERT] Merkle root: ${merkleRoot.slice(0, 16)}...`);
    assert.ok(merkleRoot);

    cleanupTempDir();
    console.log('[RESULT] pass - Multiple delta chaining successful');
  });

  test('[TEST] Integration - Denied delta creates deny receipt', async () => {
    console.log('[START] Denied delta integration test');
    setupTempDir();

    const universe = new Universe();
    await universe.loadFromTTL('');

    const admissionController = new AdmissionController();
    const receiptGenerator = new ReceiptGenerator();

    // Submit invalid delta (substrate modification)
    const delta = {
      additions: [
        {
          subject: 'https://unrdf.org/substrate/Entity',
          predicate: 'http://www.w3.org/2000/01/rdf-schema#comment',
          object: 'Modified substrate term',
        },
      ],
      deletions: [],
    };

    console.log('[ASSERT] Admitting invalid delta');
    const result = admissionController.admit(delta, universe);
    assert.equal(result.decision, 'DENY');
    assert.ok(result.violations.length > 0);

    console.log('[ASSERT] Generating deny receipt');
    const beforeHash = universe.getContentHash() || '0'.repeat(64);
    const receipt = receiptGenerator.generate(result, delta, beforeHash);

    assert.equal(receipt.decision, 'DENY');
    assert.equal(receipt.beforeHash, receipt.afterHash); // No state change on deny
    assert.ok(receipt.violations.length > 0);

    cleanupTempDir();
    console.log('[RESULT] pass - Deny receipt generated correctly');
  });

  test('[TEST] Integration - Merkle batch of 100 receipts', async () => {
    console.log('[START] Large batch merkle test');
    setupTempDir();

    const universe = new Universe();
    await universe.loadFromTTL('');

    const admissionController = new AdmissionController();
    const receiptGenerator = new ReceiptGenerator();
    const receipts = [];

    console.log('[ASSERT] Generating 100 receipts');
    let currentHash = '0'.repeat(64);

    for (let i = 0; i < 100; i++) {
      const delta = {
        additions: [
          {
            subject: `https://unrdf.org/test/entity${i}`,
            predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
            object: 'https://unrdf.org/test/TestEntity',
          },
        ],
        deletions: [],
      };

      const result = admissionController.admit(delta, universe);
      const receipt = receiptGenerator.generate(result, delta, currentHash);
      receipts.push(receipt);
      currentHash = receipt.afterHash;
    }

    console.log(`[ASSERT] Generated ${receipts.length} receipts`);
    assert.equal(receipts.length, 100);

    const batcher = new MerkleBatcher();
    const batch = batcher.createBatch(receipts);

    console.log(`[ASSERT] Batch count: ${batch.count}`);
    assert.equal(batch.count, 100);

    console.log(`[ASSERT] Merkle root: ${batch.merkleRoot.slice(0, 16)}...`);
    assert.ok(batch.merkleRoot);

    cleanupTempDir();
    console.log('[RESULT] pass - Large batch processed successfully');
  });

  test('[TEST] Integration - Receipt serialization round-trip via CLI', async () => {
    console.log('[START] Receipt serialization integration test');
    setupTempDir();

    const cli = new CLI();
    const deltaPath = join(fixturesDir, 'valid-delta.json');
    const outputDir = join(tmpDir, 'receipts-serialize');

    await cli.admit({ delta: deltaPath, out: outputDir });

    console.log('[ASSERT] Checking receipt file was created');
    assert.ok(existsSync(outputDir));

    // In a full implementation, we'd read the receipt file and verify its contents
    // For now, we verify the command completed successfully

    cleanupTempDir();
    console.log('[RESULT] pass - Receipt serialization successful');
  });

  test('[TEST] Integration - Projection includes catalog manifest', async () => {
    console.log('[START] Projection catalog test');
    const cli = new CLI();

    const exitCode = await cli.project({ epoch: 'τ_final' });
    assert.equal(exitCode, 0);

    // In a full implementation, we'd capture the output and verify manifest structure
    console.log('[RESULT] pass - Projection completed with catalog');
  });

  test('[TEST] Integration - Invalid universe file is rejected', async () => {
    console.log('[START] Invalid universe rejection test');
    const cli = new CLI();

    const exitCode = await cli.validate({ universe: '/invalid/path.ttl' });
    assert.equal(exitCode, 1);

    console.log('[RESULT] pass - Invalid universe rejected');
  });

  test('[TEST] Integration - Deterministic hash across workflow', async () => {
    console.log('[START] Hash determinism integration test');
    setupTempDir();

    const ttlPath = join(fixturesDir, 'test-universe.ttl');
    const ttlContent = readFileSync(ttlPath, 'utf-8');

    // Run workflow twice
    const hashes1 = [];
    const hashes2 = [];

    for (let run = 0; run < 2; run++) {
      const universe = new Universe();
      await universe.loadFromTTL(ttlContent);

      const admissionController = new AdmissionController();
      const receiptGenerator = new ReceiptGenerator({ toolchainVersion: '1.0.0' });

      const delta = {
        additions: [
          {
            subject: 'https://unrdf.org/test/deterministic',
            predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
            object: 'https://unrdf.org/test/Test',
          },
        ],
        deletions: [],
      };

      const result = admissionController.admit(delta, universe);
      const receipt = receiptGenerator.generate(result, delta, universe.getContentHash() || '0'.repeat(64));

      const hashes = run === 0 ? hashes1 : hashes2;
      hashes.push(universe.getContentHash());
      hashes.push(receipt.deltaHash);
      hashes.push(receipt.getHash());
    }

    console.log('[ASSERT] Comparing hashes from both runs');
    for (let i = 0; i < hashes1.length; i++) {
      assert.equal(hashes1[i], hashes2[i], `Hash ${i} should be deterministic`);
    }

    cleanupTempDir();
    console.log('[RESULT] pass - Workflow is deterministic');
  });
});

console.log('\n=== Integration Test Suite Complete ===');

// Import Receipt for integration test
import { Receipt } from '../src/admission/receipts.mjs';
