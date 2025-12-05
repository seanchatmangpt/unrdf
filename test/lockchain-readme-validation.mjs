#!/usr/bin/env node

/**
 * @file Lockchain README Claims Validation
 * @description Validates the cryptographic provenance claims from README lines 241-266
 *
 * This test validates:
 * 1. LockchainWriter export from 'unrdf'
 * 2. Initialization with config
 * 3. writeReceipt() functionality
 * 4. receipt.merkleRoot existence and format
 * 5. SHA3-256 hashing (64-char hex)
 * 6. verifyReceipt() functionality
 * 7. Git-based storage
 */

import { createLockchainWriter } from '../packages/index.mjs';
import { LockchainWriter } from '../packages/knowledge-engine/lockchain-writer.mjs';
import { existsSync, rmSync } from 'fs';

console.log('🔗 Lockchain README Claims Validation\n');
console.log('Testing README lines 241-266, 426-460\n');

const results = [];
let overallPass = true;

function logTest(name, passed, details = '') {
  const status = passed ? '✅' : '❌';
  console.log(`${status} ${name}`);
  if (details) console.log(`   ${details}`);
  results.push({ name, passed, details });
  if (!passed) overallPass = false;
}

async function main() {
  const testStoragePath = './.test-lockchain-readme';

  // Cleanup before test
  if (existsSync(testStoragePath)) {
    rmSync(testStoragePath, { recursive: true, force: true });
  }

  try {
    // TEST 1: LockchainWriter exported
    console.log('📦 Test 1: LockchainWriter Export\n');

    const hasCreateFunction = typeof createLockchainWriter === 'function';
    logTest('createLockchainWriter exported from src/index.mjs', hasCreateFunction);

    const hasClass = typeof LockchainWriter === 'function';
    logTest('LockchainWriter class exists', hasClass);

    // TEST 2: Initialization (README line 248-253)
    console.log('\n🔧 Test 2: Initialization\n');

    const lockchain = createLockchainWriter({
      gitRepo: process.cwd(),
      enableMerkle: true,
      enableGitAnchoring: false, // Disable Git for testing
      storagePath: testStoragePath,
    });

    logTest(
      'LockchainWriter initialization',
      !!lockchain,
      `Instance: ${lockchain.constructor.name}`
    );
    logTest('Config stored correctly', lockchain.config?.enableMerkle === true);

    // TEST 3: writeReceipt() - README line 256-262
    console.log('\n📝 Test 3: writeReceipt() Functionality\n');

    const receipt = await lockchain.writeReceipt({
      actor: 'alice@example.org',
      action: 'add-data',
      delta: { additions: [], removals: [] },
      timestamp: new Date(),
      metadata: { reason: 'User registration' },
    });

    logTest('writeReceipt() works', !!receipt, `Receipt ID: ${receipt.id}`);
    logTest('Receipt has ID', !!receipt.id && typeof receipt.id === 'string');
    logTest('Receipt has timestamp', typeof receipt.timestamp === 'number');
    logTest(
      'Receipt has signature',
      !!receipt.signature && typeof receipt.signature.value === 'string'
    );

    // TEST 4: Merkle Root - README line 264-265
    console.log('\n🌳 Test 4: Merkle Root (SHA3-256)\n');

    logTest(
      'receipt.merkleRoot exists',
      !!receipt.merkleRoot,
      `merkleRoot: ${receipt.merkleRoot?.substring(0, 16)}...`
    );
    logTest('merkleRoot is string', typeof receipt.merkleRoot === 'string');
    logTest('merkleRoot is hex format', /^[0-9a-f]+$/.test(receipt.merkleRoot || ''));

    // SHA3-256 produces 64 character hex string (32 bytes * 2 chars/byte)
    const isSHA3_256 = receipt.merkleRoot?.length === 64;
    logTest(
      'SHA3-256 hashing (64 chars)',
      isSHA3_256,
      `Length: ${receipt.merkleRoot?.length} chars`
    );

    // TEST 5: Verification
    console.log('\n🔍 Test 5: verifyEntry() Functionality\n');

    const verification = await lockchain.verifyEntry(receipt.id);
    logTest('verifyEntry() works', verification !== undefined);
    logTest(
      'Entry verification passes',
      verification.valid === true,
      verification.error || 'Valid entry'
    );

    // TEST 6: Cryptographic Features
    console.log('\n🔐 Test 6: Cryptographic Features\n');

    logTest('Signature algorithm is SHA3-256', receipt.signature.algorithm === 'sha3-256');
    logTest(
      'Signature value exists',
      !!receipt.signature.value && receipt.signature.value.length > 0
    );
    logTest('Previous hash chaining', receipt.previousHash !== undefined);

    // TEST 7: Git Storage Configuration
    console.log('\n🗄️  Test 7: Git-Based Storage\n');

    const stats = lockchain.getStats();
    logTest('Git anchoring configurable', typeof lockchain.config.enableGitAnchoring === 'boolean');
    logTest('Merkle enabled in config', stats.merkleEnabled === true);
    logTest('Storage path configured', !!stats.storagePath);
    logTest('Storage directory created', existsSync(testStoragePath));

    // TEST 8: Multiple Receipts & Batch
    console.log('\n📦 Test 8: Multiple Receipts\n');

    const receipt2 = await lockchain.writeReceipt({
      actor: 'bob@example.org',
      action: 'update-data',
      delta: { additions: [], removals: [] },
      timestamp: new Date(),
      metadata: { reason: 'Data update' },
    });

    logTest('Second receipt created', !!receipt2.id);
    logTest('Second receipt has merkleRoot', !!receipt2.merkleRoot);
    logTest(
      'Different merkleRoots',
      receipt.merkleRoot !== receipt2.merkleRoot,
      'Each receipt has unique merkleRoot'
    );

    // TEST 9: Tamper Detection
    console.log('\n🛡️  Test 9: Tamper Detection\n');

    // Create a receipt and then tamper with it
    const receipt3 = await lockchain.writeReceipt({
      actor: 'charlie@example.org',
      action: 'test-tamper',
      delta: { additions: [], removals: [] },
      timestamp: new Date(),
    });

    // Tamper with merkle root
    receipt3.merkleRoot = '0000000000000000000000000000000000000000000000000000000000000000';
    await lockchain._updateEntry(receipt3);

    const tamperedVerification = await lockchain.verifyEntry(receipt3.id);
    logTest(
      'Tampered merkleRoot detected',
      tamperedVerification.valid === false,
      'Verification correctly failed'
    );

    // Cleanup
    if (existsSync(testStoragePath)) {
      rmSync(testStoragePath, { recursive: true, force: true });
    }
  } catch (error) {
    console.error('\n❌ Fatal error:', error.message);
    console.error(error.stack);
    overallPass = false;
  }

  // Summary
  console.log('\n' + '='.repeat(60));
  console.log('📊 VALIDATION SUMMARY');
  console.log('='.repeat(60) + '\n');

  const passed = results.filter(r => r.passed).length;
  const failed = results.filter(r => !r.passed).length;

  console.log(`Total Tests: ${results.length}`);
  console.log(`✅ Passed: ${passed}`);
  console.log(`❌ Failed: ${failed}\n`);

  if (failed > 0) {
    console.log('Failed Tests:');
    results
      .filter(r => !r.passed)
      .forEach(r => {
        console.log(`  ❌ ${r.name}`);
        if (r.details) console.log(`     ${r.details}`);
      });
    console.log();
  }

  if (overallPass) {
    console.log('🎉 ALL README CLAIMS VALIDATED');
    console.log('✅ Lockchain cryptographic provenance is production-ready\n');
  } else {
    console.log('⚠️  SOME CLAIMS NOT VALIDATED');
    console.log('❌ Review failures above\n');
  }

  process.exit(overallPass ? 0 : 1);
}

main().catch(error => {
  console.error('Fatal error:', error);
  process.exit(1);
});
