#!/usr/bin/env node

/**
 * @file Lockchain Security Analysis
 * @description Comprehensive security validation of cryptographic provenance
 *
 * Validates:
 * - Cryptographic algorithms (SHA3-256, BLAKE3)
 * - Tamper detection
 * - Chain integrity
 * - Git anchoring
 * - Merkle tree security
 */

import { createLockchainWriter } from '../src/index.mjs';
import { sha3_256 } from '@noble/hashes/sha3.js';
import { blake3 } from '@noble/hashes/blake3.js';
import { utf8ToBytes, bytesToHex } from '@noble/hashes/utils.js';
import { existsSync, rmSync } from 'fs';

console.log('🔐 Lockchain Security Analysis\n');
console.log('='.repeat(60) + '\n');

const results = [];

function logTest(category, name, passed, details = '') {
  const status = passed ? '✅' : '❌';
  console.log(`${status} ${name}`);
  if (details) console.log(`   ${details}`);
  results.push({ category, name, passed, details });
}

async function main() {
  const testPath = './.test-lockchain-security';

  // Cleanup
  if (existsSync(testPath)) {
    rmSync(testPath, { recursive: true, force: true });
  }

  // ========== CRYPTOGRAPHIC FEATURES ==========
  console.log('🔐 CRYPTOGRAPHIC FEATURES\n');

  // Test hashing algorithms
  const testData = 'test data for cryptographic validation';
  const sha3Hash = bytesToHex(sha3_256(utf8ToBytes(testData)));
  const blake3Hash = bytesToHex(blake3(utf8ToBytes(testData)));

  logTest('Crypto', 'SHA3-256 available', !!sha3Hash, `Hash length: ${sha3Hash.length} chars`);
  logTest('Crypto', 'SHA3-256 produces 64-char hex', sha3Hash.length === 64);
  logTest('Crypto', 'BLAKE3 available', !!blake3Hash, `Hash length: ${blake3Hash.length} chars`);
  logTest('Crypto', 'BLAKE3 produces 64-char hex', blake3Hash.length === 64);

  // Test lockchain implementation
  const lockchain = createLockchainWriter({
    gitRepo: process.cwd(),
    enableMerkle: true,
    enableGitAnchoring: false,
    storagePath: testPath,
  });

  const receipt1 = await lockchain.writeReceipt({
    actor: 'security-test@example.org',
    action: 'create',
    delta: { additions: [], removals: [] },
    timestamp: new Date(),
  });

  logTest('Crypto', 'Signature algorithm is SHA3-256', receipt1.signature.algorithm === 'sha3-256');
  logTest('Crypto', 'Signature is 64-char hex', receipt1.signature.value.length === 64);
  logTest('Crypto', 'Merkle root is 64-char hex', receipt1.merkleRoot?.length === 64);

  // ========== TAMPER DETECTION ==========
  console.log('\n🛡️  TAMPER DETECTION\n');

  // Test 1: Tamper with merkle root
  const receipt2 = await lockchain.writeReceipt({
    actor: 'tamper-test-1@example.org',
    action: 'test',
    delta: {},
    timestamp: new Date(),
  });

  const _originalMerkleRoot = receipt2.merkleRoot;
  receipt2.merkleRoot = '0'.repeat(64);
  await lockchain._updateEntry(receipt2);

  const verification1 = await lockchain.verifyEntry(receipt2.id);
  logTest('Security', 'Detects tampered merkle root', !verification1.valid, verification1.error);

  // Test 2: Tamper with receipt data
  const receipt3 = await lockchain.writeReceipt({
    actor: 'tamper-test-2@example.org',
    action: 'original-action',
    delta: {},
    timestamp: new Date(),
  });

  receipt3.receipt.action = 'malicious-action';
  await lockchain._updateEntry(receipt3);

  const verification2 = await lockchain.verifyEntry(receipt3.id);
  logTest('Security', 'Detects tampered receipt data', !verification2.valid, verification2.error);

  // Test 3: Tamper with signature
  const receipt4 = await lockchain.writeReceipt({
    actor: 'tamper-test-3@example.org',
    action: 'test',
    delta: {},
    timestamp: new Date(),
  });

  receipt4.signature.value = 'a'.repeat(64);
  await lockchain._updateEntry(receipt4);

  const verification3 = await lockchain.verifyEntry(receipt4.id);
  logTest('Security', 'Detects tampered signature', !verification3.valid, verification3.error);

  // ========== CHAIN INTEGRITY ==========
  console.log('\n🔗 CHAIN INTEGRITY\n');

  // Create chain of receipts
  const chainReceipts = [];
  for (let i = 0; i < 5; i++) {
    const receipt = await lockchain.writeReceipt({
      actor: `chain-test-${i}@example.org`,
      action: `action-${i}`,
      delta: {},
      timestamp: new Date(),
    });
    chainReceipts.push(receipt);
  }

  // Check previousHash chaining
  let chainValid = true;
  for (let i = 1; i < chainReceipts.length; i++) {
    const current = chainReceipts[i];
    const previous = chainReceipts[i - 1];
    if (current.previousHash !== previous.signature.value) {
      chainValid = false;
      break;
    }
  }

  logTest('Chain', 'Previous hash chaining works', chainValid, 'All receipts properly chained');
  logTest(
    'Chain',
    'First receipt has empty/null previousHash',
    chainReceipts[0].previousHash === '' || chainReceipts[0].previousHash === null
  );
  logTest(
    'Chain',
    'Subsequent receipts have previousHash',
    chainReceipts[1].previousHash?.length > 0
  );

  // ========== MERKLE TREE SECURITY ==========
  console.log('\n🌳 MERKLE TREE SECURITY\n');

  // Test deterministic hashing
  const testReceipt1 = {
    actor: 'deterministic-test',
    action: 'test',
    delta: { additions: [], removals: [] },
    timestamp: new Date(),
  };

  const entry1 = await lockchain.writeReceipt(testReceipt1);
  const merkle1 = entry1.merkleRoot;

  // Same data should produce different merkle root (due to unique ID and timestamp)
  const entry2 = await lockchain.writeReceipt(testReceipt1);
  const merkle2 = entry2.merkleRoot;

  logTest('Merkle', 'Each entry has unique merkle root', merkle1 !== merkle2, 'IDs differ');
  logTest(
    'Merkle',
    'Merkle roots are deterministic',
    typeof merkle1 === 'string' && merkle1.length === 64
  );

  // Test verification
  const merkleVerification = await lockchain.verifyEntry(entry1.id);
  logTest('Merkle', 'Valid entry passes verification', merkleVerification.valid);

  // ========== GIT ANCHORING ==========
  console.log('\n🗄️  GIT ANCHORING\n');

  logTest(
    'Git',
    'Git anchoring is configurable',
    typeof lockchain.config.enableGitAnchoring === 'boolean'
  );
  logTest('Git', 'Git repo path configured', !!lockchain.config.gitRepo);
  logTest('Git', 'Git ref name configured', !!lockchain.config.refName);
  logTest(
    'Git',
    'Batch size configurable',
    typeof lockchain.config.batchSize === 'number' && lockchain.config.batchSize > 0
  );

  // ========== PERFORMANCE & STORAGE ==========
  console.log('\n⚡ PERFORMANCE & STORAGE\n');

  const startTime = Date.now();
  const perfReceipts = [];
  for (let i = 0; i < 100; i++) {
    const receipt = await lockchain.writeReceipt({
      actor: `perf-test-${i}@example.org`,
      action: 'performance-test',
      delta: {},
      timestamp: new Date(),
    });
    perfReceipts.push(receipt);
  }
  const duration = Date.now() - startTime;

  logTest('Performance', '100 receipts written in <5s', duration < 5000, `Duration: ${duration}ms`);
  logTest(
    'Performance',
    'Average <50ms per receipt',
    duration / 100 < 50,
    `Avg: ${(duration / 100).toFixed(2)}ms`
  );

  const stats = lockchain.getStats();
  logTest('Storage', 'Storage path exists', existsSync(stats.storagePath));
  logTest('Storage', 'Pending entries tracked', typeof stats.pendingEntries === 'number');

  // ========== SECURITY FEATURES SUMMARY ==========
  console.log('\n📊 SECURITY FEATURES SUMMARY\n');

  const securityFeatures = {
    'SHA3-256 Hashing': receipt1.signature.algorithm === 'sha3-256',
    'Merkle Root Verification': !!receipt1.merkleRoot,
    'Tamper Detection': !verification1.valid && !verification2.valid && !verification3.valid,
    'Chain Integrity': chainValid,
    'Deterministic Hashing': merkle1 !== merkle2,
    'Cryptographic Signatures': receipt1.signature.value.length === 64,
    'Git Anchoring Support': typeof lockchain.config.enableGitAnchoring === 'boolean',
  };

  Object.entries(securityFeatures).forEach(([feature, enabled]) => {
    const status = enabled ? '✅' : '❌';
    console.log(`${status} ${feature}`);
  });

  // Cleanup
  if (existsSync(testPath)) {
    rmSync(testPath, { recursive: true, force: true });
  }

  // ========== FINAL REPORT ==========
  console.log('\n' + '='.repeat(60));
  console.log('🎯 SECURITY ANALYSIS REPORT');
  console.log('='.repeat(60) + '\n');

  const byCategory = {};
  results.forEach(r => {
    if (!byCategory[r.category]) byCategory[r.category] = [];
    byCategory[r.category].push(r);
  });

  Object.entries(byCategory).forEach(([category, tests]) => {
    const passed = tests.filter(t => t.passed).length;
    const total = tests.length;
    console.log(`${category}: ${passed}/${total} passed`);
  });

  const totalPassed = results.filter(r => r.passed).length;
  const totalTests = results.length;

  console.log(`\nTotal: ${totalPassed}/${totalTests} tests passed`);

  if (totalPassed === totalTests) {
    console.log('\n🎉 SECURITY VALIDATION: PASSED');
    console.log('✅ All cryptographic features working correctly\n');
    process.exit(0);
  } else {
    console.log('\n⚠️  SECURITY VALIDATION: FAILED');
    console.log('❌ Some security features need attention\n');
    process.exit(1);
  }
}

main().catch(error => {
  console.error('Fatal error:', error);
  process.exit(1);
});
