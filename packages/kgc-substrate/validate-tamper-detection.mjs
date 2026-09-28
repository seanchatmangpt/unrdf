#!/usr/bin/env node
/**
 * Standalone validation script for tamper detection
 * Runs 5+ adversarial scenarios without needing vitest
 */

import { ReceiptChain } from './src/ReceiptChain.mjs';
import { TamperDetector } from './src/TamperDetector.mjs';
import { writeFileSync } from 'fs';
import { resolve } from 'path';

const GREEN = '\x1b[32m';
const RED = '\x1b[31m';
const YELLOW = '\x1b[33m';
const RESET = '\x1b[0m';

let passed = 0;
let failed = 0;

function assert(condition, message) {
  if (condition) {
    console.log(`${GREEN}✅ PASS${RESET}: ${message}`);
    passed++;
  } else {
    console.log(`${RED}❌ FAIL${RESET}: ${message}`);
    failed++;
  }
}

async function runTests() {
  console.log('\n=== Receipt Chain & Tamper Detection Validation ===\n');

  // Test 1: Valid chain verifies
  console.log('${YELLOW}Test 1: Valid Chain Verification${RESET}');
  const chain = new ReceiptChain();
  await chain.append({
    agent_id: 'agent-2',
    toolchain_version: '1.0.0',
    artifacts: [
      { type: 'code', path: 'ReceiptChain.mjs', hash: 'abc123', size_bytes: 8707 }
    ]
  });
  await chain.append({
    agent_id: 'agent-2',
    toolchain_version: '1.0.0',
    artifacts: [
      { type: 'code', path: 'TamperDetector.mjs', hash: 'def456', size_bytes: 9466 }
    ]
  });

  const detector = new TamperDetector();
  const validResult = await detector.verify(chain);
  assert(validResult.valid === true, 'Valid chain verifies');
  assert(validResult.errors.length === 0, 'No errors in valid chain');
  assert(validResult.details.blocks_checked === 2, 'Checked 2 blocks');

  // Test 2: Bit flip detected (content hash mismatch)
  console.log('\n${YELLOW}Test 2: Bit Flip Detection (Content Hash Mismatch)${RESET}');
  const blocks = chain.getAllBlocks();
  const tamperedBlock = { ...blocks[0], agent_id: 'malicious-agent' };
  const tamperedChain1 = new ReceiptChain();
  tamperedChain1.blocks.push(Object.freeze(tamperedBlock));
  tamperedChain1.blocks.push(Object.freeze(blocks[1]));

  const tamperResult1 = await detector.verify(tamperedChain1);
  assert(tamperResult1.valid === false, 'Tampered chain detected as invalid');
  assert(
    tamperResult1.errors.some(e => e.includes('content hash mismatch')),
    'Content hash mismatch detected'
  );

  // Test 3: Out-of-order blocks detected
  console.log('\n${YELLOW}Test 3: Out-of-Order Blocks Detection${RESET}');
  const chain2 = new ReceiptChain({ enforce_monotonic_time: false });
  const timestamp1 = BigInt(Date.now()) * 1_000_000n;
  const timestamp2 = timestamp1 + 1_000_000n;
  const timestamp3 = timestamp2 + 1_000_000n;

  await chain2.append({
    agent_id: 'agent-2',
    toolchain_version: '1.0.0',
    artifacts: [],
    timestamp_ns: timestamp1
  });
  await chain2.append({
    agent_id: 'agent-2',
    toolchain_version: '1.0.0',
    artifacts: [],
    timestamp_ns: timestamp3
  });
  await chain2.append({
    agent_id: 'agent-2',
    toolchain_version: '1.0.0',
    artifacts: [],
    timestamp_ns: timestamp2 // Out of order
  });

  const tamperResult2 = await detector.verify(chain2);
  assert(tamperResult2.valid === false, 'Out-of-order blocks detected');
  assert(
    tamperResult2.errors.some(e => e.includes('timestamp not monotonic')),
    'Non-monotonic timestamp detected'
  );

  // Test 4: Replay attack detected (duplicate timestamps)
  console.log('\n${YELLOW}Test 4: Replay Attack Detection (Duplicate Timestamps)${RESET}');
  const chain3 = new ReceiptChain({ enforce_monotonic_time: false });
  const timestamp = BigInt(Date.now()) * 1_000_000n;

  await chain3.append({
    agent_id: 'agent-2',
    toolchain_version: '1.0.0',
    artifacts: [],
    timestamp_ns: timestamp
  });
  await chain3.append({
    agent_id: 'agent-2',
    toolchain_version: '1.0.0',
    artifacts: [],
    timestamp_ns: timestamp // Replay
  });

  const tamperResult3 = await detector.verify(chain3);
  assert(tamperResult3.valid === false, 'Replay attack detected');
  assert(
    tamperResult3.errors.some(e => e.includes('timestamp not monotonic')),
    'Duplicate timestamp detected'
  );

  // Test 5: Chain reordering detected (before_hash mismatch)
  console.log('\n${YELLOW}Test 5: Chain Reordering Detection (before_hash mismatch)${RESET}');
  const chain4 = new ReceiptChain();
  let ts4 = BigInt(Date.now()) * 1_000_000n;
  await chain4.append({
    agent_id: 'agent-2',
    toolchain_version: '1.0.0',
    artifacts: [{ type: 'code', path: 'a.mjs', hash: 'hash1', size_bytes: 100 }],
    timestamp_ns: ts4
  });
  await chain4.append({
    agent_id: 'agent-2',
    toolchain_version: '1.0.0',
    artifacts: [{ type: 'code', path: 'b.mjs', hash: 'hash2', size_bytes: 200 }],
    timestamp_ns: ts4 + 1_000_000n
  });
  await chain4.append({
    agent_id: 'agent-2',
    toolchain_version: '1.0.0',
    artifacts: [{ type: 'code', path: 'c.mjs', hash: 'hash3', size_bytes: 300 }],
    timestamp_ns: ts4 + 2_000_000n
  });

  const blocks4 = chain4.getAllBlocks();
  const tamperedChain4 = new ReceiptChain();
  tamperedChain4.blocks.push(Object.freeze(blocks4[0]));
  tamperedChain4.blocks.push(Object.freeze(blocks4[2])); // Swap
  tamperedChain4.blocks.push(Object.freeze(blocks4[1])); // Swap

  const tamperResult4 = await detector.verify(tamperedChain4);
  assert(tamperResult4.valid === false, 'Reordered chain detected');
  assert(
    tamperResult4.errors.some(e => e.includes('before_hash mismatch')),
    'Hash chain break detected'
  );

  // Test 6: Artifact tampering detected
  console.log('\n${YELLOW}Test 6: Artifact Hash Tampering Detection${RESET}');
  const chain5 = new ReceiptChain();
  await chain5.append({
    agent_id: 'agent-2',
    toolchain_version: '1.0.0',
    artifacts: [{ type: 'code', path: 'file.mjs', hash: 'original_hash', size_bytes: 1000 }],
    timestamp_ns: BigInt(Date.now()) * 1_000_000n
  });

  const blocks5 = chain5.getAllBlocks();
  const tamperedArtifacts = [
    { type: 'code', path: 'file.mjs', hash: 'tampered_hash', size_bytes: 1000 }
  ];
  const tamperedBlock5 = { ...blocks5[0], artifacts: tamperedArtifacts };
  const tamperedChain5 = new ReceiptChain();
  tamperedChain5.blocks.push(Object.freeze(tamperedBlock5));

  const tamperResult5 = await detector.verify(tamperedChain5);
  assert(tamperResult5.valid === false, 'Artifact tampering detected');
  assert(
    tamperResult5.errors.some(e => e.includes('content hash mismatch')),
    'Artifact hash modification detected'
  );

  // Test 7: Genesis hash tampering
  console.log('\n${YELLOW}Test 7: Genesis Hash Tampering Detection${RESET}');
  const chain6 = new ReceiptChain({ genesis_hash: 'f'.repeat(64) });
  await chain6.append({
    agent_id: 'agent-2',
    toolchain_version: '1.0.0',
    artifacts: [],
    timestamp_ns: BigInt(Date.now()) * 1_000_000n
  });

  const json6 = chain6.toJSON();
  json6.genesis_hash = '0'.repeat(64); // Tamper genesis
  const tamperedChain6 = ReceiptChain.fromJSON(json6);
  const tamperResult6 = await detector.verify(tamperedChain6);

  assert(tamperResult6.valid === false, 'Genesis hash tampering detected');
  assert(
    tamperResult6.errors.some(e => e.includes('before_hash mismatch')),
    'Genesis mismatch detected'
  );

  // Generate comprehensive report
  console.log('\n${YELLOW}Test 8: Generate Tamper Detection Report${RESET}');
  const reportChain = new ReceiptChain();
  let tsReport = BigInt(Date.now()) * 1_000_000n;
  for (let i = 0; i < 5; i++) {
    await reportChain.append({
      agent_id: 'agent-2',
      toolchain_version: '1.0.0',
      artifacts: [
        { type: 'code', path: `file${i}.mjs`, hash: `hash${i}`, size_bytes: 100 * (i + 1) }
      ],
      timestamp_ns: tsReport + BigInt(i) * 1_000_000n
    });
  }

  const report = await detector.generateReport(reportChain);
  const reportPath = resolve(import.meta.url.replace('file://', '').replace('/validate-tamper-detection.mjs', ''), '__tests__/tamper-detection-report.json');
  writeFileSync(reportPath, JSON.stringify(report, null, 2));

  assert(report.chain_length === 5, 'Report shows 5 blocks');
  assert(report.verification.valid === true, 'Report shows valid chain');
  assert(report.blocks_analyzed.length === 5, 'All blocks analyzed');

  // Test 9: Base64 serialization
  console.log('\n${YELLOW}Test 9: Base64 Serialization & Deserialization${RESET}');
  const base64 = chain.toBase64();
  assert(typeof base64 === 'string', 'Base64 encoding produces string');

  const restored = ReceiptChain.fromBase64(base64);
  assert(restored.getLength() === chain.getLength(), 'Restored chain has same length');
  assert(restored.getHeadHash() === chain.getHeadHash(), 'Restored chain has same head hash');

  // Summary
  console.log('\n' + '='.repeat(60));
  console.log(`${GREEN}PASSED: ${passed}${RESET}`);
  console.log(`${RED}FAILED: ${failed}${RESET}`);
  console.log('='.repeat(60));

  if (failed === 0) {
    console.log(`\n${GREEN}✅ All tamper detection scenarios verified!${RESET}\n`);
    process.exit(0);
  } else {
    console.log(`\n${RED}❌ Some scenarios failed!${RESET}\n`);
    process.exit(1);
  }
}

runTests().catch(err => {
  console.error(`${RED}ERROR: ${err.message}${RESET}`);
  console.error(err.stack);
  process.exit(1);
});
