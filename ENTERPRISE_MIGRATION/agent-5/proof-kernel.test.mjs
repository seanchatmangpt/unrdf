/**
 * @fileoverview Comprehensive test suite for proof kernel system.
 * Tests capsules, receipts, hash chains, and tamper detection.
 */

import { strict as assert } from 'node:assert';
import { describe, it } from 'node:test';
import {
  // Hash
  sha256Hex,
  sha256Prefixed,
  hashChain,
  isValidSha256Hex,
  isValidSha256Prefixed,
  // Canonicalization
  canonicalize,
  areCanonicallyEqual,
  sortKeys,
  // Capsule
  createCapsule,
  validateCapsule,
  serializeCapsule,
  deserializeCapsule,
  attachReceipt,
  extractCapsuleContent,
  getCapsuleSummary,
  // Receipt
  generateReceipt,
  verifyReceipt,
  verifyChain,
  createReceiptChain,
  getReceiptHash,
  getReceiptSummary,
  // Tamper detection
  detectTamper,
  auditChain,
  findFirstTamper,
  verifyChainContinuity,
  generateChainSummary,
} from './index.mjs';

describe('Hash utilities', () => {
  it('should compute SHA-256 hex hash', () => {
    const hash = sha256Hex('test data');
    assert.equal(hash.length, 64);
    assert.match(hash, /^[a-f0-9]{64}$/);
  });

  it('should compute SHA-256 prefixed hash', () => {
    const hash = sha256Prefixed('test data');
    assert.ok(hash.startsWith('sha256:'));
    assert.equal(hash.length, 71); // 'sha256:' + 64 chars
  });

  it('should produce deterministic hashes', () => {
    const hash1 = sha256Hex('identical data');
    const hash2 = sha256Hex('identical data');
    assert.equal(hash1, hash2);
  });

  it('should create hash chain', () => {
    const items = ['first', 'second', 'third'];
    const hashes = hashChain(items);

    assert.equal(hashes.length, 3);
    assert.ok(hashes.every(h => isValidSha256Hex(h)));

    // Each hash should be different
    assert.notEqual(hashes[0], hashes[1]);
    assert.notEqual(hashes[1], hashes[2]);
  });

  it('should validate SHA-256 hex format', () => {
    const validHash = sha256Hex('test');
    assert.ok(isValidSha256Hex(validHash));
    assert.ok(!isValidSha256Hex('invalid'));
    assert.ok(!isValidSha256Hex('abc123')); // too short
  });

  it('should validate SHA-256 prefixed format', () => {
    const validHash = sha256Prefixed('test');
    assert.ok(isValidSha256Prefixed(validHash));
    assert.ok(!isValidSha256Prefixed('sha256:invalid'));
    assert.ok(!isValidSha256Prefixed('invalid:abc123'));
  });
});

describe('Canonicalization', () => {
  it('should canonicalize primitives', () => {
    assert.equal(canonicalize(null), 'null');
    assert.equal(canonicalize(true), 'true');
    assert.equal(canonicalize(false), 'false');
    assert.equal(canonicalize(42), '42');
    assert.equal(canonicalize('hello'), '"hello"');
  });

  it('should canonicalize arrays', () => {
    const arr = [1, 'two', null, true];
    const canonical = canonicalize(arr);
    assert.equal(canonical, '[1,"two",null,true]');
  });

  it('should canonicalize objects with sorted keys', () => {
    const obj = { z: 1, a: 2, m: 3 };
    const canonical = canonicalize(obj);
    assert.equal(canonical, '{"a":2,"m":3,"z":1}');
  });

  it('should produce deterministic output', () => {
    const obj1 = { b: 2, a: 1, c: 3 };
    const obj2 = { a: 1, c: 3, b: 2 };
    assert.equal(canonicalize(obj1), canonicalize(obj2));
  });

  it('should handle nested structures', () => {
    const nested = {
      outer: {
        inner: [1, 2, { deep: 'value' }],
      },
    };
    const canonical = canonicalize(nested);
    assert.ok(canonical.includes('"deep":"value"'));
  });

  it('should check canonical equality', () => {
    const obj1 = { b: 2, a: 1 };
    const obj2 = { a: 1, b: 2 };
    assert.ok(areCanonicallyEqual(obj1, obj2));
  });

  it('should sort keys recursively', () => {
    const obj = { z: { y: 1, x: 2 }, a: 1 };
    const sorted = sortKeys(obj);
    const keys = Object.keys(sorted);
    assert.deepEqual(keys, ['a', 'z']);
    assert.deepEqual(Object.keys(sorted.z), ['x', 'y']);
  });
});

describe('Capsule operations', () => {
  it('should create capsule with metadata', () => {
    const delta = { action: 'test' };
    const meta = { agentId: 'agent-5', phase: 'test' };
    const capsule = createCapsule(delta, meta);

    assert.ok(capsule.meta.id);
    assert.ok(capsule.meta.timestamp);
    assert.equal(capsule.meta.agentId, 'agent-5');
    assert.equal(capsule.meta.phase, 'test');
    assert.deepEqual(capsule.delta, delta);
    assert.equal(capsule.receipt, null);
  });

  it('should validate capsule structure', () => {
    const capsule = createCapsule(
      { action: 'test' },
      { agentId: 'agent-5', phase: 'test' }
    );

    assert.ok(validateCapsule(capsule));
  });

  it('should throw on invalid capsule', () => {
    assert.throws(() => validateCapsule(null));
    assert.throws(() => validateCapsule({ meta: {} }));
    assert.throws(() => validateCapsule({ meta: { id: '123' } }));
  });

  it('should serialize and deserialize capsule', () => {
    const capsule = createCapsule(
      { action: 'test', data: [1, 2, 3] },
      { agentId: 'agent-5', phase: 'test' }
    );

    const serialized = serializeCapsule(capsule);
    const deserialized = deserializeCapsule(serialized);

    assert.deepEqual(deserialized.meta, capsule.meta);
    assert.deepEqual(deserialized.delta, capsule.delta);
  });

  it('should extract capsule content without receipt', () => {
    const capsule = createCapsule(
      { action: 'test' },
      { agentId: 'agent-5', phase: 'test' }
    );

    const content = extractCapsuleContent(capsule);
    assert.ok(content.meta);
    assert.ok(content.delta);
    assert.ok(!('receipt' in content));
  });

  it('should attach receipt to capsule', () => {
    const capsule = createCapsule(
      { action: 'test' },
      { agentId: 'agent-5', phase: 'test' }
    );

    const receipt = { id: 'test-receipt', chainIndex: 0 };
    const withReceipt = attachReceipt(capsule, receipt);

    assert.deepEqual(withReceipt.receipt, receipt);
    assert.equal(capsule.receipt, null); // Original unchanged
  });

  it('should get capsule summary', () => {
    const capsule = createCapsule(
      { action: 'test' },
      { agentId: 'agent-5', phase: 'test' }
    );

    const summary = getCapsuleSummary(capsule);
    assert.ok(summary.id);
    assert.equal(summary.agentId, 'agent-5');
    assert.equal(summary.phase, 'test');
    assert.equal(summary.hasReceipt, false);
  });
});

describe('Receipt operations', () => {
  it('should generate receipt for capsule', () => {
    const capsule = createCapsule(
      { action: 'test' },
      { agentId: 'agent-5', phase: 'test' }
    );

    const receipt = generateReceipt(capsule);

    assert.ok(receipt.id);
    assert.ok(receipt.timestamp);
    assert.ok(receipt.capsuleHash.startsWith('sha256:'));
    assert.equal(receipt.previousReceiptHash, null);
    assert.equal(receipt.chainIndex, 0);
    assert.equal(receipt.signature, null);
  });

  it('should generate receipt with previous link', () => {
    const capsule1 = createCapsule(
      { action: 'first' },
      { agentId: 'agent-5', phase: 'test' }
    );
    const receipt1 = generateReceipt(capsule1);

    const capsule2 = createCapsule(
      { action: 'second' },
      { agentId: 'agent-5', phase: 'test' }
    );
    const receipt2 = generateReceipt(capsule2, receipt1);

    assert.equal(receipt2.chainIndex, 1);
    assert.ok(receipt2.previousReceiptHash);
    assert.ok(receipt2.previousReceiptHash.startsWith('sha256:'));
    assert.notEqual(receipt2.previousReceiptHash, receipt1.capsuleHash);
  });

  it('should verify receipt against capsule', () => {
    const capsule = createCapsule(
      { action: 'test' },
      { agentId: 'agent-5', phase: 'test' }
    );
    const receipt = generateReceipt(capsule);

    assert.ok(verifyReceipt(receipt, capsule));
  });

  it('should fail verification for modified capsule', () => {
    const capsule = createCapsule(
      { action: 'test' },
      { agentId: 'agent-5', phase: 'test' }
    );
    const receipt = generateReceipt(capsule);

    // Modify capsule
    capsule.delta.action = 'modified';

    assert.ok(!verifyReceipt(receipt, capsule));
  });

  it('should verify receipt chain', () => {
    const capsule1 = createCapsule({ action: '1' }, { agentId: 'agent-5', phase: 'test' });
    const receipt1 = generateReceipt(capsule1);

    const capsule2 = createCapsule({ action: '2' }, { agentId: 'agent-5', phase: 'test' });
    const receipt2 = generateReceipt(capsule2, receipt1);

    const capsule3 = createCapsule({ action: '3' }, { agentId: 'agent-5', phase: 'test' });
    const receipt3 = generateReceipt(capsule3, receipt2);

    const result = verifyChain([receipt1, receipt2, receipt3]);

    assert.ok(result.valid);
    assert.equal(result.verifiedCount, 3);
    assert.equal(result.errors.length, 0);
  });

  it('should detect broken chain', () => {
    const capsule1 = createCapsule({ action: '1' }, { agentId: 'agent-5', phase: 'test' });
    const receipt1 = generateReceipt(capsule1);

    const capsule2 = createCapsule({ action: '2' }, { agentId: 'agent-5', phase: 'test' });
    const receipt2 = generateReceipt(capsule2, receipt1);

    // Tamper with receipt2's previous hash
    receipt2.previousReceiptHash = 'sha256:0000000000000000000000000000000000000000000000000000000000000000';

    const result = verifyChain([receipt1, receipt2]);

    assert.ok(!result.valid);
    assert.ok(result.errors.length > 0);
  });

  it('should create receipt chain from capsules', () => {
    const capsules = [
      createCapsule({ action: '1' }, { agentId: 'agent-5', phase: 'test' }),
      createCapsule({ action: '2' }, { agentId: 'agent-5', phase: 'test' }),
      createCapsule({ action: '3' }, { agentId: 'agent-5', phase: 'test' }),
    ];

    const receipts = createReceiptChain(capsules);

    assert.equal(receipts.length, 3);
    assert.equal(receipts[0].chainIndex, 0);
    assert.equal(receipts[1].chainIndex, 1);
    assert.equal(receipts[2].chainIndex, 2);
    assert.equal(receipts[0].previousReceiptHash, null);
    assert.ok(receipts[1].previousReceiptHash);
    assert.ok(receipts[2].previousReceiptHash);
  });

  it('should get receipt hash', () => {
    const capsule = createCapsule({ action: 'test' }, { agentId: 'agent-5', phase: 'test' });
    const receipt = generateReceipt(capsule);

    const hash = getReceiptHash(receipt);

    assert.ok(hash.startsWith('sha256:'));
    assert.equal(hash.length, 71);
  });

  it('should get receipt summary', () => {
    const capsule = createCapsule({ action: 'test' }, { agentId: 'agent-5', phase: 'test' });
    const receipt = generateReceipt(capsule);

    const summary = getReceiptSummary(receipt);

    assert.ok(summary.id);
    assert.equal(summary.chainIndex, 0);
    assert.equal(summary.hasPrevious, false);
    assert.equal(summary.hasSignature, false);
  });
});

describe('Tamper detection', () => {
  it('should detect no tampering in valid capsule', () => {
    const capsule = createCapsule({ action: 'test' }, { agentId: 'agent-5', phase: 'test' });
    const receipt = generateReceipt(capsule);
    const withReceipt = attachReceipt(capsule, receipt);

    const report = detectTamper(withReceipt, receipt);

    assert.ok(!report.tampered);
    assert.equal(report.issues.length, 0);
  });

  it('should detect tampered capsule delta', () => {
    const capsule = createCapsule({ action: 'test' }, { agentId: 'agent-5', phase: 'test' });
    const receipt = generateReceipt(capsule);

    // Tamper with delta
    capsule.delta.action = 'tampered';

    const report = detectTamper(capsule, receipt);

    assert.ok(report.tampered);
    assert.ok(report.issues.some(i => i.includes('hash mismatch')));
  });

  it('should detect tampered metadata', () => {
    const capsule = createCapsule({ action: 'test' }, { agentId: 'agent-5', phase: 'test' });
    const receipt = generateReceipt(capsule);

    // Tamper with metadata
    capsule.meta.agentId = 'tampered-agent';

    const report = detectTamper(capsule, receipt);

    assert.ok(report.tampered);
    assert.ok(report.issues.some(i => i.includes('hash mismatch')));
  });

  it('should audit valid chain', () => {
    const capsules = [
      createCapsule({ action: '1' }, { agentId: 'agent-5', phase: 'test' }),
      createCapsule({ action: '2' }, { agentId: 'agent-5', phase: 'test' }),
      createCapsule({ action: '3' }, { agentId: 'agent-5', phase: 'test' }),
    ];

    const receipts = createReceiptChain(capsules);
    const capsulesWithReceipts = capsules.map((c, i) => attachReceipt(c, receipts[i]));

    const audit = auditChain(capsulesWithReceipts);

    assert.ok(audit.valid);
    assert.ok(!audit.tampered);
    assert.equal(audit.totalCapsules, 3);
    assert.equal(audit.validCapsules, 3);
    assert.equal(audit.errors.length, 0);
  });

  it('should audit tampered chain', () => {
    const capsules = [
      createCapsule({ action: '1' }, { agentId: 'agent-5', phase: 'test' }),
      createCapsule({ action: '2' }, { agentId: 'agent-5', phase: 'test' }),
      createCapsule({ action: '3' }, { agentId: 'agent-5', phase: 'test' }),
    ];

    const receipts = createReceiptChain(capsules);
    const capsulesWithReceipts = capsules.map((c, i) => attachReceipt(c, receipts[i]));

    // Tamper with middle capsule
    capsulesWithReceipts[1].delta.action = 'tampered';

    const audit = auditChain(capsulesWithReceipts);

    assert.ok(!audit.valid);
    assert.ok(audit.tampered);
    assert.ok(audit.errors.length > 0);
    assert.ok(audit.validCapsules < audit.totalCapsules);
  });

  it('should find first tamper point', () => {
    const capsules = [
      createCapsule({ action: '1' }, { agentId: 'agent-5', phase: 'test' }),
      createCapsule({ action: '2' }, { agentId: 'agent-5', phase: 'test' }),
      createCapsule({ action: '3' }, { agentId: 'agent-5', phase: 'test' }),
    ];

    const receipts = createReceiptChain(capsules);
    const capsulesWithReceipts = capsules.map((c, i) => attachReceipt(c, receipts[i]));

    // Tamper with second capsule
    capsulesWithReceipts[1].delta.action = 'tampered';

    const firstTamper = findFirstTamper(capsulesWithReceipts);

    assert.ok(firstTamper);
    assert.equal(firstTamper.index, 1);
    assert.ok(firstTamper.tampered);
  });

  it('should verify chain continuity', () => {
    const capsules = [
      createCapsule({ action: '1' }, { agentId: 'agent-5', phase: 'test' }),
      createCapsule({ action: '2' }, { agentId: 'agent-5', phase: 'test' }),
      createCapsule({ action: '3' }, { agentId: 'agent-5', phase: 'test' }),
    ];

    const receipts = createReceiptChain(capsules);
    const capsulesWithReceipts = capsules.map((c, i) => attachReceipt(c, receipts[i]));

    const continuity = verifyChainContinuity(capsulesWithReceipts);

    assert.ok(continuity.continuous);
    assert.equal(continuity.errors.length, 0);
  });

  it('should detect discontinuity in chain', () => {
    const capsules = [
      createCapsule({ action: '1' }, { agentId: 'agent-5', phase: 'test' }),
      createCapsule({ action: '2' }, { agentId: 'agent-5', phase: 'test' }),
    ];

    const receipts = createReceiptChain(capsules);
    const capsulesWithReceipts = capsules.map((c, i) => attachReceipt(c, receipts[i]));

    // Break chain index
    capsulesWithReceipts[1].receipt.chainIndex = 5;

    const continuity = verifyChainContinuity(capsulesWithReceipts);

    assert.ok(!continuity.continuous);
    assert.ok(continuity.errors.length > 0);
  });

  it('should generate chain summary', () => {
    const capsules = [
      createCapsule({ action: '1' }, { agentId: 'agent-5', phase: 'test' }),
      createCapsule({ action: '2' }, { agentId: 'agent-5', phase: 'test' }),
      createCapsule({ action: '3' }, { agentId: 'agent-5', phase: 'test' }),
    ];

    const receipts = createReceiptChain(capsules);
    const capsulesWithReceipts = capsules.map((c, i) => attachReceipt(c, receipts[i]));

    const summary = generateChainSummary(capsulesWithReceipts);

    assert.equal(summary.totalCapsules, 3);
    assert.ok(summary.valid);
    assert.ok(!summary.tampered);
    assert.equal(summary.validCapsules, 3);
    assert.ok(summary.chainHash);
    assert.ok(summary.chainHash.startsWith('sha256:'));
  });
});

describe('End-to-end workflow', () => {
  it('should create and verify complete proof chain', () => {
    // Create capsules
    const capsules = [];
    for (let i = 1; i <= 5; i++) {
      capsules.push(
        createCapsule(
          {
            action: 'migrate',
            fileCount: i,
            files: [`file${i}.mjs`],
          },
          {
            agentId: `agent-${i}`,
            phase: 'migrate',
          }
        )
      );
    }

    // Generate receipt chain
    const receipts = createReceiptChain(capsules);

    // Attach receipts to capsules
    const capsulesWithReceipts = capsules.map((c, i) => attachReceipt(c, receipts[i]));

    // Verify entire chain
    const audit = auditChain(capsulesWithReceipts);

    assert.ok(audit.valid);
    assert.ok(!audit.tampered);
    assert.equal(audit.totalCapsules, 5);
    assert.equal(audit.validCapsules, 5);

    // Verify continuity
    const continuity = verifyChainContinuity(capsulesWithReceipts);
    assert.ok(continuity.continuous);

    // Generate summary
    const summary = generateChainSummary(capsulesWithReceipts);
    assert.ok(summary.chainHash);

    // Serialize and deserialize
    const serialized = capsulesWithReceipts.map(c => serializeCapsule(c));
    const deserialized = serialized.map(s => deserializeCapsule(s));

    // Re-verify deserialized chain
    const auditAfterRoundtrip = auditChain(deserialized);
    assert.ok(auditAfterRoundtrip.valid);
  });

  it('should detect tampering anywhere in workflow', () => {
    // Create valid chain
    const capsules = [
      createCapsule({ action: '1' }, { agentId: 'agent-5', phase: 'test' }),
      createCapsule({ action: '2' }, { agentId: 'agent-5', phase: 'test' }),
      createCapsule({ action: '3' }, { agentId: 'agent-5', phase: 'test' }),
    ];

    const receipts = createReceiptChain(capsules);
    const capsulesWithReceipts = capsules.map((c, i) => attachReceipt(c, receipts[i]));

    // Verify it's valid first
    const auditBefore = auditChain(capsulesWithReceipts);
    assert.ok(auditBefore.valid);

    // Tamper with any capsule
    capsulesWithReceipts[Math.floor(Math.random() * capsules.length)].delta.action = 'tampered';

    // Should detect tampering
    const auditAfter = auditChain(capsulesWithReceipts);
    assert.ok(!auditAfter.valid);
    assert.ok(auditAfter.tampered);

    // Find where tampering occurred
    const tamperPoint = findFirstTamper(capsulesWithReceipts);
    assert.ok(tamperPoint);
    assert.ok(tamperPoint.tampered);
  });
});
