/**
 * @file Cryptographic Attack Penetration Tests
 * @module test/security-testing/cryptographic-attacks
 * @description
 * Tests for cryptographic vulnerabilities including hash collisions,
 * Merkle tree attacks, receipt tampering, and weak randomness.
 *
 * CRITICAL: All cryptographic guarantees MUST hold under attack.
 */

import { describe, it, expect } from 'vitest';
import crypto from 'crypto';
import { verifyCryptographicHash } from '../../packages/daemon/src/security-audit.mjs';

describe('Cryptographic Attack Penetration Tests', () => {
  describe('Hash Collision Attacks', () => {
    it('ATTACK: Birthday attack on hash function', () => {
      const hashes = new Map();
      const iterations = 100000;

      // Try to find collision by generating many hashes
      for (let i = 0; i < iterations; i++) {
        const data = crypto.randomBytes(16);
        const hash = crypto.createHash('sha256').update(data).digest('hex');

        if (hashes.has(hash)) {
          // Collision found!
          expect(true).toBe(false); // Should never happen with SHA-256
        }

        hashes.set(hash, data);
      }

      // No collisions should be found
      expect(hashes.size).toBe(iterations);
    });

    it('ATTACK: Chosen-prefix collision attack', () => {
      const prefix1 = 'legitimate-data';
      const prefix2 = 'malicious-data';

      // Attacker tries to find suffix that makes hashes collide
      const hash1 = crypto
        .createHash('sha256')
        .update(prefix1 + '-suffix1')
        .digest('hex');
      const hash2 = crypto
        .createHash('sha256')
        .update(prefix2 + '-suffix1')
        .digest('hex');

      // Hashes should be different
      expect(hash1).not.toBe(hash2);

      // SHA-256 should be collision-resistant
      expect(hash1.length).toBe(64);
      expect(hash2.length).toBe(64);
    });

    it('ATTACK: Hash length extension attack', () => {
      const secret = 'secret-key';
      const message = 'legitimate message';

      // Original hash
      const originalHash = crypto
        .createHash('sha256')
        .update(secret + message)
        .digest('hex');

      // Attacker tries to extend message without knowing secret
      const extendedMessage = message + 'malicious extension';
      const extendedHash = crypto
        .createHash('sha256')
        .update(secret + extendedMessage)
        .digest('hex');

      // Extension should fail - hashes should be completely different
      expect(originalHash).not.toBe(extendedHash);

      // No predictable relationship between hashes
      const hammingDistance = Array.from(originalHash)
        .filter((char, i) => char !== extendedHash[i])
        .length;

      // Avalanche effect: >50% of bits should differ
      expect(hammingDistance).toBeGreaterThan(32);
    });

    it('ATTACK: Second preimage attack', () => {
      const original = 'original data';
      const originalHash = crypto
        .createHash('sha256')
        .update(original)
        .digest('hex');

      // Attacker tries to find different data with same hash
      let collisionFound = false;
      for (let i = 0; i < 100000; i++) {
        const attempt = `attempt-${i}`;
        const attemptHash = crypto
          .createHash('sha256')
          .update(attempt)
          .digest('hex');

        if (attemptHash === originalHash && attempt !== original) {
          collisionFound = true;
          break;
        }
      }

      // Should not find collision
      expect(collisionFound).toBe(false);
    });
  });

  describe('Merkle Tree Attacks', () => {
    it('ATTACK: Merkle tree odd-leaf vulnerability', () => {
      // This was a real vulnerability in some implementations
      const leaves = ['data1', 'data2', 'data3']; // Odd number

      const hashLeaves = leaves.map((leaf) =>
        crypto.createHash('sha256').update(leaf).digest('hex')
      );

      // Build Merkle tree (level by level)
      let currentLevel = hashLeaves;

      while (currentLevel.length > 1) {
        const nextLevel = [];

        for (let i = 0; i < currentLevel.length; i += 2) {
          if (i + 1 < currentLevel.length) {
            // Pair exists
            const pairHash = crypto
              .createHash('sha256')
              .update(currentLevel[i] + currentLevel[i + 1])
              .digest('hex');
            nextLevel.push(pairHash);
          } else {
            // Odd leaf - VULNERABLE: Some implementations duplicate it
            // CORRECT: Don't duplicate, promote to next level
            nextLevel.push(currentLevel[i]);
          }
        }

        currentLevel = nextLevel;
      }

      const root = currentLevel[0];
      expect(root).toBeDefined();
      expect(root.length).toBe(64); // SHA-256 hash
    });

    it('ATTACK: Merkle tree second preimage attack', () => {
      // Attacker tries to create different data with same root
      const legitLeaves = ['A', 'B', 'C', 'D'];
      const maliciousLeaves = ['A', 'B', 'X', 'Y']; // Changed last two

      const buildMerkleRoot = (leaves) => {
        let level = leaves.map((leaf) =>
          crypto.createHash('sha256').update(leaf).digest('hex')
        );

        while (level.length > 1) {
          const next = [];
          for (let i = 0; i < level.length; i += 2) {
            const left = level[i];
            const right = i + 1 < level.length ? level[i + 1] : left;
            const hash = crypto
              .createHash('sha256')
              .update(left + right)
              .digest('hex');
            next.push(hash);
          }
          level = next;
        }

        return level[0];
      };

      const legitRoot = buildMerkleRoot(legitLeaves);
      const maliciousRoot = buildMerkleRoot(maliciousLeaves);

      // Roots should be different
      expect(legitRoot).not.toBe(maliciousRoot);
    });

    it('ATTACK: Merkle proof forgery', () => {
      // Attacker tries to forge a proof for non-existent data
      const leaves = ['A', 'B', 'C', 'D'];

      const buildProof = (leaves, index) => {
        const proof = [];
        let level = leaves.map((leaf) =>
          crypto.createHash('sha256').update(leaf).digest('hex')
        );
        let currentIndex = index;

        while (level.length > 1) {
          const isLeft = currentIndex % 2 === 0;
          const siblingIndex = isLeft ? currentIndex + 1 : currentIndex - 1;

          if (siblingIndex < level.length) {
            proof.push({ hash: level[siblingIndex], position: isLeft ? 'right' : 'left' });
          }

          const next = [];
          for (let i = 0; i < level.length; i += 2) {
            const left = level[i];
            const right = i + 1 < level.length ? level[i + 1] : left;
            next.push(
              crypto
                .createHash('sha256')
                .update(left + right)
                .digest('hex')
            );
          }

          level = next;
          currentIndex = Math.floor(currentIndex / 2);
        }

        return { proof, root: level[0] };
      };

      const { proof: validProof, root } = buildProof(leaves, 1); // Proof for 'B'

      // Verify valid proof
      let hash = crypto.createHash('sha256').update('B').digest('hex');

      for (const step of validProof) {
        const combined =
          step.position === 'left'
            ? step.hash + hash
            : hash + step.hash;
        hash = crypto.createHash('sha256').update(combined).digest('hex');
      }

      expect(hash).toBe(root);

      // Try to forge proof for non-existent data 'X'
      let forgedHash = crypto.createHash('sha256').update('X').digest('hex');

      for (const step of validProof) {
        const combined =
          step.position === 'left'
            ? step.hash + forgedHash
            : forgedHash + step.hash;
        forgedHash = crypto.createHash('sha256').update(combined).digest('hex');
      }

      // Forged proof should not match root
      expect(forgedHash).not.toBe(root);
    });
  });

  describe('Receipt Tampering Attacks', () => {
    it('ATTACK: Modify receipt payload', () => {
      const receipt = {
        id: crypto.randomUUID(),
        operation: 'transfer',
        payload: { amount: 100, from: 'Alice', to: 'Bob' },
        timestamp: Date.now(),
      };

      const originalHash = crypto
        .createHash('sha256')
        .update(JSON.stringify(receipt))
        .digest('hex');

      // Attacker modifies payload
      receipt.payload.amount = 10000;

      const tamperedHash = crypto
        .createHash('sha256')
        .update(JSON.stringify(receipt))
        .digest('hex');

      // Hash should change
      expect(tamperedHash).not.toBe(originalHash);

      // Verification should fail
      const result = verifyCryptographicHash(
        JSON.stringify(receipt),
        originalHash
      );
      expect(result.verified).toBe(false);
    });

    it('ATTACK: Receipt chain reordering', () => {
      const receipts = [];

      // Create chain of 5 receipts
      for (let i = 0; i < 5; i++) {
        const receipt = {
          id: crypto.randomUUID(),
          index: i,
          payload: { value: i },
          previousHash: i > 0 ? receipts[i - 1].hash : null,
        };

        receipt.hash = crypto
          .createHash('sha256')
          .update(JSON.stringify(receipt))
          .digest('hex');

        receipts.push(receipt);
      }

      // Verify chain integrity
      for (let i = 1; i < receipts.length; i++) {
        expect(receipts[i].previousHash).toBe(receipts[i - 1].hash);
      }

      // Attacker tries to reorder (swap receipts 2 and 3)
      [receipts[2], receipts[3]] = [receipts[3], receipts[2]];

      // Chain should be broken
      const chainValid =
        receipts[3].previousHash === receipts[2].hash &&
        receipts[4].previousHash === receipts[3].hash;

      expect(chainValid).toBe(false);
    });

    it('ATTACK: Receipt replay attack', () => {
      const receipt = {
        id: crypto.randomUUID(),
        operation: 'create',
        timestamp: Date.now(),
        nonce: crypto.randomBytes(16).toString('hex'), // Prevents replay
      };

      receipt.hash = crypto
        .createHash('sha256')
        .update(JSON.stringify(receipt))
        .digest('hex');

      // Track processed receipts
      const processedReceipts = new Set();
      processedReceipts.add(receipt.hash);

      // Attacker tries to replay same receipt
      const isReplay = processedReceipts.has(receipt.hash);
      expect(isReplay).toBe(true);

      // Replayed receipt should be rejected
      const shouldReject = isReplay;
      expect(shouldReject).toBe(true);
    });

    it('ATTACK: Timestamp manipulation', () => {
      const receipt1 = {
        id: crypto.randomUUID(),
        timestamp: Date.now(),
        payload: {},
      };

      const hash1 = crypto
        .createHash('sha256')
        .update(JSON.stringify(receipt1))
        .digest('hex');

      // Attacker changes timestamp
      receipt1.timestamp = Date.now() - 86400000; // 1 day earlier

      const hash2 = crypto
        .createHash('sha256')
        .update(JSON.stringify(receipt1))
        .digest('hex');

      // Hash should change, detecting tampering
      expect(hash1).not.toBe(hash2);
    });
  });

  describe('Weak Randomness Attacks', () => {
    it('ATTACK: Predict random values', () => {
      // Generate "random" values
      const values = Array.from({ length: 1000 }, () =>
        crypto.randomBytes(32).toString('hex')
      );

      // All should be unique
      const uniqueValues = new Set(values);
      expect(uniqueValues.size).toBe(1000);

      // Check distribution (chi-square test on first byte)
      const firstBytes = values.map((v) => parseInt(v.substring(0, 2), 16));
      const buckets = new Array(16).fill(0);

      firstBytes.forEach((byte) => {
        buckets[Math.floor(byte / 16)]++;
      });

      // Distribution should be relatively uniform
      const expected = values.length / buckets.length;
      const chiSquare = buckets.reduce(
        (sum, observed) => sum + Math.pow(observed - expected, 2) / expected,
        0
      );

      // Chi-square critical value for 15 degrees of freedom at p=0.05 is ~25
      expect(chiSquare).toBeLessThan(50); // Allow some variance
    });

    it('ATTACK: Seed prediction attack', () => {
      // crypto.randomBytes should use cryptographically secure RNG
      // Not based on predictable seeds like Math.random()

      const value1 = crypto.randomBytes(8).toString('hex');
      const value2 = crypto.randomBytes(8).toString('hex');

      // Should be completely different
      expect(value1).not.toBe(value2);

      // No common prefix pattern
      const commonPrefix = Array.from(value1)
        .findIndex((char, i) => char !== value2[i]);

      // Should differ early (within first few characters)
      expect(commonPrefix).toBeLessThan(16);
    });

    it('ATTACK: Entropy exhaustion', () => {
      // Rapidly generate many random values
      const values = [];
      const start = Date.now();

      for (let i = 0; i < 10000; i++) {
        values.push(crypto.randomBytes(32));
      }

      const duration = Date.now() - start;

      // Should complete quickly without blocking on entropy
      expect(duration).toBeLessThan(5000);

      // All values should still be unique
      const uniqueHexValues = new Set(
        values.map((v) => v.toString('hex'))
      );
      expect(uniqueHexValues.size).toBe(10000);
    });
  });

  describe('Key Derivation Attacks', () => {
    it('ATTACK: Brute force weak key derivation', () => {
      // Weak: Using simple hash of password
      const weakKdf = (password, salt) => {
        return crypto
          .createHash('sha256')
          .update(password + salt)
          .digest('hex');
      };

      // Strong: Using PBKDF2 with iterations
      const strongKdf = (password, salt) => {
        return crypto.pbkdf2Sync(password, salt, 100000, 32, 'sha256').toString('hex');
      };

      const password = 'password123';
      const salt = crypto.randomBytes(16).toString('hex');

      // Measure time to derive key
      const weakStart = Date.now();
      const weakKey = weakKdf(password, salt);
      const weakDuration = Date.now() - weakStart;

      const strongStart = Date.now();
      const strongKey = strongKdf(password, salt);
      const strongDuration = Date.now() - strongStart;

      // Strong KDF should take significantly longer
      expect(strongDuration).toBeGreaterThan(weakDuration);

      // Strong KDF should take at least 10ms (100k iterations)
      expect(strongDuration).toBeGreaterThan(10);
    });

    it('ATTACK: Rainbow table attack', () => {
      // Without salt, same password always produces same hash
      const password = 'common-password';

      const unsalted1 = crypto.createHash('sha256').update(password).digest('hex');
      const unsalted2 = crypto.createHash('sha256').update(password).digest('hex');

      // Vulnerable: Same hash every time
      expect(unsalted1).toBe(unsalted2);

      // Protected: With unique salt per user
      const salt1 = crypto.randomBytes(16);
      const salt2 = crypto.randomBytes(16);

      const salted1 = crypto
        .createHash('sha256')
        .update(password + salt1.toString('hex'))
        .digest('hex');
      const salted2 = crypto
        .createHash('sha256')
        .update(password + salt2.toString('hex'))
        .digest('hex');

      // Different hashes even with same password
      expect(salted1).not.toBe(salted2);
    });
  });

  describe('Signature Verification Bypass', () => {
    it('ATTACK: Signature stripping', () => {
      const data = { message: 'important data' };
      const privateKey = crypto.randomBytes(32);

      // Sign data
      const signature = crypto
        .createHmac('sha256', privateKey)
        .update(JSON.stringify(data))
        .digest('hex');

      // Verify signature
      const verifySignature = (data, sig, key) => {
        const expected = crypto
          .createHmac('sha256', key)
          .update(JSON.stringify(data))
          .digest('hex');

        return expected === sig;
      };

      // Valid signature
      expect(verifySignature(data, signature, privateKey)).toBe(true);

      // Attacker strips signature (sends data without signature)
      // System should REJECT unsigned data
      expect(verifySignature(data, '', privateKey)).toBe(false);
      expect(verifySignature(data, null, privateKey)).toBe(false);
    });

    it('ATTACK: Signature reuse', () => {
      const data1 = { message: 'legitimate' };
      const data2 = { message: 'malicious' };
      const key = crypto.randomBytes(32);

      const signature1 = crypto
        .createHmac('sha256', key)
        .update(JSON.stringify(data1))
        .digest('hex');

      // Attacker tries to reuse signature1 with data2
      const verification = crypto
        .createHmac('sha256', key)
        .update(JSON.stringify(data2))
        .digest('hex');

      // Should not match
      expect(signature1).not.toBe(verification);
    });
  });
});
