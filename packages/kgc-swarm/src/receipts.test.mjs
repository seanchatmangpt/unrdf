/**
 * Tests for Receipt Chain with Merkle Verification
 *
 * Test coverage:
 * - Receipt generation
 * - Chain integrity (r_i.before = r_{i-1}.after)
 * - Tamper detection (modified receipt breaks chain)
 * - Merkle verification
 * - Receipt chaining: r₀ → r₁ → ... → r_n
 *
 * @module receipts.test
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import {
  Receipt,
  ReceiptChain,
  hash,
  hashReceipt,
  merkleParent,
  buildMerkleTree,
  computeMerkleRoot,
  createGenesisReceipt,
  createTransitionReceipt,
  verifyChain,
} from './receipts.mjs';

// ============================================================================
// Hash Function Tests
// ============================================================================

describe('Hash Functions', () => {
  it('should compute SHA-256 hash', () => {
    const result = hash('test');
    assert.equal(result.length, 64); // SHA-256 = 64 hex chars
    assert.equal(
      result,
      '9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08'
    );
  });

  it('should hash objects deterministically', () => {
    const obj = { a: 1, b: 2 };
    const hash1 = hash(obj);
    const hash2 = hash(obj);
    assert.equal(hash1, hash2);
  });

  it('should compute Merkle parent hash', () => {
    const left = hash('left');
    const right = hash('right');
    const parent = merkleParent(left, right);
    assert.equal(parent.length, 64);
    assert.notEqual(parent, left);
    assert.notEqual(parent, right);
  });
});

// ============================================================================
// Receipt Class Tests
// ============================================================================

describe('Receipt Class', () => {
  it('should create valid receipt', () => {
    const before = hash('state0');
    const after = hash('state1');
    const delta = hash('operation');

    const receipt = new Receipt(before, after, delta);

    assert.equal(receipt.before, before);
    assert.equal(receipt.after, after);
    assert.equal(receipt.delta, delta);
    assert.ok(receipt.timestamp > 0);
  });

  it('should validate receipt schema', () => {
    assert.throws(() => {
      new Receipt('invalid', 'too-short', 'invalid');
    });
  });

  it('should compute receipt hash', () => {
    const receipt = new Receipt(hash('a'), hash('b'), hash('c'));
    const receiptHash = receipt.hash();
    assert.equal(receiptHash.length, 64);
  });

  it('should serialize and deserialize', () => {
    const original = new Receipt(hash('a'), hash('b'), hash('c'), {
      id: 'test-receipt',
    });

    const obj = original.toObject();
    const restored = Receipt.fromObject(obj);

    assert.equal(restored.before, original.before);
    assert.equal(restored.after, original.after);
    assert.equal(restored.delta, original.delta);
    assert.equal(restored.id, original.id);
  });
});

// ============================================================================
// Merkle Tree Tests
// ============================================================================

describe('Merkle Tree', () => {
  it('should build tree from single leaf', () => {
    const leaves = [hash('leaf1')];
    const tree = buildMerkleTree(leaves);
    assert.equal(tree.length, 1);
    assert.equal(tree[0][0], leaves[0]);
  });

  it('should build tree from two leaves', () => {
    const leaves = [hash('leaf1'), hash('leaf2')];
    const tree = buildMerkleTree(leaves);
    assert.equal(tree.length, 2);
    assert.equal(tree[0].length, 2); // Leaf level
    assert.equal(tree[1].length, 1); // Root level
  });

  it('should build tree from odd number of leaves', () => {
    const leaves = [hash('a'), hash('b'), hash('c')];
    const tree = buildMerkleTree(leaves);
    assert.equal(tree[0].length, 3); // 3 leaves
    assert.equal(tree[1].length, 2); // 2 parents (c duplicated)
    assert.equal(tree[2].length, 1); // 1 root
  });

  it('should compute Merkle root', () => {
    const leaves = [hash('a'), hash('b'), hash('c'), hash('d')];
    const root = computeMerkleRoot(leaves);
    assert.equal(root.length, 64);
  });

  it('should produce different roots for different inputs', () => {
    const root1 = computeMerkleRoot([hash('a'), hash('b')]);
    const root2 = computeMerkleRoot([hash('a'), hash('c')]);
    assert.notEqual(root1, root2);
  });

  it('should throw on empty leaves', () => {
    assert.throws(() => buildMerkleTree([]));
  });
});

// ============================================================================
// Receipt Chain Tests
// ============================================================================

describe('Receipt Chain - Basic Operations', () => {
  it('should create empty chain', () => {
    const chain = new ReceiptChain();
    assert.equal(chain.length, 0);
  });

  it('should add receipt to empty chain', () => {
    const chain = new ReceiptChain();
    const r0 = createGenesisReceipt(hash('initial'), hash('genesis'));
    chain.add(r0);
    assert.equal(chain.length, 1);
  });

  it('should get receipt by index', () => {
    const r0 = createGenesisReceipt(hash('initial'), hash('genesis'));
    const chain = new ReceiptChain([r0]);
    const retrieved = chain.get(0);
    assert.equal(retrieved.before, r0.before);
  });
});

describe('Receipt Chain - Integrity', () => {
  it('should validate chain integrity: r_i.before = r_{i-1}.after', () => {
    const state0 = hash('state0');
    const state1 = hash('state1');
    const state2 = hash('state2');

    const r0 = new Receipt(state0, state1, hash('op1'));
    const r1 = new Receipt(state1, state2, hash('op2'));

    const chain = new ReceiptChain([r0, r1]);
    assert.ok(chain.verify());
  });

  it('should reject broken chain', () => {
    const r0 = new Receipt(hash('s0'), hash('s1'), hash('op1'));
    const r1 = new Receipt(hash('WRONG'), hash('s2'), hash('op2')); // Wrong before!

    assert.throws(() => {
      new ReceiptChain([r0, r1]);
    }, /Chain integrity broken/);
  });

  it('should reject adding receipt that breaks chain', () => {
    const chain = new ReceiptChain();
    const r0 = new Receipt(hash('s0'), hash('s1'), hash('op1'));
    chain.add(r0);

    const r1 = new Receipt(hash('WRONG'), hash('s2'), hash('op2'));
    assert.throws(() => {
      chain.add(r1);
    }, /does not match previous after hash/);
  });

  it('should allow adding receipt that extends chain', () => {
    const chain = new ReceiptChain();
    const s0 = hash('s0');
    const s1 = hash('s1');
    const s2 = hash('s2');

    const r0 = new Receipt(s0, s1, hash('op1'));
    const r1 = new Receipt(s1, s2, hash('op2')); // Correct: r1.before = r0.after

    chain.add(r0);
    chain.add(r1);

    assert.equal(chain.length, 2);
    assert.ok(chain.verify());
  });
});

describe('Receipt Chain - Tamper Detection', () => {
  it('should detect tampered receipt in chain', () => {
    const s0 = hash('s0');
    const s1 = hash('s1');
    const s2 = hash('s2');

    const r0 = new Receipt(s0, s1, hash('op1'));
    const r1 = new Receipt(s1, s2, hash('op2'));
    const chain = new ReceiptChain([r0, r1]);

    // Verify original chain
    assert.ok(chain.verify());
    const originalRoot = chain.getMerkleRoot();

    // Tamper with middle receipt
    chain.receipts[0].after = hash('TAMPERED');

    // Verification should fail
    assert.equal(chain.verify(), false);

    // Merkle root should change
    const tamperedRoot = chain.getMerkleRoot();
    assert.notEqual(tamperedRoot, originalRoot);
  });

  it('should detect modified delta hash', () => {
    const s0 = hash('s0');
    const s1 = hash('s1');
    const s2 = hash('s2');

    const r0 = new Receipt(s0, s1, hash('op1'));
    const r1 = new Receipt(s1, s2, hash('op2'));
    const chain = new ReceiptChain([r0, r1]);

    const originalHash = r0.hash();

    // Tamper with delta
    r0.delta = hash('TAMPERED_DELTA');

    const tamperedHash = r0.hash();
    assert.notEqual(tamperedHash, originalHash);
  });
});

describe('Receipt Chain - Merkle Verification', () => {
  it('should compute Merkle root for chain', () => {
    const s0 = hash('s0');
    const s1 = hash('s1');
    const s2 = hash('s2');
    const s3 = hash('s3');

    const r0 = new Receipt(s0, s1, hash('op1'));
    const r1 = new Receipt(s1, s2, hash('op2'));
    const r2 = new Receipt(s2, s3, hash('op3'));

    const chain = new ReceiptChain([r0, r1, r2]);
    const root = chain.getMerkleRoot();

    assert.equal(root.length, 64);
  });

  it('should produce same root for same chain', () => {
    const s0 = hash('s0');
    const s1 = hash('s1');
    const s2 = hash('s2');

    const r0 = new Receipt(s0, s1, hash('op1'));
    const r1 = new Receipt(s1, s2, hash('op2'));

    const chain1 = new ReceiptChain([r0, r1]);
    const chain2 = new ReceiptChain([r0, r1]);

    assert.equal(chain1.getMerkleRoot(), chain2.getMerkleRoot());
  });

  it('should produce different roots for different chains', () => {
    const s0 = hash('s0');
    const s1 = hash('s1');
    const s2 = hash('s2');

    const r0 = new Receipt(s0, s1, hash('op1'));
    const r1a = new Receipt(s1, s2, hash('op2'));
    const r1b = new Receipt(s1, s2, hash('DIFFERENT'));

    const chain1 = new ReceiptChain([r0, r1a]);
    const chain2 = new ReceiptChain([r0, r1b]);

    assert.notEqual(chain1.getMerkleRoot(), chain2.getMerkleRoot());
  });

  it('should anchor receipt to chain', () => {
    const s0 = hash('s0');
    const s1 = hash('s1');
    const s2 = hash('s2');

    const r0 = new Receipt(s0, s1, hash('op1'));
    const chain = new ReceiptChain([r0]);

    const r1 = new Receipt(s1, s2, hash('op2'));
    const anchoredRoot = chain.anchor(r1);

    // Add receipt and verify root matches
    chain.add(r1);
    assert.equal(chain.getMerkleRoot(), anchoredRoot);
  });
});

describe('Receipt Chain - Full Chain: r₀ → r₁ → ... → r_n', () => {
  it('should create and verify long chain', () => {
    const chain = new ReceiptChain();

    // Create chain of 10 receipts
    let currentState = hash('genesis');
    for (let i = 0; i < 10; i++) {
      const nextState = hash(`state${i + 1}`);
      const delta = hash(`operation${i}`);
      const receipt = new Receipt(currentState, nextState, delta, {
        id: `r${i}`,
      });
      chain.add(receipt);
      currentState = nextState;
    }

    assert.equal(chain.length, 10);
    assert.ok(chain.verify());

    // Verify each link
    for (let i = 1; i < chain.length; i++) {
      assert.equal(
        chain.get(i).before,
        chain.get(i - 1).after,
        `Link ${i - 1} → ${i} broken`
      );
    }
  });

  it('should serialize and deserialize chain', () => {
    const s0 = hash('s0');
    const s1 = hash('s1');
    const s2 = hash('s2');

    const r0 = new Receipt(s0, s1, hash('op1'));
    const r1 = new Receipt(s1, s2, hash('op2'));
    const original = new ReceiptChain([r0, r1]);

    const obj = original.toObject();
    const restored = ReceiptChain.fromObject(obj);

    assert.equal(restored.length, original.length);
    assert.equal(restored.getMerkleRoot(), original.getMerkleRoot());
  });
});

describe('Utility Functions', () => {
  it('should create genesis receipt', () => {
    const initialState = hash('genesis');
    const delta = hash('init');
    const genesis = createGenesisReceipt(initialState, delta);

    assert.equal(genesis.before, initialState);
    assert.equal(genesis.after, initialState); // Genesis: before = after
    assert.equal(genesis.delta, delta);
  });

  it('should create transition receipt', () => {
    const before = hash('state1');
    const after = hash('state2');
    const delta = hash('transition');
    const receipt = createTransitionReceipt(before, after, delta);

    assert.equal(receipt.before, before);
    assert.equal(receipt.after, after);
    assert.equal(receipt.delta, delta);
  });

  it('should verify valid chain', () => {
    const s0 = hash('s0');
    const s1 = hash('s1');
    const r0 = new Receipt(s0, s1, hash('op'));

    const result = verifyChain([r0]);
    assert.ok(result.valid);
    assert.ok(result.merkleRoot);
  });

  it('should detect invalid chain', () => {
    const r0 = new Receipt(hash('s0'), hash('s1'), hash('op1'));
    const r1 = new Receipt(hash('WRONG'), hash('s2'), hash('op2'));

    const result = verifyChain([r0, r1]);
    assert.equal(result.valid, false);
    assert.ok(result.error);
  });
});

// ============================================================================
// Integration Tests
// ============================================================================

describe('Integration - Complete Receipt Chain Workflow', () => {
  it('should demonstrate full receipt chain lifecycle', () => {
    // 1. Create genesis receipt
    const genesisState = hash('initial-state');
    const r0 = createGenesisReceipt(genesisState, hash('genesis-op'));

    // 2. Initialize chain
    const chain = new ReceiptChain([r0]);
    assert.ok(chain.verify());

    // 3. Add state transitions
    const transitions = [
      { state: hash('state1'), op: hash('op1') },
      { state: hash('state2'), op: hash('op2') },
      { state: hash('state3'), op: hash('op3') },
    ];

    let currentState = r0.after;
    for (const { state, op } of transitions) {
      const receipt = createTransitionReceipt(currentState, state, op);
      chain.add(receipt);
      currentState = state;
    }

    assert.equal(chain.length, 4); // Genesis + 3 transitions

    // 4. Verify integrity
    assert.ok(chain.verify());

    // 5. Compute Merkle root
    const root1 = chain.getMerkleRoot();
    assert.equal(root1.length, 64);

    // 6. Serialize and restore
    const obj = chain.toObject();
    const restored = ReceiptChain.fromObject(obj);
    const root2 = restored.getMerkleRoot();

    assert.equal(root1, root2);

    // 7. Detect tampering
    chain.receipts[1].delta = hash('TAMPERED');
    assert.equal(chain.verify(), true); // Still linked, but hash changed
    const root3 = chain.getMerkleRoot();
    assert.notEqual(root3, root1); // Merkle root changed!
  });

  it('should demonstrate tamper detection via Merkle root', () => {
    // Build chain
    const states = ['s0', 's1', 's2'].map(hash);
    const r0 = new Receipt(states[0], states[1], hash('op1'));
    const r1 = new Receipt(states[1], states[2], hash('op2'));
    const chain = new ReceiptChain([r0, r1]);

    // Store original root
    const originalRoot = chain.getMerkleRoot();
    const originalHashes = chain.receipts.map(r => r.hash());

    // Tamper with receipt (change delta)
    chain.receipts[0].delta = hash('TAMPERED');

    // Chain integrity still passes (before/after links intact)
    assert.ok(chain.verify());

    // But Merkle root changed!
    const tamperedRoot = chain.getMerkleRoot();
    assert.notEqual(tamperedRoot, originalRoot);

    // Receipt hash changed
    const tamperedHashes = chain.receipts.map(r => r.hash());
    assert.notEqual(tamperedHashes[0], originalHashes[0]);
  });
});
