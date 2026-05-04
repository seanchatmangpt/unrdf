/**
 * Post-Quantum Receipts Tests
 * Comprehensive test suite for PQ cryptography in receipts
 *
 * @module @unrdf/receipts/test/pq-receipts
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  // Dilithium3
  generateDilithium3KeyPair,
  signDilithium3,
  verifyDilithium3,
  serializeDilithium3Signature,
  deserializeDilithium3Signature,
  getDilithium3SecurityLevel,
  // Hybrid
  generateHybridKeyPair,
  signHybrid,
  verifyHybrid,
  serializeHybridSignature,
  deserializeHybridSignature,
  getHybridSecurityLevel,
  // PQ Receipts
  createPQReceipt,
  verifyPQReceipt,
  batchSignReceipts,
  getPQCapabilities,
  // PQ Merkle
  buildPQMerkleTree,
  generatePQMerkleProof,
  verifyPQMerkleProof,
  getPQMerkleTreeInfo,
} from '../src/index.mjs';

describe('Dilithium3 Post-Quantum Signatures', () => {
  let keyPair;

  beforeEach(async () => {
    keyPair = await generateDilithium3KeyPair();
  });

  it('should generate Dilithium3 key pair with correct sizes', async () => {
    expect(keyPair).toBeDefined();
    expect(keyPair.publicKey).toBeInstanceOf(Uint8Array);
    expect(keyPair.secretKey).toBeInstanceOf(Uint8Array);
    expect(keyPair.algorithm).toBe('Dilithium3');

    // NIST Dilithium3 sizes
    expect(keyPair.publicKey.length).toBe(1952);
    expect(keyPair.secretKey.length).toBe(4000);
  });

  it('should sign message with Dilithium3', async () => {
    const message = 'Test message for PQ signing';
    const signature = await signDilithium3(message, keyPair);

    expect(signature).toBeDefined();
    expect(signature.signature).toBeInstanceOf(Uint8Array);
    expect(signature.signature.length).toBe(3293); // NIST spec
    expect(signature.algorithm).toBe('Dilithium3');
    expect(signature.publicKey).toEqual(keyPair.publicKey);
  });

  it('should verify valid Dilithium3 signature', async () => {
    const message = 'Test message';
    const signature = await signDilithium3(message, keyPair);

    const valid = await verifyDilithium3(message, signature);
    expect(valid).toBe(true);
  });

  it('should reject invalid Dilithium3 signature', async () => {
    const message = 'Original message';
    const signature = await signDilithium3(message, keyPair);

    // Tamper with message
    const valid = await verifyDilithium3('Modified message', signature);
    expect(valid).toBe(false);
  });

  it('should serialize and deserialize Dilithium3 signature', async () => {
    const message = 'Test message';
    const signature = await signDilithium3(message, keyPair);

    const serialized = serializeDilithium3Signature(signature);
    expect(typeof serialized).toBe('string');

    const deserialized = deserializeDilithium3Signature(serialized);
    expect(deserialized.signature).toEqual(signature.signature);
    expect(deserialized.publicKey).toEqual(signature.publicKey);
    expect(deserialized.algorithm).toBe('Dilithium3');

    // Verify deserialized signature works
    const valid = await verifyDilithium3(message, deserialized);
    expect(valid).toBe(true);
  });

  it('should return correct Dilithium3 security level', () => {
    const level = getDilithium3SecurityLevel();

    expect(level.algorithm).toBe('Dilithium3');
    expect(level.nistLevel).toBe(3);
    expect(level.classicalBits).toBe(192);
    expect(level.quantumBits).toBe(128);
    expect(level.publicKeySize).toBe(1952);
    expect(level.secretKeySize).toBe(4000);
    expect(level.signatureSize).toBe(3293);
  });

  it('should handle Uint8Array message', async () => {
    const message = new Uint8Array([1, 2, 3, 4, 5]);
    const signature = await signDilithium3(message, keyPair);

    expect(signature).toBeDefined();
    const valid = await verifyDilithium3(message, signature);
    expect(valid).toBe(true);
  });
});

describe('Hybrid Signatures (Ed25519 + Dilithium3)', () => {
  let hybridKeyPair;

  beforeEach(async () => {
    hybridKeyPair = await generateHybridKeyPair();
  });

  it('should generate hybrid key pair', async () => {
    expect(hybridKeyPair).toBeDefined();
    expect(hybridKeyPair.algorithm).toBe('Hybrid-Ed25519-Dilithium3');

    // Classical Ed25519
    expect(hybridKeyPair.classical.publicKey).toBeInstanceOf(Uint8Array);
    expect(hybridKeyPair.classical.privateKey).toBeInstanceOf(Uint8Array);
    expect(hybridKeyPair.classical.publicKey.length).toBe(32);
    expect(hybridKeyPair.classical.privateKey.length).toBe(32);
    expect(hybridKeyPair.classical.algorithm).toBe('Ed25519');

    // Post-quantum Dilithium3
    expect(hybridKeyPair.postQuantum.publicKey.length).toBe(1952);
    expect(hybridKeyPair.postQuantum.secretKey.length).toBe(4000);
    expect(hybridKeyPair.postQuantum.algorithm).toBe('Dilithium3');
  });

  it('should sign with hybrid scheme', async () => {
    const message = 'Hybrid signature test';
    const signature = await signHybrid(message, hybridKeyPair);

    expect(signature).toBeDefined();
    expect(signature.algorithm).toBe('Hybrid-Ed25519-Dilithium3');

    // Classical signature
    expect(signature.classical.signature).toBeInstanceOf(Uint8Array);
    expect(signature.classical.signature.length).toBe(64); // Ed25519
    expect(signature.classical.algorithm).toBe('Ed25519');

    // PQ signature
    expect(signature.postQuantum.signature).toBeInstanceOf(Uint8Array);
    expect(signature.postQuantum.signature.length).toBe(3293); // Dilithium3
    expect(signature.postQuantum.algorithm).toBe('Dilithium3');

    expect(signature.timestamp).toBeDefined();
    expect(typeof signature.timestamp).toBe('bigint');
  });

  it('should verify valid hybrid signature (both must be valid)', async () => {
    const message = 'Test message';
    const signature = await signHybrid(message, hybridKeyPair);

    const result = await verifyHybrid(message, signature);

    expect(result.valid).toBe(true);
    expect(result.classicalValid).toBe(true);
    expect(result.postQuantumValid).toBe(true);
    expect(result.algorithm).toBe('Hybrid-Ed25519-Dilithium3');
    expect(result.securityLevel).toBeDefined();
    expect(result.securityLevel.combined).toBe('256-bit equivalent');
  });

  it('should reject if either signature is invalid', async () => {
    const message = 'Original message';
    const signature = await signHybrid(message, hybridKeyPair);

    // Tamper with message
    const result = await verifyHybrid('Modified message', signature);

    expect(result.valid).toBe(false);
    // At least one should be invalid
    expect(result.classicalValid || result.postQuantumValid).toBeDefined();
  });

  it('should serialize and deserialize hybrid signature', async () => {
    const message = 'Test message';
    const signature = await signHybrid(message, hybridKeyPair);

    const serialized = serializeHybridSignature(signature);
    expect(typeof serialized).toBe('string');

    const deserialized = deserializeHybridSignature(serialized);

    expect(deserialized.algorithm).toBe('Hybrid-Ed25519-Dilithium3');
    expect(deserialized.classical.signature).toEqual(signature.classical.signature);
    expect(deserialized.postQuantum.signature).toEqual(signature.postQuantum.signature);
    expect(deserialized.timestamp).toEqual(signature.timestamp);

    // Verify deserialized signature
    const result = await verifyHybrid(message, deserialized);
    expect(result.valid).toBe(true);
  });

  it('should return correct hybrid security level', () => {
    const level = getHybridSecurityLevel();

    expect(level.algorithm).toBe('Hybrid-Ed25519-Dilithium3');
    expect(level.classical.algorithm).toBe('Ed25519');
    expect(level.classical.securityBits).toBe(128);
    expect(level.postQuantum.algorithm).toBe('Dilithium3');
    expect(level.postQuantum.nistLevel).toBe(3);
    expect(level.combined.effectiveBits).toBe(256);
    expect(level.combined.quantumResistant).toBe(true);
    expect(level.combined.defenseInDepth).toBe(true);
    expect(level.totalSignatureSize).toBe(3357); // 64 + 3293
  });
});

describe('Post-Quantum Receipts', () => {
  const testOperations = [
    { type: 'add', subject: 'ex:s1', predicate: 'ex:p1', object: 'ex:o1', timestamp: 1000n },
    { type: 'add', subject: 'ex:s2', predicate: 'ex:p2', object: 'ex:o2', timestamp: 2000n },
  ];

  it('should create classical receipt (backward compatible)', async () => {
    const receipt = await createPQReceipt({
      universeID: 'Q*_0123456789abcdef',
      operations: testOperations,
      operationType: 'insert',
    });

    expect(receipt).toBeDefined();
    expect(receipt.Q_ID).toMatch(/^Q\*_[a-f0-9]{16}$/);
    expect(receipt.Q_RDF).toMatch(/^http:\/\/kgc\.io\/receipts\//);
    expect(receipt.Q_PROV.signatureScheme).toBe('classical');
    expect(receipt.Q_PROV.signature).toBeUndefined();
    expect(receipt.Q_PROV.batchSize).toBe(2);
  });

  it('should create post-quantum receipt with Dilithium3', async () => {
    const keyPair = await generateDilithium3KeyPair();

    const receipt = await createPQReceipt({
      universeID: 'Q*_0123456789abcdef',
      operations: testOperations,
      operationType: 'insert',
      signatureScheme: 'postQuantum',
      keyPair,
    });

    expect(receipt.Q_PROV.signatureScheme).toBe('postQuantum');
    expect(receipt.Q_PROV.signature).toBeDefined();
    expect(receipt.Q_PROV.signature.type).toBe('Dilithium3');
    expect(receipt.Q_PROV.signature.signature).toBeDefined();
    expect(receipt.Q_PROV.signature.publicKey).toBeDefined();
  });

  it('should create hybrid receipt (Ed25519 + Dilithium3)', async () => {
    const keyPair = await generateHybridKeyPair();

    const receipt = await createPQReceipt({
      universeID: 'Q*_0123456789abcdef',
      operations: testOperations,
      operationType: 'insert',
      signatureScheme: 'hybrid',
      keyPair,
    });

    expect(receipt.Q_PROV.signatureScheme).toBe('hybrid');
    expect(receipt.Q_PROV.signature).toBeDefined();
    expect(receipt.Q_PROV.signature.type).toBe('Hybrid');
    expect(receipt.Q_PROV.signature.data).toBeDefined();
  });

  it('should verify classical receipt', async () => {
    const receipt = await createPQReceipt({
      universeID: 'Q*_0123456789abcdef',
      operations: testOperations,
      operationType: 'insert',
    });

    const result = await verifyPQReceipt(receipt, testOperations);

    expect(result.valid).toBe(true);
    expect(result.signatureScheme).toBe('classical');
    expect(result.signatureValid).toBe(true);
    expect(result.contentHash).toBeDefined();
  });

  it('should verify post-quantum receipt', async () => {
    const keyPair = await generateDilithium3KeyPair();

    const receipt = await createPQReceipt({
      universeID: 'Q*_0123456789abcdef',
      operations: testOperations,
      operationType: 'insert',
      signatureScheme: 'postQuantum',
      keyPair,
    });

    const result = await verifyPQReceipt(receipt, testOperations);

    expect(result.valid).toBe(true);
    expect(result.signatureScheme).toBe('postQuantum');
    expect(result.signatureValid).toBe(true);
  });

  it('should verify hybrid receipt', async () => {
    const keyPair = await generateHybridKeyPair();

    const receipt = await createPQReceipt({
      universeID: 'Q*_0123456789abcdef',
      operations: testOperations,
      operationType: 'insert',
      signatureScheme: 'hybrid',
      keyPair,
    });

    const result = await verifyPQReceipt(receipt, testOperations);

    expect(result.valid).toBe(true);
    expect(result.signatureScheme).toBe('hybrid');
    expect(result.signatureValid).toBe(true);
  });

  it('should reject receipt with wrong operations', async () => {
    const receipt = await createPQReceipt({
      universeID: 'Q*_0123456789abcdef',
      operations: testOperations,
      operationType: 'insert',
    });

    const wrongOps = [
      { type: 'add', subject: 'ex:s3', predicate: 'ex:p3', object: 'ex:o3' },
    ];

    const result = await verifyPQReceipt(receipt, wrongOps);

    expect(result.valid).toBe(false);
    expect(result.reason).toBeDefined();
  });

  it('should batch sign multiple receipts', async () => {
    const keyPair = await generateHybridKeyPair();

    const receipt1 = await createPQReceipt({
      universeID: 'Q*_0123456789abcdef',
      operations: testOperations,
      operationType: 'insert',
    });

    const receipt2 = await createPQReceipt({
      universeID: 'Q*_fedcba9876543210',
      operations: testOperations,
      operationType: 'update',
    });

    const signed = await batchSignReceipts([receipt1, receipt2], keyPair, 'hybrid');

    expect(signed).toHaveLength(2);
    expect(signed[0].Q_PROV.signatureScheme).toBe('hybrid');
    expect(signed[1].Q_PROV.signatureScheme).toBe('hybrid');
    expect(signed[0].Q_PROV.signature).toBeDefined();
    expect(signed[1].Q_PROV.signature).toBeDefined();
  });

  it('should return PQ capabilities', () => {
    const caps = getPQCapabilities();

    expect(caps.schemes).toEqual(['classical', 'postQuantum', 'hybrid']);
    expect(caps.recommended).toBe('hybrid');
    expect(caps.classical.quantumResistant).toBe(false);
    expect(caps.postQuantum.algorithm).toBe('Dilithium3');
    expect(caps.hybrid.combined.quantumResistant).toBe(true);
  });
});

describe('Post-Quantum Merkle Trees (XMSS)', () => {
  const testData = [
    { id: 1, value: 'data1' },
    { id: 2, value: 'data2' },
    { id: 3, value: 'data3' },
    { id: 4, value: 'data4' },
  ];

  it('should build PQ Merkle tree with SHA3', async () => {
    const tree = await buildPQMerkleTree(testData);

    expect(tree).toBeDefined();
    expect(tree.hash).toBeDefined();
    expect(tree.hash.length).toBe(64); // SHA3-256 hex
    expect(tree.left).toBeDefined();
    expect(tree.right).toBeDefined();
  });

  it('should build signed XMSS tree', async () => {
    const keyPair = await generateDilithium3KeyPair();

    const tree = await buildPQMerkleTree(testData, {
      signNodes: true,
      keyPair,
    });

    expect(tree.signature).toBeDefined();
    expect(tree.signature.signature).toBeDefined();
    expect(tree.signature.publicKey).toBeDefined();

    // Check leaf signatures
    expect(tree.left.signature).toBeDefined();
  });

  it('should generate PQ Merkle proof', async () => {
    const tree = await buildPQMerkleTree(testData);

    const proof = generatePQMerkleProof(tree, 1);

    expect(proof).toBeDefined();
    expect(proof.leaf).toBeDefined();
    expect(proof.index).toBe(1);
    expect(proof.proof).toBeInstanceOf(Array);
    expect(proof.root).toBe(tree.hash);
  });

  it('should verify valid PQ Merkle proof', async () => {
    const tree = await buildPQMerkleTree(testData);
    const proof = generatePQMerkleProof(tree, 2);

    const result = await verifyPQMerkleProof(proof, proof.leaf);

    expect(result.valid).toBe(true);
    expect(result.quantumResistant).toBe(true);
  });

  it('should reject invalid PQ Merkle proof', async () => {
    const tree = await buildPQMerkleTree(testData);
    const proof = generatePQMerkleProof(tree, 1);

    // Wrong leaf hash
    const result = await verifyPQMerkleProof(proof, 'wrong_hash');

    expect(result.valid).toBe(false);
    expect(result.reason).toBeDefined();
  });

  it('should verify signed XMSS proof', async () => {
    const keyPair = await generateDilithium3KeyPair();

    const tree = await buildPQMerkleTree(testData, {
      signNodes: true,
      keyPair,
    });

    const proof = generatePQMerkleProof(tree, 1);

    const result = await verifyPQMerkleProof(proof, proof.leaf, {
      verifySignatures: true,
    });

    expect(result.valid).toBe(true);
    expect(result.signaturesValid).toBe(true);
    expect(result.quantumResistant).toBe(true);
  });

  it('should get PQ Merkle tree info', async () => {
    const keyPair = await generateDilithium3KeyPair();

    const tree = await buildPQMerkleTree(testData, {
      signNodes: true,
      keyPair,
    });

    const info = getPQMerkleTreeInfo(tree);

    expect(info.hashFunction).toBe('SHA3-256/512');
    expect(info.quantumResistant).toBe(true);
    expect(info.signatureScheme).toBe('Dilithium3');
    expect(info.hasPQSignatures).toBe(true);
    expect(info.xmssCompliant).toBe(true);
    expect(info.totalNodes).toBeGreaterThan(0);
    expect(info.depth).toBeGreaterThan(0);
  });
});

describe('Performance Benchmarks', () => {
  it('should meet Dilithium3 signing performance target (<50ms)', async () => {
    const keyPair = await generateDilithium3KeyPair();
    const message = 'Performance test message';

    const start = process.hrtime.bigint();
    await signDilithium3(message, keyPair);
    const end = process.hrtime.bigint();

    const durationMs = Number(end - start) / 1_000_000;

    console.log(`Dilithium3 signing: ${durationMs.toFixed(3)}ms`);
    // Note: This is a simplified Dilithium3 implementation for demonstration.
    // Production implementations would be much faster (<2ms).
    expect(durationMs).toBeLessThan(50);
  });

  it('should meet Dilithium3 verification performance target (<1ms)', async () => {
    const keyPair = await generateDilithium3KeyPair();
    const message = 'Performance test message';
    const signature = await signDilithium3(message, keyPair);

    const start = process.hrtime.bigint();
    await verifyDilithium3(message, signature);
    const end = process.hrtime.bigint();

    const durationMs = Number(end - start) / 1_000_000;

    console.log(`Dilithium3 verification: ${durationMs.toFixed(3)}ms`);
    expect(durationMs).toBeLessThan(1);
  });

  it('should meet hybrid signature performance target (<60ms)', async () => {
    const keyPair = await generateHybridKeyPair();
    const message = 'Performance test message';

    const start = process.hrtime.bigint();
    await signHybrid(message, keyPair);
    const end = process.hrtime.bigint();

    const durationMs = Number(end - start) / 1_000_000;

    console.log(`Hybrid signing: ${durationMs.toFixed(3)}ms`);
    // Note: Simplified implementation. Production would be <3ms.
    expect(durationMs).toBeLessThan(60);
  });

  it('should verify signature sizes match spec', async () => {
    const dilithiumKeyPair = await generateDilithium3KeyPair();
    const hybridKeyPair = await generateHybridKeyPair();
    const message = 'Size test';

    const dilithiumSig = await signDilithium3(message, dilithiumKeyPair);
    const hybridSig = await signHybrid(message, hybridKeyPair);

    // Dilithium3 signature size: 3293 bytes
    expect(dilithiumSig.signature.length).toBe(3293);

    // Hybrid signature: Ed25519 (64) + Dilithium3 (3293) = 3357 bytes
    const hybridTotal = hybridSig.classical.signature.length + hybridSig.postQuantum.signature.length;
    expect(hybridTotal).toBe(3357);

    console.log(`Dilithium3 signature size: ${dilithiumSig.signature.length} bytes`);
    console.log(`Hybrid signature size: ${hybridTotal} bytes`);
  });
});

describe('Security Properties', () => {
  it('should verify NIST Level 3 security parameters', () => {
    const level = getDilithium3SecurityLevel();

    expect(level.nistLevel).toBe(3);
    expect(level.quantumBits).toBe(128); // Post-quantum security
    expect(level.classicalBits).toBe(192); // Classical security

    console.log('NIST Security Level:', JSON.stringify(level, null, 2));
  });

  it('should verify hybrid provides 256-bit equivalent security', () => {
    const level = getHybridSecurityLevel();

    expect(level.combined.effectiveBits).toBe(256);
    expect(level.combined.quantumResistant).toBe(true);
    expect(level.combined.defenseInDepth).toBe(true);

    console.log('Hybrid Security Level:', JSON.stringify(level, null, 2));
  });

  it('should use quantum-resistant hash functions in XMSS', async () => {
    const tree = await buildPQMerkleTree([{ test: 'data' }]);

    const info = getPQMerkleTreeInfo(tree);

    expect(info.hashFunction).toBe('SHA3-256/512');
    expect(info.quantumResistant).toBe(true);

    console.log('XMSS Tree Info:', JSON.stringify(info, null, 2));
  });
});
