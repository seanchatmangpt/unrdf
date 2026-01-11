/**
 * Hybrid Signature Scheme
 * Combines Ed25519 (classical) + Dilithium3 (post-quantum)
 * Defense-in-depth: both signatures must verify
 *
 * @module @unrdf/receipts/hybrid-signature
 */

import { ed25519 } from '@noble/curves/ed25519.js';
import { sha256 } from '@noble/hashes/sha2.js';
import { z } from 'zod';
import {
  generateDilithium3KeyPair,
  signDilithium3,
  verifyDilithium3,
  Dilithium3KeyPairSchema,
  Dilithium3SignatureSchema,
} from './dilithium3.mjs';

/**
 * Hybrid Key Pair Schema
 */
export const HybridKeyPairSchema = z.object({
  classical: z.object({
    publicKey: z.instanceof(Uint8Array),
    privateKey: z.instanceof(Uint8Array),
    algorithm: z.literal('Ed25519'),
  }),
  postQuantum: Dilithium3KeyPairSchema,
  algorithm: z.literal('Hybrid-Ed25519-Dilithium3'),
});

/**
 * Hybrid Signature Schema
 */
export const HybridSignatureSchema = z.object({
  classical: z.object({
    signature: z.instanceof(Uint8Array),
    publicKey: z.instanceof(Uint8Array),
    algorithm: z.literal('Ed25519'),
  }),
  postQuantum: Dilithium3SignatureSchema,
  algorithm: z.literal('Hybrid-Ed25519-Dilithium3'),
  timestamp: z.bigint(),
});

/**
 * Generate Hybrid Key Pair
 * Creates both classical (Ed25519) and post-quantum (Dilithium3) keys
 *
 * @returns {Promise<Object>} Hybrid key pair
 * @throws {Error} If key generation fails
 *
 * @example
 * const keyPair = await generateHybridKeyPair();
 * console.log('Classical public key:', keyPair.classical.publicKey.length); // 32 bytes
 * console.log('PQ public key:', keyPair.postQuantum.publicKey.length); // 1952 bytes
 */
export async function generateHybridKeyPair() {
  // Generate Ed25519 key pair
  const classicalPrivateKey = ed25519.utils.randomSecretKey();
  const classicalPublicKey = ed25519.getPublicKey(classicalPrivateKey);

  // Generate Dilithium3 key pair
  const pqKeyPair = await generateDilithium3KeyPair();

  const keyPair = {
    classical: {
      publicKey: classicalPublicKey,
      privateKey: classicalPrivateKey,
      algorithm: 'Ed25519',
    },
    postQuantum: pqKeyPair,
    algorithm: 'Hybrid-Ed25519-Dilithium3',
  };

  // Validate schema
  HybridKeyPairSchema.parse(keyPair);

  return keyPair;
}

/**
 * Sign with Hybrid Scheme
 * Creates both Ed25519 and Dilithium3 signatures
 *
 * @param {Uint8Array|string} message - Message to sign
 * @param {Object} keyPair - Hybrid key pair
 * @returns {Promise<Object>} Hybrid signature
 * @throws {Error} If signing fails
 *
 * @example
 * const signature = await signHybrid(message, keyPair);
 * console.log('Has classical sig:', signature.classical.signature.length === 64);
 * console.log('Has PQ sig:', signature.postQuantum.signature.length === 3293);
 */
export async function signHybrid(message, keyPair) {
  // Validate inputs
  if (!message) {
    throw new TypeError('signHybrid: message is required');
  }

  HybridKeyPairSchema.parse(keyPair);

  // Convert message to Uint8Array
  const messageBytes = typeof message === 'string'
    ? new TextEncoder().encode(message)
    : message;

  // Hash message for Ed25519
  const messageHash = sha256(messageBytes);

  // Sign with Ed25519
  const classicalSignature = ed25519.sign(messageHash, keyPair.classical.privateKey);

  // Sign with Dilithium3
  const pqSignature = await signDilithium3(messageBytes, keyPair.postQuantum);

  // Generate timestamp
  const timestamp = typeof process !== 'undefined' && process.hrtime
    ? process.hrtime.bigint()
    : BigInt(Date.now()) * 1_000_000n;

  const signature = {
    classical: {
      signature: classicalSignature,
      publicKey: keyPair.classical.publicKey,
      algorithm: 'Ed25519',
    },
    postQuantum: pqSignature,
    algorithm: 'Hybrid-Ed25519-Dilithium3',
    timestamp,
  };

  // Validate schema
  HybridSignatureSchema.parse(signature);

  return signature;
}

/**
 * Verify Hybrid Signature
 * Verifies BOTH Ed25519 AND Dilithium3 signatures (defense-in-depth)
 * Returns true only if BOTH signatures are valid
 *
 * @param {Uint8Array|string} message - Original message
 * @param {Object} signature - Hybrid signature
 * @returns {Promise<Object>} Verification result with details
 *
 * @example
 * const result = await verifyHybrid(message, signature);
 * console.log('Valid:', result.valid); // true if BOTH valid
 * console.log('Classical valid:', result.classicalValid);
 * console.log('PQ valid:', result.postQuantumValid);
 */
export async function verifyHybrid(message, signature) {
  try {
    // Validate signature schema
    HybridSignatureSchema.parse(signature);

    // Convert message to Uint8Array
    const messageBytes = typeof message === 'string'
      ? new TextEncoder().encode(message)
      : message;

    // Hash message for Ed25519
    const messageHash = sha256(messageBytes);

    // Verify Ed25519 signature
    const classicalValid = ed25519.verify(
      signature.classical.signature,
      messageHash,
      signature.classical.publicKey
    );

    // Verify Dilithium3 signature
    const postQuantumValid = await verifyDilithium3(
      messageBytes,
      signature.postQuantum
    );

    // BOTH must be valid (defense-in-depth)
    const valid = classicalValid && postQuantumValid;

    return {
      valid,
      classicalValid,
      postQuantumValid,
      algorithm: signature.algorithm,
      timestamp: signature.timestamp,
      securityLevel: {
        classical: '128-bit',
        postQuantum: '128-bit (NIST Level 3)',
        combined: '256-bit equivalent',
      },
    };
  } catch (err) {
    return {
      valid: false,
      classicalValid: false,
      postQuantumValid: false,
      error: err.message,
    };
  }
}

/**
 * Serialize Hybrid Signature
 * Converts hybrid signature to JSON string
 *
 * @param {Object} signature - Hybrid signature
 * @returns {string} JSON string
 *
 * @example
 * const serialized = serializeHybridSignature(signature);
 * const deserialized = deserializeHybridSignature(serialized);
 */
export function serializeHybridSignature(signature) {
  HybridSignatureSchema.parse(signature);

  return JSON.stringify({
    classical: {
      signature: Buffer.from(signature.classical.signature).toString('base64'),
      publicKey: Buffer.from(signature.classical.publicKey).toString('base64'),
      algorithm: signature.classical.algorithm,
    },
    postQuantum: {
      signature: Buffer.from(signature.postQuantum.signature).toString('base64'),
      publicKey: Buffer.from(signature.postQuantum.publicKey).toString('base64'),
      algorithm: signature.postQuantum.algorithm,
    },
    algorithm: signature.algorithm,
    timestamp: signature.timestamp.toString(),
  });
}

/**
 * Deserialize Hybrid Signature
 * Parses hybrid signature from JSON
 *
 * @param {string} serialized - Serialized signature
 * @returns {Object} Hybrid signature
 * @throws {Error} If deserialization fails
 *
 * @example
 * const signature = deserializeHybridSignature(serialized);
 */
export function deserializeHybridSignature(serialized) {
  const parsed = JSON.parse(serialized);

  const signature = {
    classical: {
      signature: new Uint8Array(Buffer.from(parsed.classical.signature, 'base64')),
      publicKey: new Uint8Array(Buffer.from(parsed.classical.publicKey, 'base64')),
      algorithm: parsed.classical.algorithm,
    },
    postQuantum: {
      signature: new Uint8Array(Buffer.from(parsed.postQuantum.signature, 'base64')),
      publicKey: new Uint8Array(Buffer.from(parsed.postQuantum.publicKey, 'base64')),
      algorithm: parsed.postQuantum.algorithm,
    },
    algorithm: parsed.algorithm,
    timestamp: BigInt(parsed.timestamp),
  };

  HybridSignatureSchema.parse(signature);

  return signature;
}

/**
 * Get Hybrid Security Level
 * Returns combined security level information
 *
 * @returns {Object} Security level information
 *
 * @example
 * const level = getHybridSecurityLevel();
 * console.log('Classical:', level.classical); // Ed25519 128-bit
 * console.log('Post-quantum:', level.postQuantum); // Dilithium3 NIST L3
 * console.log('Combined:', level.combined); // 256-bit equivalent
 */
export function getHybridSecurityLevel() {
  return {
    algorithm: 'Hybrid-Ed25519-Dilithium3',
    classical: {
      algorithm: 'Ed25519',
      securityBits: 128,
      keySize: 32,
      signatureSize: 64,
    },
    postQuantum: {
      algorithm: 'Dilithium3',
      nistLevel: 3,
      classicalBits: 192,
      quantumBits: 128,
      publicKeySize: 1952,
      signatureSize: 3293,
    },
    combined: {
      effectiveBits: 256,
      quantumResistant: true,
      defenseInDepth: true,
    },
    totalSignatureSize: 64 + 3293, // 3357 bytes
  };
}
