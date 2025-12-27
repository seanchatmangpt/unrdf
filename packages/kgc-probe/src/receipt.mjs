#!/usr/bin/env node
/**
 * @fileoverview Receipt generation with hash chains for KGC Probe
 *
 * Receipt = cryptographic proof of observation integrity.
 * Uses BLAKE3 for fast, deterministic hashing.
 *
 * Hash chain: Each receipt includes hash of previous receipt → tamper-evident sequence.
 *
 * Design principles:
 * - Deterministic: Same input → same hash
 * - Tamper-evident: Any modification breaks chain
 * - Fast: BLAKE3 is optimized for speed
 * - Verifiable: Can reconstruct and verify entire chain
 */

import { blake3 } from '@noble/hashes/blake3';
import { bytesToHex } from '@noble/hashes/utils';

/**
 * Hash data deterministically using BLAKE3
 *
 * @param {any} data - Data to hash (will be JSON-stringified with stable key order)
 * @returns {string} - Hex-encoded hash
 */
export function hashData(data) {
  // Stable JSON serialization (sorted keys)
  const json = JSON.stringify(data, Object.keys(data).sort());
  const bytes = new TextEncoder().encode(json);
  const hash = blake3(bytes);
  return bytesToHex(hash);
}

/**
 * Create receipt for an observation
 *
 * @param {import('./observation.mjs').Observation} observation - Observation to create receipt for
 * @param {string} [previousHash] - Hash of previous receipt (for chain)
 * @returns {Receipt}
 *
 * @typedef {Object} Receipt
 * @property {string} observationId - ID of observation this receipt covers
 * @property {string} hash - BLAKE3 hash of observation content
 * @property {string} [previousHash] - Hash of previous receipt (chain)
 * @property {number} index - Position in chain (0-indexed)
 * @property {string} timestamp - ISO 8601 timestamp
 */
export function createReceipt(observation, previousHash = null) {
  // Create deterministic content for hashing
  const content = {
    id: observation.id,
    category: observation.category,
    severity: observation.severity,
    message: observation.message,
    location: observation.location,
    data: observation.data,
    metadata: observation.metadata,
    tags: observation.tags.slice().sort() // Stable tag order
  };

  const hash = hashData(content);

  const receipt = {
    observationId: observation.id,
    hash,
    previousHash,
    index: previousHash ? -1 : 0, // Will be set by chain builder
    timestamp: new Date().toISOString()
  };

  return receipt;
}

/**
 * Build receipt chain from observations
 *
 * @param {import('./observation.mjs').Observation[]} observations - Observations to chain
 * @returns {Receipt[]} - Receipt chain
 */
export function buildReceiptChain(observations) {
  const receipts = [];
  let previousHash = null;

  for (let i = 0; i < observations.length; i++) {
    const receipt = createReceipt(observations[i], previousHash);
    receipt.index = i;
    receipts.push(receipt);
    previousHash = receipt.hash;
  }

  return receipts;
}

/**
 * Verify receipt chain integrity
 *
 * @param {Receipt[]} receipts - Receipt chain to verify
 * @returns {boolean} - True if chain is valid
 */
export function verifyReceiptChain(receipts) {
  if (receipts.length === 0) {
    return true;
  }

  // First receipt should have no previous hash
  if (receipts[0].previousHash !== null) {
    return false;
  }

  // Verify chain links
  for (let i = 1; i < receipts.length; i++) {
    if (receipts[i].previousHash !== receipts[i - 1].hash) {
      return false;
    }
    if (receipts[i].index !== i) {
      return false;
    }
  }

  return true;
}

/**
 * Verify observation against receipt
 *
 * @param {import('./observation.mjs').Observation} observation - Observation to verify
 * @param {Receipt} receipt - Receipt to verify against
 * @returns {boolean} - True if observation matches receipt
 */
export function verifyObservation(observation, receipt) {
  const recomputedReceipt = createReceipt(observation, receipt.previousHash);
  return recomputedReceipt.hash === receipt.hash;
}

/**
 * Create receipt manifest (summary of entire chain)
 *
 * @param {Receipt[]} receipts - Receipt chain
 * @returns {ReceiptManifest}
 *
 * @typedef {Object} ReceiptManifest
 * @property {number} count - Number of receipts in chain
 * @property {string} firstHash - Hash of first receipt
 * @property {string} lastHash - Hash of last receipt
 * @property {string} chainHash - Hash of entire chain
 * @property {string} timestamp - ISO 8601 timestamp
 */
export function createManifest(receipts) {
  if (receipts.length === 0) {
    return {
      count: 0,
      firstHash: null,
      lastHash: null,
      chainHash: null,
      timestamp: new Date().toISOString()
    };
  }

  // Chain hash = hash of all receipt hashes
  const chainContent = receipts.map(r => r.hash).join('');
  const chainHash = hashData(chainContent);

  return {
    count: receipts.length,
    firstHash: receipts[0].hash,
    lastHash: receipts[receipts.length - 1].hash,
    chainHash,
    timestamp: new Date().toISOString()
  };
}

/**
 * Verify manifest matches receipts
 *
 * @param {ReceiptManifest} manifest - Manifest to verify
 * @param {Receipt[]} receipts - Receipt chain
 * @returns {boolean} - True if manifest is valid
 */
export function verifyManifest(manifest, receipts) {
  const recomputed = createManifest(receipts);
  return (
    manifest.count === recomputed.count &&
    manifest.firstHash === recomputed.firstHash &&
    manifest.lastHash === recomputed.lastHash &&
    manifest.chainHash === recomputed.chainHash
  );
}

export default {
  hashData,
  createReceipt,
  buildReceiptChain,
  verifyReceiptChain,
  verifyObservation,
  createManifest,
  verifyManifest
};
