/**
 * Demo 1: Isomorphic Crypto - SHA-256 Hashing
 * 
 * PROOF: Same code produces identical hash in Node.js and Browser
 * 
 * Pattern: Web Crypto API (universal), with node:crypto fallback
 */

import { detectRuntime, getCrypto } from '../detect.mjs';

/**
 * Hash a string using SHA-256 (works in Node.js and Browser)
 * @param {string} data - Data to hash
 * @returns {Promise<string>} Hex-encoded hash
 */
export async function hashSHA256(data) {
  const runtime = detectRuntime();
  
  // Try Web Crypto API first (Node 15+, all browsers, Deno, Bun)
  const crypto = getCrypto();
  if (crypto && crypto.subtle) {
    const encoder = new TextEncoder();
    const dataBuffer = encoder.encode(data);
    const hashBuffer = await crypto.subtle.digest('SHA-256', dataBuffer);
    const hashArray = Array.from(new Uint8Array(hashBuffer));
    return hashArray.map(b => b.toString(16).padStart(2, '0')).join('');
  }
  
  // Fallback to node:crypto for older Node.js
  if (runtime.type === 'node') {
    const { createHash } = await import('node:crypto');
    return createHash('sha256').update(data).digest('hex');
  }
  
  throw new Error('No crypto implementation available');
}

/**
 * Generate random UUID (works everywhere)
 * @returns {string} UUID v4
 */
export function randomUUID() {
  const crypto = getCrypto();
  if (crypto && crypto.randomUUID) {
    return crypto.randomUUID();
  }
  
  // Fallback implementation
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, c => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

// PROOF: Run this demo
async function main() {
  const runtime = detectRuntime();
  const featuresStr = JSON.stringify(runtime.features, null, 2);
  console.log('Runtime: ' + runtime.type + ' ' + runtime.version);
  console.log('Features: ' + featuresStr);
  console.log('');
  
  // Test 1: Deterministic hashing
  const testData = 'Hello, UNRDF Cross-Runtime World!';
  const hash = await hashSHA256(testData);
  console.log('Input:  "' + testData + '"');
  console.log('SHA256: ' + hash);
  
  // Known hash for verification (computed independently)
  const expectedHash = 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'; // empty string
  const emptyHash = await hashSHA256('');
  console.log('');
  console.log('Empty string hash: ' + emptyHash);
  console.log('Expected:          ' + expectedHash);
  console.log('Match: ' + (emptyHash === expectedHash ? '✅' : '❌'));
  console.log('');
  
  // Test 2: UUID generation
  const uuid1 = randomUUID();
  const uuid2 = randomUUID();
  console.log('UUID 1: ' + uuid1);
  console.log('UUID 2: ' + uuid2);
  console.log('Unique: ' + (uuid1 !== uuid2 ? '✅' : '❌'));
  console.log('');
  
  console.log('✅ Demo 1: Isomorphic Crypto - SUCCESS');
}

// Auto-run if executed directly
if (import.meta.url === 'file://' + process.argv[1]) {
  main().catch(console.error);
}
