/**
 * @fileoverview Cross-Runtime Utilities for UNRDF
 * @module @unrdf/core/runtime
 * 
 * Enables UNRDF code to run in Node.js, browsers, Deno, Bun, and Web Workers.
 */

export {
  detectRuntime,
  isNode,
  isBrowser,
  isDeno,
  isBun,
  getCrypto,
  hasFeature
} from './detect.mjs';

// Re-export demo utilities for advanced users
export { hashSHA256, randomUUID } from './proofs/demo-1-isomorphic-crypto.mjs';
export { createUniversalStore, queryStore, serializeStore } from './proofs/demo-2-universal-store.mjs';
