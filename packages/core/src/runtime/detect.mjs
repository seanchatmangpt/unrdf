/**
 * @fileoverview Cross-Runtime Detection
 * Detect Node.js, Browser, Deno, Bun, and feature capabilities
 * Pure JS - no dependencies
 */

 
/* global Deno, Bun, window, document, navigator, Worker, indexedDB, localStorage, importScripts, self */

/**
 * Runtime detection results
 * @typedef {Object} RuntimeInfo
 * @property {'node'|'browser'|'deno'|'bun'|'worker'|'unknown'} type - Runtime type
 * @property {string} version - Runtime version
 * @property {Object} features - Available features
 */

/**
 * Detect current runtime environment
 * @returns {RuntimeInfo}
 */
export function detectRuntime() {
  const info = {
    type: 'unknown',
    version: 'unknown',
    features: {
      fs: false,
      crypto: false,
      webCrypto: false,
      worker: false,
      wasm: false,
      indexedDB: false,
      localStorage: false,
    },
  };

  // Deno detection (has Deno global)
  if (typeof Deno !== 'undefined') {
    info.type = 'deno';
    info.version = Deno.version?.deno || 'unknown';
    info.features.fs = true;
    info.features.crypto = true;
    info.features.webCrypto = typeof crypto !== 'undefined';
    info.features.wasm = typeof WebAssembly !== 'undefined';
    return info;
  }

  // Bun detection (has Bun global)
  if (typeof Bun !== 'undefined') {
    info.type = 'bun';
    info.version = Bun.version || 'unknown';
    info.features.fs = true;
    info.features.crypto = true;
    info.features.webCrypto = typeof crypto !== 'undefined';
    info.features.wasm = typeof WebAssembly !== 'undefined';
    return info;
  }

  // Node.js detection (has process.versions.node, no window)
  if (typeof process !== 'undefined' && process.versions?.node && typeof window === 'undefined') {
    info.type = 'node';
    info.version = process.versions.node;
    info.features.fs = true;
    info.features.crypto = true;
    info.features.webCrypto = typeof crypto !== 'undefined' && typeof crypto.subtle !== 'undefined';
    info.features.worker = true;
    info.features.wasm = typeof WebAssembly !== 'undefined';
    return info;
  }

  // Browser detection (has window and document)
  if (typeof window !== 'undefined' && typeof document !== 'undefined') {
    info.type = 'browser';
    info.version = navigator?.userAgent || 'unknown';
    info.features.crypto = typeof crypto !== 'undefined';
    info.features.webCrypto = typeof crypto !== 'undefined' && typeof crypto.subtle !== 'undefined';
    info.features.worker = typeof Worker !== 'undefined';
    info.features.wasm = typeof WebAssembly !== 'undefined';
    info.features.indexedDB = typeof indexedDB !== 'undefined';
    info.features.localStorage = typeof localStorage !== 'undefined';
    return info;
  }

  // Web Worker detection (has self, no window)
  if (typeof self !== 'undefined' && typeof importScripts !== 'undefined') {
    info.type = 'worker';
    info.version = 'web-worker';
    info.features.webCrypto = typeof crypto !== 'undefined';
    info.features.wasm = typeof WebAssembly !== 'undefined';
    info.features.indexedDB = typeof indexedDB !== 'undefined';
    return info;
  }

  return info;
}

/**
 * Check if running in Node.js
 * @returns {boolean}
 */
export function isNode() {
  try {
    return typeof process !== 'undefined' && process.versions?.node !== undefined && typeof window === 'undefined';
  } catch {
    return false;
  }
}

/**
 * Check if running in browser
 * @returns {boolean}
 */
export function isBrowser() {
  return typeof window !== 'undefined' && typeof document !== 'undefined';
}

/**
 * Check if running in Deno
 * @returns {boolean}
 */
export function isDeno() {
  return typeof Deno !== 'undefined';
}

/**
 * Check if running in Bun
 * @returns {boolean}
 */
export function isBun() {
  return typeof Bun !== 'undefined';
}

/**
 * Get a universal crypto object (Web Crypto API)
 * @returns {Crypto|null}
 */
export function getCrypto() {
  // Node.js 15+, Deno, Bun, Browser all have globalThis.crypto
  if (typeof globalThis !== 'undefined' && globalThis.crypto) {
    return globalThis.crypto;
  }
  
  // Browser fallback
  if (typeof window !== 'undefined' && window.crypto) {
    return window.crypto;
  }

  // Node.js < 15 fallback
  if (isNode()) {
    try {
      // Dynamic import to avoid errors in browser
      return null; // Will use node:crypto instead
    } catch {
      return null;
    }
  }

  return null;
}

/**
 * Check feature availability
 * @param {string} feature - Feature name ('fs', 'crypto', 'wasm', etc.)
 * @returns {boolean}
 */
export function hasFeature(feature) {
  const runtime = detectRuntime();
  return runtime.features[feature] || false;
}
