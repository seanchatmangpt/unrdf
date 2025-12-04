/**
 * @fileoverview Browser shims for Node.js APIs used by UNRDF
 *
 * Provides browser-compatible alternatives to Node.js built-in modules:
 * - fs: Uses IndexedDB-based file system adapter
 * - path: POSIX path utilities
 * - crypto: Web Crypto API wrapper
 * - Worker: Web Workers with message passing
 *
 * @module browser/browser-shim
 */

/* global Worker */

import { IndexedDBFileSystem } from './indexeddb-fs.mjs';

/**
 * Browser-compatible path utilities (POSIX style)
 */
export const path = {
  /**
   * Join path segments
   * @param {...string} segments - Path segments to join
   * @returns {string} Joined path
   */
  join(...segments) {
    return segments.join('/').replace(/\/+/g, '/').replace(/^\//, '');
  },

  /**
   * Get directory name from path
   * @param {string} filePath - File path
   * @returns {string} Directory name
   */
  dirname(filePath) {
    const parts = filePath.split('/');
    parts.pop();
    return parts.join('/') || '.';
  },

  /**
   * Get base name from path
   * @param {string} filePath - File path
   * @param {string} [ext] - Extension to remove
   * @returns {string} Base name
   */
  basename(filePath, ext) {
    let base = filePath.split('/').pop() || '';
    if (ext && base.endsWith(ext)) {
      base = base.slice(0, -ext.length);
    }
    return base;
  },

  /**
   * Get file extension
   * @param {string} filePath - File path
   * @returns {string} Extension with leading dot
   */
  extname(filePath) {
    const base = filePath.split('/').pop() || '';
    const idx = base.lastIndexOf('.');
    return idx > 0 ? base.slice(idx) : '';
  },

  /**
   * Resolve path segments to absolute path
   * @param {...string} segments - Path segments
   * @returns {string} Resolved absolute path
   */
  resolve(...segments) {
    let resolved = '';
    for (let i = segments.length - 1; i >= 0; i--) {
      const segment = segments[i];
      if (segment.startsWith('/')) {
        resolved = segment + (resolved ? '/' + resolved : '');
        break;
      }
      resolved = segment + (resolved ? '/' + resolved : '');
    }
    return resolved.startsWith('/') ? resolved : '/' + resolved;
  },

  sep: '/',
  delimiter: ':',
  posix: null, // Will be set to this object
};
path.posix = path;

/**
 * IndexedDB-based file system
 */
const idbFs = new IndexedDBFileSystem();

/**
 * Browser-compatible fs/promises API
 */
export const fs = {
  promises: {
    /**
     * Read file from IndexedDB
     * @param {string} filePath - File path
     * @param {Object|string} [options] - Read options or encoding
     * @returns {Promise<string|Uint8Array>} File contents
     */
    async readFile(filePath, options = {}) {
      const encoding = typeof options === 'string' ? options : options.encoding;
      return idbFs.readFile(filePath, encoding);
    },

    /**
     * Write file to IndexedDB
     * @param {string} filePath - File path
     * @param {string|Uint8Array} data - File contents
     * @param {Object|string} [options] - Write options or encoding
     * @returns {Promise<void>}
     */
    async writeFile(filePath, data, options = {}) {
      const encoding = typeof options === 'string' ? options : options.encoding;
      return idbFs.writeFile(filePath, data, encoding);
    },

    /**
     * Create directory in IndexedDB
     * @param {string} dirPath - Directory path
     * @param {Object} [options] - Options
     * @returns {Promise<void>}
     */
    async mkdir(dirPath, options = {}) {
      return idbFs.mkdir(dirPath, options);
    },

    /**
     * Read directory from IndexedDB
     * @param {string} dirPath - Directory path
     * @returns {Promise<string[]>} Directory entries
     */
    async readdir(dirPath) {
      return idbFs.readdir(dirPath);
    },

    /**
     * Get file stats
     * @param {string} filePath - File path
     * @returns {Promise<Object>} File stats
     */
    async stat(filePath) {
      return idbFs.stat(filePath);
    },

    /**
     * Check if file exists
     * @param {string} filePath - File path
     * @returns {Promise<boolean>} True if exists
     */
    async exists(filePath) {
      return idbFs.exists(filePath);
    },

    /**
     * Delete file
     * @param {string} filePath - File path
     * @returns {Promise<void>}
     */
    async unlink(filePath) {
      return idbFs.unlink(filePath);
    },

    /**
     * Remove directory
     * @param {string} dirPath - Directory path
     * @param {Object} [options] - Options
     * @returns {Promise<void>}
     */
    async rmdir(dirPath, options = {}) {
      return idbFs.rmdir(dirPath, options);
    },
  },
};

/**
 * Browser-compatible crypto utilities using Web Crypto API
 */
export const crypto = {
  /**
   * Generate random bytes
   * @param {number} size - Number of bytes
   * @returns {Uint8Array} Random bytes
   */
  randomBytes(size) {
    const bytes = new Uint8Array(size);
    globalThis.crypto.getRandomValues(bytes);
    return bytes;
  },

  /**
   * Create hash
   * @param {string} algorithm - Hash algorithm (sha256, sha1, etc)
   * @returns {Object} Hash object
   */
  createHash(algorithm) {
    const normalizedAlgo = algorithm.toLowerCase().replace('-', '');
    const algoMap = {
      sha256: 'SHA-256',
      sha384: 'SHA-384',
      sha512: 'SHA-512',
      sha1: 'SHA-1',
    };

    const webAlgo = algoMap[normalizedAlgo];
    if (!webAlgo) {
      throw new Error(`Unsupported hash algorithm: ${algorithm}`);
    }

    let data = new Uint8Array(0);

    return {
      update(chunk) {
        const chunkBytes = typeof chunk === 'string' ? new TextEncoder().encode(chunk) : chunk;

        const newData = new Uint8Array(data.length + chunkBytes.length);
        newData.set(data);
        newData.set(chunkBytes, data.length);
        data = newData;
        return this;
      },

      async digest(encoding = 'hex') {
        const hashBuffer = await globalThis.crypto.subtle.digest(webAlgo, data);
        const hashArray = new Uint8Array(hashBuffer);

        if (encoding === 'hex') {
          return Array.from(hashArray)
            .map(b => b.toString(16).padStart(2, '0'))
            .join('');
        } else if (encoding === 'base64') {
          return btoa(String.fromCharCode.apply(null, hashArray));
        } else {
          return hashArray;
        }
      },
    };
  },
};

/**
 * Browser Worker wrapper with Node.js Worker Thread API compatibility
 */
export class BrowserWorker {
  /**
   * @param {string|URL} filename - Worker script URL
   * @param {Object} [options] - Worker options
   */
  constructor(filename, options = {}) {
    this.worker = new Worker(filename, {
      type: options.type || 'module',
      name: options.name,
    });

    this.messageHandlers = [];
    this.errorHandlers = [];
    this.exitHandlers = [];

    // Forward worker messages
    this.worker.onmessage = event => {
      this.messageHandlers.forEach(handler => handler(event.data));
    };

    // Forward worker errors
    this.worker.onerror = event => {
      const error = new Error(event.message);
      error.filename = event.filename;
      error.lineno = event.lineno;
      error.colno = event.colno;
      this.errorHandlers.forEach(handler => handler(error));
    };

    // Handle worker termination
    this.terminated = false;
  }

  /**
   * Add event listener
   * @param {string} event - Event name
   * @param {Function} handler - Event handler
   */
  on(event, handler) {
    if (event === 'message') {
      this.messageHandlers.push(handler);
    } else if (event === 'error') {
      this.errorHandlers.push(handler);
    } else if (event === 'exit') {
      this.exitHandlers.push(handler);
    }
  }

  /**
   * Add event listener (alias for on)
   * @param {string} event - Event name
   * @param {Function} handler - Event handler
   */
  addEventListener(event, handler) {
    this.on(event, handler);
  }

  /**
   * Post message to worker
   * @param {*} message - Message to send
   */
  postMessage(message) {
    if (this.terminated) {
      throw new Error('Worker has been terminated');
    }
    this.worker.postMessage(message);
  }

  /**
   * Terminate worker
   * @returns {Promise<number>} Exit code (always 0 in browser)
   */
  async terminate() {
    if (!this.terminated) {
      this.worker.terminate();
      this.terminated = true;
      this.exitHandlers.forEach(handler => handler(0));
    }
    return 0;
  }

  /**
   * Get thread ID (simulated in browser)
   * @returns {number} Thread ID
   */
  get threadId() {
    return (Math.random() * 1000000) | 0;
  }
}

/**
 * Check if running in browser environment
 * @returns {boolean} True if in browser
 */
export function isBrowser() {
  return typeof window !== 'undefined' && typeof document !== 'undefined';
}

/**
 * Get appropriate shims for current environment
 * @returns {Promise<Object>} Environment shims
 */
export async function getEnvironmentShims() {
  if (isBrowser()) {
    return {
      fs,
      path,
      crypto,
      Worker: BrowserWorker,
    };
  } else {
    // Node.js - return native modules
    return {
      fs: await import('fs'),
      path: await import('path'),
      crypto: await import('crypto'),
      Worker: (await import('worker_threads')).Worker,
    };
  }
}
