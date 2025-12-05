/**
 * @file Browser compatibility shims for Node.js APIs
 * @module browser-shims
 *
 * @description
 * Provides browser-compatible polyfills for Node.js APIs used in the knowledge engine.
 * This allows the same codebase to work in both Node.js and browser environments.
 */

/* global window */

/**
 * Check if we're running in a browser environment
 */
export const isBrowser = typeof window !== 'undefined' && typeof window.document !== 'undefined';

/**
 * Check if we're running in Node.js environment
 */
export const isNode = (() => {
  try {
    return typeof process !== 'undefined' && !!process?.versions?.node;
  } catch {
    return false;
  }
})();

// Random UUID generation - use crypto.randomUUID if available, otherwise fallback
/**
 *
 */
export function randomUUID() {
  if (typeof crypto !== 'undefined' && crypto.randomUUID) {
    return crypto.randomUUID();
  }

  // Fallback UUID generation
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function (c) {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

// Path utilities - browser-compatible version
export const path = {
  join: (...args) => args.filter(Boolean).join('/').replace(/\/+/g, '/'),
  resolve: (...args) => args.filter(Boolean).join('/').replace(/\/+/g, '/'),
  dirname: path => path.replace(/\/$/, '').split('/').slice(0, -1).join('/') || '.',
  basename: path => path.split('/').pop() || '',
  extname: path => {
    const ext = path.split('.').pop();
    return ext && ext !== path ? '.' + ext : '';
  },
};

// Process utilities
export const process = {
  cwd: () => '/',
  env: isBrowser
    ? {}
    : (() => {
        try {
          return process.env;
        } catch {
          return {};
        }
      })(),
  versions: isBrowser
    ? {}
    : (() => {
        try {
          return process.versions;
        } catch {
          return {};
        }
      })(),
};

// File system operations - browser-compatible (no-ops or memory-based)
class BrowserFileSystem {
  #files = new Map();
  #directories = new Set(['/']);

  constructor() {
    // Create some default directories
    this.#directories.add('/src');
    this.#directories.add('/dist');
    this.#directories.add('/examples');
  }

  clear() {
    this.#files.clear();
    this.#directories.clear();
    this.#directories.add('/');
    this.#directories.add('/src');
    this.#directories.add('/dist');
    this.#directories.add('/examples');
  }

  existsSync(path) {
    return this.#files.has(path) || this.#directories.has(path);
  }

  readFileSync(path, encoding = 'utf8') {
    const content = this.#files.get(path);
    if (!content) {
      throw new Error(`ENOENT: no such file or directory, open '${path}'`);
    }
    return encoding === 'utf8' ? content : Buffer.from(content, 'utf8');
  }

  writeFileSync(path, data, encoding = 'utf8') {
    const content = encoding === 'utf8' ? data : data.toString();
    this.#files.set(path, content);

    // Ensure parent directories exist
    const parent = path.dirname(path);
    if (!this.#directories.has(parent) && parent !== path) {
      this.#directories.add(parent);
    }
  }

  mkdirSync(path, _options = {}) {
    if (this.#directories.has(path)) {
      throw new Error(`EEXIST: file already exists, mkdir '${path}'`);
    }
    this.#directories.add(path);
  }

  readdirSync(path) {
    return Array.from(this.#files.keys()).filter(file => file.startsWith(path + '/'));
  }

  // Async versions
  async readFile(path, encoding = 'utf8') {
    return Promise.resolve(this.readFileSync(path, encoding));
  }

  async writeFile(path, data, encoding = 'utf8') {
    this.writeFileSync(path, data, encoding);
    return Promise.resolve();
  }

  async mkdir(path, options = {}) {
    this.mkdirSync(path, options);
    return Promise.resolve();
  }
}

export const fs = isBrowser
  ? new BrowserFileSystem()
  : await import('node:fs').then(m => m.default);
export const fsPromises = isBrowser
  ? new BrowserFileSystem()
  : await import('node:fs/promises').then(m => m.default);

// Worker thread polyfill for browser
/**
 *
 */
export class BrowserWorker {
  /**
   *
   */
  constructor(source, options = {}) {
    if (typeof source === 'string' && isBrowser) {
      // Convert string to blob URL (browser only)
      const blob = new Blob([source], { type: 'application/javascript' });
      this.worker = new Worker(URL.createObjectURL(blob), options);
    } else if (typeof source === 'string' && isNode) {
      // In Node.js, can't create Worker from inline code - need a file
      // Create a mock worker for testing purposes
      this.worker = null;
      this.mockMode = true;
    } else {
      // Assume it's a file path
      this.worker = new Worker(source, options);
    }

    this.messageHandlers = [];
    this.errorHandlers = [];
    this.exitHandlers = [];
    this.terminated = false;

    if (this.worker) {
      this.worker.onmessage = event => {
        this.messageHandlers.forEach(handler => handler(event.data));
      };

      this.worker.onerror = error => {
        this.errorHandlers.forEach(handler => handler(error));
      };
    }
  }

  /**
   *
   */
  postMessage(data) {
    if (this.terminated) return;
    if (this.worker) {
      this.worker.postMessage(data);
    }
  }

  /**
   *
   */
  terminate() {
    this.terminated = true;
    if (this.worker) {
      this.worker.terminate();
    }
  }

  /**
   *
   */
  on(event, handler) {
    switch (event) {
      case 'message':
        this.messageHandlers.push(handler);
        break;
      case 'error':
        this.errorHandlers.push(handler);
        break;
      case 'exit':
        this.exitHandlers.push(handler);
        break;
    }
  }

  /**
   *
   */
  once(event, handler) {
    const wrappedHandler = data => {
      handler(data);
      this.removeListener(event, wrappedHandler);
    };
    this.on(event, wrappedHandler);
  }

  /**
   *
   */
  removeListener(event, handler) {
    switch (event) {
      case 'message':
        this.messageHandlers = this.messageHandlers.filter(h => h !== handler);
        break;
      case 'error':
        this.errorHandlers = this.errorHandlers.filter(h => h !== handler);
        break;
      case 'exit':
        this.exitHandlers = this.exitHandlers.filter(h => h !== handler);
        break;
    }
  }
}

export const Worker = isBrowser
  ? BrowserWorker
  : await import('node:worker_threads').then(m => m.Worker);

// Mock child_process.execSync - browser-compatible
/**
 *
 */
export async function execSync(command, options = {}) {
  if (isBrowser) {
    console.warn(`[Browser] execSync not available in browser: ${command}`);
    return ''; // Return empty string as mock
  }

  const { execSync: nodeExecSync } = await import('child_process');
  const result = nodeExecSync(command, { encoding: 'utf8', ...options });
  // Ensure we return a string, not a Buffer
  return typeof result === 'string' ? result : result.toString('utf8');
}

// Hash utilities - use Web Crypto API in browser
/**
 *
 */
export class BrowserHash {
  /**
   *
   */
  constructor(algorithm = 'SHA-256') {
    this.algorithm = algorithm.toLowerCase().replace(/[^a-z0-9]/g, '');
  }

  /**
   *
   */
  update(data) {
    this.data = typeof data === 'string' ? new TextEncoder().encode(data) : new Uint8Array(data);
    return this;
  }

  /**
   *
   */
  async digest(encoding = 'hex') {
    const hashBuffer = await crypto.subtle.digest(this.algorithm, this.data);
    const hashArray = new Uint8Array(hashBuffer);

    if (encoding === 'hex') {
      return Array.from(hashArray)
        .map(b => b.toString(16).padStart(2, '0'))
        .join('');
    }

    return hashArray;
  }
}

/**
 *
 */
export async function createHash(algorithm) {
  if (isBrowser) {
    return new BrowserHash(algorithm);
  }

  const { createHash: nodeCreateHash } = await import('node:crypto');
  return nodeCreateHash(algorithm);
}

// Export all shims as a unified object
export default {
  isBrowser,
  isNode,
  randomUUID,
  path,
  process,
  fs,
  fsPromises,
  Worker: BrowserWorker,
  execSync,
  createHash,
};
