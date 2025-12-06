/**
 * Vector Engine Client - Main thread interface to Worker
 *
 * Provides async API to vector operations running in Worker
 * Non-blocking, same interface for Node and browser
 *
 * @module @unrdf/kgc-4d/hdit/vector-engine-client
 */

import { Worker as NodeWorker } from 'node:worker_threads';
import { fileURLToPath } from 'node:url';

/**
 * Detect environment and create appropriate Worker
 */
function createWorker(workerURL) {
  // Node.js environment
  if (typeof process !== 'undefined' && process.versions?.node) {
    // Convert URL to file path for Node.js Worker
    const workerPath = workerURL instanceof URL
      ? fileURLToPath(workerURL)
      : workerURL;
    return new NodeWorker(workerPath);
  }

  // Browser environment
  if (typeof Worker !== 'undefined') {
    return new Worker(workerURL, { type: 'module' });
  }

  throw new Error('Worker API not available in this environment');
}

/**
 * Client wrapper for Vector Engine Worker
 */
export class VectorEngineClient {
  constructor(workerURL) {
    this.worker = createWorker(workerURL);
    this.nextId = 0;
    this.pending = new Map();
    this.ready = false;

    // Handle both browser Worker (onmessage) and Node Worker (on('message'))
    const messageHandler = (data) => {
      const { type, id, result, error } = data;

      if (type === 'ready') {
        this.ready = true;
        return;
      }

      const promise = this.pending.get(id);
      if (!promise) return;

      this.pending.delete(id);

      if (error) {
        promise.reject(new Error(error));
      } else {
        promise.resolve(result);
      }
    };

    if (this.worker.on) {
      // Node.js Worker
      this.worker.on('message', messageHandler);
    } else {
      // Browser Worker
      this.worker.onmessage = (e) => messageHandler(e.data);
    }
  }

  /**
   * Wait for worker to be ready
   */
  async waitReady() {
    if (this.ready) return;

    return new Promise((resolve) => {
      const check = () => {
        if (this.ready) {
          resolve();
        } else {
          setTimeout(check, 10);
        }
      };
      check();
    });
  }

  /**
   * Execute operation in worker
   * @private
   */
  async _execute(operation, params) {
    await this.waitReady();

    const id = this.nextId++;

    return new Promise((resolve, reject) => {
      this.pending.set(id, { resolve, reject });

      this.worker.postMessage({
        id,
        operation,
        params,
      });
    });
  }

  // ============================================================================
  // Primitive Operations API
  // ============================================================================

  /**
   * Dot product
   * @param {Float32Array} a
   * @param {Float32Array} b
   * @returns {Promise<number>}
   */
  async dot(a, b) {
    return this._execute('dot', [a, b]);
  }

  /**
   * Squared L2 norm
   * @param {Float32Array} a
   * @returns {Promise<number>}
   */
  async squaredNorm(a) {
    return this._execute('squaredNorm', [a]);
  }

  /**
   * Squared Euclidean distance
   * @param {Float32Array} a
   * @param {Float32Array} b
   * @returns {Promise<number>}
   */
  async squaredDistance(a, b) {
    return this._execute('squaredDistance', [a, b]);
  }

  /**
   * Cosine similarity
   * @param {Float32Array} a
   * @param {Float32Array} b
   * @returns {Promise<number>}
   */
  async cosineSimilarity(a, b) {
    return this._execute('cosineSimilarity', [a, b]);
  }

  /**
   * AXPY: y = alpha * x + y (in-place)
   * @param {number} alpha
   * @param {Float32Array} x
   * @param {Float32Array} y
   * @returns {Promise<void>}
   */
  async axpy(alpha, x, y) {
    return this._execute('axpy', [alpha, x, y]);
  }

  /**
   * Elementwise add
   * @param {Float32Array} a
   * @param {Float32Array} b
   * @param {Float32Array} out
   * @returns {Promise<void>}
   */
  async add(a, b, out) {
    return this._execute('add', [a, b, out]);
  }

  /**
   * Elementwise subtract
   * @param {Float32Array} a
   * @param {Float32Array} b
   * @param {Float32Array} out
   * @returns {Promise<void>}
   */
  async sub(a, b, out) {
    return this._execute('sub', [a, b, out]);
  }

  /**
   * Scalar multiplication
   * @param {number} alpha
   * @param {Float32Array} a
   * @param {Float32Array} out
   * @returns {Promise<void>}
   */
  async scale(alpha, a, out) {
    return this._execute('scale', [alpha, a, out]);
  }

  /**
   * ReLU nonlinearity
   * @param {Float32Array} a
   * @param {Float32Array} out
   * @returns {Promise<void>}
   */
  async relu(a, out) {
    return this._execute('relu', [a, out]);
  }

  /**
   * Clamp to range
   * @param {Float32Array} a
   * @param {number} min
   * @param {number} max
   * @param {Float32Array} out
   * @returns {Promise<void>}
   */
  async clamp(a, min, max, out) {
    return this._execute('clamp', [a, min, max, out]);
  }

  /**
   * Bitwise AND
   * @param {Int32Array} a
   * @param {Int32Array} b
   * @param {Int32Array} out
   * @returns {Promise<void>}
   */
  async andBits(a, b, out) {
    return this._execute('andBits', [a, b, out]);
  }

  /**
   * Bitwise XOR
   * @param {Int32Array} a
   * @param {Int32Array} b
   * @param {Int32Array} out
   * @returns {Promise<void>}
   */
  async xorBits(a, b, out) {
    return this._execute('xorBits', [a, b, out]);
  }

  /**
   * Hamming distance
   * @param {Int32Array} a
   * @param {Int32Array} b
   * @returns {Promise<number>}
   */
  async hammingDistance(a, b) {
    return this._execute('hammingDistance', [a, b]);
  }

  // ============================================================================
  // Higher-Level Operations
  // ============================================================================

  /**
   * Calculate centroid
   * @param {Float32Array[]} vectors
   * @returns {Promise<Float32Array>}
   */
  async centroid(vectors) {
    return this._execute('centroid', [vectors]);
  }

  /**
   * Find K nearest neighbors
   * @param {Float32Array} query
   * @param {Float32Array[]} candidates
   * @param {number} k
   * @returns {Promise<Array<{index: number, distance: number}>>}
   */
  async kNearest(query, candidates, k) {
    return this._execute('kNearest', [query, candidates, k]);
  }

  /**
   * Terminate worker
   */
  terminate() {
    this.worker.terminate();
  }
}
