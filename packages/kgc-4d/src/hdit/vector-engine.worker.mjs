/**
 * Vector Engine Worker - WASM-style performance in pure JS
 *
 * Implements 8 primitive operations optimized for JIT/WASM compilation:
 * 1. Contiguous load/store
 * 2. Fused multiply-add (FMA) pattern
 * 3. Dot product
 * 4. L2 norm / squared distance
 * 5. AXPY (a·x + y)
 * 6. Elementwise add/subtract
 * 7. Elementwise nonlinearity (ReLU/clamp)
 * 8. Bitwise ops on integer vectors
 *
 * Runs in Web Worker for non-blocking performance
 * Same code path in Node and browser
 *
 * @module @unrdf/kgc-4d/hdit/vector-engine.worker
 */

// ============================================================================
// Primitive 1: Contiguous Load/Store
// ============================================================================

/**
 * Load vector from buffer at offset
 * @param {ArrayBuffer} buffer - Source buffer
 * @param {number} offset - Byte offset
 * @param {number} length - Vector length
 * @returns {Float32Array}
 */
function loadVec(buffer, offset, length) {
  return new Float32Array(buffer, offset, length * 4);
}

/**
 * Store vector to buffer at offset
 * @param {Float32Array} vec - Source vector
 * @param {ArrayBuffer} buffer - Target buffer
 * @param {number} offset - Byte offset
 */
function storeVec(vec, buffer, offset) {
  const target = new Float32Array(buffer, offset, vec.length);
  target.set(vec);
}

// ============================================================================
// Primitive 2 & 3: Fused Multiply-Add (FMA) & Dot Product
// ============================================================================

/**
 * Dot product - the core geometric primitive
 * Optimizes to FMA on supporting hardware
 *
 * @param {Float32Array} a - First vector
 * @param {Float32Array} b - Second vector
 * @returns {number} Dot product
 */
function dot(a, b) {
  const n = a.length;
  let acc = 0.0;

  // Unroll by 4 for better JIT optimization
  const n4 = (n >> 2) << 2;
  let i = 0;

  for (; i < n4; i += 4) {
    acc += a[i] * b[i]
         + a[i + 1] * b[i + 1]
         + a[i + 2] * b[i + 2]
         + a[i + 3] * b[i + 3];
  }

  // Handle remainder
  for (; i < n; i++) {
    acc += a[i] * b[i];
  }

  return acc;
}

// ============================================================================
// Primitive 4: L2 Norm / Squared Distance
// ============================================================================

/**
 * Squared L2 norm of vector
 * @param {Float32Array} a - Vector
 * @returns {number} ||a||²
 */
function squaredNorm(a) {
  const n = a.length;
  let acc = 0.0;

  for (let i = 0; i < n; i++) {
    const v = a[i];
    acc += v * v;
  }

  return acc;
}

/**
 * Squared Euclidean distance between vectors
 * Avoids sqrt for faster comparisons
 *
 * @param {Float32Array} a - First vector
 * @param {Float32Array} b - Second vector
 * @returns {number} ||a - b||²
 */
function squaredDistance(a, b) {
  const n = a.length;
  let acc = 0.0;

  for (let i = 0; i < n; i++) {
    const diff = a[i] - b[i];
    acc += diff * diff;
  }

  return acc;
}

/**
 * Cosine similarity using dot product
 * @param {Float32Array} a - First vector
 * @param {Float32Array} b - Second vector
 * @returns {number} Cosine similarity in [-1, 1]
 */
function cosineSimilarity(a, b) {
  const dotProd = dot(a, b);
  const normA = Math.sqrt(squaredNorm(a));
  const normB = Math.sqrt(squaredNorm(b));

  const denom = normA * normB;
  return denom === 0 ? 0 : dotProd / denom;
}

// ============================================================================
// Primitive 5: AXPY (a·x + y)
// ============================================================================

/**
 * AXPY: y = alpha * x + y (in-place)
 * Linear algebra workhorse for weighted superposition
 *
 * @param {number} alpha - Scalar multiplier
 * @param {Float32Array} x - Input vector
 * @param {Float32Array} y - Output vector (modified in-place)
 */
function axpy(alpha, x, y) {
  const n = x.length;

  for (let i = 0; i < n; i++) {
    y[i] += alpha * x[i];
  }
}

/**
 * AXPY with separate output
 * @param {number} alpha - Scalar
 * @param {Float32Array} x - Input
 * @param {Float32Array} y - Input
 * @param {Float32Array} out - Output
 */
function axpyOut(alpha, x, y, out) {
  const n = x.length;

  for (let i = 0; i < n; i++) {
    out[i] = alpha * x[i] + y[i];
  }
}

// ============================================================================
// Primitive 6: Elementwise Add/Subtract
// ============================================================================

/**
 * Elementwise addition: out = a + b
 * @param {Float32Array} a - First vector
 * @param {Float32Array} b - Second vector
 * @param {Float32Array} out - Output vector
 */
function add(a, b, out) {
  const n = a.length;

  for (let i = 0; i < n; i++) {
    out[i] = a[i] + b[i];
  }
}

/**
 * Elementwise subtraction: out = a - b
 * @param {Float32Array} a - First vector
 * @param {Float32Array} b - Second vector
 * @param {Float32Array} out - Output vector
 */
function sub(a, b, out) {
  const n = a.length;

  for (let i = 0; i < n; i++) {
    out[i] = a[i] - b[i];
  }
}

/**
 * Scalar multiplication: out = alpha * a
 * @param {number} alpha - Scalar
 * @param {Float32Array} a - Input vector
 * @param {Float32Array} out - Output vector
 */
function scale(alpha, a, out) {
  const n = a.length;

  for (let i = 0; i < n; i++) {
    out[i] = alpha * a[i];
  }
}

// ============================================================================
// Primitive 7: Elementwise Nonlinearity (ReLU/Clamp)
// ============================================================================

/**
 * ReLU: out = max(0, a)
 * @param {Float32Array} a - Input vector
 * @param {Float32Array} out - Output vector
 */
function relu(a, out) {
  const n = a.length;

  for (let i = 0; i < n; i++) {
    const v = a[i];
    out[i] = v > 0 ? v : 0;
  }
}

/**
 * Clamp: out = clamp(a, min, max)
 * @param {Float32Array} a - Input vector
 * @param {number} min - Minimum value
 * @param {number} max - Maximum value
 * @param {Float32Array} out - Output vector
 */
function clamp(a, min, max, out) {
  const n = a.length;

  for (let i = 0; i < n; i++) {
    const v = a[i];
    out[i] = v < min ? min : (v > max ? max : v);
  }
}

// ============================================================================
// Primitive 8: Bitwise Ops on Integer Vectors
// ============================================================================

/**
 * Bitwise AND: out = a & b
 * For binary hypervectors / compressed features
 *
 * @param {Int32Array} a - First vector
 * @param {Int32Array} b - Second vector
 * @param {Int32Array} out - Output vector
 */
function andBits(a, b, out) {
  const n = a.length;

  for (let i = 0; i < n; i++) {
    out[i] = a[i] & b[i];
  }
}

/**
 * Bitwise XOR: out = a ^ b
 * @param {Int32Array} a - First vector
 * @param {Int32Array} b - Second vector
 * @param {Int32Array} out - Output vector
 */
function xorBits(a, b, out) {
  const n = a.length;

  for (let i = 0; i < n; i++) {
    out[i] = a[i] ^ b[i];
  }
}

/**
 * Hamming distance for binary vectors
 * @param {Int32Array} a - First vector
 * @param {Int32Array} b - Second vector
 * @returns {number} Hamming distance
 */
function hammingDistance(a, b) {
  const n = a.length;
  let count = 0;

  for (let i = 0; i < n; i++) {
    const xor = a[i] ^ b[i];
    // Count set bits in xor
    let bits = xor;
    while (bits) {
      count += bits & 1;
      bits >>>= 1;
    }
  }

  return count;
}

// ============================================================================
// Higher-Level Operations
// ============================================================================

/**
 * Calculate centroid of multiple vectors
 * @param {Float32Array[]} vectors - Array of vectors
 * @returns {Float32Array} Centroid vector
 */
function centroid(vectors) {
  if (vectors.length === 0) {
    throw new Error('Cannot calculate centroid of empty set');
  }

  const d = vectors[0].length;
  const result = new Float32Array(d);

  // Sum all vectors
  for (const vec of vectors) {
    for (let i = 0; i < d; i++) {
      result[i] += vec[i];
    }
  }

  // Divide by count
  const n = vectors.length;
  for (let i = 0; i < d; i++) {
    result[i] /= n;
  }

  return result;
}

/**
 * Find K nearest neighbors
 * @param {Float32Array} query - Query vector
 * @param {Float32Array[]} candidates - Candidate vectors
 * @param {number} k - Number of neighbors
 * @returns {Array<{index: number, distance: number}>}
 */
function kNearest(query, candidates, k) {
  const distances = candidates.map((vec, index) => ({
    index,
    distance: squaredDistance(query, vec),
  }));

  distances.sort((a, b) => a.distance - b.distance);
  return distances.slice(0, k);
}

// ============================================================================
// Worker Message Handler
// ============================================================================

const operations = {
  dot,
  squaredNorm,
  squaredDistance,
  cosineSimilarity,
  axpy,
  axpyOut,
  add,
  sub,
  scale,
  relu,
  clamp,
  andBits,
  xorBits,
  hammingDistance,
  centroid,
  kNearest,
};

// Environment detection and message handling
const isNode = typeof process !== 'undefined' && process.versions?.node;

if (isNode) {
  // Node.js Worker (worker_threads)
  const { parentPort } = await import('node:worker_threads');

  parentPort.on('message', (data) => {
    const { id, operation, params } = data;

    try {
      const fn = operations[operation];
      if (!fn) {
        throw new Error(`Unknown operation: ${operation}`);
      }

      const result = fn(...params);

      parentPort.postMessage({
        id,
        result,
        error: null,
      });
    } catch (error) {
      parentPort.postMessage({
        id,
        result: null,
        error: error.message,
      });
    }
  });

  // Signal ready
  parentPort.postMessage({ type: 'ready' });
} else {
  // Browser Worker
  self.onmessage = function(e) {
    const { id, operation, params } = e.data;

    try {
      const fn = operations[operation];
      if (!fn) {
        throw new Error(`Unknown operation: ${operation}`);
      }

      const result = fn(...params);

      self.postMessage({
        id,
        result,
        error: null,
      });
    } catch (error) {
      self.postMessage({
        id,
        result: null,
        error: error.message,
      });
    }
  };

  // Signal ready
  self.postMessage({ type: 'ready' });
}
