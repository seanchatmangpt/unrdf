/**
 * TensorFlow.js loader
 *
 * The version store only needs the pure-JS TensorFlow.js API (model
 * (de)serialization), so it uses `@tensorflow/tfjs` with the CPU backend. The
 * native `@tensorflow/tfjs-node` addon is optional (faster training in the
 * examples) and must never be imported at module load: its prebuilt binding is
 * frequently absent (pnpm ignores build scripts, unsupported platforms), and a
 * static import would crash the whole package at import time.
 *
 * @module @unrdf/ml-versioning/tf
 */

import * as tf from '@tensorflow/tfjs';
import '@tensorflow/tfjs-backend-cpu';

/**
 * Pure-JS TensorFlow.js (CPU backend registered).
 */
export { tf };

/**
 * Try to load the native tfjs-node binding.
 *
 * @returns {Promise<typeof import('@tensorflow/tfjs-node')>} The tfjs-node module
 * @throws {Error} With an actionable message when the native binding is missing
 */
export async function loadTfjsNode() {
  try {
    return await import('@tensorflow/tfjs-node');
  } catch (cause) {
    const err = new Error(
      '@unrdf/ml-versioning: native @tensorflow/tfjs-node binding is unavailable ' +
        '(tfjs_binding.node not built for this platform). Run ' +
        '`npm rebuild @tensorflow/tfjs-node --build-addon-from-source` (or approve its build script ' +
        'with `pnpm approve-builds`), or use the pure-JS `tf` export from this module.',
      { cause }
    );
    err.code = 'ERR_TFJS_NODE_UNAVAILABLE';
    throw err;
  }
}

/**
 * Load TensorFlow.js, preferring the native binding and falling back to pure JS.
 *
 * @param {Object} [options]
 * @param {boolean} [options.requireNative=false] - Throw instead of falling back
 * @returns {Promise<typeof tf>} TensorFlow.js namespace
 */
export async function loadTensorFlow({ requireNative = false } = {}) {
  try {
    return await loadTfjsNode();
  } catch (error) {
    if (requireNative) throw error;
    console.warn(
      `[ml-versioning] ${error.message}\n[ml-versioning] Falling back to pure-JS CPU backend.`
    );
    await tf.setBackend('cpu');
    await tf.ready();
    return tf;
  }
}
