/**
 * @fileoverview Advanced Compression Module for UNRDF
 *
 * Provides multiple compression algorithms with adaptive selection:
 * - LZ4: Fast compression for real-time data
 * - Brotli: High compression ratio for archival
 * - Delta: Temporal compression for time series (O_τ → O_τ+1)
 * - Columnar: Structure-aware compression for tabular data
 * - Semantic: Pattern extraction + schema-aware compression
 * - Adaptive: Auto-select best algorithm based on data characteristics
 *
 * **Idempotency Property**: μ ∘ μ = μ
 * Compressing already-compressed data should not improve ratio significantly.
 *
 * @module compression
 */

// Core algorithms
export {
  LZ4Compressor,
  createLZ4Compressor,
  compressLZ4,
  decompressLZ4
} from './algorithms/lz4.mjs';

export {
  BrotliCompressor,
  createBrotliCompressor,
  compressBrotli,
  decompressBrotli
} from './algorithms/brotli.mjs';

export {
  DeltaEncoder,
  createDeltaEncoder,
  compressDelta,
  decompressDelta
} from './algorithms/delta.mjs';

export {
  ColumnarCompressor,
  createColumnarCompressor,
  compressColumnar,
  decompressColumnar
} from './algorithms/columnar.mjs';

// Advanced features
export {
  AdaptiveCompressionSelector,
  createAdaptiveSelector,
  compressAdaptive,
  decompressAdaptive
} from './adaptive-selector.mjs';

export {
  SemanticCompressor,
  createSemanticCompressor,
  compressSemantic,
  decompressSemantic
} from './semantic-compressor.mjs';

/**
 * Quick compression with auto-selected algorithm
 *
 * @param {any} data - Data to compress
 * @param {Object} [options] - Compression options
 * @returns {Promise<Object>} Compressed result
 *
 * @example
 * import { compress, decompress } from './compression/index.mjs';
 *
 * const data = { s: 'ex:subject', p: 'ex:pred', o: 'value' };
 * const compressed = await compress(data);
 * const original = await decompress(compressed);
 */
export async function compress(data, options = {}) {
  const { createAdaptiveSelector } = await import('./adaptive-selector.mjs');
  const selector = createAdaptiveSelector(options);
  return selector.compress(data);
}

/**
 * Decompress data
 *
 * @param {Object} compressedData - Compressed data object
 * @param {Object} [options] - Decompression options
 * @returns {Promise<any>} Decompressed data
 *
 * @example
 * const decompressed = await decompress(compressed);
 */
export async function decompress(compressedData, options = {}) {
  const { createAdaptiveSelector } = await import('./adaptive-selector.mjs');
  const selector = createAdaptiveSelector(options);
  return selector.decompress(compressedData);
}

/**
 * Verify round-trip fidelity
 *
 * @param {any} original - Original data
 * @param {Object} compressedData - Compressed data
 * @returns {Promise<Object>} Verification result
 *
 * @example
 * const verification = await verifyRoundTrip(original, compressed);
 * console.log(verification.fidelityPreserved); // true
 */
export async function verifyRoundTrip(original, compressedData) {
  const decompressed = await decompress(compressedData);

  const originalSerialized = JSON.stringify(original);
  const decompressedSerialized = JSON.stringify(decompressed);

  const fidelityPreserved = originalSerialized === decompressedSerialized;

  return {
    fidelityPreserved,
    algorithm: compressedData.algorithm || compressedData.selectedAlgorithm,
    compressionRatio: compressedData.ratio,
    originalSize: compressedData.originalSize,
    compressedSize: compressedData.compressedSize,
    bytesSaved: compressedData.originalSize - compressedData.compressedSize
  };
}

/**
 * Test idempotency property: μ ∘ μ = μ
 *
 * Compressing already-compressed data should not significantly improve ratio.
 *
 * @param {any} data - Original data
 * @param {Object} [options] - Compression options
 * @returns {Promise<Object>} Idempotency test result
 *
 * @example
 * const result = await testIdempotency(data);
 * console.log(result.isIdempotent); // true if μ ∘ μ ≈ μ
 */
export async function testIdempotency(data, options = {}) {
  // First compression
  const compressed1 = await compress(data, options);

  // Second compression (compress the compressed data)
  const compressed2 = await compress(compressed1, options);

  // Ratio should not improve significantly on second compression
  const ratioImprovement = compressed2.ratio / compressed1.ratio;

  const isIdempotent = ratioImprovement < 1.1; // Less than 10% improvement

  return {
    isIdempotent,
    firstCompressionRatio: compressed1.ratio,
    secondCompressionRatio: compressed2.ratio,
    ratioImprovement,
    interpretation: isIdempotent
      ? 'Idempotent: μ ∘ μ ≈ μ (data already compressed)'
      : 'Non-idempotent: μ ∘ μ ≠ μ (further compression possible)'
  };
}

export default {
  compress,
  decompress,
  verifyRoundTrip,
  testIdempotency
};
