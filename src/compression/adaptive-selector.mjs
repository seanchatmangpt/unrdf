/**
 * @fileoverview Adaptive Compression Selector
 *
 * Automatically selects the best compression algorithm based on:
 * - Data characteristics (size, entropy, structure)
 * - Performance requirements (speed vs ratio)
 * - Historical performance metrics
 *
 * @module compression/adaptive-selector
 */

import { z } from 'zod';
import { LZ4Compressor } from './algorithms/lz4.mjs';
import { BrotliCompressor } from './algorithms/brotli.mjs';
import { DeltaEncoder } from './algorithms/delta.mjs';
import { ColumnarCompressor } from './algorithms/columnar.mjs';

/**
 * Selector options schema
 */
const SelectorOptionsSchema = z.object({
  prioritizeSpeed: z.boolean().default(false),
  prioritizeRatio: z.boolean().default(false),
  minCompressionRatio: z.number().min(1).default(1.1),
  maxCompressionTime: z.number().min(0).default(1000), // ms
  enableLearning: z.boolean().default(true)
});

/**
 * Adaptive Compression Selector
 *
 * @class AdaptiveCompressionSelector
 * @example
 * const selector = new AdaptiveCompressionSelector();
 * const result = await selector.compress(data);
 * const decompressed = await selector.decompress(result);
 */
export class AdaptiveCompressionSelector {
  /**
   * Create adaptive selector
   * @param {Object} [options] - Selector options
   */
  constructor(options = {}) {
    this.options = SelectorOptionsSchema.parse(options);

    // Initialize all compressors
    this.compressors = {
      lz4: new LZ4Compressor(),
      brotli: new BrotliCompressor({ quality: this.options.prioritizeRatio ? 9 : 6 }),
      delta: new DeltaEncoder(),
      columnar: new ColumnarCompressor()
    };

    // Performance history for learning
    this.performanceHistory = new Map();

    // Stats
    this.stats = {
      totalCompressions: 0,
      algorithmUsage: {
        lz4: 0,
        brotli: 0,
        delta: 0,
        columnar: 0
      }
    };
  }

  /**
   * Compress data with best algorithm
   *
   * @param {any} data - Data to compress
   * @param {Object} [hints] - Optional hints about data
   * @returns {Promise<Object>} Compressed result
   */
  async compress(data, hints = {}) {
    const characteristics = this._analyzeData(data, hints);
    const algorithm = this._selectAlgorithm(characteristics);

    let result;

    try {
      if (algorithm === 'delta' && !Array.isArray(data)) {
        // Fallback for non-array data
        result = await this.compressors.lz4.compress(data);
        result.selectedAlgorithm = 'lz4';
        result.fallbackReason = 'Delta requires array input';
      } else if (algorithm === 'columnar' && (!Array.isArray(data) || data.length === 0 || typeof data[0] !== 'object')) {
        // Fallback for non-structured data
        result = await this.compressors.lz4.compress(data);
        result.selectedAlgorithm = 'lz4';
        result.fallbackReason = 'Columnar requires structured array';
      } else {
        result = await this.compressors[algorithm].compress(data);
        result.selectedAlgorithm = algorithm;
      }

      // Record performance
      if (this.options.enableLearning) {
        this._recordPerformance(characteristics, algorithm, result);
      }

      // Update stats
      this.stats.totalCompressions++;
      this.stats.algorithmUsage[result.selectedAlgorithm || algorithm]++;

      result.dataCharacteristics = characteristics;

      return result;
    } catch (error) {
      // Fallback to LZ4 on error
      result = await this.compressors.lz4.compress(data);
      result.selectedAlgorithm = 'lz4';
      result.fallbackReason = `${algorithm} failed: ${error.message}`;
      return result;
    }
  }

  /**
   * Decompress data
   *
   * @param {Object} compressedData - Compressed data
   * @returns {Promise<any>} Decompressed data
   */
  async decompress(compressedData) {
    const algorithm = compressedData.selectedAlgorithm || compressedData.algorithm;

    if (!this.compressors[algorithm]) {
      throw new Error(`Unknown compression algorithm: ${algorithm}`);
    }

    return this.compressors[algorithm].decompress(compressedData);
  }

  /**
   * Analyze data characteristics
   * @private
   */
  _analyzeData(data, hints) {
    const characteristics = {
      type: typeof data,
      isArray: Array.isArray(data),
      size: 0,
      entropy: 0,
      isStructured: false,
      isTemporal: hints.isTemporal || false,
      hasRepeatedValues: false
    };

    // Estimate size
    const serialized = JSON.stringify(data);
    characteristics.size = Buffer.byteLength(serialized, 'utf-8');

    // Check if array of objects (structured data)
    if (characteristics.isArray && data.length > 0) {
      characteristics.isStructured = typeof data[0] === 'object' && !Array.isArray(data[0]);
      characteristics.length = data.length;

      // Simple entropy estimation
      const uniqueValues = new Set(serialized.split(''));
      characteristics.entropy = uniqueValues.size / 256; // Normalized 0-1
    }

    // Check for repeated values (simple heuristic)
    if (characteristics.size > 0) {
      const compressionRatio = serialized.length / characteristics.size;
      characteristics.hasRepeatedValues = compressionRatio < 0.8;
    }

    return characteristics;
  }

  /**
   * Select best algorithm based on characteristics
   * @private
   */
  _selectAlgorithm(characteristics) {
    // Check learning history first
    if (this.options.enableLearning) {
      const learned = this._learnedAlgorithm(characteristics);
      if (learned) return learned;
    }

    // Temporal data → Delta encoding
    if (characteristics.isTemporal && characteristics.isArray) {
      return 'delta';
    }

    // Structured array data → Columnar
    if (characteristics.isStructured && characteristics.length > 10) {
      return 'columnar';
    }

    // Priority-based selection
    if (this.options.prioritizeSpeed) {
      return 'lz4';
    }

    if (this.options.prioritizeRatio) {
      // Large data → Brotli for better compression
      if (characteristics.size > 10000) {
        return 'brotli';
      }
    }

    // Default selection based on size and entropy
    if (characteristics.size < 1000) {
      return 'lz4'; // Fast for small data
    }

    if (characteristics.entropy > 0.7) {
      return 'brotli'; // High entropy → need better compression
    }

    return 'lz4'; // Default to fast compression
  }

  /**
   * Get learned algorithm from history
   * @private
   */
  _learnedAlgorithm(characteristics) {
    // Simple learning: find best algorithm for similar data
    const key = this._characteristicsKey(characteristics);
    const history = this.performanceHistory.get(key);

    if (!history || history.length < 3) {
      return null;
    }

    // Find algorithm with best avg compression ratio
    const avgRatios = {};
    for (const record of history) {
      if (!avgRatios[record.algorithm]) {
        avgRatios[record.algorithm] = [];
      }
      avgRatios[record.algorithm].push(record.ratio);
    }

    let bestAlgorithm = null;
    let bestAvgRatio = 0;

    for (const [algorithm, ratios] of Object.entries(avgRatios)) {
      const avg = ratios.reduce((a, b) => a + b, 0) / ratios.length;
      if (avg > bestAvgRatio) {
        bestAvgRatio = avg;
        bestAlgorithm = algorithm;
      }
    }

    return bestAlgorithm;
  }

  /**
   * Record performance for learning
   * @private
   */
  _recordPerformance(characteristics, algorithm, result) {
    const key = this._characteristicsKey(characteristics);

    if (!this.performanceHistory.has(key)) {
      this.performanceHistory.set(key, []);
    }

    const history = this.performanceHistory.get(key);
    history.push({
      algorithm,
      ratio: result.ratio,
      compressionTime: result.compressionTime,
      timestamp: Date.now()
    });

    // Keep last 100 records per key
    if (history.length > 100) {
      history.shift();
    }
  }

  /**
   * Generate key for characteristics
   * @private
   */
  _characteristicsKey(characteristics) {
    const sizeCategory = characteristics.size < 1000 ? 'small' :
      characteristics.size < 10000 ? 'medium' : 'large';
    const entropyCategory = characteristics.entropy < 0.3 ? 'low' :
      characteristics.entropy < 0.7 ? 'medium' : 'high';

    return `${characteristics.type}-${characteristics.isArray}-${characteristics.isStructured}-${sizeCategory}-${entropyCategory}`;
  }

  /**
   * Get selector statistics
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      ...this.stats,
      learningHistorySize: this.performanceHistory.size,
      mostUsedAlgorithm: Object.entries(this.stats.algorithmUsage)
        .sort((a, b) => b[1] - a[1])[0]?.[0] || 'none'
    };
  }

  /**
   * Reset statistics and learning history
   */
  reset() {
    this.performanceHistory.clear();
    this.stats = {
      totalCompressions: 0,
      algorithmUsage: {
        lz4: 0,
        brotli: 0,
        delta: 0,
        columnar: 0
      }
    };
  }
}

/**
 * Create adaptive compression selector
 * @param {Object} [options] - Selector options
 * @returns {AdaptiveCompressionSelector} Selector instance
 */
export function createAdaptiveSelector(options = {}) {
  return new AdaptiveCompressionSelector(options);
}

/**
 * Quick compress with adaptive selection
 * @param {any} data - Data to compress
 * @param {Object} [options] - Compression options
 * @returns {Promise<Object>} Compressed result
 */
export async function compressAdaptive(data, options = {}) {
  const selector = new AdaptiveCompressionSelector(options);
  return selector.compress(data);
}

/**
 * Quick decompress adaptive-compressed data
 * @param {Object} compressedData - Compressed data
 * @param {Object} [options] - Decompression options
 * @returns {Promise<any>} Decompressed data
 */
export async function decompressAdaptive(compressedData, options = {}) {
  const selector = new AdaptiveCompressionSelector(options);
  return selector.decompress(compressedData);
}

export default AdaptiveCompressionSelector;
