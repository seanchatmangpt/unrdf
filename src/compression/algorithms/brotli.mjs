/**
 * @fileoverview Brotli Compression for High Compression Ratio
 *
 * Brotli provides excellent compression ratios at the cost of speed.
 * Ideal for cold storage, archival, and bandwidth-constrained scenarios.
 *
 * **Properties**:
 * - Compression ratio: ~4-6x (better than gzip)
 * - Speed: Moderate (slower than LZ4, faster than LZMA)
 * - Use case: Archival, cold storage, API responses
 *
 * @module compression/algorithms/brotli
 */

import { z } from 'zod';
import { createHash } from 'node:crypto';
import { brotliCompress, brotliDecompress, constants } from 'node:zlib';
import { promisify } from 'node:util';

const brotliCompressAsync = promisify(brotliCompress);
const brotliDecompressAsync = promisify(brotliDecompress);

/**
 * Brotli compression options schema
 */
const BrotliOptionsSchema = z.object({
  quality: z.number().min(0).max(11).default(6), // 0=fast, 11=best compression
  windowSize: z.number().min(10).max(24).default(22), // 2^windowSize bytes
  mode: z.enum(['generic', 'text', 'font']).default('generic'),
  enableChecksum: z.boolean().default(true)
});

/**
 * Brotli Compressor - High compression ratio for observable payloads
 *
 * @class BrotliCompressor
 * @example
 * const compressor = new BrotliCompressor({ quality: 9 });
 * const compressed = await compressor.compress(data);
 * const decompressed = await compressor.decompress(compressed);
 */
export class BrotliCompressor {
  /**
   * Create Brotli compressor
   * @param {Object} [options] - Compression options
   */
  constructor(options = {}) {
    this.options = BrotliOptionsSchema.parse(options);
    this.stats = {
      totalCompressed: 0,
      totalDecompressed: 0,
      compressionTime: 0,
      decompressionTime: 0,
      bytesSaved: 0
    };
  }

  /**
   * Compress data using Brotli algorithm
   *
   * @param {Buffer|Uint8Array|string} data - Data to compress
   * @returns {Promise<Object>} Compressed result
   */
  async compress(data) {
    const startTime = performance.now();

    const buffer = this._toBuffer(data);

    const brotliOptions = {
      params: {
        [constants.BROTLI_PARAM_QUALITY]: this.options.quality,
        [constants.BROTLI_PARAM_LGWIN]: this.options.windowSize,
        [constants.BROTLI_PARAM_MODE]: this._getModeConstant(this.options.mode)
      }
    };

    const compressed = await brotliCompressAsync(buffer, brotliOptions);

    const checksum = this.options.enableChecksum
      ? this._computeChecksum(buffer)
      : null;

    const endTime = performance.now();

    this.stats.totalCompressed++;
    this.stats.compressionTime += endTime - startTime;
    this.stats.bytesSaved += buffer.length - compressed.length;

    return {
      algorithm: 'brotli',
      compressed: compressed,
      originalSize: buffer.length,
      compressedSize: compressed.length,
      ratio: buffer.length / compressed.length,
      checksum,
      quality: this.options.quality,
      compressionTime: endTime - startTime
    };
  }

  /**
   * Decompress Brotli-compressed data
   *
   * @param {Object} compressedData - Compressed data object
   * @returns {Promise<Buffer>} Decompressed data
   */
  async decompress(compressedData) {
    const startTime = performance.now();

    const decompressed = await brotliDecompressAsync(compressedData.compressed);

    // Verify checksum if enabled
    if (this.options.enableChecksum && compressedData.checksum) {
      const checksum = this._computeChecksum(decompressed);
      if (checksum !== compressedData.checksum) {
        throw new Error('Brotli checksum mismatch - data corruption detected');
      }
    }

    const endTime = performance.now();

    this.stats.totalDecompressed++;
    this.stats.decompressionTime += endTime - startTime;

    return decompressed;
  }

  /**
   * Get Brotli mode constant
   * @private
   */
  _getModeConstant(mode) {
    const modes = {
      generic: constants.BROTLI_MODE_GENERIC,
      text: constants.BROTLI_MODE_TEXT,
      font: constants.BROTLI_MODE_FONT
    };
    return modes[mode] || constants.BROTLI_MODE_GENERIC;
  }

  /**
   * Compute checksum
   * @private
   */
  _computeChecksum(buffer) {
    const hash = createHash('sha256');
    hash.update(buffer);
    return hash.digest('hex');
  }

  /**
   * Convert input to buffer
   * @private
   */
  _toBuffer(data) {
    if (Buffer.isBuffer(data)) return data;
    if (data instanceof Uint8Array) return Buffer.from(data);
    if (typeof data === 'string') return Buffer.from(data, 'utf-8');
    return Buffer.from(JSON.stringify(data), 'utf-8');
  }

  /**
   * Get compression statistics
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      ...this.stats,
      avgCompressionTime: this.stats.totalCompressed > 0
        ? this.stats.compressionTime / this.stats.totalCompressed
        : 0,
      avgDecompressionTime: this.stats.totalDecompressed > 0
        ? this.stats.decompressionTime / this.stats.totalDecompressed
        : 0
    };
  }

  /**
   * Reset statistics
   */
  resetStats() {
    this.stats = {
      totalCompressed: 0,
      totalDecompressed: 0,
      compressionTime: 0,
      decompressionTime: 0,
      bytesSaved: 0
    };
  }
}

/**
 * Create Brotli compressor
 * @param {Object} [options] - Compression options
 * @returns {BrotliCompressor} Compressor instance
 */
export function createBrotliCompressor(options = {}) {
  return new BrotliCompressor(options);
}

/**
 * Quick compress with Brotli
 * @param {Buffer|Uint8Array|string} data - Data to compress
 * @param {Object} [options] - Compression options
 * @returns {Promise<Object>} Compressed result
 */
export async function compressBrotli(data, options = {}) {
  const compressor = new BrotliCompressor(options);
  return compressor.compress(data);
}

/**
 * Quick decompress with Brotli
 * @param {Object} compressedData - Compressed data
 * @param {Object} [options] - Decompression options
 * @returns {Promise<Buffer>} Decompressed data
 */
export async function decompressBrotli(compressedData, options = {}) {
  const compressor = new BrotliCompressor(options);
  return compressor.decompress(compressedData);
}

export default BrotliCompressor;
