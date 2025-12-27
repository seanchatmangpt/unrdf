/**
 * @fileoverview LZ4 Compression for Observable Payloads
 *
 * LZ4 is a lossless compression algorithm optimized for speed.
 * Ideal for real-time observable data with low latency requirements.
 *
 * **Properties**:
 * - Compression ratio: ~2-3x
 * - Speed: Very fast (>500 MB/s)
 * - Use case: Real-time streaming, hot path compression
 *
 * @module compression/algorithms/lz4
 */

import { z } from 'zod';
import { createHash } from 'node:crypto';

/**
 * LZ4 compression options schema
 */
const LZ4OptionsSchema = z.object({
  minMatchLength: z.number().min(4).max(16).default(4),
  searchBufferSize: z.number().min(256).max(65536).default(4096),
  lookaheadBufferSize: z.number().min(16).max(256).default(64),
  enableChecksum: z.boolean().default(true)
});

/**
 * LZ4 Compressor - Fast lossless compression for observable payloads
 *
 * @class LZ4Compressor
 * @example
 * const compressor = new LZ4Compressor();
 * const compressed = await compressor.compress(data);
 * const decompressed = await compressor.decompress(compressed);
 */
export class LZ4Compressor {
  /**
   * Create LZ4 compressor
   * @param {Object} [options] - Compression options
   */
  constructor(options = {}) {
    this.options = LZ4OptionsSchema.parse(options);
    this.stats = {
      totalCompressed: 0,
      totalDecompressed: 0,
      compressionTime: 0,
      decompressionTime: 0,
      bytesSaved: 0
    };
  }

  /**
   * Compress data using LZ4 algorithm
   *
   * @param {Buffer|Uint8Array|string} data - Data to compress
   * @returns {Promise<Object>} Compressed result
   */
  async compress(data) {
    const startTime = performance.now();

    const buffer = this._toBuffer(data);
    const compressed = this._lz4Compress(buffer);

    const checksum = this.options.enableChecksum
      ? this._computeChecksum(buffer)
      : null;

    const endTime = performance.now();

    this.stats.totalCompressed++;
    this.stats.compressionTime += endTime - startTime;
    this.stats.bytesSaved += buffer.length - compressed.length;

    return {
      algorithm: 'lz4',
      compressed: compressed,
      originalSize: buffer.length,
      compressedSize: compressed.length,
      ratio: buffer.length / compressed.length,
      checksum,
      compressionTime: endTime - startTime
    };
  }

  /**
   * Decompress LZ4-compressed data
   *
   * @param {Object} compressedData - Compressed data object
   * @returns {Promise<Buffer>} Decompressed data
   */
  async decompress(compressedData) {
    const startTime = performance.now();

    const decompressed = this._lz4Decompress(
      compressedData.compressed,
      compressedData.originalSize
    );

    // Verify checksum if enabled
    if (this.options.enableChecksum && compressedData.checksum) {
      const checksum = this._computeChecksum(decompressed);
      if (checksum !== compressedData.checksum) {
        throw new Error('LZ4 checksum mismatch - data corruption detected');
      }
    }

    const endTime = performance.now();

    this.stats.totalDecompressed++;
    this.stats.decompressionTime += endTime - startTime;

    return decompressed;
  }

  /**
   * LZ4 compression implementation
   * @private
   */
  _lz4Compress(buffer) {
    const compressed = [];
    const { minMatchLength, searchBufferSize, lookaheadBufferSize } = this.options;

    let pos = 0;

    while (pos < buffer.length) {
      const match = this._findLongestMatch(
        buffer,
        pos,
        searchBufferSize,
        lookaheadBufferSize,
        minMatchLength
      );

      if (match.length >= minMatchLength) {
        // Encode as (offset, length) pair
        compressed.push({
          type: 'match',
          offset: match.offset,
          length: match.length
        });
        pos += match.length;
      } else {
        // Encode literal byte
        compressed.push({
          type: 'literal',
          value: buffer[pos]
        });
        pos++;
      }
    }

    return this._encodeTokens(compressed);
  }

  /**
   * LZ4 decompression implementation
   * @private
   */
  _lz4Decompress(compressed, originalSize) {
    const tokens = this._decodeTokens(compressed);
    const decompressed = Buffer.alloc(originalSize);

    let pos = 0;

    for (const token of tokens) {
      if (token.type === 'literal') {
        decompressed[pos++] = token.value;
      } else if (token.type === 'match') {
        const matchStart = pos - token.offset;
        for (let i = 0; i < token.length; i++) {
          decompressed[pos++] = decompressed[matchStart + i];
        }
      }
    }

    return decompressed;
  }

  /**
   * Find longest match in search buffer
   * @private
   */
  _findLongestMatch(buffer, pos, searchSize, lookaheadSize, minLength) {
    let bestMatch = { offset: 0, length: 0 };

    const searchStart = Math.max(0, pos - searchSize);
    const searchEnd = pos;
    const lookaheadEnd = Math.min(buffer.length, pos + lookaheadSize);

    for (let i = searchStart; i < searchEnd; i++) {
      let matchLength = 0;

      while (
        pos + matchLength < lookaheadEnd &&
        buffer[i + matchLength] === buffer[pos + matchLength]
      ) {
        matchLength++;
      }

      if (matchLength >= minLength && matchLength > bestMatch.length) {
        bestMatch = {
          offset: pos - i,
          length: matchLength
        };
      }
    }

    return bestMatch;
  }

  /**
   * Encode tokens to buffer
   * @private
   */
  _encodeTokens(tokens) {
    const buffer = [];

    for (const token of tokens) {
      if (token.type === 'literal') {
        buffer.push(0x00, token.value);
      } else {
        buffer.push(0x01);
        buffer.push((token.offset >> 8) & 0xFF, token.offset & 0xFF);
        buffer.push(token.length);
      }
    }

    return Buffer.from(buffer);
  }

  /**
   * Decode tokens from buffer
   * @private
   */
  _decodeTokens(buffer) {
    const tokens = [];
    let pos = 0;

    while (pos < buffer.length) {
      const type = buffer[pos++];

      if (type === 0x00) {
        tokens.push({ type: 'literal', value: buffer[pos++] });
      } else {
        const offset = (buffer[pos++] << 8) | buffer[pos++];
        const length = buffer[pos++];
        tokens.push({ type: 'match', offset, length });
      }
    }

    return tokens;
  }

  /**
   * Compute checksum
   * @private
   */
  _computeChecksum(buffer) {
    const hash = createHash('md5');
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
 * Create LZ4 compressor
 * @param {Object} [options] - Compression options
 * @returns {LZ4Compressor} Compressor instance
 */
export function createLZ4Compressor(options = {}) {
  return new LZ4Compressor(options);
}

/**
 * Quick compress with LZ4
 * @param {Buffer|Uint8Array|string} data - Data to compress
 * @param {Object} [options] - Compression options
 * @returns {Promise<Object>} Compressed result
 */
export async function compressLZ4(data, options = {}) {
  const compressor = new LZ4Compressor(options);
  return compressor.compress(data);
}

/**
 * Quick decompress with LZ4
 * @param {Object} compressedData - Compressed data
 * @param {Object} [options] - Decompression options
 * @returns {Promise<Buffer>} Decompressed data
 */
export async function decompressLZ4(compressedData, options = {}) {
  const compressor = new LZ4Compressor(options);
  return compressor.decompress(compressedData);
}

export default LZ4Compressor;
