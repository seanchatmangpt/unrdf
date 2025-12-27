/**
 * @fileoverview Columnar Compression for Structured Observable Data
 *
 * Columnar compression leverages structure by compressing each field
 * independently, enabling better compression ratios for homogeneous data.
 *
 * **Properties**:
 * - Compression ratio: 3-10x (depends on data homogeneity)
 * - Speed: Fast
 * - Use case: Structured observables, tabular data, RDF quads
 *
 * **Theory**:
 * For structured data like [{s,p,o,g}, ...]:
 * - Separate into columns: S[], P[], O[], G[]
 * - Compress each column independently (better ratios for homogeneous data)
 * - Reconstruct by column merging
 *
 * @module compression/algorithms/columnar
 */

import { z } from 'zod';
import { createHash } from 'node:crypto';

/**
 * Columnar compression options schema
 */
const ColumnarOptionsSchema = z.object({
  enableDictionary: z.boolean().default(true), // Dictionary encoding for repeated values
  enableRLE: z.boolean().default(true), // Run-length encoding
  enableChecksum: z.boolean().default(true)
});

/**
 * Columnar Compressor - Structure-aware compression
 *
 * @class ColumnarCompressor
 * @example
 * const compressor = new ColumnarCompressor();
 * const compressed = await compressor.compress([{s,p,o,g}, ...]);
 * const decompressed = await compressor.decompress(compressed);
 */
export class ColumnarCompressor {
  /**
   * Create columnar compressor
   * @param {Object} [options] - Compression options
   */
  constructor(options = {}) {
    this.options = ColumnarOptionsSchema.parse(options);
    this.stats = {
      totalCompressed: 0,
      totalDecompressed: 0,
      compressionTime: 0,
      decompressionTime: 0,
      bytesSaved: 0
    };
  }

  /**
   * Compress structured data using columnar compression
   *
   * @param {Array<Object>} data - Array of objects with consistent schema
   * @returns {Promise<Object>} Compressed result
   */
  async compress(data) {
    const startTime = performance.now();

    if (!Array.isArray(data) || data.length === 0) {
      throw new Error('Columnar compression requires non-empty array');
    }

    // Extract schema (column names)
    const schema = Object.keys(data[0]);

    // Convert to columnar format
    const columns = {};
    for (const key of schema) {
      columns[key] = data.map(row => row[key]);
    }

    // Compress each column
    const compressedColumns = {};
    for (const [key, column] of Object.entries(columns)) {
      compressedColumns[key] = this._compressColumn(column);
    }

    const encoded = {
      schema,
      columns: compressedColumns,
      rowCount: data.length
    };

    const serialized = JSON.stringify(encoded);
    const compressed = Buffer.from(serialized, 'utf-8');
    const originalSize = this._estimateSize(data);

    const checksum = this.options.enableChecksum
      ? this._computeChecksum(compressed)
      : null;

    const endTime = performance.now();

    this.stats.totalCompressed++;
    this.stats.compressionTime += endTime - startTime;
    this.stats.bytesSaved += originalSize - compressed.length;

    return {
      algorithm: 'columnar',
      compressed: encoded,
      originalSize,
      compressedSize: compressed.length,
      ratio: originalSize / compressed.length,
      checksum,
      compressionTime: endTime - startTime,
      metadata: {
        rowCount: data.length,
        columnCount: schema.length,
        schema
      }
    };
  }

  /**
   * Decompress columnar-compressed data
   *
   * @param {Object} compressedData - Compressed data object
   * @returns {Promise<Array<Object>>} Decompressed data
   */
  async decompress(compressedData) {
    const startTime = performance.now();

    const { compressed } = compressedData;
    const { schema, columns, rowCount } = compressed;

    // Verify checksum if enabled
    if (this.options.enableChecksum && compressedData.checksum) {
      const serialized = JSON.stringify(compressed);
      const buffer = Buffer.from(serialized, 'utf-8');
      const checksum = this._computeChecksum(buffer);
      if (checksum !== compressedData.checksum) {
        throw new Error('Columnar checksum mismatch - data corruption detected');
      }
    }

    // Decompress each column
    const decompressedColumns = {};
    for (const [key, compressedColumn] of Object.entries(columns)) {
      decompressedColumns[key] = this._decompressColumn(compressedColumn);
    }

    // Reconstruct rows
    const data = [];
    for (let i = 0; i < rowCount; i++) {
      const row = {};
      for (const key of schema) {
        row[key] = decompressedColumns[key][i];
      }
      data.push(row);
    }

    const endTime = performance.now();

    this.stats.totalDecompressed++;
    this.stats.decompressionTime += endTime - startTime;

    return data;
  }

  /**
   * Compress single column
   * @private
   */
  _compressColumn(column) {
    let compressed = { values: column };

    // Apply dictionary encoding if enabled
    if (this.options.enableDictionary) {
      compressed = this._dictionaryEncode(column);
    }

    // Apply RLE if enabled
    if (this.options.enableRLE && compressed.indices) {
      compressed.indices = this._runLengthEncode(compressed.indices);
    }

    return compressed;
  }

  /**
   * Decompress single column
   * @private
   */
  _decompressColumn(compressedColumn) {
    let column = compressedColumn;

    // Decode RLE if present
    if (column.indices && column.indices.rle) {
      column = {
        ...column,
        indices: this._runLengthDecode(column.indices)
      };
    }

    // Decode dictionary if present
    if (column.dictionary) {
      return this._dictionaryDecode(column);
    }

    return column.values;
  }

  /**
   * Dictionary encoding
   * @private
   */
  _dictionaryEncode(values) {
    const dictionary = [];
    const indices = [];
    const valueToIndex = new Map();

    for (const value of values) {
      const key = JSON.stringify(value);

      if (!valueToIndex.has(key)) {
        valueToIndex.set(key, dictionary.length);
        dictionary.push(value);
      }

      indices.push(valueToIndex.get(key));
    }

    // Only use dictionary if it saves space
    const originalSize = JSON.stringify(values).length;
    const dictSize = JSON.stringify(dictionary).length + JSON.stringify(indices).length;

    if (dictSize < originalSize) {
      return { dictionary, indices };
    }

    return { values };
  }

  /**
   * Dictionary decoding
   * @private
   */
  _dictionaryDecode({ dictionary, indices }) {
    if (indices.rle) {
      indices = this._runLengthDecode(indices);
    }
    return indices.map(idx => dictionary[idx]);
  }

  /**
   * Run-length encoding
   * @private
   */
  _runLengthEncode(values) {
    const runs = [];
    let currentValue = values[0];
    let currentRun = 1;

    for (let i = 1; i < values.length; i++) {
      if (values[i] === currentValue) {
        currentRun++;
      } else {
        runs.push({ value: currentValue, length: currentRun });
        currentValue = values[i];
        currentRun = 1;
      }
    }

    runs.push({ value: currentValue, length: currentRun });

    // Only use RLE if it saves space
    if (runs.length < values.length * 0.7) {
      return { rle: true, runs };
    }

    return values;
  }

  /**
   * Run-length decoding
   * @private
   */
  _runLengthDecode({ rle, runs }) {
    if (!rle) return runs;

    const values = [];
    for (const { value, length } of runs) {
      for (let i = 0; i < length; i++) {
        values.push(value);
      }
    }
    return values;
  }

  /**
   * Estimate size of data
   * @private
   */
  _estimateSize(data) {
    const serialized = JSON.stringify(data);
    return Buffer.byteLength(serialized, 'utf-8');
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
 * Create columnar compressor
 * @param {Object} [options] - Compression options
 * @returns {ColumnarCompressor} Compressor instance
 */
export function createColumnarCompressor(options = {}) {
  return new ColumnarCompressor(options);
}

/**
 * Quick compress with columnar compression
 * @param {Array<Object>} data - Structured data
 * @param {Object} [options] - Compression options
 * @returns {Promise<Object>} Compressed result
 */
export async function compressColumnar(data, options = {}) {
  const compressor = new ColumnarCompressor(options);
  return compressor.compress(data);
}

/**
 * Quick decompress columnar-compressed data
 * @param {Object} compressedData - Compressed data
 * @param {Object} [options] - Decompression options
 * @returns {Promise<Array<Object>>} Decompressed data
 */
export async function decompressColumnar(compressedData, options = {}) {
  const compressor = new ColumnarCompressor(options);
  return compressor.decompress(compressedData);
}

export default ColumnarCompressor;
