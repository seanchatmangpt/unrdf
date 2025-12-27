/**
 * @fileoverview Semantic Compression with Pattern Extraction and Schema Awareness
 *
 * Semantic compression leverages domain knowledge to achieve superior
 * compression ratios by:
 * - Extracting and encoding repeated patterns
 * - Using Zod schemas for structure-aware compression
 * - Learning dictionaries for common structures
 *
 * @module compression/semantic-compressor
 */

import { z } from 'zod';
import { createHash } from 'node:crypto';

/**
 * Semantic compression options schema
 */
const SemanticOptionsSchema = z.object({
  enablePatternExtraction: z.boolean().default(true),
  enableSchemaAwareness: z.boolean().default(true),
  enableDictionaryLearning: z.boolean().default(true),
  minPatternOccurrences: z.number().min(2).default(3),
  maxDictionarySize: z.number().min(10).max(10000).default(1000),
  enableChecksum: z.boolean().default(true)
});

/**
 * Semantic Compressor
 *
 * @class SemanticCompressor
 * @example
 * const compressor = new SemanticCompressor();
 * const compressed = await compressor.compress(data, schema);
 * const decompressed = await compressor.decompress(compressed);
 */
export class SemanticCompressor {
  /**
   * Create semantic compressor
   * @param {Object} [options] - Compression options
   */
  constructor(options = {}) {
    this.options = SemanticOptionsSchema.parse(options);

    // Learned dictionaries
    this.globalDictionary = new Map();
    this.patternDictionary = new Map();

    this.stats = {
      totalCompressed: 0,
      totalDecompressed: 0,
      compressionTime: 0,
      decompressionTime: 0,
      bytesSaved: 0,
      patternsExtracted: 0
    };
  }

  /**
   * Compress data with semantic compression
   *
   * @param {any} data - Data to compress
   * @param {Object} [schema] - Optional Zod schema for schema-aware compression
   * @returns {Promise<Object>} Compressed result
   */
  async compress(data, schema = null) {
    const startTime = performance.now();

    const serialized = JSON.stringify(data);
    const originalSize = Buffer.byteLength(serialized, 'utf-8');

    let compressed = data;
    const transformations = [];

    // 1. Schema-aware compression
    if (this.options.enableSchemaAwareness && schema) {
      const schemaResult = this._schemaAwareCompress(compressed, schema);
      compressed = schemaResult.data;
      transformations.push(schemaResult.transformation);
    }

    // 2. Pattern extraction
    if (this.options.enablePatternExtraction) {
      const patternResult = this._extractPatterns(compressed);
      compressed = patternResult.data;
      transformations.push(patternResult.transformation);
      this.stats.patternsExtracted += patternResult.patternsFound;
    }

    // 3. Dictionary learning
    if (this.options.enableDictionaryLearning) {
      const dictResult = this._dictionaryCompress(compressed);
      compressed = dictResult.data;
      transformations.push(dictResult.transformation);
    }

    const compressedSerialized = JSON.stringify(compressed);
    const compressedSize = Buffer.byteLength(compressedSerialized, 'utf-8');

    const checksum = this.options.enableChecksum
      ? this._computeChecksum(Buffer.from(serialized, 'utf-8'))
      : null;

    const endTime = performance.now();

    this.stats.totalCompressed++;
    this.stats.compressionTime += endTime - startTime;
    this.stats.bytesSaved += originalSize - compressedSize;

    return {
      algorithm: 'semantic',
      compressed: {
        data: compressed,
        transformations,
        schema: schema ? this._serializeSchema(schema) : null
      },
      originalSize,
      compressedSize,
      ratio: originalSize / compressedSize,
      checksum,
      compressionTime: endTime - startTime
    };
  }

  /**
   * Decompress semantically-compressed data
   *
   * @param {Object} compressedData - Compressed data object
   * @returns {Promise<any>} Decompressed data
   */
  async decompress(compressedData) {
    const startTime = performance.now();

    let { data, transformations, schema } = compressedData.compressed;

    // Apply transformations in reverse order
    for (let i = transformations.length - 1; i >= 0; i--) {
      const transformation = transformations[i];

      if (transformation.type === 'dictionary') {
        data = this._dictionaryDecompress(data, transformation);
      } else if (transformation.type === 'pattern') {
        data = this._restorePatterns(data, transformation);
      } else if (transformation.type === 'schema') {
        data = this._schemaAwareDecompress(data, transformation);
      }
    }

    // Verify checksum if enabled
    if (this.options.enableChecksum && compressedData.checksum) {
      const serialized = JSON.stringify(data);
      const buffer = Buffer.from(serialized, 'utf-8');
      const checksum = this._computeChecksum(buffer);
      if (checksum !== compressedData.checksum) {
        throw new Error('Semantic checksum mismatch - data corruption detected');
      }
    }

    const endTime = performance.now();

    this.stats.totalDecompressed++;
    this.stats.decompressionTime += endTime - startTime;

    return data;
  }

  /**
   * Schema-aware compression
   * @private
   */
  _schemaAwareCompress(data, schema) {
    // Validate against schema
    const validated = schema.parse(data);

    // For arrays of objects, we can omit field names (schema provides them)
    if (Array.isArray(validated) && validated.length > 0 && typeof validated[0] === 'object') {
      const shape = schema._def.type?._def.shape || schema.shape || {};
      const fields = Object.keys(shape);

      const compressedArray = validated.map(item => {
        return fields.map(field => item[field]);
      });

      return {
        data: compressedArray,
        transformation: {
          type: 'schema',
          fields
        }
      };
    }

    return {
      data: validated,
      transformation: {
        type: 'schema',
        fields: []
      }
    };
  }

  /**
   * Schema-aware decompression
   * @private
   */
  _schemaAwareDecompress(data, transformation) {
    if (transformation.fields.length === 0) {
      return data;
    }

    // Restore field names
    return data.map(values => {
      const obj = {};
      transformation.fields.forEach((field, idx) => {
        obj[field] = values[idx];
      });
      return obj;
    });
  }

  /**
   * Extract repeated patterns
   * @private
   */
  _extractPatterns(data) {
    const serialized = JSON.stringify(data);

    // Simplified pattern extraction - disabled for now to avoid JSON parsing issues
    // Pattern extraction would need more sophisticated handling to avoid breaking JSON structure
    return {
      data,
      transformation: { type: 'pattern', patterns: {} },
      patternsFound: 0
    };
  }

  /**
   * Restore patterns
   * @private
   */
  _restorePatterns(data, transformation) {
    // No-op if patterns are empty
    if (Object.keys(transformation.patterns).length === 0) {
      return data;
    }

    let serialized = JSON.stringify(data);

    for (const [ref, pattern] of Object.entries(transformation.patterns)) {
      serialized = serialized.split(ref).join(pattern);
    }

    return JSON.parse(serialized);
  }

  /**
   * Dictionary compression
   * @private
   */
  _dictionaryCompress(data) {
    const dictionary = {};
    let dictId = 0;

    const compress = (obj) => {
      if (typeof obj !== 'object' || obj === null) {
        return obj;
      }

      if (Array.isArray(obj)) {
        return obj.map(compress);
      }

      // Check if object is in dictionary
      const key = JSON.stringify(obj);
      if (key.length > 50) { // Only compress larger objects
        if (!dictionary[key]) {
          if (dictId < this.options.maxDictionarySize) {
            dictionary[key] = { id: dictId++, value: obj };
          }
        }

        if (dictionary[key]) {
          return { __DICT: dictionary[key].id };
        }
      }

      // Recursively compress nested objects
      const compressed = {};
      for (const [k, v] of Object.entries(obj)) {
        compressed[k] = compress(v);
      }
      return compressed;
    };

    const compressedData = compress(data);

    // Build dictionary lookup
    const dictLookup = {};
    for (const [key, info] of Object.entries(dictionary)) {
      dictLookup[info.id] = info.value;
    }

    return {
      data: compressedData,
      transformation: {
        type: 'dictionary',
        dictionary: dictLookup
      }
    };
  }

  /**
   * Dictionary decompression
   * @private
   */
  _dictionaryDecompress(data, transformation) {
    const { dictionary } = transformation;

    const decompress = (obj) => {
      if (typeof obj !== 'object' || obj === null) {
        return obj;
      }

      if ('__DICT' in obj) {
        return dictionary[obj.__DICT];
      }

      if (Array.isArray(obj)) {
        return obj.map(decompress);
      }

      const decompressed = {};
      for (const [k, v] of Object.entries(obj)) {
        decompressed[k] = decompress(v);
      }
      return decompressed;
    };

    return decompress(data);
  }

  /**
   * Serialize Zod schema (simplified)
   * @private
   */
  _serializeSchema(schema) {
    // Simplified schema serialization
    return {
      type: schema._def.typeName || 'ZodObject',
      // Add more schema metadata as needed
    };
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
      dictionarySize: this.globalDictionary.size,
      avgCompressionTime: this.stats.totalCompressed > 0
        ? this.stats.compressionTime / this.stats.totalCompressed
        : 0,
      avgDecompressionTime: this.stats.totalDecompressed > 0
        ? this.stats.decompressionTime / this.stats.totalDecompressed
        : 0
    };
  }

  /**
   * Clear learned dictionaries
   */
  clearDictionaries() {
    this.globalDictionary.clear();
    this.patternDictionary.clear();
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
      bytesSaved: 0,
      patternsExtracted: 0
    };
  }
}

/**
 * Create semantic compressor
 * @param {Object} [options] - Compression options
 * @returns {SemanticCompressor} Compressor instance
 */
export function createSemanticCompressor(options = {}) {
  return new SemanticCompressor(options);
}

/**
 * Quick compress with semantic compression
 * @param {any} data - Data to compress
 * @param {Object} [schema] - Optional Zod schema
 * @param {Object} [options] - Compression options
 * @returns {Promise<Object>} Compressed result
 */
export async function compressSemantic(data, schema = null, options = {}) {
  const compressor = new SemanticCompressor(options);
  return compressor.compress(data, schema);
}

/**
 * Quick decompress semantically-compressed data
 * @param {Object} compressedData - Compressed data
 * @param {Object} [options] - Decompression options
 * @returns {Promise<any>} Decompressed data
 */
export async function decompressSemantic(compressedData, options = {}) {
  const compressor = new SemanticCompressor(options);
  return compressor.decompress(compressedData);
}

export default SemanticCompressor;
