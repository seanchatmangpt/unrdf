/**
 * @file RDF Stream Parser - Parse RDF streams with backpressure support
 * @module streaming/rdf-stream-parser
 *
 * @description
 * Node.js Transform stream for parsing RDF data with automatic backpressure
 * handling, chunking, and memory-efficient processing of large datasets.
 */

import { Transform } from 'stream';
import { StreamParser } from '@unrdf/core/rdf/n3-justified-only'; // JUSTIFIED: N3 StreamParser for streaming
import { trace, SpanStatusCode } from '@opentelemetry/api';
import { z } from 'zod';

const tracer = trace.getTracer('@unrdf/streaming');

/**
 * RDF Stream Parser configuration schema
 */
const RDFStreamParserConfigSchema = z.object({
  format: z.enum(['turtle', 'n-triples', 'n-quads', 'trig']).default('turtle'),
  baseIRI: z.string().optional(),
  blankNodePrefix: z.string().optional(),
  highWaterMark: z.number().positive().default(16384), // 16KB default
  chunkSize: z.number().positive().default(1000), // Quads per chunk
  enableBackpressure: z.boolean().default(true),
  onQuad: z.function().optional(),
  onError: z.function().optional(),
  onProgress: z.function().optional(),
});

/**
 * RDF Stream Parser with backpressure support
 *
 * @class RDFStreamParser
 * @extends Transform
 */
export class RDFStreamParser extends Transform {
  /**
   * Create an RDF stream parser
   *
   * @param {Object} [options] - Parser options
   */
  constructor(options = {}) {
    const config = RDFStreamParserConfigSchema.parse(options);

    super({
      objectMode: true,
      highWaterMark: config.highWaterMark,
    });

    this.config = config;
    
    // Create N3 StreamParser instance for incremental parsing
    this.n3StreamParser = new StreamParser({
      baseIRI: config.baseIRI,
      blankNodePrefix: config.blankNodePrefix,
      format: config.format,
    });

    this.quadCount = 0;
    this.chunkBuffer = [];
    this.bytesProcessed = 0;
    this.startTime = Date.now();
    this.backpressureActive = false;

    // Metrics
    this.metrics = {
      quadsProcessed: 0,
      chunksEmitted: 0,
      bytesProcessed: 0,
      backpressureEvents: 0,
      errors: 0,
      duration: 0,
    };

    // Setup N3 StreamParser listeners
    this.n3StreamParser.on('data', (quad) => {
      this.quadCount++;
      this.metrics.quadsProcessed++;

      if (this.config.onQuad) {
        this.config.onQuad(quad);
      }

      this.chunkBuffer.push(quad);

      // Emit chunk when full
      if (this.chunkBuffer.length >= this.config.chunkSize) {
        const canPush = this._emitChunk();
        
        // Respect backpressure from downstream
        if (!canPush && this.config.enableBackpressure) {
          this.n3StreamParser.pause();
          if (!this.backpressureActive) {
            this.backpressureActive = true;
            this.metrics.backpressureEvents++;
          }
        }
      }

      // Report progress periodically
      if (this.config.onProgress && this.quadCount % 10000 === 0) {
        this.config.onProgress({
          quadsProcessed: this.quadCount,
          bytesProcessed: this.bytesProcessed,
          chunksEmitted: this.metrics.chunksEmitted,
          duration: Date.now() - this.startTime,
        });
      }
    });

    this.n3StreamParser.on('error', (error) => {
      this.metrics.errors++;
      if (this.config.onError) {
        this.config.onError(error);
      }
      this.emit('error', error);
    });
  }

  /**
   * Transform stream implementation
   *
   * @param {Buffer|string} chunk - Input chunk
   * @param {string} encoding - Encoding
   * @param {Function} callback - Callback
   * @private
   */
  _transform(chunk, encoding, callback) {
    return tracer.startActiveSpan('rdf-stream-parser.transform', async span => {
      try {
        const len = chunk.length;
        this.bytesProcessed += len;
        this.metrics.bytesProcessed += len;

        span.setAttributes({
          'chunk.size': len,
          'quads.processed': this.quadCount,
        });

        // Write to N3 parser and handle backpressure
        if (!this.n3StreamParser.write(chunk, encoding)) {
          this.n3StreamParser.once('drain', () => {
            span.end();
            callback();
          });
        } else {
          span.end();
          callback();
        }
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        span.end();
        callback(error);
      }
    });
  }

  /**
   * Flush remaining data
   *
   * @param {Function} callback - Callback
   * @private
   */
  _flush(callback) {
    this.n3StreamParser.end();
    this.n3StreamParser.on('end', () => {
      // Emit any remaining quads in chunk buffer
      if (this.chunkBuffer.length > 0) {
        this._emitChunk();
      }

      // Finalize metrics
      this.metrics.duration = Date.now() - this.startTime;

      // Emit final stats
      this.push({
        type: 'complete',
        metrics: this.metrics,
      });

      callback();
    });
  }

  /**
   * Override _read to resume internal parser when downstream is ready
   *
   * @param {number} size - Number of bytes to read
   * @private
   */
  _read(size) {
    if (this.backpressureActive) {
      this.backpressureActive = false;
      this.n3StreamParser.resume();
    }
    super._read(size);
  }

  /**
   * Emit a chunk of quads
   *
   * @returns {boolean} Whether more data can be pushed
   * @private
   */
  _emitChunk() {
    if (this.chunkBuffer.length === 0) return true;

    const chunk = {
      type: 'quads',
      data: [...this.chunkBuffer],
      count: this.chunkBuffer.length,
      timestamp: Date.now(),
    };

    const canPush = this.push(chunk);
    this.metrics.chunksEmitted++;
    this.chunkBuffer = [];
    return canPush;
  }

  /**
   * Get current parsing metrics
   *
   * @returns {Object} Metrics
   */
  getMetrics() {
    return {
      ...this.metrics,
      throughput:
        this.metrics.duration > 0
          ? (this.metrics.quadsProcessed / this.metrics.duration) * 1000
          : 0,
      backpressureRate:
        this.metrics.backpressureEvents > 0
          ? this.metrics.backpressureEvents / this.metrics.chunksEmitted
          : 0,
    };
  }
}

/**
 * Create an RDF stream parser
 *
 * @param {Object} [options] - Parser options
 * @returns {RDFStreamParser} Stream parser instance
 */
export function createRDFStreamParser(options = {}) {
  return new RDFStreamParser(options);
}

/**
 * Parse an RDF stream and collect all quads
 *
 * @param {ReadableStream} stream - Input stream
 * @param {Object} [options] - Parser options
 * @returns {Promise<Array>} Array of all parsed quads
 */
export async function parseRDFStream(stream, options = {}) {
  return new Promise((resolve, reject) => {
    const parser = createRDFStreamParser(options);
    const allQuads = [];

    parser.on('data', chunk => {
      if (chunk.type === 'quads') {
        allQuads.push(...chunk.data);
      }
    });

    parser.on('end', () => resolve(allQuads));
    parser.on('error', reject);

    stream.pipe(parser);
  });
}
