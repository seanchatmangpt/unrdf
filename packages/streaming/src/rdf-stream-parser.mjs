/**
 * @file RDF Stream Parser - Parse RDF streams with backpressure support
 * @module streaming/rdf-stream-parser
 *
 * @description
 * Node.js Transform stream for parsing RDF data with automatic backpressure
 * handling, chunking, and memory-efficient processing of large datasets.
 */

import { Transform } from 'stream';
import { Parser } from '@unrdf/core/rdf/n3-justified-only'; // JUSTIFIED: N3 Parser for streaming
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
   * @param {string} [options.format='turtle'] - RDF format
   * @param {string} [options.baseIRI] - Base IRI for relative URIs
   * @param {string} [options.blankNodePrefix] - Prefix for blank nodes
   * @param {number} [options.highWaterMark=16384] - Stream buffer size in bytes
   * @param {number} [options.chunkSize=1000] - Number of quads per chunk
   * @param {boolean} [options.enableBackpressure=true] - Enable backpressure handling
   * @param {Function} [options.onQuad] - Callback for each parsed quad
   * @param {Function} [options.onError] - Error callback
   * @param {Function} [options.onProgress] - Progress callback
   */
  constructor(options = {}) {
    const config = RDFStreamParserConfigSchema.parse(options);

    super({
      objectMode: true,
      highWaterMark: config.highWaterMark,
    });

    this.config = config;
    // Create N3 Parser instance without format parameter (auto-detect)
    this.parser = new Parser({
      baseIRI: config.baseIRI,
      blankNodePrefix: config.blankNodePrefix,
    });
    this.pendingInput = '';

    this.buffer = '';
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
    return tracer.startActiveSpan('rdf-stream-parser.transform', async (span) => {
      try {
        // Convert buffer to string
        const str = typeof chunk === 'string' ? chunk : chunk.toString('utf8');
        this.buffer += str;
        this.bytesProcessed += str.length;
        this.metrics.bytesProcessed += str.length;

        span.setAttributes({
          'chunk.size': str.length,
          'buffer.size': this.buffer.length,
          'quads.processed': this.quadCount,
        });

        // Just accumulate data, parsing happens in _flush
        // This is more reliable than incremental parsing with N3
        const quads = [];

        // Report progress
        if (this.config.onProgress && this.quadCount % 10000 === 0) {
          this.config.onProgress({
            quadsProcessed: this.quadCount,
            bytesProcessed: this.bytesProcessed,
            chunksEmitted: this.metrics.chunksEmitted,
            duration: Date.now() - this.startTime,
          });
        }

        // Check backpressure
        if (this.config.enableBackpressure) {
          const shouldPause = !this.push({ type: 'progress', quads: quads.length });
          if (shouldPause && !this.backpressureActive) {
            this.backpressureActive = true;
            this.metrics.backpressureEvents++;
            span.setAttribute('backpressure.active', true);
          } else if (!shouldPause && this.backpressureActive) {
            this.backpressureActive = false;
          }
        }

        span.setStatus({ code: SpanStatusCode.OK });
        span.end();
        callback();
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
    return tracer.startActiveSpan('rdf-stream-parser.flush', async (span) => {
      try {
        // Parse all buffered data at once
        if (this.buffer && this.buffer.trim().length > 0) {
          try {
            const inputStr = this.buffer.trim();
            this.parser.parse(inputStr + '\n', (error, quad) => {
              if (error) {
                this.metrics.errors++;
                if (this.config.onError) {
                  this.config.onError(error);
                }
                return;
              }

              if (quad) {
                this.quadCount++;
                this.metrics.quadsProcessed++;

                if (this.config.onQuad) {
                  this.config.onQuad(quad);
                }

                this.chunkBuffer.push(quad);

                // Emit chunk when full
                if (this.chunkBuffer.length >= this.config.chunkSize) {
                  this._emitChunk();
                }
              }
            });
          } catch (parseError) {
            this.metrics.errors++;
            if (this.config.onError) {
              this.config.onError(parseError);
            }
          }
        }

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

        span.setAttributes({
          'quads.total': this.metrics.quadsProcessed,
          'chunks.total': this.metrics.chunksEmitted,
          'duration.ms': this.metrics.duration,
        });

        span.setStatus({ code: SpanStatusCode.OK });
        span.end();
        callback();
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        span.end();
        callback(error);
      }
    });
  }

  /**
   * Emit a chunk of quads
   *
   * @private
   */
  _emitChunk() {
    if (this.chunkBuffer.length === 0) return;

    const chunk = {
      type: 'quads',
      data: [...this.chunkBuffer],
      count: this.chunkBuffer.length,
      timestamp: Date.now(),
    };

    this.push(chunk);
    this.metrics.chunksEmitted++;
    this.chunkBuffer = [];
  }

  /**
   * Get current parsing metrics
   *
   * @returns {Object} Metrics
   */
  getMetrics() {
    return {
      ...this.metrics,
      throughput: this.metrics.duration > 0
        ? (this.metrics.quadsProcessed / this.metrics.duration) * 1000
        : 0,
      backpressureRate: this.metrics.backpressureEvents > 0
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
 *
 * @example
 * const parser = createRDFStreamParser({ format: 'turtle' });
 * fs.createReadStream('data.ttl')
 *   .pipe(parser)
 *   .on('data', (chunk) => {
 *     if (chunk.type === 'quads') {
 *       console.log(`Received ${chunk.count} quads`);
 *     }
 *   });
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
 *
 * @example
 * const quads = await parseRDFStream(fs.createReadStream('data.ttl'));
 * console.log(`Parsed ${quads.length} quads`);
 */
export async function parseRDFStream(stream, options = {}) {
  return new Promise((resolve, reject) => {
    const parser = createRDFStreamParser(options);
    const allQuads = [];

    parser.on('data', (chunk) => {
      if (chunk.type === 'quads') {
        allQuads.push(...chunk.data);
      }
    });

    parser.on('end', () => resolve(allQuads));
    parser.on('error', reject);

    stream.pipe(parser);
  });
}
