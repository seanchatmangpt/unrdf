/** DynamoDB RDF storage adapter with SPO/PSO/OSP access paths. */

import { createRequire } from 'node:module';
import { z } from 'zod';
import {
  DynamoRdfStore,
  createAwsCommandFactory,
  createPlainCommandFactory,
  encodeTriple,
  decodeTriple,
  encodeToken,
  decodeToken,
} from './dynamodb-core.mjs';

export const TripleSchema = z.object({
  subject: z.string().min(1),
  predicate: z.string().min(1),
  object: z.string().min(1),
  graph: z.string().min(1).optional(),
});

export const TriplePatternSchema = z.object({
  subject: z.string().min(1).optional(),
  predicate: z.string().min(1).optional(),
  object: z.string().min(1).optional(),
  graph: z.string().min(1).optional(),
});

/**
 * Validating DynamoDB triple store adapter; parses inputs with Zod before delegating to `DynamoRdfStore`.
 */
export class DynamoDBAdapter {
  #store;

  /**
   * Creates an adapter over a DynamoDB client.
   *
   * @param {{send: Function}} client - Client exposing `send(command)`.
   * @param {string} tableName - Name of the triples table.
   * @param {Object} [options={}] - Options passed to `DynamoRdfStore` (indexes, commandFactory, sleep, maxRetries).
   */
  constructor(client, tableName, options = {}) {
    this.#store = new DynamoRdfStore(client, tableName, options);
  }

  /**
   * Name of the DynamoDB table backing this adapter.
   *
   * @returns {string} Table name.
   */
  get tableName() {
    return this.#store.tableName;
  }

  /**
   * Validates and stores one triple.
   *
   * @param {{subject: string, predicate: string, object: string, graph?: string}} triple - Triple to store.
   * @param {{ifAbsent?: boolean}} [options={}] - With `ifAbsent`, the write is conditional on the key not existing.
   * @returns {Promise<void>}
   */
  async addTriple(triple, options = {}) {
    await this.#store.addTriple(TripleSchema.parse(triple), options);
  }

  /**
   * Validates and stores many triples using batch writes.
   *
   * @param {Iterable<Object>} triples - Triples to store.
   * @param {number|Object} [batchSizeOrOptions=25] - Batch size, or an options object (`batchSize`, `detailed`).
   * @returns {Promise<number|{written: number, retries: number}>} Count written, or `{written, retries}` when `detailed` is set.
   */
  async addTriples(triples, batchSizeOrOptions = 25) {
    const options =
      typeof batchSizeOrOptions === 'number'
        ? { batchSize: batchSizeOrOptions }
        : batchSizeOrOptions;
    const values = Array.from(triples || [], triple => TripleSchema.parse(triple));
    const result = await this.#store.addTriples(values, options);
    return options?.detailed ? result : result.written;
  }

  /**
   * Returns triples matching a pattern, up to a limit.
   *
   * @param {Object} [pattern={}] - Optional subject/predicate/object/graph filters.
   * @param {number} [limit=100] - Maximum number of triples to return.
   * @returns {Promise<Object[]>} Matching triples.
   */
  async queryTriples(pattern = {}, limit = 100) {
    return this.#store.queryTriples(TriplePatternSchema.parse(pattern), limit);
  }

  /**
   * Returns one page of matching triples with a continuation token.
   *
   * @param {Object} [pattern={}] - Optional subject/predicate/object/graph filters.
   * @param {{limit?: number, token?: string}} [options={}] - Page size and continuation token from a prior page.
   * @returns {Promise<{triples: Object[], token: string|null, scannedCount: number, count: number, operation: string, indexName: string|null}>} The page and query metadata.
   */
  async queryPage(pattern = {}, options = {}) {
    return this.#store.queryPage(TriplePatternSchema.parse(pattern), options);
  }

  /**
   * Lazily iterates over all matching triples, fetching pages as needed.
   *
   * @param {Object} [pattern={}] - Optional subject/predicate/object/graph filters.
   * @param {{pageSize?: number, limit?: number, token?: string}} [options={}] - Page size, total limit, and starting token.
   * @returns {AsyncGenerator<Object>} Async generator of triples.
   */
  iterateTriples(pattern = {}, options = {}) {
    return this.#store.iterateTriples(TriplePatternSchema.parse(pattern), options);
  }

  /**
   * Deletes one triple.
   *
   * @param {{subject: string, predicate: string, object: string}} triple - Triple to delete.
   * @returns {Promise<boolean>} True if an item was deleted.
   */
  async deleteTriple(triple) {
    return this.#store.deleteTriple(TripleSchema.parse(triple));
  }

  /**
   * Deletes many triples using batch writes.
   *
   * @param {Iterable<Object>} triples - Triples to delete.
   * @param {{batchSize?: number}} [options={}] - Batch options.
   * @returns {Promise<{deleted: number, retries: number}>} Deleted count and retry count.
   */
  async deleteTriples(triples, options = {}) {
    const values = Array.from(triples || [], triple => TripleSchema.parse(triple));
    return this.#store.deleteTriples(values, options);
  }

  /**
   * Deletes every triple matching a pattern.
   *
   * @param {Object} [pattern={}] - Optional subject/predicate/object/graph filters.
   * @param {{pageSize?: number, batchSize?: number}} [options={}] - Paging and batch options.
   * @returns {Promise<number>} Number of triples deleted.
   */
  async deletePattern(pattern = {}, options = {}) {
    return this.#store.deletePattern(TriplePatternSchema.parse(pattern), options);
  }

  /**
   * Counts triples matching a pattern.
   *
   * @param {Object} [pattern={}] - Optional subject/predicate/object/graph filters.
   * @returns {Promise<number>} Number of matching triples.
   */
  async countTriples(pattern = {}) {
    return this.#store.countTriples(TriplePatternSchema.parse(pattern));
  }

  /**
   * Deletes all triples in a named graph.
   *
   * @param {string} graph - Graph IRI.
   * @param {Object} [options={}] - Options passed to `deletePattern`.
   * @returns {Promise<number>} Number of triples deleted.
   * @throws {TypeError} If `graph` is not a non-empty string.
   */
  async clearGraph(graph, options = {}) {
    return this.#store.clearGraph(graph, options);
  }

  /**
   * Computes cardinality statistics over matching triples.
   *
   * @param {Object} [pattern={}] - Optional subject/predicate/object/graph filters.
   * @returns {Promise<{count: number, distinctSubjects: number, distinctObjects: number, byPredicate: Object, byGraph: Object}>} Totals, distinct counts, and per-predicate and per-graph counts.
   */
  async statistics(pattern = {}) {
    return this.#store.statistics(TriplePatternSchema.parse(pattern));
  }
}

/**
 * Creates a production adapter using the optional AWS SDK peer dependency.
 * A client may be supplied for Lambda reuse, tests, local emulators, or custom
 * credentials. The function remains synchronous so callers can initialize it at
 * module scope and retain the client across warm invocations.
 */
export function createAdapterFromEnv(options = {}) {
  const tableName = options.tableName || process.env.TRIPLES_TABLE;
  if (!tableName) throw new Error('TRIPLES_TABLE environment variable not set');
  if (options.client) {
    return new DynamoDBAdapter(options.client, tableName, {
      ...options,
      commandFactory: options.commandFactory || createPlainCommandFactory(),
    });
  }

  let sdk;
  try {
    const require = createRequire(import.meta.url);
    sdk = require('@aws-sdk/client-dynamodb');
  } catch (error) {
    throw new Error('@aws-sdk/client-dynamodb is required when no DynamoDB client is supplied', {
      cause: error,
    });
  }
  const client = new sdk.DynamoDBClient({
    region: options.region || process.env.AWS_REGION || process.env.AWS_DEFAULT_REGION,
    endpoint: options.endpoint || process.env.DYNAMODB_ENDPOINT,
    ...(options.clientConfig || {}),
  });
  return new DynamoDBAdapter(client, tableName, {
    ...options,
    commandFactory: createAwsCommandFactory(sdk),
  });
}

export {
  DynamoRdfStore,
  createAwsCommandFactory,
  createPlainCommandFactory,
  encodeTriple,
  decodeTriple,
  encodeToken,
  decodeToken,
};
