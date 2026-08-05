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

export class DynamoDBAdapter {
  #store;

  constructor(client, tableName, options = {}) {
    this.#store = new DynamoRdfStore(client, tableName, options);
  }

  get tableName() { return this.#store.tableName; }

  async addTriple(triple, options = {}) {
    await this.#store.addTriple(TripleSchema.parse(triple), options);
  }

  async addTriples(triples, batchSizeOrOptions = 25) {
    const options = typeof batchSizeOrOptions === 'number' ? { batchSize: batchSizeOrOptions } : batchSizeOrOptions;
    const values = Array.from(triples || [], triple => TripleSchema.parse(triple));
    const result = await this.#store.addTriples(values, options);
    return options?.detailed ? result : result.written;
  }

  async queryTriples(pattern = {}, limit = 100) {
    return this.#store.queryTriples(TriplePatternSchema.parse(pattern), limit);
  }

  async queryPage(pattern = {}, options = {}) {
    return this.#store.queryPage(TriplePatternSchema.parse(pattern), options);
  }

  iterateTriples(pattern = {}, options = {}) {
    return this.#store.iterateTriples(TriplePatternSchema.parse(pattern), options);
  }

  async deleteTriple(triple) {
    return this.#store.deleteTriple(TripleSchema.parse(triple));
  }

  async deleteTriples(triples, options = {}) {
    const values = Array.from(triples || [], triple => TripleSchema.parse(triple));
    return this.#store.deleteTriples(values, options);
  }

  async deletePattern(pattern = {}, options = {}) {
    return this.#store.deletePattern(TriplePatternSchema.parse(pattern), options);
  }

  async countTriples(pattern = {}) {
    return this.#store.countTriples(TriplePatternSchema.parse(pattern));
  }

  async clearGraph(graph, options = {}) {
    return this.#store.clearGraph(graph, options);
  }

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
    throw new Error('@aws-sdk/client-dynamodb is required when no DynamoDB client is supplied', { cause: error });
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
