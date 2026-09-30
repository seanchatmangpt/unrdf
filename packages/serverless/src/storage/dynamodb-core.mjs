/**
 * DynamoDB RDF storage core with no AWS SDK dependency.
 *
 * The client contract is `{ send(command): Promise<output> }`; commandFactory
 * maps logical operation names to SDK command instances in production and plain
 * `{ operation, input }` values in tests.
 */

const DEFAULT_INDEXES = Object.freeze({ predicate: 'predicate-index', object: 'object-index' });
const DEFAULT_LIMIT = 100;
const MAX_BATCH_WRITE = 25;

/**
 * Validates a triple and returns a normalized copy.
 *
 * @param {Object} triple - Candidate triple.
 * @returns {{subject: string, predicate: string, object: string, graph?: string}} Normalized triple; `graph` is included only when non-empty.
 * @throws {TypeError} If the triple or any field has the wrong type or is empty.
 */
export function assertTriple(triple) {
  if (!triple || typeof triple !== 'object') throw new TypeError('Triple must be an object');
  for (const field of ['subject', 'predicate', 'object']) {
    if (typeof triple[field] !== 'string' || triple[field].length === 0)
      throw new TypeError(`Triple ${field} must be a non-empty string`);
  }
  if (triple.graph != null && typeof triple.graph !== 'string')
    throw new TypeError('Triple graph must be a string');
  return {
    subject: triple.subject,
    predicate: triple.predicate,
    object: triple.object,
    ...(triple.graph ? { graph: triple.graph } : {}),
  };
}

function s(value) {
  return { S: value };
}

/**
 * Encodes a triple as a DynamoDB item with composite sort-key attributes.
 *
 * @param {Object} triple - Triple to encode.
 * @returns {Object} DynamoDB attribute map (subject, predicate, object, composite keys, optional graph).
 * @throws {TypeError} If the triple is invalid.
 */
export function encodeTriple(triple) {
  const value = assertTriple(triple);
  return {
    subject: s(value.subject),
    predicate_object: s(`${value.predicate}#${value.object}`),
    predicate: s(value.predicate),
    object: s(value.object),
    subject_object: s(`${value.subject}#${value.object}`),
    subject_predicate: s(`${value.subject}#${value.predicate}`),
    ...(value.graph ? { graph: s(value.graph) } : {}),
  };
}

/**
 * Decodes a DynamoDB item into a triple.
 *
 * @param {Object} item - DynamoDB attribute map.
 * @returns {{subject: string, predicate: string, object: string, graph?: string}} Decoded triple.
 * @throws {Error} If subject, predicate or object is missing.
 */
export function decodeTriple(item) {
  if (!item?.subject?.S || !item?.predicate?.S || !item?.object?.S)
    throw new Error('Malformed DynamoDB triple item');
  return {
    subject: item.subject.S,
    predicate: item.predicate.S,
    object: item.object.S,
    ...(item.graph?.S ? { graph: item.graph.S } : {}),
  };
}

/**
 * Encodes a DynamoDB LastEvaluatedKey as an opaque base64url continuation token.
 *
 * @param {Object} [lastEvaluatedKey] - Key returned by DynamoDB, if any.
 * @returns {string|null} Token, or null when there is no further page.
 */
export function encodeToken(lastEvaluatedKey) {
  if (!lastEvaluatedKey) return null;
  return Buffer.from(JSON.stringify(lastEvaluatedKey), 'utf8').toString('base64url');
}

/**
 * Decodes a continuation token produced by `encodeToken`.
 *
 * @param {string} [token] - Continuation token.
 * @returns {Object|undefined} ExclusiveStartKey, or undefined for an empty token.
 * @throws {TypeError} If the token cannot be decoded.
 */
export function decodeToken(token) {
  if (!token) return undefined;
  try {
    return JSON.parse(Buffer.from(token, 'base64url').toString('utf8'));
  } catch (error) {
    throw new TypeError(`Invalid DynamoDB continuation token: ${error.message}`);
  }
}

/**
 * Creates a command factory that returns plain `{operation, input}` objects (for tests and emulators).
 *
 * @returns {(operation: string, input: Object) => {operation: string, input: Object}} Command factory.
 */
export function createPlainCommandFactory() {
  return (operation, input) => ({ operation, input });
}

/**
 * Creates a command factory that instantiates AWS SDK command classes.
 *
 * @param {Object} sdk - The `@aws-sdk/client-dynamodb` module.
 * @returns {(operation: string, input: Object) => Object} Factory that throws for unsupported operations.
 */
export function createAwsCommandFactory(sdk) {
  const commands = {
    PutItem: sdk.PutItemCommand,
    DeleteItem: sdk.DeleteItemCommand,
    Query: sdk.QueryCommand,
    Scan: sdk.ScanCommand,
    BatchWriteItem: sdk.BatchWriteItemCommand,
  };
  return (operation, input) => {
    const Command = commands[operation];
    if (!Command) throw new Error(`Unsupported DynamoDB command ${operation}`);
    return new Command(input);
  };
}

function expressionBuilder() {
  const names = {};
  const values = {};
  const filters = [];
  let index = 0;
  return {
    equal(attribute, value) {
      const id = index++;
      const name = `#n${id}`;
      const token = `:v${id}`;
      names[name] = attribute;
      values[token] = s(value);
      filters.push(`${name} = ${token}`);
    },
    apply(input) {
      if (!filters.length) return input;
      return {
        ...input,
        FilterExpression: filters.join(' AND '),
        ExpressionAttributeNames: { ...(input.ExpressionAttributeNames || {}), ...names },
        ExpressionAttributeValues: { ...(input.ExpressionAttributeValues || {}), ...values },
      };
    },
  };
}

/**
 * Chooses the DynamoDB operation, index and filters for a triple pattern.
 *
 * @param {string} tableName - Table name.
 * @param {{predicate: string, object: string}} indexes - Global secondary index names.
 * @param {Object} pattern - Subject/predicate/object/graph filters.
 * @param {number} pageLimit - Maximum items per page.
 * @param {Object} [startKey] - ExclusiveStartKey for continuation.
 * @returns {{operation: 'Query'|'Scan', input: Object}} Operation name and command input.
 */
function planPattern(tableName, indexes, pattern, pageLimit, startKey) {
  const { subject, predicate, object, graph } = pattern;
  let operation;
  let input;
  const filter = expressionBuilder();

  if (subject) {
    operation = 'Query';
    input = {
      TableName: tableName,
      KeyConditionExpression: '#subject = :subject',
      ExpressionAttributeNames: { '#subject': 'subject' },
      ExpressionAttributeValues: { ':subject': s(subject) },
    };
    if (predicate && object) {
      input.KeyConditionExpression += ' AND #predicateObject = :predicateObject';
      input.ExpressionAttributeNames['#predicateObject'] = 'predicate_object';
      input.ExpressionAttributeValues[':predicateObject'] = s(`${predicate}#${object}`);
    } else if (predicate) {
      input.KeyConditionExpression += ' AND begins_with(#predicateObject, :predicatePrefix)';
      input.ExpressionAttributeNames['#predicateObject'] = 'predicate_object';
      input.ExpressionAttributeValues[':predicatePrefix'] = s(`${predicate}#`);
    } else if (object) {
      filter.equal('object', object);
    }
  } else if (predicate) {
    operation = 'Query';
    input = {
      TableName: tableName,
      IndexName: indexes.predicate,
      KeyConditionExpression: '#predicate = :predicate',
      ExpressionAttributeNames: { '#predicate': 'predicate' },
      ExpressionAttributeValues: { ':predicate': s(predicate) },
    };
    if (object) filter.equal('object', object);
  } else if (object) {
    operation = 'Query';
    input = {
      TableName: tableName,
      IndexName: indexes.object,
      KeyConditionExpression: '#object = :object',
      ExpressionAttributeNames: { '#object': 'object' },
      ExpressionAttributeValues: { ':object': s(object) },
    };
  } else {
    operation = 'Scan';
    input = { TableName: tableName };
  }

  if (graph != null) filter.equal('graph', graph);
  input = filter.apply(input);
  input.Limit = pageLimit;
  if (startKey) input.ExclusiveStartKey = startKey;
  return { operation, input };
}

/**
 * DynamoDB-backed RDF triple store with subject, predicate and object access paths; has no AWS SDK dependency.
 */
export class DynamoRdfStore {
  #client;
  #tableName;
  #indexes;
  #commandFactory;
  #sleep;
  #maxRetries;

  /**
   * Creates a store.
   *
   * @param {{send: Function}} client - Client exposing `send(command)`.
   * @param {string} tableName - Triples table name.
   * @param {Object} [options={}] - Options.
   * @param {Object} [options.indexes] - Overrides for the predicate/object index names.
   * @param {Function} [options.commandFactory] - Maps operation name and input to a command.
   * @param {(ms: number) => Promise<void>} [options.sleep] - Delay function used between retries.
   * @param {number} [options.maxRetries=8] - Retries for unprocessed batch items.
   * @throws {TypeError} If the client or table name is invalid.
   */
  constructor(client, tableName, options = {}) {
    if (!client || typeof client.send !== 'function')
      throw new TypeError('DynamoDB client must implement send(command)');
    if (typeof tableName !== 'string' || !tableName)
      throw new TypeError('DynamoDB table name is required');
    this.#client = client;
    this.#tableName = tableName;
    this.#indexes = { ...DEFAULT_INDEXES, ...(options.indexes || {}) };
    this.#commandFactory = options.commandFactory || createPlainCommandFactory();
    this.#sleep = options.sleep || (ms => new Promise(resolve => setTimeout(resolve, ms)));
    this.#maxRetries = options.maxRetries ?? 8;
  }

  /**
   * Name of the backing table.
   *
   * @returns {string} Table name.
   */
  get tableName() {
    return this.#tableName;
  }

  async #send(operation, input) {
    return this.#client.send(this.#commandFactory(operation, input));
  }

  /**
   * Stores one triple.
   *
   * @param {Object} triple - Triple to store.
   * @param {{ifAbsent?: boolean}} [options={}] - With `ifAbsent`, fail if the subject and predicate/object key already exist.
   * @returns {Promise<void>}
   */
  async addTriple(triple, options = {}) {
    const input = {
      TableName: this.#tableName,
      Item: encodeTriple(triple),
      ...(options.ifAbsent
        ? {
            ConditionExpression:
              'attribute_not_exists(#subject) AND attribute_not_exists(#predicateObject)',
            ExpressionAttributeNames: {
              '#subject': 'subject',
              '#predicateObject': 'predicate_object',
            },
          }
        : {}),
    };
    await this.#send('PutItem', input);
  }

  /**
   * Stores triples in batches, retrying unprocessed items with exponential backoff.
   *
   * @param {Iterable<Object>} triples - Triples to store.
   * @param {{batchSize?: number}} [options={}] - Batch size, capped at 25.
   * @returns {Promise<{written: number, retries: number}>} Items written and retries performed.
   * @throws {Error} If items stay unprocessed after `maxRetries`.
   */
  async addTriples(triples, options = {}) {
    const values = Array.from(triples || [], assertTriple);
    const batchSize = Math.min(MAX_BATCH_WRITE, Math.max(1, options.batchSize ?? MAX_BATCH_WRITE));
    let written = 0;
    let retries = 0;
    for (let offset = 0; offset < values.length; offset += batchSize) {
      let pending = values
        .slice(offset, offset + batchSize)
        .map(triple => ({ PutRequest: { Item: encodeTriple(triple) } }));
      let attempt = 0;
      while (pending.length) {
        const output = await this.#send('BatchWriteItem', {
          RequestItems: { [this.#tableName]: pending },
        });
        const unprocessed = output?.UnprocessedItems?.[this.#tableName] || [];
        written += pending.length - unprocessed.length;
        pending = unprocessed;
        if (!pending.length) break;
        if (attempt >= this.#maxRetries)
          throw new Error(
            `DynamoDB left ${pending.length} unprocessed writes after ${attempt + 1} attempts`
          );
        const delay = Math.min(1000, 25 * 2 ** attempt);
        await this.#sleep(delay);
        attempt += 1;
        retries += 1;
      }
    }
    return { written, retries };
  }

  /**
   * Fetches one page of triples matching a pattern.
   *
   * @param {Object} [pattern={}] - Subject/predicate/object/graph filters.
   * @param {{limit?: number, token?: string}} [options={}] - Page size and continuation token.
   * @returns {Promise<{triples: Object[], token: string|null, scannedCount: number, count: number, operation: string, indexName: string|null}>} Page and query metadata.
   */
  async queryPage(pattern = {}, options = {}) {
    const limit = Math.max(1, options.limit ?? DEFAULT_LIMIT);
    const startKey = decodeToken(options.token);
    const { operation, input } = planPattern(
      this.#tableName,
      this.#indexes,
      pattern,
      limit,
      startKey
    );
    const output = await this.#send(operation, input);
    return {
      triples: (output?.Items || []).map(decodeTriple),
      token: encodeToken(output?.LastEvaluatedKey),
      scannedCount: output?.ScannedCount ?? output?.Count ?? 0,
      count: output?.Count ?? output?.Items?.length ?? 0,
      operation,
      indexName: input.IndexName || null,
    };
  }

  /**
   * Collects up to `limit` triples matching a pattern across pages.
   *
   * @param {Object} [pattern={}] - Subject/predicate/object/graph filters.
   * @param {number} [limit=100] - Maximum triples to return.
   * @returns {Promise<Object[]>} Matching triples.
   * @throws {TypeError} If `limit` is not a positive finite number.
   */
  async queryTriples(pattern = {}, limit = DEFAULT_LIMIT) {
    if (!Number.isFinite(limit) || limit <= 0)
      throw new TypeError('Query limit must be a positive finite number');
    const triples = [];
    let token = null;
    do {
      const page = await this.queryPage(pattern, {
        limit: Math.min(1000, limit - triples.length),
        token,
      });
      triples.push(...page.triples);
      token = page.token;
    } while (token && triples.length < limit);
    return triples.slice(0, limit);
  }

  /**
   * Lazily yields matching triples, fetching pages as needed.
   *
   * @param {Object} [pattern={}] - Subject/predicate/object/graph filters.
   * @param {{pageSize?: number, limit?: number, token?: string}} [options={}] - Page size, total limit, and starting token.
   * @returns {AsyncGenerator<Object>} Async generator of triples.
   */
  async *iterateTriples(pattern = {}, options = {}) {
    const pageSize = Math.max(1, options.pageSize ?? DEFAULT_LIMIT);
    const limit = options.limit ?? Number.POSITIVE_INFINITY;
    let yielded = 0;
    let token = options.token || null;
    do {
      const page = await this.queryPage(pattern, {
        limit: Math.min(pageSize, limit - yielded),
        token,
      });
      for (const triple of page.triples) {
        if (yielded >= limit) return;
        yield triple;
        yielded += 1;
      }
      token = page.token;
    } while (token && yielded < limit);
  }

  /**
   * Deletes one triple.
   *
   * @param {Object} triple - Triple to delete.
   * @returns {Promise<boolean>} True if an item existed and was deleted.
   */
  async deleteTriple(triple) {
    const value = assertTriple(triple);
    const output = await this.#send('DeleteItem', {
      TableName: this.#tableName,
      Key: { subject: s(value.subject), predicate_object: s(`${value.predicate}#${value.object}`) },
      ReturnValues: 'ALL_OLD',
    });
    return Boolean(output?.Attributes);
  }

  /**
   * Deletes triples in batches, retrying unprocessed items with exponential backoff.
   *
   * @param {Iterable<Object>} triples - Triples to delete.
   * @param {{batchSize?: number}} [options={}] - Batch size, capped at 25.
   * @returns {Promise<{deleted: number, retries: number}>} Items deleted and retries performed.
   * @throws {Error} If items stay unprocessed after `maxRetries`.
   */
  async deleteTriples(triples, options = {}) {
    const values = Array.from(triples || [], assertTriple);
    const batchSize = Math.min(MAX_BATCH_WRITE, Math.max(1, options.batchSize ?? MAX_BATCH_WRITE));
    let deleted = 0;
    let retries = 0;
    for (let offset = 0; offset < values.length; offset += batchSize) {
      let pending = values.slice(offset, offset + batchSize).map(triple => ({
        DeleteRequest: {
          Key: {
            subject: s(triple.subject),
            predicate_object: s(`${triple.predicate}#${triple.object}`),
          },
        },
      }));
      let attempt = 0;
      while (pending.length) {
        const output = await this.#send('BatchWriteItem', {
          RequestItems: { [this.#tableName]: pending },
        });
        const unprocessed = output?.UnprocessedItems?.[this.#tableName] || [];
        deleted += pending.length - unprocessed.length;
        pending = unprocessed;
        if (!pending.length) break;
        if (attempt >= this.#maxRetries)
          throw new Error(
            `DynamoDB left ${pending.length} unprocessed deletes after ${attempt + 1} attempts`
          );
        await this.#sleep(Math.min(1000, 25 * 2 ** attempt));
        attempt += 1;
        retries += 1;
      }
    }
    return { deleted, retries };
  }

  /**
   * Deletes all triples matching a pattern.
   *
   * @param {Object} [pattern={}] - Subject/predicate/object/graph filters.
   * @param {{pageSize?: number, batchSize?: number}} [options={}] - Paging and batch options.
   * @returns {Promise<number>} Number of triples deleted.
   */
  async deletePattern(pattern = {}, options = {}) {
    let deleted = 0;
    const buffer = [];
    for await (const triple of this.iterateTriples(pattern, {
      pageSize: options.pageSize ?? 250,
    })) {
      buffer.push(triple);
      if (buffer.length === MAX_BATCH_WRITE) {
        deleted += (await this.deleteTriples(buffer.splice(0), options)).deleted;
      }
    }
    if (buffer.length) deleted += (await this.deleteTriples(buffer, options)).deleted;
    return deleted;
  }

  /**
   * Counts triples matching a pattern.
   *
   * @param {Object} [pattern={}] - Subject/predicate/object/graph filters.
   * @returns {Promise<number>} Number of matches.
   */
  async countTriples(pattern = {}) {
    let count = 0;
    for await (const _triple of this.iterateTriples(pattern, { pageSize: 1000 })) count += 1;
    return count;
  }

  /**
   * Deletes all triples in a graph.
   *
   * @param {string} graph - Graph IRI.
   * @param {Object} [options={}] - Options passed to `deletePattern`.
   * @returns {Promise<number>} Number of triples deleted.
   * @throws {TypeError} If `graph` is not a non-empty string.
   */
  async clearGraph(graph, options = {}) {
    if (typeof graph !== 'string' || !graph) throw new TypeError('Graph IRI is required');
    return this.deletePattern({ graph }, options);
  }

  /**
   * Computes statistics over matching triples.
   *
   * @param {Object} [pattern={}] - Subject/predicate/object/graph filters.
   * @returns {Promise<{count: number, distinctSubjects: number, distinctObjects: number, byPredicate: Object, byGraph: Object}>} Totals, distinct counts, and per-predicate and per-graph counts (sorted by key).
   */
  async statistics(pattern = {}) {
    const byPredicate = new Map();
    const byGraph = new Map();
    const subjects = new Set();
    const objects = new Set();
    let count = 0;
    for await (const triple of this.iterateTriples(pattern, { pageSize: 1000 })) {
      count += 1;
      subjects.add(triple.subject);
      objects.add(triple.object);
      byPredicate.set(triple.predicate, (byPredicate.get(triple.predicate) || 0) + 1);
      const graph = triple.graph || '';
      byGraph.set(graph, (byGraph.get(graph) || 0) + 1);
    }
    return {
      count,
      distinctSubjects: subjects.size,
      distinctObjects: objects.size,
      byPredicate: Object.fromEntries([...byPredicate].sort()),
      byGraph: Object.fromEntries([...byGraph].sort()),
    };
  }
}

export { planPattern, DEFAULT_INDEXES, MAX_BATCH_WRITE };
