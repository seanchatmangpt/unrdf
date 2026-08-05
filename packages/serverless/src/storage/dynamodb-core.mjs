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

export function assertTriple(triple) {
  if (!triple || typeof triple !== 'object') throw new TypeError('Triple must be an object');
  for (const field of ['subject', 'predicate', 'object']) {
    if (typeof triple[field] !== 'string' || triple[field].length === 0) throw new TypeError(`Triple ${field} must be a non-empty string`);
  }
  if (triple.graph != null && typeof triple.graph !== 'string') throw new TypeError('Triple graph must be a string');
  return { subject: triple.subject, predicate: triple.predicate, object: triple.object, ...(triple.graph ? { graph: triple.graph } : {}) };
}

function s(value) { return { S: value }; }

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

export function decodeTriple(item) {
  if (!item?.subject?.S || !item?.predicate?.S || !item?.object?.S) throw new Error('Malformed DynamoDB triple item');
  return {
    subject: item.subject.S,
    predicate: item.predicate.S,
    object: item.object.S,
    ...(item.graph?.S ? { graph: item.graph.S } : {}),
  };
}

export function encodeToken(lastEvaluatedKey) {
  if (!lastEvaluatedKey) return null;
  return Buffer.from(JSON.stringify(lastEvaluatedKey), 'utf8').toString('base64url');
}

export function decodeToken(token) {
  if (!token) return undefined;
  try { return JSON.parse(Buffer.from(token, 'base64url').toString('utf8')); }
  catch (error) { throw new TypeError(`Invalid DynamoDB continuation token: ${error.message}`); }
}

export function createPlainCommandFactory() {
  return (operation, input) => ({ operation, input });
}

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

export class DynamoRdfStore {
  #client;
  #tableName;
  #indexes;
  #commandFactory;
  #sleep;
  #maxRetries;

  constructor(client, tableName, options = {}) {
    if (!client || typeof client.send !== 'function') throw new TypeError('DynamoDB client must implement send(command)');
    if (typeof tableName !== 'string' || !tableName) throw new TypeError('DynamoDB table name is required');
    this.#client = client;
    this.#tableName = tableName;
    this.#indexes = { ...DEFAULT_INDEXES, ...(options.indexes || {}) };
    this.#commandFactory = options.commandFactory || createPlainCommandFactory();
    this.#sleep = options.sleep || (ms => new Promise(resolve => setTimeout(resolve, ms)));
    this.#maxRetries = options.maxRetries ?? 8;
  }

  get tableName() { return this.#tableName; }

  async #send(operation, input) {
    return this.#client.send(this.#commandFactory(operation, input));
  }

  async addTriple(triple, options = {}) {
    const input = {
      TableName: this.#tableName,
      Item: encodeTriple(triple),
      ...(options.ifAbsent ? { ConditionExpression: 'attribute_not_exists(#subject) AND attribute_not_exists(#predicateObject)', ExpressionAttributeNames: { '#subject': 'subject', '#predicateObject': 'predicate_object' } } : {}),
    };
    await this.#send('PutItem', input);
  }

  async addTriples(triples, options = {}) {
    const values = Array.from(triples || [], assertTriple);
    const batchSize = Math.min(MAX_BATCH_WRITE, Math.max(1, options.batchSize ?? MAX_BATCH_WRITE));
    let written = 0;
    let retries = 0;
    for (let offset = 0; offset < values.length; offset += batchSize) {
      let pending = values.slice(offset, offset + batchSize).map(triple => ({ PutRequest: { Item: encodeTriple(triple) } }));
      let attempt = 0;
      while (pending.length) {
        const output = await this.#send('BatchWriteItem', { RequestItems: { [this.#tableName]: pending } });
        const unprocessed = output?.UnprocessedItems?.[this.#tableName] || [];
        written += pending.length - unprocessed.length;
        pending = unprocessed;
        if (!pending.length) break;
        if (attempt >= this.#maxRetries) throw new Error(`DynamoDB left ${pending.length} unprocessed writes after ${attempt + 1} attempts`);
        const delay = Math.min(1000, 25 * 2 ** attempt);
        await this.#sleep(delay);
        attempt += 1;
        retries += 1;
      }
    }
    return { written, retries };
  }

  async queryPage(pattern = {}, options = {}) {
    const limit = Math.max(1, options.limit ?? DEFAULT_LIMIT);
    const startKey = decodeToken(options.token);
    const { operation, input } = planPattern(this.#tableName, this.#indexes, pattern, limit, startKey);
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

  async queryTriples(pattern = {}, limit = DEFAULT_LIMIT) {
    if (!Number.isFinite(limit) || limit <= 0) throw new TypeError('Query limit must be a positive finite number');
    const triples = [];
    let token = null;
    do {
      const page = await this.queryPage(pattern, { limit: Math.min(1000, limit - triples.length), token });
      triples.push(...page.triples);
      token = page.token;
    } while (token && triples.length < limit);
    return triples.slice(0, limit);
  }

  async *iterateTriples(pattern = {}, options = {}) {
    const pageSize = Math.max(1, options.pageSize ?? DEFAULT_LIMIT);
    const limit = options.limit ?? Number.POSITIVE_INFINITY;
    let yielded = 0;
    let token = options.token || null;
    do {
      const page = await this.queryPage(pattern, { limit: Math.min(pageSize, limit - yielded), token });
      for (const triple of page.triples) {
        if (yielded >= limit) return;
        yield triple;
        yielded += 1;
      }
      token = page.token;
    } while (token && yielded < limit);
  }

  async deleteTriple(triple) {
    const value = assertTriple(triple);
    const output = await this.#send('DeleteItem', {
      TableName: this.#tableName,
      Key: { subject: s(value.subject), predicate_object: s(`${value.predicate}#${value.object}`) },
      ReturnValues: 'ALL_OLD',
    });
    return Boolean(output?.Attributes);
  }

  async deleteTriples(triples, options = {}) {
    const values = Array.from(triples || [], assertTriple);
    const batchSize = Math.min(MAX_BATCH_WRITE, Math.max(1, options.batchSize ?? MAX_BATCH_WRITE));
    let deleted = 0;
    let retries = 0;
    for (let offset = 0; offset < values.length; offset += batchSize) {
      let pending = values.slice(offset, offset + batchSize).map(triple => ({ DeleteRequest: { Key: { subject: s(triple.subject), predicate_object: s(`${triple.predicate}#${triple.object}`) } } }));
      let attempt = 0;
      while (pending.length) {
        const output = await this.#send('BatchWriteItem', { RequestItems: { [this.#tableName]: pending } });
        const unprocessed = output?.UnprocessedItems?.[this.#tableName] || [];
        deleted += pending.length - unprocessed.length;
        pending = unprocessed;
        if (!pending.length) break;
        if (attempt >= this.#maxRetries) throw new Error(`DynamoDB left ${pending.length} unprocessed deletes after ${attempt + 1} attempts`);
        await this.#sleep(Math.min(1000, 25 * 2 ** attempt));
        attempt += 1;
        retries += 1;
      }
    }
    return { deleted, retries };
  }

  async deletePattern(pattern = {}, options = {}) {
    let deleted = 0;
    const buffer = [];
    for await (const triple of this.iterateTriples(pattern, { pageSize: options.pageSize ?? 250 })) {
      buffer.push(triple);
      if (buffer.length === MAX_BATCH_WRITE) {
        deleted += (await this.deleteTriples(buffer.splice(0), options)).deleted;
      }
    }
    if (buffer.length) deleted += (await this.deleteTriples(buffer, options)).deleted;
    return deleted;
  }

  async countTriples(pattern = {}) {
    let count = 0;
    for await (const _triple of this.iterateTriples(pattern, { pageSize: 1000 })) count += 1;
    return count;
  }

  async clearGraph(graph, options = {}) {
    if (typeof graph !== 'string' || !graph) throw new TypeError('Graph IRI is required');
    return this.deletePattern({ graph }, options);
  }

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
