import test from 'node:test';
import assert from 'node:assert/strict';
import {
  DynamoRdfStore,
  encodeTriple,
  decodeTriple,
  encodeToken,
  decodeToken,
} from '../src/storage/dynamodb-core.mjs';

class Client {
  constructor(handler) { this.handler = handler; this.commands = []; }
  async send(command) { this.commands.push(command); return this.handler(command, this.commands.length - 1); }
}

const triple = (suffix = '1', graph) => ({ subject: `s${suffix}`, predicate: `p${suffix}`, object: `o${suffix}`, ...(graph ? { graph } : {}) });

test('triple codec round-trips graph and index attributes', () => {
  const value = triple('1', 'g');
  const item = encodeTriple(value);
  assert.equal(item.predicate_object.S, 'p1#o1');
  assert.equal(item.subject_object.S, 's1#o1');
  assert.equal(item.subject_predicate.S, 's1#p1');
  assert.deepEqual(decodeTriple(item), value);
});

test('continuation tokens round-trip and reject malformed input', () => {
  const key = { subject: { S: 's' }, predicate_object: { S: 'p#o' } };
  assert.deepEqual(decodeToken(encodeToken(key)), key);
  assert.throws(() => decodeToken('%%%'), /Invalid DynamoDB continuation token/);
});

test('query planner uses table key for subject and predicate prefix', async () => {
  const client = new Client(command => {
    assert.equal(command.operation, 'Query');
    assert.match(command.input.KeyConditionExpression, /begins_with/);
    assert.equal(command.input.ExpressionAttributeValues[':predicatePrefix'].S, 'p#');
    return { Items: [encodeTriple({ subject: 's', predicate: 'p', object: 'o' })], Count: 1 };
  });
  const store = new DynamoRdfStore(client, 'triples');
  assert.deepEqual(await store.queryTriples({ subject: 's', predicate: 'p' }), [{ subject: 's', predicate: 'p', object: 'o' }]);
});

test('predicate and object queries route through their GSIs', async () => {
  const operations = [];
  const client = new Client(command => {
    operations.push(command.input.IndexName);
    return { Items: [] };
  });
  const store = new DynamoRdfStore(client, 'triples');
  await store.queryTriples({ predicate: 'p' });
  await store.queryTriples({ object: 'o' });
  assert.deepEqual(operations, ['predicate-index', 'object-index']);
});

test('graph and residual object filters are compiled into expressions', async () => {
  const client = new Client(command => {
    assert.match(command.input.FilterExpression, /AND/);
    const values = Object.values(command.input.ExpressionAttributeValues).map(value => value.S);
    assert.ok(values.includes('o'));
    assert.ok(values.includes('g'));
    return { Items: [] };
  });
  const store = new DynamoRdfStore(client, 'triples');
  await store.queryTriples({ subject: 's', object: 'o', graph: 'g' });
});

test('queryPage exposes stable continuation token metadata', async () => {
  const lastKey = { subject: { S: 's2' }, predicate_object: { S: 'p#o' } };
  const client = new Client(() => ({ Items: [encodeTriple(triple('1'))], Count: 1, ScannedCount: 2, LastEvaluatedKey: lastKey }));
  const store = new DynamoRdfStore(client, 'triples');
  const page = await store.queryPage({}, { limit: 1 });
  assert.equal(page.operation, 'Scan');
  assert.equal(page.count, 1);
  assert.deepEqual(decodeToken(page.token), lastKey);
});

test('queryTriples follows pagination and honors total limit', async () => {
  let page = 0;
  const client = new Client(() => {
    page += 1;
    return page === 1
      ? { Items: [encodeTriple(triple('1'))], LastEvaluatedKey: { subject: { S: 'next' } } }
      : { Items: [encodeTriple(triple('2')), encodeTriple(triple('3'))] };
  });
  const store = new DynamoRdfStore(client, 'triples');
  const result = await store.queryTriples({}, 2);
  assert.deepEqual(result, [triple('1'), triple('2')]);
});

test('batch writes retry only unprocessed items with backoff', async () => {
  const sleeps = [];
  let calls = 0;
  const client = new Client(command => {
    calls += 1;
    const requests = command.input.RequestItems.triples;
    return calls === 1 ? { UnprocessedItems: { triples: [requests[1]] } } : { UnprocessedItems: {} };
  });
  const store = new DynamoRdfStore(client, 'triples', { sleep: ms => { sleeps.push(ms); }, maxRetries: 2 });
  assert.deepEqual(await store.addTriples([triple('1'), triple('2')]), { written: 2, retries: 1 });
  assert.deepEqual(sleeps, [25]);
});

test('single delete returns whether a previous item existed', async () => {
  const existing = new Client(() => ({ Attributes: encodeTriple(triple('1')) }));
  const missing = new Client(() => ({}));
  assert.equal(await new DynamoRdfStore(existing, 'triples').deleteTriple(triple('1')), true);
  assert.equal(await new DynamoRdfStore(missing, 'triples').deleteTriple(triple('1')), false);
});

test('deletePattern batches matched keys', async () => {
  let scan = 0;
  let deleted = 0;
  const client = new Client(command => {
    if (command.operation === 'Scan') {
      scan += 1;
      return { Items: [encodeTriple(triple('1', 'g')), encodeTriple(triple('2', 'g'))] };
    }
    if (command.operation === 'BatchWriteItem') {
      deleted += command.input.RequestItems.triples.length;
      return { UnprocessedItems: {} };
    }
    return {};
  });
  const store = new DynamoRdfStore(client, 'triples');
  assert.equal(await store.clearGraph('g'), 2);
  assert.equal(scan, 1);
  assert.equal(deleted, 2);
});

test('statistics computes graph and predicate cardinalities', async () => {
  const client = new Client(() => ({ Items: [
    encodeTriple({ subject: 's1', predicate: 'p', object: 'o1', graph: 'g' }),
    encodeTriple({ subject: 's2', predicate: 'p', object: 'o2', graph: 'g' }),
    encodeTriple({ subject: 's1', predicate: 'q', object: 'o2' }),
  ] }));
  const stats = await new DynamoRdfStore(client, 'triples').statistics();
  assert.deepEqual(stats, {
    count: 3,
    distinctSubjects: 2,
    distinctObjects: 2,
    byPredicate: { p: 2, q: 1 },
    byGraph: { '': 1, g: 2 },
  });
});
