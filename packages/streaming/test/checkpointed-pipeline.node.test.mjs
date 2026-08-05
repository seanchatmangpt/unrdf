import test from 'node:test';
import assert from 'node:assert/strict';
import {
  BoundedAsyncQueue,
  BackpressureRefusal,
  MemoryCheckpointStore,
  batchBySize,
  runCheckpointedPipeline,
} from '../src/checkpointed-pipeline.mjs';

async function* source(items) { for (const item of items) yield item; }

test('bounded queue supports drop-oldest', async () => {
  const queue = new BoundedAsyncQueue({ capacity: 2, policy: 'drop-oldest' });
  await queue.push(1); await queue.push(2);
  const result = await queue.push(3);
  assert.equal(result.dropped, 1);
  assert.equal((await queue.shift()).value, 2);
  assert.equal((await queue.shift()).value, 3);
});

test('bounded queue supports drop-newest', async () => {
  const queue = new BoundedAsyncQueue({ capacity: 1, policy: 'drop-newest' });
  await queue.push(1);
  const result = await queue.push(2);
  assert.deepEqual(result, { accepted: false, dropped: 2 });
  assert.equal(queue.stats.dropped, 1);
});

test('bounded queue refuses overflow', async () => {
  const queue = new BoundedAsyncQueue({ capacity: 1, policy: 'refuse' });
  await queue.push(1);
  await assert.rejects(queue.push(2), BackpressureRefusal);
});

test('wait policy unblocks writers after a read', async () => {
  const queue = new BoundedAsyncQueue({ capacity: 1, policy: 'wait' });
  await queue.push(1);
  let resolved = false;
  const pending = queue.push(2).then(() => { resolved = true; });
  await Promise.resolve();
  assert.equal(resolved, false);
  assert.equal((await queue.shift()).value, 1);
  await pending;
  assert.equal((await queue.shift()).value, 2);
});

test('pipeline preserves checkpoint commit order under concurrency', async () => {
  const checkpointStore = new MemoryCheckpointStore();
  const result = await runCheckpointedPipeline(source([{ id: 'a', offset: 1 }, { id: 'b', offset: 2 }, { id: 'c', offset: 3 }]), async item => {
    await new Promise(resolve => setTimeout(resolve, item.id === 'a' ? 15 : 1));
    return item.id.toUpperCase();
  }, { streamId: 'ordered', concurrency: 3, checkpointStore, now: () => 10 });
  assert.deepEqual(result.committed.map(x => x.id), ['a', 'b', 'c']);
  assert.equal(result.checkpoint.offset, 3);
});

test('pipeline retries with exponential attempts', async () => {
  let calls = 0;
  const result = await runCheckpointedPipeline(source([{ id: 'a', offset: 1 }]), async () => {
    calls++;
    if (calls < 3) throw new Error('transient');
    return 'ok';
  }, { retries: 2, retryDelayMs: 0 });
  assert.equal(result.committed[0].attempts, 3);
});

test('failed items are sent to dead letter and later offsets continue', async () => {
  const dead = [];
  const result = await runCheckpointedPipeline(source([{ id: 'a', offset: 1 }, { id: 'b', offset: 2 }]), async item => {
    if (item.id === 'a') throw new Error('bad');
    return 'ok';
  }, { deadLetter: async (item, error) => dead.push([item.id, error.message]) });
  assert.deepEqual(dead, [['a', 'bad']]);
  assert.equal(result.committed[0].id, 'b');
  assert.equal(result.failures.length, 1);
});

test('exactly-once skips already seen identities', async () => {
  const store = new MemoryCheckpointStore();
  let calls = 0;
  await runCheckpointedPipeline(source([{ id: 'a', offset: 1 }]), async () => { calls++; }, { streamId: 's', checkpointStore: store });
  await store.save('s', null);
  const second = await runCheckpointedPipeline(source([{ id: 'a', offset: 1 }]), async () => { calls++; }, { streamId: 's', checkpointStore: store });
  assert.equal(calls, 1);
  assert.equal(second.committed[0].skipped, true);
});

test('pipeline resumes after saved checkpoint', async () => {
  const store = new MemoryCheckpointStore({ s: { offset: 2, sequence: 1, id: 'b' } });
  const result = await runCheckpointedPipeline(source([{ id: 'a', offset: 1 }, { id: 'b', offset: 2 }, { id: 'c', offset: 3 }]), async item => item.id, { streamId: 's', checkpointStore: store });
  assert.deepEqual(result.committed.map(x => x.id), ['c']);
});

test('batchBySize creates bounded batches', () => {
  assert.deepEqual(batchBySize([1, 2, 3, 4, 5], 2), [[1, 2], [3, 4], [5]]);
});
