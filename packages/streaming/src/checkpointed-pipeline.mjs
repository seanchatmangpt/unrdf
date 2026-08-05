import { createHash } from 'node:crypto';

export class BackpressureRefusal extends Error {
  constructor(message, details = {}) {
    super(message);
    this.name = 'BackpressureRefusal';
    this.code = 'BACKPRESSURE_REFUSED';
    this.details = details;
  }
}

export class PipelineAbort extends Error {
  constructor(message, details = {}) {
    super(message);
    this.name = 'PipelineAbort';
    this.code = 'PIPELINE_ABORTED';
    this.details = details;
  }
}

export function itemId(item) {
  if (item && typeof item === 'object' && item.id != null) return String(item.id);
  return createHash('sha256').update(JSON.stringify(item)).digest('hex');
}

export class MemoryCheckpointStore {
  constructor(initial = {}) {
    this.values = new Map(Object.entries(initial));
    this.seen = new Map();
  }

  async load(streamId) {
    return this.values.get(streamId) || null;
  }

  async save(streamId, checkpoint) {
    this.values.set(streamId, structuredClone(checkpoint));
    return checkpoint;
  }

  async hasSeen(streamId, id) {
    return this.seen.get(streamId)?.has(id) || false;
  }

  async markSeen(streamId, id) {
    if (!this.seen.has(streamId)) this.seen.set(streamId, new Set());
    this.seen.get(streamId).add(id);
  }
}

export class BoundedAsyncQueue {
  constructor({ capacity = 100, policy = 'wait' } = {}) {
    if (!Number.isInteger(capacity) || capacity < 1) throw new TypeError('capacity must be a positive integer');
    if (!['wait', 'drop-oldest', 'drop-newest', 'refuse'].includes(policy)) throw new TypeError(`Unknown backpressure policy: ${policy}`);
    this.capacity = capacity;
    this.policy = policy;
    this.items = [];
    this.readers = [];
    this.writers = [];
    this.closed = false;
    this.stats = { accepted: 0, dropped: 0, refused: 0, peakDepth: 0 };
  }

  get size() { return this.items.length; }

  async push(value) {
    if (this.closed) throw new PipelineAbort('queue is closed');
    if (this.readers.length) {
      const reader = this.readers.shift();
      this.stats.accepted++;
      reader.resolve({ value, done: false });
      return { accepted: true, dropped: null };
    }
    if (this.items.length < this.capacity) {
      this.items.push(value);
      this.stats.accepted++;
      this.stats.peakDepth = Math.max(this.stats.peakDepth, this.items.length);
      return { accepted: true, dropped: null };
    }
    if (this.policy === 'drop-oldest') {
      const dropped = this.items.shift();
      this.items.push(value);
      this.stats.accepted++;
      this.stats.dropped++;
      return { accepted: true, dropped };
    }
    if (this.policy === 'drop-newest') {
      this.stats.dropped++;
      return { accepted: false, dropped: value };
    }
    if (this.policy === 'refuse') {
      this.stats.refused++;
      throw new BackpressureRefusal('queue capacity exceeded', { capacity: this.capacity });
    }
    return new Promise((resolve, reject) => this.writers.push({ value, resolve, reject }));
  }

  shift() {
    if (this.items.length) {
      const value = this.items.shift();
      this.drainWriter();
      return Promise.resolve({ value, done: false });
    }
    if (this.closed) return Promise.resolve({ value: undefined, done: true });
    return new Promise((resolve, reject) => this.readers.push({ resolve, reject }));
  }

  drainWriter() {
    if (!this.writers.length || this.closed) return;
    const writer = this.writers.shift();
    if (this.readers.length) {
      const reader = this.readers.shift();
      this.stats.accepted++;
      reader.resolve({ value: writer.value, done: false });
      writer.resolve({ accepted: true, dropped: null });
      return;
    }
    this.items.push(writer.value);
    this.stats.accepted++;
    this.stats.peakDepth = Math.max(this.stats.peakDepth, this.items.length);
    writer.resolve({ accepted: true, dropped: null });
  }

  close(error = null) {
    if (this.closed) return;
    this.closed = true;
    for (const reader of this.readers.splice(0)) {
      if (error) reader.reject(error);
      else reader.resolve({ value: undefined, done: true });
    }
    for (const writer of this.writers.splice(0)) {
      if (error) writer.reject(error);
      else writer.reject(new PipelineAbort('queue closed before write was admitted'));
    }
  }

  [Symbol.asyncIterator]() { return this; }
  next() { return this.shift(); }
}

function sleep(ms, signal) {
  if (ms <= 0) return Promise.resolve();
  return new Promise((resolve, reject) => {
    const timer = setTimeout(resolve, ms);
    signal?.addEventListener('abort', () => {
      clearTimeout(timer);
      reject(new PipelineAbort('pipeline aborted during retry delay'));
    }, { once: true });
  });
}

async function executeWithRetry(item, handler, options, signal) {
  let attempt = 0;
  let lastError;
  while (attempt <= options.retries) {
    if (signal?.aborted) throw new PipelineAbort('pipeline aborted');
    try {
      return { value: await handler(item, { attempt, signal }), attempts: attempt + 1 };
    } catch (error) {
      lastError = error;
      if (attempt === options.retries) break;
      const delay = Math.min(options.maxRetryDelayMs, options.retryDelayMs * (2 ** attempt));
      await sleep(delay, signal);
      attempt++;
    }
  }
  throw Object.assign(lastError || new Error('handler failed'), { attempts: attempt + 1 });
}

export function batchBySize(items, size) {
  if (!Number.isInteger(size) || size < 1) throw new TypeError('batch size must be positive');
  const batches = [];
  for (let index = 0; index < items.length; index += size) batches.push(items.slice(index, index + size));
  return batches;
}

export async function runCheckpointedPipeline(source, handler, options = {}) {
  const config = {
    streamId: options.streamId || 'default',
    concurrency: options.concurrency || 1,
    capacity: options.capacity || Math.max(2, (options.concurrency || 1) * 2),
    backpressure: options.backpressure || 'wait',
    retries: options.retries ?? 0,
    retryDelayMs: options.retryDelayMs ?? 10,
    maxRetryDelayMs: options.maxRetryDelayMs ?? 1000,
    exactlyOnce: options.exactlyOnce !== false,
    failFast: options.failFast === true,
    checkpointStore: options.checkpointStore || new MemoryCheckpointStore(),
    signal: options.signal,
    id: options.id || itemId,
    offset: options.offset || ((item, index) => item?.offset ?? index),
    deadLetter: options.deadLetter || (async () => {}),
  };
  if (!Number.isInteger(config.concurrency) || config.concurrency < 1) throw new TypeError('concurrency must be positive');

  const checkpoint = await config.checkpointStore.load(config.streamId);
  const queue = new BoundedAsyncQueue({ capacity: config.capacity, policy: config.backpressure });
  const outcomes = new Map();
  const committed = [];
  const failures = [];
  let sequence = 0;
  let nextCommit = 0;
  let sourceError = null;

  const commitReady = async () => {
    while (outcomes.has(nextCommit)) {
      const outcome = outcomes.get(nextCommit);
      if (outcome.status === 'pending') return;
      outcomes.delete(nextCommit);
      if (outcome.status === 'fulfilled') {
        if (config.exactlyOnce) await config.checkpointStore.markSeen(config.streamId, outcome.id);
        const saved = {
          sequence: nextCommit,
          offset: outcome.offset,
          id: outcome.id,
          processedAt: options.now?.() ?? Date.now(),
        };
        await config.checkpointStore.save(config.streamId, saved);
        committed.push({ ...outcome, checkpoint: saved });
      } else {
        failures.push(outcome);
        await config.deadLetter(outcome.item, outcome.error, { sequence: nextCommit, id: outcome.id, offset: outcome.offset });
        if (config.failFast) throw outcome.error;
      }
      nextCommit++;
    }
  };

  const workers = Array.from({ length: config.concurrency }, async () => {
    for await (const envelope of queue) {
      const { item, sequence: itemSequence, id, offset } = envelope;
      if (config.exactlyOnce && await config.checkpointStore.hasSeen(config.streamId, id)) {
        outcomes.set(itemSequence, { status: 'fulfilled', item, id, offset, skipped: true, value: null, attempts: 0 });
        await commitReady();
        continue;
      }
      outcomes.set(itemSequence, { status: 'pending' });
      try {
        const result = await executeWithRetry(item, handler, config, config.signal);
        outcomes.set(itemSequence, { status: 'fulfilled', item, id, offset, ...result, skipped: false });
      } catch (error) {
        outcomes.set(itemSequence, { status: 'rejected', item, id, offset, error, attempts: error.attempts || config.retries + 1 });
      }
      await commitReady();
    }
  });

  try {
    let index = 0;
    for await (const item of source) {
      if (config.signal?.aborted) throw new PipelineAbort('pipeline aborted');
      const offset = config.offset(item, index);
      if (checkpoint && offset <= checkpoint.offset) { index++; continue; }
      const envelope = { item, sequence, id: config.id(item, index), offset };
      const admission = await queue.push(envelope);
      if (!admission.accepted) failures.push({ status: 'dropped', item, id: envelope.id, offset, reason: 'backpressure' });
      sequence++;
      index++;
    }
  } catch (error) {
    sourceError = error;
  } finally {
    queue.close(sourceError);
  }

  const workerResults = await Promise.allSettled(workers);
  const workerFailure = workerResults.find(result => result.status === 'rejected');
  if (sourceError) throw sourceError;
  if (workerFailure) throw workerFailure.reason;
  await commitReady();

  return {
    streamId: config.streamId,
    processed: committed.length,
    failures,
    committed,
    checkpoint: await config.checkpointStore.load(config.streamId),
    queue: { ...queue.stats },
  };
}
