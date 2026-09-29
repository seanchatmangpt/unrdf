import { createHash } from 'node:crypto';

/**
 * Error thrown when a bounded queue using the 'refuse' policy is at capacity.
 */
export class BackpressureRefusal extends Error {
  /**
   * @param {string} message - Human-readable error message.
   * @param {object} [details] - Extra context (for example the queue capacity).
   */
  constructor(message, details = {}) {
    super(message);
    this.name = 'BackpressureRefusal';
    this.code = 'BACKPRESSURE_REFUSED';
    this.details = details;
  }
}

/**
 * Error signalling that a pipeline or its queue was aborted or closed.
 */
export class PipelineAbort extends Error {
  /**
   * @param {string} message - Human-readable error message.
   * @param {object} [details] - Extra context about the abort.
   */
  constructor(message, details = {}) {
    super(message);
    this.name = 'PipelineAbort';
    this.code = 'PIPELINE_ABORTED';
    this.details = details;
  }
}

/**
 * Derives a stable identifier for a stream item: its `id` property when present,
 * otherwise the SHA-256 hex digest of its JSON serialisation.
 * @param {*} item - The stream item.
 * @returns {string} The item identifier.
 */
export function itemId(item) {
  if (item && typeof item === 'object' && item.id != null) return String(item.id);
  return createHash('sha256').update(JSON.stringify(item)).digest('hex');
}

/**
 * In-memory checkpoint store holding the last checkpoint and the set of
 * processed item ids per stream.
 */
export class MemoryCheckpointStore {
  /**
   * @param {Object<string, object>} [initial] - Initial checkpoints keyed by stream id.
   */
  constructor(initial = {}) {
    this.values = new Map(Object.entries(initial));
    this.seen = new Map();
  }

  /**
   * Loads the last saved checkpoint for a stream.
   * @param {string} streamId - Stream identifier.
   * @returns {Promise<object|null>} The checkpoint, or null when none exists.
   */
  async load(streamId) {
    return this.values.get(streamId) || null;
  }

  /**
   * Saves a structured clone of a checkpoint for a stream.
   * @param {string} streamId - Stream identifier.
   * @param {object} checkpoint - Checkpoint to store.
   * @returns {Promise<object>} The checkpoint that was passed in.
   */
  async save(streamId, checkpoint) {
    this.values.set(streamId, structuredClone(checkpoint));
    return checkpoint;
  }

  /**
   * Tells whether an item id was already marked as processed for a stream.
   * @param {string} streamId - Stream identifier.
   * @param {string} id - Item identifier.
   * @returns {Promise<boolean>} True if the id was marked seen.
   */
  async hasSeen(streamId, id) {
    return this.seen.get(streamId)?.has(id) || false;
  }

  /**
   * Records an item id as processed for a stream.
   * @param {string} streamId - Stream identifier.
   * @param {string} id - Item identifier.
   * @returns {Promise<void>}
   */
  async markSeen(streamId, id) {
    if (!this.seen.has(streamId)) this.seen.set(streamId, new Set());
    this.seen.get(streamId).add(id);
  }
}

/**
 * Bounded async queue with a configurable backpressure policy
 * ('wait', 'drop-oldest', 'drop-newest' or 'refuse'). Also an async iterator.
 */
export class BoundedAsyncQueue {
  /**
   * @param {object} [options] - Queue options.
   * @param {number} [options.capacity] - Maximum buffered items (positive integer).
   * @param {string} [options.policy] - Behaviour when full: 'wait', 'drop-oldest', 'drop-newest' or 'refuse'.
   * @throws {TypeError} If capacity or policy is invalid.
   */
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

  /**
   * Number of items currently buffered.
   * @returns {number} Buffered item count.
   */
  get size() { return this.items.length; }

  /**
   * Adds a value to the queue, applying the backpressure policy when full.
   * @param {*} value - Value to enqueue.
   * @returns {Promise<{accepted: boolean, dropped: *}>} Admission result; `dropped` holds a value discarded by the policy, or null.
   * @throws {PipelineAbort} If the queue is closed.
   * @throws {BackpressureRefusal} If the queue is full and the policy is 'refuse'.
   */
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

  /**
   * Takes the next value, waiting for one if the queue is empty.
   * @returns {Promise<{value: *, done: boolean}>} Iterator result; done is true once the queue is closed and drained.
   */
  shift() {
    if (this.items.length) {
      const value = this.items.shift();
      this.drainWriter();
      return Promise.resolve({ value, done: false });
    }
    if (this.closed) return Promise.resolve({ value: undefined, done: true });
    return new Promise((resolve, reject) => this.readers.push({ resolve, reject }));
  }

  /**
   * Admits the oldest waiting writer (from the 'wait' policy) into the freed slot,
   * or hands its value straight to a waiting reader.
   * @returns {void}
   */
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

  /**
   * Closes the queue, releasing waiting readers and rejecting waiting writers.
   * @param {Error|null} [error] - If given, waiting readers and writers are rejected with it.
   * @returns {void}
   */
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

  /**
   * Makes the queue usable in `for await` loops.
   * @returns {BoundedAsyncQueue} The queue itself.
   */
  [Symbol.asyncIterator]() { return this; }
  /**
   * Async iterator step; equivalent to {@link BoundedAsyncQueue#shift}.
   * @returns {Promise<{value: *, done: boolean}>} Iterator result.
   */
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

/**
 * Splits an array into consecutive batches of at most `size` items.
 * @param {Array} items - Items to split.
 * @param {number} size - Maximum batch size (positive integer).
 * @returns {Array<Array>} The batches.
 * @throws {TypeError} If size is not a positive integer.
 */
export function batchBySize(items, size) {
  if (!Number.isInteger(size) || size < 1) throw new TypeError('batch size must be positive');
  const batches = [];
  for (let index = 0; index < items.length; index += size) batches.push(items.slice(index, index + size));
  return batches;
}

/**
 * Runs a source through a handler with bounded concurrency, retries with
 * exponential backoff, backpressure, optional exactly-once de-duplication,
 * in-order checkpoint commits and a dead-letter callback.
 * @param {AsyncIterable|Iterable} source - Items to process.
 * @param {Function} handler - Async function `(item, {attempt, signal}) => value` applied to each item.
 * @param {object} [options] - Pipeline options (streamId, concurrency, capacity, backpressure, retries,
 *   retryDelayMs, maxRetryDelayMs, exactlyOnce, failFast, checkpointStore, signal, id, offset, deadLetter, now).
 * @returns {Promise<{streamId: string, processed: number, failures: Array, committed: Array, checkpoint: object|null, queue: object}>} Run summary.
 * @throws {TypeError} If concurrency is not a positive integer.
 * @throws {Error} The source error, a worker error, or the item error when failFast is set.
 */
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
