import { createHash, randomUUID } from 'node:crypto';

const ISOLATIONS = new Set(['snapshot', 'serializable']);

function canonical(value) {
  if (value === null || typeof value !== 'object') {
    if (typeof value === 'bigint') return { $bigint: value.toString() };
    if (typeof value === 'number' && !Number.isFinite(value)) return { $number: String(value) };
    return value;
  }
  if (Array.isArray(value)) return value.map(canonical);
  if (value instanceof Uint8Array) return { $bytes: Buffer.from(value).toString('base64') };
  const out = {};
  for (const key of Object.keys(value).sort()) out[key] = canonical(value[key]);
  return out;
}

/**
 * Serialize a value to deterministic JSON (sorted keys; bigint, non-finite numbers and bytes get tagged encodings).
 * @param {*} value - Value to serialize.
 * @returns {string} Canonical JSON string.
 */
export function canonicalJson(value) {
  return JSON.stringify(canonical(value));
}

/**
 * SHA-256 of a string, or of the canonical JSON of any other value.
 * @param {string|*} value - Input to hash.
 * @returns {string} Hex digest.
 */
export function sha256(value) {
  return createHash('sha256')
    .update(typeof value === 'string' ? value : canonicalJson(value))
    .digest('hex');
}

/**
 * Build a stable string key for an RDF/JS term.
 * @param {Object|null|undefined} term - RDF/JS term (termType, value, language, datatype).
 * @returns {string} Key, or an empty string when no term is given.
 */
export function termKey(term) {
  if (!term) return '';
  return `${term.termType || ''}|${term.value ?? ''}|${term.language || ''}|${term.datatype?.value || ''}`;
}

/**
 * Build a stable string key for an RDF/JS quad from its four terms.
 * @param {Object} quad - RDF/JS quad with subject, predicate, object and graph.
 * @returns {string} Quad key.
 * @throws {TypeError} If subject, predicate or object is missing.
 */
export function quadKey(quad) {
  if (!quad?.subject || !quad?.predicate || !quad?.object)
    throw new TypeError('quadKey requires an RDF/JS quad');
  return [quad.subject, quad.predicate, quad.object, quad.graph].map(termKey).join('||');
}

function cloneQuad(quad) {
  return {
    subject: quad.subject,
    predicate: quad.predicate,
    object: quad.object,
    graph: quad.graph || { termType: 'DefaultGraph', value: '' },
  };
}

/**
 * Error thrown when a commit conflicts with changes committed after the transaction's snapshot.
 */
export class TransactionConflict extends Error {
  /**
   * Create the error.
   * @param {string} message - Error message.
   * @param {Object} [details] - Conflict details (for example the conflicting keys).
   */
  constructor(message, details = {}) {
    super(message);
    this.name = 'TransactionConflict';
    this.code = 'TRANSACTION_CONFLICT';
    this.details = details;
  }
}

/**
 * Error thrown when a transaction operation is refused (closed transaction, failed assertion, unknown savepoint).
 */
export class TransactionRefusal extends Error {
  /**
   * Create the error.
   * @param {string} message - Error message.
   * @param {Object} [details] - Refusal details.
   */
  constructor(message, details = {}) {
    super(message);
    this.name = 'TransactionRefusal';
    this.code = 'TRANSACTION_REFUSED';
    this.details = details;
  }
}

/**
 * In-memory quad store with per-key version tracking and idempotency records, used by QuadTransaction.
 */
export class MemoryQuadStore {
  /**
   * Create a store.
   * @param {Object[]} [quads] - Initial RDF/JS quads (copied).
   */
  constructor(quads = []) {
    this.quads = new Map();
    this.keyVersions = new Map();
    this.version = 0;
    this.idempotency = new Map();
    for (const quad of quads) this.quads.set(quadKey(quad), cloneQuad(quad));
  }

  /**
   * Capture the store version and copies of its quad and key-version maps.
   * @returns {{version: number, quads: Map, keyVersions: Map}} Snapshot.
   */
  snapshot() {
    return {
      version: this.version,
      quads: new Map(this.quads),
      keyVersions: new Map(this.keyVersions),
    };
  }

  /**
   * Look up a quad by key.
   * @param {string} key - Key from quadKey.
   * @returns {Object|null} The stored quad, or null.
   */
  get(key) {
    return this.quads.get(key) || null;
  }

  /**
   * List all stored quads.
   * @returns {Object[]} Quads in insertion order.
   */
  values() {
    return [...this.quads.values()];
  }

  /**
   * Find quads matching a term pattern; omitted fields match anything.
   * @param {Object} [pattern] - Optional subject, predicate, object and graph terms.
   * @returns {Object[]} Matching quads.
   */
  match(pattern = {}) {
    return this.values().filter(quad => {
      for (const field of ['subject', 'predicate', 'object', 'graph']) {
        if (pattern[field] && termKey(pattern[field]) !== termKey(quad[field])) return false;
      }
      return true;
    });
  }

  /**
   * Digest of the sorted quad keys, identifying the store's content.
   * @returns {string} Hex digest.
   */
  digest() {
    return sha256(
      [...this.quads.entries()].sort(([a], [b]) => a.localeCompare(b)).map(([key]) => key)
    );
  }
}

function normalizeOperation(operation) {
  if (!operation || !['add', 'delete'].includes(operation.type))
    throw new TypeError('operation.type must be add or delete');
  const key = quadKey(operation.quad);
  return { type: operation.type, key, quad: cloneQuad(operation.quad) };
}

/**
 * Optimistic transaction over a MemoryQuadStore with snapshot or serializable isolation, assertions, savepoints and receipts.
 */
export class QuadTransaction {
  /**
   * Begin a transaction on a store's current snapshot.
   * @param {MemoryQuadStore} store - Target store.
   * @param {Object} [options] - Options.
   * @param {string} [options.id] - Transaction id; random UUID by default.
   * @param {string} [options.actor] - Actor name; defaults to anonymous.
   * @param {string} [options.isolation] - 'snapshot' or 'serializable'.
   * @param {string} [options.idempotencyKey] - Key making commit replay-safe.
   * @throws {TypeError} If the store is not a MemoryQuadStore or isolation is unsupported.
   */
  constructor(store, options = {}) {
    if (!(store instanceof MemoryQuadStore))
      throw new TypeError('QuadTransaction requires MemoryQuadStore');
    this.store = store;
    this.id = options.id || randomUUID();
    this.actor = options.actor || 'anonymous';
    this.isolation = options.isolation || 'snapshot';
    if (!ISOLATIONS.has(this.isolation))
      throw new TypeError(`Unsupported isolation: ${this.isolation}`);
    this.idempotencyKey = options.idempotencyKey || null;
    this.snapshot = store.snapshot();
    this.readSet = new Set();
    this.operations = new Map();
    this.assertions = [];
    this.savepoints = [];
    this.state = 'OPEN';
  }

  /**
   * Assert that the transaction is still open.
   * @throws {TransactionRefusal} If the transaction is committed or rolled back.
   */
  ensureOpen() {
    if (this.state !== 'OPEN') throw new TransactionRefusal(`Transaction is ${this.state}`);
  }

  /**
   * Read quads matching a pattern from the snapshot plus pending operations, recording them in the read set.
   * @param {Object} [pattern] - Optional subject, predicate, object and graph terms.
   * @returns {Object[]} Matching quads.
   * @throws {TransactionRefusal} If the transaction is not open.
   */
  read(pattern = {}) {
    this.ensureOpen();
    const base = new Map(this.snapshot.quads);
    for (const operation of this.operations.values()) {
      if (operation.type === 'add') base.set(operation.key, operation.quad);
      else base.delete(operation.key);
    }
    const results = [...base.entries()].filter(([, quad]) => {
      for (const field of ['subject', 'predicate', 'object', 'graph']) {
        if (pattern[field] && termKey(pattern[field]) !== termKey(quad[field])) return false;
      }
      return true;
    });
    for (const [key] of results) this.readSet.add(key);
    return results.map(([, quad]) => quad);
  }

  /**
   * Check whether a quad is present in the transaction's view, recording it in the read set.
   * @param {Object} quad - RDF/JS quad.
   * @returns {boolean} True if present.
   */
  has(quad) {
    const key = quadKey(quad);
    this.readSet.add(key);
    const pending = this.operations.get(key);
    if (pending) return pending.type === 'add';
    return this.snapshot.quads.has(key);
  }

  /**
   * Queue a quad addition.
   * @param {Object} quad - RDF/JS quad.
   * @returns {QuadTransaction} This transaction, for chaining.
   * @throws {TransactionRefusal} If the transaction is not open.
   */
  add(quad) {
    this.ensureOpen();
    const operation = normalizeOperation({ type: 'add', quad });
    this.operations.set(operation.key, operation);
    return this;
  }

  /**
   * Queue a quad deletion.
   * @param {Object} quad - RDF/JS quad.
   * @returns {QuadTransaction} This transaction, for chaining.
   * @throws {TransactionRefusal} If the transaction is not open.
   */
  delete(quad) {
    this.ensureOpen();
    const operation = normalizeOperation({ type: 'delete', quad });
    this.operations.set(operation.key, operation);
    return this;
  }

  /**
   * Queue a list of add/delete operations.
   * @param {Array<{type: string, quad: Object}>} operations - Operations with type 'add' or anything else treated as delete.
   * @returns {QuadTransaction} This transaction, for chaining.
   */
  apply(operations) {
    for (const operation of operations) {
      if (operation.type === 'add') this.add(operation.quad);
      else this.delete(operation.quad);
    }
    return this;
  }

  /**
   * Register an assertion checked against the preview at commit time.
   * @param {Function} predicate - Function `(view, transaction)` returning true when satisfied.
   * @param {string} [message] - Refusal message on failure.
   * @param {Object} [details] - Refusal details on failure.
   * @returns {QuadTransaction} This transaction, for chaining.
   * @throws {TypeError} If predicate is not a function.
   * @throws {TransactionRefusal} If the transaction is not open.
   */
  assert(predicate, message = 'Transaction assertion failed', details = {}) {
    this.ensureOpen();
    if (typeof predicate !== 'function') throw new TypeError('assert predicate must be a function');
    this.assertions.push({ predicate, message, details });
    return this;
  }

  /**
   * Record a savepoint of pending operations, read set and assertion count.
   * @param {string} [name] - Savepoint name; auto-numbered by default.
   * @returns {string} The savepoint name.
   * @throws {TransactionRefusal} If the transaction is not open.
   */
  savepoint(name = `savepoint-${this.savepoints.length + 1}`) {
    this.ensureOpen();
    const point = {
      name,
      operations: new Map(this.operations),
      readSet: new Set(this.readSet),
      assertions: this.assertions.length,
    };
    this.savepoints.push(point);
    return name;
  }

  /**
   * Restore the state recorded at the most recent savepoint with this name, discarding later savepoints.
   * @param {string} name - Savepoint name.
   * @returns {QuadTransaction} This transaction, for chaining.
   * @throws {TransactionRefusal} If the transaction is not open or the savepoint is unknown.
   */
  rollbackTo(name) {
    this.ensureOpen();
    const index = this.savepoints.map(point => point.name).lastIndexOf(name);
    if (index < 0) throw new TransactionRefusal(`Savepoint not found: ${name}`);
    const point = this.savepoints[index];
    this.operations = new Map(point.operations);
    this.readSet = new Set(point.readSet);
    this.assertions.length = point.assertions;
    this.savepoints.length = index + 1;
    return this;
  }

  /**
   * Abort the transaction.
   * @param {string} [reason] - Reason recorded in the result.
   * @returns {{transactionId: string, state: string, reason: string}} Rollback result.
   * @throws {TransactionRefusal} If the transaction is not open.
   */
  rollback(reason = 'explicit rollback') {
    this.ensureOpen();
    this.state = 'ROLLED_BACK';
    return { transactionId: this.id, state: this.state, reason };
  }

  /**
   * Find keys changed in the store since the snapshot, among written keys (and read keys under serializable isolation).
   * @returns {Array<{key: string, changedAt: number, snapshotVersion: number}>} Conflicts.
   */
  detectConflicts() {
    const keys = new Set(this.operations.keys());
    if (this.isolation === 'serializable') for (const key of this.readSet) keys.add(key);
    const conflicts = [];
    for (const key of keys) {
      const changedAt = this.store.keyVersions.get(key) || 0;
      if (changedAt > this.snapshot.version)
        conflicts.push({ key, changedAt, snapshotVersion: this.snapshot.version });
    }
    return conflicts;
  }

  /**
   * Compute the quads the store would hold if the pending operations were applied to its current state.
   * @returns {Object[]} Resulting quads.
   */
  preview() {
    const map = new Map(this.store.quads);
    for (const operation of this.operations.values()) {
      if (operation.type === 'add') map.set(operation.key, operation.quad);
      else map.delete(operation.key);
    }
    return [...map.values()];
  }

  /**
   * Commit pending operations: replays idempotent commits, rejects conflicts and failed assertions, applies operations in key order and issues a hashed receipt.
   * @param {Object} [options] - Commit options.
   * @param {Object} [options.metadata] - Metadata included in the receipt.
   * @returns {Object} Receipt with hashes, applied operations and `replayed` flag.
   * @throws {TransactionRefusal} If the transaction is not open or an assertion fails.
   * @throws {TransactionConflict} If conflicting changes were committed.
   */
  commit(options = {}) {
    this.ensureOpen();
    if (this.idempotencyKey && this.store.idempotency.has(this.idempotencyKey)) {
      this.state = 'COMMITTED';
      return { ...this.store.idempotency.get(this.idempotencyKey), replayed: true };
    }

    const conflicts = this.detectConflicts();
    if (conflicts.length)
      throw new TransactionConflict('Transaction conflicts with committed changes', { conflicts });

    const view = this.preview();
    for (const assertion of this.assertions) {
      if (!assertion.predicate(view, this))
        throw new TransactionRefusal(assertion.message, assertion.details);
    }

    const beforeHash = this.store.digest();
    const applied = [];
    const nextVersion = this.store.version + 1;
    for (const operation of [...this.operations.values()].sort((a, b) =>
      a.key.localeCompare(b.key)
    )) {
      const existed = this.store.quads.has(operation.key);
      if (operation.type === 'add') this.store.quads.set(operation.key, operation.quad);
      else this.store.quads.delete(operation.key);
      this.store.keyVersions.set(operation.key, nextVersion);
      applied.push({
        type: operation.type,
        key: operation.key,
        changed: operation.type === 'add' ? !existed : existed,
      });
    }
    this.store.version = nextVersion;
    const afterHash = this.store.digest();
    this.state = 'COMMITTED';

    const receiptBody = {
      transactionId: this.id,
      actor: this.actor,
      isolation: this.isolation,
      snapshotVersion: this.snapshot.version,
      committedVersion: nextVersion,
      beforeHash,
      afterHash,
      operationHash: sha256(applied),
      operations: applied,
      readSetHash: sha256([...this.readSet].sort()),
      metadata: options.metadata || {},
    };
    const receipt = { ...receiptBody, receiptHash: sha256(receiptBody), replayed: false };
    if (this.idempotencyKey) this.store.idempotency.set(this.idempotencyKey, receipt);
    return receipt;
  }
}

/**
 * Begin a transaction on a store.
 * @param {MemoryQuadStore} store - Target store.
 * @param {Object} [options] - Options passed to the QuadTransaction constructor.
 * @returns {QuadTransaction} The open transaction.
 */
export function beginTransaction(store, options) {
  return new QuadTransaction(store, options);
}

/**
 * Check that a receipt's hash matches its body (ignoring the `replayed` flag).
 * @param {Object} receipt - Receipt from commit.
 * @returns {{valid: boolean, reason: string|null}} Verification result.
 */
export function verifyTransactionReceipt(receipt) {
  if (!receipt || typeof receipt !== 'object') return { valid: false, reason: 'receipt missing' };
  const { receiptHash, replayed: _replayed, ...body } = receipt;
  const valid = receiptHash === sha256(body);
  return { valid, reason: valid ? null : 'receipt digest mismatch' };
}

/**
 * Apply recorded operations to a fresh store seeded with the initial quads and commit them.
 * @param {Object[]} initialQuads - Starting quads.
 * @param {Array<{type: string, quad: Object}>} operations - Operations to replay.
 * @returns {{store: MemoryQuadStore, receipt: Object}} Resulting store and commit receipt.
 */
export function replayOperations(initialQuads, operations) {
  const store = new MemoryQuadStore(initialQuads);
  const transaction = beginTransaction(store, { id: 'replay', actor: 'replay' });
  transaction.apply(operations.map(operation => ({ type: operation.type, quad: operation.quad })));
  return { store, receipt: transaction.commit() };
}
