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

export function canonicalJson(value) {
  return JSON.stringify(canonical(value));
}

export function sha256(value) {
  return createHash('sha256').update(typeof value === 'string' ? value : canonicalJson(value)).digest('hex');
}

export function termKey(term) {
  if (!term) return '';
  return `${term.termType || ''}|${term.value ?? ''}|${term.language || ''}|${term.datatype?.value || ''}`;
}

export function quadKey(quad) {
  if (!quad?.subject || !quad?.predicate || !quad?.object) throw new TypeError('quadKey requires an RDF/JS quad');
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

export class TransactionConflict extends Error {
  constructor(message, details = {}) {
    super(message);
    this.name = 'TransactionConflict';
    this.code = 'TRANSACTION_CONFLICT';
    this.details = details;
  }
}

export class TransactionRefusal extends Error {
  constructor(message, details = {}) {
    super(message);
    this.name = 'TransactionRefusal';
    this.code = 'TRANSACTION_REFUSED';
    this.details = details;
  }
}

export class MemoryQuadStore {
  constructor(quads = []) {
    this.quads = new Map();
    this.keyVersions = new Map();
    this.version = 0;
    this.idempotency = new Map();
    for (const quad of quads) this.quads.set(quadKey(quad), cloneQuad(quad));
  }

  snapshot() {
    return {
      version: this.version,
      quads: new Map(this.quads),
      keyVersions: new Map(this.keyVersions),
    };
  }

  get(key) {
    return this.quads.get(key) || null;
  }

  values() {
    return [...this.quads.values()];
  }

  match(pattern = {}) {
    return this.values().filter(quad => {
      for (const field of ['subject', 'predicate', 'object', 'graph']) {
        if (pattern[field] && termKey(pattern[field]) !== termKey(quad[field])) return false;
      }
      return true;
    });
  }

  digest() {
    return sha256([...this.quads.entries()].sort(([a], [b]) => a.localeCompare(b)).map(([key]) => key));
  }
}

function normalizeOperation(operation) {
  if (!operation || !['add', 'delete'].includes(operation.type)) throw new TypeError('operation.type must be add or delete');
  const key = quadKey(operation.quad);
  return { type: operation.type, key, quad: cloneQuad(operation.quad) };
}

export class QuadTransaction {
  constructor(store, options = {}) {
    if (!(store instanceof MemoryQuadStore)) throw new TypeError('QuadTransaction requires MemoryQuadStore');
    this.store = store;
    this.id = options.id || randomUUID();
    this.actor = options.actor || 'anonymous';
    this.isolation = options.isolation || 'snapshot';
    if (!ISOLATIONS.has(this.isolation)) throw new TypeError(`Unsupported isolation: ${this.isolation}`);
    this.idempotencyKey = options.idempotencyKey || null;
    this.snapshot = store.snapshot();
    this.readSet = new Set();
    this.operations = new Map();
    this.assertions = [];
    this.savepoints = [];
    this.state = 'OPEN';
  }

  ensureOpen() {
    if (this.state !== 'OPEN') throw new TransactionRefusal(`Transaction is ${this.state}`);
  }

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

  has(quad) {
    const key = quadKey(quad);
    this.readSet.add(key);
    const pending = this.operations.get(key);
    if (pending) return pending.type === 'add';
    return this.snapshot.quads.has(key);
  }

  add(quad) {
    this.ensureOpen();
    const operation = normalizeOperation({ type: 'add', quad });
    this.operations.set(operation.key, operation);
    return this;
  }

  delete(quad) {
    this.ensureOpen();
    const operation = normalizeOperation({ type: 'delete', quad });
    this.operations.set(operation.key, operation);
    return this;
  }

  apply(operations) {
    for (const operation of operations) {
      if (operation.type === 'add') this.add(operation.quad);
      else this.delete(operation.quad);
    }
    return this;
  }

  assert(predicate, message = 'Transaction assertion failed', details = {}) {
    this.ensureOpen();
    if (typeof predicate !== 'function') throw new TypeError('assert predicate must be a function');
    this.assertions.push({ predicate, message, details });
    return this;
  }

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

  rollback(reason = 'explicit rollback') {
    this.ensureOpen();
    this.state = 'ROLLED_BACK';
    return { transactionId: this.id, state: this.state, reason };
  }

  detectConflicts() {
    const keys = new Set(this.operations.keys());
    if (this.isolation === 'serializable') for (const key of this.readSet) keys.add(key);
    const conflicts = [];
    for (const key of keys) {
      const changedAt = this.store.keyVersions.get(key) || 0;
      if (changedAt > this.snapshot.version) conflicts.push({ key, changedAt, snapshotVersion: this.snapshot.version });
    }
    return conflicts;
  }

  preview() {
    const map = new Map(this.store.quads);
    for (const operation of this.operations.values()) {
      if (operation.type === 'add') map.set(operation.key, operation.quad);
      else map.delete(operation.key);
    }
    return [...map.values()];
  }

  commit(options = {}) {
    this.ensureOpen();
    if (this.idempotencyKey && this.store.idempotency.has(this.idempotencyKey)) {
      this.state = 'COMMITTED';
      return { ...this.store.idempotency.get(this.idempotencyKey), replayed: true };
    }

    const conflicts = this.detectConflicts();
    if (conflicts.length) throw new TransactionConflict('Transaction conflicts with committed changes', { conflicts });

    const view = this.preview();
    for (const assertion of this.assertions) {
      if (!assertion.predicate(view, this)) throw new TransactionRefusal(assertion.message, assertion.details);
    }

    const beforeHash = this.store.digest();
    const applied = [];
    const nextVersion = this.store.version + 1;
    for (const operation of [...this.operations.values()].sort((a, b) => a.key.localeCompare(b.key))) {
      const existed = this.store.quads.has(operation.key);
      if (operation.type === 'add') this.store.quads.set(operation.key, operation.quad);
      else this.store.quads.delete(operation.key);
      this.store.keyVersions.set(operation.key, nextVersion);
      applied.push({ type: operation.type, key: operation.key, changed: operation.type === 'add' ? !existed : existed });
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

export function beginTransaction(store, options) {
  return new QuadTransaction(store, options);
}

export function verifyTransactionReceipt(receipt) {
  if (!receipt || typeof receipt !== 'object') return { valid: false, reason: 'receipt missing' };
  const { receiptHash, replayed: _replayed, ...body } = receipt;
  const valid = receiptHash === sha256(body);
  return { valid, reason: valid ? null : 'receipt digest mismatch' };
}

export function replayOperations(initialQuads, operations) {
  const store = new MemoryQuadStore(initialQuads);
  const transaction = beginTransaction(store, { id: 'replay', actor: 'replay' });
  transaction.apply(operations.map(operation => ({ type: operation.type, quad: operation.quad })));
  return { store, receipt: transaction.commit() };
}
