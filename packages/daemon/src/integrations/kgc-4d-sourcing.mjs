/**
 * @file KGC-4D Event Sourcing Integration
 * @module @unrdf/daemon/integrations/kgc-4d-sourcing
 * @description Event sourcing for daemon operations with an append-only transition ledger.
 */

import { blake3 } from 'hash-wasm';
import { now, toISO } from '@unrdf/kgc-4d';
import {
  validateEventLogEntry,
  validateUniverseFreeze,
  validateMerkleProof,
  validateTemporalQuery,
  generateUUID,
} from './kgc-4d-validation.mjs';
import {
  buildMerkleTree,
  getMerkleProofPath,
  verifyMerkleProof,
} from './kgc-4d-merkle.mjs';

const HASH_SCHEMA = 'urn:unrdf:daemon:event-transition:v2';
const TERMINAL_STATUSES = new Set(['success', 'failure']);
const TEXT_ENCODER = new TextEncoder();

function failEvidence(path, message) {
  throw new TypeError(`${path}: ${message}`);
}

function admitEvidence(value, path = 'value', ancestors = new WeakSet()) {
  if (value === null) return null;

  switch (typeof value) {
    case 'string':
    case 'boolean':
    case 'bigint':
      return value;
    case 'number':
      if (!Number.isFinite(value)) failEvidence(path, 'numbers must be finite');
      return value;
    case 'undefined':
    case 'function':
    case 'symbol':
      failEvidence(path, `${typeof value} is not deterministic evidence`);
      break;
    case 'object':
      break;
    default:
      failEvidence(path, `unsupported value type ${typeof value}`);
  }

  if (ancestors.has(value)) failEvidence(path, 'cyclic evidence is not admitted');
  ancestors.add(value);
  try {
    if (Array.isArray(value)) {
      const ownKeys = Reflect.ownKeys(value).filter(key => key !== 'length');
      if (ownKeys.some(key => typeof key !== 'string' || !/^(0|[1-9]\d*)$/.test(key))) {
        failEvidence(path, 'arrays may not contain symbol or named properties');
      }
      const admitted = [];
      for (let index = 0; index < value.length; index += 1) {
        if (!Object.hasOwn(value, index)) failEvidence(`${path}[${index}]`, 'sparse arrays are not admitted');
        const descriptor = Object.getOwnPropertyDescriptor(value, String(index));
        if (!descriptor?.enumerable || !('value' in descriptor)) {
          failEvidence(`${path}[${index}]`, 'array elements must be enumerable data properties');
        }
        admitted.push(admitEvidence(descriptor.value, `${path}[${index}]`, ancestors));
      }
      return admitted;
    }

    const prototype = Object.getPrototypeOf(value);
    if (prototype !== Object.prototype && prototype !== null) {
      failEvidence(path, 'only plain records and arrays are admitted');
    }

    const admitted = {};
    for (const key of Reflect.ownKeys(value).sort((left, right) => String(left).localeCompare(String(right)))) {
      if (typeof key !== 'string') failEvidence(path, 'symbol keys are not admitted');
      const descriptor = Object.getOwnPropertyDescriptor(value, key);
      if (!descriptor?.enumerable || !('value' in descriptor)) {
        failEvidence(`${path}.${key}`, 'properties must be enumerable data properties');
      }
      Object.defineProperty(admitted, key, {
        value: admitEvidence(descriptor.value, `${path}.${key}`, ancestors),
        enumerable: true,
        writable: true,
        configurable: true,
      });
    }
    return admitted;
  } finally {
    ancestors.delete(value);
  }
}

function canonicalBytes(value) {
  if (value === null) return 'N;';
  switch (typeof value) {
    case 'string':
      return `S${TEXT_ENCODER.encode(value).byteLength}:${value};`;
    case 'boolean':
      return value ? 'B1;' : 'B0;';
    case 'number':
      return `D${Object.is(value, -0) ? '-0' : String(value)};`;
    case 'bigint':
      return `I${value};`;
    case 'object':
      if (Array.isArray(value)) {
        return `A${value.length}[${value.map(canonicalBytes).join('')}]`;
      }
      {
        const keys = Object.keys(value).sort();
        return `O${keys.length}{${keys.map(key => `${canonicalBytes(key)}${canonicalBytes(value[key])}`).join('')}}`;
      }
    default:
      throw new TypeError(`Cannot canonically encode ${typeof value}`);
  }
}

function deepFreeze(value) {
  if (!value || typeof value !== 'object' || Object.isFrozen(value)) return value;
  for (const child of Object.values(value)) deepFreeze(child);
  return Object.freeze(value);
}

function invalidReceipt(index, reason, details = {}) {
  return { valid: false, index, reason, ...details };
}

export class DaemonEventStore {
  constructor(options = {}) {
    if (options && typeof options !== 'object') {
      throw new TypeError('options must be object or undefined');
    }
    this.logger = options.logger || console;
    this.eventLog = [];
    this.transitionLog = [];
    this.eventHashMap = new Map();
    this.freezeHistory = [];
    this.previousHash = '';
    this.universeState = new Map();
    this._lastTimestamp = 0n;
    this._initialized = false;
  }

  async initialize() {
    if (!this._initialized) {
      this.previousHash = await blake3('');
      this._initialized = true;
    }
  }

  _getNs() {
    return now();
  }

  _nextTimestamp() {
    const observed = this._getNs();
    this._lastTimestamp = observed <= this._lastTimestamp ? this._lastTimestamp + 1n : observed;
    return this._lastTimestamp;
  }

  async _hash(data) {
    return blake3(canonicalBytes(data));
  }

  _hashMaterial(entry) {
    return {
      schema: HASH_SCHEMA,
      id: entry.id,
      timestamp: entry.timestamp,
      operationType: entry.operationType,
      operationId: entry.operationId,
      status: entry.status,
      payload: entry.payload,
      metadata: entry.metadata,
      previousHash: entry.previousHash,
      previousEventId: entry.previousEventId,
    };
  }

  async _manufactureEntry(fields) {
    const candidate = {
      id: generateUUID(),
      timestamp: this._nextTimestamp(),
      operationType: fields.operationType,
      operationId: fields.operationId,
      status: fields.status,
      payload: admitEvidence(fields.payload ?? {}, 'payload'),
      metadata: admitEvidence(fields.metadata ?? {}, 'metadata'),
      previousHash: this.previousHash,
      previousEventId: fields.previousEventId ?? null,
    };
    candidate.currentHash = await this._hash(this._hashMaterial(candidate));
    validateEventLogEntry(candidate);
    return deepFreeze(candidate);
  }

  async appendEvent(operationType, payload = {}, metadata = {}) {
    await this.initialize();
    if (typeof operationType !== 'string' || !operationType.trim()) {
      throw new TypeError('operationType must be non-empty string');
    }
    if (payload !== null && typeof payload !== 'object') {
      throw new TypeError('payload must be object, null, or undefined');
    }
    if (metadata !== null && typeof metadata !== 'object') {
      throw new TypeError('metadata must be object, null, or undefined');
    }

    const entry = await this._manufactureEntry({
      operationType: operationType.trim(),
      operationId: generateUUID(),
      status: 'enqueued',
      payload: payload ?? {},
      metadata: metadata ?? {},
      previousEventId: null,
    });

    this.eventLog.push(entry);
    this.transitionLog.push(entry);
    this.eventHashMap.set(entry.operationId, entry);
    this.previousHash = entry.currentHash;
    this.logger.log(`[EventStore] Appended: ${entry.operationType} (${entry.operationId})`);
    return entry;
  }

  async updateEventStatus(operationId, status, result = null) {
    await this.initialize();
    if (typeof operationId !== 'string' || !operationId.trim()) {
      throw new TypeError('operationId must be non-empty string');
    }
    if (!['started', 'success', 'failure'].includes(status)) {
      throw new TypeError('status must be one of: started, success, failure');
    }

    const current = this.eventHashMap.get(operationId);
    if (!current) throw new Error(`Operation ${operationId} not found in event log`);
    if (TERMINAL_STATUSES.has(current.status)) {
      throw new Error(`Operation ${operationId} is terminal at status ${current.status}`);
    }
    if (current.status === 'started' && status === 'started') {
      throw new Error(`Operation ${operationId} is already started`);
    }

    const payload = admitEvidence(current.payload, 'payload');
    payload.result = admitEvidence(result, 'result');
    const next = await this._manufactureEntry({
      operationType: current.operationType,
      operationId,
      status,
      payload,
      metadata: current.metadata,
      previousEventId: current.id,
    });

    const currentIndex = this.eventLog.findIndex(entry => entry.operationId === operationId);
    if (currentIndex < 0) throw new Error(`Operation ${operationId} is missing from current state`);
    this.eventLog[currentIndex] = next;
    this.transitionLog.push(next);
    this.eventHashMap.set(operationId, next);
    this.previousHash = next.currentHash;
    this.logger.log(`[EventStore] Updated: ${operationId} -> ${status}`);
    return next;
  }

  async freezeUniverse() {
    await this.initialize();
    const freezeId = generateUUID();
    const timestamp = this._nextTimestamp();
    const freezeTimestampISO = toISO(timestamp);
    const currentLeafHashes = this.eventLog.map(entry => entry.currentHash);
    const transitionLeafHashes = this.transitionLog.map(entry => entry.currentHash);
    const previousFreeze = this.freezeHistory[this.freezeHistory.length - 1];
    const operations = this.eventLog.map(entry => ({
      operationId: entry.operationId,
      status: entry.status,
      operationType: entry.operationType,
    }));
    const snapshot = deepFreeze({
      freezeId,
      timestamp,
      freezeTimestampISO,
      eventCount: this.eventLog.length,
      transitionCount: this.transitionLog.length,
      stateHash: this.previousHash,
      merkleRoot: await buildMerkleTree(currentLeafHashes),
      transitionMerkleRoot: await buildMerkleTree(transitionLeafHashes),
      previousFreezeId: previousFreeze?.freezeId || null,
      operations,
    });

    validateUniverseFreeze(snapshot);
    this.freezeHistory.push(snapshot);
    this.universeState.set(freezeId, {
      snapshot,
      eventLog: [...this.eventLog],
      transitionLog: [...this.transitionLog],
    });
    this.logger.log(`[EventStore] Froze universe: ${freezeId} with ${this.eventLog.length} operations`);
    return snapshot;
  }

  async reconstructState(targetTimestamp) {
    await this.initialize();
    if (typeof targetTimestamp !== 'bigint') {
      throw new TypeError('targetTimestamp must be BigInt');
    }
    const transitions = this.transitionLog.filter(entry => entry.timestamp <= targetTimestamp);
    const latestByOperation = new Map();
    for (const entry of transitions) latestByOperation.set(entry.operationId, entry);
    const events = [...latestByOperation.values()];
    return {
      timestamp: targetTimestamp,
      timestampISO: toISO(targetTimestamp),
      eventCount: events.length,
      transitionCount: transitions.length,
      events,
      transitions,
      merkleRoot: await buildMerkleTree(events.map(entry => entry.currentHash)),
      transitionMerkleRoot: await buildMerkleTree(transitions.map(entry => entry.currentHash)),
      stateHash: transitions.at(-1)?.currentHash || await blake3(''),
    };
  }

  async queryEvents(query = {}) {
    validateTemporalQuery(query);
    let results = query.includeHistory ? this.transitionLog : this.eventLog;
    if (query.fromTimestamp) results = results.filter(entry => entry.timestamp >= query.fromTimestamp);
    if (query.toTimestamp) results = results.filter(entry => entry.timestamp <= query.toTimestamp);
    if (query.operationType) results = results.filter(entry => entry.operationType === query.operationType);
    if (query.operationId) results = results.filter(entry => entry.operationId === query.operationId);
    if (query.status) results = results.filter(entry => entry.status === query.status);
    return [...results].sort((left, right) => {
      const difference = left.timestamp - right.timestamp;
      return difference === 0n ? 0 : difference > 0n ? 1 : -1;
    });
  }

  async generateMerkleProof(eventIndex) {
    return this._generateProof(this.eventLog, eventIndex);
  }

  async generateTransitionProof(transitionIndex) {
    return this._generateProof(this.transitionLog, transitionIndex);
  }

  async _generateProof(entries, index) {
    if (!Number.isInteger(index) || index < 0) {
      throw new TypeError('eventIndex must be non-negative integer');
    }
    if (index >= entries.length) {
      throw new Error(`Event index ${index} out of range (max ${entries.length - 1})`);
    }
    const leafHashes = entries.map(entry => entry.currentHash);
    const proof = {
      leafIndex: index,
      leafCount: leafHashes.length,
      leafHash: leafHashes[index],
      proof: await getMerkleProofPath(leafHashes, index),
      merkleRoot: await buildMerkleTree(leafHashes),
    };
    validateMerkleProof(proof);
    return proof;
  }

  async verifyProof(proof) {
    validateMerkleProof(proof);
    return verifyMerkleProof(proof);
  }

  async verifyTransitionChain() {
    await this.initialize();
    let previousHash = await blake3('');
    let previousTimestamp = 0n;
    const latestByOperation = new Map();

    for (let index = 0; index < this.transitionLog.length; index += 1) {
      const entry = this.transitionLog[index];
      try {
        validateEventLogEntry(entry);
      } catch (error) {
        return invalidReceipt(index, 'ENTRY_INVALID', { message: error.message });
      }
      if (entry.timestamp <= previousTimestamp) {
        return invalidReceipt(index, 'TIMESTAMP_NOT_MONOTONIC', {
          previous: previousTimestamp,
          observed: entry.timestamp,
        });
      }
      if (entry.previousHash !== previousHash) {
        return invalidReceipt(index, 'PREVIOUS_HASH_MISMATCH', {
          expected: previousHash,
          observed: entry.previousHash,
        });
      }

      const priorOperationEntry = latestByOperation.get(entry.operationId);
      if (!priorOperationEntry) {
        if (entry.status !== 'enqueued') {
          return invalidReceipt(index, 'INITIAL_STATUS_MISMATCH', { observed: entry.status });
        }
        if (entry.previousEventId !== null) {
          return invalidReceipt(index, 'INITIAL_PREVIOUS_EVENT_ID_MISMATCH', {
            observed: entry.previousEventId,
          });
        }
      } else {
        if (entry.previousEventId !== priorOperationEntry.id) {
          return invalidReceipt(index, 'PREVIOUS_EVENT_ID_MISMATCH', {
            expected: priorOperationEntry.id,
            observed: entry.previousEventId,
          });
        }
        if (TERMINAL_STATUSES.has(priorOperationEntry.status)) {
          return invalidReceipt(index, 'POST_TERMINAL_TRANSITION', {
            previousStatus: priorOperationEntry.status,
            observed: entry.status,
          });
        }
        if (priorOperationEntry.status === 'started' && entry.status === 'started') {
          return invalidReceipt(index, 'DUPLICATE_STARTED_TRANSITION');
        }
      }

      const expected = await this._hash(this._hashMaterial(entry));
      if (entry.currentHash !== expected) {
        return invalidReceipt(index, 'CURRENT_HASH_MISMATCH', {
          expected,
          observed: entry.currentHash,
        });
      }

      previousHash = entry.currentHash;
      previousTimestamp = entry.timestamp;
      latestByOperation.set(entry.operationId, entry);
    }

    if (previousHash !== this.previousHash) {
      return invalidReceipt(this.transitionLog.length, 'HEAD_HASH_MISMATCH', {
        expected: previousHash,
        observed: this.previousHash,
      });
    }
    if (this.eventLog.length !== latestByOperation.size) {
      return invalidReceipt(this.transitionLog.length, 'CURRENT_VIEW_SIZE_MISMATCH', {
        expected: latestByOperation.size,
        observed: this.eventLog.length,
      });
    }

    const observedOperations = new Set();
    for (let index = 0; index < this.eventLog.length; index += 1) {
      const entry = this.eventLog[index];
      if (observedOperations.has(entry.operationId)) {
        return invalidReceipt(index, 'CURRENT_VIEW_DUPLICATE_OPERATION', {
          operationId: entry.operationId,
        });
      }
      observedOperations.add(entry.operationId);
      const expected = latestByOperation.get(entry.operationId);
      if (!expected || entry.id !== expected.id || entry.currentHash !== expected.currentHash) {
        return invalidReceipt(index, 'CURRENT_VIEW_MISMATCH', {
          operationId: entry.operationId,
          expectedId: expected?.id || null,
          observedId: entry.id,
        });
      }
    }

    if (this.eventHashMap.size !== latestByOperation.size) {
      return invalidReceipt(this.transitionLog.length, 'CURRENT_INDEX_SIZE_MISMATCH', {
        expected: latestByOperation.size,
        observed: this.eventHashMap.size,
      });
    }
    for (const [operationId, expected] of latestByOperation) {
      const observed = this.eventHashMap.get(operationId);
      if (!observed || observed.id !== expected.id || observed.currentHash !== expected.currentHash) {
        return invalidReceipt(this.transitionLog.length, 'CURRENT_INDEX_MISMATCH', {
          operationId,
          expectedId: expected.id,
          observedId: observed?.id || null,
        });
      }
    }

    return {
      valid: true,
      count: this.transitionLog.length,
      operationCount: latestByOperation.size,
      head: previousHash,
      reason: null,
    };
  }

  getStats() {
    return {
      eventCount: this.eventLog.length,
      transitionCount: this.transitionLog.length,
      freezeCount: this.freezeHistory.length,
      currentHash: this.previousHash,
      oldestEventTimestamp: this.transitionLog[0]?.timestamp || null,
      newestEventTimestamp: this.transitionLog[this.transitionLog.length - 1]?.timestamp || null,
    };
  }

  getFreezeHistory() {
    return [...this.freezeHistory];
  }
}
