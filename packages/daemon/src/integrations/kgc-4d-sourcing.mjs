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

const HASH_SCHEMA = 'urn:unrdf:daemon:event-transition:v1';
const TERMINAL_STATUSES = new Set(['success', 'failure']);

function canonical(value) {
  if (typeof value === 'bigint') return value.toString();
  if (Array.isArray(value)) return value.map(canonical);
  if (value && typeof value === 'object') {
    return Object.fromEntries(Object.keys(value).sort().map(key => [key, canonical(value[key])]));
  }
  return value;
}

function clone(value) {
  return value === undefined ? undefined : structuredClone(value);
}

function deepFreeze(value) {
  if (!value || typeof value !== 'object' || Object.isFrozen(value)) return value;
  for (const child of Object.values(value)) deepFreeze(child);
  return Object.freeze(value);
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
    const previous = this.transitionLog.at(-1)?.timestamp;
    return previous !== undefined && observed <= previous ? previous + 1n : observed;
  }

  async _hash(data) {
    return blake3(JSON.stringify(canonical(data)));
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
      payload: clone(fields.payload) || {},
      metadata: clone(fields.metadata) || {},
      previousHash: this.previousHash,
      previousEventId: fields.previousEventId || null,
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
    if (payload && typeof payload !== 'object') {
      throw new TypeError('payload must be object or undefined');
    }
    if (metadata && typeof metadata !== 'object') {
      throw new TypeError('metadata must be object or undefined');
    }

    const entry = await this._manufactureEntry({
      operationType,
      operationId: generateUUID(),
      status: 'enqueued',
      payload,
      metadata,
      previousEventId: null,
    });

    this.eventLog.push(entry);
    this.transitionLog.push(entry);
    this.eventHashMap.set(entry.operationId, entry);
    this.previousHash = entry.currentHash;
    this.logger.log(`[EventStore] Appended: ${operationType} (${entry.operationId})`);
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

    const payload = clone(current.payload) || {};
    payload.result = clone(result);
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
    for (let index = 0; index < this.transitionLog.length; index += 1) {
      const entry = this.transitionLog[index];
      if (entry.previousHash !== previousHash) {
        return {
          valid: false,
          index,
          reason: 'PREVIOUS_HASH_MISMATCH',
          expected: previousHash,
          observed: entry.previousHash,
        };
      }
      const expected = await this._hash(this._hashMaterial(entry));
      if (entry.currentHash !== expected) {
        return {
          valid: false,
          index,
          reason: 'CURRENT_HASH_MISMATCH',
          expected,
          observed: entry.currentHash,
        };
      }
      previousHash = entry.currentHash;
    }
    return {
      valid: previousHash === this.previousHash,
      count: this.transitionLog.length,
      head: previousHash,
      reason: previousHash === this.previousHash ? null : 'HEAD_HASH_MISMATCH',
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
