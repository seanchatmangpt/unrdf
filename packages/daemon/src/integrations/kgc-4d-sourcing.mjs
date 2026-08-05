/**
 * @file KGC-4D Event Sourcing Integration
 * @module @unrdf/daemon/integrations/kgc-4d-sourcing
 * @description Event sourcing for daemon operations with KGC-4D temporal model
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

export class DaemonEventStore {
  constructor(options = {}) {
    if (options && typeof options !== 'object') {
      throw new TypeError('options must be object or undefined');
    }
    this.logger = options.logger || console;
    this.eventLog = [];
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

  async _hash(data) {
    const serialized = JSON.stringify(data, (key, value) => {
      if (typeof value === 'bigint') return value.toString();
      return value;
    });
    return blake3(serialized);
  }

  async appendEvent(operationType, payload = {}, metadata = {}) {
    if (typeof operationType !== 'string' || !operationType.trim()) {
      throw new TypeError('operationType must be non-empty string');
    }
    if (payload && typeof payload !== 'object') {
      throw new TypeError('payload must be object or undefined');
    }
    if (metadata && typeof metadata !== 'object') {
      throw new TypeError('metadata must be object or undefined');
    }

    const timestamp = this._getNs();
    const operationId = generateUUID();
    const previousHash = this.previousHash;
    const eventData = {
      operationType,
      operationId,
      timestamp: timestamp.toString(),
      payload,
      status: 'enqueued',
    };
    const currentHash = await this._hash(eventData);
    const entry = {
      id: generateUUID(),
      timestamp,
      operationType,
      operationId,
      status: 'enqueued',
      payload,
      previousHash,
      currentHash,
      metadata: metadata || {},
    };

    validateEventLogEntry(entry);
    this.eventLog.push(entry);
    this.eventHashMap.set(entry.operationId, entry);
    this.previousHash = currentHash;
    this.logger.log(`[EventStore] Appended: ${operationType} (${operationId})`);
    return entry;
  }

  async updateEventStatus(operationId, status, result = null) {
    if (typeof operationId !== 'string' || !operationId.trim()) {
      throw new TypeError('operationId must be non-empty string');
    }
    if (!['started', 'success', 'failure'].includes(status)) {
      throw new TypeError('status must be one of: started, success, failure');
    }

    const entry = this.eventHashMap.get(operationId);
    if (!entry) throw new Error(`Operation ${operationId} not found in event log`);

    const previousHash = this.previousHash;
    entry.status = status;
    entry.payload.result = result;
    entry.timestamp = this._getNs();
    const eventData = {
      operationType: entry.operationType,
      operationId,
      timestamp: entry.timestamp.toString(),
      payload: entry.payload,
      status,
    };
    const currentHash = await this._hash(eventData);
    entry.previousHash = previousHash;
    entry.currentHash = currentHash;

    validateEventLogEntry(entry);
    this.previousHash = currentHash;
    this.logger.log(`[EventStore] Updated: ${operationId} -> ${status}`);
    return entry;
  }

  async freezeUniverse() {
    const freezeId = generateUUID();
    const timestamp = this._getNs();
    const freezeTimestampISO = toISO(timestamp);
    const leafHashes = this.eventLog.map(entry => entry.currentHash);
    const merkleRoot = await buildMerkleTree(leafHashes);
    const previousFreeze = this.freezeHistory[this.freezeHistory.length - 1];
    const operations = this.eventLog.map(entry => ({
      operationId: entry.operationId,
      status: entry.status,
      operationType: entry.operationType,
    }));
    const snapshot = {
      freezeId,
      timestamp,
      freezeTimestampISO,
      eventCount: this.eventLog.length,
      stateHash: this.previousHash,
      merkleRoot,
      previousFreezeId: previousFreeze?.freezeId || null,
      operations,
    };

    validateUniverseFreeze(snapshot);
    this.freezeHistory.push(snapshot);
    this.universeState.set(freezeId, { snapshot, eventLog: [...this.eventLog] });
    this.logger.log(`[EventStore] Froze universe: ${freezeId} with ${this.eventLog.length} events`);
    return snapshot;
  }

  async reconstructState(targetTimestamp) {
    if (typeof targetTimestamp !== 'bigint') {
      throw new TypeError('targetTimestamp must be BigInt');
    }
    const events = this.eventLog.filter(entry => entry.timestamp <= targetTimestamp);
    const merkleRoot = await buildMerkleTree(events.map(entry => entry.currentHash));
    const stateHash = events.length > 0
      ? events[events.length - 1].currentHash
      : await blake3('');
    return {
      timestamp: targetTimestamp,
      timestampISO: toISO(targetTimestamp),
      eventCount: events.length,
      events,
      merkleRoot,
      stateHash,
    };
  }

  async queryEvents(query = {}) {
    validateTemporalQuery(query);
    let results = this.eventLog;
    if (query.fromTimestamp) results = results.filter(entry => entry.timestamp >= query.fromTimestamp);
    if (query.toTimestamp) results = results.filter(entry => entry.timestamp <= query.toTimestamp);
    if (query.operationType) results = results.filter(entry => entry.operationType === query.operationType);
    if (query.operationId) results = results.filter(entry => entry.operationId === query.operationId);
    if (query.status) results = results.filter(entry => entry.status === query.status);
    return results.sort((left, right) => {
      const difference = left.timestamp - right.timestamp;
      return difference === 0n ? 0 : difference > 0n ? 1 : -1;
    });
  }

  async generateMerkleProof(eventIndex) {
    if (!Number.isInteger(eventIndex) || eventIndex < 0) {
      throw new TypeError('eventIndex must be non-negative integer');
    }
    if (eventIndex >= this.eventLog.length) {
      throw new Error(`Event index ${eventIndex} out of range (max ${this.eventLog.length - 1})`);
    }

    const leafHashes = this.eventLog.map(entry => entry.currentHash);
    const proof = {
      leafIndex: eventIndex,
      leafCount: leafHashes.length,
      leafHash: leafHashes[eventIndex],
      proof: await getMerkleProofPath(leafHashes, eventIndex),
      merkleRoot: await buildMerkleTree(leafHashes),
    };
    validateMerkleProof(proof);
    return proof;
  }

  async verifyProof(proof) {
    validateMerkleProof(proof);
    return verifyMerkleProof(proof);
  }

  getStats() {
    return {
      eventCount: this.eventLog.length,
      freezeCount: this.freezeHistory.length,
      currentHash: this.previousHash,
      oldestEventTimestamp: this.eventLog[0]?.timestamp || null,
      newestEventTimestamp: this.eventLog[this.eventLog.length - 1]?.timestamp || null,
    };
  }

  getFreezeHistory() {
    return [...this.freezeHistory];
  }
}
