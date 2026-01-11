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
import { buildMerkleTree, getMerkleProofPath } from './kgc-4d-merkle.mjs';

/**
 * Daemon Event Store - Append-only log with KGC-4D temporal model
 * @class
 */
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
    const serialized = JSON.stringify(data, (k, v) => {
      if (typeof v === 'bigint') return v.toString();
      return v;
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
    if (!entry) {
      throw new Error(`Operation ${operationId} not found in event log`);
    }

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

    const leafHashes = [];
    for (const entry of this.eventLog) {
      leafHashes.push(entry.currentHash);
    }

    const merkleRoot = await buildMerkleTree(leafHashes);
    const previousFreeze = this.freezeHistory[this.freezeHistory.length - 1];

    const operations = this.eventLog.map((e) => ({
      operationId: e.operationId,
      status: e.status,
      operationType: e.operationType,
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
    this.universeState.set(freezeId, {
      snapshot,
      eventLog: [...this.eventLog],
    });

    this.logger.log(`[EventStore] Froze universe: ${freezeId} with ${this.eventLog.length} events`);
    return snapshot;
  }

  async reconstructState(targetTimestamp) {
    if (typeof targetTimestamp !== 'bigint') {
      throw new TypeError('targetTimestamp must be BigInt');
    }

    const events = this.eventLog.filter((e) => e.timestamp <= targetTimestamp);
    const leafHashes = events.map((e) => e.currentHash);
    const merkleRoot = await buildMerkleTree(leafHashes);

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

    if (query.fromTimestamp) {
      results = results.filter((e) => e.timestamp >= query.fromTimestamp);
    }
    if (query.toTimestamp) {
      results = results.filter((e) => e.timestamp <= query.toTimestamp);
    }
    if (query.operationType) {
      results = results.filter((e) => e.operationType === query.operationType);
    }
    if (query.operationId) {
      results = results.filter((e) => e.operationId === query.operationId);
    }
    if (query.status) {
      results = results.filter((e) => e.status === query.status);
    }

    return results.sort((a, b) => {
      const diff = a.timestamp - b.timestamp;
      return diff === 0n ? 0 : diff > 0n ? 1 : -1;
    });
  }

  async generateMerkleProof(eventIndex) {
    if (typeof eventIndex !== 'number' || eventIndex < 0 || !Number.isInteger(eventIndex)) {
      throw new TypeError('eventIndex must be non-negative integer');
    }
    if (eventIndex >= this.eventLog.length) {
      throw new Error(`Event index ${eventIndex} out of range (max ${this.eventLog.length - 1})`);
    }

    const leafHashes = this.eventLog.map((e) => e.currentHash);
    const leafHash = leafHashes[eventIndex];
    const proofPath = await getMerkleProofPath(leafHashes, eventIndex);
    const merkleRoot = await buildMerkleTree(leafHashes);

    const proof = {
      leafIndex: eventIndex,
      leafHash,
      proof: proofPath,
      merkleRoot,
    };

    validateMerkleProof(proof);
    return proof;
  }

  async verifyProof(proof) {
    validateMerkleProof(proof);

    let currentHash = proof.leafHash;
    for (const step of proof.proof) {
      const combined = step.position === 'left'
        ? step.hash + currentHash
        : currentHash + step.hash;
      currentHash = await blake3(combined);
    }

    return currentHash === proof.merkleRoot;
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
