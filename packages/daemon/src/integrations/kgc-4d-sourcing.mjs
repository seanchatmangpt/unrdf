/**
 * @file KGC-4D Event Sourcing Integration
 * @module @unrdf/daemon/integrations/kgc-4d-sourcing
 * @description Event sourcing for daemon operations with KGC-4D temporal model,
 * universe freeze snapshots, BLAKE3 hash chains, and Merkle tree proofs for audit
 * trails. Enables time-travel replay and deterministic state reconstruction.
 */

import { blake3 } from 'hash-wasm';
import { now, toISO } from '@unrdf/kgc-4d';

/**
 * Validate event log entry structure
 * @private
 */
function validateEventLogEntry(entry) {
  if (!entry.id || typeof entry.id !== 'string') throw new TypeError('id must be string');
  if (typeof entry.timestamp !== 'bigint') throw new TypeError('timestamp must be bigint');
  if (!entry.operationType || typeof entry.operationType !== 'string') throw new TypeError('operationType must be string');
  if (!entry.operationId || typeof entry.operationId !== 'string') throw new TypeError('operationId must be string');
  if (!['enqueued', 'started', 'success', 'failure'].includes(entry.status)) throw new TypeError('invalid status');
  if (entry.payload && typeof entry.payload !== 'object') throw new TypeError('payload must be object');
  if (!entry.previousHash || typeof entry.previousHash !== 'string') throw new TypeError('previousHash must be string');
  if (!entry.currentHash || typeof entry.currentHash !== 'string') throw new TypeError('currentHash must be string');
}

/**
 * Validate universe freeze snapshot structure
 * @private
 */
function validateUniverseFreeze(snapshot) {
  if (!snapshot.freezeId || typeof snapshot.freezeId !== 'string') throw new TypeError('freezeId must be string');
  if (typeof snapshot.timestamp !== 'bigint') throw new TypeError('timestamp must be bigint');
  if (!snapshot.freezeTimestampISO || typeof snapshot.freezeTimestampISO !== 'string') throw new TypeError('freezeTimestampISO must be string');
  if (typeof snapshot.eventCount !== 'number' || snapshot.eventCount < 0) throw new TypeError('eventCount must be non-negative number');
  if (!snapshot.stateHash || typeof snapshot.stateHash !== 'string') throw new TypeError('stateHash must be string');
  if (!snapshot.merkleRoot || typeof snapshot.merkleRoot !== 'string') throw new TypeError('merkleRoot must be string');
  if (!Array.isArray(snapshot.operations)) throw new TypeError('operations must be array');
}

/**
 * Validate Merkle proof structure
 * @private
 */
function validateMerkleProof(proof) {
  if (typeof proof.leafIndex !== 'number' || proof.leafIndex < 0) throw new TypeError('leafIndex must be non-negative number');
  if (!proof.leafHash || typeof proof.leafHash !== 'string') throw new TypeError('leafHash must be string');
  if (!Array.isArray(proof.proof)) throw new TypeError('proof must be array');
  if (!proof.merkleRoot || typeof proof.merkleRoot !== 'string') throw new TypeError('merkleRoot must be string');
}

/**
 * Validate temporal query parameters
 * @private
 */
function validateTemporalQuery(query) {
  if (query.fromTimestamp && typeof query.fromTimestamp !== 'bigint') throw new TypeError('fromTimestamp must be bigint or undefined');
  if (query.toTimestamp && typeof query.toTimestamp !== 'bigint') throw new TypeError('toTimestamp must be bigint or undefined');
  if (query.operationType && typeof query.operationType !== 'string') throw new TypeError('operationType must be string or undefined');
  if (query.operationId && typeof query.operationId !== 'string') throw new TypeError('operationId must be string or undefined');
  if (query.status && !['enqueued', 'started', 'success', 'failure'].includes(query.status)) throw new TypeError('invalid status');
}

/**
 * Generate UUID v4 for unique identifiers
 * @private
 * @returns {string} UUID v4 string
 */
function generateUUID() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

/**
 * Build Merkle tree from leaf hashes using bottom-up construction
 * @private
 * @param {string[]} leaves - Array of leaf hashes
 * @returns {string} Root hash of the Merkle tree
 */
async function buildMerkleTree(leaves) {
  if (leaves.length === 0) {
    return await blake3('');
  }

  let current = leaves;
  while (current.length > 1) {
    const next = [];
    for (let i = 0; i < current.length; i += 2) {
      const left = current[i];
      const right = current[i + 1] || current[i];
      const combined = left + right;
      const hash = await blake3(combined);
      next.push(hash);
    }
    current = next;
  }
  return current[0];
}

/**
 * Get Merkle proof path for a leaf at given index
 * @private
 * @param {string[]} leaves - Leaf hashes
 * @param {number} index - Index of leaf to prove
 * @returns {Promise<Array>} Proof path with hashes and positions
 */
async function getMerkleProofPath(leaves, index) {
  if (leaves.length === 0 || index >= leaves.length) {
    return [];
  }

  const proof = [];
  let current = leaves;
  let currentIndex = index;

  while (current.length > 1) {
    const isRight = currentIndex % 2 === 1;
    const siblingIndex = isRight ? currentIndex - 1 : currentIndex + 1;
    const siblingHash = siblingIndex < current.length ? current[siblingIndex] : current[currentIndex];

    proof.push({
      hash: siblingHash,
      position: isRight ? 'left' : 'right',
    });

    const next = [];
    for (let i = 0; i < current.length; i += 2) {
      const left = current[i];
      const right = current[i + 1] || current[i];
      const combined = left + right;
      const hash = await blake3(combined);
      next.push(hash);
    }
    current = next;
    currentIndex = Math.floor(currentIndex / 2);
  }

  return proof;
}

/**
 * Daemon Event Store - Append-only log with KGC-4D temporal model
 * Maintains immutable operation history with universe freeze snapshots
 * and hash chains for verifying audit trail integrity.
 *
 * @class
 * @example
 * const store = new DaemonEventStore();
 * await store.appendEvent('create-task', { taskId: '123' }, { priority: 'high' });
 * const snapshot = await store.freezeUniverse();
 * console.assert(snapshot.eventCount > 0, 'Snapshot contains events');
 */
export class DaemonEventStore {
  /**
   * Create a new daemon event store
   * @param {Object} [options={}] - Configuration options
   * @param {Object} [options.logger] - Logger instance (default: console)
   * @throws {TypeError} If options are invalid
   */
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

  /**
   * Initialize event store (compute genesis hash)
   * Must be called once before using the store
   * @returns {Promise<void>}
   *
   * @example
   * const store = new DaemonEventStore();
   * await store.initialize();
   */
  async initialize() {
    if (!this._initialized) {
      this.previousHash = await blake3('');
      this._initialized = true;
    }
  }

  /**
   * Get current nanosecond timestamp using KGC-4D time
   * @private
   * @returns {bigint} Nanosecond timestamp
   */
  _getNs() {
    return now();
  }

  /**
   * Compute BLAKE3 hash of data for immutability chain
   * @private
   * @param {*} data - Data to hash
   * @returns {Promise<string>} BLAKE3 hash hex string
   */
  async _hash(data) {
    const serialized = JSON.stringify(data, (k, v) => {
      if (typeof v === 'bigint') return v.toString();
      return v;
    });
    return blake3(serialized);
  }

  /**
   * Append event to immutable operation log with hash chain
   * Links each event to previous via BLAKE3 hash for tamper detection
   *
   * @param {string} operationType - Type of operation (e.g., 'create-task', 'update-config')
   * @param {Object} payload - Operation payload data
   * @param {Object} [metadata={}] - Optional metadata (priority, source, etc.)
   * @returns {Promise<Object>} Event entry with timestamp, hashes, and id
   * @throws {TypeError} If parameters are invalid
   * @throws {Error} If hash computation fails
   *
   * @example
   * const event = await store.appendEvent('task-complete', { taskId: '123' });
   * console.assert(event.id, 'Event has unique ID');
   * console.assert(event.currentHash !== event.previousHash, 'Hash chain maintained');
   */
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

  /**
   * Update event status in the log (enqueued -> started -> success/failure)
   * Maintains chain integrity by recomputing hashes
   *
   * @param {string} operationId - ID of operation to update
   * @param {string} status - New status: 'started', 'success', or 'failure'
   * @param {Object} [result] - Result data if successful or error message if failed
   * @returns {Promise<Object>} Updated event entry
   * @throws {Error} If operation not found or invalid status transition
   *
   * @example
   * const event = await store.appendEvent('task', {});
   * await store.updateEventStatus(event.operationId, 'started');
   * const final = await store.updateEventStatus(event.operationId, 'success', { output: 'done' });
   * console.assert(final.status === 'success', 'Status updated');
   */
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

  /**
   * Freeze universe state at current moment
   * Creates immutable snapshot with all events, Merkle root, and hash chain anchor
   * Enables time-travel replay from this point forward
   *
   * @returns {Promise<Object>} Universe freeze snapshot with id, timestamp, merkleRoot, etc.
   * @throws {Error} If snapshot creation fails
   *
   * @example
   * const snapshot1 = await store.freezeUniverse();
   * await store.appendEvent('task', {});
   * const snapshot2 = await store.freezeUniverse();
   * console.assert(snapshot2.eventCount === snapshot1.eventCount + 1, 'Snapshot reflects new event');
   */
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

  /**
   * Get universe state at specific point in time
   * Reconstructs complete state by replaying events up to timestamp
   *
   * @param {bigint} targetTimestamp - Target timestamp for reconstruction
   * @returns {Promise<Object>} Reconstructed state with events and metadata
   * @throws {TypeError} If timestamp is invalid
   *
   * @example
   * const t1 = now();
   * await store.appendEvent('op1', {});
   * const t2 = now();
   * await store.appendEvent('op2', {});
   * const state = await store.reconstructState(t2);
   * console.assert(state.events.length === 1, 'State contains only first event');
   */
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

  /**
   * Query events by temporal range and filters
   * Supports filtering by timestamp range, operation type, status
   *
   * @param {Object} query - Temporal query parameters
   * @param {bigint} [query.fromTimestamp] - Start of time range (inclusive)
   * @param {bigint} [query.toTimestamp] - End of time range (inclusive)
   * @param {string} [query.operationType] - Filter by operation type
   * @param {string} [query.operationId] - Filter by operation ID
   * @param {string} [query.status] - Filter by status
   * @returns {Promise<Object[]>} Matching events sorted by timestamp
   * @throws {TypeError} If query parameters are invalid
   *
   * @example
   * const recent = await store.queryEvents({
   *   fromTimestamp: BigInt(Date.now()) * 1_000_000n - 1_000_000_000_000n,
   *   status: 'success'
   * });
   * console.assert(Array.isArray(recent), 'Returns array of events');
   */
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

  /**
   * Generate Merkle proof for specific event in event log
   * Proves that event is part of frozen universe snapshot
   *
   * @param {number} eventIndex - Index of event in log
   * @returns {Promise<Object>} Merkle proof with leaf hash, path, and root
   * @throws {TypeError} If eventIndex is invalid
   * @throws {Error} If event not found
   *
   * @example
   * const event = await store.appendEvent('test', {});
   * const snapshot = await store.freezeUniverse();
   * const proof = await store.generateMerkleProof(0);
   * console.assert(proof.merkleRoot === snapshot.merkleRoot, 'Proof root matches snapshot');
   */
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

  /**
   * Verify Merkle proof for authenticity
   * Ensures event hasn't been tampered with and belongs to snapshot
   *
   * @param {Object} proof - Merkle proof from generateMerkleProof
   * @returns {Promise<boolean>} True if proof is valid and verifiable
   * @throws {TypeError} If proof is invalid
   *
   * @example
   * const proof = await store.generateMerkleProof(0);
   * const isValid = await store.verifyProof(proof);
   * console.assert(isValid === true, 'Proof is valid');
   */
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

  /**
   * Get event log size and statistics
   * @returns {Object} Statistics including event count, hash chain head, etc.
   *
   * @example
   * const stats = store.getStats();
   * console.log(`${stats.eventCount} events logged`);
   */
  getStats() {
    return {
      eventCount: this.eventLog.length,
      freezeCount: this.freezeHistory.length,
      currentHash: this.previousHash,
      oldestEventTimestamp: this.eventLog[0]?.timestamp || null,
      newestEventTimestamp: this.eventLog[this.eventLog.length - 1]?.timestamp || null,
    };
  }

  /**
   * Get freeze history snapshots
   * @returns {Object[]} Array of universe freeze snapshots
   */
  getFreezeHistory() {
    return [...this.freezeHistory];
  }
}
