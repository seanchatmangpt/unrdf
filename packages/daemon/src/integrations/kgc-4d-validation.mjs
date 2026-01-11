/**
 * @file KGC-4D Validation Functions
 * @module @unrdf/daemon/integrations/kgc-4d-validation
 * @description Validation functions for event log entries and related structures
 */

export function validateEventLogEntry(entry) {
  if (!entry.id || typeof entry.id !== 'string') throw new TypeError('id must be string');
  if (typeof entry.timestamp !== 'bigint') throw new TypeError('timestamp must be bigint');
  if (!entry.operationType || typeof entry.operationType !== 'string') throw new TypeError('operationType must be string');
  if (!entry.operationId || typeof entry.operationId !== 'string') throw new TypeError('operationId must be string');
  if (!['enqueued', 'started', 'success', 'failure'].includes(entry.status)) throw new TypeError('invalid status');
  if (entry.payload && typeof entry.payload !== 'object') throw new TypeError('payload must be object');
  if (!entry.previousHash || typeof entry.previousHash !== 'string') throw new TypeError('previousHash must be string');
  if (!entry.currentHash || typeof entry.currentHash !== 'string') throw new TypeError('currentHash must be string');
}

export function validateUniverseFreeze(snapshot) {
  if (!snapshot.freezeId || typeof snapshot.freezeId !== 'string') throw new TypeError('freezeId must be string');
  if (typeof snapshot.timestamp !== 'bigint') throw new TypeError('timestamp must be bigint');
  if (!snapshot.freezeTimestampISO || typeof snapshot.freezeTimestampISO !== 'string') throw new TypeError('freezeTimestampISO must be string');
  if (typeof snapshot.eventCount !== 'number' || snapshot.eventCount < 0) throw new TypeError('eventCount must be non-negative number');
  if (!snapshot.stateHash || typeof snapshot.stateHash !== 'string') throw new TypeError('stateHash must be string');
  if (!snapshot.merkleRoot || typeof snapshot.merkleRoot !== 'string') throw new TypeError('merkleRoot must be string');
  if (!Array.isArray(snapshot.operations)) throw new TypeError('operations must be array');
}

export function validateMerkleProof(proof) {
  if (typeof proof.leafIndex !== 'number' || proof.leafIndex < 0) throw new TypeError('leafIndex must be non-negative number');
  if (!proof.leafHash || typeof proof.leafHash !== 'string') throw new TypeError('leafHash must be string');
  if (!Array.isArray(proof.proof)) throw new TypeError('proof must be array');
  if (!proof.merkleRoot || typeof proof.merkleRoot !== 'string') throw new TypeError('merkleRoot must be string');
}

export function validateTemporalQuery(query) {
  if (query.fromTimestamp && typeof query.fromTimestamp !== 'bigint') throw new TypeError('fromTimestamp must be bigint or undefined');
  if (query.toTimestamp && typeof query.toTimestamp !== 'bigint') throw new TypeError('toTimestamp must be bigint or undefined');
  if (query.operationType && typeof query.operationType !== 'string') throw new TypeError('operationType must be string or undefined');
  if (query.operationId && typeof query.operationId !== 'string') throw new TypeError('operationId must be string or undefined');
  if (query.status && !['enqueued', 'started', 'success', 'failure'].includes(query.status)) throw new TypeError('invalid status');
}

export function generateUUID() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}
