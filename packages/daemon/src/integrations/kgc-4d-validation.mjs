/**
 * @file KGC-4D Validation Functions
 * @module @unrdf/daemon/integrations/kgc-4d-validation
 */

import { randomUUID } from 'node:crypto';

const HASH_PATTERN = /^[a-f0-9]{64}$/i;

export function validateEventLogEntry(entry) {
  if (!entry.id || typeof entry.id !== 'string') throw new TypeError('id must be string');
  if (typeof entry.timestamp !== 'bigint') throw new TypeError('timestamp must be bigint');
  if (!entry.operationType || typeof entry.operationType !== 'string') throw new TypeError('operationType must be string');
  if (!entry.operationId || typeof entry.operationId !== 'string') throw new TypeError('operationId must be string');
  if (!['enqueued', 'started', 'success', 'failure'].includes(entry.status)) throw new TypeError('invalid status');
  if (entry.payload && typeof entry.payload !== 'object') throw new TypeError('payload must be object');
  if (typeof entry.previousHash !== 'string' || !HASH_PATTERN.test(entry.previousHash)) {
    throw new TypeError('previousHash must be a 64-character hexadecimal hash');
  }
  if (typeof entry.currentHash !== 'string' || !HASH_PATTERN.test(entry.currentHash)) {
    throw new TypeError('currentHash must be a 64-character hexadecimal hash');
  }
  if (entry.previousEventId !== null && entry.previousEventId !== undefined && typeof entry.previousEventId !== 'string') {
    throw new TypeError('previousEventId must be string, null, or undefined');
  }
}

export function validateUniverseFreeze(snapshot) {
  if (!snapshot.freezeId || typeof snapshot.freezeId !== 'string') throw new TypeError('freezeId must be string');
  if (typeof snapshot.timestamp !== 'bigint') throw new TypeError('timestamp must be bigint');
  if (!snapshot.freezeTimestampISO || typeof snapshot.freezeTimestampISO !== 'string') throw new TypeError('freezeTimestampISO must be string');
  if (typeof snapshot.eventCount !== 'number' || snapshot.eventCount < 0) throw new TypeError('eventCount must be non-negative number');
  if (snapshot.transitionCount !== undefined && (!Number.isInteger(snapshot.transitionCount) || snapshot.transitionCount < snapshot.eventCount)) {
    throw new TypeError('transitionCount must be an integer not smaller than eventCount');
  }
  if (!snapshot.stateHash || typeof snapshot.stateHash !== 'string') throw new TypeError('stateHash must be string');
  if (!snapshot.merkleRoot || typeof snapshot.merkleRoot !== 'string') throw new TypeError('merkleRoot must be string');
  if (snapshot.transitionMerkleRoot !== undefined && typeof snapshot.transitionMerkleRoot !== 'string') {
    throw new TypeError('transitionMerkleRoot must be string');
  }
  if (!Array.isArray(snapshot.operations)) throw new TypeError('operations must be array');
}

export function validateMerkleProof(proof) {
  if (!proof || typeof proof !== 'object') throw new TypeError('proof must be object');
  if (!Number.isInteger(proof.leafCount) || proof.leafCount <= 0) {
    throw new TypeError('leafCount must be positive integer');
  }
  if (!Number.isInteger(proof.leafIndex) || proof.leafIndex < 0 || proof.leafIndex >= proof.leafCount) {
    throw new TypeError('leafIndex must identify a leaf within leafCount');
  }
  if (!proof.leafHash || typeof proof.leafHash !== 'string') throw new TypeError('leafHash must be string');
  if (!Array.isArray(proof.proof)) throw new TypeError('proof must be array');
  for (const step of proof.proof) {
    if (!step || typeof step.hash !== 'string' || !['left', 'right'].includes(step.position)) {
      throw new TypeError('proof steps must contain hash and left/right position');
    }
  }
  if (!proof.merkleRoot || typeof proof.merkleRoot !== 'string') throw new TypeError('merkleRoot must be string');
}

export function validateTemporalQuery(query) {
  if (query.fromTimestamp && typeof query.fromTimestamp !== 'bigint') throw new TypeError('fromTimestamp must be bigint or undefined');
  if (query.toTimestamp && typeof query.toTimestamp !== 'bigint') throw new TypeError('toTimestamp must be bigint or undefined');
  if (query.operationType && typeof query.operationType !== 'string') throw new TypeError('operationType must be string or undefined');
  if (query.operationId && typeof query.operationId !== 'string') throw new TypeError('operationId must be string or undefined');
  if (query.status && !['enqueued', 'started', 'success', 'failure'].includes(query.status)) throw new TypeError('invalid status');
  if (query.includeHistory !== undefined && typeof query.includeHistory !== 'boolean') {
    throw new TypeError('includeHistory must be boolean or undefined');
  }
}

export function generateUUID() {
  return randomUUID();
}
