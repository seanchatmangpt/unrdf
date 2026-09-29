/** Deterministic state snapshots and structural diffs. */
import { hashCanonical, canonicalizeJSON } from './receipt-chain.mjs';

/**
 * Create a frozen, digested snapshot of a state.
 * @param {string} subject - Subject the state belongs to.
 * @param {*} state - State to capture.
 * @param {Object} [metadata] - Metadata to include.
 * @returns {Object} Frozen snapshot with a digest.
 * @throws {TypeError} If subject is missing.
 */
export function createSnapshot(subject, state, metadata = {}) {
  if (!subject) throw new TypeError('subject is required');
  const body = canonicalizeJSON({ schema: 'unrdf.snapshot/1', subject, state, metadata });
  return Object.freeze({ ...body, digest: hashCanonical(body) });
}

/**
 * Check that a snapshot's digest matches its content.
 * @param {Object} snapshot - Snapshot with a digest.
 * @returns {{valid: boolean, expected: string, actual: string}} Verification result.
 */
export function verifySnapshot(snapshot) {
  const { digest, ...body } = snapshot;
  const expected = hashCanonical(body);
  return { valid: digest === expected, expected, actual: digest };
}

/**
 * Structurally diff the states of two snapshots of the same subject.
 * @param {Object} before - Earlier snapshot.
 * @param {Object} after - Later snapshot.
 * @returns {{subject: string, before: string, after: string, changes: Array<{path: string, before: *, after: *}>, changed: boolean}} Diff result.
 * @throws {Error} If the subjects differ.
 */
export function diffSnapshots(before, after) {
  if (before.subject !== after.subject) throw new Error('SNAPSHOT_SUBJECT_MISMATCH');
  const changes = [];
  const walk = (left, right, path = '$') => {
    if (Object.is(left, right)) return;
    if (!left || !right || typeof left !== 'object' || typeof right !== 'object' || Array.isArray(left) !== Array.isArray(right)) {
      changes.push({ path, before: left, after: right });
      return;
    }
    const keys = new Set([...Object.keys(left), ...Object.keys(right)]);
    for (const key of [...keys].sort()) walk(left[key], right[key], Array.isArray(left) ? `${path}[${key}]` : `${path}.${key}`);
  };
  walk(before.state, after.state);
  return { subject: before.subject, before: before.digest, after: after.digest, changes, changed: changes.length > 0 };
}
