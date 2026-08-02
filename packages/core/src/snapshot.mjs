/** Deterministic state snapshots and structural diffs. */
import { hashCanonical, canonicalizeJSON } from './receipt-chain.mjs';

export function createSnapshot(subject, state, metadata = {}) {
  if (!subject) throw new TypeError('subject is required');
  const body = canonicalizeJSON({ schema: 'unrdf.snapshot/1', subject, state, metadata });
  return Object.freeze({ ...body, digest: hashCanonical(body) });
}

export function verifySnapshot(snapshot) {
  const { digest, ...body } = snapshot;
  const expected = hashCanonical(body);
  return { valid: digest === expected, expected, actual: digest };
}

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
