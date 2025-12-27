import { describe, test, expect } from 'vitest';

import { now } from '../../src/time.mjs'
import { toISO } from '../../src/time.mjs'
import { fromISO } from '../../src/time.mjs'
import { addNanoseconds } from '../../src/time.mjs'
import { VectorClock } from '../../src/time.mjs'

describe('Doctests: time.mjs', () => {
  test('CLOCK_JUMP_THRESHOLD example 1 (line 1)', async () => {
    const t1 = now();
const t2 = now();
console.assert(typeof t1 === 'bigint', 'Returns BigInt');
console.assert(t1 < t2, 'Monotonic: second call returns larger value');
  });

  test('hasClockJumpDetected example 2 (line 61)', async () => {
    const ns = 1000000123456789n;  // 123.456789 milliseconds
const iso = toISO(ns);
console.assert(iso.includes('.123Z'), 'Milliseconds preserved');
console.assert(!iso.includes('456789'), 'Nanoseconds lost');
  });

  test('fromISO example 3 (line 104)', async () => {
    try {
fromISO('2025-02-31T00:00:00Z');  // February 31 doesn't exist
throw new Error('Should have thrown');
} catch (err) {
console.assert(err.message.includes('Invalid ISO'), 'Rejects invalid dates');
}
  });

  test('addNanoseconds example 4 (line 213)', async () => {
    try {
addNanoseconds(1000000000n, 500000000);  // Number instead of BigInt
throw new Error('Should have thrown');
} catch (err) {
console.assert(err.message.includes('BigInt'), 'Rejects non-BigInt');
}
  });

  test('duration example 5 (line 241)', async () => {
    const vc1 = new VectorClock('node1');
const vc2 = new VectorClock('node2');
vc1.increment();
vc2.increment();
// Merge incoming clock from node2
vc1.merge(vc2.toJSON());
// Now vc1 knows about node2's events AND has incremented its own counter
  });
});
