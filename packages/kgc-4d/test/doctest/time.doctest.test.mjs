import { describe, test, expect } from 'vitest';

import { now } from '../../src/time.mjs'
import { toISO } from '../../src/time.mjs'
import { fromISO } from '../../src/time.mjs'

describe('Doctests: time.mjs', () => {
  test('t1 example 1 (line 1)', async () => {
    const t1 = now();
const t2 = now();
console.assert(typeof t1 === 'bigint', 'Returns BigInt');
console.assert(t1 < t2, 'Monotonic: second call returns larger value');
  });

  test('toISO example 2 (line 39)', async () => {
    try {
toISO(123);  // Not a BigInt
throw new Error('Should have thrown TypeError');
} catch (err) {
console.assert(err instanceof TypeError, 'Throws on non-BigInt');
}
  });

  test('fromISO example 3 (line 64)', async () => {
    try {
fromISO('not-a-date');
throw new Error('Should have thrown');
} catch (err) {
console.assert(err.message.includes('Invalid ISO'), 'Clear error message');
}
  });
});
