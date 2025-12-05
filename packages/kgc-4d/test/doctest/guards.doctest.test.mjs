import { describe, test, expect } from 'vitest';

import { guardMonotonicOrdering } from '../../src/guards.mjs'
import { guardTimeEnvironment } from '../../src/guards.mjs'
import { guardISOFormat } from '../../src/guards.mjs'
import { guardBigIntRange } from '../../src/guards.mjs'

describe('Doctests: guards.mjs', () => {
  test('result example 1 (line 1)', async () => {
    const result = guardMonotonicOrdering(100n, 99n);
console.assert(result === 100n, 'Forwards time unchanged');
const wrapped = guardMonotonicOrdering(99n, 100n);
console.assert(wrapped === 101n, 'Backwards time incremented');
  });

  test('guardTimeEnvironment example 2 (line 38)', async () => {
    const hasNodeEnv = guardTimeEnvironment();
console.assert(typeof hasNodeEnv === 'boolean', 'Returns boolean');
  });

  test('guardISOFormat example 3 (line 59)', async () => {
    guardISOFormat('2025-01-15T10:30:00.000Z');
try {
guardISOFormat('not-an-iso');
throw new Error('Should have thrown');
} catch (err) {
console.assert(err instanceof Error, 'Throws on invalid format');
}
  });

  test('guardBigIntRange example 4 (line 93)', async () => {
    guardBigIntRange(1000000000n);
try {
guardBigIntRange(-1n);
throw new Error('Should reject negative');
} catch (err) {
console.assert(err instanceof RangeError, 'Throws RangeError for invalid range');
}
  });
});
