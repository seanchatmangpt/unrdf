/**
 * KGC Time Tests - Ultra-fast
 * Time basics smoke test only
 */

import { describe, it, expect } from 'vitest';
import { now } from '../src/time.mjs';

describe('KGC Time', () => {
  it('should return BigInt timestamp', () => {
    const timestamp = now();
    expect(typeof timestamp).toBe('bigint');
    expect(timestamp > 0n).toBe(true);
  });
});
