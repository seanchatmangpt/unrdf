/**
 * Validators Tests - Ultra-fast
 * Basic validator smoke test only
 */

import { describe, it, expect } from 'vitest';
import { validateReceiptChainIntegrity } from '../src/validators.mjs';

describe('Validators', () => {
  it('should validate receipt chain', () => {
    const receipts = [
      { id: 'r1', hash: 'abc123', parentHash: null, timestamp: 1000 },
      { id: 'r2', hash: 'def456', parentHash: 'abc123', timestamp: 2000 },
    ];
    const isValid = validateReceiptChainIntegrity(receipts);
    expect(isValid).toBe(true);
  });
});
