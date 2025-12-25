import { describe, test, expect } from 'vitest';

import { freezeUniverse } from '../../src/freeze.mjs'
import { KGCStore } from '../../src/store.mjs'
import { GitBackbone } from '../../src/git.mjs'
import { reconstructState } from '../../src/freeze.mjs'
import { verifyReceipt } from '../../src/freeze.mjs'

describe('Doctests: freeze.mjs', () => {
  test('unknown example 1 (line 1)', async () => {
    const store = new KGCStore();
const git = new GitBackbone('/tmp/freeze-test');
const result = await freezeUniverse(store, git);
console.assert(result.id, 'Returns receipt with id');
  });

  test('deltaToQuad example 2 (line 164)', async () => {
    const reconstructed = await reconstructState(store, git, targetTime);
console.log('Reconstructed quad count:', reconstructed.size());
  });

  test('verifyReceipt example 3 (line 464)', async () => {
    const result = await verifyReceipt(freezeReceipt, git);
console.log('Valid:', result.valid);
  });
});
