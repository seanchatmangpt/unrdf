import { describe, test, expect } from 'vitest';

import { freezeUniverse } from '../../src/freeze.mjs'
import { KGCStore } from '../../src/store.mjs'
import { GitBackbone } from '../../src/git.mjs'

describe('Doctests: freeze.mjs', () => {
  test('unknown example 1 (line 1)', async () => {
    const store = new KGCStore();
const git = new GitBackbone('/tmp/freeze-test');
const result = await freezeUniverse(store, git);
console.assert(result.id, 'Returns receipt with id');
  });
});
