import { describe, test, expect } from 'vitest';

import { GitBackbone } from '../../src/git.mjs'

describe('Doctests: git.mjs', () => {
  test('isNode example 1 (line 1)', async () => {
    const git = new GitBackbone('/tmp/kgc-repo');
const sha = await git.commitSnapshot('<s> <p> <o> <g> .', 'Test snapshot');
console.assert(typeof sha === 'string' && sha.length > 0, 'Returns commit hash');
  });
});
