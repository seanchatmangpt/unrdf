import test from 'node:test';
import assert from 'node:assert/strict';
import {
  assignShards,
  dependentsClosure,
  isGlobalChange,
  parseShard,
  selectAffected,
} from './affected.mjs';

const packages = [
  { name: '@u/core', path: 'packages/core', deps: [] },
  { name: '@u/hooks', path: 'packages/hooks', deps: ['@u/core'] },
  { name: '@u/cli', path: 'packages/cli', deps: ['@u/hooks'] },
  { name: '@u/docs', path: 'packages/docs', deps: [] },
  { name: '@u/core-extra', path: 'packages/core/extra', deps: [] },
];

test('a changed package selects itself and its transitive dependents', () => {
  const r = selectAffected({ packages, changedFiles: ['packages/core/src/a.mjs'] });
  assert.equal(r.all, false);
  assert.deepEqual(r.changed, ['@u/core']);
  assert.deepEqual(r.selected, ['@u/cli', '@u/core', '@u/hooks']);
});

test('leaf changes do not select their dependencies', () => {
  const r = selectAffected({ packages, changedFiles: ['packages/cli/src/x.mjs'] });
  assert.deepEqual(r.selected, ['@u/cli']);
});

test('longest path prefix decides ownership of nested packages', () => {
  const r = selectAffected({ packages, changedFiles: ['packages/core/extra/index.mjs'] });
  assert.deepEqual(r.changed, ['@u/core-extra']);
  assert.ok(!r.selected.includes('@u/core'));
});

test('a path that merely shares a name prefix is not owned by the package', () => {
  const r = selectAffected({ packages, changedFiles: ['packages/core-other/x.mjs'] });
  assert.deepEqual(r.changed, []);
});

test('docs-only and unrelated files select nothing', () => {
  const r = selectAffected({ packages, changedFiles: ['README.md', 'docs/guide.md'] });
  assert.equal(r.all, false);
  assert.deepEqual(r.selected, []);
  assert.equal(r.reason, 'no package files changed');
});

test('global files select every package', () => {
  for (const file of [
    'pnpm-lock.yaml',
    'package.json',
    '.github/actions/setup/action.yml',
    '.github/workflows/ci.yml',
    'scripts/verify-workspace-packages.mjs',
    'scripts/ci/affected.mjs',
    'vitest.config.mjs',
    'eslint.config.mjs',
    'tsconfig.base.json',
  ]) {
    assert.equal(isGlobalChange(file), true, file);
    const r = selectAffected({ packages, changedFiles: [file, 'packages/docs/a.mjs'] });
    assert.equal(r.all, true, file);
    assert.equal(r.selected.length, packages.length);
  }
});

test('package-level manifests are not global', () => {
  assert.equal(isGlobalChange('packages/core/package.json'), false);
  assert.equal(isGlobalChange('packages/core/vitest.config.mjs'), false);
});

test('an unavailable change list falls back to everything', () => {
  const r = selectAffected({ packages, changedFiles: null });
  assert.equal(r.all, true);
  assert.equal(r.selected.length, packages.length);
});

test('dependents closure handles cycles and diamonds', () => {
  const cyc = [
    { name: 'a', path: 'a', deps: ['b'] },
    { name: 'b', path: 'b', deps: ['a'] },
    { name: 'c', path: 'c', deps: ['a', 'b'] },
  ];
  assert.deepEqual([...dependentsClosure(cyc, ['a'])].sort(), ['a', 'b', 'c']);
});

test('shard assignment is complete, disjoint, stable and balanced', () => {
  const names = Array.from({ length: 40 }, (_, i) => `p${i}`);
  const weights = Object.fromEntries(names.map((n, i) => [n, 1 + ((i * 37) % 50)]));
  const a = assignShards(names, weights, 3);
  const b = assignShards([...names].reverse(), weights, 3);
  assert.deepEqual(a, b, 'input order must not matter');
  const flat = a.flat();
  assert.equal(flat.length, names.length);
  assert.equal(new Set(flat).size, names.length);
  const loads = a.map(s => s.reduce((t, n) => t + weights[n], 0));
  const spread = Math.max(...loads) - Math.min(...loads);
  assert.ok(spread <= Math.max(...Object.values(weights)), `spread ${spread} exceeds one item`);
});

test('unknown packages get a default weight instead of failing', () => {
  const shards = assignShards(['x', 'y', 'z'], {}, 2);
  assert.equal(shards.flat().length, 3);
});

test('shard spec parsing rejects nonsense', () => {
  assert.deepEqual(parseShard('2/3'), { index: 2, count: 3 });
  for (const bad of ['0/2', '3/2', 'a/b', '', undefined, '1'])
    assert.throws(() => parseShard(bad), /invalid --shard/);
});
