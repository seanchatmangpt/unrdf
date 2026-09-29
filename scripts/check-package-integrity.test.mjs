import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, mkdirSync, writeFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { spawnSync } from 'node:child_process';
import { runChecks } from './check-package-integrity.mjs';

function write(root, rel, content) {
  const p = join(root, rel);
  mkdirSync(dirname(p), { recursive: true });
  writeFileSync(p, typeof content === 'string' ? content : JSON.stringify(content, null, 2));
}

function fixture() {
  const root = mkdtempSync(join(tmpdir(), 'integrity-'));
  write(root, 'packages/good/package.json', {
    name: '@unrdf/good',
    main: './src/index.mjs',
    bin: { good: './bin/good.mjs' },
    exports: { '.': './src/index.mjs', './util': './src/util.mjs', './x/*': './src/x/*' },
    files: ['src', 'bin'],
    scripts: { test: 'node --test test/a.test.mjs', lint: 'eslint src --ext .mjs' },
  });
  write(root, 'packages/good/src/index.mjs', "import './util.mjs';\nexport const a = 1;\n");
  write(root, 'packages/good/src/util.mjs', 'export const b = 2;\n');
  write(root, 'packages/good/src/x/deep/y.mjs', 'export const y = 1;\n');
  write(root, 'packages/good/bin/good.mjs', '#!/usr/bin/env node\n');
  write(root, 'packages/good/test/a.test.mjs', '');
  write(root, 'packages/bad/package.json', {
    name: '@unrdf/bad',
    main: './src/missing.mjs',
    exports: { '.': { import: './src/index.mjs', types: './src/index.d.ts' } },
    bin: './bin/none.mjs',
    scripts: { demo: 'node examples/none.mjs', f: 'pnpm --filter @unrdf/ghost test' },
  });
  write(
    root,
    'packages/bad/src/index.mjs',
    [
      "import './nope.mjs';",
      "import '@unrdf/good/util';",
      "import '@unrdf/good/x/deep/y.mjs';",
      "import '@unrdf/good/secret';",
      "import '@unrdf/removed';",
      "const opt = await import('./optional.mjs').catch(() => null);",
      "// import './commented.mjs';",
      '',
    ].join('\n')
  );
  return root;
}

test('reports every kind of dangling reference in a broken fixture', () => {
  const root = fixture();
  try {
    const { findings } = runChecks(root);
    const errs = findings.filter(f => f.severity === 'error');
    const has = (kind, pkg, needle) =>
      errs.some(f => f.kind === kind && f.package === pkg && f.message.includes(needle));
    assert.ok(has('missing-target', '@unrdf/bad', 'main -> ./src/missing.mjs'));
    assert.ok(has('missing-target', '@unrdf/bad', 'index.d.ts'));
    assert.ok(has('missing-target', '@unrdf/bad', 'bin.@unrdf/bad'));
    assert.ok(has('missing-script-target', '@unrdf/bad', 'examples/none.mjs'));
    assert.ok(has('unknown-filter', '@unrdf/bad', '@unrdf/ghost'));
    assert.ok(has('unresolved-relative-import', '@unrdf/bad', './nope.mjs'));
    assert.ok(has('unexported-subpath', '@unrdf/bad', './secret'));
    assert.ok(has('unknown-workspace-package', '@unrdf/bad', '@unrdf/removed'));
    // guarded optional import is only a warning; comments are ignored
    assert.ok(findings.some(f => f.severity === 'warn' && f.target === './optional.mjs'));
    assert.ok(!findings.some(f => f.target === './commented.mjs'));
    // covered subpaths (exact key and wildcard crossing directories) are fine
    assert.ok(!findings.some(f => f.target === '@unrdf/good/util'));
    assert.ok(!findings.some(f => f.target === '@unrdf/good/x/deep/y.mjs'));
    // the good package has no findings at all
    assert.deepEqual(
      findings.filter(f => f.package === '@unrdf/good'),
      []
    );
  } finally {
    rmSync(root, { recursive: true, force: true });
  }
});

test('build output (dist) targets are warnings, not errors', () => {
  const root = mkdtempSync(join(tmpdir(), 'integrity-'));
  try {
    write(root, 'packages/p/package.json', {
      name: '@unrdf/p',
      types: './dist/index.d.ts',
      files: ['dist/'],
    });
    const { findings } = runChecks(root);
    assert.equal(findings.length, 2);
    assert.ok(findings.every(f => f.severity === 'warn' && f.kind === 'missing-build-output'));
  } finally {
    rmSync(root, { recursive: true, force: true });
  }
});

test('real repo: no manifest target or exports-map errors', () => {
  const { findings } = runChecks();
  const bad = findings.filter(
    f =>
      f.severity === 'error' &&
      ['missing-target', 'unexported-subpath', 'invalid-json', 'unknown-filter'].includes(f.kind)
  );
  assert.deepEqual(bad, []);
});

test('--json emits parseable output', () => {
  const script = join(dirname(fileURLToPath(import.meta.url)), 'check-package-integrity.mjs');
  const r = spawnSync(process.execPath, [script, '--json'], { encoding: 'utf8' });
  const parsed = JSON.parse(r.stdout);
  assert.ok(Array.isArray(parsed.findings));
  assert.ok(parsed.packages > 0);
});
