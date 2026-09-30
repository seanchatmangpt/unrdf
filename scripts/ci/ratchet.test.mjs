import { test } from 'node:test';
import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { mkdtempSync, writeFileSync, readFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { compare, countBy, parseJsonLoose, toBaseline } from './ratchet-lib.mjs';

const here = path.dirname(fileURLToPath(import.meta.url));
const run = (script, args) =>
  spawnSync(process.execPath, [path.join(here, script), ...args], { encoding: 'utf8' });
const tmp = mkdtempSync(path.join(tmpdir(), 'ratchet-'));
const write = (name, obj) => {
  const p = path.join(tmp, name);
  writeFileSync(p, typeof obj === 'string' ? obj : JSON.stringify(obj));
  return p;
};
process.on('exit', () => rmSync(tmp, { recursive: true, force: true }));

const adv = (id, mod, ver, severity = 'high') => ({
  github_advisory_id: id,
  module_name: mod,
  severity,
  title: `${mod} issue`,
  url: `https://example.test/${id}`,
  patched_versions: '>=9',
  findings: [{ version: ver }],
});
const audit = (...advs) => ({
  advisories: Object.fromEntries(advs.map((a, i) => [String(i + 1), a])),
  metadata: { vulnerabilities: {} },
});

test('compare: classifies added, grown and fixed', () => {
  const r = compare(
    new Map([
      ['a', 1],
      ['b', 3],
      ['c', 1],
    ]),
    { b: 2, c: 1, d: 4 }
  );
  assert.deepEqual(
    r.added.map(x => x.key),
    ['a']
  );
  assert.deepEqual(
    r.grown.map(x => x.key),
    ['b']
  );
  assert.deepEqual(
    r.fixed.map(x => x.key),
    ['d']
  );
});

test('compare: identical state has nothing added or grown', () => {
  const r = compare(new Map([['a', 2]]), { a: 2 });
  assert.equal(r.added.length + r.grown.length + r.fixed.length, 0);
});

test('toBaseline is sorted and deterministic; parseJsonLoose skips a banner; countBy skips nulls', () => {
  assert.deepEqual(
    Object.keys(
      toBaseline(
        new Map([
          ['z', 1],
          ['a', 1],
        ])
      )
    ),
    ['a', 'z']
  );
  assert.deepEqual(parseJsonLoose('Scanning...\n{"x":1}'), { x: 1 });
  assert.throws(() => parseJsonLoose('no json here'));
  assert.equal(countBy([{ s: 1 }, { s: 2 }], f => (f.s === 1 ? 'k' : null)).get('k'), 1);
});

test('audit-ratchet: baselined advisory passes, NEW advisory fails with exit 1', () => {
  const base = write('base.json', {});
  const input = write('audit.json', audit(adv('GHSA-1', 'lodash', '4.0.0')));
  assert.equal(
    run('audit-ratchet.mjs', ['--input', input, '--baseline', base, '--update']).status,
    0
  );
  assert.equal(run('audit-ratchet.mjs', ['--input', input, '--baseline', base]).status, 0);
  const worse = write(
    'audit2.json',
    audit(adv('GHSA-1', 'lodash', '4.0.0'), adv('GHSA-2', 'tar', '6.0.0', 'critical'))
  );
  const r = run('audit-ratchet.mjs', ['--input', worse, '--baseline', base]);
  assert.equal(r.status, 1);
  assert.match(r.stderr, /GHSA-2|tar/);
});

test('audit-ratchet: the same advisory on a NEW installed version is treated as new', () => {
  const base = write('base2.json', {});
  run('audit-ratchet.mjs', [
    '--input',
    write('a1.json', audit(adv('GHSA-1', 'lodash', '4.0.0'))),
    '--baseline',
    base,
    '--update',
  ]);
  const r = run('audit-ratchet.mjs', [
    '--input',
    write('a2.json', audit(adv('GHSA-1', 'lodash', '3.0.0'))),
    '--baseline',
    base,
  ]);
  assert.equal(r.status, 1);
});

test('audit-ratchet: below --min-severity is ignored; fixed advisories are reported but pass', () => {
  const base = write('base3.json', {});
  run('audit-ratchet.mjs', [
    '--input',
    write('b1.json', audit(adv('GHSA-1', 'a', '1.0.0'))),
    '--baseline',
    base,
    '--update',
  ]);
  const lowOnly = write('b2.json', audit(adv('GHSA-9', 'b', '1.0.0', 'low')));
  const r = run('audit-ratchet.mjs', ['--input', lowOnly, '--baseline', base]);
  assert.equal(r.status, 0);
  assert.match(r.stdout, /1 fixed since baseline/);
});

test('audit-ratchet: unusable input exits 2 and never passes silently', () => {
  assert.equal(
    run('audit-ratchet.mjs', [
      '--input',
      write('junk.json', 'not json'),
      '--baseline',
      path.join(tmp, 'x.json'),
    ]).status,
    2
  );
  assert.equal(
    run('audit-ratchet.mjs', [
      '--input',
      write('empty.json', {}),
      '--baseline',
      path.join(tmp, 'x.json'),
    ]).status,
    2
  );
});

const finding = (file, rule, severity = 'critical') => ({ file, rule, severity });

test('findings-ratchet: same findings pass, a new file|rule fails, a grown count fails', () => {
  const base = path.join(tmp, 'f-base.json');
  const one = write('f1.json', {
    findings: [
      finding('a.mjs', 'exec_sync_command'),
      finding('a.mjs', 'exec_sync_command'),
      finding('b.mjs', 'eval_usage', 'medium'),
    ],
  });
  assert.equal(
    run('findings-ratchet.mjs', ['--report', one, '--baseline', base, '--update']).status,
    0
  );
  assert.deepEqual(JSON.parse(readFileSync(base, 'utf8')), { 'a.mjs|exec_sync_command': 2 });
  assert.equal(run('findings-ratchet.mjs', ['--report', one, '--baseline', base]).status, 0);
  const newRule = write('f2.json', {
    findings: [
      finding('a.mjs', 'exec_sync_command'),
      finding('a.mjs', 'exec_sync_command'),
      finding('c.mjs', 'eval_usage'),
    ],
  });
  const r = run('findings-ratchet.mjs', ['--report', newRule, '--baseline', base]);
  assert.equal(r.status, 1);
  assert.match(r.stderr, /c\.mjs\|eval_usage/);
  const grown = write('f3.json', {
    findings: [
      finding('a.mjs', 'exec_sync_command'),
      finding('a.mjs', 'exec_sync_command'),
      finding('a.mjs', 'exec_sync_command'),
    ],
  });
  assert.equal(run('findings-ratchet.mjs', ['--report', grown, '--baseline', base]).status, 1);
  const fewer = write('f4.json', { findings: [finding('a.mjs', 'exec_sync_command')] });
  assert.equal(run('findings-ratchet.mjs', ['--report', fewer, '--baseline', base]).status, 0);
});

test('findings-ratchet: banner before the JSON is tolerated; missing/invalid report exits 2', () => {
  const base = path.join(tmp, 'f-base2.json');
  const banner = write('f5.json', 'Scanning 10 files...\n' + JSON.stringify({ findings: [] }));
  assert.equal(run('findings-ratchet.mjs', ['--report', banner, '--baseline', base]).status, 0);
  assert.equal(
    run('findings-ratchet.mjs', ['--report', path.join(tmp, 'nope.json'), '--baseline', base])
      .status,
    2
  );
  assert.equal(
    run('findings-ratchet.mjs', [
      '--report',
      write('f6.json', { nofindings: true }),
      '--baseline',
      base,
    ]).status,
    2
  );
});
