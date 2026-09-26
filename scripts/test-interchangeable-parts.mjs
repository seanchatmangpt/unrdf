#!/usr/bin/env node
/**
 * @file Gate for the interchangeable-parts kernel (@unrdf/core, @unrdf/receipts,
 * @unrdf/manufacturing, @unrdf/atomvm).
 *
 * These suites are node:test files. Package `test` scripts run vitest, which
 * cannot collect node:test files ("No test suite found"), so without this gate
 * the kernel's falsifiers are never executed by any package script. This runner
 * is the single source of the suite list; the vitest configs exclude the same
 * files so the wrong runner no longer reports them as collection failures.
 *
 * Refuses (exit 1) when: a listed file is missing, any test fails, any test is
 * skipped or todo, zero tests ran, or a listed file uses a test double.
 */
import { spawnSync } from 'node:child_process';
import { existsSync, readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const root = join(dirname(fileURLToPath(import.meta.url)), '..');

export const INTERCHANGEABLE_PART_SUITES = Object.freeze([
  'packages/core/test/interchangeable-part.test.mjs',
  'packages/receipts/test/substitution-receipt.test.mjs',
  'packages/manufacturing/test/artifact/interchangeable-part.test.mjs',
  'packages/atomvm/test/part-admission-broker.test.mjs',
  'packages/atomvm/test/part-admission-broker-falsifiers.test.mjs',
]);

const DOUBLE_PATTERN =
  /\bvi\.(mock|fn|spyOn|useFakeTimers)\b|\bjest\.(mock|fn|spyOn)\b|\bsinon\b|\bmock\.(fn|method|timers)\b/;

const refusals = [];

for (const suite of INTERCHANGEABLE_PART_SUITES) {
  const path = join(root, suite);
  if (!existsSync(path)) {
    refusals.push(`SUITE_MISSING ${suite}`);
    continue;
  }
  readFileSync(path, 'utf8')
    .split('\n')
    .forEach((line, index) => {
      if (DOUBLE_PATTERN.test(line))
        refusals.push(`TEST_DOUBLE ${suite}:${index + 1}: ${line.trim()}`);
    });
}

if (refusals.length === 0) {
  const run = spawnSync(
    process.execPath,
    ['--test', '--test-reporter=tap', ...INTERCHANGEABLE_PART_SUITES],
    { cwd: root, encoding: 'utf8' }
  );
  process.stdout.write(run.stdout);
  process.stderr.write(run.stderr);

  const count = name =>
    Number((run.stdout.match(new RegExp(`^# ${name} (\\d+)$`, 'm')) ?? [])[1] ?? NaN);
  const tests = count('tests');
  const pass = count('pass');
  const fail = count('fail');
  const skipped = count('skipped');
  const todo = count('todo');

  if (run.status !== 0) refusals.push(`NODE_TEST_EXIT ${run.status}`);
  if (!(tests > 0)) refusals.push(`NO_TESTS_RAN tests=${tests}`);
  if (fail !== 0) refusals.push(`TESTS_FAILED fail=${fail}`);
  if (skipped !== 0 || todo !== 0) refusals.push(`TESTS_SKIPPED skipped=${skipped} todo=${todo}`);
  if (pass !== tests) refusals.push(`PASS_MISMATCH pass=${pass} tests=${tests}`);

  if (refusals.length === 0) {
    process.stdout.write(
      `${JSON.stringify({
        gate: 'interchangeable-parts',
        status: 'ALIVE',
        suites: INTERCHANGEABLE_PART_SUITES.length,
        tests,
        pass,
      })}\n`
    );
  }
}

if (refusals.length > 0) {
  process.stderr.write(
    `${JSON.stringify({ gate: 'interchangeable-parts', status: 'REFUSED', refusals })}\n`
  );
  process.exit(1);
}
