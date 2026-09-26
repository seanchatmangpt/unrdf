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
 * skipped or todo, zero tests ran, a listed file uses a test double, or a suite's
 * executed test count differs from its pinned count. The pin closes the
 * anti-vacuity hole where a suite emptied down to its import line still counts
 * as one passing file: adding or removing a falsifier must update the pin here.
 */
import { spawnSync } from 'node:child_process';
import { existsSync, readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const root = join(dirname(fileURLToPath(import.meta.url)), '..');

/** Suite path -> exact number of top-level tests it must execute. */
export const INTERCHANGEABLE_PART_SUITE_COUNTS = Object.freeze({
  'packages/core/test/interchangeable-part.test.mjs': 6,
  'packages/core/test/interchangeable-part-canonical.test.mjs': 8,
  'packages/receipts/test/substitution-receipt.test.mjs': 4,
  'packages/manufacturing/test/artifact/interchangeable-part.test.mjs': 2,
  'packages/atomvm/test/part-admission-broker.test.mjs': 5,
  'packages/atomvm/test/part-admission-broker-falsifiers.test.mjs': 7,
  'packages/atomvm/test/part-admission-broker-adversarial.test.mjs': 8,
});

export const INTERCHANGEABLE_PART_SUITES = Object.freeze(
  Object.keys(INTERCHANGEABLE_PART_SUITE_COUNTS)
);

const EXPECTED_TOTAL = Object.values(INTERCHANGEABLE_PART_SUITE_COUNTS).reduce(
  (sum, suiteCount) => sum + suiteCount,
  0
);

const tapCount = (stdout, name) =>
  Number((stdout.match(new RegExp(`^# ${name} (\\d+)$`, 'm')) ?? [])[1] ?? NaN);

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
  for (const suite of INTERCHANGEABLE_PART_SUITES) {
    const single = spawnSync(process.execPath, ['--test', '--test-reporter=tap', suite], {
      cwd: root,
      encoding: 'utf8',
    });
    const ran = tapCount(single.stdout, 'tests');
    const expected = INTERCHANGEABLE_PART_SUITE_COUNTS[suite];
    if (ran !== expected)
      refusals.push(`SUITE_COUNT_MISMATCH ${suite} tests=${ran} expected=${expected}`);
  }
}

if (refusals.length === 0) {
  const run = spawnSync(
    process.execPath,
    ['--test', '--test-reporter=tap', ...INTERCHANGEABLE_PART_SUITES],
    { cwd: root, encoding: 'utf8' }
  );
  process.stdout.write(run.stdout);
  process.stderr.write(run.stderr);

  const count = name => tapCount(run.stdout, name);
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
  if (tests !== EXPECTED_TOTAL)
    refusals.push(`TOTAL_COUNT_MISMATCH tests=${tests} expected=${EXPECTED_TOTAL}`);

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
