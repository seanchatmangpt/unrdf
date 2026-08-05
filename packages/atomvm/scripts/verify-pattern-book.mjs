#!/usr/bin/env node
import { createHash } from 'node:crypto';
import { access, readFile, writeFile, mkdir } from 'node:fs/promises';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { SOURCE, PATTERNS } from '../books/atomvm-patterns/pattern-manifest.mjs';

const repoRoot = resolve(fileURLToPath(new URL('../../../', import.meta.url)));
const targetSummaryPath = 'packages/atomvm/books/atomvm-patterns/src/SUMMARY.md';
const summary = await readFile(resolve(repoRoot, targetSummaryPath), 'utf8');
const probe = await readFile(resolve(repoRoot, 'packages/atomvm/examples/erlang/otp_patterns_probe.erl'), 'utf8');
const failures = [];
const observed = [];

if (PATTERNS.length !== 30) failures.push({ code: 'PATTERN_COUNT_DRIFT', observed: PATTERNS.length });

for (const pattern of PATTERNS) {
  const record = { id: pattern.id, checks: {} };
  for (const [field, path] of [
    ['chapter', pattern.targetPath],
    ['example', pattern.examplePath],
    ['test', pattern.testPath],
  ]) {
    try {
      await access(resolve(repoRoot, path));
      record.checks[field] = true;
    } catch {
      record.checks[field] = false;
      failures.push({ code: `${field.toUpperCase()}_MISSING`, id: pattern.id, path });
    }
  }

  const relativeChapter = pattern.targetPath.replace('packages/atomvm/books/atomvm-patterns/src/', '');
  record.checks.summary = summary.includes(`](${relativeChapter})`);
  if (!record.checks.summary) failures.push({ code: 'SUMMARY_EDGE_MISSING', id: pattern.id, path: relativeChapter });

  if (record.checks.test) {
    const testText = await readFile(resolve(repoRoot, pattern.testPath), 'utf8');
    record.checks.testName = testText.includes(pattern.testName);
    if (!record.checks.testName) failures.push({ code: 'TEST_NAME_MISSING', id: pattern.id, testName: pattern.testName });
  }

  record.checks.atomvmMarker = probe.includes(`marker(${pattern.atomvmMarker})`);
  if (!record.checks.atomvmMarker) failures.push({ code: 'ATOMVM_MARKER_MISSING', id: pattern.id, marker: pattern.atomvmMarker });
  observed.push(record);
}

const body = {
  schema: 'urn:unrdf:atomvm:jotp-pattern-book-receipt:v1',
  source: SOURCE,
  target: { repository: 'seanchatmangpt/unrdf', summaryPath: targetSummaryPath },
  parts: 5,
  patterns: PATTERNS.length,
  observed,
  failures,
  status: failures.length === 0 ? 'ALIVE' : 'BLOCKED',
};
body.receiptDigest = createHash('sha256').update(JSON.stringify(body)).digest('hex');
const outputPath = resolve(process.env.PATTERN_BOOK_RECEIPT ?? '.build/receipts/pattern-book-receipt.json');
await mkdir(dirname(outputPath), { recursive: true });
await writeFile(outputPath, `${JSON.stringify(body, null, 2)}\n`);
console.log(JSON.stringify({ status: body.status, patterns: body.patterns, failures: body.failures.length, receiptDigest: body.receiptDigest }));
if (body.status !== 'ALIVE') process.exit(1);
