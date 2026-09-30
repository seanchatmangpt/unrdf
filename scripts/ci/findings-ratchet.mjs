#!/usr/bin/env node
/**
 * @file Scanner-findings ratchet (injection analysis etc.): fail only on findings that are new or more numerous
 * than the committed baseline. Findings are keyed by `file|rule`, so moving code within a file does not matter.
 *
 * Usage:
 *   node scripts/ci/findings-ratchet.mjs --report injection-report.json --baseline .github/baselines/injection-critical.json
 *   node scripts/ci/findings-ratchet.mjs --report injection-report.json --baseline ... --update
 *
 * Options: --severity <comma list, default critical>
 * Exit codes: 0 nothing new, 1 new/grown findings, 2 the report could not be read.
 */
import { existsSync, readFileSync, writeFileSync } from 'node:fs';
import process from 'node:process';
import { compare, countBy, parseJsonLoose, toBaseline } from './ratchet-lib.mjs';

const arg = (name, dflt) => {
  const hit = process.argv.find(a => a.startsWith(`--${name}=`));
  if (hit) return hit.split('=').slice(1).join('=');
  const i = process.argv.indexOf(`--${name}`);
  return i >= 0 && process.argv[i + 1] && !process.argv[i + 1].startsWith('--')
    ? process.argv[i + 1]
    : dflt;
};
const reportPath = arg('report', null);
const baselinePath = arg('baseline', null);
const severities = new Set(arg('severity', 'critical').split(','));
const update = process.argv.includes('--update');
if (!reportPath || !baselinePath) {
  console.error(
    'usage: findings-ratchet.mjs --report <report.json> --baseline <baseline.json> [--severity critical] [--update]'
  );
  process.exit(2);
}

let report;
try {
  report = parseJsonLoose(readFileSync(reportPath, 'utf8'));
} catch (e) {
  console.error(`REPORT_UNAVAILABLE: cannot read ${reportPath}: ${e.message}`);
  process.exit(2);
}
if (!Array.isArray(report.findings)) {
  console.error(`REPORT_UNAVAILABLE: ${reportPath} has no \`findings\` array.`);
  process.exit(2);
}

const current = countBy(report.findings, f =>
  severities.has(f.severity) ? `${f.file}|${f.rule}` : null
);
const total = [...current.values()].reduce((a, b) => a + b, 0);

if (update) {
  writeFileSync(baselinePath, JSON.stringify(toBaseline(current), null, 2) + '\n');
  console.log(
    `Wrote ${current.size} file|rule entries (${total} ${[...severities].join('/')} findings) to ${baselinePath}`
  );
  process.exit(0);
}

const baseline = existsSync(baselinePath) ? JSON.parse(readFileSync(baselinePath, 'utf8')) : {};
const { added, grown, fixed } = compare(current, baseline);
const accepted = Object.values(baseline).reduce((a, b) => a + b, 0);
console.log(
  `${[...severities].join('/')} findings: ${total} now | ${accepted} accepted in baseline | ${added.length} NEW entries | ${grown.length} GREW | ${fixed.length} improved`
);
if (fixed.length)
  console.log(
    `  Tip: ${fixed.length} baselined entries improved; run with --update to shrink ${baselinePath}.`
  );
const bad = [...added, ...grown];
if (bad.length) {
  console.error('\nNew or increased findings (file | rule):');
  for (const { key, now, was } of bad) console.error(`  ${key}  ${was} -> ${now}`);
  console.error(
    '\nFix them, or if accepted for now, re-run with --update and commit the baseline.'
  );
  process.exit(1);
}
console.log('Nothing new.');
