#!/usr/bin/env node
/**
 * @file Dependency-audit ratchet: fail only on advisories that are not in the committed baseline.
 *
 * Usage:
 *   node scripts/ci/audit-ratchet.mjs                       # run `pnpm audit --json`, compare to baseline
 *   node scripts/ci/audit-ratchet.mjs --input audit.json    # compare an existing `pnpm audit --json` file
 *   node scripts/ci/audit-ratchet.mjs --update              # rewrite the baseline from the current audit
 *
 * Options: --baseline <path> (default .github/baselines/audit.json), --min-severity <low|moderate|high|critical>
 * Exit codes: 0 no new advisories, 1 new advisories found, 2 the audit could not be obtained.
 */
import { spawnSync } from 'node:child_process';
import { existsSync, readFileSync, writeFileSync } from 'node:fs';
import process from 'node:process';
import { SEVERITY_RANK, compare, countBy, parseJsonLoose, toBaseline } from './ratchet-lib.mjs';

const arg = (name, dflt) => {
  const hit = process.argv.find(a => a.startsWith(`--${name}=`));
  if (hit) return hit.split('=').slice(1).join('=');
  const i = process.argv.indexOf(`--${name}`);
  return i >= 0 && process.argv[i + 1] && !process.argv[i + 1].startsWith('--')
    ? process.argv[i + 1]
    : dflt;
};
const baselinePath = arg('baseline', '.github/baselines/audit.json');
const minSeverity = arg('min-severity', 'moderate');
const inputPath = arg('input', null);
const update = process.argv.includes('--update');

function loadAudit() {
  let text = '';
  let stderr = '';
  try {
    if (inputPath) {
      text = readFileSync(inputPath, 'utf8');
    } else {
      const r = spawnSync('pnpm', ['audit', '--json'], {
        encoding: 'utf8',
        maxBuffer: 256 * 1024 * 1024,
      });
      // pnpm exits non-zero when it finds vulnerabilities, so judge by whether we got a JSON document.
      text = r.stdout || '';
      stderr = r.stderr || '';
    }
    return parseJsonLoose(text);
  } catch (e) {
    // Exit 2 (not 1) so "could not audit" can never be mistaken for "new advisories found", or for a pass.
    console.error(`AUDIT_UNAVAILABLE: could not obtain a pnpm audit JSON document (${e.message}).`);
    if (stderr) console.error(stderr.slice(-1500));
    process.exit(2);
  }
}

const audit = loadAudit();
const advisories = Object.values(audit.advisories ?? {});
if (!advisories.length && !audit.metadata) {
  console.error('AUDIT_UNAVAILABLE: audit JSON has neither `advisories` nor `metadata`.');
  process.exit(2);
}
const floor = SEVERITY_RANK[minSeverity] ?? SEVERITY_RANK.moderate;
const byKey = new Map();
const current = countBy(advisories, a => {
  if ((SEVERITY_RANK[a.severity] ?? 0) < floor) return null;
  const versions = [...new Set((a.findings ?? []).map(f => f.version))].sort().join(',');
  const key = `${a.github_advisory_id ?? a.id}|${a.module_name}|${versions}`;
  byKey.set(key, a);
  return key;
});

if (update) {
  writeFileSync(baselinePath, JSON.stringify(toBaseline(current), null, 2) + '\n');
  console.log(`Wrote ${current.size} advisories (>= ${minSeverity}) to ${baselinePath}`);
  process.exit(0);
}

const baseline = existsSync(baselinePath) ? JSON.parse(readFileSync(baselinePath, 'utf8')) : {};
const { added, fixed } = compare(current, baseline);
const debt = current.size - added.length;
console.log(
  `Advisories (>= ${minSeverity}): ${current.size} now | ${debt} known in baseline | ${added.length} NEW | ${fixed.length} fixed since baseline`
);
if (fixed.length)
  console.log(
    `  Tip: ${fixed.length} baselined advisories are gone; run with --update to shrink ${baselinePath}.`
  );
if (added.length) {
  console.error('\nNEW advisories (not in the baseline):');
  for (const { key } of added) {
    const a = byKey.get(key);
    console.error(
      `  [${a.severity}] ${a.module_name} ${(a.findings ?? []).map(f => f.version).join(', ')} - ${a.title}\n      ${a.url ?? ''}  (fix: ${a.patched_versions ?? 'n/a'})`
    );
  }
  console.error(
    '\nFix them, or if they are accepted for now, run `node scripts/ci/audit-ratchet.mjs --update` and commit the baseline.'
  );
  process.exit(1);
}
console.log('No new advisories.');
