#!/usr/bin/env node
/**
 * @file Deterministic-workload timing benchmark for the interchangeable-parts
 * kernel: passport verification (digest + canonical remanufacture), substitution
 * evaluation (admit and forged-refuse paths), substitution receipt mint/verify,
 * and a full mandatory-admission broker.execute round trip.
 *
 * Usage:
 *   node scripts/bench-interchangeable-parts.mjs [--iterations=N] [--json]
 *   node scripts/bench-interchangeable-parts.mjs --check
 *   node scripts/bench-interchangeable-parts.mjs --write-receipt [--runs=3]
 *
 * --check compares each operation's median against the regression bounds in
 * benchmarks/interchangeable-parts/bench-receipt.json and exits 1 when a bound
 * is exceeded or when any timed call returned the wrong verdict (an admission
 * that should refuse, or vice versa): a fast wrong answer is never a pass.
 * --check also refuses RECEIPT_SUBJECT_STALE when the committed receipt was
 * measured against kernel sources other than the ones on disk: the receipt's
 * subject is the git blob id of each kernel file (content-addressed, so it can
 * name the exact measured code from inside the commit that carries it).
 *
 * --write-receipt regenerates bench-receipt.json: it runs the benchmark in
 * `--runs` fresh processes, derives bounds as 10 x the max observed median
 * (rounded up to 1 microsecond), and keeps the committed pre-fix baseline.
 * Real collaborators only; the clock is process.hrtime.bigint.
 */
import { createHash } from 'node:crypto';
import { spawnSync } from 'node:child_process';
import { readFileSync, writeFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import {
  createPartPassport,
  createPartRequirement,
  evaluateSubstitution,
  verifyPartPassport,
} from '../packages/core/src/interchangeable-part.mjs';
import {
  createSubstitutionReceipt,
  verifySubstitutionReceipt,
} from '../packages/receipts/src/substitution-receipt.mjs';
import { createPartAdmissionBroker } from '../packages/atomvm/src/part-admission-broker.mjs';

const root = join(dirname(fileURLToPath(import.meta.url)), '..');
const RECEIPT_PATH = join(root, 'benchmarks/interchangeable-parts/bench-receipt.json');
const KERNEL_SOURCES = Object.freeze([
  'packages/atomvm/src/part-admission-broker.mjs',
  'packages/core/src/interchangeable-part.mjs',
  'packages/receipts/src/substitution-receipt.mjs',
]);

/** Git blob id (sha1 of "blob <len>\0" + bytes), identical to `git hash-object`. */
function gitBlobId(bytes) {
  return createHash('sha1').update(`blob ${bytes.length}\0`).update(bytes).digest('hex');
}

function kernelBlobs() {
  return Object.fromEntries(
    KERNEL_SOURCES.map(path => [path, gitBlobId(readFileSync(join(root, path)))])
  );
}

/**
 * JSON.stringify(value, null, 2) plus Prettier's JSON rule for arrays: an array
 * of primitives that fits the 100-column print width is printed on one line.
 */
function formatJson(value) {
  const expanded = JSON.stringify(value, null, 2);
  const collapsed = expanded.replace(
    /^( *)("[^"\n]*": )?\[\n((?: *(?:"[^"\n]*"|-?[\d.eE+-]+|true|false|null),?\n)+) *\]/gm,
    (match, indent, key = '', body) => {
      const items = body
        .split('\n')
        .map(line => line.trim().replace(/,$/, ''))
        .filter(Boolean);
      const line = `${indent}${key}[${items.join(', ')}]`;
      return line.length <= 100 ? line : match;
    }
  );
  return `${collapsed}\n`;
}

function writeReceipt(runCount) {
  const runs = [];
  for (let run = 0; run < runCount; run += 1) {
    const child = spawnSync(
      process.execPath,
      [fileURLToPath(import.meta.url), `--iterations=${iterations}`, '--json'],
      { cwd: root, encoding: 'utf8' }
    );
    if (child.status !== 0) {
      process.stderr.write(child.stdout + child.stderr);
      throw new Error(`benchmark run ${run + 1} exited ${child.status}`);
    }
    runs.push(JSON.parse(child.stdout));
  }
  const names = Object.keys(runs[0].results);
  const previous = JSON.parse(readFileSync(RECEIPT_PATH, 'utf8'));
  const receipt = {
    bench: 'interchangeable-parts',
    command: `node scripts/bench-interchangeable-parts.mjs --iterations=${iterations} --json`,
    check: 'node scripts/bench-interchangeable-parts.mjs --check',
    regenerate: `node scripts/bench-interchangeable-parts.mjs --write-receipt --runs=${runCount}`,
    subject: {
      kind: 'git-blob-ids-of-measured-kernel-sources',
      kernel_blobs: kernelBlobs(),
    },
    host: { node: process.version, platform: `${process.platform}-${process.arch}` },
    evidence_ceiling: 'HOST_NATIVE_TIMING',
    runs: runs.map(report =>
      Object.fromEntries(
        names.map(name => {
          const { medianNs, p99Ns, opsPerSec } = report.results[name];
          return [name, { medianNs, p99Ns, opsPerSec }];
        })
      )
    ),
    pre_fix_core_at_parent: previous.pre_fix_core_at_parent,
    bounds: {
      rule: `medianNs <= 10 x max observed median over ${runCount} host runs, rounded up to 1 microsecond; wrong verdicts refuse regardless of speed`,
      medianNs: Object.fromEntries(
        names.map(name => {
          const max = Math.max(...runs.map(report => report.results[name].medianNs));
          return [name, Math.ceil((10 * max) / 1000) * 1000];
        })
      ),
    },
  };
  writeFileSync(RECEIPT_PATH, formatJson(receipt));
  process.stdout.write(`${JSON.stringify({ wrote: RECEIPT_PATH, subject: receipt.subject })}\n`);
}

const args = new Map(
  process.argv.slice(2).map(arg => {
    const [key, value] = arg.replace(/^--/, '').split('=');
    return [key, value ?? true];
  })
);
const iterations = Number(args.get('iterations') ?? 2000);
const warmup = Math.max(50, Math.floor(iterations / 10));

if (args.has('write-receipt')) {
  writeReceipt(Number(args.get('runs') ?? 3));
  process.exit(0);
}

const contract = {
  semanticContract: 'urn:contract:semantic:v1',
  inputContract: 'urn:contract:input:v1',
  outputContract: 'urn:contract:output:v1',
  stateContract: 'urn:contract:state:v1',
  rulesContract: 'urn:contract:rules:v1',
  informationFlowContract: 'urn:contract:flow:v1',
  lifecycleContract: 'urn:contract:lifecycle:v1',
};

const requirement = createPartRequirement({
  requirementId: 'urn:requirement:bench',
  ...contract,
  expectedInputs: ['input:a', 'input:b'],
  requiredGuarantees: ['output:a'],
  allowedEffects: ['effect:cache'],
  allowedFailures: ['failure:timeout'],
  allowedAuthorityIssuers: ['issuer:host'],
  allowedCapabilities: ['cap:read', 'cap:write'],
  maxDelegationDepth: 1,
  resourceCeilings: { memoryMiB: 64, cpuMs: 50 },
  requiredVerifiers: ['verify:semantic', 'verify:replay'],
  receiptSchema: 'unrdf.execution-receipt/1',
  allowedRuntimes: ['atomvm', 'wasm'],
});

const candidate = createPartPassport({
  partId: 'urn:part:bench',
  version: '1.0.0',
  artifactDigest: 'sha256:bench',
  ...contract,
  acceptedInputs: ['input:a', 'input:b', 'input:c'],
  guarantees: ['output:a', 'output:b'],
  effects: ['effect:cache'],
  failures: ['failure:timeout'],
  authorityIssuer: 'issuer:host',
  authoritySubject: 'subject:bench',
  capabilities: ['cap:read'],
  delegationDepth: 0,
  resources: { memoryMiB: 32, cpuMs: 10 },
  verifiers: ['verify:semantic', 'verify:replay'],
  receiptSchema: 'unrdf.execution-receipt/1',
  replay: true,
  runtime: 'atomvm',
  provenance: { source: 'ontology://parts/bench', artifactDigest: 'sha256:bench' },
});

const context = {
  hostCapabilities: ['cap:read', 'cap:write'],
  hostAuthorityIssuers: ['issuer:host'],
  hostResourceCeilings: { memoryMiB: 48, cpuMs: 40 },
};

function canonical(value) {
  if (Array.isArray(value)) return value.map(canonical);
  if (value && typeof value === 'object') {
    return Object.fromEntries(
      Object.keys(value)
        .sort()
        .map(key => [key, canonical(value[key])])
    );
  }
  return value;
}

function forge(record, edits) {
  const { digest: _digest, ...body } = { ...record, ...edits };
  const digest = createHash('sha256')
    .update(JSON.stringify(canonical(body)))
    .digest('hex');
  return { ...body, digest };
}

const forged = forge(candidate, { resources: { memoryMiB: Number.NaN, cpuMs: 10 } });
const receipt = createSubstitutionReceipt({
  requirement,
  candidate,
  context,
  receiptId: 'bench',
  timestamp: 0,
});
const delegate = {
  executions: 0,
  async execute(request) {
    delegate.executions += 1;
    return { request };
  },
};
const broker = createPartAdmissionBroker({
  requirement,
  candidate,
  context,
  substitutionReceipt: receipt,
  delegate,
});

const wrongVerdicts = [];

const operations = [
  [
    'verifyPartPassport',
    () => {
      if (!verifyPartPassport(candidate).valid) wrongVerdicts.push('verifyPartPassport');
    },
  ],
  [
    'evaluateSubstitution.admit',
    () => {
      if (evaluateSubstitution(requirement, candidate, context).state !== 'ADMITTED')
        wrongVerdicts.push('evaluateSubstitution.admit');
    },
  ],
  [
    'evaluateSubstitution.forgedRefuse',
    () => {
      if (evaluateSubstitution(requirement, forged, context).state !== 'REFUSED')
        wrongVerdicts.push('evaluateSubstitution.forgedRefuse');
    },
  ],
  [
    'createSubstitutionReceipt',
    () => {
      const minted = createSubstitutionReceipt({
        requirement,
        candidate,
        context,
        receiptId: 'bench-mint',
        timestamp: 0,
      });
      if (minted.action !== 'part.substitution.admit')
        wrongVerdicts.push('createSubstitutionReceipt');
    },
  ],
  [
    'verifySubstitutionReceipt',
    () => {
      if (!verifySubstitutionReceipt(receipt, { requirement, candidate, context }).valid)
        wrongVerdicts.push('verifySubstitutionReceipt');
    },
  ],
  [
    'broker.execute',
    async () => {
      const result = await broker.execute({ operation: 'bench' });
      if (result.partAdmission.substitutionReceiptHash !== receipt.receiptHash)
        wrongVerdicts.push('broker.execute');
    },
  ],
];

function percentile(sorted, fraction) {
  return sorted[Math.min(sorted.length - 1, Math.floor(sorted.length * fraction))];
}

const results = {};
for (const [name, operation] of operations) {
  for (let i = 0; i < warmup; i += 1) await operation();
  const samples = new Array(iterations);
  for (let i = 0; i < iterations; i += 1) {
    const start = process.hrtime.bigint();
    await operation();
    samples[i] = Number(process.hrtime.bigint() - start);
  }
  samples.sort((a, b) => a - b);
  const medianNs = percentile(samples, 0.5);
  results[name] = {
    iterations,
    medianNs,
    p99Ns: percentile(samples, 0.99),
    opsPerSec: Math.round(1e9 / medianNs),
  };
}

const expectedExecutions = warmup + iterations;
if (delegate.executions !== expectedExecutions) {
  wrongVerdicts.push(
    `broker.delegateExecutions=${delegate.executions} expected=${expectedExecutions}`
  );
}

const report = {
  bench: 'interchangeable-parts',
  node: process.version,
  platform: `${process.platform}-${process.arch}`,
  iterations,
  results,
  wrongVerdicts: [...new Set(wrongVerdicts)],
};

if (args.has('check')) {
  const committed = JSON.parse(readFileSync(RECEIPT_PATH, 'utf8'));
  const refusals = [...report.wrongVerdicts.map(entry => `WRONG_VERDICT ${entry}`)];
  const measured = committed.subject?.kernel_blobs ?? {};
  for (const [path, blob] of Object.entries(kernelBlobs())) {
    if (measured[path] !== blob)
      refusals.push(
        `RECEIPT_SUBJECT_STALE ${path} measured=${measured[path] ?? 'NONE'} disk=${blob}`
      );
  }
  for (const [name, bound] of Object.entries(committed.bounds.medianNs)) {
    const observed = results[name]?.medianNs;
    if (observed === undefined) refusals.push(`OPERATION_MISSING ${name}`);
    else if (observed > bound)
      refusals.push(`REGRESSION ${name} medianNs=${observed} bound=${bound}`);
  }
  process.stdout.write(
    `${JSON.stringify({ ...report, status: refusals.length ? 'REFUSED' : 'ALIVE', refusals })}\n`
  );
  if (refusals.length) process.exit(1);
} else {
  process.stdout.write(`${JSON.stringify(report, null, args.has('json') ? 0 : 2)}\n`);
  if (report.wrongVerdicts.length) process.exit(1);
}
