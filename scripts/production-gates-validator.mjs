#!/usr/bin/env node
/**
 * @file Production Gates Validator
 * @description Executes ten production gates and emits a command-bound JSON receipt.
 */

import { createHash } from 'node:crypto';
import { existsSync, readFileSync, writeFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { spawnSync } from 'node:child_process';
import process from 'node:process';

const ROOT = resolve(process.cwd());
const REPORT_PATH = resolve(ROOT, 'production-gates-report.json');
const RECEIPT_SCHEMA = 'urn:unrdf:production-gates-receipt:v2';

function digest(value) {
  return createHash('sha256').update(value).digest('hex');
}

function stable(value) {
  if (Array.isArray(value)) return value.map(stable);
  if (value && typeof value === 'object') {
    return Object.fromEntries(Object.keys(value).sort().map(key => [key, stable(value[key])]));
  }
  return value;
}

function currentRevision() {
  const result = spawnSync('git', ['rev-parse', 'HEAD'], {
    cwd: ROOT,
    encoding: 'utf8',
    timeout: 5_000,
  });
  if (result.status === 0) return result.stdout.trim();
  return process.env.GITHUB_SHA || null;
}

function execute(command, args, timeoutMs) {
  const startedAt = Date.now();
  const result = spawnSync(command, args, {
    cwd: ROOT,
    env: { ...process.env, CI: '1', NO_COLOR: '1' },
    encoding: 'utf8',
    timeout: timeoutMs,
    maxBuffer: 32 * 1024 * 1024,
  });
  const stdout = result.stdout || '';
  const stderr = result.stderr || '';
  const execution = {
    command: [command, ...args],
    exitCode: result.status,
    signal: result.signal,
    timedOut: result.error?.code === 'ETIMEDOUT',
    spawnError: result.error ? result.error.message : null,
    durationMs: Date.now() - startedAt,
    stdoutBytes: Buffer.byteLength(stdout),
    stderrBytes: Buffer.byteLength(stderr),
    stdoutDigest: digest(stdout),
    stderrDigest: digest(stderr),
    stdoutTail: stdout.slice(-4000),
    stderrTail: stderr.slice(-4000),
  };

  Object.defineProperties(execution, {
    stdout: { value: stdout, enumerable: false },
    stderr: { value: stderr, enumerable: false },
  });
  return execution;
}

function commandFailureReason(execution) {
  if (execution.timedOut) return 'COMMAND_TIMED_OUT';
  if (execution.spawnError) return 'COMMAND_SPAWN_FAILED';
  if (execution.exitCode !== 0) return 'COMMAND_EXIT_NONZERO';
  return null;
}

function inspectSafely(inspect, execution) {
  try {
    return inspect(execution);
  } catch (error) {
    return {
      ok: false,
      reason: 'INSPECTOR_EXCEPTION',
      evidence: {
        name: error?.name || 'Error',
        message: error?.message || String(error),
      },
    };
  }
}

function commandGate(number, name, command, args, timeoutMs, inspect = null, options = {}) {
  return {
    number,
    name,
    run() {
      const execution = execute(command, args, timeoutMs);
      const failureReason = commandFailureReason(execution);
      const inspection = inspect && (!failureReason || options.inspectOnFailure)
        ? inspectSafely(inspect, execution)
        : null;

      if (failureReason) {
        return {
          status: 'FAIL',
          reason: inspection && !inspection.ok ? inspection.reason : failureReason,
          execution,
          ...(inspection?.evidence === undefined ? {} : { evidence: inspection.evidence }),
        };
      }
      if (inspection && !inspection.ok) {
        return {
          status: 'FAIL',
          reason: inspection.reason,
          execution,
          evidence: inspection.evidence,
        };
      }
      return {
        status: 'PASS',
        execution,
        ...(inspection?.evidence === undefined ? {} : { evidence: inspection.evidence }),
      };
    },
  };
}

function parseCoverage() {
  const path = resolve(ROOT, 'coverage/coverage-summary.json');
  if (!existsSync(path)) return { ok: false, reason: 'COVERAGE_REPORT_MISSING' };
  try {
    const total = JSON.parse(readFileSync(path, 'utf8')).total;
    const metrics = Object.fromEntries(
      ['lines', 'functions', 'branches', 'statements'].map(name => [name, total?.[name]?.pct])
    );
    if (Object.values(metrics).some(value => typeof value !== 'number' || !Number.isFinite(value))) {
      return { ok: false, reason: 'COVERAGE_REPORT_INVALID', evidence: metrics };
    }
    const belowThreshold = Object.entries(metrics).filter(([, value]) => value < 80);
    return belowThreshold.length === 0
      ? { ok: true, evidence: { threshold: 80, metrics } }
      : {
          ok: false,
          reason: 'COVERAGE_BELOW_THRESHOLD',
          evidence: { threshold: 80, metrics, belowThreshold },
        };
  } catch (error) {
    return {
      ok: false,
      reason: 'COVERAGE_REPORT_INVALID',
      evidence: { name: error.name, message: error.message },
    };
  }
}

function inspectOtel(execution) {
  const output = `${execution.stdout}\n${execution.stderr}`;
  const scoreMatch = output.match(/Score:\s*(\d+)\/100/i);
  if (!scoreMatch) return { ok: false, reason: 'OTEL_SCORE_NOT_OBSERVED' };
  const score = Number(scoreMatch[1]);
  return score >= 80
    ? { ok: true, evidence: { score, threshold: 80 } }
    : { ok: false, reason: 'OTEL_SCORE_BELOW_THRESHOLD', evidence: { score, threshold: 80 } };
}

function inspectWip(execution) {
  try {
    const audit = JSON.parse(execution.stdout);
    const actionable = audit.summary?.actionable;
    const digestValid = /^[a-f0-9]{64}$/.test(audit.digest || '');
    return audit.standing === 'ALIVE' && actionable === 0 && digestValid
      ? { ok: true, evidence: { standing: audit.standing, actionable, digest: audit.digest } }
      : {
          ok: false,
          reason: 'WIP_AUDIT_NOT_ALIVE',
          evidence: { standing: audit.standing, actionable, digestValid },
        };
  } catch (error) {
    return {
      ok: false,
      reason: 'WIP_AUDIT_JSON_NOT_OBSERVED',
      evidence: { name: error.name, message: error.message },
    };
  }
}

function inspectAudit(execution) {
  try {
    const audit = JSON.parse(execution.stdout);
    if (audit.error) {
      return {
        ok: false,
        reason: 'AUDIT_UNAVAILABLE',
        evidence: {
          code: audit.error.code || null,
          summary: audit.error.summary || audit.error.message || null,
        },
      };
    }
    const vulnerabilities = audit.metadata?.vulnerabilities;
    if (!vulnerabilities || typeof vulnerabilities !== 'object') {
      return { ok: false, reason: 'AUDIT_REPORT_INVALID' };
    }
    const high = vulnerabilities.high || 0;
    const critical = vulnerabilities.critical || 0;
    return high === 0 && critical === 0
      ? { ok: true, evidence: { high, critical } }
      : { ok: false, reason: 'HIGH_OR_CRITICAL_VULNERABILITIES', evidence: { high, critical } };
  } catch (error) {
    return {
      ok: false,
      reason: 'AUDIT_JSON_NOT_OBSERVED',
      evidence: { name: error.name, message: error.message },
    };
  }
}

const gates = [
  commandGate(1, 'Test Suite', 'pnpm', ['test'], 60_000),
  commandGate(2, 'OTEL Score', process.execPath, ['validation/run-all.mjs', 'comprehensive'], 30_000, inspectOtel),
  commandGate(3, 'ESLint', 'pnpm', ['lint'], 60_000),
  commandGate(4, 'Coverage', 'pnpm', ['test:coverage'], 300_000, parseCoverage),
  commandGate(5, 'Performance Benchmarks', 'pnpm', ['benchmark:core'], 60_000),
  commandGate(6, 'Examples', process.execPath, ['scripts/validate-all-examples.mjs'], 120_000),
  commandGate(7, 'Build', 'pnpm', ['build'], 120_000),
  commandGate(
    8,
    'Executable WIP Audit',
    process.execPath,
    ['scripts/audit-wip.mjs', '--scope', 'packages', '--scope', 'scripts', '--json'],
    60_000,
    inspectWip
  ),
  commandGate(
    9,
    'Security Audit',
    'pnpm',
    ['audit', '--audit-level', 'high', '--json'],
    120_000,
    inspectAudit,
    { inspectOnFailure: true }
  ),
  commandGate(10, 'Documentation Accuracy', process.execPath, ['scripts/validate-docs.mjs'], 120_000),
];

function renderSummary(report) {
  console.log(`Production gates: ${report.standing}`);
  for (const gate of report.gates) {
    console.log(`${gate.status.padEnd(4)} ${String(gate.number).padStart(2)} ${gate.name}${gate.reason ? ` — ${gate.reason}` : ''}`);
  }
  console.log(`Receipt: ${report.digest}`);
}

function verifyReport(report) {
  if (!report || typeof report !== 'object' || Array.isArray(report)) {
    return { valid: false, reason: 'REPORT_NOT_OBJECT' };
  }
  if (report.schema !== RECEIPT_SCHEMA) {
    return { valid: false, reason: 'REPORT_SCHEMA_MISMATCH' };
  }
  if (!/^[a-f0-9]{64}$/.test(report.digest || '')) {
    return { valid: false, reason: 'REPORT_DIGEST_INVALID' };
  }
  const { digest: observed, ...body } = report;
  const expected = digest(JSON.stringify(stable(body)));
  return observed === expected
    ? { valid: true }
    : { valid: false, reason: 'REPORT_DIGEST_MISMATCH', expected, observed };
}

function runGate(gate) {
  try {
    return {
      number: gate.number,
      name: gate.name,
      ...gate.run(),
    };
  } catch (error) {
    return {
      number: gate.number,
      name: gate.name,
      status: 'FAIL',
      reason: 'GATE_EXECUTION_EXCEPTION',
      evidence: {
        name: error?.name || 'Error',
        message: error?.message || String(error),
      },
    };
  }
}

function main() {
  const args = process.argv.slice(2);
  if (args.includes('--summary')) {
    if (!existsSync(REPORT_PATH)) {
      console.error('No production-gates-report.json exists.');
      process.exit(2);
    }
    try {
      const report = JSON.parse(readFileSync(REPORT_PATH, 'utf8'));
      const verification = verifyReport(report);
      if (!verification.valid) {
        console.error(`Receipt verification failed: ${verification.reason}`);
        process.exit(2);
      }
      renderSummary(report);
      process.exit(report.standing === 'ALIVE' ? 0 : 1);
    } catch (error) {
      console.error(`Receipt verification failed: ${error.message}`);
      process.exit(2);
    }
  }

  const selected = args.find(argument => argument.startsWith('--gate='));
  const gateNumber = selected ? Number(selected.slice('--gate='.length)) : null;
  if (selected && (!Number.isInteger(gateNumber) || !gates.some(gate => gate.number === gateNumber))) {
    console.error(`Unknown gate: ${selected}`);
    process.exit(2);
  }

  const admittedGates = gateNumber ? gates.filter(gate => gate.number === gateNumber) : gates;
  const observations = admittedGates.map(runGate);
  const receiptWithoutDigest = {
    schema: RECEIPT_SCHEMA,
    subject: {
      repository: process.env.GITHUB_REPOSITORY || null,
      root: '.',
      revision: currentRevision(),
      selectedGate: gateNumber,
      gateCount: admittedGates.length,
    },
    standing: observations.every(gate => gate.status === 'PASS') ? 'ALIVE' : 'BUILD_BROKEN',
    gates: observations,
  };
  const report = {
    ...receiptWithoutDigest,
    digest: digest(JSON.stringify(stable(receiptWithoutDigest))),
  };
  writeFileSync(REPORT_PATH, `${JSON.stringify(report, null, 2)}\n`);
  renderSummary(report);
  process.exit(report.standing === 'ALIVE' ? 0 : 1);
}

main();
