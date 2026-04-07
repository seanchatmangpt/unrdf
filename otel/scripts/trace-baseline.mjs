#!/usr/bin/env node
/**
 * @file Trace Baseline Comparison Script
 * @module otel/scripts/trace-baseline
 *
 * Compares two OTLP trace JSON files (baseline and current) and reports
 * regressions: new/missing services, new/missing operations, span count
 * deltas.
 *
 * Usage:
 *   node otel/scripts/trace-baseline.mjs baseline.json current.json
 *   node otel/scripts/trace-baseline.mjs --json baseline.json current.json > report.json
 *
 * Exit codes:
 *   0 - No regressions detected
 *   1 - Regressions detected (missing services, missing operations, etc.)
 */

import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/**
 * Extract a summary from an OTLP trace JSON blob.
 * OTLP JSON is an array of { resourceSpans: [...] } or a single such object.
 */
function extractTraceSummary(data) {
  // OTLP JSON can be { resourceSpans: [...] } (single export) or [{ resourceSpans: [...] }] (batch)
  let resourceSpans;
  if (Array.isArray(data) && data.length > 0 && data[0]?.resourceSpans) {
    resourceSpans = data.flatMap(d => d.resourceSpans ?? []);
  } else if (data?.resourceSpans) {
    resourceSpans = data.resourceSpans;
  } else {
    resourceSpans = Array.isArray(data) ? data : [data];
  }

  const services = new Map(); // serviceName -> { operations, spanCount }

  for (const rs of resourceSpans) {
    const attrs = rs?.resource?.attributes ?? [];
    const svcAttr = attrs.find(a => a.key === 'service.name');
    const serviceName = svcAttr?.value?.stringValue ?? 'unknown';

    if (!services.has(serviceName)) {
      services.set(serviceName, { operations: new Map(), spanCount: 0 });
    }
    const svc = services.get(serviceName);

    for (const ss of rs?.scopeSpans ?? []) {
      for (const span of ss?.spans ?? []) {
        svc.spanCount++;
        const opName = span.name ?? '<unnamed>';
        svc.operations.set(opName, (svc.operations.get(opName) ?? 0) + 1);
      }
    }
  }

  // Convert Maps to plain objects for JSON output
  const result = {};
  for (const [name, info] of services) {
    result[name] = {
      spanCount: info.spanCount,
      operations: Object.fromEntries(info.operations),
    };
  }
  return result;
}

/**
 * Compute a diff between two trace summaries.
 */
function diffSummaries(baseline, current) {
  const baselineServices = new Set(Object.keys(baseline));
  const currentServices = new Set(Object.keys(current));

  const newServices = [...currentServices].filter(s => !baselineServices.has(s));
  const missingServices = [...baselineServices].filter(s => !currentServices.has(s));

  const commonServices = [...baselineServices].filter(s => currentServices.has(s));

  const newOperations = [];
  const missingOperations = [];
  const spanCountDeltas = {};

  for (const svc of commonServices) {
    const bOps = new Set(Object.keys(baseline[svc].operations));
    const cOps = new Set(Object.keys(current[svc].operations));

    for (const op of cOps) {
      if (!bOps.has(op)) {
        newOperations.push({ service: svc, operation: op, count: current[svc].operations[op] });
      }
    }

    for (const op of bOps) {
      if (!cOps.has(op)) {
        missingOperations.push({ service: svc, operation: op, count: baseline[svc].operations[op] });
      }
    }

    spanCountDeltas[svc] = {
      baseline: baseline[svc].spanCount,
      current: current[svc].spanCount,
      delta: current[svc].spanCount - baseline[svc].spanCount,
    };
  }

  const hasRegressions = missingServices.length > 0 || missingOperations.length > 0;

  return {
    newServices,
    missingServices,
    newOperations,
    missingOperations,
    spanCountDeltas,
    hasRegressions,
  };
}

// ---------------------------------------------------------------------------
// Output formatters
// ---------------------------------------------------------------------------

function formatTextReport(diff, baselineFile, currentFile) {
  const lines = [];
  const SEP = '='.repeat(64);

  lines.push(SEP);
  lines.push('TRACE BASELINE COMPARISON');
  lines.push(SEP);
  lines.push(`  Baseline : ${baselineFile}`);
  lines.push(`  Current  : ${currentFile}`);
  lines.push('');

  // Services
  lines.push('--- Services ---');
  if (diff.missingServices.length === 0 && diff.newServices.length === 0) {
    lines.push('  No service changes detected.');
  } else {
    for (const svc of diff.missingServices) {
      lines.push(`  MISSING : ${svc}`);
    }
    for (const svc of diff.newServices) {
      lines.push(`  NEW     : ${svc}`);
    }
  }
  lines.push('');

  // Operations
  lines.push('--- Operations ---');
  if (diff.missingOperations.length === 0 && diff.newOperations.length === 0) {
    lines.push('  No operation changes detected.');
  } else {
    for (const { service, operation, count } of diff.missingOperations) {
      lines.push(`  MISSING : ${service} / ${operation} (was ${count} spans)`);
    }
    for (const { service, operation, count } of diff.newOperations) {
      lines.push(`  NEW     : ${service} / ${operation} (${count} spans)`);
    }
  }
  lines.push('');

  // Span counts
  lines.push('--- Span Counts ---');
  const colLen = Math.max(24, ...Object.keys(diff.spanCountDeltas).map(k => k.length + 2));
  lines.push(`  ${'Service'.padEnd(colLen)} ${'Baseline'.padStart(10)} ${'Current'.padStart(10)} ${'Delta'.padStart(10)}`);
  lines.push(`  ${'-'.repeat(colLen + 30)}`);
  for (const [svc, counts] of Object.entries(diff.spanCountDeltas)) {
    const deltaStr = counts.delta >= 0 ? `+${counts.delta}` : String(counts.delta);
    lines.push(`  ${svc.padEnd(colLen)} ${String(counts.baseline).padStart(10)} ${String(counts.current).padStart(10)} ${deltaStr.padStart(10)}`);
  }
  lines.push('');

  // Verdict
  lines.push(SEP);
  if (diff.hasRegressions) {
    lines.push('RESULT: REGRESSIONS DETECTED');
    lines.push(SEP);
  } else {
    lines.push('RESULT: NO REGRESSIONS');
    lines.push(SEP);
  }

  return lines.join('\n');
}

function formatJsonReport(diff, baselineFile, currentFile) {
  return JSON.stringify({
    baseline: baselineFile,
    current: currentFile,
    timestamp: new Date().toISOString(),
    hasRegressions: diff.hasRegressions,
    newServices: diff.newServices,
    missingServices: diff.missingServices,
    newOperations: diff.newOperations,
    missingOperations: diff.missingOperations,
    spanCountDeltas: diff.spanCountDeltas,
  }, null, 2);
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

async function main() {
  const args = process.argv.slice(2);
  const jsonFlag = args.includes('--json');
  const positional = args.filter(a => !a.startsWith('--'));

  if (positional.length !== 2) {
    console.error('Usage: node otel/scripts/trace-baseline.mjs [--json] <baseline.json> <current.json>');
    process.exit(2);
  }

  const [baselinePath, currentPath] = positional.map(p => resolve(p));

  let baselineData, currentData;

  try {
    baselineData = JSON.parse(await readFile(baselinePath, 'utf-8'));
  } catch (err) {
    console.error(`Error reading baseline file: ${err.message}`);
    process.exit(2);
  }

  try {
    currentData = JSON.parse(await readFile(currentPath, 'utf-8'));
  } catch (err) {
    console.error(`Error reading current file: ${err.message}`);
    process.exit(2);
  }

  const baselineSummary = extractTraceSummary(baselineData);
  const currentSummary = extractTraceSummary(currentData);
  const diff = diffSummaries(baselineSummary, currentSummary);

  if (jsonFlag) {
    console.log(formatJsonReport(diff, baselinePath, currentPath));
  } else {
    console.log(formatTextReport(diff, baselinePath, currentPath));
  }

  process.exit(diff.hasRegressions ? 1 : 0);
}

main().catch(err => {
  console.error(`Fatal: ${err.message}`);
  process.exit(2);
});
