#!/usr/bin/env node
/**
 * Compatibility entrypoint for the package projection surface.
 * Generation authority moved from hand-written JS string assembly to ggen.
 *
 * Important boundary: observation standing describes the PRODUCT graph. A
 * BUILD_BROKEN product graph is still valid O* for deterministic projection;
 * only failure to manufacture the observation artifacts blocks ggen.
 */
import { spawnSync } from 'node:child_process';
import { existsSync } from 'node:fs';
import path from 'node:path';
import process from 'node:process';
import { fileURLToPath } from 'node:url';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const ggen = process.env.GGEN_BIN || 'ggen';
const observationArtifacts = [
  '.artifacts/package-observation/package-topology.ttl',
  '.artifacts/package-observation/receipt.json',
];

const observe = spawnSync(process.execPath, ['scripts/unrdf-package-discovery.mjs'], {
  cwd: root,
  encoding: 'utf8',
  stdio: 'inherit',
  timeout: 30000,
});
const observationMissing = observationArtifacts.filter(file => !existsSync(path.join(root, file)));
if (observe.error || observationMissing.length) {
  console.error(`PACKAGE_OBSERVATION_BLOCKED: ${observe.error?.message || `missing artifacts: ${observationMissing.join(', ')}`}`);
  process.exit(2);
}
if (observe.status !== 0) {
  console.error(`PACKAGE_PRODUCT_STANDING_NONZERO: observation exit ${observe.status}; projecting emitted O* without granting product ALIVE`);
}

const result = spawnSync(ggen, ['sync', 'run'], {
  cwd: root,
  encoding: 'utf8',
  stdio: 'inherit',
  timeout: +(process.env.GGEN_TIMEOUT_MS || 120000),
});

if (result.error?.code === 'ENOENT') {
  console.error('GGEN_NOT_AVAILABLE: install ggen v26.8.12+ or set GGEN_BIN');
  process.exit(2);
}
if (result.error) {
  console.error(`GGEN_EXECUTION_BLOCKED: ${result.error.message}`);
  process.exit(2);
}
if (result.status !== 0) process.exit(result.status ?? 1);

const required = ['src/generated/package-exports.mjs'];
const missing = required.filter(file => !existsSync(path.join(root, file)));
if (missing.length) {
  console.error(`GGEN_PROJECTION_MISSING: ${missing.join(', ')}`);
  process.exit(1);
}

const { ALL_PACKAGES } = await import(`../src/generated/package-exports.mjs?run=${Date.now()}`);
console.log(`GGEN_PACKAGE_PROJECTION ${JSON.stringify({ standing: 'ALIVE', observationExit: observe.status ?? 0, packageCount: ALL_PACKAGES.length, generated: required })}`);
