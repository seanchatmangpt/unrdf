#!/usr/bin/env node
/**
 * Compatibility entrypoint for the package projection surface.
 * Generation authority moved from hand-written JS string assembly to ggen.
 */
import { spawnSync } from 'node:child_process';
import { existsSync } from 'node:fs';
import path from 'node:path';
import process from 'node:process';
import { fileURLToPath } from 'node:url';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const ggen = process.env.GGEN_BIN || 'ggen';
const observe = spawnSync(process.execPath, ['scripts/unrdf-package-discovery.mjs'], { cwd: root, encoding: 'utf8', stdio: 'inherit', timeout: 30000 });
if (observe.status !== 0 || observe.error) {
  console.error(`PACKAGE_OBSERVATION_BLOCKED: ${observe.error?.message || `exit ${observe.status}`}`);
  process.exit(observe.status ?? 1);
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
console.log(`GGEN_PACKAGE_PROJECTION ${JSON.stringify({ standing: 'ALIVE', packageCount: ALL_PACKAGES.length, generated: required })}`);
