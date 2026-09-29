#!/usr/bin/env node
/**
 * @file Run `node --test` in a way that survives `pnpm -r test -- --coverage`.
 *
 * pnpm appends `-- --coverage` to every package's test script. Node 22 tolerates
 * that after the test files, but Node 20 treats `--` as a test file path and exits
 * 1 with "Could not find '<pkg>/--'". This runner drops the `--` separator and the
 * `--coverage` flag (node:test coverage is a different flag, --experimental-test-coverage),
 * and forwards every other argument (flags and files) to `node --test` unchanged.
 *
 * Usage: node ../../scripts/run-node-tests.mjs [node flags] <test files...>
 */
import { spawnSync } from 'node:child_process';

const DROPPED = new Set(['--', '--coverage']);
const forwarded = process.argv.slice(2).filter(arg => !DROPPED.has(arg));

const result = spawnSync(process.execPath, ['--test', ...forwarded], { stdio: 'inherit' });
if (result.error) throw result.error;
process.exit(result.status ?? 1);
