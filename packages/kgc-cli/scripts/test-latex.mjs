#!/usr/bin/env node
/**
 * @fileoverview LaTeX Pipeline Test Runner
 *
 * Runs all LaTeX pipeline tests with appropriate configuration.
 * Can be invoked directly or via package.json scripts.
 *
 * Usage:
 *   node scripts/test-latex.mjs [options]
 *   pnpm run test:latex
 *
 * Options:
 *   --coverage    Generate coverage report
 *   --watch       Watch mode for development
 *   --verbose     Verbose output
 *
 * @module scripts/test-latex
 */

import { spawn } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const packageRoot = join(__dirname, '..');

// Parse command-line arguments
const args = process.argv.slice(2);
const coverage = args.includes('--coverage');
const watch = args.includes('--watch');
const verbose = args.includes('--verbose');

// Build vitest command
const vitestArgs = [
  'vitest',
  watch ? 'watch' : 'run',
  '--root', packageRoot,
  '--config', join(packageRoot, 'vitest.config.mjs'),
];

// Add test file patterns
vitestArgs.push(
  'test/latex-pipeline.test.mjs',
  'test/latex-cli.test.mjs',
  'test/latex-build.test.mjs',
  'test/latex-diagnostics.test.mjs',
  'test/latex-vfs.test.mjs'
);

// Add coverage if requested
if (coverage) {
  vitestArgs.push('--coverage');
}

// Add verbose if requested
if (verbose) {
  vitestArgs.push('--reporter=verbose');
}

// Add timeout configuration (per CLAUDE.md SLAs)
vitestArgs.push('--testTimeout=10000'); // 10s for integration tests

console.log('Running LaTeX pipeline tests...');
if (coverage) console.log('Coverage: enabled');
if (watch) console.log('Watch mode: enabled');
console.log('');

// Execute vitest
const child = spawn('pnpm', vitestArgs, {
  stdio: 'inherit',
  cwd: packageRoot,
  shell: true
});

child.on('exit', (code) => {
  process.exit(code || 0);
});

child.on('error', (err) => {
  console.error('Failed to start test runner:', err);
  process.exit(1);
});
