#!/usr/bin/env node
/**
 * Simple test runner for streaming package
 * Bypasses vitest configuration issues
 */

import { spawn } from 'child_process';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));

const testFiles = [
  join(__dirname, 'test/streaming.test.mjs'),
  join(__dirname, 'test/v6-streaming.test.mjs'),
];

console.log('🧪 Running Streaming Package Tests\n');
console.log(`Test files: ${testFiles.length}\n`);

const vitest = spawn('npx', ['vitest', 'run', '--no-coverage', '--reporter=verbose', ...testFiles], {
  cwd: __dirname,
  stdio: 'inherit',
  env: {
    ...process.env,
    NODE_OPTIONS: '--experimental-vm-modules',
  },
});

vitest.on('exit', (code) => {
  console.log(`\nTest runner exited with code: ${code}`);
  process.exit(code);
});
