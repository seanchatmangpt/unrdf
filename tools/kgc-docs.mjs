#!/usr/bin/env node
/**
 * @file KGC Docs Tool (workspace-level)
 * @module tools/kgc-docs
 *
 * Wrapper tool for KGC documentation in the UNRDF workspace
 */

import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';
import { spawn } from 'node:child_process';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const kgcDocsBin = join(
  __dirname,
  '..',
  'packages',
  'kgc-docs',
  'bin',
  'kgc-docs.mjs'
);

/**
 * Execute kgc-docs CLI with workspace defaults
 */
async function main() {
  const args = process.argv.slice(2);

  const child = spawn('node', [kgcDocsBin, ...args], {
    stdio: 'inherit',
    cwd: join(__dirname, '..'),
  });

  child.on('exit', (code) => {
    process.exit(code || 0);
  });

  child.on('error', (error) => {
    console.error('Failed to execute kgc-docs:', error);
    process.exit(1);
  });
}

main();
