#!/usr/bin/env node

/**
 * AtomVM Node.js CLI
 *
 * Command-line interface for executing .avm files using AtomVM in Node.js.
 *
 * Usage: node src/cli.mjs <file.avm>
 */

import { AtomVMNodeRuntime } from './node-runtime.mjs';
import { existsSync } from 'fs';
import { join, resolve, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const rootDir = resolve(__dirname, '../');
const publicDir = join(rootDir, 'public');

const avmFile = process.argv[2];

if (!avmFile) {
  console.error('Usage: node src/cli.mjs <file.avm>');
  console.error('Example: node src/cli.mjs public/hello_world.avm');
  process.exit(1);
}

try {
  if (!existsSync(avmFile)) {
    console.error(`Error: File not found: ${avmFile}`);
    console.error(`Build a module first: pnpm run build:erlang <module>`);
    process.exit(1);
  }
} catch (error) {
  if (error.code === 'EACCES' || error.code === 'EPERM') {
    console.error(`Error: Permission denied accessing file: ${avmFile}`);
    process.exit(1);
  }
  throw error;
}

const runtime = new AtomVMNodeRuntime({
  log: (msg) => process.stdout.write(msg + '\n'),
  errorLog: (msg) => process.stderr.write(msg + '\n')
});

(async () => {
  try {
    await runtime.load();
    const result = await runtime.execute(avmFile);
    process.exit(result.exitCode || 0);
  } catch (error) {
    console.error(`Error: ${error.message}`);
    process.exit(1);
  }
})();

