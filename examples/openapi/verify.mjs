#!/usr/bin/env node
/**
 * @file Golden File Verification Script
 * @module examples/openapi/verify
 * @description Verifies sync command output against golden files
 */

import { readFile, writeFile, readdir, mkdir, rm } from 'fs/promises';
import { existsSync } from 'fs';
import { resolve, join, relative } from 'path';
import { fileURLToPath } from 'url';
import { spawn } from 'child_process';

const __dirname = fileURLToPath(new URL('.', import.meta.url));
const LIB_DIR = resolve(__dirname, 'lib');
const GOLDEN_DIR = resolve(__dirname, 'golden/lib');

/**
 * ANSI color codes for terminal output
 */
const c = {
  reset: '\x1b[0m',
  bold: '\x1b[1m',
  dim: '\x1b[2m',
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  cyan: '\x1b[36m',
};

// Disable colors if not TTY or NO_COLOR is set
const noColor = !process.stdout.isTTY || process.env.NO_COLOR;
if (noColor) {
  Object.keys(c).forEach((k) => (c[k] = ''));
}

/**
 * Normalizes generated content by removing timestamps and other variable parts
 * @param {string} content - File content
 * @returns {string} Normalized content
 */
function normalizeContent(content) {
  // Remove @generated timestamps (format: @generated YYYY-MM-DD HH:MM:SS)
  return content.replace(/@generated\s+\d{4}-\d{2}-\d{2}\s+\d{2}:\d{2}:\d{2}/g, '@generated [TIMESTAMP]');
}

/**
 * Recursively gets all files in a directory
 * @param {string} dir - Directory path
 * @param {string} [base=''] - Base path for relative paths
 * @returns {Promise<string[]>} Array of relative file paths
 */
async function getAllFiles(dir, base = '') {
  if (!existsSync(dir)) {
    return [];
  }

  const files = [];
  const entries = await readdir(dir, { withFileTypes: true });

  for (const entry of entries) {
    const relativePath = base ? join(base, entry.name) : entry.name;
    if (entry.isDirectory()) {
      const subFiles = await getAllFiles(join(dir, entry.name), relativePath);
      files.push(...subFiles);
    } else if (entry.isFile()) {
      files.push(relativePath);
    }
  }

  return files;
}

/**
 * Computes a simple line-by-line diff between two strings
 * @param {string} expected - Expected content
 * @param {string} actual - Actual content
 * @param {string} filename - Filename for context
 * @returns {string} Diff output
 */
function computeDiff(expected, actual, filename) {
  const expectedLines = expected.split('\n');
  const actualLines = actual.split('\n');
  const output = [];

  output.push(`${c.bold}--- golden/${filename}${c.reset}`);
  output.push(`${c.bold}+++ lib/${filename}${c.reset}`);

  const maxLines = Math.max(expectedLines.length, actualLines.length);
  let diffStarted = false;
  let contextBefore = [];

  for (let i = 0; i < maxLines; i++) {
    const expectedLine = expectedLines[i] ?? '';
    const actualLine = actualLines[i] ?? '';

    if (expectedLine !== actualLine) {
      // Output context lines before diff
      if (!diffStarted && contextBefore.length > 0) {
        output.push(`${c.cyan}@@ -${Math.max(1, i - 2)} +${Math.max(1, i - 2)} @@${c.reset}`);
        contextBefore.forEach((ctx) => output.push(` ${ctx}`));
      }
      diffStarted = true;

      if (expectedLines[i] !== undefined) {
        output.push(`${c.red}-${expectedLine}${c.reset}`);
      }
      if (actualLines[i] !== undefined) {
        output.push(`${c.green}+${actualLine}${c.reset}`);
      }
    } else if (diffStarted) {
      // Show some context after diff
      output.push(` ${expectedLine}`);
      if (output.length > 50) {
        output.push(`${c.dim}... (truncated)${c.reset}`);
        break;
      }
    } else {
      // Keep last 3 lines as context
      contextBefore.push(expectedLine);
      if (contextBefore.length > 3) {
        contextBefore.shift();
      }
    }
  }

  return output.join('\n');
}

/**
 * Runs the sync command to generate files
 * @returns {Promise<boolean>} True if sync succeeded
 */
async function runSync() {
  return new Promise((resolve) => {
    console.log(`${c.cyan}Running sync command...${c.reset}\n`);

    const cliPath = fileURLToPath(new URL('../../packages/cli/src/cli/main.mjs', import.meta.url));
    const proc = spawn(
      'node',
      [cliPath, 'sync', '--config', 'ggen.toml', '--force'],
      {
        cwd: __dirname,
        stdio: 'inherit',
        env: { ...process.env, NO_COLOR: '1' },
      }
    );

    proc.on('close', (code) => {
      resolve(code === 0);
    });

    proc.on('error', (err) => {
      console.error(`${c.red}Failed to run sync:${c.reset} ${err.message}`);
      resolve(false);
    });
  });
}

/**
 * Updates golden files from current lib/ output
 * @returns {Promise<void>}
 */
async function updateGoldenFiles() {
  console.log(`${c.cyan}Updating golden files...${c.reset}\n`);

  // Clean up old golden files
  if (existsSync(GOLDEN_DIR)) {
    await rm(GOLDEN_DIR, { recursive: true });
  }
  await mkdir(GOLDEN_DIR, { recursive: true });

  const files = await getAllFiles(LIB_DIR);

  for (const file of files) {
    const srcPath = join(LIB_DIR, file);
    const destPath = join(GOLDEN_DIR, file);

    // Ensure destination directory exists
    await mkdir(resolve(destPath, '..'), { recursive: true });

    // Read, normalize, and write
    const content = await readFile(srcPath, 'utf-8');
    const normalized = normalizeContent(content);
    await writeFile(destPath, normalized, 'utf-8');

    console.log(`  ${c.green}Updated:${c.reset} golden/lib/${file}`);
  }

  console.log(`\n${c.green}Golden files updated successfully!${c.reset}`);
}

/**
 * Verifies generated files against golden files
 * @returns {Promise<{passed: boolean, mismatches: string[]}>}
 */
async function verifyFiles() {
  const goldenFiles = await getAllFiles(GOLDEN_DIR);
  const generatedFiles = await getAllFiles(LIB_DIR);

  const mismatches = [];
  const onlyInGolden = [];
  const onlyInGenerated = [];

  // Find files only in golden
  for (const file of goldenFiles) {
    if (!generatedFiles.includes(file)) {
      onlyInGolden.push(file);
    }
  }

  // Find files only in generated
  for (const file of generatedFiles) {
    if (!goldenFiles.includes(file)) {
      onlyInGenerated.push(file);
    }
  }

  // Compare common files
  for (const file of goldenFiles) {
    if (!generatedFiles.includes(file)) continue;

    const goldenPath = join(GOLDEN_DIR, file);
    const generatedPath = join(LIB_DIR, file);

    const goldenContent = await readFile(goldenPath, 'utf-8');
    const generatedContent = normalizeContent(await readFile(generatedPath, 'utf-8'));

    if (goldenContent !== generatedContent) {
      mismatches.push({
        file,
        diff: computeDiff(goldenContent, generatedContent, file),
      });
    }
  }

  return { mismatches, onlyInGolden, onlyInGenerated };
}

/**
 * Main entry point
 */
async function main() {
  const args = process.argv.slice(2);
  const updateMode = args.includes('--update') || args.includes('-u');
  const skipSync = args.includes('--skip-sync') || args.includes('-s');
  const helpMode = args.includes('--help') || args.includes('-h');

  if (helpMode) {
    console.log(`
${c.bold}Golden File Verification Script${c.reset}

Usage: node verify.mjs [options]

Options:
  --update, -u     Update golden files from current lib/ output
  --skip-sync, -s  Skip running sync command (use existing lib/ files)
  --help, -h       Show this help message

Examples:
  node verify.mjs              # Run sync and verify against golden files
  node verify.mjs --update     # Update golden files with current output
  node verify.mjs --skip-sync  # Verify existing lib/ without re-running sync

Exit codes:
  0 - All files match golden files
  1 - Mismatches found or verification failed
`);
    process.exit(0);
  }

  console.log(`\n${c.bold}${c.cyan}UNRDF Sync Golden File Verification${c.reset}\n`);

  // Run sync unless skipped
  if (!skipSync) {
    const syncSuccess = await runSync();
    if (!syncSuccess) {
      console.error(`\n${c.red}Sync command failed!${c.reset}`);
      process.exit(1);
    }
    console.log('');
  }

  // Update mode: copy lib/ to golden/
  if (updateMode) {
    await updateGoldenFiles();
    process.exit(0);
  }

  // Verify mode: compare lib/ against golden/
  console.log(`${c.cyan}Verifying against golden files...${c.reset}\n`);

  const { mismatches, onlyInGolden, onlyInGenerated } = await verifyFiles();

  // Report results
  let hasErrors = false;

  if (onlyInGolden.length > 0) {
    hasErrors = true;
    console.log(`${c.red}Files missing from generated output:${c.reset}`);
    for (const file of onlyInGolden) {
      console.log(`  ${c.red}-${c.reset} ${file}`);
    }
    console.log('');
  }

  if (onlyInGenerated.length > 0) {
    hasErrors = true;
    console.log(`${c.yellow}Extra files in generated output:${c.reset}`);
    for (const file of onlyInGenerated) {
      console.log(`  ${c.yellow}+${c.reset} ${file}`);
    }
    console.log('');
  }

  if (mismatches.length > 0) {
    hasErrors = true;
    console.log(`${c.red}Content mismatches:${c.reset}\n`);
    for (const { file, diff } of mismatches) {
      console.log(`${c.bold}File: ${file}${c.reset}`);
      console.log(diff);
      console.log('');
    }
  }

  if (hasErrors) {
    console.log(`${c.red}${c.bold}Verification FAILED${c.reset}`);
    console.log(`\nTo update golden files, run: ${c.cyan}node verify.mjs --update${c.reset}\n`);
    process.exit(1);
  } else {
    const goldenFiles = await getAllFiles(GOLDEN_DIR);
    console.log(`${c.green}${c.bold}Verification PASSED${c.reset}`);
    console.log(`  ${goldenFiles.length} file(s) verified\n`);
    process.exit(0);
  }
}

main().catch((err) => {
  console.error(`${c.red}Unexpected error:${c.reset} ${err.message}`);
  if (process.env.DEBUG) {
    console.error(err.stack);
  }
  process.exit(1);
});
