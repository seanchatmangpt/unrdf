/**
 * @fileoverview LaTeX CLI Command Tests
 *
 * Tests the command-line interface for LaTeX operations:
 * - `kgc latex build` command
 * - `kgc latex diagnose` command
 * - Exit codes and error handling
 * - JSON envelope output
 *
 * @module test/latex-cli
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
import { join } from 'node:path';
import { promises as fs } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { dirname } from 'node:path';

import { createTempDir, cleanupTempDir, validatePDFFormat } from './fixtures/setup.mjs';

const execFileAsync = promisify(execFile);

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const FIXTURES_DIR = join(__dirname, 'fixtures', 'latex');

// Path to kgc CLI (assumes we're in packages/kgc-cli/test/)
const KGC_CLI = join(__dirname, '..', 'src', 'cli.mjs');

let tempCacheDir;
let tempOutputDir;

beforeEach(async () => {
  tempCacheDir = await createTempDir('cli-cache-');
  tempOutputDir = await createTempDir('cli-output-');
});

afterEach(async () => {
  await cleanupTempDir(tempCacheDir);
  await cleanupTempDir(tempOutputDir);
});

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/**
 * Execute kgc CLI command
 *
 * @param {string[]} args - Command arguments
 * @param {Object} [options={}] - Execution options
 * @returns {Promise<{stdout: string, stderr: string, exitCode: number}>}
 */
async function runKGC(args, options = {}) {
  try {
    const { stdout, stderr } = await execFileAsync('node', [KGC_CLI, ...args], {
      timeout: 10000,
      maxBuffer: 10 * 1024 * 1024, // 10MB buffer for PDF output
      ...options
    });
    return { stdout, stderr, exitCode: 0 };
  } catch (error) {
    // execFile throws on non-zero exit, but we want to test error cases
    return {
      stdout: error.stdout || '',
      stderr: error.stderr || '',
      exitCode: error.code || 1
    };
  }
}

// ============================================================================
// BUILD COMMAND TESTS
// ============================================================================

describe('kgc latex build', () => {
  it('should compile minimal.tex successfully', async () => {
    const inputPath = join(FIXTURES_DIR, 'minimal.tex');
    const outputPath = join(tempOutputDir, 'output.pdf');

    const result = await runKGC([
      'latex',
      'build',
      '--input', inputPath,
      '--output', outputPath,
      '--cache', tempCacheDir
    ]);

    expect(result.exitCode).toBe(0);

    // Verify PDF was created
    const pdfBytes = await fs.readFile(outputPath);
    const validation = validatePDFFormat(pdfBytes);
    expect(validation.valid).toBe(true);
  });

  it('should support --passes option for multi-pass compilation', async () => {
    const inputPath = join(FIXTURES_DIR, 'with-refs', 'main.tex');
    const outputPath = join(tempOutputDir, 'refs.pdf');

    const result = await runKGC([
      'latex',
      'build',
      '--input', inputPath,
      '--output', outputPath,
      '--cache', tempCacheDir,
      '--passes', '2'
    ]);

    expect(result.exitCode).toBe(0);

    const pdfBytes = await fs.readFile(outputPath);
    expect(validatePDFFormat(pdfBytes).valid).toBe(true);
  });

  it('should support --engine option', async () => {
    const inputPath = join(FIXTURES_DIR, 'minimal.tex');
    const outputPath = join(tempOutputDir, 'minimal.pdf');

    const result = await runKGC([
      'latex',
      'build',
      '--input', inputPath,
      '--output', outputPath,
      '--cache', tempCacheDir,
      '--engine', 'pdflatex'
    ]);

    expect(result.exitCode).toBe(0);
  });

  it('should return non-zero exit code on compilation error', async () => {
    const inputPath = join(FIXTURES_DIR, 'missing-package.tex');
    const outputPath = join(tempOutputDir, 'error.pdf');

    const result = await runKGC([
      'latex',
      'build',
      '--input', inputPath,
      '--output', outputPath,
      '--cache', tempCacheDir
    ]);

    expect(result.exitCode).not.toBe(0);
    expect(result.stderr).toContain('Error');
  });

  it('should output JSON envelope with --json flag', async () => {
    const inputPath = join(FIXTURES_DIR, 'minimal.tex');
    const outputPath = join(tempOutputDir, 'output.pdf');

    const result = await runKGC([
      'latex',
      'build',
      '--input', inputPath,
      '--output', outputPath,
      '--cache', tempCacheDir,
      '--json'
    ]);

    // Parse JSON output
    const json = JSON.parse(result.stdout);
    expect(json).toHaveProperty('ok');
    expect(json).toHaveProperty('data');
    expect(json).toHaveProperty('meta');
    expect(json.ok).toBe(true);
  });

  it('should handle non-existent input file gracefully', async () => {
    const inputPath = join(FIXTURES_DIR, 'does-not-exist.tex');
    const outputPath = join(tempOutputDir, 'output.pdf');

    const result = await runKGC([
      'latex',
      'build',
      '--input', inputPath,
      '--output', outputPath,
      '--cache', tempCacheDir
    ]);

    expect(result.exitCode).not.toBe(0);
  });
});

// ============================================================================
// DIAGNOSE COMMAND TESTS
// ============================================================================

describe('kgc latex diagnose', () => {
  it('should analyze minimal.tex and report no issues', async () => {
    const inputPath = join(FIXTURES_DIR, 'minimal.tex');

    const result = await runKGC([
      'latex',
      'diagnose',
      '--input', inputPath,
      '--cache', tempCacheDir
    ]);

    expect(result.exitCode).toBe(0);
    expect(result.stdout).toContain('No issues found');
  });

  it('should detect missing packages', async () => {
    const inputPath = join(FIXTURES_DIR, 'missing-package.tex');

    const result = await runKGC([
      'latex',
      'diagnose',
      '--input', inputPath,
      '--cache', tempCacheDir
    ]);

    expect(result.stdout).toContain('missing');
  });

  it('should output JSON with --json flag', async () => {
    const inputPath = join(FIXTURES_DIR, 'minimal.tex');

    const result = await runKGC([
      'latex',
      'diagnose',
      '--input', inputPath,
      '--cache', tempCacheDir,
      '--json'
    ]);

    const json = JSON.parse(result.stdout);
    expect(json).toHaveProperty('ok');
    expect(json).toHaveProperty('data');
  });
});

// ============================================================================
// EXIT CODE TESTS
// ============================================================================

describe('Exit Codes', () => {
  it('should exit with 0 on successful compilation', async () => {
    const inputPath = join(FIXTURES_DIR, 'minimal.tex');
    const outputPath = join(tempOutputDir, 'output.pdf');

    const result = await runKGC([
      'latex',
      'build',
      '--input', inputPath,
      '--output', outputPath,
      '--cache', tempCacheDir
    ]);

    expect(result.exitCode).toBe(0);
  });

  it('should exit with 1 on LaTeX compilation error', async () => {
    const inputPath = join(FIXTURES_DIR, 'missing-package.tex');
    const outputPath = join(tempOutputDir, 'error.pdf');

    const result = await runKGC([
      'latex',
      'build',
      '--input', inputPath,
      '--output', outputPath,
      '--cache', tempCacheDir
    ]);

    expect(result.exitCode).toBe(1);
  });

  it('should exit with 1 on invalid arguments', async () => {
    const result = await runKGC([
      'latex',
      'build',
      '--invalid-flag'
    ]);

    expect(result.exitCode).toBe(1);
  });
});

// ============================================================================
// HELP TEXT TESTS
// ============================================================================

describe('Help Text', () => {
  it('should display help for latex command', async () => {
    const result = await runKGC(['latex', '--help']);

    expect(result.stdout).toContain('latex');
    expect(result.exitCode).toBe(0);
  });

  it('should display help for latex build', async () => {
    const result = await runKGC(['latex', 'build', '--help']);

    expect(result.stdout).toContain('build');
    expect(result.exitCode).toBe(0);
  });
});
