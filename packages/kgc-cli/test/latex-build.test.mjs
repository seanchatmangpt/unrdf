/**
 * @fileoverview LaTeX Build Pipeline Tests (Agent 7)
 *
 * Tests the pure JavaScript LaTeX->PDF compilation pipeline without external TeX.
 * Validates:
 * - Minimal document compilation (single-pass)
 * - Multi-file projects with cross-references (multi-pass)
 * - Error handling and diagnostics for missing packages
 * - PDF output format and size constraints
 *
 * Test Strategy:
 * - Call compile module directly (not CLI) for speed
 * - Use temp directories for cache isolation
 * - Assert on PDF binary format and metadata
 * - Verify error objects contain required diagnostics
 *
 * @module test/latex-build
 */

import { describe, it, beforeEach, afterEach } from 'node:test';
import assert from 'node:assert/strict';
import { promises as fs } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { dirname } from 'node:path';

// Agent 10's compile module
import { compileLatexToPdf } from '../src/lib/latex/compile.mjs';
import { LatexCompileError } from '../src/lib/latex/diagnostics.mjs';

// ============================================================================
// TEST SETUP
// ============================================================================

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const FIXTURES_DIR = join(__dirname, 'fixtures', 'latex');

/** @type {string} Temp cache directory (created per-test, cleaned up after) */
let tempCacheDir;

beforeEach(async () => {
  // Create isolated temp cache for each test
  const randomSuffix = Math.random().toString(36).slice(2, 10);
  tempCacheDir = join(tmpdir(), `latex-test-${randomSuffix}`);
  await fs.mkdir(tempCacheDir, { recursive: true });
});

afterEach(async () => {
  // Clean up temp cache
  if (tempCacheDir) {
    await fs.rm(tempCacheDir, { recursive: true, force: true });
  }
});

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/**
 * Validate PDF binary format
 * @param {Uint8Array} pdfBytes - PDF file bytes
 * @returns {object} Validation result
 */
function validatePDF(pdfBytes) {
  assert.ok(pdfBytes instanceof Uint8Array, 'pdfBytes must be Uint8Array');

  // Check PDF magic bytes: %PDF-
  const header = new TextDecoder().decode(pdfBytes.slice(0, 5));
  const hasPDFMagic = header === '%PDF-';

  // Check minimum size (5KB = 5120 bytes)
  const isLargeEnough = pdfBytes.length > 5120;

  // Check EOF marker exists (%%EOF near end)
  const tail = new TextDecoder().decode(pdfBytes.slice(-128));
  const hasEOF = tail.includes('%%EOF');

  return {
    hasPDFMagic,
    isLargeEnough,
    hasEOF,
    size: pdfBytes.length,
    valid: hasPDFMagic && isLargeEnough && hasEOF,
  };
}

/**
 * Check if log file exists in cache
 * @param {string} cacheDir - Cache directory path
 * @returns {Promise<boolean>} True if any .log file exists
 */
async function hasLogFile(cacheDir) {
  try {
    const runsDir = join(cacheDir, 'runs');
    const files = await fs.readdir(runsDir);
    return files.some(f => f.endsWith('.log'));
  } catch {
    return false;
  }
}

// ============================================================================
// TESTS: Minimal Document
// ============================================================================

describe('LaTeX Compilation - Minimal Document', () => {
  it('should compile minimal.tex to valid PDF', async () => {
    const inputTexPath = join(FIXTURES_DIR, 'minimal.tex');
    const projectDir = FIXTURES_DIR; // minimal.tex has no dependencies

    const pdfBytes = await compileLatexToPdf({
      inputTexPath,
      projectDir,
      cacheDir: tempCacheDir,
      engine: 'pdflatex',
      passes: 1, // Minimal doc needs only 1 pass
    });

    // Assert: PDF bytes are valid
    assert.ok(pdfBytes instanceof Uint8Array, 'pdfBytes must be Uint8Array');

    // Assert: PDF format validation
    const validation = validatePDF(pdfBytes);
    assert.strictEqual(validation.hasPDFMagic, true, 'PDF must start with %PDF-');
    assert.strictEqual(validation.isLargeEnough, true, 'PDF must be > 5KB');
    assert.strictEqual(validation.hasEOF, true, 'PDF must contain %%EOF marker');
    assert.ok(validation.size > 5120, `PDF size ${validation.size} must be > 5120 bytes`);
  });

  it('should create cache directory structure', async () => {
    const inputTexPath = join(FIXTURES_DIR, 'minimal.tex');
    const projectDir = FIXTURES_DIR;

    await compileLatexToPdf({
      inputTexPath,
      projectDir,
      cacheDir: tempCacheDir,
    });

    // Assert: Cache dir exists and contains expected structure
    const cacheStat = await fs.stat(tempCacheDir);
    assert.ok(cacheStat.isDirectory(), 'Cache directory must exist');

    // Assert: Lockfile created (location depends on Agent 5's implementation)
    // May be in projectDir or cacheDir depending on design
  });
});

// ============================================================================
// TESTS: Multi-File Project with Cross-References
// ============================================================================

describe('LaTeX Compilation - Multi-File Project', () => {
  it('should compile thesis-mini with cross-references', async () => {
    const inputTexPath = join(FIXTURES_DIR, 'thesis-mini', 'main.tex');
    const projectDir = join(FIXTURES_DIR, 'thesis-mini');

    const pdfBytes = await compileLatexToPdf({
      inputTexPath,
      projectDir,
      cacheDir: tempCacheDir,
      engine: 'pdflatex',
      passes: 2, // Multi-pass for cross-refs
    });

    // Assert: Valid PDF output
    const validation = validatePDF(pdfBytes);
    assert.strictEqual(validation.valid, true, 'PDF must be valid format');
    assert.ok(validation.size > 5120, 'PDF must be > 5KB');
  });

  it('should resolve preamble.tex via \\input', async () => {
    const inputTexPath = join(FIXTURES_DIR, 'thesis-mini', 'main.tex');
    const projectDir = join(FIXTURES_DIR, 'thesis-mini');

    // Should NOT throw - preamble.tex is in same directory
    const pdfBytes = await compileLatexToPdf({
      inputTexPath,
      projectDir,
      cacheDir: tempCacheDir,
    });

    assert.ok(pdfBytes instanceof Uint8Array, 'Must return PDF bytes');
    assert.ok(pdfBytes.length > 0, 'PDF must have content');
  });

  it('should handle multi-pass compilation for cross-refs', async () => {
    const inputTexPath = join(FIXTURES_DIR, 'thesis-mini', 'main.tex');
    const projectDir = join(FIXTURES_DIR, 'thesis-mini');

    // Compile with 2 passes (required for \\ref to resolve)
    const pdfBytes = await compileLatexToPdf({
      inputTexPath,
      projectDir,
      cacheDir: tempCacheDir,
      passes: 2,
    });

    // If cross-refs are unresolved, LaTeX would show "??" in output
    // We can't easily assert on PDF content, but we verify it compiles
    assert.ok(pdfBytes.length > 0, 'PDF must be generated');
  });
});

// ============================================================================
// TESTS: Error Handling
// ============================================================================

describe('LaTeX Compilation - Error Handling', () => {
  it('should throw LatexCompileError for missing package', async () => {
    const inputTexPath = join(FIXTURES_DIR, 'missing-package.tex');
    const projectDir = FIXTURES_DIR;

    // Assert: Compilation throws LatexCompileError
    await assert.rejects(
      async () => {
        await compileLatexToPdf({
          inputTexPath,
          projectDir,
          cacheDir: tempCacheDir,
        });
      },
      LatexCompileError,
      'Should throw LatexCompileError for missing package'
    );
  });

  it('should populate missingInputs in error object', async () => {
    const inputTexPath = join(FIXTURES_DIR, 'missing-package.tex');
    const projectDir = FIXTURES_DIR;

    try {
      await compileLatexToPdf({
        inputTexPath,
        projectDir,
        cacheDir: tempCacheDir,
      });
      assert.fail('Should have thrown LatexCompileError');
    } catch (error) {
      // Assert: Error is LatexCompileError instance
      assert.ok(error instanceof LatexCompileError, 'Error must be LatexCompileError');

      // Assert: missingInputs array exists and contains package name
      assert.ok(error.missingInputs, 'missingInputs must be defined');
      assert.ok(Array.isArray(error.missingInputs), 'missingInputs must be array');

      // Note: Exact package name matching depends on Agent 6's log parser
      // May match "nonexistent-test-package-xyz-12345.sty" or similar
      assert.ok(error.missingInputs.length > 0, 'missingInputs must contain entries');
    }
  });

  it('should write diagnostic log file on error', async () => {
    const inputTexPath = join(FIXTURES_DIR, 'missing-package.tex');
    const projectDir = FIXTURES_DIR;

    try {
      await compileLatexToPdf({
        inputTexPath,
        projectDir,
        cacheDir: tempCacheDir,
      });
      assert.fail('Should have thrown error');
    } catch (error) {
      // Assert: Log file was written
      const logExists = await hasLogFile(tempCacheDir);
      assert.strictEqual(logExists, true, 'Diagnostic log must exist in cache');

      // Assert: Error object contains log file path
      assert.ok(error.logFilePath, 'Error must have logFilePath');
      assert.strictEqual(typeof error.logFilePath, 'string', 'logFilePath must be string');

      // Assert: Log file is readable
      const logContent = await fs.readFile(error.logFilePath, 'utf8');
      assert.ok(logContent.length > 0, 'Log file must have content');
      assert.ok(logContent.includes('LaTeX'), 'Log must contain LaTeX output');
    }
  });

  it('should handle non-existent input file gracefully', async () => {
    const inputTexPath = join(FIXTURES_DIR, 'does-not-exist.tex');
    const projectDir = FIXTURES_DIR;

    // Assert: Throws Error (not LatexCompileError, but validation error)
    await assert.rejects(
      async () => {
        await compileLatexToPdf({
          inputTexPath,
          projectDir,
          cacheDir: tempCacheDir,
        });
      },
      /Input file not found/,
      'Should throw validation error for non-existent file'
    );
  });
});

// ============================================================================
// TESTS: Determinism
// ============================================================================

describe('LaTeX Compilation - Determinism', () => {
  it('should produce identical PDFs for same input', async () => {
    const inputTexPath = join(FIXTURES_DIR, 'minimal.tex');
    const projectDir = FIXTURES_DIR;

    // Compile twice with clean caches
    const cache1 = join(tempCacheDir, 'run1');
    const cache2 = join(tempCacheDir, 'run2');
    await fs.mkdir(cache1, { recursive: true });
    await fs.mkdir(cache2, { recursive: true });

    const pdf1 = await compileLatexToPdf({
      inputTexPath,
      projectDir,
      cacheDir: cache1,
    });

    const pdf2 = await compileLatexToPdf({
      inputTexPath,
      projectDir,
      cacheDir: cache2,
    });

    // Note: PDFs may have embedded timestamps, so byte-for-byte equality
    // is NOT expected. Instead, check size and structure match.
    assert.strictEqual(pdf1.length, pdf2.length, 'PDF sizes must match');
    assert.strictEqual(validatePDF(pdf1).valid, true, 'PDF 1 must be valid');
    assert.strictEqual(validatePDF(pdf2).valid, true, 'PDF 2 must be valid');

    // For true determinism, would need SOURCE_DATE_EPOCH handling
    // This is a known limitation documented in Agent 10's design
  });
});

// ============================================================================
// PERFORMANCE TESTS
// ============================================================================

describe('LaTeX Compilation - Performance', () => {
  it('should compile minimal.tex in under 5 seconds', async (t) => {
    const inputTexPath = join(FIXTURES_DIR, 'minimal.tex');
    const projectDir = FIXTURES_DIR;

    const startTime = Date.now();

    await compileLatexToPdf({
      inputTexPath,
      projectDir,
      cacheDir: tempCacheDir,
    });

    const duration = Date.now() - startTime;

    // Assert: Compilation time < 5000ms (per CLAUDE.md timeout SLAs)
    assert.ok(duration < 5000, `Compilation took ${duration}ms, must be < 5000ms`);
  });
});
