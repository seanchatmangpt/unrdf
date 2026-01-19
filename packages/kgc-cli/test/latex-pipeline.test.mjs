/**
 * @fileoverview Comprehensive LaTeX Pipeline Tests
 *
 * Tests the complete LaTeX→PDF compilation pipeline including:
 * - Minimal document compilation (single-pass)
 * - Multi-pass compilation for cross-references
 * - Missing package detection and error handling
 * - VFS determinism and file resolution
 * - Performance benchmarks
 *
 * NOTE: These tests require SwiftLaTeX WASM binaries to be installed.
 * If WASM binaries are not available, all tests will be skipped.
 * To install: Run `node scripts/vendor-tex-engine.mjs`
 *
 * @module test/latex-pipeline
 */

import { describe, it, expect, beforeEach, afterEach, beforeAll } from 'vitest';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { dirname } from 'node:path';

import { compileLatexToPdf } from '../src/lib/latex/compile.mjs';
import { LatexCompileError } from '../src/lib/latex/diagnostics.mjs';
import {
  createTempDir,
  cleanupTempDir,
  validatePDFFormat,
  countFiles
} from './fixtures/setup.mjs';
import { checkLatexAvailable } from './fixtures/check-latex-available.mjs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const FIXTURES_DIR = join(__dirname, 'fixtures', 'latex');

// Check if LaTeX WASM binaries are available
let latexAvailable = false;
let _skipReason = '';

beforeAll(async () => {
  const result = await checkLatexAvailable();
  latexAvailable = result.available;
  if (!result.available) {
    _skipReason = result.reason;
    console.warn(`\n⚠️  LaTeX pipeline tests skipped: ${result.reason}\n`);
  }
});

let tempCacheDir;

beforeEach(async () => {
  if (!latexAvailable) return;
  tempCacheDir = await createTempDir('pipeline-test-');
});

afterEach(async () => {
  if (!latexAvailable) return;
  await cleanupTempDir(tempCacheDir);
});

// ============================================================================
// MINIMAL DOCUMENT TESTS
// ============================================================================

describe.skipIf(!latexAvailable)('Minimal Document Compilation', () => {
  it('should compile minimal.tex to valid PDF', async () => {
    const inputTexPath = join(FIXTURES_DIR, 'minimal.tex');
    const projectDir = FIXTURES_DIR;

    const pdfBytes = await compileLatexToPdf({
      inputTexPath,
      projectDir,
      cacheDir: tempCacheDir,
      engine: 'pdflatex',
      passes: 1,
    });

    expect(pdfBytes).toBeInstanceOf(Uint8Array);

    const validation = validatePDFFormat(pdfBytes);
    expect(validation.valid).toBe(true);
    expect(validation.hasPDFMagic).toBe(true);
    expect(validation.isLargeEnough).toBe(true);
    expect(validation.hasEOF).toBe(true);
  });

  it('should complete in under 5 seconds (SLA compliance)', { timeout: 6000 }, async () => {
    const inputTexPath = join(FIXTURES_DIR, 'minimal.tex');
    const projectDir = FIXTURES_DIR;

    const startTime = Date.now();

    await compileLatexToPdf({
      inputTexPath,
      projectDir,
      cacheDir: tempCacheDir,
    });

    const duration = Date.now() - startTime;
    expect(duration).toBeLessThan(5000); // CLAUDE.md timeout SLA
  });

  it('should create cache directory structure', async () => {
    const inputTexPath = join(FIXTURES_DIR, 'minimal.tex');
    const projectDir = FIXTURES_DIR;

    await compileLatexToPdf({
      inputTexPath,
      projectDir,
      cacheDir: tempCacheDir,
    });

    // Verify cache directory exists
    const logCount = await countFiles(tempCacheDir, /\.log$/);
    expect(logCount).toBeGreaterThan(0);
  });
});

// ============================================================================
// CROSS-REFERENCE TESTS (MULTI-PASS)
// ============================================================================

describe.skipIf(!latexAvailable)('Multi-Pass Compilation for Cross-References', () => {
  it('should compile with-refs/main.tex with 2 passes', async () => {
    const inputTexPath = join(FIXTURES_DIR, 'with-refs', 'main.tex');
    const projectDir = join(FIXTURES_DIR, 'with-refs');

    const pdfBytes = await compileLatexToPdf({
      inputTexPath,
      projectDir,
      cacheDir: tempCacheDir,
      engine: 'pdflatex',
      passes: 2, // Required for cross-refs
    });

    const validation = validatePDFFormat(pdfBytes);
    expect(validation.valid).toBe(true);
    expect(pdfBytes.length).toBeGreaterThan(5120);
  });

  it('should compile thesis-mini with \\input resolution', async () => {
    const inputTexPath = join(FIXTURES_DIR, 'thesis-mini', 'main.tex');
    const projectDir = join(FIXTURES_DIR, 'thesis-mini');

    const pdfBytes = await compileLatexToPdf({
      inputTexPath,
      projectDir,
      cacheDir: tempCacheDir,
      passes: 2,
    });

    expect(pdfBytes).toBeInstanceOf(Uint8Array);
    expect(validatePDFFormat(pdfBytes).valid).toBe(true);
  });
});

// ============================================================================
// MISSING PACKAGE ERROR HANDLING
// ============================================================================

describe.skipIf(!latexAvailable)('Missing Package Detection', () => {
  it('should throw LatexCompileError for missing package', async () => {
    const inputTexPath = join(FIXTURES_DIR, 'missing-package.tex');
    const projectDir = FIXTURES_DIR;

    await expect(
      compileLatexToPdf({
        inputTexPath,
        projectDir,
        cacheDir: tempCacheDir,
      })
    ).rejects.toThrow(LatexCompileError);
  });

  it('should populate missingInputs array in error', async () => {
    const inputTexPath = join(FIXTURES_DIR, 'missing-package.tex');
    const projectDir = FIXTURES_DIR;

    try {
      await compileLatexToPdf({
        inputTexPath,
        projectDir,
        cacheDir: tempCacheDir,
      });
      expect.fail('Should have thrown LatexCompileError');
    } catch (error) {
      expect(error).toBeInstanceOf(LatexCompileError);
      expect(error.missingInputs).toBeDefined();
      expect(Array.isArray(error.missingInputs)).toBe(true);
      expect(error.missingInputs.length).toBeGreaterThan(0);
    }
  });

  it('should write diagnostic log on error', async () => {
    const inputTexPath = join(FIXTURES_DIR, 'missing-package.tex');
    const projectDir = FIXTURES_DIR;

    try {
      await compileLatexToPdf({
        inputTexPath,
        projectDir,
        cacheDir: tempCacheDir,
      });
      expect.fail('Should have thrown error');
    } catch (error) {
      expect(error.logFilePath).toBeDefined();
      expect(typeof error.logFilePath).toBe('string');

      // Verify log file exists
      const logCount = await countFiles(tempCacheDir, /\.log$/);
      expect(logCount).toBeGreaterThan(0);
    }
  });
});

// ============================================================================
// VFS DETERMINISM TESTS
// ============================================================================

describe.skipIf(!latexAvailable)('VFS Determinism', () => {
  it('should produce consistent PDF sizes for same input', async () => {
    const inputTexPath = join(FIXTURES_DIR, 'minimal.tex');
    const projectDir = FIXTURES_DIR;

    const cache1 = await createTempDir('vfs-test-1-');
    const cache2 = await createTempDir('vfs-test-2-');

    try {
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

      // PDFs may have timestamps, so exact byte equality not expected
      // But sizes should match closely (within 1%)
      const sizeDiff = Math.abs(pdf1.length - pdf2.length);
      const tolerance = pdf1.length * 0.01;
      expect(sizeDiff).toBeLessThan(tolerance);
    } finally {
      await cleanupTempDir(cache1);
      await cleanupTempDir(cache2);
    }
  });
});

// ============================================================================
// IMAGE INCLUSION TESTS
// ============================================================================

describe.skipIf(!latexAvailable)('Image Inclusion', () => {
  it('should compile document with embedded PNG', async () => {
    const inputTexPath = join(FIXTURES_DIR, 'with-image', 'main.tex');
    const projectDir = join(FIXTURES_DIR, 'with-image');

    const pdfBytes = await compileLatexToPdf({
      inputTexPath,
      projectDir,
      cacheDir: tempCacheDir,
      passes: 1,
    });

    const validation = validatePDFFormat(pdfBytes);
    expect(validation.valid).toBe(true);
    expect(pdfBytes.length).toBeGreaterThan(5120);
  });

  it('should handle graphicx package correctly', async () => {
    const inputTexPath = join(FIXTURES_DIR, 'with-image', 'main.tex');
    const projectDir = join(FIXTURES_DIR, 'with-image');

    // Should NOT throw - graphicx is a standard package
    const pdfBytes = await compileLatexToPdf({
      inputTexPath,
      projectDir,
      cacheDir: tempCacheDir,
    });

    expect(pdfBytes).toBeInstanceOf(Uint8Array);
  });
});

// ============================================================================
// ERROR HANDLING EDGE CASES
// ============================================================================

describe.skipIf(!latexAvailable)('Error Handling Edge Cases', () => {
  it('should reject non-existent input file', async () => {
    const inputTexPath = join(FIXTURES_DIR, 'does-not-exist.tex');
    const projectDir = FIXTURES_DIR;

    await expect(
      compileLatexToPdf({
        inputTexPath,
        projectDir,
        cacheDir: tempCacheDir,
      })
    ).rejects.toThrow();
  });

  it('should validate cache directory parameter', async () => {
    const inputTexPath = join(FIXTURES_DIR, 'minimal.tex');
    const projectDir = FIXTURES_DIR;

    // Empty cache dir should be rejected by validation
    await expect(
      compileLatexToPdf({
        inputTexPath,
        projectDir,
        cacheDir: '',
      })
    ).rejects.toThrow();
  });
});

// ============================================================================
// PERFORMANCE BENCHMARKS
// ============================================================================

describe.skipIf(!latexAvailable)('Performance Benchmarks', () => {
  it('should handle minimal doc in <5s (cold cache)', { timeout: 6000 }, async () => {
    const inputTexPath = join(FIXTURES_DIR, 'minimal.tex');
    const projectDir = FIXTURES_DIR;

    const startTime = performance.now();

    await compileLatexToPdf({
      inputTexPath,
      projectDir,
      cacheDir: tempCacheDir,
    });

    const duration = performance.now() - startTime;
    expect(duration).toBeLessThan(5000);
  });

  it('should handle multi-file doc in <10s', { timeout: 12000 }, async () => {
    const inputTexPath = join(FIXTURES_DIR, 'thesis-mini', 'main.tex');
    const projectDir = join(FIXTURES_DIR, 'thesis-mini');

    const startTime = performance.now();

    await compileLatexToPdf({
      inputTexPath,
      projectDir,
      cacheDir: tempCacheDir,
      passes: 2,
    });

    const duration = performance.now() - startTime;
    expect(duration).toBeLessThan(10000);
  });
});
