/**
 * @fileoverview LaTeX diagnostics tests.
 *
 * Validates:
 * - Log file writing with deterministic paths
 * - LatexCompileError structure and serialization
 * - Missing input detection from LaTeX logs
 * - Error summary extraction
 * - Compilation success detection
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { rm, readFile, mkdir } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import {
  LatexCompileError,
  writeLatexRunLog,
  parseMissingInputsFromLog,
  extractErrorSummary,
  isCompileSuccessful,
  LogWriteOptionsSchema
} from '../src/lib/latex/diagnostics.mjs';

describe('LaTeX Diagnostics', () => {
  let testCacheDir;

  beforeEach(async () => {
    // Create temporary cache directory for tests
    testCacheDir = join(tmpdir(), `latex-test-${Date.now()}`);
    await mkdir(testCacheDir, { recursive: true });
  });

  afterEach(async () => {
    // Cleanup test directory
    await rm(testCacheDir, { recursive: true, force: true });
  });

  describe('LatexCompileError', () => {
    it('should create error with all required properties', () => {
      const error = new LatexCompileError('Compilation failed', {
        engine: 'pdflatex',
        inputTexPath: '/path/to/main.tex',
        logFilePath: '/path/to/log.txt',
        missingInputs: ['chapter1.tex', 'logo.pdf'],
        exitCode: '1'
      });

      expect(error.name).toBe('LatexCompileError');
      expect(error.message).toBe('Compilation failed');
      expect(error.engine).toBe('pdflatex');
      expect(error.inputTexPath).toBe('/path/to/main.tex');
      expect(error.logFilePath).toBe('/path/to/log.txt');
      expect(error.missingInputs).toEqual(['chapter1.tex', 'logo.pdf']);
      expect(error.exitCode).toBe('1');
    });

    it('should default missingInputs to empty array', () => {
      const error = new LatexCompileError('Failed', {
        engine: 'lualatex',
        inputTexPath: '/main.tex',
        logFilePath: '/log.txt'
      });

      expect(error.missingInputs).toEqual([]);
    });

    it('should serialize to JSON correctly', () => {
      const error = new LatexCompileError('Test error', {
        engine: 'xelatex',
        inputTexPath: '/test.tex',
        logFilePath: '/test.log',
        missingInputs: ['missing.sty'],
        exitCode: '1'
      });

      const json = error.toJSON();

      expect(json).toEqual({
        name: 'LatexCompileError',
        message: 'Test error',
        engine: 'xelatex',
        inputTexPath: '/test.tex',
        logFilePath: '/test.log',
        missingInputs: ['missing.sty'],
        exitCode: '1'
      });
    });

    it('should be instanceof Error', () => {
      const error = new LatexCompileError('Test', {
        engine: 'pdflatex',
        inputTexPath: '/test.tex',
        logFilePath: '/test.log'
      });

      expect(error instanceof Error).toBe(true);
      expect(error instanceof LatexCompileError).toBe(true);
    });
  });

  describe('writeLatexRunLog', () => {
    it('should write log file with correct timestamp format', async () => {
      const logText = 'This is pdfTeX, Version 3.14159265-2.6-1.40.21\n! Error here.';

      const logFilePath = await writeLatexRunLog({
        cacheDir: testCacheDir,
        engine: 'pdflatex',
        inputTexPath: '/path/to/main.tex',
        logText
      });

      // Verify file was created
      expect(logFilePath).toMatch(/\.log$/);
      expect(logFilePath).toContain('pdflatex');

      // Verify file contents
      const contents = await readFile(logFilePath, 'utf8');
      expect(contents).toContain('# LaTeX Compilation Log');
      expect(contents).toContain('# Engine: pdflatex');
      expect(contents).toContain('# Input:  /path/to/main.tex');
      expect(contents).toContain(logText);
    });

    it('should create runs directory if it does not exist', async () => {
      const logFilePath = await writeLatexRunLog({
        cacheDir: testCacheDir,
        engine: 'lualatex',
        inputTexPath: '/test.tex',
        logText: 'Log content'
      });

      expect(logFilePath).toContain(join(testCacheDir, 'runs'));

      // Verify directory was created
      const contents = await readFile(logFilePath, 'utf8');
      expect(contents).toContain('Log content');
    });

    it('should generate unique filenames for concurrent writes', async () => {
      const writes = await Promise.all([
        writeLatexRunLog({
          cacheDir: testCacheDir,
          engine: 'pdflatex',
          inputTexPath: '/test1.tex',
          logText: 'Log 1'
        }),
        writeLatexRunLog({
          cacheDir: testCacheDir,
          engine: 'pdflatex',
          inputTexPath: '/test2.tex',
          logText: 'Log 2'
        }),
        writeLatexRunLog({
          cacheDir: testCacheDir,
          engine: 'lualatex',
          inputTexPath: '/test3.tex',
          logText: 'Log 3'
        })
      ]);

      // All paths should be unique
      const uniquePaths = new Set(writes);
      expect(uniquePaths.size).toBe(3);

      // Verify each file has correct content
      const contents = await Promise.all(writes.map(p => readFile(p, 'utf8')));
      expect(contents[0]).toContain('Log 1');
      expect(contents[1]).toContain('Log 2');
      expect(contents[2]).toContain('Log 3');
    });

    it('should validate inputs with Zod schema', async () => {
      await expect(async () => {
        await writeLatexRunLog({
          cacheDir: '',  // Invalid: empty string
          engine: 'pdflatex',
          inputTexPath: '/test.tex',
          logText: 'content'
        });
      }).rejects.toThrow();
    });

    it('should handle different engines correctly', async () => {
      const engines = ['pdflatex', 'lualatex', 'xelatex'];

      for (const engine of engines) {
        const logPath = await writeLatexRunLog({
          cacheDir: testCacheDir,
          engine,
          inputTexPath: '/test.tex',
          logText: `Log for ${engine}`
        });

        expect(logPath).toContain(engine);
        const contents = await readFile(logPath, 'utf8');
        expect(contents).toContain(`# Engine: ${engine}`);
      }
    });
  });

  describe('parseMissingInputsFromLog', () => {
    it('should detect LaTeX Error: File not found pattern', () => {
      const log = `
! LaTeX Error: File \`chapter1.tex' not found.

Type X to quit or <RETURN> to proceed,
or enter new name. (Default extension: tex)
`;

      const missing = parseMissingInputsFromLog(log);
      expect(missing).toEqual(['chapter1.tex']);
    });

    it('should detect "I can\'t find file" pattern', () => {
      const log = `
! I can't find file \`thesis.cls'.
<*> \\input thesis.cls
Please type another input file name:
`;

      const missing = parseMissingInputsFromLog(log);
      expect(missing).toEqual(['thesis.cls']);
    });

    it('should detect Package Error: File not found pattern', () => {
      const log = `
! Package graphicx Error: File \`logo.pdf' not found.

See the graphicx package documentation for explanation.
`;

      const missing = parseMissingInputsFromLog(log);
      expect(missing).toEqual(['logo.pdf']);
    });

    it('should detect simple "File not found" pattern', () => {
      const log = `
! File \`biblio.bib' not found.
`;

      const missing = parseMissingInputsFromLog(log);
      expect(missing).toEqual(['biblio.bib']);
    });

    it('should detect parenthesis "not found" pattern', () => {
      const log = `
(missing-file.tex not found)
`;

      const missing = parseMissingInputsFromLog(log);
      expect(missing).toEqual(['missing-file.tex']);
    });

    it('should detect multiple missing files', () => {
      const log = `
! LaTeX Error: File \`chapter1.tex' not found.
! I can't find file \`chapter2.tex'.
! Package graphicx Error: File \`logo.pdf' not found.
! File \`refs.bib' not found.
`;

      const missing = parseMissingInputsFromLog(log);
      expect(missing).toEqual([
        'chapter1.tex',
        'chapter2.tex',
        'logo.pdf',
        'refs.bib'
      ]);
    });

    it('should deduplicate missing files', () => {
      const log = `
! LaTeX Error: File \`chapter1.tex' not found.
! I can't find file \`chapter1.tex'.
! LaTeX Error: File \`chapter1.tex' not found.
`;

      const missing = parseMissingInputsFromLog(log);
      expect(missing).toEqual(['chapter1.tex']);
    });

    it('should return empty array for clean log', () => {
      const log = `
This is pdfTeX, Version 3.14159265-2.6-1.40.21
Output written on main.pdf (10 pages, 123456 bytes).
`;

      const missing = parseMissingInputsFromLog(log);
      expect(missing).toEqual([]);
    });

    it('should handle empty or undefined input', () => {
      expect(parseMissingInputsFromLog('')).toEqual([]);
      expect(parseMissingInputsFromLog(null)).toEqual([]);
      expect(parseMissingInputsFromLog(undefined)).toEqual([]);
    });

    it('should return sorted results (deterministic)', () => {
      const log = `
! File \`zzz.tex' not found.
! File \`aaa.tex' not found.
! File \`mmm.tex' not found.
`;

      const missing = parseMissingInputsFromLog(log);
      expect(missing).toEqual(['aaa.tex', 'mmm.tex', 'zzz.tex']);
    });
  });

  describe('extractErrorSummary', () => {
    it('should extract first error line', () => {
      const log = `
This is pdfTeX, Version 3.14159265
! Undefined control sequence.
l.42 \\invalidcommand

`;

      const summary = extractErrorSummary(log);
      expect(summary).toBe('! Undefined control sequence.');
    });

    it('should return undefined if no error found', () => {
      const log = `
This is pdfTeX, Version 3.14159265
Output written on main.pdf (1 page).
`;

      const summary = extractErrorSummary(log);
      expect(summary).toBeUndefined();
    });

    it('should handle empty input', () => {
      expect(extractErrorSummary('')).toBeUndefined();
      expect(extractErrorSummary(null)).toBeUndefined();
      expect(extractErrorSummary(undefined)).toBeUndefined();
    });

    it('should extract LaTeX Error lines', () => {
      const log = `
! LaTeX Error: File \`missing.tex' not found.
`;

      const summary = extractErrorSummary(log);
      expect(summary).toContain("LaTeX Error");
    });
  });

  describe('isCompileSuccessful', () => {
    it('should return true for successful compilation', () => {
      const log = `
This is pdfTeX, Version 3.14159265-2.6-1.40.21
Output written on main.pdf (10 pages, 123456 bytes).
PDF statistics:
 100 PDF objects out of 1000
`;

      expect(isCompileSuccessful(log)).toBe(true);
    });

    it('should return false if output written but has errors', () => {
      const log = `
This is pdfTeX, Version 3.14159265
! Undefined control sequence.
Output written on main.pdf (1 page, 1234 bytes).
`;

      expect(isCompileSuccessful(log)).toBe(false);
    });

    it('should return false if no output written', () => {
      const log = `
This is pdfTeX, Version 3.14159265
! Emergency stop.
No pages of output.
`;

      expect(isCompileSuccessful(log)).toBe(false);
    });

    it('should return false for empty log', () => {
      expect(isCompileSuccessful('')).toBe(false);
      expect(isCompileSuccessful(null)).toBe(false);
      expect(isCompileSuccessful(undefined)).toBe(false);
    });

    it('should detect various output patterns', () => {
      const logs = [
        'Output written on thesis.pdf (150 pages, 5MB).',
        'Output written on main.pdf (1 page, 100 bytes).',
        'Output written on document.pdf (999 pages, 1234567 bytes).'
      ];

      for (const log of logs) {
        expect(isCompileSuccessful(log)).toBe(true);
      }
    });
  });

  describe('LogWriteOptionsSchema', () => {
    it('should validate correct options', () => {
      const valid = {
        cacheDir: '/tmp/cache',
        engine: 'pdflatex',
        inputTexPath: '/path/to/main.tex',
        logText: 'Log content here'
      };

      const result = LogWriteOptionsSchema.safeParse(valid);
      expect(result.success).toBe(true);
    });

    it('should reject invalid engine', () => {
      const invalid = {
        cacheDir: '/tmp/cache',
        engine: 'invalid-engine',
        inputTexPath: '/main.tex',
        logText: 'content'
      };

      const result = LogWriteOptionsSchema.safeParse(invalid);
      expect(result.success).toBe(false);
    });

    it('should reject empty cacheDir', () => {
      const invalid = {
        cacheDir: '',
        engine: 'pdflatex',
        inputTexPath: '/main.tex',
        logText: 'content'
      };

      const result = LogWriteOptionsSchema.safeParse(invalid);
      expect(result.success).toBe(false);
    });

    it('should accept all valid engines', () => {
      const engines = ['pdflatex', 'lualatex', 'xelatex'];

      for (const engine of engines) {
        const result = LogWriteOptionsSchema.safeParse({
          cacheDir: '/tmp',
          engine,
          inputTexPath: '/test.tex',
          logText: 'content'
        });
        expect(result.success).toBe(true);
      }
    });
  });

  describe('Integration: Full error flow', () => {
    it('should capture complete error context', async () => {
      const fakeLog = `
This is pdfTeX, Version 3.14159265-2.6-1.40.21
entering extended mode
! LaTeX Error: File \`missing-chapter.tex' not found.

Type X to quit or <RETURN> to proceed,
! I can't find file \`logo.pdf'.
! Package babel Error: Unknown option \`invalid'.
`;

      // Write log
      const logFilePath = await writeLatexRunLog({
        cacheDir: testCacheDir,
        engine: 'pdflatex',
        inputTexPath: '/thesis/main.tex',
        logText: fakeLog
      });

      // Parse missing inputs
      const missingInputs = parseMissingInputsFromLog(fakeLog);

      // Create error
      const error = new LatexCompileError('Compilation failed', {
        engine: 'pdflatex',
        inputTexPath: '/thesis/main.tex',
        logFilePath,
        missingInputs,
        exitCode: '1'
      });

      // Verify complete error context
      expect(error.logFilePath).toBe(logFilePath);
      expect(error.missingInputs).toContain('missing-chapter.tex');
      expect(error.missingInputs).toContain('logo.pdf');
      expect(error.engine).toBe('pdflatex');

      // Verify log file persisted
      const savedLog = await readFile(logFilePath, 'utf8');
      expect(savedLog).toContain(fakeLog);
      expect(savedLog).toContain('# Engine: pdflatex');
    });
  });
});
