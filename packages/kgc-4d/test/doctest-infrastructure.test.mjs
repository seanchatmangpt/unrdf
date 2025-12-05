/**
 * Unit tests for doctest infrastructure
 * Tests the extractor, transformer, and runner modules
 */

import { describe, test, expect } from 'vitest';
import { extractExamples, extractAllExamples } from '../src/doctest/extractor.mjs';
import { transformToVitest } from '../src/doctest/transformer.mjs';

describe('Doctest Infrastructure', () => {
  describe('Extractor', () => {
    test('extracts examples from source file', () => {
      const examples = extractExamples('src/time.mjs');
      expect(examples.length).toBeGreaterThan(0);
      expect(examples[0]).toHaveProperty('code');
      expect(examples[0]).toHaveProperty('functionName');
      expect(examples[0]).toHaveProperty('lineNumber');
      expect(examples[0]).toHaveProperty('sourceFile');
    });

    test('extracted example has valid structure', () => {
      const examples = extractExamples('src/guards.mjs');
      expect(examples.length).toBeGreaterThan(0);

      const example = examples[0];
      expect(typeof example.code).toBe('string');
      expect(example.code.length).toBeGreaterThan(0);
      expect(typeof example.functionName).toBe('string');
      expect(typeof example.lineNumber).toBe('number');
      expect(example.lineNumber).toBeGreaterThan(0);
    });

    test('handles files with no examples', () => {
      const examples = extractExamples('src/constants.mjs');
      expect(examples).toEqual([]);
    });

    test('returns empty array for missing files', () => {
      const examples = extractExamples('src/nonexistent.mjs');
      expect(examples).toEqual([]);
    });

    test('extracts multiple examples from single file', () => {
      const examples = extractExamples('src/time.mjs');
      expect(examples.length).toBeGreaterThanOrEqual(3);
    });

    test('extracted code includes import statements', () => {
      const examples = extractExamples('src/store.mjs');
      const hasImports = examples.some(ex => ex.code.includes('import'));
      expect(hasImports).toBe(true);
    });

    test('extracted code is trimmed and clean', () => {
      const examples = extractExamples('src/time.mjs');
      expect(examples.length).toBeGreaterThan(0);

      examples.forEach(example => {
        // Code should not start/end with whitespace
        expect(example.code).toBe(example.code.trim());
        // Code should not start with asterisk (JSDoc artifact)
        expect(example.code.startsWith('*')).toBe(false);
      });
    });

    test('extractAllExamples returns object map', () => {
      const all = extractAllExamples('src');
      expect(typeof all).toBe('object');
      expect(Object.keys(all).length).toBeGreaterThan(0);
      // Each file should map to array of examples
      Object.values(all).forEach(examples => {
        expect(Array.isArray(examples)).toBe(true);
      });
    });
  });

  describe('Transformer', () => {
    test('transforms examples to Vitest test code', () => {
      const examples = [
        {
          code: 'import { now } from \'./time.mjs\';\nconst t = now();\nconsole.assert(typeof t === \'bigint\', \'Returns BigInt\');',
          functionName: 'now',
          lineNumber: 42
        }
      ];

      const testCode = transformToVitest(examples, 'time.mjs');
      expect(typeof testCode).toBe('string');
      expect(testCode).toContain('import { describe, test, expect }');
      expect(testCode).toContain('describe(');
      expect(testCode).toContain('test(');
      expect(testCode).toContain('Doctests: time.mjs');
    });

    test('generated test includes describe and test blocks', () => {
      const examples = extractExamples('src/time.mjs');
      expect(examples.length).toBeGreaterThan(0);

      const testCode = transformToVitest(examples, 'time.mjs');
      expect(testCode).toContain('describe(\'Doctests: time.mjs\'');
      expect(testCode).toContain('test(\'');
      expect(testCode).toContain('async () => {');
    });

    test('rewrites imports from source to test path', () => {
      const examples = [
        {
          code: 'import { now } from \'./time.mjs\';\nconst t = now();',
          functionName: 'now',
          lineNumber: 42
        }
      ];

      const testCode = transformToVitest(examples, 'time.mjs');
      expect(testCode).toContain('import { now } from \'../../src/time.mjs\'');
    });

    test('deduplicates imports', () => {
      const examples = [
        {
          code: 'import { now } from \'./time.mjs\';\nconst t1 = now();',
          functionName: 'now',
          lineNumber: 42
        },
        {
          code: 'import { now } from \'./time.mjs\';\nconst t2 = now();',
          functionName: 'now',
          lineNumber: 50
        }
      ];

      const testCode = transformToVitest(examples, 'time.mjs');
      // Should have only one import statement
      const importCount = (testCode.match(/import { now } from/g) || []).length;
      expect(importCount).toBe(1);
    });

    test('removes imports from test code blocks', () => {
      const examples = [
        {
          code: 'import { now } from \'./time.mjs\';\nconst t = now();\nconsole.assert(t > 0n);',
          functionName: 'now',
          lineNumber: 42
        }
      ];

      const testCode = transformToVitest(examples, 'time.mjs');
      // Test code inside the test function should not have the import
      const testBlockStart = testCode.indexOf('async () => {');
      const testBlockEnd = testCode.indexOf('});', testBlockStart);
      const testBlock = testCode.substring(testBlockStart, testBlockEnd);

      expect(testBlock).not.toContain('import { now }');
      expect(testBlock).toContain('const t = now()');
    });

    test('includes function name and line number in test name', () => {
      const examples = [
        {
          code: 'const x = 5;',
          functionName: 'testFunc',
          lineNumber: 100
        }
      ];

      const testCode = transformToVitest(examples, 'test.mjs');
      expect(testCode).toContain('testFunc example 1 (line 100)');
    });

    test('generates valid JavaScript syntax', () => {
      const examples = extractExamples('src/time.mjs');
      expect(examples.length).toBeGreaterThan(0);

      const testCode = transformToVitest(examples, 'time.mjs');

      // Should have balanced braces and parentheses (basic syntax check)
      const openBraces = (testCode.match(/\{/g) || []).length;
      const closeBraces = (testCode.match(/\}/g) || []).length;
      expect(openBraces).toBe(closeBraces);
    });
  });

  describe('End-to-End', () => {
    test('extracted examples transform to valid test code', () => {
      const examples = extractExamples('src/store.mjs');
      expect(examples.length).toBeGreaterThan(0);

      const testCode = transformToVitest(examples, 'store.mjs');

      // Validate structure
      expect(testCode).toContain('import { describe, test');
      expect(testCode).toContain('describe(\'Doctests: store.mjs\'');
      expect(testCode.match(/test\(/g).length).toBe(examples.length);
    });

    test('all extracted files have valid examples', () => {
      const all = extractAllExamples('src');

      Object.entries(all).forEach(([fileName, examples]) => {
        expect(examples.length).toBeGreaterThan(0);
        examples.forEach(example => {
          expect(example.code).toBeTruthy();
          expect(example.functionName).toBeTruthy();
          expect(example.lineNumber).toBeGreaterThan(0);
        });
      });
    });

    test('total example count matches individual counts', () => {
      const all = extractAllExamples('src');
      let totalCount = 0;

      Object.values(all).forEach(examples => {
        totalCount += examples.length;
      });

      expect(totalCount).toBeGreaterThanOrEqual(11);
    });
  });
});
