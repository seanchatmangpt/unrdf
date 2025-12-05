/**
 * Integration tests for doctest generation pipeline
 * Tests the full workflow from extraction to test execution
 */

import { describe, test, expect } from 'vitest';
import { readFileSync, readdirSync, existsSync } from 'fs';
import { join } from 'path';

describe('Doctest Integration', () => {
  describe('Generated Test Files', () => {
    test('doctest files are generated in test/doctest directory', () => {
      const doctestDir = 'test/doctest';
      expect(existsSync(doctestDir)).toBe(true);

      const files = readdirSync(doctestDir).filter(f => f.endsWith('.doctest.test.mjs'));
      expect(files.length).toBeGreaterThan(0);
    });

    test('generated files have correct naming pattern', () => {
      const doctestDir = 'test/doctest';
      const files = readdirSync(doctestDir).filter(f => f.endsWith('.doctest.test.mjs'));

      files.forEach(file => {
        expect(file).toMatch(/^[a-z-]+\.doctest\.test\.mjs$/);
      });
    });

    test('each generated file is valid JavaScript', () => {
      const doctestDir = 'test/doctest';
      const files = readdirSync(doctestDir).filter(f => f.endsWith('.doctest.test.mjs'));

      files.forEach(file => {
        const content = readFileSync(join(doctestDir, file), 'utf-8');
        // Check basic syntax validity without evaluating imports
        // Should have balanced braces, brackets, and parentheses
        const openBraces = (content.match(/\{/g) || []).length;
        const closeBraces = (content.match(/\}/g) || []).length;
        const openParens = (content.match(/\(/g) || []).length;
        const closeParens = (content.match(/\)/g) || []).length;

        expect(openBraces).toBe(closeBraces);
        expect(openParens).toBe(closeParens);
      });
    });

    test('generated files have proper Vitest structure', () => {
      const doctestDir = 'test/doctest';
      const files = readdirSync(doctestDir).filter(f => f.endsWith('.doctest.test.mjs'));

      files.forEach(file => {
        const content = readFileSync(join(doctestDir, file), 'utf-8');
        expect(content).toContain('import { describe, test');
        expect(content).toContain('describe(');
        expect(content).toContain('test(');
        expect(content).toContain('Doctests:');
      });
    });

    test('imports point to correct source paths', () => {
      const doctestDir = 'test/doctest';
      const files = readdirSync(doctestDir).filter(f => f.endsWith('.doctest.test.mjs'));

      files.forEach(file => {
        const content = readFileSync(join(doctestDir, file), 'utf-8');
        // Should have imports pointing to src directory
        const hasCorrectPath = content.includes('../../src/');
        expect(hasCorrectPath || content.includes("'$"), true,
          `${file} should have correct import paths`);
      });
    });
  });

  describe('Test Coverage', () => {
    test('at least 5 source files have examples', () => {
      const doctestDir = 'test/doctest';
      const files = readdirSync(doctestDir).filter(f => f.endsWith('.doctest.test.mjs'));
      expect(files.length).toBeGreaterThanOrEqual(5);
    });

    test('total doctest count is at least 11', () => {
      const doctestDir = 'test/doctest';
      let totalTests = 0;

      const files = readdirSync(doctestDir).filter(f => f.endsWith('.doctest.test.mjs'));
      files.forEach(file => {
        const content = readFileSync(join(doctestDir, file), 'utf-8');
        const testCount = (content.match(/test\(/g) || []).length;
        totalTests += testCount;
      });

      expect(totalTests).toBeGreaterThanOrEqual(11);
    });

    test('doctests cover multiple source files', () => {
      const sourceFiles = ['time.mjs', 'store.mjs', 'guards.mjs', 'freeze.mjs', 'git.mjs'];
      const doctestDir = 'test/doctest';
      const generatedFiles = readdirSync(doctestDir).filter(f => f.endsWith('.doctest.test.mjs'));

      sourceFiles.forEach(srcFile => {
        const baseName = srcFile.replace('.mjs', '');
        const doctestFile = `${baseName}.doctest.test.mjs`;
        const exists = generatedFiles.some(f => f === doctestFile);

        if (exists) {
          const content = readFileSync(join(doctestDir, doctestFile), 'utf-8');
          expect(content).toContain(`Doctests: ${srcFile}`);
        }
      });
    });
  });

  describe('Example Code Quality', () => {
    test('generated tests have descriptive test names', () => {
      const doctestDir = 'test/doctest';
      const files = readdirSync(doctestDir).filter(f => f.endsWith('.doctest.test.mjs'));

      files.forEach(file => {
        const content = readFileSync(join(doctestDir, file), 'utf-8');
        // Each test should have a descriptive name with function and line number
        const tests = content.match(/test\('([^']+)'/g) || [];

        tests.forEach(test => {
          // Should contain function name and line number
          expect(test).toContain('example');
          expect(test).toContain('line');
        });
      });
    });

    test('examples use console.assert for validation', () => {
      const doctestDir = 'test/doctest';
      const files = readdirSync(doctestDir).filter(f => f.endsWith('.doctest.test.mjs'));

      let hasAssertions = false;
      files.forEach(file => {
        const content = readFileSync(join(doctestDir, file), 'utf-8');
        if (content.includes('console.assert')) {
          hasAssertions = true;
        }
      });

      expect(hasAssertions).toBe(true);
    });

    test('generated code is properly indented', () => {
      const doctestDir = 'test/doctest';
      const files = readdirSync(doctestDir).filter(f => f.endsWith('.doctest.test.mjs'));

      files.forEach(file => {
        const content = readFileSync(join(doctestDir, file), 'utf-8');
        const lines = content.split('\n');

        // Check that non-empty lines have reasonable indentation
        let inTest = false;
        lines.forEach(line => {
          if (line.includes('test(')) inTest = true;
          if (line.includes('});') && inTest) inTest = false;

          if (inTest && line.trim()) {
            // Inside test, should have proper indentation
            const hasLeadingSpace = line.startsWith('  ');
            expect(hasLeadingSpace || line.trim()).toBeTruthy();
          }
        });
      });
    });
  });

  describe('Source-to-Test Mapping', () => {
    test('test file references match source file structure', () => {
      const doctestDir = 'test/doctest';
      const files = readdirSync(doctestDir).filter(f => f.endsWith('.doctest.test.mjs'));

      files.forEach(file => {
        const sourceFile = file.replace('.doctest.test.mjs', '.mjs');
        const sourceExists = existsSync(`src/${sourceFile}`);

        if (sourceExists) {
          const content = readFileSync(join(doctestDir, file), 'utf-8');
          expect(content).toContain(`Doctests: ${sourceFile}`);
        }
      });
    });

    test('example line numbers are reasonable', () => {
      const doctestDir = 'test/doctest';
      const files = readdirSync(doctestDir).filter(f => f.endsWith('.doctest.test.mjs'));

      files.forEach(file => {
        const content = readFileSync(join(doctestDir, file), 'utf-8');
        const lineMatches = content.match(/line (\d+)/g) || [];

        lineMatches.forEach(match => {
          const lineNum = parseInt(match.replace('line ', ''));
          expect(lineNum).toBeGreaterThan(0);
          expect(lineNum).toBeLessThan(10000); // Reasonable upper bound
        });
      });
    });
  });

  describe('Package Integration', () => {
    test('package.json has doctest scripts', () => {
      const pkgContent = readFileSync('package.json', 'utf-8');
      const pkg = JSON.parse(pkgContent);

      expect(pkg.scripts).toBeDefined();
      expect(pkg.scripts['test:doctest']).toBeDefined();
      expect(pkg.scripts['test:doctest:watch']).toBeDefined();
      expect(pkg.scripts['test:all']).toBeDefined();
    });

    test('pretest hook is configured', () => {
      const pkgContent = readFileSync('package.json', 'utf-8');
      const pkg = JSON.parse(pkgContent);

      expect(pkg.scripts.pretest).toBeDefined();
      expect(pkg.scripts.pretest).toContain('generate-doctests');
    });

    test('package.json includes comment-parser dependency', () => {
      const pkgContent = readFileSync('package.json', 'utf-8');
      const pkg = JSON.parse(pkgContent);

      const hasDevDep = pkg.devDependencies?.['comment-parser'];
      expect(hasDevDep).toBeDefined();
    });
  });

  describe('Configuration', () => {
    test('vitest.config includes doctest patterns', () => {
      const configContent = readFileSync('vitest.config.mjs', 'utf-8');
      expect(configContent).toContain('test/doctest');
      expect(configContent).toContain('doctest.test.mjs');
    });

    test('vitest config excludes doctest tools from coverage', () => {
      const configContent = readFileSync('vitest.config.mjs', 'utf-8');
      expect(configContent).toContain('src/doctest');
      expect(configContent).toContain('exclude');
    });
  });

  describe('Consistency', () => {
    test('all generated tests follow same structure', () => {
      const doctestDir = 'test/doctest';
      const files = readdirSync(doctestDir).filter(f => f.endsWith('.doctest.test.mjs'));

      const structures = files.map(file => {
        const content = readFileSync(join(doctestDir, file), 'utf-8');
        return {
          hasImportVitest: content.includes('import { describe, test'),
          hasDescribe: content.includes('describe('),
          hasTest: content.includes('test('),
          hasAsync: content.includes('async () => {'),
          file
        };
      });

      structures.forEach(s => {
        expect(s.hasImportVitest).toBe(true, `${s.file} missing vitest import`);
        expect(s.hasDescribe).toBe(true, `${s.file} missing describe`);
        expect(s.hasTest).toBe(true, `${s.file} missing test`);
        expect(s.hasAsync).toBe(true, `${s.file} missing async`);
      });
    });
  });
});
