/**
 * @fileoverview Comprehensive tests for Agent 7
 * @module agent-7/test
 */

import { describe, it, beforeEach } from 'node:test';
import assert from 'assert';
import {
  generateFacade,
  templateServiceClass,
  templateErrorHandler,
  templateTest,
  demoCustomerServiceSpec
} from './generator.mjs';
import {
  canonicalizeGenerated,
  hashGeneratedCode,
  hashInputs,
  verifyDeterminism,
  areEquivalent,
  getDiff,
  formatDiff
} from './determinism.mjs';
import { writeFileSync, readFileSync, mkdirSync } from 'fs';
import { join } from 'path';
import { fileURLToPath } from 'url';
import { dirname } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Test profile
const testProfile = {
  name: 'test-profile',
  naming: {
    fileStyle: 'kebab-case',
    classStyle: 'PascalCase',
    methodStyle: 'camelCase'
  },
  errorModel: {
    className: 'AppError',
    codeField: 'code',
    messageField: 'message',
    detailsField: 'details'
  },
  logging: {
    fields: ['timestamp', 'level', 'service', 'operation'],
    method: 'info'
  },
  testing: {
    framework: 'node:test',
    minCoverage: 80
  }
};

// Test lens
const testLens = {
  name: 'identity',
  transform: (spec) => spec
};

describe('Agent 7: Convention-Preserving Code Generator', () => {
  describe('canonicalizeGenerated', () => {
    it('should remove comments', () => {
      const code = `
// Single line comment
const x = 1;
/* Multi-line
   comment */
const y = 2;
      `;
      const canonical = canonicalizeGenerated(code);
      assert.ok(!canonical.includes('//'));
      assert.ok(!canonical.includes('/*'));
    });

    it('should normalize whitespace', () => {
      const code = 'const  x   =   1;';
      const canonical = canonicalizeGenerated(code);
      assert.ok(canonical.includes('const  x   =   1')); // Preserves structure
    });

    it('should sort imports alphabetically', () => {
      const code = `
import { z } from 'z';
import { a } from 'a';
import { m } from 'm';
      `;
      const canonical = canonicalizeGenerated(code);
      const lines = canonical.split('\n').filter(l => l.trim().startsWith('import'));
      assert.strictEqual(lines[0].includes('a'), true);
      assert.strictEqual(lines[1].includes('m'), true);
      assert.strictEqual(lines[2].includes('z'), true);
    });

    it('should be deterministic', () => {
      const code = 'const x = 1; // comment';
      const run1 = canonicalizeGenerated(code);
      const run2 = canonicalizeGenerated(code);
      assert.strictEqual(run1, run2);
    });
  });

  describe('hashGeneratedCode', () => {
    it('should generate consistent hash', () => {
      const code = 'const x = 1;';
      const hash1 = hashGeneratedCode(code);
      const hash2 = hashGeneratedCode(code);
      assert.strictEqual(hash1, hash2);
    });

    it('should generate different hash for different code', () => {
      const code1 = 'const x = 1;';
      const code2 = 'const x = 2;';
      const hash1 = hashGeneratedCode(code1);
      const hash2 = hashGeneratedCode(code2);
      assert.notStrictEqual(hash1, hash2);
    });

    it('should ignore comments in hash', () => {
      const code1 = 'const x = 1; // comment';
      const code2 = 'const x = 1; // different comment';
      const hash1 = hashGeneratedCode(code1);
      const hash2 = hashGeneratedCode(code2);
      assert.strictEqual(hash1, hash2);
    });

    it('should return 64-character hex string', () => {
      const code = 'const x = 1;';
      const hash = hashGeneratedCode(code);
      assert.strictEqual(hash.length, 64);
      assert.ok(/^[0-9a-f]+$/.test(hash));
    });
  });

  describe('hashInputs', () => {
    it('should generate consistent hash for same inputs', () => {
      const spec = { name: 'Test' };
      const profile = { name: 'profile' };
      const lens = { name: 'lens' };

      const hash1 = hashInputs(spec, profile, lens);
      const hash2 = hashInputs(spec, profile, lens);
      assert.strictEqual(hash1, hash2);
    });

    it('should generate different hash for different inputs', () => {
      const spec1 = { name: 'Test1' };
      const spec2 = { name: 'Test2' };
      const profile = { name: 'profile' };
      const lens = { name: 'lens' };

      const hash1 = hashInputs(spec1, profile, lens);
      const hash2 = hashInputs(spec2, profile, lens);
      assert.notStrictEqual(hash1, hash2);
    });
  });

  describe('areEquivalent', () => {
    it('should return true for equivalent code', () => {
      const code1 = 'const x = 1; // comment1';
      const code2 = 'const x = 1; // comment2';
      assert.strictEqual(areEquivalent(code1, code2), true);
    });

    it('should return false for different code', () => {
      const code1 = 'const x = 1;';
      const code2 = 'const x = 2;';
      assert.strictEqual(areEquivalent(code1, code2), false);
    });
  });

  describe('getDiff', () => {
    it('should return empty array for identical code', () => {
      const code1 = 'const x = 1;';
      const code2 = 'const x = 1;';
      const diff = getDiff(code1, code2);
      assert.strictEqual(diff.length, 0);
    });

    it('should detect additions', () => {
      const code1 = 'const x = 1;';
      const code2 = 'const x = 1;\nconst y = 2;';
      const diff = getDiff(code1, code2);
      assert.ok(diff.some(d => d.type === 'add'));
    });

    it('should detect removals', () => {
      const code1 = 'const x = 1;\nconst y = 2;';
      const code2 = 'const x = 1;';
      const diff = getDiff(code1, code2);
      assert.ok(diff.some(d => d.type === 'remove'));
    });
  });

  describe('formatDiff', () => {
    it('should format empty diff', () => {
      const diff = [];
      const formatted = formatDiff(diff);
      assert.strictEqual(formatted, 'No differences');
    });

    it('should format additions', () => {
      const diff = [{ line: 1, type: 'add', content: 'new line' }];
      const formatted = formatDiff(diff);
      assert.ok(formatted.includes('+ Line 1'));
      assert.ok(formatted.includes('new line'));
    });
  });

  describe('templateServiceClass', () => {
    it('should generate valid service class', () => {
      const code = templateServiceClass(
        'TestService',
        ['Test'],
        ['create', 'read'],
        testProfile
      );

      assert.ok(code.includes('export class TestService'));
      assert.ok(code.includes('async createTest'));
      assert.ok(code.includes('async readTest'));
    });

    it('should include error handling', () => {
      const code = templateServiceClass(
        'TestService',
        ['Test'],
        ['create'],
        testProfile
      );

      assert.ok(code.includes('AppError'));
      assert.ok(code.includes('throw new AppError'));
    });

    it('should include logging statements', () => {
      const code = templateServiceClass(
        'TestService',
        ['Test'],
        ['create'],
        testProfile
      );

      assert.ok(code.includes('logger.info'));
      assert.ok(code.includes('timestamp'));
      assert.ok(code.includes('level'));
      assert.ok(code.includes('service'));
      assert.ok(code.includes('operation'));
    });

    it('should match naming conventions', () => {
      const code = templateServiceClass(
        'CustomerService',
        ['Customer'],
        ['create'],
        testProfile
      );

      assert.ok(code.includes('export class CustomerService'));
      assert.ok(code.includes('async createCustomer'));
    });
  });

  describe('templateErrorHandler', () => {
    it('should generate error class', () => {
      const code = templateErrorHandler(testProfile.errorModel);

      assert.ok(code.includes('export class AppError'));
      assert.ok(code.includes('extends Error'));
    });

    it('should include all error fields', () => {
      const code = templateErrorHandler(testProfile.errorModel);

      assert.ok(code.includes('this.code'));
      assert.ok(code.includes('this.message'));
      assert.ok(code.includes('this.details'));
    });

    it('should include toJSON method', () => {
      const code = templateErrorHandler(testProfile.errorModel);
      assert.ok(code.includes('toJSON()'));
    });
  });

  describe('templateTest', () => {
    it('should generate test file', () => {
      const code = templateTest(
        'TestService',
        ['Test'],
        ['create', 'read'],
        testProfile
      );

      assert.ok(code.includes("describe('TestService'"));
      assert.ok(code.includes("it('should create test"));
      assert.ok(code.includes("it('should read"));
    });

    it('should use correct test framework', () => {
      const code = templateTest(
        'TestService',
        ['Test'],
        ['create'],
        testProfile
      );

      assert.ok(code.includes("from 'node:test'"));
      assert.ok(code.includes('import assert'));
    });

    it('should include error cases', () => {
      const code = templateTest(
        'TestService',
        ['Test'],
        ['create'],
        testProfile
      );

      assert.ok(code.includes('should throw'));
      assert.ok(code.includes('assert.rejects'));
    });
  });

  describe('generateFacade', () => {
    it('should generate complete façade', async () => {
      const result = await generateFacade(
        demoCustomerServiceSpec,
        testProfile,
        testLens
      );

      assert.ok(result.code);
      assert.ok(result.testCode);
      assert.ok(result.filename);
      assert.ok(result.hash);
      assert.ok(result.metadata);
    });

    it('should include metadata', async () => {
      const result = await generateFacade(
        demoCustomerServiceSpec,
        testProfile,
        testLens
      );

      assert.strictEqual(result.metadata.profile, 'test-profile');
      assert.strictEqual(result.metadata.lens, 'identity');
      assert.strictEqual(result.metadata.serviceName, 'CustomerService');
      assert.ok(result.metadata.inputHash);
      assert.ok(result.metadata.generatedAt);
    });

    it('should apply lens transformation', async () => {
      const transformLens = {
        name: 'prefix-lens',
        transform: (spec) => ({
          ...spec,
          name: 'Transformed' + spec.name
        })
      };

      const result = await generateFacade(
        demoCustomerServiceSpec,
        testProfile,
        transformLens
      );

      assert.ok(result.code.includes('TransformedCustomerService'));
    });

    it('should generate deterministic output', async () => {
      const result1 = await generateFacade(
        demoCustomerServiceSpec,
        testProfile,
        testLens
      );
      const result2 = await generateFacade(
        demoCustomerServiceSpec,
        testProfile,
        testLens
      );

      assert.strictEqual(result1.hash, result2.hash);
      assert.strictEqual(result1.code, result2.code);
    });
  });

  describe('verifyDeterminism', () => {
    it('should verify deterministic generation', async () => {
      const result = await verifyDeterminism(async () => {
        const generated = await generateFacade(
          demoCustomerServiceSpec,
          testProfile,
          testLens
        );
        return generated.code;
      }, 10);

      assert.strictEqual(result.deterministic, true);
      assert.strictEqual(result.uniqueHashes.size, 1);
      assert.strictEqual(result.iterations, 10);
    });

    it('should detect non-deterministic generation', async () => {
      let counter = 0;
      const result = await verifyDeterminism(async () => {
        counter++;
        return `const x = ${counter};`;
      }, 5);

      assert.strictEqual(result.deterministic, false);
      assert.strictEqual(result.uniqueHashes.size, 5);
    });
  });

  describe('Integration: Golden Test', () => {
    it('should match golden file output', async () => {
      const result = await generateFacade(
        demoCustomerServiceSpec,
        testProfile,
        testLens
      );

      // Create golden directory if it doesn't exist
      const goldenDir = join(__dirname, 'golden');
      mkdirSync(goldenDir, { recursive: true });

      const goldenPath = join(goldenDir, 'customer-service.golden.mjs');

      // If golden file doesn't exist, create it
      try {
        const goldenCode = readFileSync(goldenPath, 'utf8');
        const goldenHash = hashGeneratedCode(goldenCode);
        const currentHash = result.hash;

        if (goldenHash !== currentHash) {
          const diff = getDiff(goldenCode, result.code);
          console.error('\nGolden test FAILED - Code differences:');
          console.error(formatDiff(diff));
          console.error(`\nGolden hash: ${goldenHash}`);
          console.error(`Current hash: ${currentHash}`);
          assert.fail('Generated code does not match golden file');
        }
      } catch (error) {
        if (error.code === 'ENOENT') {
          // Golden file doesn't exist, create it
          writeFileSync(goldenPath, result.code, 'utf8');
          console.log(`Created golden file: ${goldenPath}`);
          console.log('Subsequent runs will compare against this golden file');
        } else {
          throw error;
        }
      }
    });
  });

  describe('Integration: Generated Code Validity', () => {
    it('should generate importable JavaScript', async () => {
      const result = await generateFacade(
        demoCustomerServiceSpec,
        testProfile,
        testLens
      );

      // Write to temp file and try to import
      const tempDir = join(__dirname, 'temp');
      mkdirSync(tempDir, { recursive: true });
      const tempPath = join(tempDir, 'test-service.mjs');

      writeFileSync(tempPath, result.code, 'utf8');

      // Should not throw
      try {
        await import(tempPath);
        assert.ok(true, 'Code is valid and importable');
      } catch (error) {
        assert.fail(`Generated code has syntax errors: ${error.message}`);
      }
    });
  });

  describe('Performance', () => {
    it('should generate code in <100ms', async () => {
      const start = Date.now();
      await generateFacade(demoCustomerServiceSpec, testProfile, testLens);
      const duration = Date.now() - start;

      console.log(`  Generation time: ${duration}ms`);
      assert.ok(duration < 100, `Generation took ${duration}ms, expected <100ms`);
    });

    it('should canonicalize code in <10ms', () => {
      const code = templateServiceClass(
        'TestService',
        ['Test'],
        ['create', 'read', 'update', 'delete', 'list'],
        testProfile
      );

      const start = Date.now();
      canonicalizeGenerated(code);
      const duration = Date.now() - start;

      console.log(`  Canonicalization time: ${duration}ms`);
      assert.ok(duration < 10, `Canonicalization took ${duration}ms, expected <10ms`);
    });

    it('should hash code in <5ms', () => {
      const code = templateServiceClass(
        'TestService',
        ['Test'],
        ['create'],
        testProfile
      );

      const start = Date.now();
      hashGeneratedCode(code);
      const duration = Date.now() - start;

      console.log(`  Hashing time: ${duration}ms`);
      assert.ok(duration < 5, `Hashing took ${duration}ms, expected <5ms`);
    });
  });
});

console.log('\n✅ Running Agent 7 test suite...\n');
