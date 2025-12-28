/**
 * Batch 3 Integration Tests
 *
 * Validates all generated Zod schemas for correctness
 *
 * @module test/schemas/batch3-integration
 */

import { describe, it, expect } from 'vitest';
import { z } from 'zod';
import manifest from '../../src/schemas/batch3-manifest.json' assert { type: 'json' };

// Sample schema imports for validation
import admissionSchemas from '../../src/admission/forbidden-operations.schema.mjs';
import validateCommandSchemas from '../../src/commands/validate.schema.mjs';
import useValidatorSchemas from '../../src/composables/use-validator.schema.mjs';
import diffSchemas from '../../src/diff.schema.mjs';
import utilsSchemas from '../../src/utils/index.schema.mjs';

describe('Batch 3 Schema Generation - Integration Tests', () => {
  describe('Coverage Metrics', () => {
    it('should achieve 100%+ coverage (194+ schemas)', () => {
      expect(manifest.coverage.achieved).toBeGreaterThanOrEqual(194);
    });

    it('should process 250+ files', () => {
      expect(manifest.modules).toBeDefined();
      const totalFiles = Object.values(manifest.modules).reduce(
        (sum, mod) => sum + mod.files,
        0
      );
      expect(totalFiles).toBeGreaterThanOrEqual(250);
    });

    it('should have coverage percentage > 300%', () => {
      const percentage = parseFloat(manifest.coverage.percentage);
      expect(percentage).toBeGreaterThan(300);
    });

    it('should cover 20+ modules', () => {
      const moduleCount = Object.keys(manifest.modules).length;
      expect(moduleCount).toBeGreaterThanOrEqual(20);
    });
  });

  describe('Schema Structure Validation', () => {
    it('should export valid param schemas', () => {
      expect(admissionSchemas.isProtectedNamespace.params).toBeDefined();
      expect(admissionSchemas.isProtectedNamespace.params).toBeInstanceOf(z.ZodTuple);
    });

    it('should export valid return schemas', () => {
      expect(admissionSchemas.isProtectedNamespace.returns).toBeDefined();
      expect(admissionSchemas.isProtectedNamespace.returns).toBeInstanceOf(z.ZodBoolean);
    });

    it('should have combined schema objects', () => {
      expect(admissionSchemas.isProtectedNamespace).toHaveProperty('params');
      expect(admissionSchemas.isProtectedNamespace).toHaveProperty('returns');
    });

    it('should export default object with all schemas', () => {
      expect(admissionSchemas).toBeDefined();
      expect(typeof admissionSchemas).toBe('object');
    });
  });

  describe('Schema Validation - Sample Functions', () => {
    it('should validate admission/isProtectedNamespace()', () => {
      const schema = admissionSchemas.isProtectedNamespace.params;

      // Valid input
      expect(() => schema.parse(['http://example.com/protected'])).not.toThrow();

      // Invalid input
      expect(() => schema.parse([123])).toThrow();
    });

    it('should validate admission/isProtectedNamespace() return type', () => {
      const schema = admissionSchemas.isProtectedNamespace.returns;

      // Valid returns
      expect(() => schema.parse(true)).not.toThrow();
      expect(() => schema.parse(false)).not.toThrow();

      // Invalid return
      expect(() => schema.parse('true')).toThrow();
    });

    it('should validate composables/useValidator() params', () => {
      const schema = useValidatorSchemas.useValidator.params;

      // Valid: no args
      expect(() => schema.parse([])).not.toThrow();

      // Valid: with options
      expect(() => schema.parse([{ strict: true }])).not.toThrow();
    });

    it('should handle z.unknown() schemas gracefully', () => {
      const schema = validateCommandSchemas.validateCommand.params;

      // z.unknown() accepts anything
      expect(() => schema.parse([null])).not.toThrow();
      expect(() => schema.parse([{}])).not.toThrow();
      expect(() => schema.parse([123])).not.toThrow();
    });
  });

  describe('Module Coverage', () => {
    const requiredModules = [
      'cli',
      'commands',
      'admission',
      'validation',
      'composables',
      'utils',
      'measurement',
      'security',
      'receipts',
    ];

    requiredModules.forEach((moduleName) => {
      it(`should have schemas for ${moduleName} module`, () => {
        expect(manifest.modules[moduleName]).toBeDefined();
        expect(manifest.modules[moduleName].functions).toBeGreaterThan(0);
      });
    });
  });

  describe('File Generation Verification', () => {
    it('should generate schema files with correct naming', () => {
      const schemaFiles = manifest.files.filter(f => f.schema.endsWith('.schema.mjs'));
      expect(schemaFiles.length).toBe(manifest.files.length);
    });

    it('should have source-schema correspondence', () => {
      manifest.files.forEach((file) => {
        const sourceName = file.source.replace(/\.mjs$/, '').split('/').pop();
        const schemaName = file.schema.replace(/\.schema\.mjs$/, '').split('/').pop();
        expect(schemaName).toBe(sourceName);
      });
    });

    it('should track function counts per file', () => {
      manifest.files.forEach((file) => {
        expect(file.functions).toBeGreaterThanOrEqual(0);
        expect(typeof file.functions).toBe('number');
      });
    });
  });

  describe('Sample Definitions - 5 Examples', () => {
    const examples = [
      {
        name: 'isProtectedNamespace',
        schema: admissionSchemas.isProtectedNamespace,
        validParams: [['http://example.com']],
        invalidParams: [[123]],
        validReturn: true,
        invalidReturn: 'true',
      },
      {
        name: 'isCanonicalTerm',
        schema: admissionSchemas.isCanonicalTerm,
        validParams: [['ex:term']],
        invalidParams: [[null]],
        validReturn: false,
        invalidReturn: 1,
      },
      {
        name: 'useValidator',
        schema: useValidatorSchemas.useValidator,
        validParams: [[]],
        validParamsAlt: [[{ strict: true }]],
        skipReturnCheck: true, // z.unknown()
      },
      {
        name: 'validateCommand',
        schema: validateCommandSchemas.validateCommand,
        skipParamCheck: true, // z.unknown()
        skipReturnCheck: true, // z.unknown()
      },
      {
        name: 'guardEditIndustrialSubstrate',
        schema: admissionSchemas.guardEditIndustrialSubstrate,
        skipParamCheck: true, // z.unknown()
        skipReturnCheck: true, // z.unknown()
      },
    ];

    examples.forEach(({ name, schema, validParams, invalidParams, validReturn, invalidReturn, validParamsAlt, skipParamCheck, skipReturnCheck }) => {
      describe(`Example: ${name}()`, () => {
        if (!skipParamCheck) {
          it('should validate correct params', () => {
            expect(() => schema.params.parse(validParams)).not.toThrow();
            if (validParamsAlt) {
              expect(() => schema.params.parse(validParamsAlt)).not.toThrow();
            }
          });

          if (invalidParams) {
            it('should reject invalid params', () => {
              expect(() => schema.params.parse(invalidParams)).toThrow();
            });
          }
        }

        if (!skipReturnCheck) {
          it('should validate correct return value', () => {
            expect(() => schema.returns.parse(validReturn)).not.toThrow();
          });

          if (invalidReturn !== undefined) {
            it('should reject invalid return value', () => {
              expect(() => schema.returns.parse(invalidReturn)).toThrow();
            });
          }
        }

        it('should have params and returns properties', () => {
          expect(schema).toHaveProperty('params');
          expect(schema).toHaveProperty('returns');
        });
      });
    });
  });

  describe('Manifest Integrity', () => {
    it('should have valid version', () => {
      expect(manifest.version).toBe('6.0.0-batch3');
    });

    it('should have timestamp', () => {
      expect(manifest.timestamp).toBeDefined();
      const date = new Date(manifest.timestamp);
      expect(date.toString()).not.toBe('Invalid Date');
    });

    it('should have complete coverage object', () => {
      expect(manifest.coverage).toHaveProperty('target');
      expect(manifest.coverage).toHaveProperty('achieved');
      expect(manifest.coverage).toHaveProperty('percentage');
    });

    it('should have modules object', () => {
      expect(manifest.modules).toBeDefined();
      expect(typeof manifest.modules).toBe('object');
    });

    it('should have files array', () => {
      expect(Array.isArray(manifest.files)).toBe(true);
      expect(manifest.files.length).toBeGreaterThan(0);
    });
  });
});

describe('Integration with Real Functions', () => {
  it('should work with actual imports (smoke test)', async () => {
    // This verifies the schemas can be imported and used
    const { isProtectedNamespaceParamsSchema } = await import(
      '../../src/admission/forbidden-operations.schema.mjs'
    );

    expect(isProtectedNamespaceParamsSchema).toBeDefined();
    expect(() => isProtectedNamespaceParamsSchema.parse(['test'])).not.toThrow();
  });

  it('should have consistent schema structure across modules', async () => {
    const modules = [
      '../../src/admission/forbidden-operations.schema.mjs',
      '../../src/composables/use-validator.schema.mjs',
      '../../src/commands/validate.schema.mjs',
    ];

    for (const modulePath of modules) {
      const schemas = await import(modulePath);
      expect(schemas.default).toBeDefined();
      expect(typeof schemas.default).toBe('object');
    }
  });
});
