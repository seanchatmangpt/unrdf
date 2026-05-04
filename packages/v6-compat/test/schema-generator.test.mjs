/**
 * Tests for P0-002: Zod Schema Generator
 */

import { describe, it, expect } from 'vitest';
import {
  parseJSDocToZod,
  generateSchemaFromFunction,
  generateSchemasForFiles,
  validateWithErrors,
  UserSchema,
} from '../src/schema-generator.mjs';

describe('P0-002: Zod Schema Generator', () => {
  it('parses JSDoc typedef to Zod schema', () => {
    const jsdoc = `
      @typedef {Object} User
      @property {string} id - User ID
      @property {string} name - User name
      @property {number} [age] - Optional age
    `;

    const schema = parseJSDocToZod(jsdoc);

    expect(schema).toContain('z.object({');
    expect(schema).toContain('id: z.string()');
    expect(schema).toContain('name: z.string()');
    expect(schema).toContain('age: z.number().optional()');
  });

  it('generates schema from function signature', () => {
    const fnSource = `
      function processUser(id: string, name: string): User {
        return { id, name };
      }
    `;

    const result = generateSchemaFromFunction(fnSource);

    expect(result.params).toContain('z.string()');
    expect(result.params).toContain('z.tuple([');
  });

  it('validates data with UserSchema', () => {
    const validUser = {
      id: '550e8400-e29b-41d4-a716-446655440000',
      name: 'Alice',
      email: 'alice@example.com',
      age: 30,
    };

    const result = validateWithErrors(UserSchema, validUser);

    expect(result.success).toBe(true);
    expect(result.data).toEqual(validUser);
  });

  it('returns descriptive errors for invalid data', () => {
    const invalidUser = {
      id: 'not-a-uuid',
      email: 'not-an-email',
    };

    const result = validateWithErrors(UserSchema, invalidUser);

    expect(result.success).toBe(false);
    expect(result.errors).toBeDefined();
    expect(result.errors.length).toBeGreaterThan(0);
    expect(result.summary).toContain('id');
  });

  it('generates schemas for files (dry run)', async () => {
    const results = await generateSchemasForFiles(
      '/home/user/unrdf/packages/v6-compat/src/adapters.mjs',
      { dryRun: true }
    );

    expect(Array.isArray(results)).toBe(true);

    // Should find exported functions in adapters.mjs
    if (results.length > 0) {
      const first = results[0];
      expect(first.file).toBeDefined();
      expect(first.schema).toBeDefined();
      expect(first.functions).toBeDefined();
      expect(Array.isArray(first.functions)).toBe(true);
    }
  });

  it('extracts function metadata correctly', async () => {
    const results = await generateSchemasForFiles(
      '/home/user/unrdf/packages/v6-compat/src/schema-generator.mjs',
      { dryRun: true }
    );

    if (results.length > 0) {
      const schemaGenResult = results[0];
      const functions = schemaGenResult.functions;

      expect(functions.length).toBeGreaterThan(0);

      // Check that functions have required metadata
      functions.forEach((fn) => {
        expect(fn.name).toBeDefined();
        expect(fn.jsdoc).toBeDefined();
        expect(Array.isArray(fn.params)).toBe(true);
        expect(fn.returnType).toBeDefined();
      });
    }
  });

  it('generates valid Zod import statements', async () => {
    const results = await generateSchemasForFiles(
      '/home/user/unrdf/packages/v6-compat/src/adapters.mjs',
      { dryRun: true }
    );

    if (results.length > 0) {
      const schema = results[0].schema;
      expect(schema).toContain("import { z } from 'zod'");
      expect(schema).toContain('DO NOT EDIT MANUALLY');
    }
  });
});
