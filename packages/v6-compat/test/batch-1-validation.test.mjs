/**
 * Batch 1 Schema Validation Tests
 *
 * Verifies that generated schemas:
 * 1. Can be imported without errors
 * 2. Export expected schemas
 * 3. Validate data correctly
 * 4. Reject invalid data
 */

import { describe, it, expect } from 'vitest';

describe('Batch 1: Generated Schema Validation', () => {
  it('imports adapters.schema.mjs successfully', async () => {
    const schemas = await import('../src/adapters.schema.mjs');

    expect(schemas).toBeDefined();
    expect(schemas.default).toBeDefined();
    expect(schemas.createStoreSchema).toBeDefined();
    expect(schemas.wrapWorkflowSchema).toBeDefined();
    expect(schemas.wrapFederationSchema).toBeDefined();
    expect(schemas.withReceiptSchema).toBeDefined();
    expect(schemas.validateSchemaSchema).toBeDefined();
  });

  it('imports types.schema.mjs successfully', async () => {
    const schemas = await import('../../core/src/types.schema.mjs');

    expect(schemas).toBeDefined();
    expect(schemas.default).toBeDefined();
    expect(schemas.createTermsSchema).toBeDefined();
    expect(schemas.createNamedNodeSchema).toBeDefined();
    expect(schemas.createLiteralSchema).toBeDefined();
    expect(schemas.createBlankNodeSchema).toBeDefined();
    expect(schemas.createVariableSchema).toBeDefined();
    expect(schemas.createQuadSchema).toBeDefined();
  });

  it('validates createNamedNode params with string', async () => {
    const { createNamedNodeParamsSchema } = await import('../../core/src/types.schema.mjs');

    const validResult = createNamedNodeParamsSchema.safeParse(['http://example.org/resource']);
    expect(validResult.success).toBe(true);

    const invalidResult = createNamedNodeParamsSchema.safeParse([123]); // number instead of string
    expect(invalidResult.success).toBe(false);
  });

  it('validates createLiteral params with optional arguments', async () => {
    const { createLiteralParamsSchema } = await import('../../core/src/types.schema.mjs');

    // All 3 params
    const result1 = createLiteralParamsSchema.safeParse(['value', 'en', 'http://example.org/datatype']);
    expect(result1.success).toBe(true);

    // Only required param
    const result2 = createLiteralParamsSchema.safeParse(['value']);
    expect(result2.success).toBe(true);

    // Empty tuple
    const result3 = createLiteralParamsSchema.safeParse([]);
    expect(result3.success).toBe(false);
  });

  it('validates createQuad params (4 required params)', async () => {
    const { createQuadParamsSchema } = await import('../../core/src/types.schema.mjs');

    // Valid: 4 params
    const validResult = createQuadParamsSchema.safeParse([
      { termType: 'NamedNode' },
      { termType: 'NamedNode' },
      { termType: 'Literal' },
      { termType: 'DefaultGraph' }
    ]);
    expect(validResult.success).toBe(true);

    // Invalid: only 3 params
    const invalidResult = createQuadParamsSchema.safeParse([
      { termType: 'NamedNode' },
      { termType: 'NamedNode' },
      { termType: 'Literal' }
    ]);
    expect(invalidResult.success).toBe(false);
  });

  it('validates schema-generator schemas', async () => {
    const { parseJSDocToZodParamsSchema, validateWithErrorsParamsSchema } =
      await import('../src/schema-generator.schema.mjs');

    // parseJSDocToZod: takes 1 string param
    const result1 = parseJSDocToZodParamsSchema.safeParse(['/** @param {string} foo */']);
    expect(result1.success).toBe(true);

    // validateWithErrors: takes 2 params (schema, data)
    const result2 = validateWithErrorsParamsSchema.safeParse([{}, { test: 'data' }]);
    expect(result2.success).toBe(true);

    const result3 = validateWithErrorsParamsSchema.safeParse([{}]); // Missing 2nd param
    expect(result3.success).toBe(false);
  });

  it('all schemas export default object', async () => {
    const schemaFiles = [
      '../src/adapters.schema.mjs',
      '../src/schema-generator.schema.mjs',
      '../../core/src/types.schema.mjs',
      '../../core/src/security.schema.mjs',
      '../../core/src/logger.schema.mjs',
    ];

    for (const file of schemaFiles) {
      const schemas = await import(file);
      expect(schemas.default).toBeDefined();
      expect(typeof schemas.default).toBe('object');
      expect(Object.keys(schemas.default).length).toBeGreaterThan(0);
    }
  });

  it('schema default export matches named exports', async () => {
    const schemas = await import('../src/adapters.schema.mjs');

    // Check that default export has keys matching named schemas
    expect(schemas.default.createStore).toBe(schemas.createStoreSchema);
    expect(schemas.default.wrapWorkflow).toBe(schemas.wrapWorkflowSchema);
    expect(schemas.default.wrapFederation).toBe(schemas.wrapFederationSchema);
    expect(schemas.default.withReceipt).toBe(schemas.withReceiptSchema);
    expect(schemas.default.validateSchema).toBe(schemas.validateSchemaSchema);
  });
});
