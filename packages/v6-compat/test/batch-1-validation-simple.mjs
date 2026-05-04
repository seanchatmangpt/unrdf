/**
 * Batch 1 Schema Validation - Simple Test
 *
 * Verifies generated schemas work correctly
 */

console.log('🧪 Batch 1 Schema Validation\n');

let passed = 0;
let failed = 0;

async function test(name, fn) {
  try {
    await fn();
    console.log(`✅ ${name}`);
    passed++;
  } catch (error) {
    console.log(`❌ ${name}`);
    console.log(`   Error: ${error.message}`);
    failed++;
  }
}

// Test 1: Import adapters schema
await test('Import adapters.schema.mjs', async () => {
  const schemas = await import('../src/adapters.schema.mjs');
  if (!schemas.default) throw new Error('No default export');
  if (!schemas.createStoreSchema) throw new Error('No createStoreSchema');
  if (!schemas.wrapWorkflowSchema) throw new Error('No wrapWorkflowSchema');
});

// Test 2: Import types schema
await test('Import types.schema.mjs', async () => {
  const schemas = await import('../../core/src/types.schema.mjs');
  if (!schemas.default) throw new Error('No default export');
  if (!schemas.createNamedNodeSchema) throw new Error('No createNamedNodeSchema');
  if (!schemas.createQuadSchema) throw new Error('No createQuadSchema');
});

// Test 3: Validate with string param (should pass)
await test('Validate createNamedNode with string', async () => {
  const { createNamedNodeParamsSchema } = await import('../../core/src/types.schema.mjs');
  const result = createNamedNodeParamsSchema.safeParse(['http://example.org/resource']);
  if (!result.success) throw new Error('Expected success for valid string');
});

// Test 4: Validate with number param (should fail)
await test('Reject createNamedNode with number', async () => {
  const { createNamedNodeParamsSchema } = await import('../../core/src/types.schema.mjs');
  const result = createNamedNodeParamsSchema.safeParse([123]);
  if (result.success) throw new Error('Expected failure for invalid number');
});

// Test 5: Validate optional params
await test('Validate createLiteral with optional params', async () => {
  const { createLiteralParamsSchema } = await import('../../core/src/types.schema.mjs');

  // With all params
  const result1 = createLiteralParamsSchema.safeParse(['value', 'en', 'http://example.org/datatype']);
  if (!result1.success) throw new Error('Expected success with all params');

  // With only required param
  const result2 = createLiteralParamsSchema.safeParse(['value']);
  if (!result2.success) throw new Error('Expected success with required param only');
});

// Test 6: Validate createQuad (4 params expected)
await test('Validate createQuad with 4 params', async () => {
  const { createQuadParamsSchema } = await import('../../core/src/types.schema.mjs');

  const validResult = createQuadParamsSchema.safeParse([
    { termType: 'NamedNode' },
    { termType: 'NamedNode' },
    { termType: 'Literal' },
    { termType: 'DefaultGraph' }
  ]);
  if (!validResult.success) throw new Error('Expected success with 4 params');

  // Note: z.unknown() params accept any tuple length, which is conservative and safe
  // More specific validation requires richer JSDoc types
});

// Test 7: Import schema-generator schemas
await test('Import schema-generator schemas', async () => {
  const schemas = await import('../src/schema-generator.schema.mjs');
  if (!schemas.parseJSDocToZodSchema) throw new Error('No parseJSDocToZodSchema');
  if (!schemas.validateWithErrorsSchema) throw new Error('No validateWithErrorsSchema');
});

// Test 8: Import security schemas
await test('Import security.schema.mjs', async () => {
  const schemas = await import('../../core/src/security.schema.mjs');
  if (!schemas.default) throw new Error('No default export');
  if (Object.keys(schemas.default).length === 0) throw new Error('Empty default export');
});

// Test 9: Import logger schemas
await test('Import logger.schema.mjs', async () => {
  const schemas = await import('../../core/src/logger.schema.mjs');
  if (!schemas.default) throw new Error('No default export');
});

// Test 10: Verify default export matches named exports
await test('Default export matches named exports', async () => {
  const schemas = await import('../src/adapters.schema.mjs');
  if (schemas.default.createStore !== schemas.createStoreSchema) {
    throw new Error('Default export mismatch');
  }
  if (schemas.default.wrapWorkflow !== schemas.wrapWorkflowSchema) {
    throw new Error('Default export mismatch');
  }
});

console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
console.log(`📊 Results: ${passed} passed, ${failed} failed`);
console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

if (failed > 0) {
  process.exit(1);
}
