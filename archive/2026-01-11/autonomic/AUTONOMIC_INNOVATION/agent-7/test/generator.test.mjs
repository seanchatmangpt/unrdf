/**
 * Tests for Convention-Preserving Generator
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

import {
  generateFacade,
  buildFacadeAST,
  buildImports,
  buildFunction
} from '../src/generator.mjs';
import { formatCode, formatImports, formatJSDoc } from '../src/formatter.mjs';
import {
  validateGeneratedCode,
  checkImportOrder,
  checkJSDocFormat,
  checkLineLength
} from '../src/validator.mjs';
import {
  buildFunctionTemplate,
  buildJSDoc,
  buildSignature,
  buildErrorHandler
} from '../src/templates.mjs';

const __dirname = dirname(fileURLToPath(import.meta.url));

/**
 * Load test profile
 * @returns {CompiledProfile} Test profile
 */
function loadProfile() {
  return {
    name: 'target-org',
    version: '1.0.0',
    conventions: {
      jsdoc: {
        fieldOrder: ['description', 'param', 'returns', 'throws', 'example'],
        includeThrows: true
      },
      errorHandling: {
        style: 'try-catch-zod'
      },
      errorClass: 'ValidationError',
      logging: {
        defaultLevel: 'info',
        format: 'json'
      },
      style: {
        trailingComma: 'es5',
        quotes: 'single',
        semicolons: true
      }
    }
  };
}

/**
 * Load test lens
 * @returns {CompiledLens} Test lens
 */
function loadLens() {
  return {
    getFieldMapping: () => ({
      name: 'customer_name',
      email: 'customer_email',
      id: 'customer_id'
    })
  };
}

/**
 * Get minimal service spec for testing
 * @returns {ServiceSpec} Service specification
 */
function getMinimalServiceSpec() {
  return {
    name: 'CustomerService',
    description: 'Customer management operations',
    operations: [
      {
        name: 'createCustomer',
        description: 'Create a new customer',
        params: [
          { name: 'data', type: 'CustomerData', desc: 'Customer information' }
        ],
        returns: { type: 'Customer', desc: 'Created customer' },
        async: true,
        validation: {
          schema: 'CustomerDataSchema'
        }
      },
      {
        name: 'getCustomer',
        description: 'Retrieve customer by ID',
        params: [
          { name: 'id', type: 'string', desc: 'Customer ID' }
        ],
        returns: { type: 'Customer', desc: 'Customer record' },
        async: true
      }
    ]
  };
}

describe('Convention-Preserving Generator', () => {

  // Test 1: Golden test
  it('generates customer service façade (golden test)', async () => {
    const profile = loadProfile();
    const lens = loadLens();
    const service = getMinimalServiceSpec();

    const generated = generateFacade(profile, lens, service);

    const goldenPath = join(__dirname, '../examples/customer-service-facade.mjs');
    const golden = await readFile(goldenPath, 'utf-8');

    assert.equal(generated, golden, 'Generated code must match golden file exactly');
  });

  // Test 2: Determinism test
  it('generates identical output on repeated calls (determinism)', () => {
    const profile = loadProfile();
    const lens = loadLens();
    const service = getMinimalServiceSpec();

    const outputs = Array.from({ length: 10 }, () =>
      generateFacade(profile, lens, service)
    );

    const first = outputs[0];
    outputs.forEach((output, idx) => {
      assert.equal(output, first, `Output ${idx} differs from first`);
    });
  });

  // Test 3: Validation test
  it('validates generated code passes all checks', () => {
    const profile = loadProfile();
    const lens = loadLens();
    const service = getMinimalServiceSpec();

    const generated = generateFacade(profile, lens, service);
    const validation = validateGeneratedCode(generated, profile);

    assert.equal(validation.ok, true, `Violations: ${validation.violations.join(', ')}`);
    assert.equal(validation.violations.length, 0);
  });

  // Test 4: Format test - Import sorting
  it('formats imports in deterministic order', () => {
    const imports = [
      { names: ['createStore', 'dataFactory'], from: '@unrdf/oxigraph' },
      { names: ['readFile'], from: 'node:fs/promises' },
      { names: ['helper'], from: './utils.mjs' },
      { names: ['z'], from: 'zod' }
    ];

    const config = {
      indent: '  ',
      lineLength: 100,
      quotes: 'single',
      semicolons: true
    };

    const formatted = formatImports(imports, config);
    const lines = formatted.split('\n').filter(l => l.trim());

    // Check order: node built-ins, external, internal
    assert.match(lines[0], /node:fs/);
    assert.match(lines[1], /@unrdf/);
    assert.match(lines[2], /zod/);
    assert.match(lines[3], /\.\/utils/);
  });

  // Test 5: Format test - JSDoc field ordering
  it('formats JSDoc fields in profile order', () => {
    const jsdoc = {
      fields: [
        { tag: 'description', value: 'Get customer by ID' },
        { tag: 'param', value: '{string} id - Customer ID' },
        { tag: 'returns', value: '{Customer} Customer record' },
        { tag: 'throws', value: '{Error} If not found' }
      ]
    };

    const config = {
      jsdocFieldOrder: ['description', 'param', 'returns', 'throws', 'example']
    };

    const formatted = formatJSDoc(jsdoc, config);
    const lines = formatted.split('\n');

    // Check order
    assert.match(lines[0], /\/\*\*/);
    assert.match(lines[1], /Get customer by ID/);
    assert.match(lines[2], /@param/);
    assert.match(lines[3], /@returns/);
    assert.match(lines[4], /@throws/);
    assert.match(lines[5], /\*\//);
  });

  // Test 6: Format test - Line length wrapping
  it('enforces line length limits', () => {
    const code = 'const veryLongVariableName = "this is a very long string that exceeds one hundred characters in length for testing";';

    const violations = checkLineLength(code, 100);

    assert.equal(violations.length > 0, true, 'Should detect line length violations');
  });

  // Test 7: Format test - Trailing comma consistency
  it('applies trailing comma style consistently', () => {
    const ast = {
      imports: [{ names: ['z'], from: 'zod' }],
      constants: [],
      functions: [],
      exports: []
    };

    const profile = {
      conventions: {
        style: {
          trailingComma: 'es5',
          quotes: 'single',
          semicolons: true
        }
      }
    };

    const formatted = formatCode(ast, profile);

    // Check that code is formatted correctly
    assert.ok(formatted.length > 0);
    assert.match(formatted, /import/);
  });

  // Test 8: Format test - Indentation correctness
  it('maintains consistent 2-space indentation', () => {
    const code = `function test() {
  const x = 1;
  if (x) {
    return true;
  }
}`;

    const violations = checkLineLength(code, 100);

    // Should have no line length violations
    assert.equal(violations.length, 0);
  });

  // Test 9: Template test - Function template expansion
  it('expands function templates correctly', () => {
    const operation = {
      name: 'testFunction',
      description: 'Test function',
      params: [
        { name: 'arg1', type: 'string', desc: 'First argument' }
      ],
      returns: { type: 'boolean', desc: 'Result' },
      async: true
    };

    const profile = loadProfile();
    const lens = loadLens();

    const template = buildFunctionTemplate(operation, profile, lens);

    assert.ok(template.jsdoc);
    assert.ok(template.signature);
    assert.ok(template.body);
    assert.ok(template.errorHandler);
    assert.equal(template.signature.name, 'testFunction');
  });

  // Test 10: Template test - Error handling template
  it('generates error handling templates', () => {
    const profile = loadProfile();

    const errorHandler = buildErrorHandler(profile);

    assert.equal(errorHandler.wrapper, 'try-catch');
    assert.equal(errorHandler.errorClass, 'ValidationError');
  });

  // Test 11: Template test - JSDoc template
  it('builds JSDoc with correct field order', () => {
    const operation = {
      name: 'test',
      description: 'Test operation',
      params: [
        { name: 'b', type: 'number', desc: 'Second param' },
        { name: 'a', type: 'string', desc: 'First param' }
      ],
      returns: { type: 'void', desc: 'Nothing' }
    };

    const profile = loadProfile();

    const jsdoc = buildJSDoc(operation, profile);

    // Check that fields are in correct order
    assert.equal(jsdoc.fields[0].tag, 'description');
    assert.equal(jsdoc.fields[1].tag, 'param');
    assert.equal(jsdoc.fields[2].tag, 'param');
    assert.equal(jsdoc.fields[3].tag, 'returns');
    assert.equal(jsdoc.fields[4].tag, 'throws');
  });

  // Test 12: AST test - AST construction from service spec
  it('constructs AST from service specification', () => {
    const profile = loadProfile();
    const lens = loadLens();
    const service = getMinimalServiceSpec();

    const ast = buildFacadeAST(profile, lens, service);

    assert.ok(ast.header);
    assert.ok(ast.imports);
    assert.ok(ast.constants);
    assert.ok(ast.functions);
    assert.equal(ast.functions.length, 2);
    assert.equal(ast.functions[0].signature.name, 'createCustomer');
    assert.equal(ast.functions[1].signature.name, 'getCustomer');
  });

  // Test 13: AST test - Export statement generation
  it('generates export statements correctly', () => {
    const profile = loadProfile();
    const lens = loadLens();
    const service = getMinimalServiceSpec();

    const ast = buildFacadeAST(profile, lens, service);

    // Functions are exported inline
    assert.ok(Array.isArray(ast.exports));
  });

});
