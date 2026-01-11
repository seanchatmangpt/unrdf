/**
 * Property-Based Test Generator from SHACL/Zod Constraints
 * @module @unrdf/codegen/property-test-generator
 * @description
 * Innovation: Generate property-based tests from schema constraints
 *
 * Pattern 4 from Code Generation Research
 */

import { z } from 'zod';
import { createHash } from 'crypto';

const PropertyTestOptionsSchema = z.object({
  framework: z.enum(['fast-check', 'vitest']).default('fast-check'),
  testCount: z.number().int().positive().default(100),
  includeEdgeCases: z.boolean().default(true),
  verbose: z.boolean().default(false),
});

/**
 * Generate property-based tests from Zod schema
 * @param {z.ZodSchema} schema - Zod schema to generate tests from
 * @param {Object} options - Generation options
 * @returns {Promise<Object>} Generated test code
 */
export async function generatePropertyTests(schema, options = {}) {
  const config = PropertyTestOptionsSchema.parse(options);

  // Extract constraints from schema
  const constraints = extractConstraints(schema);

  if (constraints.length === 0) {
    return {
      code: '// No constraints found to generate property tests',
      testCount: 0,
      constraints: [],
    };
  }

  // Generate test cases
  const testCases = constraints.map(constraint =>
    generateTestFromConstraint(constraint, config)
  );

  // Generate complete test file
  const code = generateTestFile(testCases, schema, config);

  return {
    code,
    testCount: testCases.length,
    constraints: constraints.map(c => c.type),
    metadata: {
      framework: config.framework,
      testCount: config.testCount,
      generatedAt: new Date().toISOString(),
    },
  };
}

/**
 * Extract constraints from Zod schema
 * @param {z.ZodSchema} schema - Zod schema
 * @returns {Array} Array of constraint objects
 */
function extractConstraints(schema) {
  const constraints = [];

  // Handle ZodObject
  if (schema._def?.typeName === 'ZodObject') {
    const shape = schema._def.shape();

    for (const [fieldName, fieldSchema] of Object.entries(shape)) {
      const fieldConstraints = extractFieldConstraints(fieldName, fieldSchema);
      constraints.push(...fieldConstraints);
    }
  }

  // Handle direct schema
  else {
    const directConstraints = extractFieldConstraints('value', schema);
    constraints.push(...directConstraints);
  }

  return constraints;
}

/**
 * Extract constraints from a field schema
 * @param {string} fieldName - Field name
 * @param {z.ZodSchema} fieldSchema - Field schema
 * @returns {Array} Field constraints
 */
function extractFieldConstraints(fieldName, fieldSchema) {
  const constraints = [];
  const typeName = fieldSchema._def?.typeName;

  // String constraints
  if (typeName === 'ZodString') {
    const checks = fieldSchema._def?.checks || [];

    for (const check of checks) {
      if (check.kind === 'min') {
        constraints.push({
          field: fieldName,
          type: 'minLength',
          value: check.value,
          message: check.message,
        });
      }
      else if (check.kind === 'max') {
        constraints.push({
          field: fieldName,
          type: 'maxLength',
          value: check.value,
          message: check.message,
        });
      }
      else if (check.kind === 'email') {
        constraints.push({
          field: fieldName,
          type: 'email',
          message: check.message,
        });
      }
      else if (check.kind === 'url') {
        constraints.push({
          field: fieldName,
          type: 'url',
          message: check.message,
        });
      }
      else if (check.kind === 'regex') {
        constraints.push({
          field: fieldName,
          type: 'pattern',
          value: check.regex.source,
          message: check.message,
        });
      }
    }
  }

  // Number constraints
  else if (typeName === 'ZodNumber') {
    const checks = fieldSchema._def?.checks || [];

    for (const check of checks) {
      if (check.kind === 'min') {
        constraints.push({
          field: fieldName,
          type: 'min',
          value: check.value,
          inclusive: check.inclusive !== false,
        });
      }
      else if (check.kind === 'max') {
        constraints.push({
          field: fieldName,
          type: 'max',
          value: check.value,
          inclusive: check.inclusive !== false,
        });
      }
      else if (check.kind === 'int') {
        constraints.push({
          field: fieldName,
          type: 'integer',
        });
      }
    }
  }

  // Array constraints
  else if (typeName === 'ZodArray') {
    if (fieldSchema._def?.minLength) {
      constraints.push({
        field: fieldName,
        type: 'minItems',
        value: fieldSchema._def.minLength.value,
      });
    }
    if (fieldSchema._def?.maxLength) {
      constraints.push({
        field: fieldName,
        type: 'maxItems',
        value: fieldSchema._def.maxLength.value,
      });
    }
  }

  // Optional check
  if (typeName === 'ZodOptional') {
    constraints.push({
      field: fieldName,
      type: 'optional',
    });
  }

  return constraints;
}

/**
 * Generate test code from constraint
 * @param {Object} constraint - Constraint object
 * @param {Object} config - Configuration
 * @returns {string} Test code
 */
function generateTestFromConstraint(constraint, config) {
  const { field, type, value } = constraint;

  switch (type) {
    case 'minLength':
      return `
  it('should enforce minLength=${value} on ${field}', () => {
    fc.assert(
      fc.property(
        fc.string({ minLength: ${value} }),
        (value) => {
          const result = schema.safeParse({ ${field}: value });
          expect(result.success).toBe(true);
        }
      ),
      { numRuns: ${config.testCount} }
    );
  });`.trim();

    case 'maxLength':
      return `
  it('should enforce maxLength=${value} on ${field}', () => {
    fc.assert(
      fc.property(
        fc.string({ maxLength: ${value} }),
        (value) => {
          const result = schema.safeParse({ ${field}: value });
          expect(result.success).toBe(true);
        }
      ),
      { numRuns: ${config.testCount} }
    );
  });`.trim();

    case 'email':
      return `
  it('should validate email format on ${field}', () => {
    fc.assert(
      fc.property(
        fc.emailAddress(),
        (email) => {
          const result = schema.safeParse({ ${field}: email });
          expect(result.success).toBe(true);
        }
      ),
      { numRuns: ${config.testCount} }
    );
  });`.trim();

    case 'url':
      return `
  it('should validate URL format on ${field}', () => {
    fc.assert(
      fc.property(
        fc.webUrl(),
        (url) => {
          const result = schema.safeParse({ ${field}: url });
          expect(result.success).toBe(true);
        }
      ),
      { numRuns: ${config.testCount} }
    );
  });`.trim();

    case 'pattern':
      return `
  it('should match pattern /${value}/ on ${field}', () => {
    fc.assert(
      fc.property(
        fc.stringMatching(/${value}/),
        (value) => {
          const result = schema.safeParse({ ${field}: value });
          expect(result.success).toBe(true);
        }
      ),
      { numRuns: ${config.testCount} }
    );
  });`.trim();

    case 'min':
      return `
  it('should enforce min=${value} on ${field}', () => {
    fc.assert(
      fc.property(
        fc.integer({ min: ${value} }),
        (num) => {
          const result = schema.safeParse({ ${field}: num });
          expect(result.success).toBe(true);
        }
      ),
      { numRuns: ${config.testCount} }
    );
  });`.trim();

    case 'max':
      return `
  it('should enforce max=${value} on ${field}', () => {
    fc.assert(
      fc.property(
        fc.integer({ max: ${value} }),
        (num) => {
          const result = schema.safeParse({ ${field}: num });
          expect(result.success).toBe(true);
        }
      ),
      { numRuns: ${config.testCount} }
    );
  });`.trim();

    case 'integer':
      return `
  it('should enforce integer constraint on ${field}', () => {
    fc.assert(
      fc.property(
        fc.integer(),
        (num) => {
          const result = schema.safeParse({ ${field}: num });
          expect(result.success).toBe(true);
        }
      ),
      { numRuns: ${config.testCount} }
    );
  });`.trim();

    case 'minItems':
      return `
  it('should enforce minItems=${value} on ${field}', () => {
    fc.assert(
      fc.property(
        fc.array(fc.anything(), { minLength: ${value} }),
        (arr) => {
          const result = schema.safeParse({ ${field}: arr });
          expect(result.success).toBe(true);
        }
      ),
      { numRuns: ${config.testCount} }
    );
  });`.trim();

    case 'maxItems':
      return `
  it('should enforce maxItems=${value} on ${field}', () => {
    fc.assert(
      fc.property(
        fc.array(fc.anything(), { maxLength: ${value} }),
        (arr) => {
          const result = schema.safeParse({ ${field}: arr });
          expect(result.success).toBe(true);
        }
      ),
      { numRuns: ${config.testCount} }
    );
  });`.trim();

    default:
      return `  // Unsupported constraint: ${type}`;
  }
}

/**
 * Generate complete test file
 * @param {Array} testCases - Array of test case strings
 * @param {z.ZodSchema} schema - Schema being tested
 * @param {Object} config - Configuration
 * @returns {string} Complete test file content
 */
function generateTestFile(testCases, schema, config) {
  const schemaName = schema._def?.description || 'Schema';

  return `
/**
 * Property-Based Tests for ${schemaName}
 * Auto-generated from Zod schema constraints
 * Generated: ${new Date().toISOString()}
 *
 * Framework: ${config.framework}
 * Test runs per property: ${config.testCount}
 */

import { describe, it, expect } from 'vitest';
import * as fc from 'fast-check';
import { schema } from '../schema.mjs';

describe('${schemaName} Property Tests', () => {
${testCases.join('\n\n')}
});
  `.trim();
}

/**
 * Generate property tests from SHACL constraints (RDF-based)
 * @param {Object} shaclStore - RDF store with SHACL shapes
 * @param {string} targetClass - Target class IRI
 * @param {Object} options - Generation options
 * @returns {Promise<Object>} Generated tests
 */
export async function generateFromSHACL(shaclStore, targetClass, options = {}) {
  const config = PropertyTestOptionsSchema.parse(options);

  // Query SHACL constraints
  const constraintsQuery = `
    PREFIX sh: <http://www.w3.org/ns/shacl#>

    SELECT ?property ?constraint ?value WHERE {
      ?shape sh:targetClass <${targetClass}> .
      ?shape sh:property ?propShape .
      ?propShape sh:path ?property .
      ?propShape ?constraint ?value .
      FILTER(?constraint IN (sh:minLength, sh:maxLength, sh:pattern, sh:minInclusive, sh:maxInclusive))
    }
  `;

  const results = await shaclStore.query(constraintsQuery);
  const constraints = results.map(binding => ({
    field: extractLocalName(binding.get('property').value),
    type: mapSHACLConstraint(extractLocalName(binding.get('constraint').value)),
    value: binding.get('value').value,
  }));

  // Generate tests from SHACL constraints
  const testCases = constraints.map(constraint =>
    generateTestFromConstraint(constraint, config)
  );

  const code = generateTestFile(testCases, { _def: { description: targetClass } }, config);

  return {
    code,
    testCount: testCases.length,
    constraints: constraints.map(c => c.type),
  };
}

/**
 * Map SHACL constraint to internal type
 * @param {string} shaclConstraint - SHACL constraint name
 * @returns {string} Internal constraint type
 */
function mapSHACLConstraint(shaclConstraint) {
  const map = {
    minLength: 'minLength',
    maxLength: 'maxLength',
    pattern: 'pattern',
    minInclusive: 'min',
    maxInclusive: 'max',
  };
  return map[shaclConstraint] || shaclConstraint;
}

/**
 * Extract local name from IRI
 * @param {string} iri - Full IRI
 * @returns {string} Local name
 */
function extractLocalName(iri) {
  const match = iri.match(/[#/]([^#/]+)$/);
  return match ? match[1] : iri;
}

export default generatePropertyTests;
