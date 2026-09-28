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
      metadata: {
        framework: config.framework,
        testCount: config.testCount,
        generatedAt: new Date().toISOString(),
      },
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
 * Read the schema kind. Zod v4 exposes `_def.type`; Zod v3 exposed `_def.typeName`.
 * @param {z.ZodSchema} schema - Zod schema
 * @returns {string|undefined} Normalised lower-case kind (object, string, number, array, optional, ...)
 */
function schemaKind(schema) {
  const def = schema?._def ?? schema?._zod?.def;
  if (def?.type) return def.type;
  if (typeof def?.typeName === 'string') return def.typeName.replace(/^Zod/, '').toLowerCase();
  return undefined;
}

/**
 * Return the raw check definitions of a schema in a version-neutral shape.
 * Zod v4 stores checks as `check._zod.def`; Zod v3 stored plain `check` objects.
 * @param {z.ZodSchema} schema - Zod schema
 * @returns {Array<Object>} Check definitions
 */
function checkDefs(schema) {
  const checks = schema?._def?.checks ?? schema?._zod?.def?.checks ?? [];
  return checks.map(check => check?._zod?.def ?? check);
}

/**
 * Extract constraints from Zod schema
 * @param {z.ZodSchema} schema - Zod schema
 * @returns {Array} Array of constraint objects
 */
function extractConstraints(schema) {
  const constraints = [];

  // Handle ZodObject
  if (schemaKind(schema) === 'object') {
    const rawShape = schema._def.shape;
    const shape = typeof rawShape === 'function' ? rawShape() : rawShape;

    for (const [fieldName, fieldSchema] of Object.entries(shape)) {
      constraints.push(...extractFieldConstraints(fieldName, fieldSchema));
    }
  }

  // Handle direct schema
  else {
    constraints.push(...extractFieldConstraints('value', schema));
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
  const kind = schemaKind(fieldSchema);

  // Optional wrapper: record it, then extract constraints of the wrapped schema
  if (kind === 'optional') {
    constraints.push({ field: fieldName, type: 'optional' });
    const inner = fieldSchema._def?.innerType ?? fieldSchema._zod?.def?.innerType;
    if (inner) constraints.push(...extractFieldConstraints(fieldName, inner));
    return constraints;
  }

  const checks = checkDefs(fieldSchema);

  // String constraints
  if (kind === 'string') {
    for (const check of checks) {
      const message = check.message ?? check.error;
      const format = check.format ?? check.kind;
      if (check.check === 'min_length' || check.kind === 'min') {
        constraints.push({ field: fieldName, type: 'minLength', value: check.minimum ?? check.value, message });
      } else if (check.check === 'max_length' || check.kind === 'max') {
        constraints.push({ field: fieldName, type: 'maxLength', value: check.maximum ?? check.value, message });
      } else if (format === 'email') {
        constraints.push({ field: fieldName, type: 'email', message });
      } else if (format === 'url') {
        constraints.push({ field: fieldName, type: 'url', message });
      } else if (format === 'regex') {
        const regex = check.pattern ?? check.regex;
        constraints.push({ field: fieldName, type: 'pattern', value: regex.source, message });
      }
    }
  }

  // Number constraints
  else if (kind === 'number') {
    for (const check of checks) {
      if (check.check === 'greater_than' || check.kind === 'min') {
        constraints.push({
          field: fieldName,
          type: 'min',
          value: check.value,
          inclusive: check.inclusive !== false,
        });
      } else if (check.check === 'less_than' || check.kind === 'max') {
        constraints.push({
          field: fieldName,
          type: 'max',
          value: check.value,
          inclusive: check.inclusive !== false,
        });
      } else if (
        (check.check === 'number_format' && /int/.test(check.format)) ||
        check.kind === 'int'
      ) {
        constraints.push({ field: fieldName, type: 'integer' });
      }
    }
  }

  // Array constraints
  else if (kind === 'array') {
    for (const check of checks) {
      if (check.check === 'min_length') {
        constraints.push({ field: fieldName, type: 'minItems', value: check.minimum });
      } else if (check.check === 'max_length') {
        constraints.push({ field: fieldName, type: 'maxItems', value: check.maximum });
      }
    }
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
