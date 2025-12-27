/**
 * Zod Schema Generator
 *
 * Generates Zod schemas from:
 * - TypeScript type definitions
 * - JSDoc @typedef comments
 * - Existing function signatures
 *
 * @module @unrdf/v6-compat/schema-generator
 */

import { z } from 'zod';

/**
 * TypeScript type to Zod schema mapping
 */
const TS_TO_ZOD = {
  string: 'z.string()',
  number: 'z.number()',
  boolean: 'z.boolean()',
  Date: 'z.date()',
  unknown: 'z.unknown()',
  any: 'z.any()',
  void: 'z.void()',
  null: 'z.null()',
  undefined: 'z.undefined()'
};

/**
 * Parse JSDoc typedef to Zod schema
 *
 * @param {string} jsdoc - JSDoc comment block
 * @returns {string} Zod schema code
 *
 * @example
 * const jsdoc = `
 *   @typedef {Object} User
 *   @property {string} id - User ID
 *   @property {string} name - User name
 *   @property {number} [age] - Optional age
 * `;
 *
 * parseJSDocToZod(jsdoc);
 * // Returns:
 * // z.object({
 * //   id: z.string(),
 * //   name: z.string(),
 * //   age: z.number().optional()
 * // })
 */
export function parseJSDocToZod(jsdoc) {
  const lines = jsdoc.split('\n').map((l) => l.trim());
  const properties = [];

  for (const line of lines) {
    // Match: @property {type} [name] - description
    const match = line.match(/@property\s+\{([^}]+)\}\s+(\[?)(\w+)\]?\s*-?\s*(.*)/);
    if (match) {
      const [, type, optional, name, description] = match;
      const zodType = TS_TO_ZOD[type] || `z.unknown() /* ${type} */`;
      const zodField = optional
        ? `${zodType}.optional() // ${description}`
        : `${zodType} // ${description}`;

      properties.push(`  ${name}: ${zodField}`);
    }
  }

  if (properties.length === 0) {
    return 'z.unknown()';
  }

  return `z.object({\n${properties.join(',\n')}\n})`;
}

/**
 * Generate schema from function signature
 *
 * @param {string} fnSource - Function source code
 * @returns {Object} Generated schemas
 *
 * @example
 * const fn = `
 *   function processUser(id: string, name: string): User {
 *     return { id, name };
 *   }
 * `;
 *
 * generateSchemaFromFunction(fn);
 * // Returns:
 * // {
 * //   params: z.tuple([z.string(), z.string()]),
 * //   returns: z.object({ id: z.string(), name: z.string() })
 * // }
 */
export function generateSchemaFromFunction(fnSource) {
  // Simple parser (production version should use actual TS parser)
  const paramMatch = fnSource.match(/\(([^)]+)\)/);
  const returnMatch = fnSource.match(/:\s*(\w+)/);

  const params = [];
  if (paramMatch) {
    const paramList = paramMatch[1].split(',').map((p) => p.trim());
    for (const param of paramList) {
      const [_name, type] = param.split(':').map((s) => s.trim());
      const zodType = TS_TO_ZOD[type] || 'z.unknown()';
      params.push(zodType);
    }
  }

  const returns = returnMatch
    ? TS_TO_ZOD[returnMatch[1]] || 'z.unknown()'
    : 'z.void()';

  return {
    params: params.length > 0 ? `z.tuple([${params.join(', ')}])` : 'z.tuple([])',
    returns
  };
}

/**
 * CLI: Generate schemas for all files in directory
 *
 * Usage:
 * node schema-generator.mjs src/**\/*.mjs
 *
 * Outputs: src/**\/*.schema.mjs
 */
export async function generateSchemasForFiles(filePatterns) {
  // Placeholder - production version should:
  // 1. Read all matching files
  // 2. Parse JSDoc comments
  // 3. Generate Zod schemas
  // 4. Write to .schema.mjs files

  console.log('Generating schemas for:', filePatterns);
  console.log('Not implemented yet - see V6-002 capsule');
}

/**
 * Example: User schema
 */
export const UserSchema = z.object({
  id: z.string().uuid(),
  name: z.string().min(1),
  email: z.string().email(),
  age: z.number().int().positive().optional()
});

/**
 * Example: Receipt schema
 */
export const ReceiptSchema = z.object({
  version: z.string(),
  operation: z.string(),
  timestamp: z.number().int().positive(),
  duration: z.number().nonnegative(),
  args: z.string(),
  result: z.string()
});

/**
 * Validate data against schema and return descriptive errors
 *
 * @param {z.ZodSchema} schema - Zod schema
 * @param {unknown} data - Data to validate
 * @returns {Object} Validation result
 *
 * @example
 * const result = validateWithErrors(UserSchema, { id: 123 });
 * if (!result.success) {
 *   console.error(result.errors);
 * }
 */
export function validateWithErrors(schema, data) {
  const result = schema.safeParse(data);

  if (!result.success) {
    const errors = result.error.errors.map((e) => ({
      path: e.path.join('.'),
      message: e.message,
      code: e.code
    }));

    return {
      success: false,
      errors,
      summary: errors.map((e) => `${e.path}: ${e.message}`).join('; ')
    };
  }

  return {
    success: true,
    data: result.data
  };
}

/**
 * Generate TypeScript type from Zod schema (reverse)
 *
 * @param {z.ZodSchema} schema - Zod schema
 * @returns {string} TypeScript type definition
 *
 * @example
 * generateTSFromZod(UserSchema);
 * // Returns:
 * // type User = {
 * //   id: string;
 * //   name: string;
 * //   email: string;
 * //   age?: number;
 * // }
 */
export function generateTSFromZod(_schema) {
  // Placeholder - production version should walk Zod schema AST
  return '// TypeScript generation not implemented yet';
}
