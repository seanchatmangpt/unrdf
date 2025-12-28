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
 * **P0-002 Implementation**
 *
 * Usage:
 * node schema-generator.mjs src/**\/*.mjs
 *
 * Outputs: src/**\/*.schema.mjs
 *
 * @param {string|Array<string>} filePatterns - Glob patterns for files
 * @param {Object} options - Generation options
 * @param {string} [options.outputSuffix='.schema.mjs'] - Output file suffix
 * @param {boolean} [options.dryRun=false] - Don't write files, just return schemas
 * @returns {Promise<Array<{file: string, schema: string, functions: Array}>>}
 */
export async function generateSchemasForFiles(filePatterns, options = {}) {
  const {
    outputSuffix = '.schema.mjs',
    dryRun = false,
  } = options;

  // Ensure filePatterns is an array
  const patterns = Array.isArray(filePatterns) ? filePatterns : [filePatterns];

  const results = [];

  // Import dynamic modules
  const { glob } = await import('glob');
  const { readFile, writeFile } = await import('fs/promises');
  const { basename, dirname, join } = await import('path');

  for (const pattern of patterns) {
    const files = await glob(pattern, { absolute: true });

    for (const file of files) {
      try {
        const source = await readFile(file, 'utf-8');
        const functions = extractFunctionsFromSource(source);

        if (functions.length === 0) {
          continue; // Skip files with no exported functions
        }

        // Generate schema module
        const schemaCode = generateSchemaModule(functions, file);

        // Determine output path
        const dir = dirname(file);
        const base = basename(file, '.mjs');
        const outputPath = join(dir, `${base}${outputSuffix}`);

        if (!dryRun) {
          await writeFile(outputPath, schemaCode, 'utf-8');
        }

        results.push({
          file,
          outputPath,
          schema: schemaCode,
          functions,
        });
      } catch (error) {
        console.warn(`Failed to process ${file}:`, error.message);
      }
    }
  }

  return results;
}

/**
 * Extract exported functions from source code
 *
 * @param {string} source - Source code
 * @returns {Array<{name: string, jsdoc: string, params: Array, returnType: string}>}
 */
function extractFunctionsFromSource(source) {
  const functions = [];
  const lines = source.split('\n');

  let jsdocBuffer = [];
  let inJSDoc = false;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    // Detect JSDoc start
    if (line.trim().startsWith('/**')) {
      inJSDoc = true;
      jsdocBuffer = [line];
      continue;
    }

    // Collect JSDoc lines
    if (inJSDoc) {
      jsdocBuffer.push(line);
      if (line.trim().includes('*/')) {
        inJSDoc = false;
      }
      continue;
    }

    // Detect exported function
    const exportMatch = line.match(/^export\s+(async\s+)?function\s+(\w+)\s*\(([^)]*)\)/);
    if (exportMatch && jsdocBuffer.length > 0) {
      const [, isAsync, name, paramsStr] = exportMatch;

      // Parse params
      const params = paramsStr
        .split(',')
        .map((p) => p.trim())
        .filter(Boolean)
        .map((p) => {
          const [paramName, defaultValue] = p.split('=').map((s) => s.trim());
          return { name: paramName, optional: !!defaultValue };
        });

      // Extract return type from JSDoc
      const jsdoc = jsdocBuffer.join('\n');
      const returnMatch = jsdoc.match(/@returns?\s+\{([^}]+)\}/);
      const returnType = returnMatch ? returnMatch[1] : 'unknown';

      // Extract param types from JSDoc
      const paramTypes = extractParamTypesFromJSDoc(jsdoc);

      functions.push({
        name,
        jsdoc,
        params: params.map((p) => ({
          ...p,
          type: paramTypes[p.name] || 'unknown',
        })),
        returnType,
        isAsync: !!isAsync,
      });

      jsdocBuffer = [];
    } else if (exportMatch) {
      jsdocBuffer = [];
    }
  }

  return functions;
}

/**
 * Extract parameter types from JSDoc
 *
 * @param {string} jsdoc - JSDoc comment
 * @returns {Object} Map of param name → type
 */
function extractParamTypesFromJSDoc(jsdoc) {
  const paramTypes = {};
  const paramMatches = jsdoc.matchAll(/@param\s+\{([^}]+)\}\s+(\w+)/g);

  for (const match of paramMatches) {
    const [, type, name] = match;
    paramTypes[name] = type;
  }

  return paramTypes;
}

/**
 * Generate schema module code
 *
 * @param {Array} functions - Function metadata
 * @param {string} sourceFile - Source file path
 * @returns {string} Generated schema module
 */
function generateSchemaModule(functions, sourceFile) {
  const schemas = functions.map((fn) => {
    const paramsSchema = fn.params.map((p) => {
      const zodType = TS_TO_ZOD[p.type] || 'z.unknown()';
      return p.optional ? `${zodType}.optional()` : zodType;
    });

    const returnZodType = TS_TO_ZOD[fn.returnType] || 'z.unknown()';

    return `
/**
 * Schema for ${fn.name}
 */
export const ${fn.name}ParamsSchema = z.tuple([${paramsSchema.join(', ')}]);

export const ${fn.name}ReturnSchema = ${returnZodType};

export const ${fn.name}Schema = {
  params: ${fn.name}ParamsSchema,
  returns: ${fn.name}ReturnSchema,
};
`.trim();
  });

  // Extract basename for header
  const fileName = sourceFile.split('/').pop();

  return `/**
 * Auto-generated Zod schemas for ${fileName}
 *
 * Generated by @unrdf/v6-compat/schema-generator
 *
 * DO NOT EDIT MANUALLY
 */

import { z } from 'zod';

${schemas.join('\n\n')}

export default {
  ${functions.map((fn) => `${fn.name}: ${fn.name}Schema`).join(',\n  ')}
};
`;
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
    // In Zod v4, errors are in .issues instead of .errors
    const errorList = result.error?.issues || result.error?.errors || [];
    const errors = errorList.map((e) => ({
      path: Array.isArray(e.path) ? e.path.join('.') : String(e.path || ''),
      message: e.message,
      code: e.code
    }));

    return {
      success: false,
      errors,
      summary: errors.length > 0
        ? errors.map((e) => `${e.path}: ${e.message}`).join('; ')
        : 'Validation failed'
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
