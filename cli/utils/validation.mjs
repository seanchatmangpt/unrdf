/**
 * @file CLI input validation helpers using Zod
 * @module cli/utils/validation
 *
 * Provides Zod-based validation schemas for CLI commands
 * Implements poka-yoke guards to catch invalid inputs early
 */

import { z } from 'zod';

/**
 * Custom error formatter for CLI-friendly messages
 */
function formatZodError(error) {
  return error.errors
    .map(err => {
      const path = err.path.join('.');
      return `${path || 'argument'}: ${err.message}`;
    })
    .join('\n');
}

/**
 * Safely validate and return errors instead of throwing
 */
export function safeValidate(schema, data) {
  const result = schema.safeParse(data);
  if (!result.success) {
    return {
      valid: false,
      errors: result.error.errors.map(e => ({
        path: e.path.join('.'),
        message: e.message
      })),
      formatted: formatZodError(result.error)
    };
  }
  return {
    valid: true,
    data: result.data,
    errors: []
  };
}

/**
 * Throw on validation error with formatted message
 */
export function validate(schema, data, context = '') {
  const result = safeValidate(schema, data);
  if (!result.valid) {
    const contextStr = context ? `${context}: ` : '';
    throw new Error(`${contextStr}${result.formatted}`);
  }
  return result.data;
}

// ============================================================================
// Command-Specific Schemas
// ============================================================================

/**
 * store/import validation schema
 */
export const storeImportSchema = z.object({
  file: z.string()
    .min(1, 'file path required')
    .refine(
      (path) => !path.includes('..'),
      'file path cannot contain ".." (security)'
    ),
  graph: z.string().optional(),
  format: z.enum(['auto', 'turtle', 'ntriples', 'nquads', 'jsonld']).optional(),
  'format-override': z.boolean().optional(),
  force: z.boolean().optional(),
  validate: z.boolean().optional(),
  quiet: z.boolean().optional()
});

/**
 * store/query validation schema
 */
export const storeQuerySchema = z.object({
  query: z.string()
    .min(1, 'SPARQL query required')
    .refine(
      (q) => q.trim().length > 0,
      'query cannot be empty'
    ),
  format: z.enum(['json', 'table', 'csv', 'xml', 'sparql']).optional(),
  timeout: z.number().min(100).max(30000).optional(),
  limit: z.number().min(1).max(10000).optional(),
  graph: z.string().optional(),
  validate: z.boolean().optional()
});

/**
 * hook/create validation schema
 */
export const hookCreateSchema = z.object({
  name: z.string()
    .min(1, 'hook name required')
    .max(255, 'hook name too long')
    .regex(/^[a-z0-9-]+$/, 'hook name must be lowercase alphanumeric with hyphens'),
  type: z.enum(
    ['sparql-ask', 'sparql-select', 'shacl', 'custom'],
    'hook type must be one of: sparql-ask, sparql-select, shacl, custom'
  ),
  file: z.string().optional(),
  description: z.string().optional(),
  enabled: z.boolean().optional()
});

/**
 * graph/delete validation schema
 */
export const graphDeleteSchema = z.object({
  name: z.string()
    .min(1, 'graph name required')
    .max(255),
  force: z.boolean().optional(),
  'confirm': z.boolean().optional()
});

/**
 * hook/delete validation schema
 */
export const hookDeleteSchema = z.object({
  name: z.string()
    .min(1, 'hook name required')
    .max(255),
  force: z.boolean().optional(),
  'confirm': z.boolean().optional()
});

/**
 * context/delete validation schema
 */
export const contextDeleteSchema = z.object({
  name: z.string()
    .min(1, 'context name required')
    .max(255),
  force: z.boolean().optional(),
  'confirm': z.boolean().optional()
});

/**
 * policy/apply validation schema
 */
export const policyApplySchema = z.object({
  file: z.string()
    .min(1, 'policy file required')
    .refine(
      (path) => path.endsWith('.json') || path.endsWith('.yaml') || path.endsWith('.yml'),
      'policy file must be .json, .yaml, or .yml'
    ),
  'dry-run': z.boolean().optional(),
  force: z.boolean().optional(),
  'confirm': z.boolean().optional(),
  verbose: z.boolean().optional()
});

/**
 * context/use validation schema
 */
export const contextUseSchema = z.object({
  name: z.string()
    .min(1, 'context name required')
    .max(255)
});

/**
 * context/create validation schema
 */
export const contextCreateSchema = z.object({
  name: z.string()
    .min(1, 'context name required')
    .max(255)
    .regex(/^[a-z0-9-]+$/, 'context name must be lowercase alphanumeric'),
  endpoint: z.string().url().optional(),
  description: z.string().optional()
});

/**
 * Validate file exists (async)
 */
export async function validateFileExists(filePath) {
  try {
    const { access } = await import('node:fs/promises');
    await access(filePath);
    return { valid: true };
  } catch (error) {
    return {
      valid: false,
      error: `File not found: ${filePath}`,
      suggestion: `Check that the file exists and is readable. Current directory: ${process.cwd()}`
    };
  }
}

/**
 * Validate file is readable text file
 */
export async function validateFileContent(filePath, expectedFormat = null) {
  try {
    const { readFile } = await import('node:fs/promises');
    const content = await readFile(filePath, 'utf-8');

    if (!content || content.trim().length === 0) {
      return {
        valid: false,
        error: 'File is empty',
        suggestion: 'Add content to the file before importing'
      };
    }

    // Basic format detection
    if (expectedFormat) {
      const firstLine = content.split('\n')[0].toLowerCase();
      const detectedFormat = detectRdfFormat(firstLine, filePath);

      if (detectedFormat && detectedFormat !== expectedFormat) {
        return {
          valid: false,
          error: `Format mismatch: file appears to be ${detectedFormat}, expected ${expectedFormat}`,
          suggestion: `Use --format-override to force import as ${expectedFormat}, or re-save file as ${detectedFormat}`
        };
      }
    }

    return { valid: true, content };
  } catch (error) {
    return {
      valid: false,
      error: `Cannot read file: ${error.message}`,
      suggestion: 'Check file permissions and try again'
    };
  }
}

/**
 * Detect RDF format from file content
 */
function detectRdfFormat(firstLine, filePath) {
  // By file extension
  if (filePath.endsWith('.ttl')) return 'turtle';
  if (filePath.endsWith('.nt')) return 'ntriples';
  if (filePath.endsWith('.nq')) return 'nquads';
  if (filePath.endsWith('.jsonld') || filePath.endsWith('.json')) return 'jsonld';
  if (filePath.endsWith('.rdf') || filePath.endsWith('.xml')) return 'rdfxml';

  // By content
  if (firstLine.includes('@prefix') || firstLine.includes('@base')) return 'turtle';
  if (firstLine.startsWith('<')) return 'rdfxml';
  if (firstLine.includes('http') && firstLine.includes('>')) return 'ntriples';
  if (firstLine.includes('{') || firstLine.includes('[')) return 'jsonld';

  return null;
}

/**
 * Validate SPARQL query syntax (basic checks)
 */
export function validateSparqlQuery(query) {
  const errors = [];
  const warnings = [];

  if (!query || query.trim().length === 0) {
    return {
      valid: false,
      errors: ['Query cannot be empty'],
      warnings
    };
  }

  const upperQuery = query.toUpperCase().trim();

  // Check for known query types
  const isSelect = upperQuery.startsWith('SELECT');
  const isAsk = upperQuery.startsWith('ASK');
  const isConstruct = upperQuery.startsWith('CONSTRUCT');
  const isDescribe = upperQuery.startsWith('DESCRIBE');
  const isInsert = upperQuery.startsWith('INSERT');
  const isDelete = upperQuery.startsWith('DELETE');

  if (!isSelect && !isAsk && !isConstruct && !isDescribe && !isInsert && !isDelete) {
    errors.push('Query must start with SELECT, ASK, CONSTRUCT, DESCRIBE, INSERT, or DELETE');
  }

  // Check for unclosed braces
  const openBraces = (query.match(/{/g) || []).length;
  const closeBraces = (query.match(/}/g) || []).length;
  if (openBraces !== closeBraces) {
    errors.push(`Mismatched braces: ${openBraces} open, ${closeBraces} close`);
  }

  // Check for WHERE clause (except for INSERT/DELETE)
  if ((isSelect || isAsk || isConstruct || isDescribe) && !upperQuery.includes('WHERE')) {
    warnings.push('Query may be missing WHERE clause');
  }

  // Check for common typos
  if (upperQuery.includes('SELCT')) {
    errors.push('Possible typo: "SELCT" should be "SELECT"');
  }
  if (upperQuery.includes('PREFEX')) {
    errors.push('Possible typo: "PREFEX" should be "PREFIX"');
  }

  return {
    valid: errors.length === 0,
    errors,
    warnings,
    suggestions: errors.length > 0 ? [
      'Check SPARQL syntax: https://www.w3.org/TR/sparql11-query/',
      'Use --format sparql to validate syntax'
    ] : []
  };
}

export default {
  validate,
  safeValidate,
  validateFileExists,
  validateFileContent,
  validateSparqlQuery,
  storeImportSchema,
  storeQuerySchema,
  hookCreateSchema,
  graphDeleteSchema,
  hookDeleteSchema,
  contextDeleteSchema,
  policyApplySchema,
  contextUseSchema,
  contextCreateSchema
};
