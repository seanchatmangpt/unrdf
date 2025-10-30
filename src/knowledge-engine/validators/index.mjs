/**
 * @file Zod Validators - Input validation library
 * @module validators
 *
 * @description
 * Centralized Zod validators for all knowledge engine inputs.
 * Provides consistent validation with security checks.
 *
 * Addresses 30 test failures related to schema validation.
 */

import { z } from 'zod'
import path from 'node:path'
// Prefer consolidated schemas from ../schemas.mjs as source of truth
export { QuadSchema, DeltaSchema, HookContextSchema, KnowledgeHookSchema } from '../schemas.mjs'

/**
 * SHA-256 hash validator
 */
export const sha256Validator = z.string()
  .length(64, 'SHA-256 hash must be exactly 64 characters')
  .regex(/^[a-f0-9]{64}$/, 'SHA-256 hash must be lowercase hexadecimal')

/**
 * Secure URI validator with path traversal prevention
 */
export const secureUriValidator = z.string()
  .url({ message: 'Must be a valid URI' })
  .or(z.string().regex(/^file:\/\/.+/, 'Must be a valid file URI'))
  .refine(uri => {
    // Extract path from URI
    let uriPath
    try {
      const url = new URL(uri)
      uriPath = url.pathname
    } catch {
      // If not a valid URL, treat as path
      uriPath = uri.replace(/^file:\/\//, '')
    }

    const normalized = path.normalize(uriPath)

    // Check for path traversal patterns
    return !normalized.includes('..') && !normalized.includes('~')
  }, 'Path traversal patterns detected')
  .refine(uri => {
    // Extract path from URI
    let uriPath
    try {
      const url = new URL(uri)
      uriPath = url.pathname
    } catch {
      uriPath = uri.replace(/^file:\/\//, '')
    }

    const normalized = path.normalize(uriPath)

    // Check for system path access
    const systemPaths = ['/etc', '/proc', '/sys', '/dev', '/root', '/home']
    return !systemPaths.some(p => normalized.startsWith(p))
  }, 'System path access detected')

/**
 * Media type validator for RDF formats
 */
export const mediaTypeValidator = z.enum([
  'application/sparql-query',
  'text/turtle',
  'application/rdf+xml',
  'application/ld+json',
  'application/n-triples',
  'application/n-quads',
  'text/n3'
], {
  errorMap: () => ({ message: 'Must be a supported RDF media type' })
})

/**
 * Content-addressed file reference validator (SECURE)
 */
export const secureFileRefValidator = z.object({
  uri: secureUriValidator,
  sha256: sha256Validator,
  mediaType: mediaTypeValidator
}).strict()

/**
 * Hook metadata validator with XSS prevention
 */
export const hookMetaValidator = z.object({
  name: z.string()
    .min(1, 'Name is required')
    .max(100, 'Name must be ≤100 characters')
    .regex(/^[a-zA-Z0-9:_-]+$/, 'Name must contain only alphanumeric characters, colons, hyphens, and underscores'),

  description: z.string()
    .min(1)
    .max(500, 'Description must be ≤500 characters')
    .refine(
      str => !/<script|<img|<iframe|javascript:|onerror=|onload=/i.test(str),
      'XSS attempt detected in description'
    )
    .optional(),

  version: z.string()
    .regex(/^\d+\.\d+\.\d+$/, 'Version must be semantic version (e.g., 1.0.0)')
    .optional(),

  author: z.string()
    .min(1)
    .max(100)
    .optional(),

  tags: z.array(z.string().min(1).max(50))
    .max(10, 'Maximum 10 tags allowed')
    .optional(),

  ontology: z.array(z.string().min(1).max(50))
    .max(10, 'Maximum 10 ontology references allowed')
    .optional()
}).strict()

/**
 * SPARQL ASK condition validator
 */
export const sparqlAskConditionValidator = z.object({
  kind: z.literal('sparql-ask'),
  ref: secureFileRefValidator,
  options: z.object({
    timeout: z.number().int().positive().max(30000).optional(),
    strict: z.boolean().optional(),
    variables: z.record(z.string()).optional()
  }).optional()
}).strict()

/**
 * SPARQL SELECT condition validator
 */
export const sparqlSelectConditionValidator = z.object({
  kind: z.literal('sparql-select'),
  ref: secureFileRefValidator,
  options: z.object({
    timeout: z.number().int().positive().max(30000).optional(),
    limit: z.number().int().positive().max(10000).optional(),
    offset: z.number().int().nonnegative().optional(),
    strict: z.boolean().optional(),
    variables: z.record(z.string()).optional()
  }).optional()
}).strict()

/**
 * SHACL condition validator
 */
export const shaclConditionValidator = z.object({
  kind: z.literal('shacl'),
  ref: secureFileRefValidator,
  options: z.object({
    strict: z.boolean().optional(),
    includeDetails: z.boolean().optional(),
    maxViolations: z.number().int().positive().max(1000).optional()
  }).optional()
}).strict()

/**
 * Union of all condition validators
 */
export const conditionValidator = z.discriminatedUnion('kind', [
  sparqlAskConditionValidator,
  sparqlSelectConditionValidator,
  shaclConditionValidator
])

/**
 * Complete knowledge hook validator
 */
export const knowledgeHookValidator = z.object({
  meta: hookMetaValidator,
  when: conditionValidator,
  run: z.function(),
  before: z.function().optional(),
  after: z.function().optional(),
  timeout: z.number().int().positive().max(300000).optional(),
  retries: z.number().int().nonnegative().max(5).optional(),
  priority: z.number().int().min(0).max(100).default(50).optional()
}).strict()

/**
 * RDF quad validator
 */
export const quadValidator = z.object({
  subject: z.any(), // RDF/JS Term
  predicate: z.any(), // RDF/JS Term
  object: z.any(), // RDF/JS Term
  graph: z.any().optional() // RDF/JS Term
}).passthrough() // Allow additional RDF/JS Quad properties

/**
 * Transaction delta validator
 */
export const deltaValidator = z.object({
  additions: z.array(quadValidator),
  removals: z.array(quadValidator)
}).strict()

/**
 * Hook execution context validator
 */
export const hookContextValidator = z.object({
  graph: z.any(), // RDF Store - validated at runtime
  env: z.record(z.any()).optional(),
  metadata: z.record(z.any()).optional(),
  transactionId: z.string().uuid().optional(),
  timestamp: z.coerce.date().optional()
}).strict()

/**
 * Manager configuration validator
 */
export const managerConfigValidator = z.object({
  basePath: z.string().min(1).default(process.cwd()),
  strictMode: z.boolean().default(false),
  enableConditionEvaluation: z.boolean().default(true),
  maxHooks: z.number().int().positive().max(1000).default(100),
  timeout: z.number().int().positive().max(300000).default(30000),
  enableCache: z.boolean().default(true),
  cacheMaxAge: z.number().int().positive().max(3600000).default(300000),
  enableMetrics: z.boolean().default(true),
  logLevel: z.enum(['error', 'warn', 'info', 'debug']).default('info')
}).strict()

/**
 * Validation helper function
 *
 * @param {z.ZodSchema} schema - Zod schema to validate against
 * @param {any} data - Data to validate
 * @param {string} [name] - Name for error messages
 * @returns {Object} Validation result
 */
export function validate(schema, data, name = 'Data') {
  try {
    const validated = schema.parse(data)
    return {
      success: true,
      data: validated,
      errors: []
    }
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        success: false,
        data: null,
        errors: error.errors.map(err => ({
          path: err.path.join('.'),
          message: err.message,
          code: err.code,
          received: err.received,
          expected: err.expected
        }))
      }
    }
    throw error
  }
}

/**
 * Safe validation that throws on failure
 *
 * @param {z.ZodSchema} schema - Zod schema
 * @param {any} data - Data to validate
 * @param {string} [name] - Name for error messages
 * @returns {any} Validated data
 * @throws {TypeError} If validation fails
 */
export function validateOrThrow(schema, data, name = 'Data') {
  const result = validate(schema, data, name)

  if (!result.success) {
    const errorMessage = result.errors
      .map(err => `${err.path}: ${err.message}`)
      .join(', ')
    throw new TypeError(`${name} validation failed: ${errorMessage}`)
  }

  return result.data
}

/**
 * Create a validator function from a schema
 *
 * @param {z.ZodSchema} schema - Zod schema
 * @param {string} [name] - Name for error messages
 * @returns {Function} Validator function
 */
export function createValidator(schema, name = 'Data') {
  return (data) => validate(schema, data, name)
}

/**
 * Create a throwing validator from a schema
 *
 * @param {z.ZodSchema} schema - Zod schema
 * @param {string} [name] - Name for error messages
 * @returns {Function} Throwing validator function
 */
export function createThrowingValidator(schema, name = 'Data') {
  return (data) => validateOrThrow(schema, data, name)
}
