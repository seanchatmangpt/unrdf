import { z } from 'zod';

/**
 * @typedef {Object} FileLayoutRules
 * @property {string} src - Glob pattern for source files (e.g., "src/**\/*.mjs")
 * @property {string} test - Glob pattern for test files (e.g., "test/**\/*.test.mjs")
 * @property {string} [examples] - Glob pattern for example files
 * @property {string} [docs] - Glob pattern for documentation
 */

/**
 * @typedef {Object} NamingRules
 * @property {string} [filePrefix] - Required file name prefix (e.g., "api-")
 * @property {string} [functionPrefix] - Required function name prefix (e.g., "api")
 * @property {string} exportPattern - Regex pattern for export names
 * @property {string[]} [reservedWords] - Words that cannot be used in names
 */

/**
 * @typedef {Object} ErrorModelRules
 * @property {string} [namespace] - Error namespace/prefix (e.g., "ApiError")
 * @property {string[]} fields - Required error fields (e.g., ["code", "message", "context"])
 * @property {Object.<string, string>} codes - Error code mappings
 * @property {string} [baseClass] - Required base class name
 */

/**
 * @typedef {Object} LoggingRules
 * @property {string[]} fields - Required log fields (e.g., ["timestamp", "level", "message"])
 * @property {string[]} levels - Valid log levels (e.g., ["debug", "info", "warn", "error"])
 * @property {"json"|"text"} [format] - Required log format
 * @property {boolean} [requireContext] - Context field mandatory
 */

/**
 * @typedef {Object} ConventionProfile
 * @property {string} id - Unique profile identifier (e.g., "company-api-v1")
 * @property {FileLayoutRules} fileLayout - Directory structure rules
 * @property {NamingRules} naming - Naming convention rules
 * @property {ErrorModelRules} errors - Error class and code rules
 * @property {LoggingRules} logging - Logging field and level rules
 */

export const FileLayoutSchema = z.object({
  src: z.string().regex(/\*\*\/\*/, 'Must be a glob pattern'),
  test: z.string().regex(/\*\*\/\*/, 'Must be a glob pattern'),
  examples: z.string().optional(),
  docs: z.string().optional(),
});

export const NamingSchema = z.object({
  filePrefix: z.string().optional(),
  functionPrefix: z.string().optional(),
  exportPattern: z.string().regex(/^\/.*\/$/, 'Must be a regex pattern'),
  reservedWords: z.array(z.string()).optional(),
});

export const ErrorModelSchema = z.object({
  namespace: z.string().optional(),
  fields: z.array(z.string()).min(1, 'At least one field required'),
  codes: z.record(z.string(), z.string()),
  baseClass: z.string().optional(),
});

export const LoggingSchema = z.object({
  fields: z.array(z.string()).min(1, 'At least one field required'),
  levels: z.array(z.string()).min(1, 'At least one level required'),
  format: z.enum(['json', 'text']).optional(),
  requireContext: z.boolean().optional(),
});

export const ConventionProfileSchema = z.object({
  id: z.string().min(1, 'Profile ID required'),
  fileLayout: FileLayoutSchema,
  naming: NamingSchema,
  errors: ErrorModelSchema,
  logging: LoggingSchema,
});
