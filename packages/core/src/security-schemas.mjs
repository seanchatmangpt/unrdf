/**
 * @fileoverview Zod security validation schemas for UNRDF
 * Common validation patterns for secure input handling
 * @module security-schemas
 */

import { z } from 'zod';

/**
 * Safe string schema - prevents common injection attacks
 */
export const SafeStringSchema = z.string()
  .min(1)
  .max(1000)
  .refine(
    (val) => !val.includes('\0'),
    { message: 'String contains null byte' }
  )
  .refine(
    (val) => !/[<>]/.test(val),
    { message: 'String contains HTML characters' }
  );

/**
 * Email schema with strict validation
 */
export const EmailSchema = z.string()
  .email()
  .max(254) // RFC 5321
  .toLowerCase()
  .refine(
    (val) => !val.includes('..'),
    { message: 'Invalid email format' }
  );

/**
 * URL schema with protocol whitelist
 */
export const URLSchema = z.string()
  .url()
  .refine(
    (val) => {
      try {
        const url = new URL(val);
        return ['http:', 'https:'].includes(url.protocol);
      } catch {
        return false;
      }
    },
    { message: 'URL must use HTTP or HTTPS protocol' }
  );

/**
 * SPARQL query schema - basic validation
 */
export const SPARQLQuerySchema = z.string()
  .min(1)
  .max(10000)
  .refine(
    (val) => {
      // Must contain SELECT, CONSTRUCT, ASK, or DESCRIBE
      return /\b(SELECT|CONSTRUCT|ASK|DESCRIBE)\b/i.test(val);
    },
    { message: 'Invalid SPARQL query format' }
  )
  .refine(
    (val) => {
      // Check for potentially dangerous operations
      const dangerous = [
        /\bLOAD\b/i,
        /\bCLEAR\b/i,
        /\bDROP\b/i,
        /\bCREATE\b/i,
        /\bINSERT\b/i,
        /\bDELETE\b/i
      ];
      return !dangerous.some(pattern => pattern.test(val));
    },
    { message: 'SPARQL query contains disallowed operations' }
  );

/**
 * RDF namespace/prefix schema
 */
export const NamespaceSchema = z.string()
  .min(1)
  .max(100)
  .regex(/^[a-zA-Z][a-zA-Z0-9_-]*$/, 'Invalid namespace format');

/**
 * File path schema - prevents directory traversal
 */
export const SafeFilePathSchema = z.string()
  .min(1)
  .max(255)
  .refine(
    (val) => !val.includes('..'),
    { message: 'Path contains directory traversal' }
  )
  .refine(
    (val) => !val.includes('\0'),
    { message: 'Path contains null byte' }
  )
  .refine(
    (val) => !/^[~/]/.test(val),
    { message: 'Absolute paths not allowed' }
  );

/**
 * Pagination parameters schema
 */
export const PaginationSchema = z.object({
  page: z.number().int().positive().max(10000).default(1),
  limit: z.number().int().positive().max(100).default(20),
  offset: z.number().int().nonnegative().max(1000000).optional()
});

/**
 * API key schema
 */
export const APIKeySchema = z.string()
  .length(64)
  .regex(/^[a-zA-Z0-9_-]+$/, 'Invalid API key format');

/**
 * JWT token schema (basic format validation)
 */
export const JWTSchema = z.string()
  .regex(/^[A-Za-z0-9-_]+\.[A-Za-z0-9-_]+\.[A-Za-z0-9-_]+$/, 'Invalid JWT format');

/**
 * Username schema - alphanumeric with limited special chars
 */
export const UsernameSchema = z.string()
  .min(3)
  .max(32)
  .regex(/^[a-zA-Z0-9_-]+$/, 'Username can only contain letters, numbers, underscores, and hyphens');

/**
 * Password schema - enforce strong passwords
 */
export const PasswordSchema = z.string()
  .min(12)
  .max(128)
  .refine(
    (val) => /[a-z]/.test(val),
    { message: 'Password must contain at least one lowercase letter' }
  )
  .refine(
    (val) => /[A-Z]/.test(val),
    { message: 'Password must contain at least one uppercase letter' }
  )
  .refine(
    (val) => /[0-9]/.test(val),
    { message: 'Password must contain at least one number' }
  )
  .refine(
    (val) => /[!@#$%^&*()_+\-=\[\]{};':"\\|,.<>\/?]/.test(val),
    { message: 'Password must contain at least one special character' }
  );

/**
 * IP address schema (IPv4 and IPv6)
 */
export const IPAddressSchema = z.string()
  .refine(
    (val) => {
      // IPv4
      const ipv4 = /^(\d{1,3}\.){3}\d{1,3}$/;
      // IPv6
      const ipv6 = /^([0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}$/;
      return ipv4.test(val) || ipv6.test(val);
    },
    { message: 'Invalid IP address' }
  );

/**
 * Port number schema
 */
export const PortSchema = z.number()
  .int()
  .positive()
  .min(1)
  .max(65535);

/**
 * UUID schema
 */
export const UUIDSchema = z.string()
  .uuid();

/**
 * Timestamp schema (ISO 8601)
 */
export const TimestampSchema = z.string()
  .datetime();

/**
 * Content-Type schema
 */
export const ContentTypeSchema = z.enum([
  'application/json',
  'application/ld+json',
  'application/n-triples',
  'application/n-quads',
  'text/turtle',
  'application/rdf+xml',
  'application/trig',
  'text/plain'
]);

/**
 * HTTP method schema
 */
export const HTTPMethodSchema = z.enum([
  'GET',
  'POST',
  'PUT',
  'PATCH',
  'DELETE',
  'HEAD',
  'OPTIONS'
]);

/**
 * RDF triple schema
 */
export const RDFTripleSchema = z.object({
  subject: z.string().min(1),
  predicate: z.string().min(1),
  object: z.string().min(1)
});

/**
 * RDF quad schema
 */
export const RDFQuadSchema = RDFTripleSchema.extend({
  graph: z.string().optional()
});

/**
 * Environment variable schema
 */
export const EnvSchema = z.object({
  NODE_ENV: z.enum(['development', 'production', 'test']).default('development'),
  PORT: PortSchema.optional(),
  LOG_LEVEL: z.enum(['debug', 'info', 'warn', 'error']).default('info'),
  API_KEY: APIKeySchema.optional(),
  DATABASE_URL: URLSchema.optional()
});

/**
 * Sanitized HTML schema
 */
export const SanitizedHTMLSchema = z.string()
  .refine(
    (val) => !/<script/i.test(val),
    { message: 'HTML contains script tags' }
  )
  .refine(
    (val) => !/(onerror|onload|onclick)/i.test(val),
    { message: 'HTML contains event handlers' }
  );

/**
 * File upload schema
 */
export const FileUploadSchema = z.object({
  filename: SafeFilePathSchema,
  mimetype: z.string().max(100),
  size: z.number().int().positive().max(10 * 1024 * 1024), // 10MB max
  encoding: z.string().max(20).optional()
});

/**
 * Rate limit configuration schema
 */
export const RateLimitConfigSchema = z.object({
  maxRequests: z.number().int().positive().max(10000),
  windowMs: z.number().int().positive().max(3600000), // 1 hour max
  message: z.string().max(200).optional()
});

/**
 * CORS configuration schema
 */
export const CORSConfigSchema = z.object({
  origin: z.union([z.string(), z.array(z.string()), z.boolean()]),
  methods: z.array(HTTPMethodSchema).optional(),
  allowedHeaders: z.array(z.string()).optional(),
  exposedHeaders: z.array(z.string()).optional(),
  credentials: z.boolean().optional(),
  maxAge: z.number().int().positive().optional()
});

/**
 * Validate and sanitize user input
 * @param {unknown} data - Data to validate
 * @param {z.ZodSchema} schema - Zod schema to use
 * @returns {Object} Validation result
 * @example
 * const result = validateInput(userInput, EmailSchema);
 * if (!result.success) throw new Error(result.error);
 */
export function validateInput(data, schema) {
  return schema.safeParse(data);
}

/**
 * Validate multiple fields at once
 * @param {Object} data - Data object to validate
 * @param {Object} schemas - Schema for each field
 * @returns {Object} Validation results
 * @example
 * const results = validateFields(
 *   { email: 'test@example.com', username: 'user123' },
 *   { email: EmailSchema, username: UsernameSchema }
 * );
 */
export function validateFields(data, schemas) {
  const results = {};
  const errors = {};

  for (const [field, schema] of Object.entries(schemas)) {
    const result = schema.safeParse(data[field]);
    results[field] = result.success;

    if (!result.success) {
      errors[field] = result.error.errors.map(e => e.message);
    }
  }

  return {
    valid: Object.values(results).every(Boolean),
    results,
    errors: Object.keys(errors).length > 0 ? errors : null
  };
}
