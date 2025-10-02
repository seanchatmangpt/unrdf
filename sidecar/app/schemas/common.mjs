/**
 * Common validation schemas used across the Knowledge Hooks system
 * @module schemas/common
 */

import { z } from 'zod'

/**
 * SHA-256 hash schema
 * Validates 64-character hexadecimal strings
 * @example "a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a"
 */
export const Sha256Schema = z.string().regex(/^[a-f0-9]{64}$/, {
  message: 'Must be a valid SHA-256 hash (64 hexadecimal characters)'
})

/**
 * ISO 8601 timestamp schema
 * Validates date-time strings in ISO 8601 format
 * @example "2025-10-01T12:34:56.789Z"
 */
export const TimestampSchema = z.string().datetime({
  message: 'Must be a valid ISO 8601 datetime string'
})

/**
 * Hook phase enumeration
 * Defines the execution phase of a knowledge hook
 * - pre: Execute before an operation
 * - post: Execute after an operation
 * - invariant: Execute as a continuous validation
 */
export const HookPhaseSchema = z.enum(['pre', 'post', 'invariant'], {
  errorMap: () => ({ message: 'Hook phase must be one of: pre, post, invariant' })
})

/**
 * Hook ID schema
 * Validates lowercase alphanumeric strings with hyphens
 * @example "validate-budget-constraints"
 */
export const HookIdSchema = z.string().regex(/^[a-z0-9-]+$/, {
  message: 'Hook ID must contain only lowercase letters, numbers, and hyphens'
}).min(1).max(100)

/**
 * URI schema for RDF resources
 * Validates absolute URIs
 * @example "https://example.org/resource/123"
 */
export const UriSchema = z.string().url({
  message: 'Must be a valid absolute URI'
})

/**
 * SPARQL query schema
 * Validates non-empty SPARQL query strings
 */
export const SparqlQuerySchema = z.string().min(1, {
  message: 'SPARQL query cannot be empty'
})

/**
 * Policy pack ID schema
 * Validates policy pack identifiers
 * @example "financial-compliance-pack"
 */
export const PolicyPackIdSchema = z.string().regex(/^[a-z0-9-]+$/, {
  message: 'Policy pack ID must contain only lowercase letters, numbers, and hyphens'
}).min(1).max(100)

/**
 * Version schema for semantic versioning
 * @example "1.0.0"
 */
export const VersionSchema = z.string().regex(/^\d+\.\d+\.\d+$/, {
  message: 'Version must follow semantic versioning (e.g., 1.0.0)'
})

/**
 * Error code schema
 * Validates uppercase error codes with underscores
 * @example "VALIDATION_FAILED"
 */
export const ErrorCodeSchema = z.string().regex(/^[A-Z_]+$/, {
  message: 'Error code must contain only uppercase letters and underscores'
})

/**
 * Severity level schema
 * Defines the severity of validation results or errors
 */
export const SeveritySchema = z.enum(['info', 'warning', 'error', 'critical'], {
  errorMap: () => ({ message: 'Severity must be one of: info, warning, error, critical' })
})
