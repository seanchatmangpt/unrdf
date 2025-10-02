/**
 * @file Zod Validation Schemas
 * @description Runtime validation schemas for API requests
 */

import { z } from 'zod'

/**
 * Hook predicate schema
 */
export const hookPredicateSchema = z.object({
  kind: z.enum(['ASK', 'SHACL', 'DELTA', 'THRESHOLD', 'COUNT', 'WINDOW']),
  query: z.string().optional(),
  shapes: z.any().optional(),
  variable: z.string().optional(),
  operator: z.enum(['>', '<', '=', '>=', '<=']).optional(),
  value: z.number().optional(),
  windowSize: z.number().int().positive().optional(),
  countVariable: z.string().optional()
})

/**
 * Register hook request schema
 */
export const registerHookSchema = z.object({
  id: z.string().min(1, 'Hook ID is required'),
  select: z.string().min(1, 'SELECT query is required'),
  predicates: z.array(hookPredicateSchema).min(1, 'At least one predicate required'),
  combine: z.enum(['AND', 'OR']),
  phase: z.enum(['pre', 'post'])
})

/**
 * RDF quad schema
 */
export const rdfQuadSchema = z.object({
  subject: z.object({
    termType: z.string(),
    value: z.string()
  }),
  predicate: z.object({
    termType: z.string(),
    value: z.string()
  }),
  object: z.object({
    termType: z.string(),
    value: z.string(),
    datatype: z.object({ value: z.string() }).optional(),
    language: z.string().optional()
  }),
  graph: z.object({
    termType: z.string(),
    value: z.string()
  }).optional()
})

/**
 * Apply transaction request schema
 */
export const applyTransactionSchema = z.object({
  delta: z.array(rdfQuadSchema).min(1, 'Delta must contain at least one quad'),
  author: z.string().optional(),
  metadata: z.record(z.any()).optional()
})

/**
 * Register policy request schema
 */
export const registerPolicySchema = z.object({
  id: z.string().min(1, 'Policy ID is required'),
  shapes: z.string().min(1, 'SHACL shapes are required'),
  priority: z.number().int().nonnegative().optional()
})

/**
 * Register effect request schema with security fields
 */
export const registerEffectSchema = z.object({
  id: z.string()
    .min(1, 'Effect ID is required')
    .max(100, 'Effect ID too long')
    .regex(/^[a-zA-Z0-9-_]+$/, 'Effect ID must be alphanumeric with dashes/underscores'),
  code: z.string()
    .min(1, 'Effect code is required')
    .max(100000, 'Effect code too large (max 100KB)'),
  timeout: z.number()
    .int()
    .positive()
    .max(30000, 'Timeout too large (max 30 seconds)')
    .optional(),
  memoryLimit: z.number()
    .int()
    .positive()
    .max(512, 'Memory limit too large (max 512MB)')
    .optional(),
  signature: z.string()
    .regex(/^[0-9a-f]+$/i, 'Signature must be hex string')
    .optional(),
  publicKey: z.string()
    .regex(/^[0-9a-f]+$/i, 'Public key must be hex string')
    .optional()
})

/**
 * Execute effect schema
 */
export const executeEffectSchema = z.object({
  effectId: z.string()
    .min(1, 'Effect ID is required')
    .max(100, 'Effect ID too long'),
  input: z.record(z.any())
    .optional()
    .default({})
})

/**
 * Initialize lockchain request schema
 */
export const initLockchainSchema = z.object({
  repoUrl: z.string().url('Invalid repository URL'),
  branch: z.string().optional(),
  credentials: z.record(z.any()).optional()
})

/**
 * Register agent request schema
 */
export const registerAgentSchema = z.object({
  id: z.string().min(1, 'Agent ID is required'),
  endpoint: z.string().url().optional(),
  priority: z.number().int().nonnegative().optional()
})

/**
 * SPARQL query request schema
 */
export const querySchema = z.object({
  query: z.string().min(1, 'SPARQL query is required'),
  format: z.enum(['json', 'turtle', 'ntriples', 'jsonld']).optional()
})
