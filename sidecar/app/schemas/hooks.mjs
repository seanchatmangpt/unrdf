/**
 * Knowledge Hook validation schemas
 * @module schemas/hooks
 */

import { z } from 'zod'
import {
  Sha256Schema,
  HookPhaseSchema,
  HookIdSchema,
  TimestampSchema,
  SparqlQuerySchema,
  SeveritySchema,
  ErrorCodeSchema
} from './common.mjs'

/**
 * Predicate schema for hook conditions
 * Defines the condition that triggers a knowledge hook
 */
export const PredicateSchema = z.object({
  /** Type of predicate: SPARQL query or custom function */
  type: z.enum(['sparql', 'custom'], {
    errorMap: () => ({ message: 'Predicate type must be either "sparql" or "custom"' })
  }),

  /** SPARQL query string (required for sparql type) */
  query: SparqlQuerySchema.optional(),

  /** Name of custom predicate function (required for custom type) */
  name: z.string().min(1).max(100).optional(),

  /** Additional parameters for custom predicates */
  params: z.record(z.any()).optional()
}).refine(
  (data) => {
    // Ensure sparql predicates have query, custom predicates have name
    if (data.type === 'sparql') return !!data.query
    if (data.type === 'custom') return !!data.name
    return true
  },
  {
    message: 'SPARQL predicates require "query", custom predicates require "name"'
  }
)

/**
 * Knowledge Hook schema
 * Core schema for defining knowledge hooks
 */
export const KnowledgeHookSchema = z.object({
  /** Unique identifier for the hook */
  id: HookIdSchema,

  /** Human-readable name */
  name: z.string().min(1).max(100, {
    message: 'Hook name must be between 1 and 100 characters'
  }),

  /** Optional description of the hook's purpose */
  description: z.string().max(500).optional(),

  /** Execution phase */
  phase: HookPhaseSchema,

  /** Array of predicates that must be satisfied */
  predicates: z.array(PredicateSchema).min(1, {
    message: 'At least one predicate is required'
  }),

  /** Optional SPARQL SELECT query for data retrieval */
  selectQuery: SparqlQuerySchema.optional(),

  /** SHA-256 hash of the select query for integrity verification */
  selectQuerySha256: Sha256Schema.optional(),

  /** Whether the hook is currently disabled */
  disabled: z.boolean().default(false),

  /** Priority for execution order (lower numbers execute first) */
  priority: z.number().int().min(0).max(100).default(50),

  /** Tags for categorization */
  tags: z.array(z.string()).default([]),

  /** Metadata for additional context */
  metadata: z.record(z.any()).optional(),

  /** Creation timestamp */
  createdAt: TimestampSchema.optional(),

  /** Last update timestamp */
  updatedAt: TimestampSchema.optional()
}).refine(
  (data) => {
    // If selectQuerySha256 is provided, selectQuery must also be provided
    if (data.selectQuerySha256) return !!data.selectQuery
    return true
  },
  {
    message: 'selectQuerySha256 requires selectQuery to be present'
  }
)

/**
 * Hook creation input schema
 * Schema for creating new hooks (omits auto-generated fields)
 */
export const CreateHookSchema = KnowledgeHookSchema.omit({
  createdAt: true,
  updatedAt: true
})

/**
 * Hook update input schema
 * Schema for updating existing hooks (all fields optional except id)
 */
export const UpdateHookSchema = KnowledgeHookSchema.partial().required({
  id: true
})

/**
 * Hook list response schema
 * Schema for API responses returning multiple hooks
 */
export const HookListResponseSchema = z.object({
  /** Array of knowledge hooks */
  hooks: z.array(KnowledgeHookSchema),

  /** Total number of hooks */
  total: z.number().int().nonnegative(),

  /** Number of hooks per page (for pagination) */
  limit: z.number().int().positive().optional(),

  /** Offset for pagination */
  offset: z.number().int().nonnegative().optional()
})

/**
 * Hook evaluation result schema
 * Schema for results from evaluating a knowledge hook
 */
export const HookEvaluationResultSchema = z.object({
  /** Whether the evaluation completed without errors */
  success: z.boolean(),

  /** Whether all predicates passed */
  passed: z.boolean(),

  /** Execution duration in milliseconds */
  duration: z.number().nonnegative(),

  /** Results from predicate evaluations */
  results: z.array(z.object({
    /** Predicate that was evaluated */
    predicate: PredicateSchema,

    /** Whether this predicate passed */
    passed: z.boolean(),

    /** SPARQL query results (if applicable) */
    bindings: z.array(z.record(z.any())).optional(),

    /** Error message (if failed) */
    error: z.string().optional()
  })),

  /** SELECT query results (if selectQuery was provided) */
  selectResults: z.array(z.record(z.any())).optional(),

  /** Overall error message (if evaluation failed) */
  error: z.string().optional(),

  /** Timestamp of evaluation */
  timestamp: TimestampSchema.optional()
})

/**
 * Hook validation error schema
 * Schema for validation errors
 */
export const HookValidationErrorSchema = z.object({
  /** Error code */
  code: ErrorCodeSchema,

  /** Human-readable error message */
  message: z.string(),

  /** Severity of the error */
  severity: SeveritySchema,

  /** Path to the field that failed validation */
  path: z.array(z.union([z.string(), z.number()])).optional(),

  /** Additional context */
  context: z.record(z.any()).optional()
})

/**
 * Batch hook evaluation request schema
 * Schema for evaluating multiple hooks in a single request
 */
export const BatchHookEvaluationSchema = z.object({
  /** Array of hook IDs to evaluate */
  hookIds: z.array(HookIdSchema).min(1, {
    message: 'At least one hook ID is required'
  }),

  /** Optional context data for evaluation */
  context: z.record(z.any()).optional(),

  /** Whether to stop on first failure */
  failFast: z.boolean().default(false)
})

/**
 * Batch hook evaluation response schema
 */
export const BatchHookEvaluationResponseSchema = z.object({
  /** Individual evaluation results */
  results: z.array(z.object({
    hookId: HookIdSchema,
    result: HookEvaluationResultSchema
  })),

  /** Overall success (all hooks passed) */
  success: z.boolean(),

  /** Total execution duration */
  totalDuration: z.number().nonnegative()
})
