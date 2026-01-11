/**
 * @file Daemon Knowledge Rules Schemas
 * @module @unrdf/daemon/integrations/knowledge-rules-schemas
 * @description Zod schema definitions for knowledge rules
 */

import { z } from 'zod';

/**
 * Confidence level enumeration
 * @type {Object}
 */
export const ConfidenceLevels = {
  VERY_HIGH: 0.95,
  HIGH: 0.75,
  MEDIUM: 0.5,
  LOW: 0.25,
  VERY_LOW: 0.05,
};

/**
 * SPARQL pattern condition schema
 * @type {z.ZodType}
 */
export const SparqlPatternSchema = z.object({
  type: z.literal('sparql'),
  query: z.string().min(10),
  bindings: z.record(z.string(), z.any()).optional(),
  timeout: z.number().int().min(100).max(60000).default(5000),
});

/**
 * Business logic condition schema
 * @type {z.ZodType}
 */
export const BusinessLogicConditionSchema = z.object({
  type: z.literal('business-logic'),
  evaluator: z.function(),
  description: z.string(),
});

/**
 * Rule condition schema (union of pattern types)
 * @type {z.ZodType}
 */
export const RuleConditionSchema = z.lazy(() => z.union([
  SparqlPatternSchema,
  z.object({
    type: z.literal('business-logic'),
    evaluator: z.function(),
    description: z.string(),
  }),
  z.object({
    type: z.literal('composite'),
    operator: z.enum(['and', 'or', 'not']),
    conditions: z.array(RuleConditionSchema),
  }),
]));

/**
 * Rule action schema
 * @type {z.ZodType}
 */
export const RuleActionSchema = z.object({
  type: z.string().min(1),
  payload: z.record(z.string(), z.any()),
  priority: z.enum(['critical', 'high', 'normal', 'low']).default('normal'),
});

/**
 * Inference rule schema
 * @type {z.ZodType}
 */
export const InferenceRuleSchema = z.object({
  id: z.string().uuid(),
  name: z.string().min(1).max(255),
  description: z.string().max(500).optional(),
  version: z.string().regex(/^\d+\.\d+\.\d+$/),
  condition: RuleConditionSchema,
  action: RuleActionSchema,
  dependencies: z.array(z.string()).default([]),
  minConfidence: z.number().min(0).max(1).default(0.5),
  metadata: z.object({
    author: z.string().optional(),
    createdAt: z.date().default(() => new Date()),
    updatedAt: z.date().default(() => new Date()),
    tags: z.array(z.string()).default([]),
    abTest: z.object({
      enabled: z.boolean().default(false),
      variant: z.enum(['control', 'treatment']).optional(),
      splitPercentage: z.number().min(0).max(100).default(50),
    }).optional(),
  }).default({}),
}).refine(
  (rule) => !rule.metadata.abTest?.enabled || rule.metadata.abTest.variant,
  { message: 'A/B test variant must be specified when enabled', path: ['metadata.abTest.variant'] }
);

/**
 * Rule execution result schema
 * @type {z.ZodType}
 */
export const RuleExecutionResultSchema = z.object({
  ruleId: z.string().uuid(),
  ruleName: z.string(),
  matched: z.boolean(),
  confidence: z.number().min(0).max(1),
  explanation: z.object({
    reason: z.string(),
    matchedPatterns: z.array(z.string()),
    failedConditions: z.array(z.string()).default([]),
    inferenceChain: z.array(z.string()).default([]),
  }),
  action: RuleActionSchema.optional(),
  executedAt: z.date().default(() => new Date()),
  duration: z.number().int().min(0),
});

/**
 * Daemon rule engine configuration schema
 * @type {z.ZodType}
 */
export const DaemonRuleEngineConfigSchema = z.object({
  engineId: z.string().min(1).default(() => `rule-engine-${Date.now()}`),
  enableInference: z.boolean().default(true),
  enableExplanations: z.boolean().default(true),
  enableABTesting: z.boolean().default(true),
  maxRuleChainDepth: z.number().int().min(1).max(50).default(10),
  confidenceThreshold: z.number().min(0).max(1).default(0.5),
  timeout: z.number().int().min(1000).max(300000).default(30000),
  logger: z.any().optional(),
});
