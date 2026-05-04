/**
 * @file Event Automation Schemas
 * @module @unrdf/event-automation/schemas
 * @description Zod schemas for event automation configuration and validation
 */

import { z } from 'zod';

/**
 * Delta operation schema
 */
export const DeltaOperationSchema = z.object({
  operation: z.enum(['insert', 'delete', 'update']),
  subject: z.string().min(1),
  predicate: z.string().min(1),
  object: z.string().min(1),
  graph: z.string().optional(),
  timestamp: z.number().int().positive().optional(),
  metadata: z.any().optional(),
});

/**
 * Delta schema
 */
export const DeltaSchema = z.object({
  id: z.string().min(1),
  operations: z.array(DeltaOperationSchema).min(1),
  timestamp: z.number().int().positive(),
  source: z.string().optional(),
  metadata: z.any().optional(),
});

/**
 * Receipt schema
 */
export const ReceiptSchema = z.object({
  id: z.string().min(1),
  deltaId: z.string().min(1),
  operation: z.string().min(1),
  entityType: z.string().min(1),
  timestamp: z.number().int().positive(),
  hash: z.string().min(1),
  merkleRoot: z.string().optional(),
  merkleProof: z.array(z.string()).optional(),
  metadata: z.any().optional(),
});

/**
 * Policy schema
 */
export const PolicySchema = z.object({
  id: z.string().min(1),
  name: z.string().min(1),
  description: z.string().optional(),
  trigger: z.enum(['before:delta', 'after:delta', 'before:receipt', 'after:receipt']),
  condition: z.any().optional(),
  action: z.any(),
  priority: z.number().int().min(0).max(100).optional().default(50),
  enabled: z.boolean().optional().default(true),
});

/**
 * Event automation configuration schema
 */
export const EventAutomationConfigSchema = z.object({
  id: z.string().optional().default('default-automation'),
  autoStart: z.boolean().optional().default(true),
  maxConcurrent: z.number().int().positive().optional().default(10),
  enableReceipts: z.boolean().optional().default(true),
  enablePolicies: z.boolean().optional().default(true),
  enableReplay: z.boolean().optional().default(true),
  replayBufferSize: z.number().int().positive().optional().default(1000),
  logger: z.any().optional(),
});

/**
 * Process delta result schema
 */
export const ProcessDeltaResultSchema = z.object({
  success: z.boolean(),
  deltaId: z.string(),
  receipts: z.array(ReceiptSchema).optional(),
  policyResults: z.array(z.object({
    policyId: z.string(),
    passed: z.boolean(),
    message: z.string().optional(),
  })).optional(),
  error: z.string().optional(),
  duration: z.number().optional(),
});

/**
 * Event metadata schema
 */
export const EventMetadataSchema = z.object({
  eventId: z.string().min(1),
  eventType: z.string().min(1),
  timestamp: z.number().int().positive(),
  source: z.string().optional(),
  correlationId: z.string().optional(),
  metadata: z.any().optional(),
});

/**
 * Replay options schema
 */
export const ReplayOptionsSchema = z.object({
  fromTimestamp: z.number().int().positive().optional(),
  toTimestamp: z.number().int().positive().optional(),
  deltaIds: z.array(z.string()).optional(),
  batchSize: z.number().int().positive().optional().default(100),
  parallel: z.boolean().optional().default(false),
});

/**
 * Statistics schema
 */
export const StatisticsSchema = z.object({
  totalProcessed: z.number().int().nonnegative(),
  totalSucceeded: z.number().int().nonnegative(),
  totalFailed: z.number().int().nonnegative(),
  totalReceipts: z.number().int().nonnegative(),
  averageDuration: z.number().nonnegative(),
  p95Duration: z.number().nonnegative().optional(),
  p99Duration: z.number().nonnegative().optional(),
  uptimeMs: z.number().int().nonnegative(),
});
