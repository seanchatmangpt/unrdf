/**
 * @file YAWL Daemon Bridge Configuration Schemas
 * @module @unrdf/daemon/integrations/yawl-schemas
 * @description Zod schema definitions for YAWL daemon bridge configuration
 */

import { z } from 'zod';

/**
 * Retry policy for YAWL task failures
 * Configures exponential backoff for task retry attempts
 * @type {z.ZodType}
 */
export const YawlRetryPolicySchema = z.object({
  maxAttempts: z.number().int().min(1).max(10).default(3),
  backoffMs: z.number().int().min(100).max(60000).default(1000),
  backoffMultiplier: z.number().min(1.1).max(10).default(2),
  maxBackoffMs: z.number().int().min(1000).max(300000).default(30000),
  jitterFactor: z.number().min(0).max(1).default(0.1),
}).default({});

/**
 * Timeout configuration for YAWL tasks
 * @type {z.ZodType}
 */
export const YawlTimeoutConfigSchema = z.object({
  taskTimeoutMs: z.number().int().min(1000).max(3600000).default(30000),
  caseTimeoutMs: z.number().int().min(5000).max(86400000).default(3600000),
  checkIntervalMs: z.number().int().min(100).max(30000).default(5000),
}).default({});

/**
 * Distribution strategy for parallel task execution
 * @type {z.ZodType}
 */
export const DistributionStrategySchema = z.enum([
  'round-robin',
  'least-loaded',
  'random',
  'affinity',
]).default('round-robin');

/**
 * YAWL Daemon Bridge configuration schema
 * @type {z.ZodType}
 */
export const YawlDaemonBridgeConfigSchema = z.object({
  bridgeId: z.string().min(1).default(() => `yawl-bridge-${Date.now()}`),
  daemonNodeId: z.string().min(1),
  maxConcurrentCases: z.number().int().min(1).max(10000).default(100),
  retryPolicy: YawlRetryPolicySchema,
  timeoutDefaults: YawlTimeoutConfigSchema,
  enableAutoRetry: z.boolean().default(true),
  enableTimeoutTracking: z.boolean().default(true),
  enableDistribution: z.boolean().default(true),
  logger: z.any().optional(),
}).refine(
  (config) => config.maxConcurrentCases > 0,
  { message: 'maxConcurrentCases must be greater than 0', path: ['maxConcurrentCases'] }
);
