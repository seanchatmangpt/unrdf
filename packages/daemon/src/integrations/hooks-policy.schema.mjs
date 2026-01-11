/**
 * @file Daemon Hooks Policy Schemas
 * @module @unrdf/daemon/integrations/hooks-policy-schemas
 * @description Zod schema definitions for hooks policy framework
 */

import { z } from 'zod';

/**
 * Policy definition schema
 * @type {Object}
 */
export const PolicySchema = z.object({
  id: z.string().min(1),
  name: z.string().min(1),
  version: z.number().int().min(1),
  type: z.enum(['approval', 'time-window', 'resource-limit', 'rate-limit', 'custom']),
  enabled: z.boolean().default(true),
  priority: z.number().int().min(0).max(100).default(50),
  description: z.string().optional(),
  config: z.record(z.string(), z.unknown()),
  createdAt: z.date().default(() => new Date()),
  updatedAt: z.date().default(() => new Date()),
  metadata: z.record(z.string(), z.unknown()).optional(),
});

/**
 * Policy decision schema
 * @type {Object}
 */
export const PolicyDecisionSchema = z.object({
  id: z.string(),
  operationId: z.string(),
  timestamp: z.date(),
  decision: z.enum(['allow', 'deny', 'defer']),
  reason: z.string(),
  policiesEvaluated: z.array(z.string()),
  conflictResolved: z.boolean().default(false),
  conflictStrategy: z.enum(['highest-priority', 'unanimous', 'majority', 'first-match']).optional(),
  metadata: z.record(z.string(), z.unknown()).optional(),
});

/**
 * Policy audit entry schema
 * @type {Object}
 */
export const PolicyAuditSchema = z.object({
  id: z.string(),
  timestamp: z.date(),
  policyId: z.string(),
  action: z.enum(['created', 'updated', 'deleted', 'enabled', 'disabled', 'rolled-back']),
  version: z.number().int().min(1),
  changes: z.record(z.string(), z.unknown()).optional(),
  actor: z.string().optional(),
  metadata: z.record(z.string(), z.unknown()).optional(),
});
