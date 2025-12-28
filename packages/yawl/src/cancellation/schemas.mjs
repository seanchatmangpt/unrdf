/**
 * @file YAWL Cancellation Schemas
 * @module yawl/cancellation/schemas
 *
 * @description
 * Zod schemas and constants for YAWL cancellation semantics.
 */

import { z } from 'zod';
import { randomUUID } from 'crypto';

// ============================================================================
// CONSTANTS
// ============================================================================

/**
 * Valid receipt types
 * @type {readonly string[]}
 */
export const VALID_RECEIPT_TYPES = Object.freeze([
  'CANCELLED_WORK_ITEM',
  'TIMEOUT_OCCURRED',
  'CIRCUIT_BREAKER_OPEN',
  'CIRCUIT_BREAKER_CLOSED',
  'REGION_CANCELLED',
  'TASK_DISABLED',
  'TASK_ENABLED',
  'CANCELLATION_PROPAGATED',
]);

// ============================================================================
// SCHEMAS
// ============================================================================

/**
 * Schema for cancellation reason
 */
export const CancellationReasonSchema = z.enum([
  'timeout',
  'manual',
  'circuit_breaker',
  'parent_cancelled',
  'region_cancelled',
  'task_disabled',
  'dependency_failed',
  'workflow_terminated',
]);

/**
 * Schema for work item state
 */
export const WorkItemStateSchema = z.enum([
  'pending',
  'enabled',
  'executing',
  'completed',
  'cancelled',
  'failed',
]);

/**
 * Schema for work item
 */
export const WorkItemSchema = z.object({
  id: z.string().uuid(),
  taskId: z.string().min(1),
  caseId: z.string().uuid(),
  regionId: z.string().uuid().optional(),
  state: WorkItemStateSchema,
  createdAt: z.coerce.date(),
  startedAt: z.coerce.date().optional(),
  completedAt: z.coerce.date().optional(),
  cancelledAt: z.coerce.date().optional(),
  cancellationReason: CancellationReasonSchema.optional(),
  timeoutMs: z.number().int().positive().max(300000).default(30000),
  retryCount: z.number().int().nonnegative().default(0),
  metadata: z.record(z.any()).optional(),
});

/**
 * Schema for cancellation region
 */
export const CancellationRegionSchema = z.object({
  id: z.string().uuid(),
  name: z.string().min(1).max(100),
  taskIds: z.array(z.string().min(1)),
  parentRegionId: z.string().uuid().optional(),
  childRegionIds: z.array(z.string().uuid()).default([]),
  createdAt: z.coerce.date(),
  active: z.boolean().default(true),
});

/**
 * Schema for cancellation receipt (for export validation)
 */
export const CancellationReceiptSchema = z.object({
  id: z.string(),
  type: z.string(),
  timestamp: z.any(),
  payload: z.record(z.any()),
});

/**
 * Schema for circuit breaker state
 */
export const CircuitBreakerStateSchema = z.enum(['closed', 'open', 'half_open']);

/**
 * @typedef {z.infer<typeof WorkItemSchema>} WorkItem
 * @typedef {z.infer<typeof CancellationRegionSchema>} CancellationRegion
 * @typedef {z.infer<typeof CancellationReceiptSchema>} CancellationReceipt
 * @typedef {z.infer<typeof CancellationReasonSchema>} CancellationReason
 */

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/**
 * Create a validated receipt object
 * @param {string} type
 * @param {Object} payload
 * @returns {Object}
 */
export function createReceipt(type, payload) {
  if (!VALID_RECEIPT_TYPES.includes(type)) {
    throw new Error(`Invalid receipt type: ${type}`);
  }
  return {
    id: randomUUID(),
    type,
    timestamp: new Date(),
    payload,
  };
}
