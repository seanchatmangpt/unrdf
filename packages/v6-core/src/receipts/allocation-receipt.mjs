/**
 * Allocation Receipt - V6 Resource/Pool/Calendar Receipts
 *
 * For YAWL resource allocation, pool management, and capacity tracking.
 *
 * @module @unrdf/v6-core/receipts/allocation-receipt
 */

import { z } from 'zod';
import { BaseReceiptSchema, RECEIPT_TYPES } from './base-receipt.mjs';

// =============================================================================
// Event Types
// =============================================================================

/**
 * Allocation event types
 * @readonly
 * @enum {string}
 */
export const ALLOCATION_EVENT_TYPES = Object.freeze({
  RESOURCE_ALLOCATED: 'RESOURCE_ALLOCATED',
  RESOURCE_RELEASED: 'RESOURCE_RELEASED',
  POOL_CREATED: 'POOL_CREATED',
  POOL_UPDATED: 'POOL_UPDATED',
  CAPACITY_CHANGED: 'CAPACITY_CHANGED',
  CALENDAR_UPDATED: 'CALENDAR_UPDATED',
});

// =============================================================================
// Schemas
// =============================================================================

/**
 * Event type schema
 */
export const AllocationEventTypeSchema = z.enum([
  'RESOURCE_ALLOCATED',
  'RESOURCE_RELEASED',
  'POOL_CREATED',
  'POOL_UPDATED',
  'CAPACITY_CHANGED',
  'CALENDAR_UPDATED',
]);

/**
 * Time period schema
 */
export const AllocationPeriodSchema = z.object({
  /** Start timestamp (ISO 8601) */
  start: z.string(),
  /** End timestamp (ISO 8601) */
  end: z.string(),
  /** Duration in milliseconds */
  durationMs: z.number().optional(),
});

/**
 * Capacity schema
 */
export const CapacitySchema = z.object({
  /** Total capacity */
  total: z.number(),
  /** Available capacity */
  available: z.number(),
  /** Allocated capacity */
  allocated: z.number(),
  /** Unit of capacity (e.g., 'hours', 'units', 'concurrent') */
  unit: z.string(),
});

/**
 * Allocation payload schema
 */
export const AllocationPayloadSchema = z.object({
  /** Allocation action (e.g., 'ALLOCATE', 'RELEASE', 'UPDATE') */
  action: z.string(),
  /** Amount allocated/released */
  amount: z.number().optional(),
  /** Resource attributes */
  resourceAttributes: z.record(z.string(), z.any()).optional(),
  /** Allocation priority */
  priority: z.number().optional(),
  /** Allocation reason */
  reason: z.string().optional(),
}).passthrough();

/**
 * Allocation receipt schema - extends base with resource-specific fields
 */
export const AllocationReceiptSchema = BaseReceiptSchema.extend({
  receiptType: z.literal(RECEIPT_TYPES.ALLOCATION),

  /** Event type (RESOURCE_ALLOCATED, etc.) */
  eventType: AllocationEventTypeSchema,

  /** Resource identifier */
  resourceId: z.string().min(1),

  /** Resource pool identifier */
  poolId: z.string().min(1),

  /** Allocation time period */
  allocationPeriod: AllocationPeriodSchema,

  /** Current capacity state */
  capacity: CapacitySchema,

  /** Entity/task resource is allocated to */
  allocatedTo: z.string().optional(),

  /** Allocation payload */
  payload: AllocationPayloadSchema,
});

// =============================================================================
// Type Definitions (JSDoc)
// =============================================================================

/**
 * @typedef {Object} AllocationPeriod
 * @property {string} start - Start timestamp (ISO 8601)
 * @property {string} end - End timestamp (ISO 8601)
 * @property {number} [durationMs] - Duration in milliseconds
 */

/**
 * @typedef {Object} Capacity
 * @property {number} total - Total capacity
 * @property {number} available - Available capacity
 * @property {number} allocated - Allocated capacity
 * @property {string} unit - Unit of capacity
 */

/**
 * @typedef {Object} AllocationPayload
 * @property {string} action - Allocation action
 * @property {number} [amount] - Amount allocated/released
 * @property {Record<string, any>} [resourceAttributes] - Resource attributes
 * @property {number} [priority] - Allocation priority
 * @property {string} [reason] - Allocation reason
 */

/**
 * @typedef {Object} AllocationReceipt
 * @property {string} id - UUID of receipt
 * @property {'allocation'} receiptType - Receipt type discriminator
 * @property {bigint} t_ns - Nanosecond timestamp
 * @property {string} timestamp_iso - ISO timestamp
 * @property {string|null} previousHash - Previous receipt hash
 * @property {string} payloadHash - Payload hash
 * @property {string} receiptHash - Receipt hash
 * @property {string} eventType - Allocation event type
 * @property {string} resourceId - Resource ID
 * @property {string} poolId - Resource pool ID
 * @property {AllocationPeriod} allocationPeriod - Allocation period
 * @property {Capacity} capacity - Capacity state
 * @property {string} [allocatedTo] - Allocated to entity/task
 * @property {AllocationPayload} payload - Allocation payload
 * @property {Object} [attestation] - Signature/attestation
 * @property {Object} [vectorClock] - Vector clock
 * @property {string} [gitRef] - Git reference
 * @property {string} [kgcEventId] - KGC event ID
 */

// =============================================================================
// Exports
// =============================================================================

export default AllocationReceiptSchema;
