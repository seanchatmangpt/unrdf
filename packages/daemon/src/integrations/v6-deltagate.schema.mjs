/**
 * @file v6 DeltaGate Schemas
 * @module @unrdf/daemon/integrations/v6-deltagate-schemas
 * @description Zod schema definitions for delta contracts and receipts
 */

import { z } from 'zod';

/**
 * Delta Operation Schema - Individual state change
 * @type {z.ZodDiscriminatedUnion}
 */
export const DeltaOperationSchema = z.discriminatedUnion('op', [
  z.object({
    op: z.literal('set'),
    path: z.string().min(1),
    oldValue: z.any().optional(),
    newValue: z.any(),
    timestamp_ns: z.bigint(),
  }),
  z.object({
    op: z.literal('delete'),
    path: z.string().min(1),
    oldValue: z.any(),
    timestamp_ns: z.bigint(),
  }),
  z.object({
    op: z.literal('insert'),
    path: z.string().min(1),
    index: z.number().int().min(0),
    value: z.any(),
    timestamp_ns: z.bigint(),
  }),
]);

/**
 * Delta Contract Schema - Change proposal with metadata
 * @type {z.ZodObject}
 */
export const DeltaContractSchema = z.object({
  id: z.string().uuid(),
  timestamp_ns: z.bigint(),
  timestamp_iso: z.string().datetime(),
  operations: z.array(DeltaOperationSchema).min(1),
  source: z.object({
    package: z.string().min(1),
    actor: z.string().optional(),
    nodeId: z.string().optional(),
    context: z.record(z.any()).optional(),
  }),
  admissibility: z.object({
    policyId: z.string().optional(),
    constraints: z.array(z.string()).optional(),
    preConditions: z.array(z.string()).optional(),
  }).optional(),
  previousDeltaId: z.string().uuid().nullable(),
});

/**
 * Receipt Schema - Delta execution receipt
 * @type {z.ZodObject}
 */
export const DeltaReceiptSchema = z.object({
  id: z.string().uuid(),
  deltaId: z.string().uuid(),
  timestamp_ns: z.bigint(),
  timestamp_iso: z.string().datetime(),
  applied: z.boolean(),
  reason: z.string().optional(),
  stateHash: z.string().length(64).optional(),
  operationsApplied: z.number().int().min(0),
  operationsFailed: z.number().int().min(0).default(0),
  metadata: z.record(z.any()).optional(),
  previousReceiptHash: z.string().length(64).nullable(),
  receiptHash: z.string().length(64),
});

/**
 * Health Status Schema
 * @type {z.ZodObject}
 */
export const HealthStatusSchema = z.object({
  status: z.enum(['healthy', 'degraded', 'unhealthy']),
  deltasProcessed: z.number().int().min(0),
  deltasRejected: z.number().int().min(0),
  lastDeltaId: z.string().uuid().nullable().optional(),
  lastReceiptHash: z.string().length(64).nullable().optional(),
  timestamp_ns: z.bigint(),
});
