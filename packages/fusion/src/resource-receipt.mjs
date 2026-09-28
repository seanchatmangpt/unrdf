/**
 * @file Resource Receipt - BLAKE3 hash-chained receipts for resource allocation events
 * @module @unrdf/fusion/resource-receipt
 *
 * @description
 * Minimal receipt generator used by the resource manager. This was previously
 * imported from `@unrdf/yawl/receipt`; that package was removed from the
 * workspace, so the (small) subset of its receipt-core that fusion actually
 * needs lives here: RESOURCE_ALLOCATED / RESOURCE_RELEASED events chained with
 * BLAKE3 (same hashing scheme as the original: payload hash, then
 * `previousHash|GENESIS : payloadHash`).
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';
import { now, toISO } from '@unrdf/kgc-4d';

/** BLAKE3 hash length in hex characters */
export const BLAKE3_HEX_LENGTH = 64;

/**
 * Supported receipt event types for resource management
 * @readonly
 * @enum {string}
 */
export const RECEIPT_EVENT_TYPES = Object.freeze({
  RESOURCE_ALLOCATED: 'RESOURCE_ALLOCATED',
  RESOURCE_RELEASED: 'RESOURCE_RELEASED',
});

const PayloadSchema = z
  .object({
    decision: z.string(),
    justification: z.object({ reasoning: z.string().optional() }).passthrough().optional(),
    actor: z.string().optional(),
    context: z.any().optional(),
  })
  .passthrough();

const ReceiptSchema = z.object({
  id: z.string().uuid(),
  eventType: z.enum(Object.values(RECEIPT_EVENT_TYPES)),
  t_ns: z.bigint(),
  timestamp_iso: z.string(),
  caseId: z.string().min(1),
  taskId: z.string().min(1),
  workItemId: z.string().optional(),
  previousReceiptHash: z.string().length(BLAKE3_HEX_LENGTH).nullable(),
  payloadHash: z.string().length(BLAKE3_HEX_LENGTH),
  receiptHash: z.string().length(BLAKE3_HEX_LENGTH),
  payload: PayloadSchema,
});

/**
 * Serialize an object deterministically (sorted keys at every level)
 * @param {*} obj - Value to serialize
 * @returns {string} Deterministic JSON string
 */
export function deterministicSerialize(obj) {
  if (obj === null || obj === undefined) return JSON.stringify(null);
  if (typeof obj === 'bigint') return obj.toString();
  if (typeof obj !== 'object') return JSON.stringify(obj);
  if (Array.isArray(obj)) return `[${obj.map(deterministicSerialize).join(',')}]`;
  const pairs = Object.keys(obj)
    .sort()
    .map(key => `${JSON.stringify(key)}:${deterministicSerialize(obj[key])}`);
  return `{${pairs.join(',')}}`;
}

/**
 * Generate a BLAKE3 hash-chained receipt for a resource event
 *
 * @param {Object} event - Event to receipt
 * @param {string} event.eventType - One of RECEIPT_EVENT_TYPES
 * @param {string} event.caseId - Case identifier
 * @param {string} event.taskId - Task (resource) identifier
 * @param {string} [event.workItemId] - Work item (allocation) identifier
 * @param {Object} event.payload - Decision payload
 * @param {Object|null} [previousReceipt=null] - Previous receipt for chaining
 * @returns {Promise<Object>} Validated receipt
 */
export async function generateReceipt(event, previousReceipt = null) {
  if (!Object.values(RECEIPT_EVENT_TYPES).includes(event.eventType)) {
    throw new Error(`Invalid event type: ${event.eventType}`);
  }

  const t_ns = now();
  const payloadHash = await blake3(
    deterministicSerialize({
      eventType: event.eventType,
      caseId: event.caseId,
      taskId: event.taskId,
      workItemId: event.workItemId || null,
      payload: event.payload,
      t_ns: t_ns.toString(),
    })
  );

  const previousReceiptHash = previousReceipt ? previousReceipt.receiptHash : null;
  const receiptHash = await blake3(`${previousReceiptHash || 'GENESIS'}:${payloadHash}`);

  return ReceiptSchema.parse({
    id: globalThis.crypto.randomUUID(),
    eventType: event.eventType,
    t_ns,
    timestamp_iso: toISO(t_ns),
    caseId: event.caseId,
    taskId: event.taskId,
    workItemId: event.workItemId || undefined,
    previousReceiptHash,
    payloadHash,
    receiptHash,
    payload: event.payload,
  });
}
