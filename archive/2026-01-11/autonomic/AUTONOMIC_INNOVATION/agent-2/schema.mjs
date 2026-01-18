/**
 * @file Capsule IR Schema Definitions
 * @description Zod schemas for portable, content-addressed change programs
 * @module agent-2/schema
 */

import { z } from 'zod';

/**
 * Intent operation schema
 * Operations: set (update field), create (new entity), link (add relation), unlink (remove relation)
 * @typedef {z.infer<typeof IntentOpSchema>} IntentOp
 */
export const IntentOpSchema = z.object({
  op: z.enum(['set', 'create', 'link', 'unlink']),
  target: z.string().min(1, 'Target must be non-empty'),
  value: z.union([z.string(), z.number(), z.boolean(), z.null()]).optional(),
  profile: z.string().optional(),
});

/**
 * RDF delta operation schema
 * Operations: add (insert triple), del (delete triple)
 * @typedef {z.infer<typeof DeltaOpSchema>} DeltaOp
 */
export const DeltaOpSchema = z.object({
  op: z.enum(['add', 'del']),
  subject: z.string().min(1),
  predicate: z.string().min(1),
  object: z.string().min(1),
  graph: z.string().optional(),
});

/**
 * Guard constraints schema
 * Defines limits, profiles, and invariants for capsule execution
 * @typedef {z.infer<typeof GuardSchema>} Guard
 */
export const GuardSchema = z.object({
  limits: z.object({
    maxDeltas: z.number().int().positive().default(1000),
    maxDepth: z.number().int().positive().default(10),
    timeout: z.number().int().positive().default(5000),
  }).default({}),
  profiles: z.array(z.string()).default([]),
  invariants: z.array(z.string()).default([]),
});

/**
 * Receipt schema for tamper-evident chains
 * Hash creates content-addressed identity, parents create Merkle DAG
 * @typedef {z.infer<typeof ReceiptSchema>} Receipt
 */
export const ReceiptSchema = z.object({
  hash: z.string().regex(/^[a-f0-9]{64}$/, 'Hash must be 64-char hex SHA-256'),
  parents: z.array(z.string().regex(/^[a-f0-9]{64}$/)).default([]),
  timestamp: z.number().int().positive(),
  signer: z.string().optional(),
});

/**
 * Complete Capsule schema
 * A portable, content-addressed change program with:
 * - intent: high-level operations
 * - delta: low-level RDF changes
 * - guard: execution constraints
 * - receipt: tamper-evident identity
 * @typedef {z.infer<typeof CapsuleSchema>} Capsule
 */
export const CapsuleSchema = z.object({
  intent: z.array(IntentOpSchema).min(1, 'Capsule must have at least one intent operation'),
  delta: z.array(DeltaOpSchema).default([]),
  guard: GuardSchema.default({}),
  receipt: ReceiptSchema.optional(),
});

/**
 * Partial capsule schema for planning phase (before compilation)
 * @typedef {z.infer<typeof PartialCapsuleSchema>} PartialCapsule
 */
export const PartialCapsuleSchema = z.object({
  intent: z.array(IntentOpSchema).min(1),
  delta: z.array(DeltaOpSchema).optional().default([]),
  guard: GuardSchema.optional().default({}),
  receipt: ReceiptSchema.optional(),
});
