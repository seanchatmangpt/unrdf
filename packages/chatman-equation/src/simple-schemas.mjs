/**
 * @file simple-schemas.mjs
 * @module @unrdf/chatman-equation/simple-schemas
 * @description Simple Zod schemas for 3T methodology (Zod v4 compatible)
 */

import { z } from 'zod';

/**
 * Domain enumeration
 */
export const DomainEnum = z.enum([
  'market',
  'organization',
  'strategy',
  'product',
  'customer',
  'blue_ocean',
]);

/**
 * Observation Schema
 */
export const ObservationSchema = z.object({
  id: z.string().uuid(),
  timestamp: z.string().datetime(),
  domain: DomainEnum,
  state: z.record(z.any()),
  metadata: z.record(z.any()).optional(),
});

/**
 * Delta Operation Schema
 */
export const DeltaOperationSchema = z.object({
  op: z.enum(['add', 'update', 'delete']),
  field: z.string().min(1),
  value: z.any(),
  reason: z.string().optional(),
});

/**
 * Delta Schema
 */
export const DeltaSchema = z.object({
  id: z.string().uuid(),
  timestamp: z.string().datetime(),
  domain: DomainEnum,
  operations: z.array(DeltaOperationSchema).min(1),
});

/**
 * Closure Operator Schema
 */
export const ClosureOperatorSchema = z.object({
  type: z.enum(['merge', 'transform', 'reconcile', 'compose']),
  name: z.string().min(1),
  domain: DomainEnum,
  policies: z.array(z.string()).optional(),
  conflict_resolution: z.enum(['delta_wins', 'current_wins', 'merge', 'reject']).default('delta_wins'),
  invariants: z.array(z.string()).optional(),
});

/**
 * Artifact Schema
 */
export const ArtifactSchema = z.object({
  id: z.string().uuid(),
  timestamp: z.string().datetime(),
  source_observation: z.string().uuid(),
  applied_deltas: z.array(z.string().uuid()).min(1),
  operator: z.string().min(1),
  result: z.record(z.any()),
  proof: z.record(z.any()).optional(),
  receipt: z.record(z.any()).optional(),
});
