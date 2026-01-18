/**
 * Type Definitions and Zod Schemas for Commutativity Analysis
 * @module agent-5/types
 */

import { z } from 'zod';

/**
 * @typedef {Object} Quad
 * @property {Object} subject - Subject term
 * @property {string} subject.value - Subject IRI
 * @property {Object} predicate - Predicate term
 * @property {string} predicate.value - Predicate IRI
 * @property {Object} object - Object term
 * @property {string} object.value - Object value
 * @property {Object} [graph] - Graph term
 * @property {string} [graph.value] - Graph IRI
 */

/**
 * @typedef {Object} Capsule
 * @property {string} id - UUID or content hash
 * @property {Set<Quad>} add - Quads to add
 * @property {Set<Quad>} del - Quads to delete
 * @property {Object} [metadata] - Provenance, timestamp, etc.
 */

/**
 * @typedef {Object} ReorderResult
 * @property {boolean} ok - Can safely reorder
 * @property {string} reason - Why OK or conflict type
 * @property {Quad[]} [witness] - Minimal counterexample
 */

/**
 * @typedef {Object} ConflictCertificate
 * @property {Quad[]} counterexample - Minimal quad set showing conflict
 * @property {string} explanation - Human-readable reason
 * @property {string} hash - SHA-256 of canonicalized certificate
 * @property {string[]} capsuleIds - IDs of conflicting capsules
 * @property {string} conflictType - Classification (add-del, write-write, etc.)
 * @property {string} version - Certificate format version
 * @property {string} timestamp - ISO 8601 UTC timestamp
 * @property {Object} metadata - Additional certificate metadata
 */

/**
 * @typedef {Object} ImpactSet
 * @property {Set<string>} subjects - All touched subject IRIs
 * @property {Set<string>} predicates - All touched predicate IRIs
 * @property {Set<string>} graphs - All touched graph IRIs
 */

// Zod schema for Quad (simplified, quads are complex objects)
export const QuadSchema = z.object({
  subject: z.object({ value: z.string() }),
  predicate: z.object({ value: z.string() }),
  object: z.object({ value: z.string() }),
  graph: z.object({ value: z.string() }).optional(),
});

// Zod schema for Capsule
export const CapsuleSchema = z.object({
  id: z.string().min(1),
  add: z.instanceof(Set),
  del: z.instanceof(Set),
  metadata: z.record(z.unknown()).optional(),
});

// Zod schema for ReorderResult
export const ReorderResultSchema = z.object({
  ok: z.boolean(),
  reason: z.string(),
  witness: z.array(z.any()).optional(),
});

// Zod schema for ConflictCertificate
export const ConflictCertificateSchema = z.object({
  counterexample: z.array(z.any()),
  explanation: z.string(),
  hash: z.string(),
  capsuleIds: z.array(z.string()),
  conflictType: z.string(),
  version: z.string(),
  timestamp: z.string(),
  metadata: z.record(z.unknown()),
});
