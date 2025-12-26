/**
 * Capsule IR - Core data structures and schemas
 * @module capsule
 */

import { z } from '../../../node_modules/.pnpm/zod@4.2.1/node_modules/zod/index.js';
import { hashCapsule } from './hash.mjs';
import { canonicalizeQuads } from './canonicalize.mjs';
import { now, toISO } from '../../../packages/kgc-4d/src/index.mjs';

/**
 * Intent operation types
 */
export const IntentOpSchema = z.discriminatedUnion('type', [
  z.object({
    type: z.literal('set'),
    subject: z.string(),
    predicate: z.string(),
    object: z.any(),
  }),
  z.object({
    type: z.literal('create'),
    subject: z.string(),
    graph: z.string(),
  }),
  z.object({
    type: z.literal('link'),
    subject: z.string(),
    predicate: z.string(),
    target: z.string(),
  }),
  z.object({
    type: z.literal('unlink'),
    subject: z.string(),
    predicate: z.string(),
    target: z.string(),
  }),
]);

/**
 * Quad representation (serialized for determinism)
 */
export const QuadSchema = z.object({
  subject: z.string(),
  subjectType: z.enum(['NamedNode', 'BlankNode']),
  predicate: z.string(),
  object: z.object({
    value: z.string(),
    type: z.enum(['NamedNode', 'BlankNode', 'Literal']),
    datatype: z.string().optional(),
    language: z.string().optional(),
  }),
  graph: z.string(),
});

/**
 * Capsule schema - complete validation
 */
export const CapsuleSchema = z.object({
  id: z.string(),
  version: z.literal('capsule/v1'),
  intent: z.object({ ops: z.array(IntentOpSchema) }),
  delta: z.object({
    add: z.array(QuadSchema),
    del: z.array(QuadSchema),
  }),
  guard: z.object({
    limits: z.object({
      maxQuads: z.number().int().positive(),
      maxDepth: z.number().int().positive(),
      timeout: z.number().int().positive(),
    }),
    profiles: z.array(z.string()),
  }),
  receipt: z.object({
    hash: z.string(),
    parents: z.array(z.string()),
    timestamp: z.string(),
    signer: z.string().optional(),
  }),
});

/**
 * Create a new Capsule from intent and delta
 * @param {Object} intent - High-level operations
 * @param {Object} delta - Compiled quad changes
 * @param {Object} guard - Safety constraints
 * @param {Array<string>} parents - Parent capsule hashes
 * @returns {Promise<Object>} Validated Capsule
 */
export async function createCapsule(intent, delta, guard, parents = []) {
  // 1. Canonicalize delta quads (deterministic sorting)
  const canonicalDelta = {
    add: canonicalizeQuads(delta.add || []),
    del: canonicalizeQuads(delta.del || []),
  };

  // 2. Generate timestamp (nanosecond precision from @unrdf/kgc-4d)
  const timestamp = toISO(now());

  // 3. Build capsule (without hash/id yet)
  const capsule = {
    version: 'capsule/v1',
    intent,
    delta: canonicalDelta,
    guard,
    receipt: {
      hash: '', // Computed next
      parents: parents.slice().sort(), // Chronological order
      timestamp,
    },
  };

  // 4. Compute content hash
  const hash = await hashCapsule(capsule);
  capsule.id = hash;
  capsule.receipt.hash = hash;

  // 5. Validate with Zod
  return CapsuleSchema.parse(capsule);
}

/**
 * Validate a capsule against schema
 * @param {Object} capsule - Capsule to validate
 * @returns {Object} Validated capsule
 * @throws {Error} If validation fails
 */
export function validateCapsule(capsule) {
  return CapsuleSchema.parse(capsule);
}

/**
 * Serialize quad for storage (from RDF quad object)
 * @param {Object} quad - RDF quad object
 * @returns {Object} Serialized quad
 */
export function serializeQuad(quad) {
  return {
    subject: quad.subject.value,
    subjectType: quad.subject.termType,
    predicate: quad.predicate.value,
    object: {
      value: quad.object.value,
      type: quad.object.termType,
      datatype: quad.object.datatype?.value,
      language: quad.object.language,
    },
    graph: quad.graph.value || '',
  };
}

/**
 * Deserialize quad from storage (to RDF quad object)
 * @param {Object} obj - Serialized quad
 * @param {Object} dataFactory - RDF data factory
 * @returns {Object} RDF quad
 */
export function deserializeQuad(obj, dataFactory) {
  // Create subject
  const subject =
    obj.subjectType === 'BlankNode'
      ? dataFactory.blankNode(obj.subject)
      : dataFactory.namedNode(obj.subject);

  // Create predicate
  const predicate = dataFactory.namedNode(obj.predicate);

  // Create object
  let object;
  switch (obj.object.type) {
    case 'NamedNode':
      object = dataFactory.namedNode(obj.object.value);
      break;
    case 'BlankNode':
      object = dataFactory.blankNode(obj.object.value);
      break;
    case 'Literal':
      if (obj.object.language) {
        object = dataFactory.literal(obj.object.value, obj.object.language);
      } else if (obj.object.datatype) {
        object = dataFactory.literal(
          obj.object.value,
          dataFactory.namedNode(obj.object.datatype)
        );
      } else {
        object = dataFactory.literal(obj.object.value);
      }
      break;
    default:
      throw new Error(`Unknown object type: ${obj.object.type}`);
  }

  // Create graph
  const graph = obj.graph
    ? dataFactory.namedNode(obj.graph)
    : dataFactory.defaultGraph();

  return dataFactory.quad(subject, predicate, object, graph);
}
