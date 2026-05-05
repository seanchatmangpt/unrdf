/**
 * @file RDF graph and ontology diff utilities for UNRDF.
 * @module diff
 */

import { z } from 'zod';

/**
 * @typedef {import('n3').Quad} Quad
 * @typedef {import('n3').Store} Store
 */

/**
 * Minimal RDF/JS store-like interface.
 * N3.Store conforms to this.
 *
 * @typedef {Object} RdfStoreLike
 * @property {function(...any): Quad[]} getQuads
 */

/**
 * A simple triple representation used for diffs.
 *
 * @typedef {Object} DiffTriple
 * @property {string} subject
 * @property {string} predicate
 * @property {string} object
 */

/**
 * Triple-level graph diff.
 *
 * @typedef {Object} GraphDiff
 * @property {DiffTriple[]} added
 * @property {DiffTriple[]} removed
 */

/**
 * Ontology-level change description.
 *
 * @typedef {Object} OntologyChange
 * @property {string} kind          - Application-level change kind, e.g. 'FeatureAdded', 'RoleRemoved'
 * @property {string} [entity]      - IRI or identifier of the entity/feature
 * @property {string} [role]        - Role name, e.g. 'hasService', 'hasTest'
 * @property {any} [details]        - Optional structured details for UIs / logs
 */

/**
 * Combined graph + ontology diff.
 *
 * @typedef {Object} OntologyDiff
 * @property {GraphDiff} triples
 * @property {OntologyChange[]} changes
 */

/**
 * Ontology lens: maps low-level triple changes to ontology-level changes.
 *
 * @callback OntologyLensFn
 * @param {DiffTriple} triple
 * @param {'added' | 'removed'} direction
 * @returns {OntologyChange | null}
 */

/* ========================================================================= */
/* Zod Schemas                                                              */
/* ========================================================================= */

export const DiffTripleSchema = z.object({
  subject: z.string(),
  predicate: z.string(),
  object: z.string(),
});

export const GraphDiffSchema = z.object({
  added: z.array(DiffTripleSchema),
  removed: z.array(DiffTripleSchema),
});

export const OntologyChangeSchema = z
  .object({
    kind: z.string(),
    entity: z.string().optional(),
    role: z.string().optional(),
  })
  .passthrough();

export const OntologyDiffSchema = z.object({
  triples: GraphDiffSchema,
  changes: z.array(OntologyChangeSchema),
});

/**
 * Local Quad-like schema for deltas.
 * This intentionally mirrors N3 / RDF-JS shape without importing your global schemas.
 */
const QuadLikeSchema = z.object({
  subject: z.object({ value: z.string() }),
  predicate: z.object({ value: z.string() }),
  object: z.object({ value: z.string() }),
  graph: z.object({ value: z.string() }).optional(),
});

/**
 * Local Delta schema (additions/removals of quads).
 * You can swap this with your global DeltaSchema if desired.
 */
export const DeltaLikeSchema = z.object({
  additions: z.array(QuadLikeSchema),
  removals: z.array(QuadLikeSchema),
});

/* ========================================================================= */
/* Internal helpers                                                         */
/* ========================================================================= */

/**
 * Convert an N3/RDF-JS quad to a DiffTriple.
 *
 * @param {Quad} quad
 * @returns {DiffTriple}
 */
export function quadToDiffTriple(quad) {
  return {
    subject: quad.subject.value,
    predicate: quad.predicate.value,
    object: quad.object.value,
  };
}

/**
 * Create a stable string key for a DiffTriple.
 * This is used for set membership / equality.
 *
 * NOTE: This does not attempt blank-node canonicalization. If you rely
 * heavily on blank nodes, you may want to normalize them before diffing.
 *
 * @param {DiffTriple} t
 * @returns {string}
 */
export function diffTripleKey(t) {
  // Use a delimiter unlikely to appear in IRIs; whitespace is fine for most cases.
  return `${t.subject} ${t.predicate} ${t.object}`;
}

/**
 * Collect all quads from a store as DiffTriples.
 *
 * @param {RdfStoreLike} store
 * @returns {DiffTriple[]}
 */
export function collectDiffTriplesFromStore(store) {
  if (!store || typeof store.getQuads !== 'function') {
    throw new TypeError('collectDiffTriplesFromStore: store must implement getQuads()');
  }

  /** @type {Quad[]} */
  const quads = store.getQuads(null, null, null, null);
  return quads.map(quadToDiffTriple);
}

/**
 * Build a Set of triple keys from a list of DiffTriples.
 *
 * @param {DiffTriple[]} triples
 * @returns {Set<string>}
 */
function buildTripleKeySet(triples) {
  const set = new Set();
  for (const t of triples) {
    set.add(diffTripleKey(t));
  }
  return set;
}

/* ========================================================================= */
/* Public API: Graph diff                                                   */
/* ========================================================================= */

/**
 * Compute a triple-level diff between two RDF graphs.
 *
 * @param {RdfStoreLike} beforeStore - Graph before the change
 * @param {RdfStoreLike} afterStore  - Graph after the change
 * @returns {GraphDiff}
 */
export function diffGraphFromStores(beforeStore, afterStore) {
  const beforeTriples = collectDiffTriplesFromStore(beforeStore);
  const afterTriples = collectDiffTriplesFromStore(afterStore);

  const beforeSet = buildTripleKeySet(beforeTriples);
  const afterSet = buildTripleKeySet(afterTriples);

  /** @type {DiffTriple[]} */
  const added = [];
  /** @type {DiffTriple[]} */
  const removed = [];

  for (const t of afterTriples) {
    const key = diffTripleKey(t);
    if (!beforeSet.has(key)) {
      added.push(t);
    }
  }

  for (const t of beforeTriples) {
    const key = diffTripleKey(t);
    if (!afterSet.has(key)) {
      removed.push(t);
    }
  }

  return GraphDiffSchema.parse({ added, removed });
}

/**
 * Compute a triple-level diff directly from a Delta (additions/removals).
 * This is useful inside the transaction layer where you already have a Delta.
 *
 * @param {import('zod').infer<typeof DeltaLikeSchema>} delta
 * @returns {GraphDiff}
 */
export function diffGraphFromDelta(delta) {
  const validated = DeltaLikeSchema.parse(delta);

  /** @type {DiffTriple[]} */
  const added = validated.additions.map(quadToDiffTriple);
  /** @type {DiffTriple[]} */
  const removed = validated.removals.map(quadToDiffTriple);

  return GraphDiffSchema.parse({ added, removed });
}

/* ========================================================================= */
/* Public API: Ontology diff                                                */
/* ========================================================================= */

/**
 * Compute an ontology-level diff using an ontology lens and a GraphDiff.
 *
 * @param {GraphDiff} graphDiff
 * @param {OntologyLensFn} lens
 * @returns {OntologyDiff}
 */
export function diffOntologyFromGraphDiff(graphDiff, lens) {
  const validatedGraphDiff = GraphDiffSchema.parse(graphDiff);

  /** @type {OntologyChange[]} */
  const changes = [];

  for (const t of validatedGraphDiff.added) {
    const change = lens(t, 'added');
    if (change) {
      changes.push(OntologyChangeSchema.parse(change));
    }
  }

  for (const t of validatedGraphDiff.removed) {
    const change = lens(t, 'removed');
    if (change) {
      changes.push(OntologyChangeSchema.parse(change));
    }
  }

  return OntologyDiffSchema.parse({
    triples: validatedGraphDiff,
    changes,
  });
}

/**
 * Compute an ontology-level diff between two stores.
 *
 * @param {RdfStoreLike} beforeStore
 * @param {RdfStoreLike} afterStore
 * @param {OntologyLensFn} lens
 * @returns {OntologyDiff}
 */
export function diffOntologyFromStores(beforeStore, afterStore, lens) {
  const graphDiff = diffGraphFromStores(beforeStore, afterStore);
  return diffOntologyFromGraphDiff(graphDiff, lens);
}

/**
 * Compute an ontology-level diff directly from a Delta (additions/removals).
 * This is ideal for transactions where you already have the delta and don't
 * want to re-scan the store.
 *
 * @param {import('zod').infer<typeof DeltaLikeSchema>} delta
 * @param {OntologyLensFn} lens
 * @returns {OntologyDiff}
 */
export function diffOntologyFromDelta(delta, lens) {
  const graphDiff = diffGraphFromDelta(delta);
  return diffOntologyFromGraphDiff(graphDiff, lens);
}

/* ========================================================================= */
/* Convenience: change summarization                                        */
/* ========================================================================= */

/**
 * Summarize ontology changes by kind.
 *
 * @param {OntologyDiff} ontologyDiff
 * @returns {Record<string, number>}
 */
export function summarizeChangesByKind(ontologyDiff) {
  const validated = OntologyDiffSchema.parse(ontologyDiff);
  /** @type {Record<string, number>} */
  const summary = {};

  for (const change of validated.changes) {
    summary[change.kind] = (summary[change.kind] || 0) + 1;
  }

  return summary;
}

/**
 * Summarize ontology changes for a specific entity.
 *
 * @param {OntologyDiff} ontologyDiff
 * @param {string} entityIri
 * @returns {OntologyChange[]}
 */
export function changesForEntity(ontologyDiff, entityIri) {
  const validated = OntologyDiffSchema.parse(ontologyDiff);
  return validated.changes.filter(c => c.entity === entityIri);
}
