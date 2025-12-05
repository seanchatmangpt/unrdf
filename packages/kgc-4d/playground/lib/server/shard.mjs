/**
 * KGC-4D Shard Projection - Server-Side mu_out(O)
 *
 * A Shard is a projection of the Universe - a filtered, minimal view
 * sent to a Browser client. It contains only what the client needs.
 *
 * This is the "Check-Out" operation in the Shard-Based Architecture.
 */

import { getUniverse, GRAPHS, dataFactory, now, VectorClock } from './universe.mjs';

/**
 * Project a Shard from the Universe based on a query pattern
 *
 * @param {Object} options
 * @param {string} [options.subject] - Filter by subject IRI
 * @param {string} [options.predicate] - Filter by predicate IRI
 * @param {string} [options.type] - Filter by rdf:type
 * @param {string} [options.belongsTo] - Filter by belongsTo relationship
 * @param {string} [options.sparql] - Raw SPARQL query (advanced)
 * @returns {Promise<Object>} Shard object with quads and metadata
 */
export async function projectShard(options = {}) {
  const store = await getUniverse();
  const universeGraph = dataFactory.namedNode(GRAPHS.UNIVERSE);

  let quads = [];

  // Pattern-based filtering
  const subjectNode = options.subject ? dataFactory.namedNode(options.subject) : null;
  const predicateNode = options.predicate ? dataFactory.namedNode(options.predicate) : null;

  // Get all matching quads from Universe graph
  const matches = [...store.match(subjectNode, predicateNode, null, universeGraph)];

  // Additional filtering by type
  if (options.type) {
    const typeNode = dataFactory.namedNode(options.type);
    const rdfType = dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');

    // Find all subjects of the requested type
    const typeMatches = [...store.match(null, rdfType, typeNode, universeGraph)];
    const typedSubjects = new Set(typeMatches.map((q) => q.subject.value));

    // Get all quads for typed subjects
    for (const subj of typedSubjects) {
      const subjNode = dataFactory.namedNode(subj);
      const subjQuads = [...store.match(subjNode, null, null, universeGraph)];
      quads.push(...subjQuads);
    }
  }

  // Additional filtering by belongsTo relationship
  if (options.belongsTo) {
    const belongsToNode = dataFactory.namedNode(options.belongsTo);
    const belongsToPred = dataFactory.namedNode('http://kgc.io/ontology/belongsTo');

    // Find all subjects that belong to the target
    const belongsMatches = [...store.match(null, belongsToPred, belongsToNode, universeGraph)];
    const relatedSubjects = new Set(belongsMatches.map((q) => q.subject.value));

    // Get all quads for related subjects
    for (const subj of relatedSubjects) {
      const subjNode = dataFactory.namedNode(subj);
      const subjQuads = [...store.match(subjNode, null, null, universeGraph)];
      quads.push(...subjQuads);
    }

    // Also include the target entity itself
    const targetQuads = [...store.match(belongsToNode, null, null, universeGraph)];
    quads.push(...targetQuads);
  }

  // If no specific filters, return pattern matches
  if (!options.type && !options.belongsTo) {
    quads = matches;
  }

  // Deduplicate quads
  const seen = new Set();
  quads = quads.filter((q) => {
    const key = `${q.subject.value}|${q.predicate.value}|${q.object.value}`;
    if (seen.has(key)) return false;
    seen.add(key);
    return true;
  });

  // Serialize quads for transmission
  const serializedQuads = quads.map((q) => ({
    subject: {
      value: q.subject.value,
      termType: q.subject.termType,
    },
    predicate: {
      value: q.predicate.value,
      termType: q.predicate.termType,
    },
    object: {
      value: q.object.value,
      termType: q.object.termType,
      datatype: q.object.datatype?.value,
      language: q.object.language,
    },
    graph: {
      value: q.graph.value,
      termType: q.graph.termType,
    },
  }));

  // Build Shard metadata
  const t_ns = now();
  const shardId = crypto.randomUUID();

  return {
    id: shardId,
    t_ns: t_ns.toString(),
    timestamp_iso: new Date().toISOString(),
    vector_clock: store.vectorClock.toJSON(),
    query: options,
    quad_count: serializedQuads.length,
    quads: serializedQuads,
  };
}

/**
 * Project the full Universe state (for debugging/admin)
 * WARNING: This can be large in production!
 */
export async function projectFullUniverse() {
  return projectShard({});
}

/**
 * Get Universe statistics (quad counts, event counts, etc.)
 */
export async function getUniverseStats() {
  const store = await getUniverse();
  const universeGraph = dataFactory.namedNode(GRAPHS.UNIVERSE);
  const eventLogGraph = dataFactory.namedNode(GRAPHS.EVENT_LOG);
  const systemGraph = dataFactory.namedNode(GRAPHS.SYSTEM);

  const universeQuads = [...store.match(null, null, null, universeGraph)];
  const eventLogQuads = [...store.match(null, null, null, eventLogGraph)];
  const systemQuads = [...store.match(null, null, null, systemGraph)];

  // Count unique subjects (entities)
  const subjects = new Set(universeQuads.map((q) => q.subject.value));

  // Count by type
  const rdfType = dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
  const typeQuads = [...store.match(null, rdfType, null, universeGraph)];
  const typeCounts = {};
  for (const q of typeQuads) {
    const type = q.object.value.split('/').pop();
    typeCounts[type] = (typeCounts[type] || 0) + 1;
  }

  return {
    universe: {
      quad_count: universeQuads.length,
      entity_count: subjects.size,
      types: typeCounts,
    },
    event_log: {
      quad_count: eventLogQuads.length,
      event_count: store.getEventCount(),
    },
    system: {
      quad_count: systemQuads.length,
    },
    vector_clock: store.vectorClock.toJSON(),
    timestamp: new Date().toISOString(),
  };
}
