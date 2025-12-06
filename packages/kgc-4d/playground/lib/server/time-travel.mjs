/**
 * Time-Travel Server Utilities
 *
 * Server-side wrappers for KGC 4D's time-travel functionality.
 * Handles WASM dependencies and provides a clean API for Next.js routes.
 */

import { reconstructState, GRAPHS } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';
import { getUniverse, getGitBackbone } from './universe.mjs';

/**
 * Reconstruct Universe state at a specific nanosecond timestamp
 *
 * @param {BigInt} targetTime - Target timestamp in nanoseconds
 * @param {Object} filters - Optional filters (subject, predicate, type)
 * @returns {Promise<Object>} Shard object with reconstructed quads
 */
export async function reconstructAtTime(targetTime, filters = {}) {
  // Get singletons
  const store = await getUniverse();
  const git = getGitBackbone();

  // Reconstruct state at target time
  const reconstructedStore = await reconstructState(store, git, targetTime);

  // Extract quads from reconstructed store
  const universeGraph = dataFactory.namedNode(GRAPHS.UNIVERSE);
  const quads = [...reconstructedStore.match(null, null, null, universeGraph)];

  // Apply optional filters
  let filteredQuads = quads;

  if (filters.subject) {
    filteredQuads = filteredQuads.filter((q) => q.subject.value === filters.subject);
  }

  if (filters.predicate) {
    filteredQuads = filteredQuads.filter((q) => q.predicate.value === filters.predicate);
  }

  if (filters.type) {
    const rdfType = dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
    const typeNode = dataFactory.namedNode(filters.type);
    const typeMatches = [...reconstructedStore.match(null, rdfType, typeNode, universeGraph)];
    const typedSubjects = new Set(typeMatches.map((q) => q.subject.value));
    filteredQuads = filteredQuads.filter((q) => typedSubjects.has(q.subject.value));
  }

  // Serialize quads
  const serializedQuads = filteredQuads.map((q) => ({
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

  // Format timestamp
  const timestamp = new Date(Number(targetTime / 1000000n)).toISOString();

  return {
    id: crypto.randomUUID(),
    t_ns: targetTime.toString(),
    timestamp_iso: timestamp,
    quad_count: serializedQuads.length,
    quads: serializedQuads,
  };
}
