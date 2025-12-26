import { Graph } from '@dagrejs/graphlib';
import { z } from 'zod';

/**
 * RDF to Graphlib Converter
 *
 * Converts RDF triple stores to graphlib Graph instances for analysis
 * CRITICAL: Foundation for all graph algorithms
 */

const ConversionOptionsSchema = z.object({
  directed: z.boolean().default(true),
  multigraph: z.boolean().default(false),
  compound: z.boolean().default(false),
  includePredicateAsEdgeLabel: z.boolean().default(true),
  nodeIdExtractor: z.function().optional(),
  edgeWeightExtractor: z.function().optional(),
}).passthrough();

/**
 * Convert RDF store to graphlib Graph
 *
 * @param {Object} store - Oxigraph store instance
 * @param {Object} options - Conversion options
 * @returns {Graph} Graphlib graph instance
 */
export function rdfToGraph(store, options = {}) {
  const opts = ConversionOptionsSchema.parse(options);

  const graph = new Graph({
    directed: opts.directed,
    multigraph: opts.multigraph,
    compound: opts.compound,
  });

  // Default node ID extractor - get URI value
  const getNodeId = opts.nodeIdExtractor || ((node) => {
    if (!node) return null;
    return node.value || node.id || node.toString();
  });

  // Default edge weight extractor - uniform weight of 1
  const getEdgeWeight = opts.edgeWeightExtractor || (() => 1);

  // Get all triples from store
  const quads = store.match();

  // Process each triple
  for (const quad of quads) {
    const subjectId = getNodeId(quad.subject);
    const objectId = getNodeId(quad.object);
    const predicateId = getNodeId(quad.predicate);

    if (!subjectId || !objectId) continue;

    // Add nodes if they don't exist
    if (!graph.hasNode(subjectId)) {
      graph.setNode(subjectId, {
        uri: subjectId,
        quad: quad.subject,
        type: quad.subject.termType || 'NamedNode',
      });
    }

    if (!graph.hasNode(objectId)) {
      graph.setNode(objectId, {
        uri: objectId,
        quad: quad.object,
        type: quad.object.termType || 'NamedNode',
      });
    }

    // Add edge
    // Only use edge label if multigraph is enabled
    const edgeLabel = (opts.includePredicateAsEdgeLabel && opts.multigraph) ? predicateId : undefined;
    const edgeWeight = getEdgeWeight(quad);

    graph.setEdge(subjectId, objectId, {
      predicate: predicateId,
      weight: edgeWeight,
      quad: quad,
    }, edgeLabel);
  }

  return graph;
}

/**
 * Convert RDF SPARQL results to graph
 *
 * @param {Array} sparqlResults - SPARQL SELECT results
 * @param {Object} options - Conversion options
 * @returns {Graph} Graphlib graph instance
 */
export function sparqlResultsToGraph(sparqlResults, options = {}) {
  const opts = ConversionOptionsSchema.parse(options);

  const graph = new Graph({
    directed: opts.directed,
    multigraph: opts.multigraph,
    compound: opts.compound,
  });

  for (const binding of sparqlResults) {
    // Expect bindings with ?s, ?p, ?o pattern
    const subject = binding.get('s') || binding.get('subject');
    const predicate = binding.get('p') || binding.get('predicate');
    const object = binding.get('o') || binding.get('object');

    if (!subject || !object) continue;

    const subjectId = subject.value;
    const objectId = object.value;
    const predicateId = predicate?.value || 'related';

    // Add nodes
    if (!graph.hasNode(subjectId)) {
      graph.setNode(subjectId, { uri: subjectId, term: subject });
    }

    if (!graph.hasNode(objectId)) {
      graph.setNode(objectId, { uri: objectId, term: object });
    }

    // Add edge
    const edgeLabel = opts.includePredicateAsEdgeLabel ? predicateId : undefined;
    graph.setEdge(subjectId, objectId, { predicate: predicateId }, edgeLabel);
  }

  return graph;
}

/**
 * Get graph statistics
 *
 * @param {Graph} graph - Graphlib graph
 * @returns {Object} Graph statistics
 */
export function getGraphStats(graph) {
  const nodes = graph.nodes();
  const edges = graph.edges();

  // Calculate degree distribution
  const degrees = nodes.map(n => graph.nodeEdges(n)?.length || 0);
  const avgDegree = degrees.reduce((a, b) => a + b, 0) / nodes.length;
  const maxDegree = Math.max(...degrees);

  return {
    nodeCount: nodes.length,
    edgeCount: edges.length,
    averageDegree: avgDegree,
    maxDegree: maxDegree,
    density: edges.length / (nodes.length * (nodes.length - 1)),
    isDirected: graph.isDirected(),
    isMultigraph: graph.isMultigraph(),
  };
}

export default {
  rdfToGraph,
  sparqlResultsToGraph,
  getGraphStats,
};
