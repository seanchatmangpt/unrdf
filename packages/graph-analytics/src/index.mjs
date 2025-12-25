/**
 * @unrdf/graph-analytics
 *
 * Advanced graph analytics for RDF knowledge graphs
 *
 * Features:
 * - RDF to graphlib conversion
 * - PageRank & centrality metrics
 * - Shortest paths & relationship discovery
 * - Community detection & clustering
 */

export {
  rdfToGraph,
  sparqlResultsToGraph,
  getGraphStats,
} from './converter/rdf-to-graph.mjs';

export {
  computePageRank,
  computeDegreeCentrality,
  computeBetweennessCentrality,
  getTopNodes,
} from './centrality/pagerank-analyzer.mjs';

export {
  findShortestPath,
  findAllPaths,
  findCommonNeighbors,
  findKHopNeighbors,
  discoverRelationshipChains,
} from './paths/relationship-finder.mjs';

export {
  detectCommunitiesLPA,
  detectCommunitiesModularity,
  findKCore,
  getCommunityStats,
} from './clustering/community-detector.mjs';

// Default export not needed - use named exports
