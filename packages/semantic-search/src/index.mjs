/**
 * @unrdf/semantic-search - AI-powered semantic search over RDF knowledge graphs
 *
 * Features:
 * - Vector embeddings using transformer models
 * - Natural language queries over RDF
 * - Hybrid semantic + SPARQL search
 * - Knowledge discovery and recommendations
 * - Entity similarity and autocomplete
 */

export { RDFEmbedder } from './embeddings/rdf-embedder.mjs';
export { SemanticQueryEngine } from './search/semantic-query-engine.mjs';
export { KnowledgeRecommender } from './discovery/knowledge-recommender.mjs';

export default {
  RDFEmbedder,
  SemanticQueryEngine,
  KnowledgeRecommender,
};
