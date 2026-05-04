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

import { RDFEmbedder } from './embeddings/rdf-embedder.mjs';
import { SemanticQueryEngine } from './search/semantic-query-engine.mjs';
import { KnowledgeRecommender } from './discovery/knowledge-recommender.mjs';

export { RDFEmbedder, SemanticQueryEngine, KnowledgeRecommender };

export default {
  RDFEmbedder,
  SemanticQueryEngine,
  KnowledgeRecommender,
};
