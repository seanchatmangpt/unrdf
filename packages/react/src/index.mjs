/**
 * @unrdf/react
 *
 * AI Semantic Analysis Tools for RDF Knowledge Graphs
 *
 * Provides AI-powered analysis capabilities for:
 * - Semantic analysis and concept extraction (SemanticAnalyzer)
 * - Graph embeddings with TransE/ComplEx/RotatE (EmbeddingsManager)
 * - Natural language to SPARQL query building (NLPQueryBuilder)
 * - Anomaly detection for data quality (AnomalyDetector)
 *
 * @module @unrdf/react
 */

// AI Semantic Analysis
export {
  SemanticAnalyzer,
  createSemanticAnalyzer,
  defaultSemanticAnalyzer,
} from './ai-semantic/semantic-analyzer.mjs';

export {
  EmbeddingsManager,
  createEmbeddingsManager,
  defaultEmbeddingsManager,
} from './ai-semantic/embeddings-manager.mjs';

export {
  NLPQueryBuilder,
  createNLPQueryBuilder,
  defaultNLPQueryBuilder,
} from './ai-semantic/nlp-query-builder.mjs';

export {
  AnomalyDetector,
  createAnomalyDetector,
  defaultAnomalyDetector,
} from './ai-semantic/anomaly-detector.mjs';
