/**
 * @file AI Semantic Analysis Module - Entry Point
 * @module ai-semantic
 *
 * @description
 * Entry point for the AI semantic analysis module, providing semantic analysis,
 * NL→SPARQL translation, graph embeddings, and anomaly detection for RDF graphs.
 *
 * @example
 * import { createSemanticAnalyzer, createNLPQueryBuilder } from './ai-semantic/index.mjs';
 *
 * const analyzer = createSemanticAnalyzer();
 * const result = await analyzer.analyze(store);
 *
 * const nlpBuilder = createNLPQueryBuilder();
 * const query = await nlpBuilder.buildQuery('List all people', store);
 */

export {
  SemanticAnalyzer,
  createSemanticAnalyzer,
  defaultSemanticAnalyzer,
} from './semantic-analyzer.mjs';

export {
  NLPQueryBuilder,
  createNLPQueryBuilder,
  defaultNLPQueryBuilder,
} from './nlp-query-builder.mjs';

export {
  EmbeddingsManager,
  createEmbeddingsManager,
  defaultEmbeddingsManager,
} from './embeddings-manager.mjs';

export {
  AnomalyDetector,
  createAnomalyDetector,
  defaultAnomalyDetector,
} from './anomaly-detector.mjs';
