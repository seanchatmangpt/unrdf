/**
 * @file AI Semantic Analysis Tests
 * @module test/ai-semantic
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  SemanticAnalyzer,
  createSemanticAnalyzer,
  defaultSemanticAnalyzer,
} from '../src/ai-semantic/semantic-analyzer.mjs';
import {
  EmbeddingsManager,
  createEmbeddingsManager,
  defaultEmbeddingsManager,
} from '../src/ai-semantic/embeddings-manager.mjs';
import {
  NLPQueryBuilder,
  createNLPQueryBuilder,
  defaultNLPQueryBuilder,
} from '../src/ai-semantic/nlp-query-builder.mjs';
import {
  AnomalyDetector,
  createAnomalyDetector,
  defaultAnomalyDetector,
} from '../src/ai-semantic/anomaly-detector.mjs';

describe('AI Semantic Analysis', () => {
  describe('SemanticAnalyzer', () => {
    it('should create instance with default config', () => {
      const analyzer = new SemanticAnalyzer();
      expect(analyzer).toBeDefined();
      expect(analyzer.config).toBeDefined();
    });

    it('should create instance with custom config', () => {
      const analyzer = new SemanticAnalyzer({ cacheSize: 500 });
      expect(analyzer.config.cacheSize).toBe(500);
    });

    it('should export factory function', () => {
      const analyzer = createSemanticAnalyzer();
      expect(analyzer).toBeInstanceOf(SemanticAnalyzer);
    });

    it('should export default instance', () => {
      expect(defaultSemanticAnalyzer).toBeInstanceOf(SemanticAnalyzer);
    });

    it('should have stats tracking', () => {
      const analyzer = new SemanticAnalyzer();
      const stats = analyzer.getStats();
      expect(stats).toBeDefined();
      expect(stats.analyses).toBe(0);
    });

    it('should clear cache', () => {
      const analyzer = new SemanticAnalyzer({ enableCache: true });
      analyzer.clearCache();
      expect(analyzer.getStats().cacheSize).toBe(0);
    });
  });

  describe('EmbeddingsManager', () => {
    it('should create instance with default config', () => {
      const manager = new EmbeddingsManager();
      expect(manager).toBeDefined();
      expect(manager.config.dimension).toBe(100);
      expect(manager.config.algorithm).toBe('TransE');
    });

    it('should create instance with custom config', () => {
      const manager = new EmbeddingsManager({
        dimension: 50,
        algorithm: 'ComplEx',
      });
      expect(manager.config.dimension).toBe(50);
      expect(manager.config.algorithm).toBe('ComplEx');
    });

    it('should export factory function', () => {
      const manager = createEmbeddingsManager();
      expect(manager).toBeInstanceOf(EmbeddingsManager);
    });

    it('should export default instance', () => {
      expect(defaultEmbeddingsManager).toBeInstanceOf(EmbeddingsManager);
    });

    it('should train on triples', async () => {
      const manager = new EmbeddingsManager({ dimension: 10 });
      const triples = [
        { subject: 'A', predicate: 'relatedTo', object: 'B' },
        { subject: 'B', predicate: 'relatedTo', object: 'C' },
        { subject: 'C', predicate: 'relatedTo', object: 'A' },
      ];

      const result = await manager.train(triples, { epochs: 10 });

      expect(result).toBeDefined();
      expect(result.epochs).toBe(10);
      expect(result.embeddingsCount).toBeGreaterThan(0);
      expect(result.duration).toBeGreaterThanOrEqual(0);
    });

    it('should get embeddings for entities', async () => {
      const manager = new EmbeddingsManager({ dimension: 10 });
      const triples = [
        { subject: 'A', predicate: 'relatedTo', object: 'B' },
      ];

      await manager.train(triples, { epochs: 5 });
      const embedding = manager.getEmbedding('A');

      expect(embedding).toBeDefined();
      expect(Array.isArray(embedding)).toBe(true);
      expect(embedding.length).toBe(10);
    });

    it('should predict link probability', async () => {
      const manager = new EmbeddingsManager({ dimension: 10 });
      const triples = [
        { subject: 'A', predicate: 'relatedTo', object: 'B' },
        { subject: 'B', predicate: 'relatedTo', object: 'C' },
      ];

      await manager.train(triples, { epochs: 10 });
      const score = manager.predictLink('A', 'relatedTo', 'C');

      expect(score).toBeDefined();
      expect(typeof score).toBe('number');
    });

    it('should find similar entities', async () => {
      const manager = new EmbeddingsManager({ dimension: 10 });
      const triples = [
        { subject: 'A', predicate: 'relatedTo', object: 'B' },
        { subject: 'B', predicate: 'relatedTo', object: 'C' },
        { subject: 'C', predicate: 'relatedTo', object: 'D' },
      ];

      await manager.train(triples, { epochs: 10 });
      const similar = manager.findSimilar('A', 2);

      expect(Array.isArray(similar)).toBe(true);
      expect(similar.length).toBeLessThanOrEqual(2);
    });

    it('should track statistics', async () => {
      const manager = new EmbeddingsManager();
      const triples = [
        { subject: 'A', predicate: 'relatedTo', object: 'B' },
      ];

      await manager.train(triples, { epochs: 5 });
      const stats = manager.getStats();

      expect(stats.trainings).toBe(1);
      expect(stats.entityCount).toBeGreaterThan(0);
    });
  });

  describe('NLPQueryBuilder', () => {
    it('should create instance with default config', () => {
      const builder = new NLPQueryBuilder();
      expect(builder).toBeDefined();
      expect(builder.config.confidenceThreshold).toBe(0.6);
    });

    it('should create instance with custom config', () => {
      const builder = new NLPQueryBuilder({ confidenceThreshold: 0.8 });
      expect(builder.config.confidenceThreshold).toBe(0.8);
    });

    it('should export factory function', () => {
      const builder = createNLPQueryBuilder();
      expect(builder).toBeInstanceOf(NLPQueryBuilder);
    });

    it('should export default instance', () => {
      expect(defaultNLPQueryBuilder).toBeInstanceOf(NLPQueryBuilder);
    });

    it('should build SPARQL from simple query', async () => {
      const builder = new NLPQueryBuilder();
      const result = await builder.buildQuery('find all Person');

      expect(result).toBeDefined();
      expect(result.sparql).toContain('SELECT');
      expect(result.intent).toBe('select');
      expect(result.confidence).toBeGreaterThan(0);
    });

    it('should detect ASK intent', async () => {
      const builder = new NLPQueryBuilder();
      const result = await builder.buildQuery('is Alice a Person');

      expect(result.intent).toBe('ask');
      expect(result.sparql).toContain('ASK');
    });

    it('should detect DESCRIBE intent', async () => {
      const builder = new NLPQueryBuilder();
      const result = await builder.buildQuery('describe Alice');

      expect(result.intent).toBe('describe');
      expect(result.sparql).toContain('DESCRIBE');
    });

    it('should extract entities from query', async () => {
      const builder = new NLPQueryBuilder();
      const result = await builder.buildQuery('find all Person with name Alice');

      expect(result.entities).toBeDefined();
      expect(Array.isArray(result.entities)).toBe(true);
    });

    it('should track statistics', async () => {
      const builder = new NLPQueryBuilder();
      await builder.buildQuery('find all Person');

      const stats = builder.getStats();
      expect(stats.queries).toBe(1);
      expect(stats.successful).toBe(1);
    });

    it('should cache queries', async () => {
      const builder = new NLPQueryBuilder({ enableCache: true });
      await builder.buildQuery('find all Person');
      await builder.buildQuery('find all Person'); // Same query

      const stats = builder.getStats();
      expect(stats.cacheHits).toBe(1);
    });
  });

  describe('AnomalyDetector', () => {
    let mockStore;

    beforeEach(() => {
      // Create a minimal mock store for testing
      mockStore = {
        size: 10,
        [Symbol.iterator]: function* () {
          yield {
            subject: { value: 'http://example.org/entity1', termType: 'NamedNode' },
            predicate: {
              value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
              termType: 'NamedNode',
            },
            object: { value: 'http://example.org/Person', termType: 'NamedNode' },
          };
          yield {
            subject: { value: 'http://example.org/entity2', termType: 'NamedNode' },
            predicate: {
              value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
              termType: 'NamedNode',
            },
            object: { value: 'http://example.org/Person', termType: 'NamedNode' },
          };
        },
      };
    });

    it('should create instance with default config', () => {
      const detector = new AnomalyDetector();
      expect(detector).toBeDefined();
      expect(detector.config.outlierThreshold).toBe(3.0);
    });

    it('should create instance with custom config', () => {
      const detector = new AnomalyDetector({ outlierThreshold: 2.5 });
      expect(detector.config.outlierThreshold).toBe(2.5);
    });

    it('should export factory function', () => {
      const detector = createAnomalyDetector();
      expect(detector).toBeInstanceOf(AnomalyDetector);
    });

    it('should export default instance', () => {
      expect(defaultAnomalyDetector).toBeInstanceOf(AnomalyDetector);
    });

    it('should detect anomalies in store', async () => {
      const detector = new AnomalyDetector();
      const result = await detector.detect(mockStore);

      expect(result).toBeDefined();
      expect(result.anomalies).toBeDefined();
      expect(Array.isArray(result.anomalies)).toBe(true);
      expect(result.statistics).toBeDefined();
      expect(result.statistics.totalEntities).toBeGreaterThanOrEqual(0);
    });

    it('should return severity breakdown', async () => {
      const detector = new AnomalyDetector();
      const result = await detector.detect(mockStore);

      expect(result.statistics.severityBreakdown).toBeDefined();
      expect(result.statistics.severityBreakdown.high).toBeGreaterThanOrEqual(0);
      expect(result.statistics.severityBreakdown.medium).toBeGreaterThanOrEqual(0);
      expect(result.statistics.severityBreakdown.low).toBeGreaterThanOrEqual(0);
    });

    it('should track statistics', async () => {
      const detector = new AnomalyDetector();
      await detector.detect(mockStore);

      const stats = detector.getStats();
      expect(stats.detections).toBe(1);
      expect(stats.anomaliesFound).toBeGreaterThanOrEqual(0);
    });

    it('should cache results', async () => {
      const detector = new AnomalyDetector({ enableCache: true });
      await detector.detect(mockStore);
      await detector.detect(mockStore); // Same store

      const stats = detector.getStats();
      expect(stats.cacheHits).toBe(1);
    });

    it('should clear cache', () => {
      const detector = new AnomalyDetector({ enableCache: true });
      detector.clearCache();
      expect(detector.getStats().cacheSize).toBe(0);
    });
  });
});
