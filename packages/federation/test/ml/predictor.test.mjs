/**
 * @file ML Predictor Tests
 * @vitest-environment jsdom
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  createPredictor,
  getGlobalPredictor,
} from '@unrdf/federation/ml/predictor';

describe('ML Predictor', () => {
  let predictor;

  beforeEach(() => {
    predictor = createPredictor({
      confidenceThreshold: 0.95,
      maxHistorySize: 1000,
      minSamplesForTraining: 10,
    });
  });

  afterEach(() => {
    predictor.clear();
  });

  describe('Feature Extraction', () => {
    it('should extract features from simple SELECT query', () => {
      const sparql = `
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT ?s WHERE {
          ?s rdf:type <http://example.org/Person> .
        }
      `;

      const federationState = {
        totalPeers: 5,
        healthyPeers: 5,
        degradedPeers: 0,
        avgPeerLatency: 50,
        minPeerLatency: 30,
        maxPeerLatency: 70,
        concurrentQueries: 2,
      };

      const features = predictor.extractFeatures(sparql, federationState);

      expect(features.queryType).toBe('SELECT');
      expect(features.variableCount).toBe(1);
      expect(features.hasFilter).toBe(false);
      expect(features.hasGroupBy).toBe(false);
      expect(features.healthyPeerCount).toBe(5);
    });

    it('should extract features from complex query with FILTER', () => {
      const sparql = `
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT ?s ?name WHERE {
          ?s rdf:type <http://example.org/Person> .
          ?s <http://example.org/name> ?name .
          FILTER(?name = "Alice")
        }
      `;

      const federationState = {
        totalPeers: 3,
        healthyPeers: 2,
        degradedPeers: 1,
        avgPeerLatency: 100,
        minPeerLatency: 50,
        maxPeerLatency: 200,
      };

      const features = predictor.extractFeatures(sparql, federationState);

      expect(features.variableCount).toBe(2);
      expect(features.hasFilter).toBe(true);
      expect(features.healthyPeerCount).toBe(2);
      expect(features.totalPeerCount).toBe(3);
    });

    it('should extract time-based features', () => {
      const sparql = 'SELECT ?s WHERE { ?s a :b }';
      const federationState = { totalPeers: 2, healthyPeers: 2 };

      const features = predictor.extractFeatures(sparql, federationState);

      expect(features.timeOfDay).toBeGreaterThanOrEqual(0);
      expect(features.timeOfDay).toBeLessThan(24);
      expect(features.dayOfWeek).toBeGreaterThanOrEqual(0);
      expect(features.dayOfWeek).toBeLessThanOrEqual(6);
    });
  });

  describe('Prediction', () => {
    it('should recommend bypass for healthy federation', () => {
      const features = {
        queryLength: 100,
        queryType: 'SELECT',
        variableCount: 1,
        hasFilter: false,
        hasOptional: false,
        hasUnion: false,
        hasGroupBy: false,
        hasOrderBy: false,
        hasLimit: false,
        tripleCount: 1,
        healthyPeerCount: 5,
        totalPeerCount: 5,
        degradedPeerCount: 0,
        avgPeerLatency: 50,
        minPeerLatency: 30,
        maxPeerLatency: 70,
        recentSuccessRate: 0.98,
        recentAvgLatency: 100,
        recentErrorRate: 0.01,
        currentConcurrency: 1,
        timeOfDay: 10,
        dayOfWeek: 2,
      };

      const prediction = predictor.predict(features);

      console.log('Prediction:', JSON.stringify(prediction, null, 2));

      expect(prediction.shouldBypass).toBe(true);
      expect(prediction.confidence).toBeGreaterThanOrEqual(0.95);
      expect(prediction.recommendedStrategy).toBe('first-available');
    });

    it('should not recommend bypass for unhealthy federation', () => {
      const features = {
        queryLength: 100,
        queryType: 'SELECT',
        variableCount: 1,
        hasFilter: false,
        hasOptional: false,
        hasUnion: false,
        hasGroupBy: false,
        hasOrderBy: false,
        hasLimit: false,
        tripleCount: 1,
        healthyPeerCount: 1,
        totalPeerCount: 5,
        degradedPeerCount: 4,
        avgPeerLatency: 500,
        minPeerLatency: 200,
        maxPeerLatency: 1000,
        recentSuccessRate: 0.6,
        recentAvgLatency: 800,
        recentErrorRate: 0.4,
        currentConcurrency: 50,
        timeOfDay: 14,
        dayOfWeek: 3,
      };

      const prediction = predictor.predict(features);

      expect(prediction.shouldBypass).toBe(false);
      expect(prediction.confidence).toBeLessThan(0.95);
    });

    it('should recommend single-peer strategy when only one peer', () => {
      const features = {
        queryLength: 100,
        queryType: 'SELECT',
        variableCount: 1,
        hasFilter: false,
        hasOptional: false,
        hasUnion: false,
        hasGroupBy: false,
        hasOrderBy: false,
        hasLimit: false,
        tripleCount: 1,
        healthyPeerCount: 1,
        totalPeerCount: 1,
        degradedPeerCount: 0,
        avgPeerLatency: 50,
        minPeerLatency: 50,
        maxPeerLatency: 50,
        recentSuccessRate: 0.95,
        recentAvgLatency: 100,
        recentErrorRate: 0.02,
        currentConcurrency: 1,
        timeOfDay: 10,
        dayOfWeek: 2,
      };

      const prediction = predictor.predict(features);

      expect(prediction.recommendedStrategy).toBe('single-peer');
    });

    it('should recommend selective strategy for complex queries', () => {
      const features = {
        queryLength: 200,
        queryType: 'SELECT',
        variableCount: 3,
        hasFilter: true,
        hasOptional: false,
        hasUnion: false,
        hasGroupBy: true,
        hasOrderBy: true,
        hasLimit: false,
        tripleCount: 2,
        healthyPeerCount: 5,
        totalPeerCount: 5,
        degradedPeerCount: 0,
        avgPeerLatency: 50,
        minPeerLatency: 30,
        maxPeerLatency: 70,
        recentSuccessRate: 0.95,
        recentAvgLatency: 100,
        recentErrorRate: 0.02,
        currentConcurrency: 1,
        timeOfDay: 10,
        dayOfWeek: 2,
      };

      const prediction = predictor.predict(features);

      expect(prediction.recommendedStrategy).toBe('selective');
    });

    it('should predict latency based on historical data', () => {
      // First, record some historical outcomes
      for (let i = 0; i < 10; i++) {
        const features = {
          queryLength: 100 + i * 10,
          queryType: 'SELECT',
          variableCount: 1,
          hasFilter: false,
          hasOptional: false,
          hasUnion: false,
          hasGroupBy: false,
          hasOrderBy: false,
          hasLimit: false,
          tripleCount: 1,
          healthyPeerCount: 5,
          totalPeerCount: 5,
          degradedPeerCount: 0,
          avgPeerLatency: 50,
          minPeerLatency: 30,
          maxPeerLatency: 70,
          recentSuccessRate: 1,
          recentAvgLatency: 100,
          recentErrorRate: 0,
          currentConcurrency: 1,
          timeOfDay: 10,
          dayOfWeek: 2,
        };

        predictor.recordOutcome(features, {
          success: true,
          latency: 100 + i * 10,
          strategy: 'broadcast',
        });
      }

      const features = {
        queryLength: 105,
        queryType: 'SELECT',
        variableCount: 1,
        hasFilter: false,
        hasOptional: false,
        hasUnion: false,
        hasGroupBy: false,
        hasOrderBy: false,
        hasLimit: false,
        tripleCount: 1,
        healthyPeerCount: 5,
        totalPeerCount: 5,
        degradedPeerCount: 0,
        avgPeerLatency: 50,
        minPeerLatency: 30,
        maxPeerLatency: 70,
        recentSuccessRate: 1,
        recentAvgLatency: 100,
        recentErrorRate: 0,
        currentConcurrency: 1,
        timeOfDay: 10,
        dayOfWeek: 2,
      };

      const prediction = predictor.predict(features);

      expect(prediction.predictedLatency).toBeGreaterThan(0);
      expect(prediction.predictedLatency).toBeLessThan(500);
    });
  });

  describe('Training', () => {
    it('should not train without sufficient samples', () => {
      const result = predictor.train();

      expect(result.trained).toBe(false);
      expect(result.reason).toBe('insufficient_samples');
      expect(result.samples).toBe(0);
    });

    it('should train with sufficient samples', () => {
      // Add training samples
      for (let i = 0; i < 100; i++) {
        const features = {
          queryLength: 100,
          queryType: 'SELECT',
          variableCount: 1,
          hasFilter: false,
          hasOptional: false,
          hasUnion: false,
          hasGroupBy: false,
          hasOrderBy: false,
          hasLimit: false,
          tripleCount: 1,
          healthyPeerCount: 5,
          totalPeerCount: 5,
          degradedPeerCount: 0,
          avgPeerLatency: 50,
          minPeerLatency: 30,
          maxPeerLatency: 70,
          recentSuccessRate: 0.95,
          recentAvgLatency: 100,
          recentErrorRate: 0.02,
          currentConcurrency: 1,
          timeOfDay: 10,
          dayOfWeek: 2,
        };

        predictor.recordOutcome(features, {
          success: i < 90, // 90% success rate
          latency: 100,
          strategy: 'broadcast',
        });
      }

      const result = predictor.train();

      expect(result.trained).toBe(true);
      expect(result.samples).toBe(100);
      expect(result.metrics).toBeDefined();
      expect(result.metrics.accuracy).toBeGreaterThan(0);
    });

    it('should calculate training metrics correctly', () => {
      // Add samples with known outcomes
      for (let i = 0; i < 50; i++) {
        const features = {
          queryLength: 100,
          queryType: 'SELECT',
          variableCount: 1,
          hasFilter: false,
          hasOptional: false,
          hasUnion: false,
          hasGroupBy: false,
          hasOrderBy: false,
          hasLimit: false,
          tripleCount: 1,
          healthyPeerCount: 5,
          totalPeerCount: 5,
          degradedPeerCount: 0,
          avgPeerLatency: 50,
          minPeerLatency: 30,
          maxPeerLatency: 70,
          recentSuccessRate: 1,
          recentAvgLatency: 100,
          recentErrorRate: 0,
          currentConcurrency: 1,
          timeOfDay: 10,
          dayOfWeek: 2,
        };

        predictor.recordOutcome(features, {
          success: true,
          latency: 100,
          strategy: 'broadcast',
        });
      }

      const result = predictor.train();

      expect(result.metrics.accuracy).toBeCloseTo(1, 1);
      expect(result.metrics.avgConfidence).toBeGreaterThan(0);
    });
  });

  describe('Learning', () => {
    it('should reinforce successful features', () => {
      const features = {
        queryLength: 100,
        queryType: 'SELECT',
        variableCount: 1,
        hasFilter: false,
        healthyPeerCount: 5,
        totalPeerCount: 5,
        degradedPeerCount: 0,
        avgPeerLatency: 50,
        minPeerLatency: 30,
        maxPeerLatency: 70,
        recentSuccessRate: 1,
        recentAvgLatency: 100,
        recentErrorRate: 0,
        currentConcurrency: 1,
        timeOfDay: 10,
        dayOfWeek: 2,
      };

      const statsBefore = predictor.getStats();

      predictor.recordOutcome(features, {
        success: true,
        latency: 50,
        strategy: 'broadcast',
      });

      const statsAfter = predictor.getStats();

      expect(statsAfter.historySize).toBe(statsBefore.historySize + 1);
    });

    it('should penalize failed features', () => {
      const features = {
        queryLength: 100,
        queryType: 'SELECT',
        variableCount: 1,
        hasFilter: false,
        healthyPeerCount: 1,
        totalPeerCount: 5,
        degradedPeerCount: 4,
        avgPeerLatency: 500,
        minPeerLatency: 200,
        maxPeerLatency: 1000,
        recentSuccessRate: 0.5,
        recentAvgLatency: 500,
        recentErrorRate: 0.5,
        currentConcurrency: 50,
        timeOfDay: 10,
        dayOfWeek: 2,
      };

      predictor.recordOutcome(features, {
        success: false,
        latency: 5000,
        strategy: 'broadcast',
      });

      const stats = predictor.getStats();

      expect(stats.historySize).toBe(1);
      expect(stats.successRate).toBe(0);
    });
  });

  describe('Statistics', () => {
    it('should return predictor statistics', () => {
      const stats = predictor.getStats();

      expect(stats.historySize).toBe(0);
      expect(stats.isTrained).toBe(false);
      expect(stats.featureWeights).toBeDefined();
    });

    it('should calculate success rate correctly', () => {
      // Add successful outcomes
      for (let i = 0; i < 8; i++) {
        predictor.recordOutcome(
          {
            queryLength: 100,
            queryType: 'SELECT',
            variableCount: 1,
            hasFilter: false,
            healthyPeerCount: 5,
            totalPeerCount: 5,
            degradedPeerCount: 0,
            avgPeerLatency: 50,
            minPeerLatency: 30,
            maxPeerLatency: 70,
            recentSuccessRate: 1,
            recentAvgLatency: 100,
            recentErrorRate: 0,
            currentConcurrency: 1,
            timeOfDay: 10,
            dayOfWeek: 2,
          },
          {
            success: true,
            latency: 100,
            strategy: 'broadcast',
          }
        );
      }

      // Add failed outcomes
      for (let i = 0; i < 2; i++) {
        predictor.recordOutcome(
          {
            queryLength: 100,
            queryType: 'SELECT',
            variableCount: 1,
            hasFilter: false,
            healthyPeerCount: 5,
            totalPeerCount: 5,
            degradedPeerCount: 0,
            avgPeerLatency: 50,
            minPeerLatency: 30,
            maxPeerLatency: 70,
            recentSuccessRate: 1,
            recentAvgLatency: 100,
            recentErrorRate: 0,
            currentConcurrency: 1,
            timeOfDay: 10,
            dayOfWeek: 2,
          },
          {
            success: false,
            latency: 500,
            strategy: 'broadcast',
          }
        );
      }

      const stats = predictor.getStats();

      expect(stats.historySize).toBe(10);
      expect(stats.successRate).toBeCloseTo(0.8, 1);
      expect(stats.avgLatency).toBeCloseTo(100, 10); // Only successful samples: 8 * 100 / 8
    });
  });

  describe('History Management', () => {
    it('should export and import history', () => {
      const features = {
        queryLength: 100,
        queryType: 'SELECT',
        variableCount: 1,
        hasFilter: false,
        healthyPeerCount: 5,
        totalPeerCount: 5,
        degradedPeerCount: 0,
        avgPeerLatency: 50,
        minPeerLatency: 30,
        maxPeerLatency: 70,
        recentSuccessRate: 1,
        recentAvgLatency: 100,
        recentErrorRate: 0,
        currentConcurrency: 1,
        timeOfDay: 10,
        dayOfWeek: 2,
      };

      predictor.recordOutcome(features, {
        success: true,
        latency: 100,
        strategy: 'broadcast',
      });

      const exported = predictor.exportHistory();

      expect(exported).toHaveLength(1);
      expect(exported[0].features).toBeDefined();
      expect(exported[0].outcome).toBeDefined();

      // Create new predictor and import
      const newPredictor = createPredictor();
      newPredictor.importHistory(exported);

      const newStats = newPredictor.getStats();
      expect(newStats.historySize).toBe(1);
    });

    it('should clear history', () => {
      const features = {
        queryLength: 100,
        queryType: 'SELECT',
        variableCount: 1,
        hasFilter: false,
        healthyPeerCount: 5,
        totalPeerCount: 5,
        degradedPeerCount: 0,
        avgPeerLatency: 50,
        minPeerLatency: 30,
        maxPeerLatency: 70,
        recentSuccessRate: 1,
        recentAvgLatency: 100,
        recentErrorRate: 0,
        currentConcurrency: 1,
        timeOfDay: 10,
        dayOfWeek: 2,
      };

      predictor.recordOutcome(features, {
        success: true,
        latency: 100,
        strategy: 'broadcast',
      });

      expect(predictor.getStats().historySize).toBe(1);

      predictor.clear();

      expect(predictor.getStats().historySize).toBe(0);
    });

    it('should trim history when exceeding max size', () => {
      const smallPredictor = createPredictor({ maxHistorySize: 5 });

      // Add 10 samples
      for (let i = 0; i < 10; i++) {
        const features = {
          queryLength: 100 + i,
          queryType: 'SELECT',
          variableCount: 1,
          hasFilter: false,
          healthyPeerCount: 5,
          totalPeerCount: 5,
          degradedPeerCount: 0,
          avgPeerLatency: 50,
          minPeerLatency: 30,
          maxPeerLatency: 70,
          recentSuccessRate: 1,
          recentAvgLatency: 100,
          recentErrorRate: 0,
          currentConcurrency: 1,
          timeOfDay: 10,
          dayOfWeek: 2,
        };

        smallPredictor.recordOutcome(features, {
          success: true,
          latency: 100,
          strategy: 'broadcast',
        });
      }

      const stats = smallPredictor.getStats();

      expect(stats.historySize).toBe(5); // Should be trimmed to max
    });
  });

  describe('Global Predictor', () => {
    it('should return same instance for global predictor', () => {
      const predictor1 = getGlobalPredictor();
      const predictor2 = getGlobalPredictor();

      expect(predictor1).toBe(predictor2);
    });

    it('should use default config for global predictor after first call', () => {
      // Get existing global predictor (already created by previous test)
      const customPredictor = getGlobalPredictor();

      const stats = customPredictor.getStats();

      // Uses default config (0.95 threshold) since it was created before
      expect(stats.confidenceThreshold).toBeDefined();
      expect(stats.historySize).toBeGreaterThanOrEqual(0);
    });
  });

  describe('Error Handling', () => {
    it('should return safe default on prediction error', () => {
      const invalidFeatures = {
        queryLength: -1, // Invalid
        queryType: 'INVALID',
        variableCount: -1,
      };

      const prediction = predictor.predict(invalidFeatures);

      expect(prediction.shouldBypass).toBe(false);
      expect(prediction.confidence).toBe(0);
      expect(prediction.reasoning).toContain('Prediction failed');
    });

    it('should handle malformed SPARQL queries', () => {
      const malformedSparql = 'NOT A VALID SPARQL QUERY';

      const features = predictor.extractFeatures(malformedSparql, {
        totalPeers: 5,
        healthyPeers: 5,
      });

      // Should still extract basic features
      expect(features.queryLength).toBe(malformedSparql.length);
      expect(features.queryType).toBe('SELECT'); // Default
    });
  });

  describe('Performance Benchmarks', () => {
    it('should predict in under 1ms', () => {
      const features = {
        queryLength: 100,
        queryType: 'SELECT',
        variableCount: 1,
        hasFilter: false,
        healthyPeerCount: 5,
        totalPeerCount: 5,
        degradedPeerCount: 0,
        avgPeerLatency: 50,
        minPeerLatency: 30,
        maxPeerLatency: 70,
        recentSuccessRate: 0.95,
        recentAvgLatency: 100,
        recentErrorRate: 0.02,
        currentConcurrency: 1,
        timeOfDay: 10,
        dayOfWeek: 2,
      };

      const start = performance.now();
      for (let i = 0; i < 1000; i++) {
        predictor.predict(features);
      }
      const duration = performance.now() - start;

      const avgTime = duration / 1000;

      console.log(`Prediction benchmark: 1000 predictions in ${duration.toFixed(2)}ms`);
      console.log(`Average time per prediction: ${avgTime.toFixed(4)}ms`);

      expect(avgTime).toBeLessThan(1); // < 1ms per prediction
    });

    it('should train efficiently', () => {
      // Add training samples
      for (let i = 0; i < 100; i++) {
        const features = {
          queryLength: 100,
          queryType: 'SELECT',
          variableCount: 1,
          hasFilter: false,
          healthyPeerCount: 5,
          totalPeerCount: 5,
          degradedPeerCount: 0,
          avgPeerLatency: 50,
          minPeerLatency: 30,
          maxPeerLatency: 70,
          recentSuccessRate: 0.95,
          recentAvgLatency: 100,
          recentErrorRate: 0.02,
          currentConcurrency: 1,
          timeOfDay: 10,
          dayOfWeek: 2,
        };

        predictor.recordOutcome(features, {
          success: true,
          latency: 100,
          strategy: 'broadcast',
        });
      }

      const start = performance.now();
      const result = predictor.train();
      const duration = performance.now() - start;

      console.log(`Training benchmark: trained ${result.samples} samples in ${duration.toFixed(2)}ms`);

      expect(result.trained).toBe(true);
      expect(duration).toBeLessThan(100); // Should train in under 100ms
    });
  });
});
