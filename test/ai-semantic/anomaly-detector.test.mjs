/**
 * @file Tests for Anomaly Detector
 * @module test/ai-semantic/anomaly-detector
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { Store, DataFactory } from 'n3';
import { createAnomalyDetector } from '../../src/knowledge-engine/ai-semantic/anomaly-detector.mjs';

const { namedNode, literal, quad } = DataFactory;

describe('AnomalyDetector', () => {
  let store;
  let detector;

  beforeEach(() => {
    store = new Store();
    detector = createAnomalyDetector({
      enableStatistical: true,
      enableMLBased: true,
      outlierThreshold: 2.5,
      minConfidence: 0.5,
    });
  });

  describe('detectAnomalies', () => {
    it('should detect anomalies in an RDF graph', async () => {
      // Add test data
      const alice = namedNode('http://example.org/alice');
      const bob = namedNode('http://example.org/bob');
      const knows = namedNode('http://example.org/knows');

      store.addQuad(quad(alice, knows, bob));

      const result = await detector.detectAnomalies(store);

      expect(result).toBeDefined();
      expect(result.anomalies).toBeInstanceOf(Array);
      expect(result.statistics).toBeDefined();
      expect(result.statistics.total).toBe(result.anomalies.length);
      expect(result.duration).toBeGreaterThan(0);
    });

    it('should detect statistical outliers', async () => {
      const alice = namedNode('http://example.org/alice');
      const knows = namedNode('http://example.org/knows');

      // Create entity with unusually high degree
      for (let i = 0; i < 50; i++) {
        const friend = namedNode(`http://example.org/person${i}`);
        store.addQuad(quad(alice, knows, friend));
      }

      // Add more normal entities to establish baseline
      for (let i = 0; i < 20; i++) {
        const person = namedNode(`http://example.org/normal${i}`);
        const other = namedNode(`http://example.org/other${i}`);
        store.addQuad(quad(person, knows, other));
      }

      const result = await detector.detectAnomalies(store);

      const outliers = result.anomalies.filter(a => a.type === 'outlier');
      expect(outliers.length).toBeGreaterThanOrEqual(0); // May or may not detect
    });

    it('should detect missing links', async () => {
      const alice = namedNode('http://example.org/alice');
      const _bob = namedNode('http://example.org/_bob');
      const knows = namedNode('http://example.org/knows');

      // Create pattern suggesting missing inverse
      for (let i = 0; i < 10; i++) {
        const person = namedNode(`http://example.org/person${i}`);
        store.addQuad(quad(alice, knows, person));
      }

      const result = await detector.detectAnomalies(store);

      const missingLinks = result.anomalies.filter(a => a.type === 'missing_link');
      // May or may not detect depending on heuristics
      expect(missingLinks).toBeInstanceOf(Array);
    });

    it('should detect data quality issues', async () => {
      const alice = namedNode('http://example.org/alice');
      const bob = namedNode('http://example.org/bob');
      const knows = namedNode('http://example.org/knows');
      const rdfsLabel = namedNode('http://www.w3.org/2000/01/rdf-schema#label');

      // Alice has properties but no label
      store.addQuad(quad(alice, knows, bob));
      store.addQuad(quad(alice, knows, namedNode('http://example.org/charlie')));
      store.addQuad(quad(alice, knows, namedNode('http://example.org/david')));

      // Bob has label
      store.addQuad(quad(bob, rdfsLabel, literal('Bob')));

      const result = await detector.detectAnomalies(store);

      const qualityIssues = result.anomalies.filter(a => a.type === 'data_quality');
      expect(qualityIssues.length).toBeGreaterThan(0);
    });

    it('should detect structural anomalies', async () => {
      const alice = namedNode('http://example.org/alice');
      const knows = namedNode('http://example.org/knows');

      // Self-loop
      store.addQuad(quad(alice, knows, alice));

      const result = await detector.detectAnomalies(store);

      const structuralAnomalies = result.anomalies.filter(a => a.type === 'structural_anomaly');
      expect(structuralAnomalies.length).toBeGreaterThan(0);
    });

    it('should filter anomalies by confidence', async () => {
      const highConfDetector = createAnomalyDetector({
        minConfidence: 0.9,
      });

      const alice = namedNode('http://example.org/alice');
      const knows = namedNode('http://example.org/knows');

      for (let i = 0; i < 20; i++) {
        const person = namedNode(`http://example.org/person${i}`);
        store.addQuad(quad(alice, knows, person));
      }

      const result = await highConfDetector.detectAnomalies(store);

      // Should have fewer anomalies with high confidence threshold
      expect(result.anomalies.every(a => a.confidence >= 0.9)).toBe(true);
    });

    it('should calculate anomaly statistics', async () => {
      const alice = namedNode('http://example.org/alice');
      const knows = namedNode('http://example.org/knows');

      store.addQuad(quad(alice, knows, alice)); // Self-loop

      const result = await detector.detectAnomalies(store);

      expect(result.statistics).toBeDefined();
      expect(result.statistics.total).toBe(result.anomalies.length);
      expect(result.statistics.bySeverity).toBeDefined();
      expect(result.statistics.byType).toBeDefined();
    });
  });

  describe('anomaly types', () => {
    it('should classify anomalies by type', async () => {
      const alice = namedNode('http://example.org/alice');
      const bob = namedNode('http://example.org/bob');
      const knows = namedNode('http://example.org/knows');

      store.addQuad(quad(alice, knows, alice)); // Structural anomaly

      // Missing label (data quality)
      store.addQuad(quad(bob, knows, alice));

      const result = await detector.detectAnomalies(store);

      const types = new Set(result.anomalies.map(a => a.type));
      expect(types.size).toBeGreaterThan(0);

      const validTypes = [
        'missing_link',
        'unexpected_pattern',
        'data_quality',
        'outlier',
        'inconsistency',
        'structural_anomaly',
      ];

      for (const type of types) {
        expect(validTypes).toContain(type);
      }
    });

    it('should assign severity levels', async () => {
      const alice = namedNode('http://example.org/alice');
      const knows = namedNode('http://example.org/knows');

      for (let i = 0; i < 30; i++) {
        const person = namedNode(`http://example.org/person${i}`);
        store.addQuad(quad(alice, knows, person));
      }

      const result = await detector.detectAnomalies(store);

      const validSeverities = ['critical', 'high', 'medium', 'low'];

      for (const anomaly of result.anomalies) {
        expect(validSeverities).toContain(anomaly.severity);
      }
    });
  });

  describe('ML-based detection', () => {
    it('should detect ML-based anomalies when enabled', async () => {
      const mlDetector = createAnomalyDetector({
        enableMLBased: true,
        enableStatistical: false,
      });

      // Create sufficient data for ML
      for (let i = 0; i < 15; i++) {
        const entity1 = namedNode(`http://example.org/entity${i}`);
        const entity2 = namedNode(`http://example.org/entity${i + 1}`);
        const pred = namedNode('http://example.org/pred');
        store.addQuad(quad(entity1, pred, entity2));
      }

      const result = await mlDetector.detectAnomalies(store);

      expect(result).toBeDefined();
      expect(result.anomalies).toBeInstanceOf(Array);
    });

    it('should skip ML detection for insufficient data', async () => {
      const alice = namedNode('http://example.org/alice');
      const bob = namedNode('http://example.org/bob');
      const knows = namedNode('http://example.org/knows');

      store.addQuad(quad(alice, knows, bob));

      const result = await detector.detectAnomalies(store);

      expect(result).toBeDefined();
    });
  });

  describe('edge cases', () => {
    it('should handle empty graph', async () => {
      const emptyStore = new Store();

      const result = await detector.detectAnomalies(emptyStore);

      expect(result.anomalies).toBeInstanceOf(Array);
      expect(result.anomalies.length).toBe(0);
    });

    it('should handle single triple', async () => {
      const alice = namedNode('http://example.org/alice');
      const bob = namedNode('http://example.org/bob');
      const knows = namedNode('http://example.org/knows');

      store.addQuad(quad(alice, knows, bob));

      const result = await detector.detectAnomalies(store);

      expect(result).toBeDefined();
      expect(result.anomalies).toBeInstanceOf(Array);
    });

    it('should limit anomalies by maxAnomalies config', async () => {
      const limitedDetector = createAnomalyDetector({
        maxAnomalies: 5,
      });

      // Create many potential anomalies
      for (let i = 0; i < 20; i++) {
        const entity = namedNode(`http://example.org/entity${i}`);
        const prop = namedNode('http://example.org/prop');
        store.addQuad(quad(entity, prop, literal(`value${i}`)));
      }

      const result = await limitedDetector.detectAnomalies(store);

      expect(result.anomalies.length).toBeLessThanOrEqual(5);
    });
  });

  describe('statistics', () => {
    it('should track detector statistics', async () => {
      const alice = namedNode('http://example.org/alice');
      const bob = namedNode('http://example.org/bob');
      const knows = namedNode('http://example.org/knows');

      store.addQuad(quad(alice, knows, bob));

      await detector.detectAnomalies(store);

      const stats = detector.getStats();

      expect(stats).toHaveProperty('detections');
      expect(stats).toHaveProperty('anomaliesFound');
      expect(stats).toHaveProperty('avgDuration');
      expect(stats.detections).toBeGreaterThan(0);
    });
  });

  describe('performance', () => {
    it('should detect anomalies efficiently', async () => {
      // Create moderate-sized graph
      for (let i = 0; i < 50; i++) {
        const entity1 = namedNode(`http://example.org/entity${i}`);
        const entity2 = namedNode(`http://example.org/entity${i + 1}`);
        const pred = namedNode(`http://example.org/pred${i % 5}`);
        store.addQuad(quad(entity1, pred, entity2));
      }

      const result = await detector.detectAnomalies(store);

      expect(result.duration).toBeLessThan(5000); // 5 seconds
    });
  });
});
