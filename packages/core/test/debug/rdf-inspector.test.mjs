/**
 * @file RDF inspector tests
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { createStore, namedNode, literal } from '../../src/rdf/store.mjs';
import {
  getGraphStatistics,
  analyzeNamespaces,
  detectOrphans,
  assessDataQuality,
  checkSchemaConformance,
  generateInspectionReport,
} from '../../src/debug/rdf-inspector.mjs';

describe('RDFInspector', () => {
  let store;

  beforeEach(() => {
    store = createStore();

    // Add test data
    store.add({
      subject: namedNode('http://example.org/alice'),
      predicate: namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      object: namedNode('http://xmlns.com/foaf/0.1/Person'),
    });

    store.add({
      subject: namedNode('http://example.org/alice'),
      predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
      object: literal('Alice'),
    });

    store.add({
      subject: namedNode('http://example.org/alice'),
      predicate: namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
      object: literal('Alice Person', 'en'),
    });
  });

  describe('getGraphStatistics()', () => {
    it('should return basic statistics', () => {
      const stats = getGraphStatistics(store);

      expect(stats.tripleCount).toBe(3);
      expect(stats.subjectCount).toBe(1);
      expect(stats.predicateCount).toBe(3);
      expect(stats.objectCount).toBe(3);
      expect(stats.literalCount).toBe(2);
    });

    it('should include namespaces when requested', () => {
      const stats = getGraphStatistics(store, { includeNamespaces: true });

      expect(stats.namespaces).toBeDefined();
      expect(stats.namespaces.length).toBeGreaterThan(0);
    });

    it('should include orphans when requested', () => {
      const stats = getGraphStatistics(store, { includeOrphans: true });

      expect(stats.orphans).toBeDefined();
      expect(Array.isArray(stats.orphans)).toBe(true);
    });

    it('should include quality when requested', () => {
      const stats = getGraphStatistics(store, { includeQuality: true });

      expect(stats.quality).toBeDefined();
      expect(stats.quality.score).toBeGreaterThanOrEqual(0);
      expect(stats.quality.score).toBeLessThanOrEqual(100);
    });

    it('should throw on invalid store', () => {
      expect(() => getGraphStatistics(null))
        .toThrow('store must be a valid RDF store');
    });
  });

  describe('analyzeNamespaces()', () => {
    it('should analyze namespace usage', () => {
      const namespaces = analyzeNamespaces(store);

      expect(Array.isArray(namespaces)).toBe(true);
      expect(namespaces.length).toBeGreaterThan(0);
      expect(namespaces[0]).toHaveProperty('namespace');
      expect(namespaces[0]).toHaveProperty('count');
    });

    it('should sort namespaces by count', () => {
      const namespaces = analyzeNamespaces(store);

      for (let i = 1; i < namespaces.length; i++) {
        expect(namespaces[i - 1].count).toBeGreaterThanOrEqual(namespaces[i].count);
      }
    });

    it('should throw on invalid store', () => {
      expect(() => analyzeNamespaces(null))
        .toThrow('store must be a valid RDF store');
    });
  });

  describe('detectOrphans()', () => {
    it('should detect orphaned nodes', () => {
      const orphans = detectOrphans(store);

      expect(Array.isArray(orphans)).toBe(true);
      expect(orphans).toContain('http://example.org/alice');
    });

    it('should return empty array when no orphans', () => {
      store.add({
        subject: namedNode('http://example.org/bob'),
        predicate: namedNode('http://xmlns.com/foaf/0.1/knows'),
        object: namedNode('http://example.org/alice'),
      });

      const orphans = detectOrphans(store);
      expect(orphans).not.toContain('http://example.org/alice');
    });

    it('should throw on invalid store', () => {
      expect(() => detectOrphans(null))
        .toThrow('store must be a valid RDF store');
    });
  });

  describe('assessDataQuality()', () => {
    it('should assess data quality', () => {
      const quality = assessDataQuality(store);

      expect(quality.score).toBeGreaterThanOrEqual(0);
      expect(quality.score).toBeLessThanOrEqual(100);
      expect(quality).toHaveProperty('labelCoverage');
      expect(quality).toHaveProperty('typeCoverage');
      expect(quality).toHaveProperty('literalQuality');
    });

    it('should calculate label coverage', () => {
      const quality = assessDataQuality(store);

      expect(quality.labelCoverage).toBeGreaterThan(0);
    });

    it('should calculate type coverage', () => {
      const quality = assessDataQuality(store);

      expect(quality.typeCoverage).toBeGreaterThan(0);
    });

    it('should include detailed metrics', () => {
      const quality = assessDataQuality(store);

      expect(quality.metrics).toBeDefined();
      expect(quality.metrics.totalTriples).toBe(3);
      expect(quality.metrics.uniqueSubjects).toBe(1);
    });

    it('should throw on invalid store', () => {
      expect(() => assessDataQuality(null))
        .toThrow('store must be a valid RDF store');
    });
  });

  describe('checkSchemaConformance()', () => {
    it('should check required predicates', () => {
      const schema = {
        requiredPredicates: ['http://www.w3.org/1999/02/22-rdf-syntax-ns#type'],
      };

      const report = checkSchemaConformance(store, schema);

      expect(report.conforms).toBe(true);
      expect(report.violationCount).toBe(0);
    });

    it('should detect missing required predicates', () => {
      const schema = {
        requiredPredicates: ['http://example.org/missing'],
      };

      const report = checkSchemaConformance(store, schema);

      expect(report.conforms).toBe(false);
      expect(report.violationCount).toBeGreaterThan(0);
      expect(report.violations[0].type).toBe('MISSING_REQUIRED_PREDICATE');
    });

    it('should check minimum triples', () => {
      const schema = {
        minTriples: 10,
      };

      const report = checkSchemaConformance(store, schema);

      expect(report.conforms).toBe(false);
      expect(report.violations[0].type).toBe('MIN_TRIPLES_VIOLATION');
    });

    it('should throw on invalid store', () => {
      expect(() => checkSchemaConformance(null, {}))
        .toThrow('store must be a valid RDF store');
    });

    it('should throw on invalid schema', () => {
      expect(() => checkSchemaConformance(store, null))
        .toThrow('schema must be an object');
    });
  });

  describe('generateInspectionReport()', () => {
    it('should generate comprehensive report', () => {
      const report = generateInspectionReport(store);

      expect(report).toContain('RDF GRAPH INSPECTION REPORT');
      expect(report).toContain('Total Triples:');
      expect(report).toContain('Unique Subjects:');
      expect(report).toContain('QUALITY SCORE:');
    });

    it('should include quality metrics', () => {
      const report = generateInspectionReport(store);

      expect(report).toContain('Label Coverage:');
      expect(report).toContain('Type Coverage:');
      expect(report).toContain('Literal Quality:');
    });

    it('should include namespace information', () => {
      const report = generateInspectionReport(store);

      expect(report).toContain('Top Namespaces:');
    });

    it('should throw on invalid store', () => {
      expect(() => generateInspectionReport(null))
        .toThrow('store must be a valid RDF store');
    });
  });
});
