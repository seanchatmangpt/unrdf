/**
 * @vitest-environment node
 */
import { describe, it, expect, beforeEach } from 'vitest';
import { createStore } from '@unrdf/oxigraph';
import {
  analyzeSparqlQuery,
  estimateComplexity,
  identifyBottlenecks,
  optimizeQuery,
  suggestIndexes,
  explainOptimization,
  createMetricsCollector,
  recordQuery,
  analyzePerformance,
  analyzeIndexNeeds,
  suggestIndexForPattern,
  calculateIndexBenefit,
} from '../src/index.mjs';

describe('@unrdf/dark-matter', () => {
  describe('Query Analyzer', () => {
    it('should analyze simple SELECT query', () => {
      const query = `
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        SELECT ?name WHERE {
          ?person foaf:name ?name .
        }
      `;

      const analysis = analyzeSparqlQuery(query);

      expect(analysis.type).toBe('select');
      expect(analysis.variables).toContain('name');
      expect(analysis.variables).toContain('person');
      expect(analysis.patterns).toHaveLength(1);
      expect(analysis.prefixes.foaf).toBe('http://xmlns.com/foaf/0.1/');
    });

    it('should identify joins in query', () => {
      const query = `
        SELECT ?name ?email WHERE {
          ?person foaf:name ?name .
          ?person foaf:mbox ?email .
        }
      `;

      const analysis = analyzeSparqlQuery(query);

      expect(analysis.joins.length).toBeGreaterThan(0);
      const personJoin = analysis.joins.find(j => j.variable === '?person');
      expect(personJoin).toBeDefined();
      expect(personJoin.patterns).toHaveLength(2);
    });

    it('should calculate pattern selectivity', () => {
      const query = `
        SELECT ?o WHERE {
          <http://example.org/person1> <http://xmlns.com/foaf/0.1/name> ?o .
        }
      `;

      const analysis = analyzeSparqlQuery(query);

      expect(analysis.patterns[0].selectivity).toBeGreaterThan(0.5);
    });

    it('should estimate complexity', () => {
      const simpleQuery = 'SELECT ?s WHERE { ?s ?p ?o }';
      const complexQuery = `
        SELECT ?name ?email ?friend WHERE {
          ?person foaf:name ?name .
          ?person foaf:mbox ?email .
          ?person foaf:knows ?friend .
          FILTER(regex(?name, "John"))
        }
      `;

      const simpleComplexity = estimateComplexity(simpleQuery);
      const complexComplexity = estimateComplexity(complexQuery);

      expect(simpleComplexity).toBeLessThan(complexComplexity);
      expect(simpleComplexity).toBeGreaterThanOrEqual(1);
      expect(complexComplexity).toBeLessThanOrEqual(10);
    });

    it('should identify bottlenecks', () => {
      const query = `
        SELECT * WHERE {
          ?s1 ?p1 ?o1 .
          ?s2 ?p2 ?o2 .
          ?s3 ?p3 ?o3 .
          ?s4 ?p4 ?o4 .
          FILTER(regex(?o1, ".*pattern.*"))
        }
      `;

      const bottlenecks = identifyBottlenecks(query);

      expect(bottlenecks.length).toBeGreaterThan(0);
      expect(bottlenecks.some(b => b.type === 'low_selectivity')).toBe(true);
    });
  });

  describe('Query Optimizer', () => {
    it('should optimize query pattern order', () => {
      const query = `
        SELECT ?name WHERE {
          ?person foaf:knows ?friend .
          ?person foaf:name ?name .
        }
      `;

      const result = optimizeQuery(query);

      expect(result.optimizedQuery).toBeDefined();
      expect(result.changes.length).toBeGreaterThanOrEqual(0);
      expect(result.estimatedSpeedup).toBeGreaterThanOrEqual(1);
    });

    it('should suggest indexes', () => {
      const store = createStore();
      const query = `
        SELECT ?name WHERE {
          ?person <http://xmlns.com/foaf/0.1/name> ?name .
        }
      `;

      const suggestions = suggestIndexes(store, query);

      expect(Array.isArray(suggestions)).toBe(true);
      expect(suggestions.length).toBeGreaterThan(0);
      expect(suggestions[0].type).toBe('predicate_index');
    });

    it('should explain optimization', () => {
      const original = 'SELECT ?s WHERE { ?s ?p ?o }';
      const optimized = 'SELECT ?s WHERE { ?s ?p ?o }';

      const explanation = explainOptimization(original, optimized);

      expect(explanation.summary).toBeDefined();
      expect(explanation.originalComplexity).toBeGreaterThanOrEqual(0);
      expect(explanation.optimizedComplexity).toBeGreaterThanOrEqual(0);
    });

    it('should handle queries without WHERE clause', () => {
      const query = 'SELECT ?s ?p ?o';

      const result = optimizeQuery(query);

      expect(result.optimizedQuery).toBe(query);
    });
  });

  describe('Performance Metrics', () => {
    let metrics;

    beforeEach(() => {
      metrics = createMetricsCollector();
    });

    it('should record query execution', () => {
      const query = 'SELECT ?s WHERE { ?s ?p ?o }';

      metrics.recordQuery(query, 150, 42);

      const collected = metrics.getMetrics();
      expect(collected).toHaveLength(1);
      expect(collected[0].query).toBe(query);
      expect(collected[0].executionTime).toBe(150);
      expect(collected[0].resultCount).toBe(42);
    });

    it('should analyze performance statistics', () => {
      metrics.recordQuery('SELECT ?s WHERE { ?s ?p ?o }', 100, 10);
      metrics.recordQuery('SELECT ?s WHERE { ?s ?p ?o }', 200, 20);
      metrics.recordQuery('SELECT ?s WHERE { ?s ?p ?o }', 300, 30);

      const stats = metrics.analyzePerformance();

      expect(stats.totalQueries).toBe(3);
      expect(stats.averageExecutionTime).toBe(200);
      expect(stats.slowestQueries).toHaveLength(3);
    });

    it('should track most executed queries', () => {
      const query1 = 'SELECT ?s WHERE { ?s ?p ?o }';
      const query2 = 'SELECT ?name WHERE { ?s foaf:name ?name }';

      metrics.recordQuery(query1, 100, 10);
      metrics.recordQuery(query1, 100, 10);
      metrics.recordQuery(query2, 100, 10);

      const stats = metrics.analyzePerformance();

      expect(stats.mostExecutedQueries).toHaveLength(2);
      expect(stats.mostExecutedQueries[0].executionCount).toBe(2);
    });

    it('should handle empty metrics', () => {
      const stats = metrics.analyzePerformance();

      expect(stats.totalQueries).toBe(0);
      expect(stats.averageExecutionTime).toBe(0);
    });

    it('should clear metrics', () => {
      metrics.recordQuery('SELECT ?s WHERE { ?s ?p ?o }', 100, 10);
      metrics.clearMetrics();

      const collected = metrics.getMetrics();
      expect(collected).toHaveLength(0);
    });

    it('should validate query parameters', () => {
      expect(() => metrics.recordQuery(123, 100, 10)).toThrow(TypeError);
      expect(() => metrics.recordQuery('query', -1, 10)).toThrow(TypeError);
      expect(() => metrics.recordQuery('query', 100, -1)).toThrow(TypeError);
    });

    it('should analyze store performance', () => {
      const store = createStore();

      const analysis = analyzePerformance(store);

      expect(analysis.quadCount).toBeDefined();
      expect(analysis.estimatedMemoryUsage).toBeDefined();
      expect(Array.isArray(analysis.recommendations)).toBe(true);
    });

    it('should record standalone query', () => {
      const record = recordQuery('SELECT ?s WHERE { ?s ?p ?o }', 150, 42);

      expect(record.query).toBe('SELECT ?s WHERE { ?s ?p ?o }');
      expect(record.executionTime).toBe(150);
      expect(record.resultCount).toBe(42);
      expect(record.timestamp).toBeDefined();
    });
  });

  describe('Index Advisor', () => {
    it('should analyze index needs from query log', () => {
      const store = createStore();
      const queryLog = [
        'SELECT ?name WHERE { ?s <http://xmlns.com/foaf/0.1/name> ?name }',
        'SELECT ?name WHERE { ?s <http://xmlns.com/foaf/0.1/name> ?name }',
        'SELECT ?name WHERE { ?s <http://xmlns.com/foaf/0.1/name> ?name }',
      ];

      const recommendations = analyzeIndexNeeds(store, queryLog);

      expect(Array.isArray(recommendations)).toBe(true);
      expect(recommendations.length).toBeGreaterThan(0);
      expect(recommendations[0].type).toBeDefined();
      expect(recommendations[0].priority).toBeDefined();
    });

    it('should suggest index for specific pattern', () => {
      const pattern = {
        subject: '?s',
        predicate: '<http://xmlns.com/foaf/0.1/name>',
        object: '?name',
      };

      const suggestion = suggestIndexForPattern(pattern);

      expect(suggestion.type).toBe('predicate');
      expect(suggestion.priority).toBe('high');
      expect(suggestion.estimatedBenefit).toBeGreaterThan(0);
    });

    it('should calculate index benefit', () => {
      const pattern = {
        subject: '?s',
        predicate: '<http://xmlns.com/foaf/0.1/name>',
        object: '?name',
      };
      const indexConfig = {
        fields: ['predicate'],
        unique: false,
      };

      const benefit = calculateIndexBenefit(pattern, indexConfig);

      expect(benefit).toBeGreaterThan(0);
      expect(benefit).toBeLessThanOrEqual(100);
    });

    it('should handle different pattern types', () => {
      const patterns = [
        {
          subject: '<http://example.org/person1>',
          predicate: '<http://xmlns.com/foaf/0.1/name>',
          object: '"Alice"',
        },
        {
          subject: '?s',
          predicate: '?p',
          object: '?o',
        },
      ];

      patterns.forEach(pattern => {
        const suggestion = suggestIndexForPattern(pattern);
        expect(suggestion).toBeDefined();
        expect(suggestion.type).toBeDefined();
      });
    });

    it('should validate input parameters', () => {
      const store = createStore();

      expect(() => analyzeIndexNeeds(null, [])).toThrow(TypeError);
      expect(() => analyzeIndexNeeds(store, 'not-array')).toThrow(TypeError);
      expect(() => suggestIndexForPattern(null)).toThrow(TypeError);
      expect(() => calculateIndexBenefit(null, {})).toThrow(TypeError);
    });
  });

  describe('Integration Tests', () => {
    it('should optimize and explain changes', () => {
      const query = `
        SELECT ?name ?email WHERE {
          ?person foaf:knows ?friend .
          ?person foaf:name ?name .
          ?person foaf:mbox ?email .
        }
      `;

      const optimized = optimizeQuery(query);
      const explanation = explainOptimization(query, optimized.optimizedQuery);

      expect(optimized.estimatedSpeedup).toBeGreaterThanOrEqual(1);
      expect(explanation.changes).toBeDefined();
    });

    it('should combine analysis and metrics', () => {
      const metrics = createMetricsCollector();
      const query = 'SELECT ?s WHERE { ?s foaf:name "Alice" }';

      const analysis = analyzeSparqlQuery(query);
      metrics.recordQuery(query, 150, analysis.estimatedResults);

      const stats = metrics.analyzePerformance();

      expect(stats.totalQueries).toBe(1);
      expect(analysis.type).toBe('select');
    });

    it('should provide index recommendations for slow queries', () => {
      const store = createStore();
      const metrics = createMetricsCollector();

      const query = `
        SELECT ?name WHERE {
          ?person <http://xmlns.com/foaf/0.1/name> ?name .
        }
      `;

      metrics.recordQuery(query, 500, 100);
      const stats = metrics.analyzePerformance();
      const suggestions = suggestIndexes(store, query);

      expect(stats.slowestQueries).toHaveLength(1);
      expect(suggestions.length).toBeGreaterThan(0);
    });
  });
});
