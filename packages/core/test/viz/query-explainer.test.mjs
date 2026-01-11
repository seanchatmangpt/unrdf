/**
 * @file Query explainer tests
 */

import { describe, it, expect } from 'vitest';
import {
  explainQuery,
  formatPlanAsTree,
  trackQueryStats,
  compareQueryPerformance,
} from '../../src/viz/query-explainer.mjs';

describe('QueryExplainer', () => {
  describe('explainQuery()', () => {
    it('should explain SELECT query', () => {
      const query = 'SELECT * WHERE { ?s ?p ?o }';
      const plan = explainQuery(query);

      expect(plan.queryType).toBe('SELECT');
      expect(plan.steps).toBeInstanceOf(Array);
      expect(plan.steps.length).toBeGreaterThan(0);
      expect(plan.estimatedCost).toBeGreaterThan(0);
    });

    it('should explain query with PREFIX', () => {
      const query = `
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        SELECT * WHERE { ?s foaf:name ?name }
      `;
      const plan = explainQuery(query);

      expect(plan.steps[0].operation).toBe('RESOLVE_PREFIXES');
    });

    it('should explain query with FILTER', () => {
      const query = `
        SELECT * WHERE {
          ?s ?p ?o
          FILTER(?o > 10)
        }
      `;
      const plan = explainQuery(query);

      const filterStep = plan.steps.find(s => s.operation === 'FILTER');
      expect(filterStep).toBeDefined();
    });

    it('should explain query with LIMIT', () => {
      const query = 'SELECT * WHERE { ?s ?p ?o } LIMIT 10';
      const plan = explainQuery(query);

      const limitStep = plan.steps.find(s => s.operation === 'LIMIT');
      expect(limitStep).toBeDefined();
      expect(limitStep.description).toContain('10');
    });

    it('should include optimizations when requested', () => {
      const query = 'SELECT * WHERE { ?s ?p ?o . ?x ?y ?z . ?a ?b ?c }';
      const plan = explainQuery(query, { includeOptimizations: true });

      expect(plan.optimizations).toBeDefined();
      expect(plan.optimizations.length).toBeGreaterThan(0);
    });

    it('should throw on empty query', () => {
      expect(() => explainQuery('')).toThrow('query must be a non-empty string');
    });

    it('should throw on non-string query', () => {
      expect(() => explainQuery(123)).toThrow('query must be a non-empty string');
    });
  });

  describe('formatPlanAsTree()', () => {
    it('should format plan as tree', () => {
      const query = 'SELECT * WHERE { ?s ?p ?o } LIMIT 10';
      const plan = explainQuery(query);
      const tree = formatPlanAsTree(plan);

      expect(tree).toContain('Query Type:');
      expect(tree).toContain('Estimated Cost:');
      expect(tree).toContain('Execution Steps:');
      expect(tree).toContain('├──');
    });

    it('should include optimizations in tree', () => {
      const query = 'SELECT * WHERE { ?s ?p ?o . ?x ?y ?z . ?a ?b ?c }';
      const plan = explainQuery(query, { includeOptimizations: true });
      const tree = formatPlanAsTree(plan);

      expect(tree).toContain('Optimization Suggestions:');
    });

    it('should throw on invalid plan', () => {
      expect(() => formatPlanAsTree(null)).toThrow('plan must have steps array');
    });
  });

  describe('trackQueryStats()', () => {
    it('should track successful query execution', async () => {
      const mockQuery = async () => ['result1', 'result2'];
      const stats = await trackQueryStats(mockQuery, 'SELECT * WHERE { ?s ?p ?o }');

      expect(stats.success).toBe(true);
      expect(stats.executionTime).toBeGreaterThanOrEqual(0);
      expect(stats.resultCount).toBe(2);
      expect(stats.error).toBeNull();
    });

    it('should track failed query execution', async () => {
      const mockQuery = async () => {
        throw new Error('Query failed');
      };
      const stats = await trackQueryStats(mockQuery, 'SELECT * WHERE { ?s ?p ?o }');

      expect(stats.success).toBe(false);
      expect(stats.error).toBe('Query failed');
      expect(stats.result).toBeNull();
    });

    it('should measure execution time', async () => {
      const mockQuery = async () => {
        await new Promise(resolve => setTimeout(resolve, 10));
        return ['result'];
      };
      const stats = await trackQueryStats(mockQuery, 'SELECT * WHERE { ?s ?p ?o }');

      expect(stats.executionTime).toBeGreaterThan(5);
    });

    it('should throw on non-function queryFn', async () => {
      await expect(trackQueryStats('not a function', 'query'))
        .rejects.toThrow('queryFn must be a function');
    });
  });

  describe('compareQueryPerformance()', () => {
    it('should compare query performance', () => {
      const stats = [
        { executionTime: 10, query: 'q1' },
        { executionTime: 5, query: 'q2' },
        { executionTime: 15, query: 'q3' },
      ];

      const comparison = compareQueryPerformance(stats);

      expect(comparison.fastest.executionTime).toBe(5);
      expect(comparison.slowest.executionTime).toBe(15);
      expect(comparison.average).toBe(10);
      expect(comparison.median).toBe(10);
      expect(comparison.totalTime).toBe(30);
    });

    it('should throw on empty array', () => {
      expect(() => compareQueryPerformance([]))
        .toThrow('stats must be a non-empty array');
    });

    it('should throw on non-array', () => {
      expect(() => compareQueryPerformance('not an array'))
        .toThrow('stats must be a non-empty array');
    });
  });
});
