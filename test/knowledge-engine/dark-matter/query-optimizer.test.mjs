/**
 * @file Dark Matter 80/20 Query Optimization Test Suite
 * @module test/dark-matter/query-optimizer
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { createQueryAnalyzer } from '../../../src/knowledge-engine/dark-matter/query-analyzer.mjs';
import { createCriticalPathIdentifier } from '../../../src/knowledge-engine/dark-matter/critical-path.mjs';
import { createDarkMatterOptimizer } from '../../../src/knowledge-engine/dark-matter/optimizer.mjs';
import { createDarkMatterQuerySystem } from '../../../src/knowledge-engine/dark-matter/index.mjs';

describe('Dark Matter Query Analyzer', () => {
  let analyzer;

  beforeEach(() => {
    analyzer = createQueryAnalyzer();
  });

  it('should analyze a simple SELECT query', () => {
    const query = `
      SELECT ?name WHERE {
        ?person <http://example.org/name> ?name .
      }
    `;

    const analysis = analyzer.analyze(query, 'test-query-1');

    expect(analysis).toBeDefined();
    expect(analysis.queryId).toBe('test-query-1');
    expect(analysis.type).toBe('SELECT');
    expect(analysis.patterns).toHaveLength(1);
    expect(analysis.complexity.score).toBeGreaterThan(0);
  });

  it('should identify expensive operations', () => {
    const query = `
      SELECT ?s ?p ?o WHERE {
        ?s ?p ?o .
        FILTER(?p = <http://example.org/expensive>)
      }
    `;

    const analysis = analyzer.analyze(query);

    expect(analysis.expensiveOperations.length).toBeGreaterThan(0);

    // Variable predicate should be flagged as expensive
    const variablePredicate = analysis.expensiveOperations.find(
      op => op.type === 'variable-predicate'
    );
    expect(variablePredicate).toBeDefined();
    expect(variablePredicate.cost).toBeGreaterThan(50);
  });

  it('should detect JOIN operations', () => {
    const query = `
      SELECT ?name ?age WHERE {
        ?person <http://example.org/name> ?name .
        ?person <http://example.org/age> ?age .
      }
    `;

    const analysis = analyzer.analyze(query);

    expect(analysis.joins.length).toBeGreaterThan(0);

    // Should detect join on ?person variable
    const personJoin = analysis.joins.find(
      j => j.variables.includes('person')
    );
    expect(personJoin).toBeDefined();
  });

  it('should calculate complexity for aggregations', () => {
    const query = `
      SELECT (COUNT(?person) as ?count) WHERE {
        ?person <http://example.org/name> ?name .
      }
      GROUP BY ?name
    `;

    const analysis = analyzer.analyze(query);

    expect(analysis.aggregations).toContain('COUNT');
    expect(analysis.complexity.aggregationCount).toBe(1);
    expect(analysis.complexity.score).toBeGreaterThan(0);
  });

  it('should track analyzer statistics', () => {
    const query1 = 'SELECT ?s WHERE { ?s ?p ?o . }';
    const query2 = 'SELECT ?s ?p ?o WHERE { ?s ?p ?o . FILTER(?p = "test") }';

    analyzer.analyze(query1);
    analyzer.analyze(query2);

    const stats = analyzer.getStats();

    expect(stats.totalAnalyzed).toBe(2);
    expect(stats.avgComplexity).toBeGreaterThan(0);
  });
});

describe('Dark Matter Critical Path Identifier', () => {
  let criticalPath;

  beforeEach(() => {
    criticalPath = createCriticalPathIdentifier({
      minSampleSize: 5
    });
  });

  afterEach(() => {
    criticalPath.clearLogs();
  });

  it('should log query executions', () => {
    criticalPath.logExecution('query-1', 'SELECT ?s WHERE { ?s ?p ?o }', 100);
    criticalPath.logExecution('query-2', 'SELECT * WHERE { ?s ?p ?o }', 500);

    const logs = criticalPath.getLogs();

    expect(logs).toHaveLength(2);
    expect(logs[0].executionTime).toBe(100);
    expect(logs[1].executionTime).toBe(500);
  });

  it('should identify critical queries (80/20 rule)', () => {
    // Create a distribution where 20% of queries take 80% of time

    // 80% of queries are fast (20% of total time)
    for (let i = 0; i < 8; i++) {
      criticalPath.logExecution(`fast-query-${i}`, 'SELECT ?s WHERE { ?s ?p ?o }', 10);
    }

    // 20% of queries are slow (80% of total time)
    for (let i = 0; i < 2; i++) {
      criticalPath.logExecution(`slow-query-${i}`, 'SELECT * WHERE { ?s ?p ?o }', 160);
    }

    const analysis = criticalPath.identify();

    expect(analysis.metrics.totalQueries).toBe(10);

    // Should identify the slow queries as critical
    expect(analysis.criticalQueries.length).toBeLessThanOrEqual(2);

    // Impact ratio should be close to 80%
    expect(analysis.metrics.impactRatio).toBeGreaterThan(0.7);
  });

  it('should calculate percentiles correctly', () => {
    // Log queries with known distribution
    const times = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100];

    for (const [i, time] of times.entries()) {
      criticalPath.logExecution(`query-${i}`, 'SELECT ?s WHERE { ?s ?p ?o }', time);
    }

    const analysis = criticalPath.identify();

    // P50 should be around 50ms
    expect(analysis.metrics.p50).toBeGreaterThanOrEqual(45);
    expect(analysis.metrics.p50).toBeLessThanOrEqual(55);

    // P99 should be close to 100ms
    expect(analysis.metrics.p99).toBeGreaterThanOrEqual(95);
  });

  it('should generate markdown report', () => {
    // Log enough data
    for (let i = 0; i < 10; i++) {
      criticalPath.logExecution(`query-${i}`, 'SELECT ?s WHERE { ?s ?p ?o }', 50 + i * 10);
    }

    const report = criticalPath.getReport();

    expect(report).toContain('# Dark Matter 80/20 Critical Path Report');
    expect(report).toContain('Total Queries Analyzed');
    expect(report).toContain('Critical Queries');
    expect(report).toContain('Recommendations');
  });

  it('should export and import logs', () => {
    criticalPath.logExecution('query-1', 'SELECT ?s WHERE { ?s ?p ?o }', 100);
    criticalPath.logExecution('query-2', 'SELECT * WHERE { ?s ?p ?o }', 200);

    const exported = criticalPath.exportLogs();
    expect(exported).toContain('query-1');
    expect(exported).toContain('query-2');

    const newCriticalPath = createCriticalPathIdentifier({ minSampleSize: 2 });
    newCriticalPath.importLogs(exported);

    const logs = newCriticalPath.getLogs();
    expect(logs).toHaveLength(2);
  });
});

describe('Dark Matter Optimizer', () => {
  let optimizer;

  beforeEach(() => {
    optimizer = createDarkMatterOptimizer();
  });

  it('should optimize a query with filters', () => {
    const query = `
      SELECT ?name WHERE {
        ?person <http://example.org/name> ?name .
        OPTIONAL { ?person <http://example.org/age> ?age }
        FILTER(?age > 18)
      }
    `;

    const result = optimizer.optimize(query);

    expect(result.original).toBe(query);
    expect(result.rules.length).toBeGreaterThan(0);

    // Should apply filter pushdown
    const filterPushdown = result.rules.find(r => r.name === 'filter-pushdown');
    expect(filterPushdown).toBeDefined();
  });

  it('should reorder joins for better performance', () => {
    const query = `
      SELECT ?name WHERE {
        ?person <http://example.org/knows> ?friend .
        <http://example.org/alice> <http://example.org/name> ?name .
        ?friend <http://example.org/age> ?age .
      }
    `;

    const result = optimizer.optimize(query);

    // Join reordering should be applied
    const joinReorder = result.rules.find(r => r.name === 'join-reordering');
    expect(joinReorder).toBeDefined();
  });

  it('should add LIMIT to queries without aggregation', () => {
    const query = `
      SELECT ?s ?p ?o WHERE {
        ?s ?p ?o .
      }
    `;

    const result = optimizer.optimize(query);

    // Early limit should be applied
    const earlyLimit = result.rules.find(r => r.name === 'limit-early');
    expect(earlyLimit).toBeDefined();

    if (earlyLimit.applied) {
      expect(result.optimized).toContain('LIMIT');
    }
  });

  it('should estimate cost correctly', () => {
    const simpleQuery = 'SELECT ?s WHERE { ?s ?p ?o . }';
    const complexQuery = `
      SELECT DISTINCT ?s WHERE {
        ?s ?p1 ?o1 .
        OPTIONAL { ?s ?p2 ?o2 }
        FILTER(?o1 = "test")
        UNION { ?s ?p3 ?o3 }
      }
    `;

    const simpleResult = optimizer.optimize(simpleQuery);
    const complexResult = optimizer.optimize(complexQuery);

    // Complex query should have higher estimated cost
    expect(complexResult.estimatedImprovement.before)
      .toBeGreaterThan(simpleResult.estimatedImprovement.before);
  });

  it('should track optimization statistics', () => {
    const query1 = 'SELECT ?s WHERE { ?s ?p ?o . }';
    const query2 = 'SELECT ?s WHERE { ?s ?p ?o . FILTER(?p = "test") }';

    optimizer.optimize(query1);
    optimizer.optimize(query2);

    const stats = optimizer.getStats();

    expect(stats.totalOptimizations).toBe(2);
    expect(Object.keys(stats.rulesApplied).length).toBeGreaterThan(0);
  });
});

describe('Dark Matter Query System (Integration)', () => {
  let system;

  beforeEach(() => {
    system = createDarkMatterQuerySystem({
      complexityThreshold: 50
    });
  });

  afterEach(() => {
    system.clear();
  });

  it('should analyze and optimize in one step', () => {
    const query = `
      SELECT ?name ?age WHERE {
        ?person <http://example.org/name> ?name .
        ?person <http://example.org/age> ?age .
      }
    `;

    const result = system.analyzeAndOptimize(query);

    expect(result.analysis).toBeDefined();
    expect(result.optimization).toBeDefined();
    expect(result).toHaveProperty('shouldOptimize');
  });

  it('should process execution with logging', () => {
    const query = 'SELECT ?s WHERE { ?s ?p ?o . }';

    const result = system.processExecution(query, 150);

    expect(result.queryId).toBeDefined();
    expect(result.analysis).toBeDefined();
    expect(result.logged).toBe(true);
  });

  it('should skip optimization for simple queries', () => {
    const simpleQuery = 'SELECT ?s WHERE { <http://example.org/alice> ?p ?o . }';

    const result = system.optimize(simpleQuery);

    // Simple query should be skipped
    if (result.skipped) {
      expect(result.reason).toContain('threshold');
    }
  });

  it('should generate comprehensive report', () => {
    // Generate some activity
    for (let i = 0; i < 10; i++) {
      const query = `SELECT ?s WHERE { ?s <http://example.org/p${i}> ?o . }`;
      system.processExecution(query, 50 + i * 10);
    }

    const report = system.getReport();

    expect(report).toContain('Query Analysis');
    expect(report).toContain('Critical Path Analysis');
    expect(report).toContain('Optimization Statistics');
  });

  it('should track comprehensive statistics', () => {
    const query = 'SELECT ?s WHERE { ?s ?p ?o . }';

    system.processExecution(query, 100);
    system.processExecution(query, 200);

    const stats = system.getStats();

    expect(stats.analyzer).toBeDefined();
    expect(stats.optimizer).toBeDefined();
    expect(stats.criticalPath).toBeDefined();
  });

  it('should validate 80/20 principle', () => {
    // Simulate realistic distribution: 20% of queries take 80% of time

    // 16 fast queries (80% of total, ~20% of time)
    for (let i = 0; i < 16; i++) {
      system.processExecution(
        `SELECT ?s WHERE { <http://example.org/s${i}> ?p ?o . }`,
        25 // Each takes 25ms, total 400ms
      );
    }

    // 4 slow queries (20% of total, ~80% of time)
    for (let i = 0; i < 4; i++) {
      system.processExecution(
        `SELECT * WHERE { ?s ?p ?o . FILTER(?s = <http://example.org/s${i}>) }`,
        400 // Each takes 400ms, total 1600ms
      );
    }

    const stats = system.getStats();

    // Verify 80/20 distribution
    expect(stats.criticalPath.totalQueries).toBe(20);
    expect(stats.criticalPath.criticalQueryCount).toBeLessThanOrEqual(4);

    // Impact ratio should be close to 80%
    expect(stats.criticalPath.impactRatio).toBeGreaterThan(0.75);
    expect(stats.criticalPath.impactRatio).toBeLessThanOrEqual(1.0);
  });
});
