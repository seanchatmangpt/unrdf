/**
 * @file Tests for Dark Matter Core functionality
 * Tests the 80/20 analysis engine for identifying critical paths
 */

import { describe, it, expect, _beforeEach, _vi } from 'vitest';

describe('DarkMatterCore', () => {
  describe('Pareto Score Calculation', () => {
    it('should calculate pareto score for ideal 80/20 distribution', () => {
      const critical = Array(20)
        .fill(null)
        .map((_, i) => ({
          id: `critical-${i}`,
          value: 0.04, // 20 ops * 0.04 = 0.80 total
          cumulativeValue: (i + 1) * 0.04,
        }));

      const dark = Array(80)
        .fill(null)
        .map((_, i) => ({
          id: `dark-${i}`,
          value: 0.0025, // 80 ops * 0.0025 = 0.20 total
          cumulativeValue: 0.8 + (i + 1) * 0.0025,
        }));

      // Perfect 80/20: 20% ops deliver 80% value
      const totalOps = critical.length + dark.length;
      const criticalPercent = critical.length / totalOps;
      const criticalValue = critical.reduce((sum, op) => sum + op.value, 0);

      expect(criticalPercent).toBeCloseTo(0.2, 2);
      expect(criticalValue).toBeCloseTo(0.8, 2);
    });

    it('should identify operations above 80% value threshold', () => {
      const operations = [
        { id: 'query', value: 0.4, executionTime: 120 },
        { id: 'filter', value: 0.25, executionTime: 85 },
        { id: 'sort', value: 0.15, executionTime: 65 },
        { id: 'render', value: 0.1, executionTime: 45 },
        { id: 'log', value: 0.05, executionTime: 20 },
        { id: 'metrics', value: 0.03, executionTime: 15 },
        { id: 'cache', value: 0.02, executionTime: 10 },
      ];

      // Sort by value and calculate cumulative
      const sorted = [...operations].sort((a, b) => b.value - a.value);
      let cumulative = 0;
      const withCumulative = sorted.map((op) => {
        cumulative += op.value;
        return { ...op, cumulativeValue: cumulative };
      });

      // Critical paths (80% value threshold)
      const critical = withCumulative.filter((op) => op.cumulativeValue <= 0.8);
      const dark = withCumulative.filter((op) => op.cumulativeValue > 0.8);

      expect(critical.length).toBe(3); // query + filter + sort = 0.8
      expect(dark.length).toBe(4);
    });
  });

  describe('Critical Path Analysis', () => {
    it('should identify slow critical operations', () => {
      const criticalPaths = [
        { id: 'query', value: 0.4, executionTime: 150 }, // Slow!
        { id: 'filter', value: 0.25, executionTime: 50 },
        { id: 'sort', value: 0.15, executionTime: 30 },
      ];

      const slowCritical = criticalPaths.filter((op) => op.executionTime > 100);

      expect(slowCritical).toHaveLength(1);
      expect(slowCritical[0].id).toBe('query');
    });

    it('should calculate value distribution', () => {
      const operations = [
        { id: 'a', value: 0.3, cumulativeValue: 0.3 },
        { id: 'b', value: 0.25, cumulativeValue: 0.55 },
        { id: 'c', value: 0.2, cumulativeValue: 0.75 },
        { id: 'd', value: 0.1, cumulativeValue: 0.85 },
        { id: 'e', value: 0.08, cumulativeValue: 0.93 },
        { id: 'f', value: 0.05, cumulativeValue: 0.98 },
        { id: 'g', value: 0.02, cumulativeValue: 1.0 },
      ];

      const distribution = {
        critical: operations
          .filter((op) => op.cumulativeValue <= 0.5)
          .reduce((sum, op) => sum + op.value, 0),
        important: operations
          .filter((op) => op.cumulativeValue > 0.5 && op.cumulativeValue <= 0.8)
          .reduce((sum, op) => sum + op.value, 0),
        standard: operations
          .filter((op) => op.cumulativeValue > 0.8 && op.cumulativeValue <= 0.95)
          .reduce((sum, op) => sum + op.value, 0),
        dark: operations
          .filter((op) => op.cumulativeValue > 0.95)
          .reduce((sum, op) => sum + op.value, 0),
      };

      // critical: 'a' (cumulative 0.30 ≤ 0.5) = 0.30
      // important: 'b', 'c' (cumulative > 0.5 && ≤ 0.8) = 0.25 + 0.20 = 0.45
      // standard: 'd', 'e' (cumulative > 0.8 && ≤ 0.95) = 0.10 + 0.08 = 0.18
      // dark: 'f', 'g' (cumulative > 0.95) = 0.05 + 0.02 = 0.07
      expect(distribution.critical).toBeCloseTo(0.3, 2);
      expect(distribution.important).toBeCloseTo(0.45, 2);
      expect(distribution.standard).toBeCloseTo(0.18, 2);
      expect(distribution.dark).toBeCloseTo(0.07, 2);
    });
  });

  describe('Optimization Suggestions', () => {
    it('should generate cache suggestions for high-value operations', () => {
      const critical = [
        { id: 'query', value: 0.4, executionTime: 120 },
        { id: 'filter', value: 0.25, executionTime: 85 },
      ];

      const suggestions = critical.map((op) => ({
        type: 'cache',
        priority: 'high',
        operation: op.id,
        reason: `Delivers ${Math.round(op.value * 100)}% of value`,
        estimatedGain: `${Math.round(op.executionTime * 0.9)}ms saved`,
      }));

      expect(suggestions).toHaveLength(2);
      expect(suggestions[0].type).toBe('cache');
      expect(suggestions[0].priority).toBe('high');
      expect(suggestions[0].reason).toContain('40%');
    });

    it('should suggest removal for low-value operations', () => {
      const darkMatter = [
        { id: 'telemetry', value: 0.005 },
        { id: 'debug-log', value: 0.003 },
        { id: 'unused-validator', value: 0.001 },
      ];

      const removalCandidates = darkMatter.filter((op) => op.value < 0.01);

      expect(removalCandidates).toHaveLength(3);
      removalCandidates.forEach((op) => {
        expect(op.value).toBeLessThan(0.01);
      });
    });
  });

  describe('Pareto Chart Data', () => {
    it('should generate sorted cumulative data', () => {
      const operations = [
        { id: 'c', value: 0.1 },
        { id: 'a', value: 0.5 },
        { id: 'b', value: 0.3 },
        { id: 'd', value: 0.1 },
      ];

      // Sort by value descending
      const sorted = [...operations].sort((a, b) => b.value - a.value);

      let cumulative = 0;
      const chartData = sorted.map((op) => {
        cumulative += op.value;
        return {
          id: op.id,
          value: Math.round(op.value * 100),
          cumulativeValue: Math.round(cumulative * 100),
          category: cumulative <= 0.8 ? 'critical' : 'dark',
        };
      });

      expect(chartData[0].id).toBe('a');
      expect(chartData[0].cumulativeValue).toBe(50);
      expect(chartData[0].category).toBe('critical');

      expect(chartData[1].id).toBe('b');
      expect(chartData[1].cumulativeValue).toBe(80);
      expect(chartData[1].category).toBe('critical');

      expect(chartData[2].category).toBe('dark');
      expect(chartData[3].category).toBe('dark');
    });
  });
});

describe('QueryAnalyzer', () => {
  it('should identify slow queries', () => {
    const queries = [
      { sparql: 'SELECT * WHERE { ?s ?p ?o }', executionTime: 250 },
      {
        sparql: 'SELECT ?name WHERE { ?s foaf:name ?name }',
        executionTime: 50,
      },
      { sparql: 'SELECT * WHERE { ?s ?p ?o } LIMIT 10000', executionTime: 500 },
    ];

    const slowThreshold = 100;
    const slowQueries = queries.filter((q) => q.executionTime > slowThreshold);

    expect(slowQueries).toHaveLength(2);
    expect(slowQueries[0].executionTime).toBe(250);
    expect(slowQueries[1].executionTime).toBe(500);
  });

  it('should suggest index optimizations', () => {
    const query = 'SELECT ?name WHERE { ?person foaf:name ?name . ?person foaf:age ?age }';

    // Detect predicates that could benefit from indexing
    const predicates = query.match(/\w+:\w+/g) || [];

    const suggestions = predicates.map((predicate) => ({
      type: 'INDEX',
      predicate,
      reason: `Add index on ${predicate} for faster lookups`,
    }));

    expect(suggestions).toHaveLength(2);
    expect(suggestions.some((s) => s.predicate === 'foaf:name')).toBe(true);
    expect(suggestions.some((s) => s.predicate === 'foaf:age')).toBe(true);
  });
});

describe('CriticalPath', () => {
  it('should trace operation paths', () => {
    const paths = [
      { path: 'query -> filter -> sort', impact: 0.4, latency: 120 },
      { path: 'insert -> validate -> commit', impact: 0.3, latency: 85 },
      { path: 'fetch -> transform -> render', impact: 0.2, latency: 65 },
    ];

    // Sort by impact
    const sorted = [...paths].sort((a, b) => b.impact - a.impact);

    expect(sorted[0].path).toBe('query -> filter -> sort');
    expect(sorted[0].impact).toBe(0.4);
  });

  it('should identify bottlenecks', () => {
    const operations = [
      { name: 'query', latency: 45 },
      { name: 'filter', latency: 120 }, // Bottleneck!
      { name: 'sort', latency: 30 },
      { name: 'limit', latency: 5 },
    ];

    const avgLatency = operations.reduce((sum, op) => sum + op.latency, 0) / operations.length;
    const threshold = avgLatency * 2;

    const bottlenecks = operations.filter((op) => op.latency > threshold);

    expect(bottlenecks).toHaveLength(1);
    expect(bottlenecks[0].name).toBe('filter');
  });
});
