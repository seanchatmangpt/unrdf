/**
 * @file Behavioural tests for the Dark Matter QueryAnalyzer (real SPARQL text in, analysis out).
 */

import { describe, it, expect } from 'vitest';
import QueryAnalyzer, { createQueryAnalyzer } from '../src/dark-matter/query-analyzer.mjs';
import * as entry from '../src/index.mjs';

const SIMPLE = `SELECT ?name WHERE {
  <http://example.org/alice> <http://example.org/name> "Alice" .
}`;

const JOINED = `PREFIX ex: <http://example.org/>
SELECT DISTINCT ?person ?name WHERE {
  ?person ex:knows ?friend .
  ?friend ex:name ?name .
  FILTER (?name != "Bob")
}
ORDER BY ?name`;

describe('package entrypoint', () => {
  it('re-exports the analyzer', () => {
    expect(entry.QueryAnalyzer).toBe(QueryAnalyzer);
    expect(entry.createQueryAnalyzer).toBe(createQueryAnalyzer);
  });
});

describe('QueryAnalyzer.analyze', () => {
  it('classifies the query type and echoes id, query and metadata', () => {
    const a = createQueryAnalyzer();
    const r = a.analyze(SIMPLE, 'q-1', { source: 'test' });
    expect(r.queryId).toBe('q-1');
    expect(r.query).toBe(SIMPLE);
    expect(r.type).toBe('SELECT');
    expect(r.metadata).toEqual({ source: 'test' });
  });

  it('generates an id when none is supplied', () => {
    const r = new QueryAnalyzer().analyze(SIMPLE);
    expect(r.queryId).toMatch(/^query-\d+$/);
  });

  it('extracts fully-bound triple patterns with the lowest complexity', () => {
    const r = new QueryAnalyzer().analyze(SIMPLE);
    expect(r.patterns).toEqual([
      {
        type: 'triple',
        subject: '<http://example.org/alice>',
        predicate: '<http://example.org/name>',
        object: '"Alice"',
        complexity: 1,
      },
    ]);
    expect(r.joins).toEqual([]);
    expect(r.filters).toEqual([]);
  });

  it('scores variables by position: subject +5, predicate +10, object +3', () => {
    const r = new QueryAnalyzer().analyze('SELECT * WHERE { ?s ?p ?o . }');
    expect(r.patterns[0].complexity).toBe(1 + 5 + 10 + 3);
    const objOnly = new QueryAnalyzer().analyze('SELECT ?o WHERE { <http://a/s> <http://a/p> ?o . }');
    expect(objOnly.patterns[0].complexity).toBe(4);
  });

  it('detects shared variables as joins and extracts FILTER expressions', () => {
    const r = new QueryAnalyzer().analyze(JOINED);
    expect(r.filters).toEqual(['?name != "Bob"']);
    const joinVars = r.joins.filter(j => j.type === 'variable-join').flatMap(j => j.variables);
    expect(joinVars).toEqual(expect.arrayContaining(['friend', 'name']));
    expect(joinVars).not.toContain('person');
    expect(r.complexity.patternCount).toBe(2);
    expect(r.complexity.filterCount).toBe(1);
  });

  it('does not treat a variable whose name is a prefix of another as a join', () => {
    // ?s appears once, ?street appears once - no variable is shared
    const q = `SELECT ?s ?street WHERE {
  ?s <http://example.org/p> ?street .
}`;
    const r = new QueryAnalyzer().analyze(q);
    expect(r.joins.filter(j => j.type === 'variable-join')).toEqual([]);
  });

  it('flags OPTIONAL and UNION with their fixed costs', () => {
    const opt = new QueryAnalyzer().analyze(
      'SELECT ?a WHERE { ?a <http://a/p> ?b . OPTIONAL { ?a <http://a/q> ?c } }'
    );
    expect(opt.joins).toContainEqual({ type: 'optional-join', variables: [], estimatedCost: 20 });

    const uni = new QueryAnalyzer().analyze(
      'SELECT ?a WHERE { { ?a <http://a/p> 1 } UNION { ?a <http://a/q> 2 } }'
    );
    expect(uni.joins).toContainEqual({ type: 'union', variables: [], estimatedCost: 30 });
    expect(uni.expensiveOperations.some(o => o.type === 'union' && o.cost === 50)).toBe(true);
  });

  it('extracts aggregation functions in order and counts them', () => {
    const q = `SELECT (COUNT(?s) AS ?n) (avg(?v) AS ?m) WHERE { ?s <http://a/p> ?v . } GROUP BY ?s`;
    const r = new QueryAnalyzer().analyze(q);
    expect(r.aggregations).toEqual(['COUNT', 'AVG']);
    expect(r.complexity.aggregationCount).toBe(2);
  });

  it('multiplies score for GROUP BY / ORDER BY / DISTINCT modifiers', () => {
    const base = new QueryAnalyzer().analyze('SELECT ?s WHERE { ?s <http://a/p> "x" . }');
    const distinct = new QueryAnalyzer().analyze('SELECT DISTINCT ?s WHERE { ?s <http://a/p> "x" . }');
    expect(distinct.complexity.score).toBeGreaterThan(base.complexity.score);
    expect(distinct.complexity.score).toBe(Math.round(base.complexity.score * 1.3));
  });

  it('marks variable predicates as full-scan expensive operations, sorted by cost desc', () => {
    const r = new QueryAnalyzer().analyze('SELECT * WHERE { ?s ?p ?o . }');
    const vp = r.expensiveOperations.find(o => o.type === 'variable-predicate');
    expect(vp).toMatchObject({ cost: 100 });
    expect(vp.reason).toContain('?p');
    const costs = r.expensiveOperations.map(o => o.cost);
    expect(costs).toEqual([...costs].sort((a, b) => b - a));
  });

  it('uses configured cost multipliers', () => {
    const q = 'SELECT ?s WHERE { ?s <http://a/p> ?o . FILTER (?o > 1) }';
    const cheap = new QueryAnalyzer({ filterCostMultiplier: 1 }).analyze(q);
    const dear = new QueryAnalyzer({ filterCostMultiplier: 50 }).analyze(q);
    expect(dear.complexity.score - cheap.complexity.score).toBe(49);
  });

  it('degrades gracefully for non-SELECT text and empty WHERE', () => {
    const r = new QueryAnalyzer().analyze('nonsense');
    expect(r.type).toBe('UNKNOWN');
    expect(r.patterns).toEqual([]);
    expect(r.complexity.score).toBe(0);
  });
});

describe('QueryAnalyzer statistics', () => {
  it('tracks totals, complex/simple split and running average, and can reset', () => {
    const a = new QueryAnalyzer({ complexityThreshold: 20 });
    const simple = a.analyze(SIMPLE);
    const heavy = a.analyze('SELECT * WHERE { ?s ?p ?o . ?o ?q ?r . }');
    const stats = a.getStats();
    expect(stats.totalAnalyzed).toBe(2);
    expect(stats.simpleQueries).toBe(1);
    expect(stats.complexQueries).toBe(1);
    expect(stats.avgComplexity).toBeCloseTo((simple.complexity.score + heavy.complexity.score) / 2, 5);
    expect(stats.complexQueryRatio).toBe(0.5);

    a.resetStats();
    expect(a.getStats()).toMatchObject({ totalAnalyzed: 0, complexQueryRatio: 0, avgComplexity: 0 });
  });
});
