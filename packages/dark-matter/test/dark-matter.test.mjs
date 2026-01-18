/**
 * @file Dark Matter Tests - Query Analyzer
 * @description Comprehensive tests for SPARQL query analysis and optimization
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { QueryAnalyzer, createQueryAnalyzer } from '../src/index.mjs';

describe('QueryAnalyzer - Initialization', () => {
  it('should create analyzer with default config', () => {
    const analyzer = new QueryAnalyzer();

    expect(analyzer).toBeDefined();
    expect(analyzer.config.complexityThreshold).toBe(100);
    expect(analyzer.config.expensiveOperationThreshold).toBe(50);
  });

  it('should create analyzer with custom config', () => {
    const analyzer = new QueryAnalyzer({
      complexityThreshold: 200,
      expensiveOperationThreshold: 75,
    });

    expect(analyzer.config.complexityThreshold).toBe(200);
    expect(analyzer.config.expensiveOperationThreshold).toBe(75);
  });

  it('should create analyzer via factory function', () => {
    const analyzer = createQueryAnalyzer();
    expect(analyzer).toBeInstanceOf(QueryAnalyzer);
  });

  it('should initialize stats', () => {
    const analyzer = new QueryAnalyzer();
    const stats = analyzer.getStats();

    expect(stats.totalAnalyzed).toBe(0);
    expect(stats.complexQueries).toBe(0);
    expect(stats.simpleQueries).toBe(0);
    expect(stats.avgComplexity).toBe(0);
  });
});

describe('QueryAnalyzer - Simple Queries', () => {
  let analyzer;

  beforeEach(() => {
    analyzer = new QueryAnalyzer();
  });

  it('should analyze simple SELECT query', () => {
    const query = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      SELECT ?name WHERE {
        ?person foaf:name ?name .
      }
    `;

    const result = analyzer.analyze(query);

    expect(result).toBeDefined();
    expect(result.type).toBe('SELECT');
    expect(result.patterns.length).toBeGreaterThan(0);
    expect(result.complexity.score).toBeGreaterThan(0);
  });

  it('should analyze ASK query', () => {
    const query = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      ASK WHERE {
        ?person foaf:name "Alice" .
      }
    `;

    const result = analyzer.analyze(query);
    expect(result.type).toBe('ASK');
  });

  it('should analyze CONSTRUCT query', () => {
    const query = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      CONSTRUCT { ?person a foaf:Person }
      WHERE {
        ?person foaf:name ?name .
      }
    `;

    const result = analyzer.analyze(query);
    expect(result.type).toBe('CONSTRUCT');
  });

  it('should extract triple patterns', () => {
    const query = `
      SELECT ?s ?p ?o WHERE {
        ?s ?p ?o .
        ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?o .
      }
    `;

    const result = analyzer.analyze(query);

    expect(result.patterns.length).toBe(2);
    expect(result.patterns[0].type).toBe('triple');
  });
});

describe('QueryAnalyzer - Pattern Complexity', () => {
  let analyzer;

  beforeEach(() => {
    analyzer = new QueryAnalyzer();
  });

  it('should calculate higher complexity for variable predicates', () => {
    const queryWithVariablePredicate = `
      SELECT ?s ?p ?o WHERE {
        ?s ?p ?o .
      }
    `;

    const result = analyzer.analyze(queryWithVariablePredicate);

    expect(result.complexity.score).toBeGreaterThan(10);
    expect(result.expensiveOperations.some(op => op.type === 'variable-predicate')).toBe(true);
  });

  it('should calculate lower complexity for fixed predicates', () => {
    const queryWithFixedPredicate = `
      SELECT ?s ?o WHERE {
        ?s <http://example.org/predicate> ?o .
      }
    `;

    const result = analyzer.analyze(queryWithFixedPredicate);

    expect(result.patterns[0].complexity).toBeLessThan(15);
  });

  it('should detect expensive variable predicate pattern', () => {
    const query = `
      SELECT ?subject ?predicate ?object WHERE {
        ?subject ?predicate ?object .
      }
    `;

    const result = analyzer.analyze(query);

    const varPredicateOps = result.expensiveOperations.filter(
      op => op.type === 'variable-predicate'
    );

    expect(varPredicateOps.length).toBeGreaterThan(0);
    expect(varPredicateOps[0].cost).toBe(100);
  });
});

describe('QueryAnalyzer - Filters', () => {
  let analyzer;

  beforeEach(() => {
    analyzer = new QueryAnalyzer();
  });

  it('should extract FILTER clauses', () => {
    const query = `
      SELECT ?person ?age WHERE {
        ?person <http://example.org/age> ?age .
        FILTER (?age > 18)
      }
    `;

    const result = analyzer.analyze(query);

    expect(result.filters.length).toBe(1);
    expect(result.filters[0]).toContain('?age > 18');
  });

  it('should extract multiple FILTER clauses', () => {
    const query = `
      SELECT ?person ?age ?name WHERE {
        ?person <http://example.org/age> ?age .
        ?person <http://example.org/name> ?name .
        FILTER (?age > 18)
        FILTER (REGEX(?name, "^A"))
      }
    `;

    const result = analyzer.analyze(query);

    expect(result.filters.length).toBe(2);
    expect(result.complexity.filterCount).toBe(2);
  });

  it('should include filter cost in complexity', () => {
    const queryWithoutFilter = `
      SELECT ?person WHERE {
        ?person <http://example.org/name> "Alice" .
      }
    `;

    const queryWithFilter = `
      SELECT ?person ?age WHERE {
        ?person <http://example.org/name> "Alice" .
        ?person <http://example.org/age> ?age .
        FILTER (?age > 18)
        FILTER (?age < 65)
      }
    `;

    const result1 = analyzer.analyze(queryWithoutFilter);
    const result2 = analyzer.analyze(queryWithFilter);

    expect(result2.complexity.score).toBeGreaterThan(result1.complexity.score);
  });
});

describe('QueryAnalyzer - Joins', () => {
  let analyzer;

  beforeEach(() => {
    analyzer = new QueryAnalyzer();
  });

  it('should detect variable joins', () => {
    const query = `
      SELECT ?person ?friend WHERE {
        ?person <http://xmlns.com/foaf/0.1/knows> ?friend .
        ?person <http://xmlns.com/foaf/0.1/name> "Alice" .
        ?friend <http://xmlns.com/foaf/0.1/name> ?friendName .
      }
    `;

    const result = analyzer.analyze(query);

    expect(result.joins.length).toBeGreaterThan(0);
    expect(result.joins.some(j => j.type === 'variable-join')).toBe(true);
  });

  it('should detect OPTIONAL joins', () => {
    const query = `
      SELECT ?person ?email WHERE {
        ?person <http://xmlns.com/foaf/0.1/name> ?name .
        OPTIONAL { ?person <http://xmlns.com/foaf/0.1/mbox> ?email }
      }
    `;

    const result = analyzer.analyze(query);

    expect(result.joins.some(j => j.type === 'optional-join')).toBe(true);
  });

  it('should detect UNION', () => {
    const query = `
      SELECT ?contact WHERE {
        { ?contact <http://xmlns.com/foaf/0.1/mbox> ?email }
        UNION
        { ?contact <http://xmlns.com/foaf/0.1/phone> ?phone }
      }
    `;

    const result = analyzer.analyze(query);

    expect(result.joins.some(j => j.type === 'union')).toBe(true);
  });

  it('should assign high cost to UNION', () => {
    const query = `
      SELECT ?x WHERE {
        { ?x <http://example.org/p1> ?y }
        UNION
        { ?x <http://example.org/p2> ?z }
      }
    `;

    const result = analyzer.analyze(query);

    const unionJoin = result.joins.find(j => j.type === 'union');
    expect(unionJoin.estimatedCost).toBe(30);
  });
});

describe('QueryAnalyzer - Aggregations', () => {
  let analyzer;

  beforeEach(() => {
    analyzer = new QueryAnalyzer();
  });

  it('should detect COUNT aggregation', () => {
    const query = `
      SELECT (COUNT(?person) AS ?count) WHERE {
        ?person <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .
      }
    `;

    const result = analyzer.analyze(query);

    expect(result.aggregations).toContain('COUNT');
  });

  it('should detect multiple aggregations', () => {
    const query = `
      SELECT (COUNT(?person) AS ?count) (AVG(?age) AS ?avgAge) (MAX(?salary) AS ?maxSalary) WHERE {
        ?person <http://example.org/age> ?age .
        ?person <http://example.org/salary> ?salary .
      }
    `;

    const result = analyzer.analyze(query);

    expect(result.aggregations).toContain('COUNT');
    expect(result.aggregations).toContain('AVG');
    expect(result.aggregations).toContain('MAX');
    expect(result.complexity.aggregationCount).toBe(3);
  });
});

describe('QueryAnalyzer - Complexity Scoring', () => {
  let analyzer;

  beforeEach(() => {
    analyzer = new QueryAnalyzer();
  });

  it('should classify simple queries correctly', () => {
    const simpleQuery = `
      SELECT ?name WHERE {
        <http://example.org/person/1> <http://xmlns.com/foaf/0.1/name> ?name .
      }
    `;

    const result = analyzer.analyze(simpleQuery);
    const stats = analyzer.getStats();

    expect(result.complexity.score).toBeLessThan(100);
    expect(stats.simpleQueries).toBe(1);
    expect(stats.complexQueries).toBe(0);
  });

  it('should classify complex queries correctly', () => {
    const complexQuery = `
      SELECT ?person (COUNT(?friend) AS ?friendCount) WHERE {
        ?person ?p1 ?o1 .
        ?person <http://xmlns.com/foaf/0.1/knows> ?friend .
        ?friend ?p2 ?o2 .
        ?friend <http://example.org/age> ?age .
        FILTER (?age > 18)
        FILTER (?age < 65)
        OPTIONAL { ?person <http://xmlns.com/foaf/0.1/mbox> ?email }
      }
      GROUP BY ?person
    `;

    const result = analyzer.analyze(complexQuery);
    const stats = analyzer.getStats();

    expect(result.complexity.score).toBeGreaterThanOrEqual(100);
    expect(stats.complexQueries).toBe(1);
  });

  it('should track average complexity', () => {
    analyzer.analyze('SELECT ?s WHERE { ?s ?p ?o }');
    analyzer.analyze('SELECT ?s WHERE { ?s <http://example.org/name> ?name }');

    const stats = analyzer.getStats();

    expect(stats.avgComplexity).toBeGreaterThan(0);
    expect(stats.totalAnalyzed).toBe(2);
  });
});

describe('QueryAnalyzer - Expensive Operations', () => {
  let analyzer;

  beforeEach(() => {
    analyzer = new QueryAnalyzer();
  });

  it('should identify UNION as expensive', () => {
    const query = `
      SELECT ?x WHERE {
        { ?x <http://example.org/p1> ?y }
        UNION
        { ?x <http://example.org/p2> ?z }
      }
    `;

    const result = analyzer.analyze(query);

    expect(result.expensiveOperations.some(op => op.type === 'union')).toBe(true);
  });

  it('should warn about unfiltered queries with high cardinality', () => {
    const query = `
      SELECT ?s ?p ?o WHERE {
        ?s ?p ?o .
      }
    `;

    const result = analyzer.analyze(query);

    // Should have high estimated rows and be flagged as unfiltered
    expect(result.complexity.estimatedRows).toBeGreaterThan(1000);
  });

  it('should sort expensive operations by cost', () => {
    const query = `
      SELECT ?s ?p ?o WHERE {
        ?s ?p ?o .
        ?s <http://example.org/knows> ?friend .
        FILTER (?s = "test")
      }
      UNION
      SELECT ?x WHERE { ?x <http://example.org/type> ?t }
    `;

    const result = analyzer.analyze(query);

    // Operations should be sorted by cost descending
    for (let i = 1; i < result.expensiveOperations.length; i++) {
      expect(result.expensiveOperations[i - 1].cost).toBeGreaterThanOrEqual(
        result.expensiveOperations[i].cost
      );
    }
  });
});

describe('QueryAnalyzer - Statistics', () => {
  let analyzer;

  beforeEach(() => {
    analyzer = new QueryAnalyzer();
  });

  it('should track total analyzed queries', () => {
    analyzer.analyze('SELECT ?s WHERE { ?s ?p ?o }');
    analyzer.analyze('SELECT ?s WHERE { ?s ?p ?o }');
    analyzer.analyze('SELECT ?s WHERE { ?s ?p ?o }');

    const stats = analyzer.getStats();
    expect(stats.totalAnalyzed).toBe(3);
  });

  it('should calculate complex query ratio', () => {
    // 2 simple queries
    analyzer.analyze('SELECT ?s WHERE { <http://example.org/1> ?p ?o }');
    analyzer.analyze('SELECT ?s WHERE { <http://example.org/2> ?p ?o }');

    // 1 complex query
    analyzer.analyze(`
      SELECT ?s WHERE {
        ?s ?p1 ?o1 .
        ?s ?p2 ?o2 .
        ?s ?p3 ?o3 .
        FILTER (?o1 > 10)
        OPTIONAL { ?s ?p4 ?o4 }
      }
    `);

    const stats = analyzer.getStats();
    expect(stats.complexQueryRatio).toBeCloseTo(0.333, 1);
  });

  it('should reset statistics', () => {
    analyzer.analyze('SELECT ?s WHERE { ?s ?p ?o }');
    analyzer.analyze('SELECT ?s WHERE { ?s ?p ?o }');

    analyzer.resetStats();

    const stats = analyzer.getStats();
    expect(stats.totalAnalyzed).toBe(0);
    expect(stats.complexQueries).toBe(0);
    expect(stats.simpleQueries).toBe(0);
    expect(stats.avgComplexity).toBe(0);
  });
});

describe('QueryAnalyzer - Zod Validation', () => {
  let analyzer;

  beforeEach(() => {
    analyzer = new QueryAnalyzer();
  });

  it('should return valid schema-compliant result', () => {
    const query = 'SELECT ?s WHERE { ?s ?p ?o }';
    const result = analyzer.analyze(query);

    expect(result).toHaveProperty('queryId');
    expect(result).toHaveProperty('query');
    expect(result).toHaveProperty('type');
    expect(result).toHaveProperty('patterns');
    expect(result).toHaveProperty('filters');
    expect(result).toHaveProperty('joins');
    expect(result).toHaveProperty('aggregations');
    expect(result).toHaveProperty('complexity');
    expect(result).toHaveProperty('expensiveOperations');
    expect(result).toHaveProperty('timestamp');

    expect(Array.isArray(result.patterns)).toBe(true);
    expect(Array.isArray(result.filters)).toBe(true);
    expect(Array.isArray(result.joins)).toBe(true);
    expect(Array.isArray(result.aggregations)).toBe(true);
    expect(Array.isArray(result.expensiveOperations)).toBe(true);

    expect(typeof result.complexity.score).toBe('number');
  });

  it('should accept custom queryId', () => {
    const query = 'SELECT ?s WHERE { ?s ?p ?o }';
    const result = analyzer.analyze(query, 'custom-query-id-123');

    expect(result.queryId).toBe('custom-query-id-123');
  });

  it('should generate queryId if not provided', () => {
    const query = 'SELECT ?s WHERE { ?s ?p ?o }';
    const result = analyzer.analyze(query);

    expect(result.queryId).toMatch(/^query-\d+$/);
  });
});

describe('QueryAnalyzer - Performance', () => {
  let analyzer;

  beforeEach(() => {
    analyzer = new QueryAnalyzer();
  });

  it('should analyze query in <10ms', () => {
    const query = `
      SELECT ?person ?friend ?age WHERE {
        ?person <http://xmlns.com/foaf/0.1/knows> ?friend .
        ?person <http://xmlns.com/foaf/0.1/name> "Alice" .
        ?friend <http://example.org/age> ?age .
        FILTER (?age > 18)
      }
    `;

    const start = performance.now();
    analyzer.analyze(query);
    const duration = performance.now() - start;

    expect(duration).toBeLessThan(10);
  });

  it('should handle 100 queries in <100ms', () => {
    const query = 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }';

    const start = performance.now();
    for (let i = 0; i < 100; i++) {
      analyzer.analyze(query);
    }
    const duration = performance.now() - start;

    expect(duration).toBeLessThan(100);
  });
});
