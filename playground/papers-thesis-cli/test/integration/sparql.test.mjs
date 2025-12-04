/**
 * @fileoverview Tests for SPARQL integration layer
 *
 * @description
 * Comprehensive tests for the SPARQL query execution layer including:
 * - Initialization and ontology loading
 * - SELECT, ASK, CONSTRUCT queries
 * - Named queries with parameters
 * - Query caching and performance
 * - Error handling
 *
 * @module test/integration/sparql
 * @version 1.0.0
 * @license MIT
 */

import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest';
import {
  initKnowledgeGraph,
  loadOntology,
  shutdown,
  isInitialized,
  executeSparqlSelect,
  executeSparqlAsk,
  executeSparqlConstruct,
  executeNamedQuery,
  listNamedQueries,
  insertTriples,
  exportAsTurtle,
  getOntologyClasses,
  getOntologyProperties,
  getQueryMetrics,
  resetQueryMetrics,
  getGraphStats,
  getLoadedOntologies,
  clearQueryCache,
  getCacheStats,
  PREFIXES,
  NAMED_QUERIES,
  ONTOLOGY_PATHS,
  createPrefixDeclarations,
  SparqlQueryError,
} from '../../src/integration/sparql.mjs';

describe('SPARQL Integration Layer', () => {
  describe('Module Exports', () => {
    it('should export all required functions', () => {
      expect(initKnowledgeGraph).toBeTypeOf('function');
      expect(loadOntology).toBeTypeOf('function');
      expect(shutdown).toBeTypeOf('function');
      expect(isInitialized).toBeTypeOf('function');
      expect(executeSparqlSelect).toBeTypeOf('function');
      expect(executeSparqlAsk).toBeTypeOf('function');
      expect(executeSparqlConstruct).toBeTypeOf('function');
      expect(executeNamedQuery).toBeTypeOf('function');
      expect(listNamedQueries).toBeTypeOf('function');
      expect(insertTriples).toBeTypeOf('function');
      expect(exportAsTurtle).toBeTypeOf('function');
      expect(getOntologyClasses).toBeTypeOf('function');
      expect(getOntologyProperties).toBeTypeOf('function');
      expect(getQueryMetrics).toBeTypeOf('function');
      expect(resetQueryMetrics).toBeTypeOf('function');
      expect(getGraphStats).toBeTypeOf('function');
      expect(getLoadedOntologies).toBeTypeOf('function');
      expect(clearQueryCache).toBeTypeOf('function');
      expect(getCacheStats).toBeTypeOf('function');
      expect(createPrefixDeclarations).toBeTypeOf('function');
    });

    it('should export constants', () => {
      expect(PREFIXES).toBeDefined();
      expect(PREFIXES.pt).toBe('http://papers-thesis.org/ontology#');
      expect(PREFIXES.ex).toBe('http://papers-thesis.org/examples#');
      expect(PREFIXES.rdf).toBe('http://www.w3.org/1999/02/22-rdf-syntax-ns#');

      expect(ONTOLOGY_PATHS).toBeDefined();
      expect(ONTOLOGY_PATHS.papersThesis).toContain('papers-thesis.ttl');
      expect(ONTOLOGY_PATHS.examples).toContain('examples.ttl');
    });

    it('should export NAMED_QUERIES with 20+ queries', () => {
      expect(NAMED_QUERIES).toBeDefined();
      const queryCount = Object.keys(NAMED_QUERIES).length;
      expect(queryCount).toBeGreaterThanOrEqual(20);
    });

    it('should export SparqlQueryError class', () => {
      expect(SparqlQueryError).toBeDefined();
      const error = new SparqlQueryError('Test error', 'SELECT * WHERE { ?s ?p ?o }');
      expect(error).toBeInstanceOf(Error);
      expect(error.name).toBe('SparqlQueryError');
      expect(error.message).toBe('Test error');
      expect(error.query).toBe('SELECT * WHERE { ?s ?p ?o }');
    });
  });

  describe('Prefix Declarations', () => {
    it('should generate valid SPARQL prefix declarations', () => {
      const prefixes = createPrefixDeclarations();
      expect(prefixes).toContain('PREFIX pt:');
      expect(prefixes).toContain('PREFIX ex:');
      expect(prefixes).toContain('PREFIX rdf:');
      expect(prefixes).toContain('PREFIX rdfs:');
      expect(prefixes).toContain('PREFIX owl:');
      expect(prefixes).toContain('PREFIX foaf:');
      expect(prefixes).toContain('<http://papers-thesis.org/ontology#>');
    });
  });

  describe('Named Queries Registry', () => {
    it('should have query definitions with required fields', () => {
      for (const [name, def] of Object.entries(NAMED_QUERIES)) {
        expect(def.query, `${name} should have query`).toBeDefined();
        expect(def.description, `${name} should have description`).toBeDefined();
        expect(def.params, `${name} should have params array`).toBeInstanceOf(Array);
      }
    });

    it('should include paper queries', () => {
      expect(NAMED_QUERIES.findPapersByAuthor).toBeDefined();
      expect(NAMED_QUERIES.findPapersByFamily).toBeDefined();
      expect(NAMED_QUERIES.listPaperSections).toBeDefined();
      expect(NAMED_QUERIES.searchPapersByKeyword).toBeDefined();
      expect(NAMED_QUERIES.getPaperDetails).toBeDefined();
    });

    it('should include thesis queries', () => {
      expect(NAMED_QUERIES.getThesisByType).toBeDefined();
      expect(NAMED_QUERIES.listAllTheses).toBeDefined();
      expect(NAMED_QUERIES.findUpcomingDefenses).toBeDefined();
      expect(NAMED_QUERIES.getThesisProgress).toBeDefined();
      expect(NAMED_QUERIES.findMilestonesByStatus).toBeDefined();
      expect(NAMED_QUERIES.getOverdueMilestones).toBeDefined();
    });

    it('should include ontology introspection queries', () => {
      expect(NAMED_QUERIES.listOntologyClasses).toBeDefined();
      expect(NAMED_QUERIES.listOntologyProperties).toBeDefined();
      expect(NAMED_QUERIES.ontologyIntrospection).toBeDefined();
    });

    it('should include author and statistics queries', () => {
      expect(NAMED_QUERIES.getAuthorStatistics).toBeDefined();
      expect(NAMED_QUERIES.findCoAuthors).toBeDefined();
    });

    it('should include validation queries (ASK)', () => {
      expect(NAMED_QUERIES.validateIMRADStructure).toBeDefined();
      expect(NAMED_QUERIES.validateIMRADStructure.query).toContain('ASK');
      expect(NAMED_QUERIES.checkPaperHasAuthor).toBeDefined();
      expect(NAMED_QUERIES.checkPaperHasAuthor.query).toContain('ASK');
    });

    it('should include CONSTRUCT queries', () => {
      expect(NAMED_QUERIES.constructPaperSummary).toBeDefined();
      expect(NAMED_QUERIES.constructPaperSummary.query).toContain('CONSTRUCT');
    });

    it('should list named queries correctly', () => {
      const list = listNamedQueries();
      expect(Array.isArray(list)).toBe(true);
      expect(list.length).toBeGreaterThanOrEqual(20);

      const first = list[0];
      expect(first.name).toBeDefined();
      expect(first.description).toBeDefined();
      expect(first.params).toBeInstanceOf(Array);
    });
  });

  describe('Initialization State (Pre-init)', () => {
    beforeEach(() => {
      shutdown(); // Ensure clean state
    });

    it('should report not initialized before init', () => {
      expect(isInitialized()).toBe(false);
    });

    it('should return empty loaded ontologies before init', () => {
      const ontologies = getLoadedOntologies();
      expect(ontologies).toEqual([]);
    });

    it('should return zero stats before init', async () => {
      const stats = await getGraphStats();
      expect(stats.initialized).toBe(false);
      expect(stats.tripleCount).toBe(0);
    });

    it('should throw when executing query before init', async () => {
      await expect(executeSparqlSelect('SELECT * WHERE { ?s ?p ?o } LIMIT 1')).rejects.toThrow(
        'Knowledge graph not initialized'
      );
    });

    it('should throw when executing named query before init', async () => {
      await expect(executeNamedQuery('listAllTheses')).rejects.toThrow(
        'Knowledge graph not initialized'
      );
    });
  });

  describe('Cache Operations (Pre-init)', () => {
    it('should clear cache without error', () => {
      expect(() => clearQueryCache()).not.toThrow();
    });

    it('should get cache stats', () => {
      const stats = getCacheStats();
      expect(stats.size).toBe(0);
      expect(stats.max).toBeGreaterThan(0);
    });
  });

  describe('Query Metrics (Pre-init)', () => {
    it('should return initial metrics', () => {
      resetQueryMetrics();
      const metrics = getQueryMetrics();
      expect(metrics.queriesExecuted).toBe(0);
      expect(metrics.totalQueryTime).toBe(0);
      expect(metrics.avgQueryTime).toBe(0);
      expect(metrics.cacheHits).toBe(0);
      expect(metrics.cacheMisses).toBe(0);
      expect(metrics.hitRate).toBe(0);
    });

    it('should reset metrics', () => {
      resetQueryMetrics();
      const metrics = getQueryMetrics();
      expect(metrics.queriesExecuted).toBe(0);
    });
  });
});

describe('SPARQL Integration Layer (With Initialization)', () => {
  beforeAll(async () => {
    // Initialize with examples for comprehensive testing
    await initKnowledgeGraph({ loadExamples: true });
  });

  afterAll(() => {
    shutdown();
  });

  beforeEach(() => {
    resetQueryMetrics();
    clearQueryCache();
  });

  describe('Initialization', () => {
    it('should be initialized', () => {
      expect(isInitialized()).toBe(true);
    });

    it('should have loaded ontologies', () => {
      const ontologies = getLoadedOntologies();
      expect(ontologies.length).toBeGreaterThan(0);
      expect(ontologies.some(o => o.includes('papers-thesis.ttl'))).toBe(true);
    });

    it('should have graph stats', async () => {
      const stats = await getGraphStats();
      expect(stats.initialized).toBe(true);
      expect(stats.tripleCount).toBeGreaterThan(0);
    });
  });

  describe('SELECT Queries', () => {
    it('should execute basic SELECT query', async () => {
      const results = await executeSparqlSelect('SELECT * WHERE { ?s ?p ?o } LIMIT 5');
      expect(Array.isArray(results)).toBe(true);
      expect(results.length).toBeLessThanOrEqual(5);
    });

    it('should execute SELECT with prefixes auto-added', async () => {
      const results = await executeSparqlSelect(`
        SELECT ?class ?label
        WHERE {
          ?class a owl:Class .
          OPTIONAL { ?class rdfs:label ?label }
        }
        LIMIT 5
      `);
      expect(Array.isArray(results)).toBe(true);
    });

    it('should respect limit option', async () => {
      const results = await executeSparqlSelect('SELECT * WHERE { ?s ?p ?o }', { limit: 3 });
      expect(results.length).toBeLessThanOrEqual(3);
    });

    it('should use cache on repeated queries', async () => {
      const query = 'SELECT * WHERE { ?s ?p ?o } LIMIT 2';

      // First execution
      await executeSparqlSelect(query, { useCache: true });
      const metrics1 = getQueryMetrics();
      expect(metrics1.cacheMisses).toBe(1);
      expect(metrics1.cacheHits).toBe(0);

      // Second execution (should hit cache)
      await executeSparqlSelect(query, { useCache: true });
      const metrics2 = getQueryMetrics();
      expect(metrics2.cacheHits).toBe(1);
    });

    it('should bypass cache when requested', async () => {
      const query = 'SELECT * WHERE { ?s ?p ?o } LIMIT 1';

      await executeSparqlSelect(query, { useCache: false });
      await executeSparqlSelect(query, { useCache: false });

      const metrics = getQueryMetrics();
      expect(metrics.cacheHits).toBe(0);
    });
  });

  describe('ASK Queries', () => {
    it('should execute ASK query returning boolean', async () => {
      const result = await executeSparqlAsk(`
        ASK {
          ?s a owl:Class .
        }
      `);
      expect(typeof result).toBe('boolean');
    });
  });

  describe('CONSTRUCT Queries', () => {
    it('should execute CONSTRUCT query returning Turtle', async () => {
      const result = await executeSparqlConstruct(`
        CONSTRUCT {
          ?s ?p ?o .
        }
        WHERE {
          ?s ?p ?o .
        }
        LIMIT 5
      `);
      expect(typeof result).toBe('string');
    });
  });

  describe('Named Queries', () => {
    it('should execute named query without parameters', async () => {
      const results = await executeNamedQuery('listAllTheses');
      expect(Array.isArray(results)).toBe(true);
    });

    it('should execute named query with parameters', async () => {
      const results = await executeNamedQuery('findPapersByFamily', { family: 'imrad' });
      expect(Array.isArray(results)).toBe(true);
    });

    it('should throw for unknown named query', async () => {
      await expect(executeNamedQuery('nonExistentQuery')).rejects.toThrow('Unknown named query');
    });

    it('should throw for missing required parameter', async () => {
      await expect(executeNamedQuery('findPapersByFamily', {})).rejects.toThrow(
        'Missing required parameter'
      );
    });
  });

  describe('Ontology Introspection', () => {
    it('should get ontology classes', async () => {
      const classes = await getOntologyClasses();
      expect(Array.isArray(classes)).toBe(true);
    });

    it('should get ontology properties', async () => {
      const properties = await getOntologyProperties();
      expect(Array.isArray(properties)).toBe(true);
    });
  });

  describe('Data Operations', () => {
    it('should insert triples', async () => {
      const turtle = `
        @prefix pt: <http://papers-thesis.org/ontology#> .
        @prefix ex: <http://papers-thesis.org/examples#> .

        ex:test-paper-insert a pt:Paper ;
          pt:hasTitle "Test Paper for Insert" ;
          pt:paperFamily "imrad" .
      `;

      const result = await insertTriples(turtle);
      expect(result.success).toBe(true);
      expect(result.tripleCount).toBeGreaterThan(0);
    });

    it('should export as Turtle', async () => {
      const turtle = await exportAsTurtle();
      expect(typeof turtle).toBe('string');
    });
  });

  describe('Query Metrics', () => {
    it('should track query execution count', async () => {
      resetQueryMetrics();

      await executeSparqlSelect('SELECT * WHERE { ?s ?p ?o } LIMIT 1', { useCache: false });
      await executeSparqlSelect('SELECT * WHERE { ?s ?p ?o } LIMIT 2', { useCache: false });

      const metrics = getQueryMetrics();
      expect(metrics.queriesExecuted).toBe(2);
    });

    it('should track total query time', async () => {
      resetQueryMetrics();

      await executeSparqlSelect('SELECT * WHERE { ?s ?p ?o } LIMIT 1', { useCache: false });

      const metrics = getQueryMetrics();
      expect(metrics.totalQueryTime).toBeGreaterThanOrEqual(0);
    });

    it('should calculate average query time', async () => {
      resetQueryMetrics();

      await executeSparqlSelect('SELECT * WHERE { ?s ?p ?o } LIMIT 1', { useCache: false });
      await executeSparqlSelect('SELECT * WHERE { ?s ?p ?o } LIMIT 1', { useCache: false });

      const metrics = getQueryMetrics();
      expect(metrics.avgQueryTime).toBeGreaterThanOrEqual(0);
    });

    it('should calculate cache hit rate', async () => {
      resetQueryMetrics();
      clearQueryCache();

      const query = 'SELECT * WHERE { ?s ?p ?o } LIMIT 1';

      // Cache miss
      await executeSparqlSelect(query, { useCache: true });
      // Cache hit
      await executeSparqlSelect(query, { useCache: true });

      const metrics = getQueryMetrics();
      expect(metrics.hitRate).toBe(50);
    });
  });

  describe('Error Handling', () => {
    it('should throw SparqlQueryError for invalid query', async () => {
      try {
        await executeSparqlSelect('INVALID SPARQL QUERY');
        expect.fail('Should have thrown');
      } catch (error) {
        expect(error).toBeInstanceOf(SparqlQueryError);
        expect(error.query).toBeDefined();
      }
    });
  });
});
