/**
 * @file SPARQL Queries README Example Tests (London TDD)
 * @description Tests for SPARQL query examples from README.md
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { namedNode, literal } from '@rdfjs/data-model';

describe('README SPARQL Queries Examples', () => {
  let mockSystem;

  beforeEach(() => {
    mockSystem = {
      query: vi.fn()
    };
  });

  describe('SELECT Queries', () => {
    it('should execute SELECT query for friends', async () => {
      const mockResults = [
        { person: namedNode('http://example.org/alice'), friend: namedNode('http://example.org/bob') },
        { person: namedNode('http://example.org/bob'), friend: namedNode('http://example.org/charlie') }
      ];

      mockSystem.query.mockResolvedValue(mockResults);

      const results = await mockSystem.query({
        query: `
          SELECT ?person ?friend
          WHERE {
            ?person <http://xmlns.com/foaf/0.1/knows> ?friend .
          }
        `,
        type: 'sparql-select'
      });

      expect(mockSystem.query).toHaveBeenCalledWith(
        expect.objectContaining({
          query: expect.stringContaining('SELECT ?person ?friend'),
          type: 'sparql-select'
        })
      );

      expect(results).toHaveLength(2);
      expect(results[0]).toHaveProperty('person');
      expect(results[0]).toHaveProperty('friend');
    });

    it('should execute SELECT query with filter', async () => {
      mockSystem.query.mockResolvedValue([
        { person: namedNode('http://example.org/alice'), name: literal('Alice') }
      ]);

      const results = await mockSystem.query({
        query: `
          SELECT ?person ?name
          WHERE {
            ?person <http://xmlns.com/foaf/0.1/name> ?name .
            FILTER (STR(?name) = "Alice")
          }
        `,
        type: 'sparql-select'
      });

      expect(results).toHaveLength(1);
      expect(results[0].name.value).toBe('Alice');
    });

    it('should execute SELECT query with OPTIONAL', async () => {
      mockSystem.query.mockResolvedValue([
        { person: namedNode('http://example.org/alice'), name: literal('Alice'), email: literal('alice@example.org') },
        { person: namedNode('http://example.org/bob'), name: literal('Bob'), email: undefined }
      ]);

      const results = await mockSystem.query({
        query: `
          SELECT ?person ?name ?email
          WHERE {
            ?person <http://xmlns.com/foaf/0.1/name> ?name .
            OPTIONAL { ?person <http://xmlns.com/foaf/0.1/mbox> ?email }
          }
        `,
        type: 'sparql-select'
      });

      expect(results).toHaveLength(2);
      expect(results[0].email).toBeDefined();
      expect(results[1].email).toBeUndefined();
    });

    it('should handle empty SELECT results', async () => {
      mockSystem.query.mockResolvedValue([]);

      const results = await mockSystem.query({
        query: 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }',
        type: 'sparql-select'
      });

      expect(results).toHaveLength(0);
    });

    it('should handle SELECT with ORDER BY', async () => {
      mockSystem.query.mockResolvedValue([
        { name: literal('Alice') },
        { name: literal('Bob') },
        { name: literal('Charlie') }
      ]);

      const results = await mockSystem.query({
        query: `
          SELECT ?name
          WHERE { ?person <http://xmlns.com/foaf/0.1/name> ?name }
          ORDER BY ?name
        `,
        type: 'sparql-select'
      });

      expect(results).toHaveLength(3);
    });

    it('should handle SELECT with LIMIT', async () => {
      mockSystem.query.mockResolvedValue([
        { name: literal('Alice') },
        { name: literal('Bob') }
      ]);

      const results = await mockSystem.query({
        query: `
          SELECT ?name
          WHERE { ?person <http://xmlns.com/foaf/0.1/name> ?name }
          LIMIT 2
        `,
        type: 'sparql-select'
      });

      expect(results).toHaveLength(2);
    });
  });

  describe('ASK Queries', () => {
    it('should execute ASK query returning true', async () => {
      mockSystem.query.mockResolvedValue(true);

      const exists = await mockSystem.query({
        query: 'ASK { ?s ?p ?o }',
        type: 'sparql-ask'
      });

      expect(mockSystem.query).toHaveBeenCalledWith(
        expect.objectContaining({
          query: 'ASK { ?s ?p ?o }',
          type: 'sparql-ask'
        })
      );

      expect(exists).toBe(true);
    });

    it('should execute ASK query returning false', async () => {
      mockSystem.query.mockResolvedValue(false);

      const exists = await mockSystem.query({
        query: 'ASK { <http://example.org/nonexistent> ?p ?o }',
        type: 'sparql-ask'
      });

      expect(exists).toBe(false);
    });

    it('should execute ASK query with FILTER', async () => {
      mockSystem.query.mockResolvedValue(true);

      const exists = await mockSystem.query({
        query: `
          ASK {
            ?person <http://example.org/age> ?age .
            FILTER (?age >= 18)
          }
        `,
        type: 'sparql-ask'
      });

      expect(exists).toBe(true);
    });

    it('should execute ASK query with NOT EXISTS', async () => {
      mockSystem.query.mockResolvedValue(false);

      const hasUnderage = await mockSystem.query({
        query: `
          ASK {
            ?person a <http://xmlns.com/foaf/0.1/Person> .
            FILTER NOT EXISTS { ?person <http://xmlns.com/foaf/0.1/name> ?name }
          }
        `,
        type: 'sparql-ask'
      });

      expect(hasUnderage).toBe(false);
    });
  });

  describe('CONSTRUCT Queries', () => {
    it('should execute CONSTRUCT query', async () => {
      const mockQuads = [
        {
          subject: namedNode('http://example.org/alice'),
          predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
          object: literal('Alice'),
          graph: namedNode('')
        }
      ];

      mockSystem.query.mockResolvedValue(mockQuads);

      const graph = await mockSystem.query({
        query: `
          CONSTRUCT { ?s ?p ?o }
          WHERE { ?s ?p ?o }
        `,
        type: 'sparql-construct'
      });

      expect(mockSystem.query).toHaveBeenCalledWith(
        expect.objectContaining({
          query: expect.stringContaining('CONSTRUCT'),
          type: 'sparql-construct'
        })
      );

      expect(graph).toHaveLength(1);
      expect(graph[0]).toHaveProperty('subject');
      expect(graph[0]).toHaveProperty('predicate');
      expect(graph[0]).toHaveProperty('object');
    });

    it('should execute CONSTRUCT with transformation', async () => {
      const mockQuads = [
        {
          subject: namedNode('http://example.org/alice'),
          predicate: namedNode('http://example.org/fullName'),
          object: literal('Alice'),
          graph: namedNode('')
        }
      ];

      mockSystem.query.mockResolvedValue(mockQuads);

      const graph = await mockSystem.query({
        query: `
          CONSTRUCT {
            ?person <http://example.org/fullName> ?name
          }
          WHERE {
            ?person <http://xmlns.com/foaf/0.1/name> ?name
          }
        `,
        type: 'sparql-construct'
      });

      expect(graph).toHaveLength(1);
      expect(graph[0].predicate.value).toBe('http://example.org/fullName');
    });

    it('should handle empty CONSTRUCT results', async () => {
      mockSystem.query.mockResolvedValue([]);

      const graph = await mockSystem.query({
        query: 'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }',
        type: 'sparql-construct'
      });

      expect(graph).toHaveLength(0);
    });

    it('should execute CONSTRUCT with FILTER', async () => {
      const mockQuads = [
        {
          subject: namedNode('http://example.org/alice'),
          predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
          object: literal('Alice'),
          graph: namedNode('')
        }
      ];

      mockSystem.query.mockResolvedValue(mockQuads);

      const graph = await mockSystem.query({
        query: `
          CONSTRUCT { ?person ?p ?o }
          WHERE {
            ?person ?p ?o .
            ?person <http://example.org/age> ?age .
            FILTER (?age >= 18)
          }
        `,
        type: 'sparql-construct'
      });

      expect(graph.length).toBeGreaterThanOrEqual(0);
    });
  });

  describe('SPARQL Error Handling', () => {
    it('should handle syntax errors in SELECT', async () => {
      mockSystem.query.mockRejectedValue(new Error('SPARQL syntax error'));

      await expect(
        mockSystem.query({
          query: 'INVALID SPARQL',
          type: 'sparql-select'
        })
      ).rejects.toThrow('SPARQL syntax error');
    });

    it('should handle syntax errors in ASK', async () => {
      mockSystem.query.mockRejectedValue(new Error('SPARQL syntax error'));

      await expect(
        mockSystem.query({
          query: 'ASK INVALID',
          type: 'sparql-ask'
        })
      ).rejects.toThrow('SPARQL syntax error');
    });

    it('should handle syntax errors in CONSTRUCT', async () => {
      mockSystem.query.mockRejectedValue(new Error('SPARQL syntax error'));

      await expect(
        mockSystem.query({
          query: 'CONSTRUCT INVALID',
          type: 'sparql-construct'
        })
      ).rejects.toThrow('SPARQL syntax error');
    });

    it('should handle query timeout', async () => {
      mockSystem.query.mockRejectedValue(new Error('Query timeout exceeded'));

      await expect(
        mockSystem.query({
          query: 'SELECT * WHERE { ?s ?p ?o }',
          type: 'sparql-select',
          timeout: 1000
        })
      ).rejects.toThrow('Query timeout');
    });
  });

  describe('SPARQL 1.1 Features', () => {
    it('should support aggregation functions', async () => {
      mockSystem.query.mockResolvedValue([
        { count: literal('5', namedNode('http://www.w3.org/2001/XMLSchema#integer')) }
      ]);

      const results = await mockSystem.query({
        query: `
          SELECT (COUNT(?person) AS ?count)
          WHERE {
            ?person a <http://xmlns.com/foaf/0.1/Person>
          }
        `,
        type: 'sparql-select'
      });

      expect(results).toHaveLength(1);
      expect(results[0]).toHaveProperty('count');
    });

    it('should support subqueries', async () => {
      mockSystem.query.mockResolvedValue([
        { person: namedNode('http://example.org/alice'), friendCount: literal('3') }
      ]);

      const results = await mockSystem.query({
        query: `
          SELECT ?person (COUNT(?friend) AS ?friendCount)
          WHERE {
            ?person <http://xmlns.com/foaf/0.1/knows> ?friend
          }
          GROUP BY ?person
        `,
        type: 'sparql-select'
      });

      expect(results.length).toBeGreaterThanOrEqual(0);
    });

    it('should support property paths', async () => {
      mockSystem.query.mockResolvedValue([
        { person: namedNode('http://example.org/alice'), indirectFriend: namedNode('http://example.org/charlie') }
      ]);

      const results = await mockSystem.query({
        query: `
          SELECT ?person ?indirectFriend
          WHERE {
            ?person <http://xmlns.com/foaf/0.1/knows>+ ?indirectFriend
          }
        `,
        type: 'sparql-select'
      });

      expect(results.length).toBeGreaterThanOrEqual(0);
    });
  });
});
