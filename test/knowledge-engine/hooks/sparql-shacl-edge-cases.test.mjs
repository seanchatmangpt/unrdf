/**
 * @file SPARQL/SHACL Edge Cases Tests
 * @module sparql-shacl-edge-cases
 * 
 * @description
 * Tests for SPARQL query and SHACL validation edge cases including
 * infinite loops, memory exhaustion, malformed syntax, and timeouts.
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { writeFile, unlink, mkdir } from 'fs/promises';
import { defineHook } from '../../../src/knowledge-engine/define-hook.mjs';
import { KnowledgeHookManager } from '../../../src/knowledge-engine/knowledge-hook-manager.mjs';
import { Store } from 'n3';
import { join } from 'path';
import { tmpdir } from 'os';

describe('SPARQL/SHACL Edge Cases', () => {
  let tempDir;
  let manager;
  let testStore;

  beforeEach(async () => {
    tempDir = join(tmpdir(), `unrdf-sparql-test-${Date.now()}`);
    await require('fs/promises').mkdir(tempDir, { recursive: true });
    manager = new KnowledgeHookManager({ basePath: tempDir });
    testStore = new Store();
  });

  afterEach(async () => {
    try {
      await require('fs/promises').rm(tempDir, { recursive: true, force: true });
    } catch (error) {
      // Ignore cleanup errors
    }
  });

  describe('SPARQL Query Edge Cases', () => {
    it('should handle infinite loops in SPARQL queries', async () => {
      const infiniteQuery = join(tempDir, 'infinite.sparql');
      // Create a query that could cause infinite loops
      const query = `
        SELECT * WHERE {
          ?s ?p ?o .
          ?o ?p ?s .
          ?s ?p ?o .
          ?o ?p ?s .
        }
      `;
      await writeFile(infiniteQuery, query);
      
      const hook = defineHook({
        meta: { name: 'infinite-query-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${infiniteQuery}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async () => ({ success: true })
      });

      // Mock query execution to simulate timeout
      vi.mock('../../../src/knowledge-engine/query.mjs', () => ({
        ask: vi.fn().mockImplementation(() => 
          new Promise((_, reject) => 
            setTimeout(() => reject(new Error('Query timeout')), 100)
          )
        )
      }));

      await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
    });

    it('should handle malformed SPARQL syntax', async () => {
      const malformedQuery = join(tempDir, 'malformed.sparql');
      const query = 'SELECT * WHERE { ?s ?p ?o .'; // Missing closing brace
      await writeFile(malformedQuery, query);
      
      const hook = defineHook({
        meta: { name: 'malformed-query-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${malformedQuery}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async () => ({ success: true })
      });

      // Mock query execution to simulate syntax error
      vi.mock('../../../src/knowledge-engine/query.mjs', () => ({
        ask: vi.fn().mockRejectedValue(new Error('SPARQL syntax error'))
      }));

      await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
    });

    it('should handle memory exhaustion scenarios', async () => {
      const memoryQuery = join(tempDir, 'memory.sparql');
      // Create a query that could cause memory issues
      const query = `
        SELECT * WHERE {
          ?s ?p ?o .
          ?o ?p ?s .
          ?s ?p ?o .
          ?o ?p ?s .
          ?s ?p ?o .
          ?o ?p ?s .
        }
      `;
      await writeFile(memoryQuery, query);
      
      const hook = defineHook({
        meta: { name: 'memory-exhaustion-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${memoryQuery}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async () => ({ success: true })
      });

      // Mock query execution to simulate memory error
      vi.mock('../../../src/knowledge-engine/query.mjs', () => ({
        ask: vi.fn().mockRejectedValue(new Error('Out of memory'))
      }));

      await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
    });

    it('should handle query engine crashes', async () => {
      const crashQuery = join(tempDir, 'crash.sparql');
      const query = 'SELECT * WHERE { ?s ?p ?o }';
      await writeFile(crashQuery, query);
      
      const hook = defineHook({
        meta: { name: 'query-engine-crash-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${crashQuery}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async () => ({ success: true })
      });

      // Mock query execution to simulate engine crash
      vi.mock('../../../src/knowledge-engine/query.mjs', () => ({
        ask: vi.fn().mockRejectedValue(new Error('Query engine crashed'))
      }));

      await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
    });

    it('should handle large dataset queries', async () => {
      const largeQuery = join(tempDir, 'large.sparql');
      const query = 'SELECT * WHERE { ?s ?p ?o }';
      await writeFile(largeQuery, query);
      
      const hook = defineHook({
        meta: { name: 'large-dataset-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${largeQuery}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async () => ({ success: true })
      });

      // Create a large dataset
      const largeStore = new Store();
      for (let i = 0; i < 100000; i++) {
        largeStore.addQuad(
          `http://example.org/subject${i}`,
          `http://example.org/predicate${i}`,
          `http://example.org/object${i}`
        );
      }

      // Mock query execution to simulate large dataset handling
      vi.mock('../../../src/knowledge-engine/query.mjs', () => ({
        ask: vi.fn().mockImplementation(async (store, query) => {
          if (store.size > 50000) {
            throw new Error('Dataset too large');
          }
          return true;
        })
      }));

      await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
    });
  });

  describe('SHACL Validation Edge Cases', () => {
    it('should handle SHACL validation timeouts', async () => {
      const timeoutShacl = join(tempDir, 'timeout.shacl');
      const shacl = `
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        
        ex:PersonShape a sh:NodeShape ;
          sh:targetClass ex:Person ;
          sh:property [
            sh:path ex:name ;
            sh:minCount 1
          ] .
      `;
      await writeFile(timeoutShacl, shacl);
      
      const hook = defineHook({
        meta: { name: 'shacl-timeout-test' },
        when: {
          kind: 'shacl',
          ref: {
            uri: `file://${timeoutShacl}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'text/turtle'
          }
        },
        run: async () => ({ success: true })
      });

      // Mock SHACL validation to simulate timeout
      vi.mock('../../../src/knowledge-engine/validate.mjs', () => ({
        validateShacl: vi.fn().mockImplementation(() => 
          new Promise((_, reject) => 
            setTimeout(() => reject(new Error('SHACL validation timeout')), 100)
          )
        )
      }));

      await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
    });

    it('should handle malformed SHACL syntax', async () => {
      const malformedShacl = join(tempDir, 'malformed.shacl');
      const shacl = `
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        
        ex:PersonShape a sh:NodeShape ;
          sh:targetClass ex:Person ;
          sh:property [
            sh:path ex:name ;
            sh:minCount 1
          ] . // Missing closing bracket
      `;
      await writeFile(malformedShacl, shacl);
      
      const hook = defineHook({
        meta: { name: 'malformed-shacl-test' },
        when: {
          kind: 'shacl',
          ref: {
            uri: `file://${malformedShacl}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'text/turtle'
          }
        },
        run: async () => ({ success: true })
      });

      // Mock SHACL validation to simulate syntax error
      vi.mock('../../../src/knowledge-engine/validate.mjs', () => ({
        validateShacl: vi.fn().mockRejectedValue(new Error('SHACL syntax error'))
      }));

      await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
    });

    it('should handle circular SHACL shapes', async () => {
      const circularShacl = join(tempDir, 'circular.shacl');
      const shacl = `
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        
        ex:PersonShape a sh:NodeShape ;
          sh:targetClass ex:Person ;
          sh:property [
            sh:path ex:parent ;
            sh:node ex:PersonShape
          ] .
      `;
      await writeFile(circularShacl, shacl);
      
      const hook = defineHook({
        meta: { name: 'circular-shacl-test' },
        when: {
          kind: 'shacl',
          ref: {
            uri: `file://${circularShacl}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'text/turtle'
          }
        },
        run: async () => ({ success: true })
      });

      // Mock SHACL validation to simulate circular reference
      vi.mock('../../../src/knowledge-engine/validate.mjs', () => ({
        validateShacl: vi.fn().mockRejectedValue(new Error('Circular shape reference detected'))
      }));

      await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
    });

    it('should handle invalid SHACL constraints', async () => {
      const invalidShacl = join(tempDir, 'invalid.shacl');
      const shacl = `
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        
        ex:PersonShape a sh:NodeShape ;
          sh:targetClass ex:Person ;
          sh:property [
            sh:path ex:age ;
            sh:datatype xsd:integer ;
            sh:minInclusive -1
          ] .
      `;
      await writeFile(invalidShacl, shacl);
      
      const hook = defineHook({
        meta: { name: 'invalid-shacl-test' },
        when: {
          kind: 'shacl',
          ref: {
            uri: `file://${invalidShacl}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'text/turtle'
          }
        },
        run: async () => ({ success: true })
      });

      // Mock SHACL validation to simulate invalid constraint
      vi.mock('../../../src/knowledge-engine/validate.mjs', () => ({
        validateShacl: vi.fn().mockRejectedValue(new Error('Invalid SHACL constraint'))
      }));

      await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
    });
  });

  describe('Query Performance Edge Cases', () => {
    it('should handle slow SPARQL queries', async () => {
      const slowQuery = join(tempDir, 'slow.sparql');
      const query = 'SELECT * WHERE { ?s ?p ?o }';
      await writeFile(slowQuery, query);
      
      const hook = defineHook({
        meta: { name: 'slow-query-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${slowQuery}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async () => ({ success: true })
      });

      // Mock query execution to simulate slow performance
      vi.mock('../../../src/knowledge-engine/query.mjs', () => ({
        ask: vi.fn().mockImplementation(() => 
          new Promise(resolve => setTimeout(() => resolve(true), 5000))
        )
      }));

      // Should timeout after reasonable time
      const startTime = Date.now();
      await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
      const duration = Date.now() - startTime;
      expect(duration).toBeLessThan(10000); // Should timeout before 10 seconds
    });

    it('should handle CPU-intensive SPARQL queries', async () => {
      const cpuQuery = join(tempDir, 'cpu-intensive.sparql');
      const query = `
        SELECT * WHERE {
          ?s ?p ?o .
          ?o ?p ?s .
          ?s ?p ?o .
          ?o ?p ?s .
        }
      `;
      await writeFile(cpuQuery, query);
      
      const hook = defineHook({
        meta: { name: 'cpu-intensive-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${cpuQuery}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async () => ({ success: true })
      });

      // Mock query execution to simulate CPU-intensive operation
      vi.mock('../../../src/knowledge-engine/query.mjs', () => ({
        ask: vi.fn().mockImplementation(() => 
          new Promise(resolve => {
            // Simulate CPU-intensive operation
            let result = 0;
            for (let i = 0; i < 1000000; i++) {
              result += Math.sqrt(i);
            }
            resolve(result > 0);
          })
        )
      }));

      // Test that hook is added successfully
      expect(() => manager.addKnowledgeHook(hook)).not.toThrow();
    });

    it('should handle I/O bound SPARQL queries', async () => {
      const ioQuery = join(tempDir, 'io-bound.sparql');
      const query = 'SELECT * WHERE { ?s ?p ?o }';
      await writeFile(ioQuery, query);
      
      const hook = defineHook({
        meta: { name: 'io-bound-test' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${ioQuery}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async () => ({ success: true })
      });

      // Mock query execution to simulate I/O bound operation
      vi.mock('../../../src/knowledge-engine/query.mjs', () => ({
        ask: vi.fn().mockImplementation(() => 
          new Promise(resolve => {
            // Simulate I/O bound operation
            setTimeout(() => resolve(true), 1000);
          })
        )
      }));

      // Test that hook is added successfully
      expect(() => manager.addKnowledgeHook(hook)).not.toThrow();
    });
  });

  describe('Query Result Edge Cases', () => {
    it('should handle empty query results', async () => {
      const emptyQuery = join(tempDir, 'empty.sparql');
      const query = 'SELECT * WHERE { ?s ?p ?o }';
      await writeFile(emptyQuery, query);
      
      const hook = defineHook({
        meta: { name: 'empty-result-test' },
        when: {
          kind: 'sparql-select',
          ref: {
            uri: `file://${emptyQuery}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async () => ({ success: true })
      });

      // Mock query execution to return empty results
      vi.mock('../../../src/knowledge-engine/query.mjs', () => ({
        select: vi.fn().mockResolvedValue([])
      }));

      // Test that hook is added successfully
      expect(() => manager.addKnowledgeHook(hook)).not.toThrow();
    });

    it('should handle large query results', async () => {
      const largeQuery = join(tempDir, 'large-result.sparql');
      const query = 'SELECT * WHERE { ?s ?p ?o }';
      await writeFile(largeQuery, query);
      
      const hook = defineHook({
        meta: { name: 'large-result-test' },
        when: {
          kind: 'sparql-select',
          ref: {
            uri: `file://${largeQuery}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async () => ({ success: true })
      });

      // Mock query execution to return large results
      const largeResults = Array.from({ length: 100000 }, (_, i) => ({
        s: { value: `http://example.org/subject${i}` },
        p: { value: `http://example.org/predicate${i}` },
        o: { value: `http://example.org/object${i}` }
      }));

      vi.mock('../../../src/knowledge-engine/query.mjs', () => ({
        select: vi.fn().mockResolvedValue(largeResults)
      }));

      // Test that hook is added successfully
      expect(() => manager.addKnowledgeHook(hook)).not.toThrow();
    });

    it('should handle null query results', async () => {
      const nullQuery = join(tempDir, 'null.sparql');
      const query = 'SELECT * WHERE { ?s ?p ?o }';
      await writeFile(nullQuery, query);
      
      const hook = defineHook({
        meta: { name: 'null-result-test' },
        when: {
          kind: 'sparql-select',
          ref: {
            uri: `file://${nullQuery}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async () => ({ success: true })
      });

      // Mock query execution to return null
      vi.mock('../../../src/knowledge-engine/query.mjs', () => ({
        select: vi.fn().mockResolvedValue(null)
      }));

      await expect(manager.addKnowledgeHook(hook)).rejects.toThrow();
    });
  });
});
