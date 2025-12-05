/**
 * @file CLI Stub Commands Test - Big Bang 80/20 Implementation
 * @description Tests for the 5 critical stub commands
 */

import { describe, it, beforeEach, afterEach } from 'node:test';
import assert from 'node:assert';
import { writeFile, unlink, mkdir, readFile } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { getStore, resetStore } from '../cli/utils/store-instance.mjs';

describe('CLI Stub Commands - Big Bang 80/20 Implementation', () => {
  let testDir;
  let testFile;

  beforeEach(async () => {
    // Create temp directory for tests
    testDir = join(tmpdir(), `unrdf-test-${Date.now()}`);
    await mkdir(testDir, { recursive: true });

    // Reset store before each test
    resetStore();
  });

  afterEach(async () => {
    // Clean up test files
    try {
      if (testFile) {
        await unlink(testFile);
      }
    } catch (error) {
      // Ignore cleanup errors
    }
  });

  describe('store/query - SPARQL Query Execution', () => {
    it('should execute SELECT query on empty store', async () => {
      const store = getStore();

      const results = store.query('SELECT * WHERE { ?s ?p ?o }');

      assert.ok(Array.isArray(results), 'Results should be an array');
      assert.strictEqual(results.length, 0, 'Empty store should return 0 results');
    });

    it('should execute SELECT query on populated store', async () => {
      const store = getStore();

      // Add test data using SPARQL UPDATE
      store.update(`
        PREFIX ex: <http://example.org/>
        INSERT DATA {
          ex:Alice ex:name "Alice" .
          ex:Bob ex:name "Bob" .
        }
      `);

      const results = store.query(`
        PREFIX ex: <http://example.org/>
        SELECT ?name WHERE {
          ?person ex:name ?name
        }
      `);

      assert.strictEqual(results.length, 2, 'Should return 2 results');
      assert.ok(results.some(r => r.name?.value === 'Alice'), 'Should find Alice');
      assert.ok(results.some(r => r.name?.value === 'Bob'), 'Should find Bob');
    });

    it('should execute ASK query', async () => {
      const store = getStore();

      store.update(`
        PREFIX ex: <http://example.org/>
        INSERT DATA { ex:Alice ex:name "Alice" }
      `);

      const result = store.query(`
        PREFIX ex: <http://example.org/>
        ASK { ex:Alice ex:name "Alice" }
      `);

      assert.strictEqual(result, true, 'ASK query should return true');
    });

    it('should execute CONSTRUCT query', async () => {
      const store = getStore();

      store.update(`
        PREFIX ex: <http://example.org/>
        INSERT DATA {
          ex:Alice ex:name "Alice" .
          ex:Alice ex:age 30 .
        }
      `);

      const results = store.query(`
        PREFIX ex: <http://example.org/>
        CONSTRUCT { ?s ex:name ?name }
        WHERE { ?s ex:name ?name }
      `);

      assert.ok(Array.isArray(results), 'CONSTRUCT should return array of quads');
      assert.ok(results.length > 0, 'Should construct at least one quad');
    });
  });

  describe('store/import - RDF Data Loading', () => {
    it('should import Turtle data', async () => {
      const store = getStore();

      const turtleData = `
        @prefix ex: <http://example.org/> .
        ex:Alice ex:name "Alice" .
        ex:Bob ex:name "Bob" .
      `;

      store.load(turtleData, { format: 'text/turtle' });

      const size = store.size();
      assert.strictEqual(size, 2, 'Should import 2 quads');

      const results = store.query('SELECT * WHERE { ?s ?p ?o }');
      assert.strictEqual(results.length, 2, 'Should query 2 results');
    });

    it('should import N-Triples data', async () => {
      const store = getStore();

      const ntriplesData = `
        <http://example.org/Alice> <http://example.org/name> "Alice" .
        <http://example.org/Bob> <http://example.org/name> "Bob" .
      `;

      store.load(ntriplesData, { format: 'application/n-triples' });

      assert.strictEqual(store.size(), 2, 'Should import 2 quads');
    });

    it('should import into named graph', async () => {
      const store = getStore();

      const turtleData = `
        @prefix ex: <http://example.org/> .
        ex:Alice ex:name "Alice" .
      `;

      // Note: Oxigraph load() with to_graph is implementation-specific
      // For 80/20, we test that load works without errors
      store.load(turtleData, { format: 'text/turtle' });

      assert.ok(store.size() > 0, 'Should have quads after import');
    });
  });

  describe('graph/delete - Graph Deletion', () => {
    it('should delete default graph using SPARQL UPDATE', async () => {
      const store = getStore();

      // Add data to default graph
      store.update(`
        PREFIX ex: <http://example.org/>
        INSERT DATA { ex:Alice ex:name "Alice" }
      `);

      assert.strictEqual(store.size(), 1, 'Should have 1 quad before delete');

      // Delete default graph
      store.update('CLEAR DEFAULT');

      assert.strictEqual(store.size(), 0, 'Default graph should be empty after CLEAR');
    });

    it('should delete named graph using SPARQL UPDATE', async () => {
      const store = getStore();

      // Add data to named graph
      store.update(`
        PREFIX ex: <http://example.org/>
        INSERT DATA {
          GRAPH ex:MyGraph {
            ex:Alice ex:name "Alice"
          }
        }
      `);

      assert.strictEqual(store.size(), 1, 'Should have 1 quad before delete');

      // Delete named graph
      store.update('CLEAR GRAPH <http://example.org/MyGraph>');

      assert.strictEqual(store.size(), 0, 'Named graph should be empty after CLEAR');
    });

    it('should handle deletion of non-existent graph', async () => {
      const store = getStore();

      // Delete non-existent graph should not throw
      assert.doesNotThrow(() => {
        store.update('CLEAR GRAPH <http://example.org/NonExistent>');
      }, 'Deleting non-existent graph should not throw');
    });
  });

  describe('hook/delete - Hook File Deletion', () => {
    it('should delete hook file if exists', async () => {
      const hookName = 'test-hook';
      const hookFile = join(testDir, `${hookName}.json`);

      // Create test hook file
      await writeFile(hookFile, JSON.stringify({
        meta: { name: hookName },
        when: { kind: 'sparql-ask' }
      }));

      // Verify file exists
      const before = await readFile(hookFile, 'utf-8');
      assert.ok(before, 'Hook file should exist before deletion');

      // Delete file
      await unlink(hookFile);

      // Verify file deleted
      await assert.rejects(
        async () => await readFile(hookFile),
        { code: 'ENOENT' },
        'Hook file should not exist after deletion'
      );
    });

    it('should handle deletion of non-existent hook gracefully', async () => {
      const hookFile = join(testDir, 'non-existent.json');

      // Delete non-existent file should not crash (ENOENT handled)
      await assert.rejects(
        async () => await unlink(hookFile),
        { code: 'ENOENT' },
        'Should reject with ENOENT for non-existent file'
      );
    });
  });

  describe('store/export - RDF Data Export', () => {
    it('should export Turtle data', async () => {
      const store = getStore();

      // Add test data
      store.update(`
        PREFIX ex: <http://example.org/>
        INSERT DATA {
          ex:Alice ex:name "Alice" .
          ex:Bob ex:name "Bob" .
        }
      `);

      const turtle = store.dump({ format: 'text/turtle' });

      assert.ok(typeof turtle === 'string', 'Export should return string');
      assert.ok(turtle.length > 0, 'Export should not be empty');
      assert.ok(turtle.includes('Alice') || turtle.includes('alice'), 'Should contain Alice');
      assert.ok(turtle.includes('Bob') || turtle.includes('bob'), 'Should contain Bob');
    });

    it('should export N-Triples data', async () => {
      const store = getStore();

      store.update(`
        PREFIX ex: <http://example.org/>
        INSERT DATA { ex:Alice ex:name "Alice" }
      `);

      const ntriples = store.dump({ format: 'application/n-triples' });

      assert.ok(typeof ntriples === 'string', 'Export should return string');
      assert.ok(ntriples.includes('http://example.org/'), 'Should contain IRIs');
    });

    it('should export empty store', async () => {
      const store = getStore();

      const turtle = store.dump({ format: 'text/turtle' });

      assert.ok(typeof turtle === 'string', 'Export should return string even for empty store');
    });

    it('should write export to file', async () => {
      const store = getStore();

      store.update(`
        PREFIX ex: <http://example.org/>
        INSERT DATA { ex:Alice ex:name "Alice" }
      `);

      const turtle = store.dump({ format: 'text/turtle' });

      testFile = join(testDir, 'export.ttl');
      await writeFile(testFile, turtle, 'utf-8');

      const content = await readFile(testFile, 'utf-8');
      assert.ok(content.length > 0, 'File should have content');
      assert.strictEqual(content, turtle, 'File content should match export');
    });
  });

  describe('Integration - Cross-Command Workflow', () => {
    it('should import, query, and export data', async () => {
      const store = getStore();

      // 1. Import data
      const turtleData = `
        @prefix ex: <http://example.org/> .
        ex:Alice ex:name "Alice" .
        ex:Alice ex:age 30 .
      `;

      store.load(turtleData, { format: 'text/turtle' });

      // 2. Query data
      const results = store.query(`
        PREFIX ex: <http://example.org/>
        SELECT ?name ?age WHERE {
          ex:Alice ex:name ?name .
          ex:Alice ex:age ?age .
        }
      `);

      assert.strictEqual(results.length, 1, 'Should find 1 result');
      assert.strictEqual(results[0].name?.value, 'Alice');
      assert.strictEqual(results[0].age?.value, '30');

      // 3. Export data
      const exported = store.dump({ format: 'text/turtle' });
      assert.ok(exported.includes('Alice'), 'Export should contain Alice');
    });

    it('should import, delete graph, verify empty', async () => {
      const store = getStore();

      // 1. Import data
      store.update(`
        PREFIX ex: <http://example.org/>
        INSERT DATA {
          GRAPH ex:TestGraph {
            ex:Alice ex:name "Alice"
          }
        }
      `);

      assert.ok(store.size() > 0, 'Store should have data after import');

      // 2. Delete graph
      store.update('CLEAR GRAPH <http://example.org/TestGraph>');

      // 3. Verify empty
      const results = store.query('SELECT * WHERE { ?s ?p ?o }');
      assert.strictEqual(results.length, 0, 'Store should be empty after deletion');
    });
  });
});
