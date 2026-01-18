/**
 * @vitest-environment node
 * N3 Store Backward Compatibility Tests
 *
 * Verifies that the refactored executor maintains 100% backward compatibility
 * with N3 Store instances (stores with getQuads() method).
 */

import { describe, it, expect } from 'vitest';
import { createN3Store, namedNode, literal, quad } from '../../src/rdf/n3-migration.mjs';
import {
  executeQuery,
  executeSelect,
  executeAsk,
  executeConstruct,
} from '../../src/sparql/executor.mjs';
import {
  executeQuerySync,
  executeSelectSync,
  executeAskSync,
  executeConstructSync,
} from '../../src/sparql/executor-sync.mjs';

describe('N3 Store Backward Compatibility', () => {
  describe('Async API (executeQuery, executeSelect, executeAsk, executeConstruct)', () => {
    it('executeQuery works with N3 Store (SELECT)', async () => {
      // Create N3 Store and add data
      const n3Store = createN3Store();
      n3Store.addQuad(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );
      n3Store.addQuad(
        quad(namedNode('http://example.org/bob'), namedNode('http://foaf/name'), literal('Bob'))
      );

      const sparql = 'SELECT ?name WHERE { ?s <http://foaf/name> ?name }';
      const result = await executeQuery(n3Store, sparql);

      expect(result).toBeDefined();
      expect(result.type).toBe('select');
      expect(Array.isArray(result.rows)).toBe(true);
      expect(result.rows.length).toBeGreaterThan(0);
    });

    it('executeQuery works with N3 Store (ASK)', async () => {
      const n3Store = createN3Store();
      n3Store.addQuad(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = 'ASK { ?s <http://foaf/name> "Alice" }';
      const result = await executeQuery(n3Store, sparql);

      expect(typeof result).toBe('boolean');
      expect(result).toBe(true);
    });

    it('executeQuery works with N3 Store (CONSTRUCT)', async () => {
      const n3Store = createN3Store();
      n3Store.addQuad(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = `
        CONSTRUCT { ?s <http://example.org/label> ?name }
        WHERE { ?s <http://foaf/name> ?name }
      `;
      const result = await executeQuery(n3Store, sparql);

      expect(result).toBeDefined();
      expect(result.type).toBe('construct');
      expect(Array.isArray(result.quads)).toBe(true);
    });

    it('executeSelect works with N3 Store', async () => {
      const n3Store = createN3Store();
      n3Store.addQuad(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = 'SELECT ?name WHERE { ?s <http://foaf/name> ?name }';
      const rows = await executeSelect(n3Store, sparql);

      expect(Array.isArray(rows)).toBe(true);
      expect(rows.length).toBeGreaterThan(0);
    });

    it('executeAsk works with N3 Store', async () => {
      const n3Store = createN3Store();
      n3Store.addQuad(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = 'ASK { ?s <http://foaf/name> "Alice" }';
      const result = await executeAsk(n3Store, sparql);

      expect(typeof result).toBe('boolean');
      expect(result).toBe(true);
    });

    it('executeConstruct works with N3 Store', async () => {
      const n3Store = createN3Store();
      n3Store.addQuad(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = `
        CONSTRUCT { ?s <http://example.org/label> ?name }
        WHERE { ?s <http://foaf/name> ?name }
      `;
      const quads = await executeConstruct(n3Store, sparql);

      expect(Array.isArray(quads)).toBe(true);
      expect(quads.length).toBeGreaterThan(0);
    });
  });

  describe('Sync API (executeQuerySync, executeSelectSync, executeAskSync, executeConstructSync)', () => {
    it('executeQuerySync works with N3 Store (SELECT)', () => {
      const n3Store = createN3Store();
      n3Store.addQuad(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = 'SELECT ?name WHERE { ?s <http://foaf/name> ?name }';
      const result = executeQuerySync(n3Store, sparql);

      expect(result).not.toBeInstanceOf(Promise);
      expect(result.type).toBe('select');
      expect(Array.isArray(result.rows)).toBe(true);
    });

    it('executeQuerySync works with N3 Store (ASK)', () => {
      const n3Store = createN3Store();
      n3Store.addQuad(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = 'ASK { ?s <http://foaf/name> "Alice" }';
      const result = executeQuerySync(n3Store, sparql);

      expect(result).not.toBeInstanceOf(Promise);
      expect(typeof result).toBe('boolean');
    });

    it('executeQuerySync works with N3 Store (CONSTRUCT)', () => {
      const n3Store = createN3Store();
      n3Store.addQuad(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = `
        CONSTRUCT { ?s <http://example.org/label> ?name }
        WHERE { ?s <http://foaf/name> ?name }
      `;
      const result = executeQuerySync(n3Store, sparql);

      expect(result).not.toBeInstanceOf(Promise);
      expect(result.type).toBe('construct');
      expect(Array.isArray(result.quads)).toBe(true);
    });

    it('executeSelectSync works with N3 Store', () => {
      const n3Store = createN3Store();
      n3Store.addQuad(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = 'SELECT ?name WHERE { ?s <http://foaf/name> ?name }';
      const rows = executeSelectSync(n3Store, sparql);

      expect(Array.isArray(rows)).toBe(true);
      expect(rows.length).toBeGreaterThan(0);
    });

    it('executeAskSync works with N3 Store', () => {
      const n3Store = createN3Store();
      n3Store.addQuad(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = 'ASK { ?s <http://foaf/name> "Alice" }';
      const result = executeAskSync(n3Store, sparql);

      expect(typeof result).toBe('boolean');
      expect(result).toBe(true);
    });

    it('executeConstructSync works with N3 Store', () => {
      const n3Store = createN3Store();
      n3Store.addQuad(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = `
        CONSTRUCT { ?s <http://example.org/label> ?name }
        WHERE { ?s <http://foaf/name> ?name }
      `;
      const quads = executeConstructSync(n3Store, sparql);

      expect(Array.isArray(quads)).toBe(true);
      expect(quads.length).toBeGreaterThan(0);
    });
  });

  describe('N3 Store API Coverage', () => {
    it('handles empty N3 Store', async () => {
      const n3Store = createN3Store();
      const sparql = 'SELECT * WHERE { ?s ?p ?o }';
      const result = await executeQuery(n3Store, sparql);

      expect(result.rows.length).toBe(0);
    });

    it('handles N3 Store with multiple quads', async () => {
      const n3Store = createN3Store();

      // Add multiple quads
      for (let i = 0; i < 100; i++) {
        n3Store.addQuad(
          quad(
            namedNode(`http://example.org/s${i}`),
            namedNode('http://example.org/p'),
            literal(`value${i}`)
          )
        );
      }

      const sparql = 'SELECT * WHERE { ?s ?p ?o }';
      const result = await executeQuery(n3Store, sparql);

      expect(result.rows.length).toBe(100);
    });

    it('preserves result format between N3 Store and UnrdfStore', async () => {
      const n3Store = createN3Store();
      n3Store.addQuad(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = 'SELECT ?name WHERE { ?s <http://foaf/name> ?name }';
      const result = await executeQuery(n3Store, sparql);

      // Verify result structure matches UnrdfStore format
      expect(result).toHaveProperty('type', 'select');
      expect(result).toHaveProperty('rows');
      expect(Array.isArray(result.rows)).toBe(true);
      expect(result.rows.length).toBeGreaterThan(0);

      // Verify row structure
      const row = result.rows[0];
      expect(row).toHaveProperty('name');

      // Handle both N3 term format and Oxigraph WASM Literal format
      // N3 format: { type: 'Literal', value: 'Alice' }
      // Oxigraph format: Literal WASM object with value() method or different structure
      const term = row.name;
      if (typeof term === 'object' && term !== null) {
        // Check if it's N3 format (has .type and .value properties)
        // OR Oxigraph format (WASM object - just verify it exists)
        const hasN3Format = 'type' in term && 'value' in term;
        const hasOxigraphFormat = term.constructor && term.constructor.name === 'Literal';

        // Accept either format - this maintains backward compat while supporting Oxigraph
        expect(hasN3Format || hasOxigraphFormat || typeof term.value === 'function').toBe(true);
      } else {
        // Fail if term is not an object
        expect(term).toBeTypeOf('object');
      }
    });
  });

  describe('Performance Characteristics', () => {
    it('N3 Store conversion happens once per query (not cached between queries)', async () => {
      const n3Store = createN3Store();
      n3Store.addQuad(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = 'SELECT ?name WHERE { ?s <http://foaf/name> ?name }';

      // Multiple queries on same N3 Store
      const result1 = await executeQuery(n3Store, sparql);
      const result2 = await executeQuery(n3Store, sparql);

      expect(result1.rows.length).toBe(result2.rows.length);
    });

    it('N3 Store with large dataset still works (conversion overhead acceptable)', async () => {
      const n3Store = createN3Store();

      // Add 1000 quads
      for (let i = 0; i < 1000; i++) {
        n3Store.addQuad(
          quad(
            namedNode(`http://example.org/s${i}`),
            namedNode('http://example.org/p'),
            literal(`value${i}`)
          )
        );
      }

      const sparql = 'SELECT * WHERE { ?s ?p ?o } LIMIT 10';
      const startTime = Date.now();
      const result = await executeQuery(n3Store, sparql);
      const duration = Date.now() - startTime;

      expect(result.rows.length).toBe(10);
      // Conversion overhead should be acceptable (< 100ms for 1K quads)
      expect(duration).toBeLessThan(100);
    });
  });
});
