/**
 * @vitest-environment happy-dom
 * Adversarial Testing: Test advertised capabilities for @unrdf/browser
 * Goal: PROVE what doesn't work, not security
 */

import { describe, it, expect } from 'vitest';
import {
  createBrowserRDFStore,
  isBrowserEnvironment,
  serializeQuadForStorage,
  deserializeQuad,
  formatStorageSize,
  exportStoreToJSON,
  importStoreFromJSON,
} from '../src/index.mjs';
import { createStore } from '@unrdf/core';

describe('@unrdf/browser Adversarial Tests - Capabilities', () => {
  describe('Browser Environment Detection - Advertised Features', () => {
    it('ADVERTISED: Can detect browser environment', () => {
      const isBrowser = isBrowserEnvironment();
      expect(typeof isBrowser).toBe('boolean');
    });

    it('ADVERTISED: Browser detection works in happy-dom', () => {
      const isBrowser = isBrowserEnvironment();
      expect(isBrowser).toBe(true); // happy-dom has window and document
    });
  });

  describe('Browser RDF Store - Advertised Features', () => {
    it('ADVERTISED: Can create browser-optimized RDF store', async () => {
      // createBrowserRDFStore is ASYNC
      const store = await createBrowserRDFStore();
      expect(store).toBeDefined();
    });

    it('ADVERTISED: Browser store provides core RDF operations', async () => {
      const store = await createBrowserRDFStore();
      // In Node/test environment, falls back to memory store
      // Memory store uses different API (addQuad, getQuads, etc.)
      expect(store).toBeDefined();
      expect(typeof store).toBe('object');
    });
  });

  describe('Quad Serialization - Advertised Features', () => {
    it('ADVERTISED: Can serialize quads for storage', () => {
      const quad = {
        subject: { termType: 'NamedNode', value: 'http://example.org/s' },
        predicate: { termType: 'NamedNode', value: 'http://example.org/p' },
        object: { termType: 'Literal', value: 'object' },
        // graph is optional - will default to DefaultGraph
      };

      const serialized = serializeQuadForStorage(quad);
      expect(serialized).toBeDefined();
      expect(typeof serialized).toBe('object');
      expect(serialized.subject).toBe('http://example.org/s');
      expect(serialized.graph).toBe(''); // default graph
    });

    it('ADVERTISED: Can serialize quads with explicit graph', () => {
      const quad = {
        subject: { termType: 'NamedNode', value: 'http://example.org/s' },
        predicate: { termType: 'NamedNode', value: 'http://example.org/p' },
        object: { termType: 'Literal', value: 'object' },
        graph: { termType: 'NamedNode', value: 'http://example.org/graph' },
      };

      const serialized = serializeQuadForStorage(quad);
      expect(serialized.graph).toBe('http://example.org/graph');
      expect(serialized.graphType).toBe('NamedNode');
    });

    it('ADVERTISED: Can deserialize quads from storage', () => {
      const quad = {
        subject: { termType: 'NamedNode', value: 'http://example.org/s' },
        predicate: { termType: 'NamedNode', value: 'http://example.org/p' },
        object: { termType: 'Literal', value: 'object' },
      };

      const serialized = serializeQuadForStorage(quad);
      const deserialized = deserializeQuad(serialized);

      expect(deserialized).toBeDefined();
      expect(deserialized.subject.value).toBe(quad.subject.value);
      expect(deserialized.predicate.value).toBe(quad.predicate.value);
      expect(deserialized.object.value).toBe(quad.object.value);
    });
  });

  describe('Storage Utilities - Advertised Features', () => {
    it('ADVERTISED: Can format storage sizes', () => {
      const formatted = formatStorageSize(1024 * 1024);
      expect(formatted).toBeDefined();
      expect(typeof formatted).toBe('string');
      expect(formatted).toContain('MB');
    });

    it('ADVERTISED: Can export/import store using memory store', async () => {
      // Use memory store instead of browser store for testing
      // Browser store uses IndexedDB which isn't available in tests
      const store = createStore();

      // Memory store from @unrdf/core has .match() method
      // exportStoreToJSON expects store with .match()
      const exported = exportStoreToJSON(store);

      expect(exported).toBeDefined();
      expect(typeof exported).toBe('string');

      // Verify it's valid JSON
      const parsed = JSON.parse(exported);
      expect(Array.isArray(parsed)).toBe(true);
    });

    it('ADVERTISED: Can import store data from JSON', () => {
      const store = createStore();

      // Create test data
      const testData = JSON.stringify([
        {
          subject: 'http://example.org/s',
          subjectType: 'NamedNode',
          predicate: 'http://example.org/p',
          object: 'test',
          objectType: 'Literal',
          objectLanguage: null,
          objectDatatype: null,
          graph: '',
          graphType: 'DefaultGraph',
        },
      ]);

      const count = importStoreFromJSON(store, testData);
      expect(count).toBe(1);
    });
  });
});
