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

describe('@unrdf/browser Adversarial Tests - Capabilities', () => {
  describe('Browser Environment Detection - Advertised Features', () => {
    it('ADVERTISED: Can detect browser environment', () => {
      const isBrowser = isBrowserEnvironment();
      expect(typeof isBrowser).toBe('boolean');
    });

    it('ADVERTISED: Browser detection works in happy-dom', () => {
      const isBrowser = isBrowserEnvironment();
      expect(isBrowser).toBe(true);
    });
  });

  describe('Browser RDF Store - Advertised Features', () => {
    it('ADVERTISED: Can create browser-optimized RDF store', () => {
      const store = createBrowserRDFStore();
      expect(store).toBeDefined();
    });

    it('ADVERTISED: Browser store provides core RDF operations', () => {
      const store = createBrowserRDFStore();
      expect(store.add).toBeDefined();
      expect(store.delete).toBeDefined();
      expect(store.match).toBeDefined();
    });
  });

  describe('Quad Serialization - Advertised Features', () => {
    it('ADVERTISED: Can serialize quads for storage', () => {
      const quad = {
        subject: { termType: 'NamedNode', value: 'http://example.org/s' },
        predicate: { termType: 'NamedNode', value: 'http://example.org/p' },
        object: { termType: 'Literal', value: 'object' },
      };

      const serialized = serializeQuadForStorage(quad);
      expect(serialized).toBeDefined();
      expect(typeof serialized).toBe('string');
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
    });
  });

  describe('Storage Utilities - Advertised Features', () => {
    it('ADVERTISED: Can format storage sizes', () => {
      const formatted = formatStorageSize(1024 * 1024);
      expect(formatted).toBeDefined();
      expect(typeof formatted).toBe('string');
      expect(formatted).toContain('MB');
    });

    it('ADVERTISED: Can export store to JSON', () => {
      const store = createBrowserRDFStore();
      const exported = exportStoreToJSON(store);

      expect(exported).toBeDefined();
      expect(typeof exported).toBe('string');
    });

    it('ADVERTISED: Can import store from JSON', () => {
      const store = createBrowserRDFStore();
      const exported = exportStoreToJSON(store);
      const imported = importStoreFromJSON(exported);

      expect(imported).toBeDefined();
    });
  });
});
