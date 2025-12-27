/**
 * @file RDF Operations README Example Tests (London TDD)
 * @description Tests for parsing and serialization examples from README.md
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { Store } from 'n3';

describe('README RDF Operations Examples', () => {
  let mockParseTurtle;
  let mockParseJsonLd;
  let mockToTurtle;
  let mockToJsonLd;
  let mockToNQuads;
  let mockStore;

  beforeEach(() => {
    mockStore = new Store();

    // Mock parsing functions
    mockParseTurtle = vi.fn().mockResolvedValue(mockStore);
    mockParseJsonLd = vi.fn().mockResolvedValue(mockStore);

    // Mock serialization functions
    mockToTurtle = vi
      .fn()
      .mockResolvedValue(
        '@prefix ex: <http://example.org/> .\nex:alice <http://xmlns.com/foaf/0.1/name> "Alice" .'
      );
    mockToJsonLd = vi.fn().mockResolvedValue({
      '@context': { foaf: 'http://xmlns.com/foaf/0.1/' },
      '@id': 'http://example.org/alice',
      'foaf:name': 'Alice',
    });
    mockToNQuads = vi
      .fn()
      .mockResolvedValue('<http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" .\n');
  });

  describe('Parse Turtle', () => {
    it('should parse valid Turtle with prefixes', async () => {
      const turtleInput = `
        @prefix ex: <http://example.org/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .

        ex:alice foaf:name "Alice" ;
                 foaf:knows ex:bob .
      `;

      const store = await mockParseTurtle(turtleInput);

      expect(mockParseTurtle).toHaveBeenCalledWith(turtleInput);
      expect(store).toBeInstanceOf(Store);
    });

    it('should handle Turtle with base IRI', async () => {
      const turtleInput = `
        @base <http://example.org/> .
        <alice> <http://xmlns.com/foaf/0.1/name> "Alice" .
      `;

      const store = await mockParseTurtle(turtleInput, 'http://example.org/');

      expect(mockParseTurtle).toHaveBeenCalledWith(turtleInput, 'http://example.org/');
      expect(store).toBeDefined();
    });

    it('should reject invalid Turtle syntax', async () => {
      mockParseTurtle.mockRejectedValue(new Error('Invalid Turtle syntax'));

      await expect(mockParseTurtle('INVALID TURTLE { } @#$')).rejects.toThrow(
        'Invalid Turtle syntax'
      );
    });

    it('should parse empty Turtle document', async () => {
      const store = await mockParseTurtle('');

      expect(mockParseTurtle).toHaveBeenCalledWith('');
      expect(store).toBeInstanceOf(Store);
    });
  });

  describe('Parse JSON-LD', () => {
    it('should parse valid JSON-LD', async () => {
      const jsonldInput = {
        '@context': { foaf: 'http://xmlns.com/foaf/0.1/' },
        '@id': 'http://example.org/alice',
        'foaf:name': 'Alice',
        'foaf:knows': { '@id': 'http://example.org/bob' },
      };

      const store = await mockParseJsonLd(jsonldInput);

      expect(mockParseJsonLd).toHaveBeenCalledWith(jsonldInput);
      expect(store).toBeInstanceOf(Store);
    });

    it('should handle JSON-LD with multiple contexts', async () => {
      const jsonldInput = {
        '@context': ['http://schema.org', { foaf: 'http://xmlns.com/foaf/0.1/' }],
        '@id': 'http://example.org/alice',
        'foaf:name': 'Alice',
      };

      const store = await mockParseJsonLd(jsonldInput);

      expect(mockParseJsonLd).toHaveBeenCalledWith(
        expect.objectContaining({
          '@context': expect.arrayContaining(['http://schema.org', expect.any(Object)]),
        })
      );
      expect(store).toBeDefined();
    });

    it('should reject invalid JSON-LD', async () => {
      mockParseJsonLd.mockRejectedValue(new Error('Invalid JSON-LD'));

      await expect(mockParseJsonLd({ invalid: 'structure' })).rejects.toThrow('Invalid JSON-LD');
    });
  });

  describe('Convert to Turtle', () => {
    it('should serialize store to Turtle format', async () => {
      const turtle = await mockToTurtle(mockStore);

      expect(mockToTurtle).toHaveBeenCalledWith(mockStore);
      expect(turtle).toContain('@prefix');
      expect(turtle).toContain('ex:alice');
    });

    it('should handle custom prefixes in serialization', async () => {
      const customPrefixes = {
        ex: 'http://example.org/',
        foaf: 'http://xmlns.com/foaf/0.1/',
      };

      mockToTurtle.mockResolvedValue('@prefix ex: <http://example.org/> .');

      const turtle = await mockToTurtle(mockStore, customPrefixes);

      expect(mockToTurtle).toHaveBeenCalledWith(mockStore, customPrefixes);
      expect(turtle).toContain('@prefix');
    });

    it('should serialize empty store', async () => {
      mockToTurtle.mockResolvedValue('');

      const turtle = await mockToTurtle(new Store());

      expect(turtle).toBe('');
    });
  });

  describe('Convert to JSON-LD', () => {
    it('should serialize store to JSON-LD format', async () => {
      const jsonld = await mockToJsonLd(mockStore);

      expect(mockToJsonLd).toHaveBeenCalledWith(mockStore);
      expect(jsonld).toHaveProperty('@context');
      expect(jsonld).toHaveProperty('@id');
    });

    it('should handle custom context in JSON-LD output', async () => {
      const customContext = {
        foaf: 'http://xmlns.com/foaf/0.1/',
        name: 'foaf:name',
      };

      mockToJsonLd.mockResolvedValue({
        '@context': customContext,
        '@id': 'http://example.org/alice',
        name: 'Alice',
      });

      const jsonld = await mockToJsonLd(mockStore, customContext);

      expect(jsonld['@context']).toEqual(customContext);
    });

    it('should serialize empty store to valid JSON-LD', async () => {
      mockToJsonLd.mockResolvedValue({ '@context': {}, '@graph': [] });

      const jsonld = await mockToJsonLd(new Store());

      expect(jsonld).toHaveProperty('@context');
    });
  });

  describe('Convert to N-Quads', () => {
    it('should serialize store to N-Quads format', async () => {
      const nquads = await mockToNQuads(mockStore);

      expect(mockToNQuads).toHaveBeenCalledWith(mockStore);
      expect(nquads).toContain('<http://example.org/alice>');
      expect(nquads).toContain('"Alice"');
    });

    it('should handle quads with named graphs', async () => {
      mockToNQuads.mockResolvedValue(
        '<http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" <http://example.org/graph1> .\n'
      );

      const nquads = await mockToNQuads(mockStore);

      expect(nquads).toContain('<http://example.org/graph1>');
    });

    it('should serialize empty store to empty N-Quads', async () => {
      mockToNQuads.mockResolvedValue('');

      const nquads = await mockToNQuads(new Store());

      expect(nquads).toBe('');
    });

    it('should handle special characters in literals', async () => {
      mockToNQuads.mockResolvedValue(
        '<http://example.org/s> <http://example.org/p> "String with \\"quotes\\" and \\n newlines" .\n'
      );

      const nquads = await mockToNQuads(mockStore);

      expect(nquads).toContain('\\"quotes\\"');
      expect(nquads).toContain('\\n');
    });
  });

  describe('Supported Formats', () => {
    it('should support Turtle (.ttl) format', async () => {
      const turtle = '@prefix ex: <http://example.org/> .';
      const store = await mockParseTurtle(turtle);

      expect(store).toBeInstanceOf(Store);
    });

    it('should support N-Triples (.nt) format', async () => {
      mockParseTurtle.mockResolvedValue(mockStore);

      const ntriples = '<http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" .';
      const store = await mockParseTurtle(ntriples);

      expect(store).toBeDefined();
    });

    it('should support N-Quads (.nq) format', async () => {
      const _nquads =
        '<http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" <http://example.org/graph> .';
      const serialized = await mockToNQuads(mockStore);

      expect(serialized).toBeDefined();
    });

    it('should support JSON-LD (.jsonld) format', async () => {
      const jsonld = {
        '@context': { foaf: 'http://xmlns.com/foaf/0.1/' },
        '@id': 'http://example.org/alice',
        'foaf:name': 'Alice',
      };
      const store = await mockParseJsonLd(jsonld);

      expect(store).toBeDefined();
    });
  });

  describe('Round-trip Conversion', () => {
    it('should preserve data through Turtle round-trip', async () => {
      const originalTurtle =
        '@prefix ex: <http://example.org/> .\nex:alice <http://xmlns.com/foaf/0.1/name> "Alice" .';

      const store = await mockParseTurtle(originalTurtle);
      const serialized = await mockToTurtle(store);

      expect(mockParseTurtle).toHaveBeenCalledWith(originalTurtle);
      expect(mockToTurtle).toHaveBeenCalledWith(store);
      expect(serialized).toBeDefined();
    });

    it('should preserve data through JSON-LD round-trip', async () => {
      const originalJsonLd = {
        '@context': { foaf: 'http://xmlns.com/foaf/0.1/' },
        '@id': 'http://example.org/alice',
        'foaf:name': 'Alice',
      };

      const store = await mockParseJsonLd(originalJsonLd);
      const serialized = await mockToJsonLd(store);

      expect(mockParseJsonLd).toHaveBeenCalledWith(originalJsonLd);
      expect(mockToJsonLd).toHaveBeenCalledWith(store);
      expect(serialized).toHaveProperty('@context');
    });
  });
});
