/**
 * @vitest-environment node
 */

import { describe, it, expect } from 'vitest';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import {
  detectFormat,
  parseRDF,
  serializeRDF,
  convertFormat,
  validateRDF,
  getFormatInfo,
  RDF_FORMATS
} from '../src/converter.mjs';
const { namedNode, literal, quad } = dataFactory;

describe('Format Conversion Example', () => {
  describe('Format Detection', () => {
    it('should detect Turtle format from .ttl extension', () => {
      expect(detectFormat('file.ttl')).toBe('turtle');
    });

    it('should detect N-Triples format from .nt extension', () => {
      expect(detectFormat('file.nt')).toBe('ntriples');
    });

    it('should detect N-Quads format from .nq extension', () => {
      expect(detectFormat('file.nq')).toBe('nquads');
    });

    it('should detect TriG format from .trig extension', () => {
      expect(detectFormat('file.trig')).toBe('trig');
    });

    it('should detect JSON-LD format from .jsonld extension', () => {
      expect(detectFormat('file.jsonld')).toBe('jsonld');
    });

    it('should default to turtle for unknown extensions', () => {
      expect(detectFormat('file.xyz')).toBe('turtle');
    });

    it('should handle uppercase extensions', () => {
      expect(detectFormat('FILE.TTL')).toBe('turtle');
    });

    it('should reject empty filepath', () => {
      expect(() => detectFormat('')).toThrow();
    });
  });

  describe('RDF Parsing', () => {
    it('should parse Turtle syntax', async () => {
      const content = '<http://ex.org/s> <http://ex.org/p> "o" .';
      const quads = await parseRDF(content, 'turtle');

      expect(quads).toBeInstanceOf(Array);
      expect(quads.length).toBe(1);
      expect(quads[0].subject.value).toBe('http://ex.org/s');
    });

    it('should parse N-Triples syntax', async () => {
      const content = '<http://ex.org/s> <http://ex.org/p> "o" .';
      const quads = await parseRDF(content, 'ntriples');

      expect(quads.length).toBe(1);
    });

    it('should parse multiple triples', async () => {
      const content = `
        <http://ex.org/s1> <http://ex.org/p> "o1" .
        <http://ex.org/s2> <http://ex.org/p> "o2" .
        <http://ex.org/s3> <http://ex.org/p> "o3" .
      `;
      const quads = await parseRDF(content, 'turtle');

      expect(quads.length).toBe(3);
    });

    it('should reject invalid RDF syntax', async () => {
      const content = 'invalid rdf syntax';
      await expect(parseRDF(content, 'turtle')).rejects.toThrow();
    });
  });

  describe('RDF Serialization', () => {
    it('should serialize to Turtle format', async () => {
      const quads = [
        quad(
          namedNode('http://ex.org/s'),
          namedNode('http://ex.org/p'),
          literal('o')
        )
      ];

      const result = await serializeRDF(quads, 'turtle');
      expect(result).toContain('http://ex.org/s');
      expect(result).toContain('http://ex.org/p');
    });

    it('should serialize to N-Triples format', async () => {
      const quads = [
        quad(
          namedNode('http://ex.org/s'),
          namedNode('http://ex.org/p'),
          literal('o')
        )
      ];

      const result = await serializeRDF(quads, 'ntriples');
      expect(result).toContain('<http://ex.org/s>');
      expect(result).toContain('<http://ex.org/p>');
    });

    it('should handle empty quad array', async () => {
      const result = await serializeRDF([], 'turtle');
      expect(result).toBeDefined();
    });
  });

  describe('Format Conversion', () => {
    it('should convert Turtle to N-Triples', async () => {
      const turtle = '<http://ex.org/s> <http://ex.org/p> "o" .';
      const ntriples = await convertFormat(turtle, 'turtle', 'ntriples');

      expect(ntriples).toContain('<http://ex.org/s>');
      expect(ntriples).toContain('<http://ex.org/p>');
    });

    it('should convert N-Triples to Turtle', async () => {
      const ntriples = '<http://ex.org/s> <http://ex.org/p> "o" .';
      const turtle = await convertFormat(ntriples, 'ntriples', 'turtle');

      expect(turtle).toContain('http://ex.org/s');
    });

    it('should handle identity conversion', async () => {
      const content = '<http://ex.org/s> <http://ex.org/p> "o" .';
      const result = await convertFormat(content, 'turtle', 'turtle');

      expect(result).toBeDefined();
      expect(result).toContain('http://ex.org/s');
    });

    it('should preserve data in round-trip conversion', async () => {
      const original = '<http://ex.org/s> <http://ex.org/p> "o" .';
      const ntriples = await convertFormat(original, 'turtle', 'ntriples');
      const backToTurtle = await convertFormat(ntriples, 'ntriples', 'turtle');

      const originalQuads = await parseRDF(original, 'turtle');
      const roundtripQuads = await parseRDF(backToTurtle, 'turtle');

      expect(roundtripQuads.length).toBe(originalQuads.length);
    });
  });

  describe('Syntax Validation', () => {
    it('should validate correct Turtle syntax', async () => {
      const content = '<http://ex.org/s> <http://ex.org/p> "o" .';
      const isValid = await validateRDF(content, 'turtle');

      expect(isValid).toBe(true);
    });

    it('should reject invalid syntax', async () => {
      const content = 'invalid rdf syntax';
      const isValid = await validateRDF(content, 'turtle');

      expect(isValid).toBe(false);
    });

    it('should validate N-Triples', async () => {
      const content = '<http://ex.org/s> <http://ex.org/p> "o" .';
      const isValid = await validateRDF(content, 'ntriples');

      expect(isValid).toBe(true);
    });
  });

  describe('Format Information', () => {
    it('should return format info for Turtle', () => {
      const info = getFormatInfo('turtle');

      expect(info.format).toBe('turtle');
      expect(info.extensions).toEqual(RDF_FORMATS.turtle);
      expect(info.supported).toBe(true);
    });

    it('should handle unsupported formats', () => {
      const info = getFormatInfo('unknown');

      expect(info.format).toBe('unknown');
      expect(info.extensions).toEqual([]);
      expect(info.supported).toBe(false);
    });

    it('should list all format extensions', () => {
      const info = getFormatInfo('turtle');
      expect(info.extensions.length).toBeGreaterThan(0);
    });
  });

  describe('Encoding Handling', () => {
    it('should handle UTF-8 characters', async () => {
      const content = '<http://ex.org/s> <http://ex.org/p> "héllo wørld" .';
      const quads = await parseRDF(content, 'turtle');

      expect(quads[0].object.value).toBe('héllo wørld');
    });

    it('should handle special characters in literals', async () => {
      const content = '<http://ex.org/s> <http://ex.org/p> "line1\\nline2" .';
      const quads = await parseRDF(content, 'turtle');

      expect(quads.length).toBe(1);
    });
  });

  describe('Error Handling', () => {
    it('should handle malformed URIs', async () => {
      const content = '<invalid uri> <http://ex.org/p> "o" .';
      await expect(parseRDF(content, 'turtle')).rejects.toThrow();
    });

    it('should handle missing closing angle bracket', async () => {
      const content = '<http://ex.org/s <http://ex.org/p> "o" .';
      await expect(parseRDF(content, 'turtle')).rejects.toThrow();
    });
  });
});
