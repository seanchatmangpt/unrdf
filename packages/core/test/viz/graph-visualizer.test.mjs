/**
 * @file Graph visualizer tests
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { createStore, namedNode, literal } from '../../src/rdf/store.mjs';
import {
  toDOT,
  toMermaid,
  toASCII,
  toHTML,
  extractSubgraph,
} from '../../src/viz/graph-visualizer.mjs';

describe('GraphVisualizer', () => {
  let store;

  beforeEach(() => {
    store = createStore();

    // Add test data
    store.add({
      subject: namedNode('http://example.org/alice'),
      predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
      object: literal('Alice'),
    });

    store.add({
      subject: namedNode('http://example.org/alice'),
      predicate: namedNode('http://xmlns.com/foaf/0.1/knows'),
      object: namedNode('http://example.org/bob'),
    });

    store.add({
      subject: namedNode('http://example.org/bob'),
      predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
      object: literal('Bob'),
    });
  });

  describe('toDOT()', () => {
    it('should export graph to DOT format', () => {
      const dot = toDOT(store);

      expect(dot).toContain('digraph RDF');
      expect(dot).toContain('rankdir=TB');
      expect(dot).toContain('->');
    });

    it('should handle limit option', () => {
      const dot = toDOT(store, { limit: 1 });

      expect(dot).toContain('digraph RDF');
      const arrows = (dot.match(/->/g) || []).length;
      expect(arrows).toBeLessThanOrEqual(1);
    });

    it('should support different directions', () => {
      const dot = toDOT(store, { direction: 'LR' });

      expect(dot).toContain('rankdir=LR');
    });

    it('should throw on invalid store', () => {
      expect(() => toDOT(null)).toThrow('store must be a valid RDF store');
    });
  });

  describe('toMermaid()', () => {
    it('should export graph to Mermaid format', () => {
      const mermaid = toMermaid(store);

      expect(mermaid).toContain('graph TB');
      expect(mermaid).toContain('-->');
    });

    it('should support different directions', () => {
      const mermaid = toMermaid(store, { direction: 'LR' });

      expect(mermaid).toContain('graph LR');
    });

    it('should handle literals correctly', () => {
      const mermaid = toMermaid(store);

      expect(mermaid).toContain('Alice');
      expect(mermaid).toContain('Bob');
    });

    it('should throw on invalid store', () => {
      expect(() => toMermaid({})).toThrow('store must be a valid RDF store');
    });
  });

  describe('toASCII()', () => {
    it('should export graph to ASCII art', () => {
      const ascii = toASCII(store);

      expect(ascii).toContain('RDF Graph');
      expect(ascii).toContain('-->');
      expect(ascii).toContain('├──');
    });

    it('should respect limit option', () => {
      const ascii = toASCII(store, { limit: 1 });

      const lines = ascii.split('\n');
      expect(lines.length).toBeLessThan(10);
    });

    it('should throw on invalid store', () => {
      expect(() => toASCII(null)).toThrow('store must be a valid RDF store');
    });
  });

  describe('toHTML()', () => {
    it('should export graph to HTML with D3.js', () => {
      const html = toHTML(store);

      expect(html).toContain('<!DOCTYPE html>');
      expect(html).toContain('d3.js');
      expect(html).toContain('forceSimulation');
    });

    it('should support custom dimensions', () => {
      const html = toHTML(store, { width: 1000, height: 800 });

      expect(html).toContain('width="1000"');
      expect(html).toContain('height="800"');
    });

    it('should support dark theme', () => {
      const html = toHTML(store, { theme: 'dark' });

      expect(html).toContain('#1a1a1a');
    });

    it('should throw on invalid store', () => {
      expect(() => toHTML(null)).toThrow('store must be a valid RDF store');
    });
  });

  describe('extractSubgraph()', () => {
    it('should extract subgraph by subject', () => {
      const subgraph = extractSubgraph(store, {
        subject: 'http://example.org/alice',
      });

      expect(subgraph.length).toBeGreaterThan(0);
      expect(subgraph[0].subject.value).toBe('http://example.org/alice');
    });

    it('should respect depth limit', () => {
      const subgraph = extractSubgraph(store, {
        subject: 'http://example.org/alice',
        depth: 1,
      });

      expect(subgraph.length).toBeLessThanOrEqual(3);
    });

    it('should respect maxTriples limit', () => {
      const subgraph = extractSubgraph(store, {
        subject: 'http://example.org/alice',
        maxTriples: 1,
      });

      expect(subgraph.length).toBeLessThanOrEqual(1);
    });

    it('should throw on invalid store', () => {
      expect(() => extractSubgraph(null, { subject: 'test' }))
        .toThrow('store must be a valid RDF store');
    });
  });
});
