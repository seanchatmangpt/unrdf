/**
 * @vitest-environment node
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { loadGraph, saveGraph, getGraphStats, mergeGraphs } from '../src/custom-commands.mjs';
import { mkdtempSync, writeFileSync, readFileSync, rmSync, existsSync } from 'fs';
import { join } from 'path';
import { tmpdir } from 'os';

const { namedNode, literal, quad } = dataFactory;

describe('Graph Commands Example', () => {
  let tmpDir;

  beforeEach(() => {
    tmpDir = mkdtempSync(join(tmpdir(), 'graph-test-'));
  });

  afterEach(() => {
    if (existsSync(tmpDir)) {
      rmSync(tmpDir, { recursive: true, force: true });
    }
  });

  describe('Load Graph from File', () => {
    it('should load graph from Turtle file', async () => {
      const inputPath = join(tmpDir, 'input.ttl');
      writeFileSync(inputPath, '<http://ex.org/s> <http://ex.org/p> "o" .');

      const store = await loadGraph(inputPath);
      expect(store.getQuads().length).toBe(1);
    });

    it('should load graph with multiple triples', async () => {
      const inputPath = join(tmpDir, 'input.ttl');
      const content = `
        <http://ex.org/s1> <http://ex.org/p1> "o1" .
        <http://ex.org/s2> <http://ex.org/p2> "o2" .
        <http://ex.org/s3> <http://ex.org/p3> "o3" .
      `;
      writeFileSync(inputPath, content);

      const store = await loadGraph(inputPath);
      expect(store.getQuads().length).toBe(3);
    });

    it('should reject invalid file path', async () => {
      await expect(loadGraph('')).rejects.toThrow();
    });
  });

  describe('Save Graph to File', () => {
    it('should save graph in Turtle format', async () => {
      const outputPath = join(tmpDir, 'output.ttl');
      const store = createStore();
      store.addQuad(quad(
        namedNode('http://ex.org/s'),
        namedNode('http://ex.org/p'),
        literal('o')
      ));

      await saveGraph(store, outputPath, 'turtle');
      const output = readFileSync(outputPath, 'utf-8');
      expect(output).toContain('http://ex.org/s');
      expect(output).toContain('http://ex.org/p');
    });

    it('should save graph in N-Triples format', async () => {
      const outputPath = join(tmpDir, 'output.nt');
      const store = createStore();
      store.addQuad(quad(
        namedNode('http://ex.org/s'),
        namedNode('http://ex.org/p'),
        literal('o')
      ));

      await saveGraph(store, outputPath, 'ntriples');
      const output = readFileSync(outputPath, 'utf-8');
      expect(output).toContain('<http://ex.org/s>');
      expect(output).toContain('<http://ex.org/p>');
    });

    it('should validate format before saving', async () => {
      const store = createStore();
      await expect(
        saveGraph(store, join(tmpDir, 'output.txt'), 'invalid')
      ).rejects.toThrow();
    });
  });

  describe('Statistics Calculation', () => {
    it('should calculate graph statistics', () => {
      const store = createStore();
      store.addQuad(quad(
        namedNode('http://ex.org/s1'),
        namedNode('http://ex.org/p1'),
        literal('o1')
      ));
      store.addQuad(quad(
        namedNode('http://ex.org/s2'),
        namedNode('http://ex.org/p1'),
        literal('o2')
      ));

      const stats = getGraphStats(store);

      expect(stats.totalQuads).toBe(2);
      expect(stats.uniqueSubjects).toBe(2);
      expect(stats.uniquePredicates).toBe(1);
      expect(stats.uniqueObjects).toBe(2);
    });

    it('should handle empty store', () => {
      const store = createStore();
      const stats = getGraphStats(store);

      expect(stats.totalQuads).toBe(0);
      expect(stats.uniqueSubjects).toBe(0);
      expect(stats.uniquePredicates).toBe(0);
      expect(stats.uniqueObjects).toBe(0);
    });

    it('should count duplicate subjects correctly', () => {
      const store = createStore();
      store.addQuad(quad(
        namedNode('http://ex.org/s1'),
        namedNode('http://ex.org/p1'),
        literal('o1')
      ));
      store.addQuad(quad(
        namedNode('http://ex.org/s1'),
        namedNode('http://ex.org/p2'),
        literal('o2')
      ));

      const stats = getGraphStats(store);
      expect(stats.uniqueSubjects).toBe(1);
      expect(stats.totalQuads).toBe(2);
    });
  });

  describe('Merge Operations', () => {
    it('should merge multiple graphs', () => {
      const store1 = createStore();
      store1.addQuad(quad(
        namedNode('http://ex.org/s1'),
        namedNode('http://ex.org/p1'),
        literal('o1')
      ));

      const store2 = createStore();
      store2.addQuad(quad(
        namedNode('http://ex.org/s2'),
        namedNode('http://ex.org/p2'),
        literal('o2')
      ));

      const merged = mergeGraphs([store1, store2]);

      expect(merged.getQuads().length).toBe(2);
    });

    it('should handle duplicate quads in merge', () => {
      const store1 = createStore();
      const testQuad = quad(
        namedNode('http://ex.org/s1'),
        namedNode('http://ex.org/p1'),
        literal('o1')
      );
      store1.addQuad(testQuad);

      const store2 = createStore();
      store2.addQuad(testQuad);

      const merged = mergeGraphs([store1, store2]);
      expect(merged.getQuads().length).toBe(1);
    });

    it('should reject empty store array', () => {
      expect(() => mergeGraphs([])).toThrow();
    });
  });

  describe('Format Detection', () => {
    it('should load and save with correct format', async () => {
      const inputPath = join(tmpDir, 'input.ttl');
      writeFileSync(inputPath, '<http://ex.org/s> <http://ex.org/p> "o" .');

      const store = await loadGraph(inputPath);
      const outputPath = join(tmpDir, 'output.nt');
      await saveGraph(store, outputPath, 'ntriples');

      expect(existsSync(outputPath)).toBe(true);
    });
  });

  describe('Error Handling', () => {
    it('should handle file read errors', async () => {
      const nonExistentPath = join(tmpDir, 'nonexistent.ttl');
      await expect(loadGraph(nonExistentPath)).rejects.toThrow();
    });

    it('should handle invalid RDF syntax', async () => {
      const inputPath = join(tmpDir, 'invalid.ttl');
      writeFileSync(inputPath, 'invalid rdf syntax here');

      await expect(loadGraph(inputPath)).rejects.toThrow();
    });
  });

  describe('File I/O Operations', () => {
    it('should roundtrip save and load', async () => {
      const store = createStore();
      store.addQuad(quad(
        namedNode('http://ex.org/s'),
        namedNode('http://ex.org/p'),
        literal('o')
      ));

      const outputPath = join(tmpDir, 'roundtrip.ttl');
      await saveGraph(store, outputPath, 'turtle');
      const loaded = await loadGraph(outputPath);

      expect(loaded.getQuads().length).toBe(1);
    });
  });
});
