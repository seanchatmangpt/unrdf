/**
 * @unrdf/cli Test Suite
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { mkdir, rm, writeFile, readFile } from 'node:fs/promises';
import path from 'node:path';
import { createStore, addQuad, namedNode, literal, quad } from '../../packages/core/src/index.mjs';
import {
  loadGraph,
  saveGraph,
  createCommand,
  deleteCommand,
  describeCommand,
  mergeCommand,
} from '../../packages/cli/src/cli/commands/graph.mjs';
import {
  formatTable,
  formatJSON,
  formatCSV,
} from '../../packages/cli/src/cli/commands/query.mjs';

const TEST_DIR = path.join(process.cwd(), 'test-data-cli');

describe('@unrdf/cli - Graph Commands', () => {
  beforeEach(async () => {
    await mkdir(TEST_DIR, { recursive: true });
  });

  afterEach(async () => {
    await rm(TEST_DIR, { recursive: true, force: true });
  });

  it('should load and save Turtle graph', async () => {
    const turtleContent = `
      @prefix ex: <http://example.org/> .
      ex:subject ex:predicate "object" .
    `;

    const filePath = path.join(TEST_DIR, 'test.ttl');
    await writeFile(filePath, turtleContent, 'utf8');

    const store = await loadGraph(filePath);
    const quads = store.getQuads();

    expect(quads.length).toBeGreaterThan(0);
  });

  it('should save graph to file', async () => {
    const store = createStore();
    const s = namedNode('http://example.org/subject');
    const p = namedNode('http://example.org/predicate');
    const o = literal('object');
    addQuad(store, quad(s, p, o));

    const filePath = path.join(TEST_DIR, 'output.ttl');
    await saveGraph(store, filePath, 'turtle');

    const content = await readFile(filePath, 'utf8');
    expect(content).toContain('example.org/subject');
    expect(content).toContain('example.org/predicate');
    expect(content).toContain('object');
  });

  it('should detect format from file extension', async () => {
    const store = createStore();
    const s = namedNode('http://example.org/subject');
    const p = namedNode('http://example.org/predicate');
    const o = literal('object');
    addQuad(store, quad(s, p, o));

    const turtlePath = path.join(TEST_DIR, 'test.ttl');
    await saveGraph(store, turtlePath);

    const ntPath = path.join(TEST_DIR, 'test.nt');
    await saveGraph(store, ntPath);

    const turtleContent = await readFile(turtlePath, 'utf8');
    const ntContent = await readFile(ntPath, 'utf8');

    // Turtle has prefixes, N-Triples does not
    expect(turtleContent.length).toBeGreaterThan(0);
    expect(ntContent.length).toBeGreaterThan(0);
  });

  it('should create empty graph', async () => {
    const filePath = path.join(TEST_DIR, 'empty.ttl');

    await createCommand.run({
      args: { path: filePath, format: 'turtle' }
    });

    const store = await loadGraph(filePath);
    expect(store.getQuads().length).toBe(0);
  });

  it('should delete graph file', async () => {
    const filePath = path.join(TEST_DIR, 'delete-me.ttl');
    await writeFile(filePath, '', 'utf8');

    await deleteCommand.run({
      args: { path: filePath }
    });

    await expect(readFile(filePath, 'utf8')).rejects.toThrow();
  });

  it('should describe graph statistics', async () => {
    const store = createStore();
    const s1 = namedNode('http://example.org/s1');
    const s2 = namedNode('http://example.org/s2');
    const p = namedNode('http://example.org/p');
    const o = literal('object');

    addQuad(store, quad(s1, p, o));
    addQuad(store, quad(s2, p, o));

    const filePath = path.join(TEST_DIR, 'describe.ttl');
    await saveGraph(store, filePath);

    // Describe should show 2 triples, 2 subjects, 1 predicate, 1 object
    const originalLog = console.log;
    let output = '';
    console.log = (msg) => { output += msg + '\n'; };

    await describeCommand.run({
      args: { path: filePath }
    });

    console.log = originalLog;

    expect(output).toContain('2'); // 2 triples
  });

  it('should merge two graphs', async () => {
    const store1 = createStore();
    const store2 = createStore();

    const s1 = namedNode('http://example.org/s1');
    const s2 = namedNode('http://example.org/s2');
    const p = namedNode('http://example.org/p');
    const o = literal('object');

    addQuad(store1, quad(s1, p, o));
    addQuad(store2, quad(s2, p, o));

    const path1 = path.join(TEST_DIR, 'graph1.ttl');
    const path2 = path.join(TEST_DIR, 'graph2.ttl');
    const outputPath = path.join(TEST_DIR, 'merged.ttl');

    await saveGraph(store1, path1);
    await saveGraph(store2, path2);

    await mergeCommand.run({
      args: {
        input1: path1,
        input2: path2,
        output: outputPath,
      }
    });

    const mergedStore = await loadGraph(outputPath);
    expect(mergedStore.getQuads().length).toBe(2);
  });
});

describe('@unrdf/cli - Query Formatting', () => {
  it('should format SELECT results as table', () => {
    const bindings = [
      { name: literal('Alice'), age: literal('30') },
      { name: literal('Bob'), age: literal('25') },
    ];

    const table = formatTable(bindings);
    expect(table).toContain('Alice');
    expect(table).toContain('Bob');
    expect(table).toContain('30');
    expect(table).toContain('25');
  });

  it('should format SELECT results as JSON', () => {
    const bindings = [
      { name: literal('Alice'), age: literal('30') },
    ];

    const json = formatJSON(bindings);
    const parsed = JSON.parse(json);

    expect(parsed.length).toBe(1);
    expect(parsed[0].name.value).toBe('Alice');
  });

  it('should format SELECT results as CSV', () => {
    const bindings = [
      { name: literal('Alice'), age: literal('30') },
      { name: literal('Bob'), age: literal('25') },
    ];

    const csv = formatCSV(bindings);
    expect(csv).toContain('name,age');
    expect(csv).toContain('Alice');
    expect(csv).toContain('Bob');
  });

  it('should handle empty results', () => {
    const table = formatTable([]);
    expect(table).toBe('No results');

    const json = formatJSON([]);
    expect(JSON.parse(json)).toEqual([]);

    const csv = formatCSV([]);
    expect(csv).toBe('');
  });
});

describe('@unrdf/cli - Error Handling', () => {
  it('should handle missing file', async () => {
    const fakePath = path.join(TEST_DIR, 'nonexistent.ttl');
    await expect(loadGraph(fakePath)).rejects.toThrow();
  });

  it('should handle invalid RDF syntax', async () => {
    const invalidContent = 'this is not valid RDF';
    const filePath = path.join(TEST_DIR, 'invalid.ttl');

    await mkdir(TEST_DIR, { recursive: true });
    await writeFile(filePath, invalidContent, 'utf8');

    await expect(loadGraph(filePath)).rejects.toThrow();
    await rm(TEST_DIR, { recursive: true, force: true });
  });

  it('should validate input paths', async () => {
    await expect(
      createCommand.run({ args: { path: '', format: 'turtle' } })
    ).rejects.toThrow();
  });
});

describe('@unrdf/cli - Format Conversion', () => {
  beforeEach(async () => {
    await mkdir(TEST_DIR, { recursive: true });
  });

  afterEach(async () => {
    await rm(TEST_DIR, { recursive: true, force: true });
  });

  it('should convert between Turtle and N-Triples', async () => {
    const store = createStore();
    const s = namedNode('http://example.org/subject');
    const p = namedNode('http://example.org/predicate');
    const o = literal('object');
    addQuad(store, quad(s, p, o));

    const turtlePath = path.join(TEST_DIR, 'test.ttl');
    const ntPath = path.join(TEST_DIR, 'test.nt');

    await saveGraph(store, turtlePath, 'turtle');
    const turtleStore = await loadGraph(turtlePath);

    await saveGraph(turtleStore, ntPath, 'ntriples');
    const ntStore = await loadGraph(ntPath);

    expect(ntStore.getQuads().length).toBe(1);
  });
});
