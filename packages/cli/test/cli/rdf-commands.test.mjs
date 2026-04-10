/**
 * Integration tests for RDF CLI commands
 * Tests: graph, query, context, convert commands
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { mkdirSync, rmSync, writeFileSync, readFileSync, existsSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { graphCommand } from '../../src/cli/commands/graph.mjs';
import { queryCommand } from '../../src/cli/commands/query.mjs';
import { contextCommand } from '../../src/cli/commands/context.mjs';
import { convertCommand } from '../../src/cli/commands/convert.mjs';

describe('RDF CLI Commands', () => {
  let testDir, testFile;
  const testTurtle = `
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
ex:Alice a ex:Person ; rdfs:label "Alice Smith" ; ex:age 30 .
ex:Bob a ex:Person ; rdfs:label "Bob Jones" ; ex:age 25 .
`;

  beforeEach(() => {
    testDir = join(tmpdir(), 'unrdf-cli-test-' + Date.now());
    mkdirSync(testDir, { recursive: true });
    testFile = join(testDir, 'test-data.ttl');
    writeFileSync(testFile, testTurtle, 'utf-8');
  });

  afterEach(() => {
    if (existsSync(testDir)) rmSync(testDir, { recursive: true, force: true });
  });

  it('should create graph, load data, query SPARQL, and convert formats', async () => {
    // Create graph
    const graphFile = join(testDir, 'new-graph.nq');
    await graphCommand.subCommands.create.run({ args: { name: 'test-graph', file: graphFile } });
    expect(existsSync(graphFile)).toBe(true);
    const graphContent = readFileSync(graphFile, 'utf-8');
    expect(graphContent).toContain('urn:graph:test-graph');

    // Load RDF data
    await expect(graphCommand.subCommands.load.run({ args: { file: testFile, graph: 'test-data' } })).resolves.not.toThrow();

    // Query with SPARQL
    await expect(queryCommand.run({ args: { file: testFile, query: 'SELECT ?s WHERE { ?s ?p ?o }', format: 'json' } })).resolves.not.toThrow();
    await expect(queryCommand.run({ args: { file: testFile, query: 'SELECT ?s WHERE { ?s ?p ?o }', format: 'table' } })).resolves.not.toThrow();

    // Convert Turtle to N-Triples
    const ntFile = join(testDir, 'output.nt');
    await convertCommand.run({ args: { input: testFile, output: ntFile, to: 'N-Triples' } });
    expect(existsSync(ntFile)).toBe(true);
    const ntContent = readFileSync(ntFile, 'utf-8');
    expect(ntContent).toContain('http://example.org/Alice');
  });

  it('should fail on non-existent files for query and convert', async () => {
    const nonExistent = join(testDir, 'does-not-exist.ttl');

    await expect(queryCommand.run({ args: { file: nonExistent, query: 'SELECT ?s WHERE { ?s ?p ?o }' } })).rejects.toThrow();
    await expect(convertCommand.run({ args: { input: nonExistent, output: join(testDir, 'out.nt') } })).rejects.toThrow();
  });

  it('should create JSON-LD context, add/remove prefixes, and list them', async () => {
    const contextFile = join(testDir, 'context.jsonld');

    // Create context
    await contextCommand.subCommands.create.run({ args: { name: 'my-context', output: contextFile } });
    expect(existsSync(contextFile)).toBe(true);
    const content = JSON.parse(readFileSync(contextFile, 'utf-8'));
    expect(content).toHaveProperty('@context');

    // Add prefix
    await contextCommand.subCommands.add.run({ args: { file: contextFile, prefix: 'foaf', namespace: 'http://xmlns.com/foaf/0.1/' } });
    const withPrefix = JSON.parse(readFileSync(contextFile, 'utf-8'));
    expect(withPrefix['@context'].foaf).toBe('http://xmlns.com/foaf/0.1/');

    // Add and remove prefix
    await contextCommand.subCommands.add.run({ args: { file: contextFile, prefix: 'test', namespace: 'http://test.org/' } });
    await contextCommand.subCommands.remove.run({ args: { file: contextFile, prefix: 'test' } });
    const afterRemove = JSON.parse(readFileSync(contextFile, 'utf-8'));
    expect(afterRemove['@context'].test).toBeUndefined();

    // List
    await expect(contextCommand.subCommands.list.run({ args: { file: contextFile, format: 'json' } })).resolves.not.toThrow();
  });
});
