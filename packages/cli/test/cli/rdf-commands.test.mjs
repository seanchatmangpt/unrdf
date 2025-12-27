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
import { convertCommand, toTurtleCommand, toNTriplesCommand } from '../../src/cli/commands/convert.mjs';

describe('RDF CLI Commands', () => {
  let testDir;
  let testFile;

  const testTurtle = `
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Alice a ex:Person ;
  rdfs:label "Alice Smith" ;
  ex:age 30 .

ex:Bob a ex:Person ;
  rdfs:label "Bob Jones" ;
  ex:age 25 .
`;

  beforeEach(() => {
    testDir = join(tmpdir(), 'unrdf-cli-test-' + Date.now());
    mkdirSync(testDir, { recursive: true });
    testFile = join(testDir, 'test-data.ttl');
    writeFileSync(testFile, testTurtle, 'utf-8');
  });

  afterEach(() => {
    if (existsSync(testDir)) {
      rmSync(testDir, { recursive: true, force: true });
    }
  });

  describe('Graph Command', () => {
    it('should create a new graph', async () => {
      const outputFile = join(testDir, 'new-graph.nq');

      await graphCommand.subCommands.create.run({
        args: {
          name: 'test-graph',
          file: outputFile,
        },
      });

      expect(existsSync(outputFile)).toBe(true);
      const content = readFileSync(outputFile, 'utf-8');
      expect(content).toContain('urn:graph:test-graph');
    });

    it('should load RDF data', async () => {
      await expect(
        graphCommand.subCommands.load.run({
          args: {
            file: testFile,
            graph: 'test-data',
          },
        })
      ).resolves.not.toThrow();
    });

    it('should show graph statistics', async () => {
      await expect(
        graphCommand.subCommands.stats.run({
          args: {
            file: testFile,
          },
        })
      ).resolves.not.toThrow();
    });

    it('should dump graph to file', async () => {
      const outputFile = join(testDir, 'output.ttl');

      await graphCommand.subCommands.dump.run({
        args: {
          file: testFile,
          output: outputFile,
          format: 'turtle',
        },
      });

      expect(existsSync(outputFile)).toBe(true);
      const content = readFileSync(outputFile, 'utf-8');
      expect(content.length).toBeGreaterThan(0);
    });
  });

  describe('Query Command', () => {
    it('should execute SPARQL query with JSON output', async () => {
      await expect(
        queryCommand.run({
          args: {
            file: testFile,
            query: 'SELECT ?s WHERE { ?s ?p ?o }',
            format: 'json',
          },
        })
      ).resolves.not.toThrow();
    });

    it('should execute SPARQL query with table output', async () => {
      await expect(
        queryCommand.run({
          args: {
            file: testFile,
            query: 'SELECT ?s WHERE { ?s ?p ?o }',
            format: 'table',
          },
        })
      ).resolves.not.toThrow();
    });

    it('should fail on non-existent file', async () => {
      const nonExistent = join(testDir, 'does-not-exist.ttl');

      await expect(
        queryCommand.run({
          args: {
            file: nonExistent,
            query: 'SELECT ?s WHERE { ?s ?p ?o }',
          },
        })
      ).rejects.toThrow();
    });
  });

  describe('Context Command', () => {
    it('should create a new context', async () => {
      const outputFile = join(testDir, 'context.jsonld');

      await contextCommand.subCommands.create.run({
        args: {
          name: 'my-context',
          output: outputFile,
        },
      });

      expect(existsSync(outputFile)).toBe(true);
      const content = JSON.parse(readFileSync(outputFile, 'utf-8'));
      expect(content).toHaveProperty('@context');
    });

    it('should add prefix to context', async () => {
      const contextFile = join(testDir, 'context.jsonld');

      // Create context first
      await contextCommand.subCommands.create.run({
        args: {
          name: 'test',
          output: contextFile,
        },
      });

      // Add prefix
      await contextCommand.subCommands.add.run({
        args: {
          file: contextFile,
          prefix: 'foaf',
          namespace: 'http://xmlns.com/foaf/0.1/',
        },
      });

      const content = JSON.parse(readFileSync(contextFile, 'utf-8'));
      expect(content['@context'].foaf).toBe('http://xmlns.com/foaf/0.1/');
    });

    it('should list context prefixes', async () => {
      const contextFile = join(testDir, 'context.jsonld');

      // Create context
      await contextCommand.subCommands.create.run({
        args: {
          name: 'test',
          output: contextFile,
        },
      });

      // List prefixes
      await expect(
        contextCommand.subCommands.list.run({
          args: {
            file: contextFile,
            format: 'json',
          },
        })
      ).resolves.not.toThrow();
    });

    it('should remove prefix from context', async () => {
      const contextFile = join(testDir, 'context.jsonld');

      // Create and add prefix
      await contextCommand.subCommands.create.run({
        args: {
          name: 'test',
          output: contextFile,
        },
      });

      await contextCommand.subCommands.add.run({
        args: {
          file: contextFile,
          prefix: 'test',
          namespace: 'http://test.org/',
        },
      });

      // Remove prefix
      await contextCommand.subCommands.remove.run({
        args: {
          file: contextFile,
          prefix: 'test',
        },
      });

      const content = JSON.parse(readFileSync(contextFile, 'utf-8'));
      expect(content['@context'].test).toBeUndefined();
    });
  });

  describe('Convert Command', () => {
    it('should convert Turtle to N-Triples', async () => {
      const outputFile = join(testDir, 'output.nt');

      await convertCommand.run({
        args: {
          input: testFile,
          output: outputFile,
          to: 'N-Triples',
        },
      });

      expect(existsSync(outputFile)).toBe(true);
      const content = readFileSync(outputFile, 'utf-8');
      expect(content).toContain('http://example.org/Alice');
    });

    it('should convert using toTurtleCommand shorthand', async () => {
      const ntFile = join(testDir, 'data.nt');
      const outputFile = join(testDir, 'output.ttl');

      // First convert to NT
      await convertCommand.run({
        args: {
          input: testFile,
          output: ntFile,
          to: 'N-Triples',
        },
      });

      // Then convert back to Turtle
      await toTurtleCommand.run({
        args: {
          input: ntFile,
          output: outputFile,
        },
      });

      expect(existsSync(outputFile)).toBe(true);
    });

    it('should convert using toNTriplesCommand shorthand', async () => {
      const outputFile = join(testDir, 'output.nt');

      await toNTriplesCommand.run({
        args: {
          input: testFile,
          output: outputFile,
        },
      });

      expect(existsSync(outputFile)).toBe(true);
      const content = readFileSync(outputFile, 'utf-8');
      expect(content.length).toBeGreaterThan(0);
    });

    it('should auto-detect input format', async () => {
      const outputFile = join(testDir, 'output.nt');

      await convertCommand.run({
        args: {
          input: testFile,
          output: outputFile,
          // No 'from' specified - should auto-detect
          to: 'N-Triples',
        },
      });

      expect(existsSync(outputFile)).toBe(true);
    });

    it('should fail on non-existent input file', async () => {
      const nonExistent = join(testDir, 'does-not-exist.ttl');
      const outputFile = join(testDir, 'output.nt');

      await expect(
        convertCommand.run({
          args: {
            input: nonExistent,
            output: outputFile,
          },
        })
      ).rejects.toThrow();
    });
  });

  describe('Integration Tests', () => {
    it('should complete full workflow: create -> load -> query -> convert', async () => {
      // 1. Create graph
      const graphFile = join(testDir, 'my-graph.nq');
      await graphCommand.subCommands.create.run({
        args: {
          name: 'workflow-test',
          file: graphFile,
        },
      });
      expect(existsSync(graphFile)).toBe(true);

      // 2. Load data
      await graphCommand.subCommands.load.run({
        args: {
          file: testFile,
          graph: 'test',
        },
      });

      // 3. Query data
      await queryCommand.run({
        args: {
          file: testFile,
          query: 'SELECT ?s WHERE { ?s ?p ?o }',
          format: 'json',
        },
      });

      // 4. Convert format
      const convertedFile = join(testDir, 'converted.nt');
      await convertCommand.run({
        args: {
          input: testFile,
          output: convertedFile,
          to: 'N-Triples',
        },
      });
      expect(existsSync(convertedFile)).toBe(true);
    });

    it('should handle context workflow', async () => {
      const contextFile = join(testDir, 'workflow-context.jsonld');

      // Create
      await contextCommand.subCommands.create.run({
        args: {
          name: 'workflow',
          output: contextFile,
        },
      });

      // Add multiple prefixes
      await contextCommand.subCommands.add.run({
        args: {
          file: contextFile,
          prefix: 'foaf',
          namespace: 'http://xmlns.com/foaf/0.1/',
        },
      });

      await contextCommand.subCommands.add.run({
        args: {
          file: contextFile,
          prefix: 'schema',
          namespace: 'http://schema.org/',
        },
      });

      // List
      await contextCommand.subCommands.list.run({
        args: {
          file: contextFile,
          format: 'json',
        },
      });

      const content = JSON.parse(readFileSync(contextFile, 'utf-8'));
      expect(content['@context'].foaf).toBe('http://xmlns.com/foaf/0.1/');
      expect(content['@context'].schema).toBe('http://schema.org/');
    });
  });
});
