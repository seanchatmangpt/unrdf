/**
 * @file Ontology Loader Tests
 * @module cli/test/sync/ontology-loader
 * @description Tests for RDF ontology loading functionality
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { writeFile, mkdir, rm } from 'fs/promises';
import { join } from 'path';
import { tmpdir } from 'os';
import { loadOntology, extractPrefixes } from '../../src/cli/commands/sync/ontology-loader.mjs';
import { COMMON_PREFIXES } from '@unrdf/core';

describe('Ontology Loader', () => {
  let testDir;

  beforeEach(async () => {
    testDir = join(tmpdir(), `ontology-loader-test-${Date.now()}`);
    await mkdir(testDir, { recursive: true });
  });

  afterEach(async () => {
    await rm(testDir, { recursive: true, force: true });
  });

  it('should load a valid Turtle file with prefix extraction and triple counting', async () => {
    // Arrange
    const turtleContent = `
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:Person rdf:type ex:Class .
foaf:Person a foaf:Agent .
ex:Subject ex:predicate ex:Object .
`;
    const ontologyPath = join(testDir, 'test.ttl');
    await writeFile(ontologyPath, turtleContent);

    // Act
    const result = await loadOntology({ source: 'test.ttl' }, testDir);

    // Assert
    expect(result).toBeDefined();
    expect(result.store).toBeDefined();
    expect(result.source).toBe(ontologyPath);
    expect(result.format).toBe('turtle');
    expect(result.prefixes).toBeDefined();
    expect(result.prefixes.ex).toBe('http://example.org/');
    expect(result.prefixes.foaf).toBe('http://xmlns.com/foaf/0.1/');
    expect(result.prefixes.rdf).toBe(COMMON_PREFIXES.rdf);
    expect(result.tripleCount).toBeGreaterThanOrEqual(3);
  });

  it('should throw error for missing ontology file with absolute path in message', async () => {
    // Arrange
    const config = { source: 'nonexistent.ttl' };

    // Act & Assert
    await expect(loadOntology(config, testDir)).rejects.toThrow('Ontology file not found');
  });

  it('should handle various RDF formats (turtle, ntriples, nquads, trig) and reject unknown formats', async () => {
    // turtle
    await writeFile(join(testDir, 'data.ttl'), '@prefix ex: <http://example.org/> .\nex:A ex:b ex:C .');
    const ttlResult = await loadOntology({ source: 'data.ttl', format: 'turtle' }, testDir);
    expect(ttlResult.format).toBe('turtle');

    // ntriples
    await writeFile(join(testDir, 'data.nt'), '<http://example.org/A> <http://example.org/b> <http://example.org/C> .');
    const ntResult = await loadOntology({ source: 'data.nt', format: 'ntriples' }, testDir);
    expect(ntResult.format).toBe('ntriples');

    // nquads
    await writeFile(join(testDir, 'data.nq'), '<http://example.org/A> <http://example.org/b> <http://example.org/C> <http://example.org/graph> .');
    const nqResult = await loadOntology({ source: 'data.nq', format: 'nquads' }, testDir);
    expect(nqResult.format).toBe('nquads');

    // trig
    await writeFile(join(testDir, 'data.trig'), '@prefix ex: <http://example.org/> .\nex:graph { ex:A ex:b ex:C . }');
    const trigResult = await loadOntology({ source: 'data.trig', format: 'trig' }, testDir);
    expect(trigResult.format).toBe('trig');

    // unknown format
    await writeFile(join(testDir, 'custom.ttl'), '@prefix ex: <http://example.org/> .\nex:A ex:b ex:C .');
    await expect(loadOntology({
      source: 'custom.ttl',
      format: 'custom-format',
    }, testDir)).rejects.toThrow(/not supported|Load operation failed/i);
  });
});
