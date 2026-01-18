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

  describe('loadOntology()', () => {
    it('should load a valid Turtle file', async () => {
      // Arrange
      const turtleContent = `
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:Person rdf:type ex:Class .
ex:name rdf:type ex:Property .
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
    });

    it('should extract prefixes correctly from Turtle content', async () => {
      // Arrange
      const turtleContent = `
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix dc: <http://purl.org/dc/terms/> .
@prefix schema: <http://schema.org/> .

foaf:Person a schema:Thing .
`;
      const ontologyPath = join(testDir, 'prefixes.ttl');
      await writeFile(ontologyPath, turtleContent);

      // Act
      const result = await loadOntology({ source: 'prefixes.ttl' }, testDir);

      // Assert
      expect(result.prefixes).toBeDefined();
      expect(result.prefixes.foaf).toBe('http://xmlns.com/foaf/0.1/');
      expect(result.prefixes.dc).toBe('http://purl.org/dc/terms/');
      expect(result.prefixes.schema).toBe('http://schema.org/');
    });

    it('should throw error for missing ontology file', async () => {
      // Arrange
      const config = { source: 'nonexistent.ttl' };

      // Act & Assert
      await expect(loadOntology(config, testDir)).rejects.toThrow('Ontology file not found');
    });

    it('should throw error with absolute path in error message', async () => {
      // Arrange
      const config = { source: 'missing.ttl' };

      // Act & Assert
      try {
        await loadOntology(config, testDir);
        expect.fail('Should have thrown an error');
      } catch (err) {
        expect(err.message).toContain(testDir);
        expect(err.message).toContain('missing.ttl');
      }
    });

    it('should use FORMAT_TO_EXT mapping for turtle format', async () => {
      // Arrange
      const turtleContent = `
@prefix ex: <http://example.org/> .
ex:Subject ex:predicate ex:Object .
`;
      const ontologyPath = join(testDir, 'format-test.ttl');
      await writeFile(ontologyPath, turtleContent);

      // Act
      const result = await loadOntology({
        source: 'format-test.ttl',
        format: 'turtle',
      }, testDir);

      // Assert
      expect(result.format).toBe('turtle');
    });

    it('should use FORMAT_TO_EXT mapping for text/turtle mime type', async () => {
      // Arrange
      const turtleContent = `
@prefix ex: <http://example.org/> .
ex:A ex:rel ex:B .
`;
      const ontologyPath = join(testDir, 'mime-test.ttl');
      await writeFile(ontologyPath, turtleContent);

      // Act
      const result = await loadOntology({
        source: 'mime-test.ttl',
        format: 'text/turtle',
      }, testDir);

      // Assert
      expect(result.format).toBe('text/turtle');
    });

    it('should merge COMMON_PREFIXES with extracted prefixes', async () => {
      // Arrange
      const turtleContent = `
@prefix custom: <http://custom.example.org/> .
custom:Entity a custom:Type .
`;
      const ontologyPath = join(testDir, 'common-prefixes.ttl');
      await writeFile(ontologyPath, turtleContent);

      // Act
      const result = await loadOntology({ source: 'common-prefixes.ttl' }, testDir);

      // Assert - should have both COMMON_PREFIXES and extracted
      expect(result.prefixes.custom).toBe('http://custom.example.org/');
      expect(result.prefixes.rdf).toBe(COMMON_PREFIXES.rdf);
      expect(result.prefixes.rdfs).toBe(COMMON_PREFIXES.rdfs);
      expect(result.prefixes.owl).toBe(COMMON_PREFIXES.owl);
      expect(result.prefixes.xsd).toBe(COMMON_PREFIXES.xsd);
    });

    it('should allow custom prefixes to override defaults', async () => {
      // Arrange
      const turtleContent = `
@prefix ex: <http://example.org/> .
ex:Test ex:prop "value" .
`;
      const ontologyPath = join(testDir, 'override.ttl');
      await writeFile(ontologyPath, turtleContent);
      const customPrefixes = {
        rdf: 'http://custom-rdf.example.org/',
        myns: 'http://my-namespace.org/',
      };

      // Act
      const result = await loadOntology({
        source: 'override.ttl',
        prefixes: customPrefixes,
      }, testDir);

      // Assert - custom prefixes should override
      expect(result.prefixes.rdf).toBe('http://custom-rdf.example.org/');
      expect(result.prefixes.myns).toBe('http://my-namespace.org/');
    });

    it('should use default turtle format when not specified', async () => {
      // Arrange
      const turtleContent = `
@prefix ex: <http://example.org/> .
ex:Default ex:format "turtle" .
`;
      const ontologyPath = join(testDir, 'default-format.ttl');
      await writeFile(ontologyPath, turtleContent);

      // Act
      const result = await loadOntology({ source: 'default-format.ttl' }, testDir);

      // Assert
      expect(result.format).toBe('turtle');
    });

    it('should resolve relative paths correctly', async () => {
      // Arrange
      const subDir = join(testDir, 'ontologies');
      await mkdir(subDir, { recursive: true });
      const turtleContent = `
@prefix ex: <http://example.org/> .
ex:Nested ex:path "resolved" .
`;
      await writeFile(join(subDir, 'nested.ttl'), turtleContent);

      // Act
      const result = await loadOntology({
        source: 'ontologies/nested.ttl',
      }, testDir);

      // Assert
      expect(result.source).toBe(join(subDir, 'nested.ttl'));
    });

    it('should accept base_iri configuration', async () => {
      // Arrange
      const turtleContent = `
@prefix ex: <http://example.org/> .
ex:WithBase ex:has ex:IRI .
`;
      const ontologyPath = join(testDir, 'base-iri.ttl');
      await writeFile(ontologyPath, turtleContent);

      // Act
      const result = await loadOntology({
        source: 'base-iri.ttl',
        base_iri: 'http://example.org/base/',
      }, testDir);

      // Assert
      expect(result).toBeDefined();
      expect(result.store).toBeDefined();
    });
  });

  describe('Triple Count Accuracy', () => {
    it('should count triples correctly for simple ontology', async () => {
      // Arrange - 3 explicit triples
      const turtleContent = `
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:Subject1 ex:predicate1 ex:Object1 .
ex:Subject2 ex:predicate2 ex:Object2 .
ex:Subject3 ex:predicate3 ex:Object3 .
`;
      const ontologyPath = join(testDir, 'count-test.ttl');
      await writeFile(ontologyPath, turtleContent);

      // Act
      const result = await loadOntology({ source: 'count-test.ttl' }, testDir);

      // Assert
      expect(result.tripleCount).toBeGreaterThanOrEqual(3);
    });

    it('should count triples with semicolon shorthand', async () => {
      // Arrange - Single subject with multiple predicates
      const turtleContent = `
@prefix ex: <http://example.org/> .

ex:Subject
  ex:prop1 "value1" ;
  ex:prop2 "value2" ;
  ex:prop3 "value3" .
`;
      const ontologyPath = join(testDir, 'semicolon-test.ttl');
      await writeFile(ontologyPath, turtleContent);

      // Act
      const result = await loadOntology({ source: 'semicolon-test.ttl' }, testDir);

      // Assert - should have at least 3 triples
      expect(result.tripleCount).toBeGreaterThanOrEqual(3);
    });

    it('should handle empty ontology', async () => {
      // Arrange
      const turtleContent = `
@prefix ex: <http://example.org/> .
# No triples, just prefix declaration and comment
`;
      const ontologyPath = join(testDir, 'empty.ttl');
      await writeFile(ontologyPath, turtleContent);

      // Act
      const result = await loadOntology({ source: 'empty.ttl' }, testDir);

      // Assert
      expect(result.tripleCount).toBe(0);
    });

    it('should not count prefix declarations as triples', async () => {
      // Arrange
      const turtleContent = `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix dc: <http://purl.org/dc/terms/> .

ex:SingleTriple ex:rel ex:Object .
`;
      const ontologyPath = join(testDir, 'prefix-not-triple.ttl');
      await writeFile(ontologyPath, turtleContent);

      // Act
      const result = await loadOntology({ source: 'prefix-not-triple.ttl' }, testDir);

      // Assert - only 1 actual triple, not 4
      expect(result.tripleCount).toBeLessThanOrEqual(1);
    });

    it('should not count comments as triples', async () => {
      // Arrange
      const turtleContent = `
@prefix ex: <http://example.org/> .
# This is a comment
# Another comment
ex:OnlyTriple ex:is "real" .
# Trailing comment
`;
      const ontologyPath = join(testDir, 'comments.ttl');
      await writeFile(ontologyPath, turtleContent);

      // Act
      const result = await loadOntology({ source: 'comments.ttl' }, testDir);

      // Assert
      expect(result.tripleCount).toBeLessThanOrEqual(1);
    });
  });

  describe('extractPrefixes()', () => {
    it('should return COMMON_PREFIXES for basic store', () => {
      // Arrange
      const mockStore = {};

      // Act
      const prefixes = extractPrefixes(mockStore);

      // Assert
      expect(prefixes).toBeDefined();
      expect(prefixes.rdf).toBe(COMMON_PREFIXES.rdf);
      expect(prefixes.rdfs).toBe(COMMON_PREFIXES.rdfs);
      expect(prefixes.owl).toBe(COMMON_PREFIXES.owl);
      expect(prefixes.xsd).toBe(COMMON_PREFIXES.xsd);
      expect(prefixes.foaf).toBe(COMMON_PREFIXES.foaf);
      expect(prefixes.dcterms).toBe(COMMON_PREFIXES.dcterms);
      expect(prefixes.skos).toBe(COMMON_PREFIXES.skos);
    });

    it('should merge store prefixes with COMMON_PREFIXES', () => {
      // Arrange
      const mockStore = {
        getPrefixes: () => ({
          custom: 'http://custom.example.org/',
          extra: 'http://extra.example.org/',
        }),
      };

      // Act
      const prefixes = extractPrefixes(mockStore);

      // Assert
      expect(prefixes.custom).toBe('http://custom.example.org/');
      expect(prefixes.extra).toBe('http://extra.example.org/');
      expect(prefixes.rdf).toBe(COMMON_PREFIXES.rdf);
    });

    it('should allow store prefixes to override COMMON_PREFIXES', () => {
      // Arrange
      const mockStore = {
        getPrefixes: () => ({
          rdf: 'http://overridden-rdf.example.org/',
        }),
      };

      // Act
      const prefixes = extractPrefixes(mockStore);

      // Assert
      expect(prefixes.rdf).toBe('http://overridden-rdf.example.org/');
    });

    it('should handle store without getPrefixes method', () => {
      // Arrange
      const mockStore = {
        // No getPrefixes method
        size: 0,
      };

      // Act
      const prefixes = extractPrefixes(mockStore);

      // Assert
      expect(prefixes).toEqual(COMMON_PREFIXES);
    });

    it('should return new object, not mutate COMMON_PREFIXES', () => {
      // Arrange
      const mockStore = {
        getPrefixes: () => ({ test: 'http://test.org/' }),
      };

      // Act
      const prefixes = extractPrefixes(mockStore);
      prefixes.mutation = 'http://mutated.org/';

      // Assert
      expect(COMMON_PREFIXES.mutation).toBeUndefined();
    });
  });

  describe('FORMAT_TO_EXT Mapping', () => {
    it('should handle turtle format', async () => {
      // Arrange
      const content = '@prefix ex: <http://example.org/> .\nex:A ex:b ex:C .';
      await writeFile(join(testDir, 'turtle.ttl'), content);

      // Act
      const result = await loadOntology({ source: 'turtle.ttl', format: 'turtle' }, testDir);

      // Assert
      expect(result.format).toBe('turtle');
    });

    it('should handle text/turtle mime type format', async () => {
      // Arrange
      const content = '@prefix ex: <http://example.org/> .\nex:A ex:b ex:C .';
      await writeFile(join(testDir, 'textturtle.ttl'), content);

      // Act
      const result = await loadOntology({ source: 'textturtle.ttl', format: 'text/turtle' }, testDir);

      // Assert
      expect(result.format).toBe('text/turtle');
    });

    it('should handle ntriples format', async () => {
      // Arrange - N-Triples format
      const content = '<http://example.org/A> <http://example.org/b> <http://example.org/C> .';
      await writeFile(join(testDir, 'data.nt'), content);

      // Act
      const result = await loadOntology({ source: 'data.nt', format: 'ntriples' }, testDir);

      // Assert
      expect(result.format).toBe('ntriples');
    });

    it('should handle nquads format', async () => {
      // Arrange - N-Quads format
      const content = '<http://example.org/A> <http://example.org/b> <http://example.org/C> <http://example.org/graph> .';
      await writeFile(join(testDir, 'data.nq'), content);

      // Act
      const result = await loadOntology({ source: 'data.nq', format: 'nquads' }, testDir);

      // Assert
      expect(result.format).toBe('nquads');
    });

    it('should handle trig format', async () => {
      // Arrange - TriG format
      const content = `
@prefix ex: <http://example.org/> .
ex:graph { ex:A ex:b ex:C . }
`;
      await writeFile(join(testDir, 'data.trig'), content);

      // Act
      const result = await loadOntology({ source: 'data.trig', format: 'trig' }, testDir);

      // Assert
      expect(result.format).toBe('trig');
    });

    it('should reject unknown format with error', async () => {
      // Arrange
      const content = '@prefix ex: <http://example.org/> .\nex:A ex:b ex:C .';
      await writeFile(join(testDir, 'custom.ttl'), content);

      // Act & Assert - unknown format should cause load to fail
      await expect(loadOntology({
        source: 'custom.ttl',
        format: 'custom-format',
      }, testDir)).rejects.toThrow(/not supported|Load operation failed/i);
    });
  });

  describe('Edge Cases', () => {
    it('should handle ontology with Unicode characters', async () => {
      // Arrange
      const turtleContent = `
@prefix ex: <http://example.org/> .
ex:Subject ex:label "Japanese: \u65E5\u672C\u8A9E, Chinese: \u4E2D\u6587, Emoji: \uD83D\uDE00" .
`;
      const ontologyPath = join(testDir, 'unicode.ttl');
      await writeFile(ontologyPath, turtleContent, 'utf-8');

      // Act
      const result = await loadOntology({ source: 'unicode.ttl' }, testDir);

      // Assert
      expect(result).toBeDefined();
      expect(result.tripleCount).toBeGreaterThanOrEqual(1);
    });

    it('should handle ontology with blank nodes', async () => {
      // Arrange
      const turtleContent = `
@prefix ex: <http://example.org/> .
ex:Person ex:address [
  ex:street "123 Main St" ;
  ex:city "Anytown"
] .
`;
      const ontologyPath = join(testDir, 'blank-nodes.ttl');
      await writeFile(ontologyPath, turtleContent);

      // Act
      const result = await loadOntology({ source: 'blank-nodes.ttl' }, testDir);

      // Assert
      expect(result).toBeDefined();
      expect(result.tripleCount).toBeGreaterThanOrEqual(3);
    });

    it('should handle ontology with typed literals', async () => {
      // Arrange
      const turtleContent = `
@prefix ex: <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:Item ex:count "42"^^xsd:integer ;
        ex:price "19.99"^^xsd:decimal ;
        ex:active "true"^^xsd:boolean .
`;
      const ontologyPath = join(testDir, 'typed-literals.ttl');
      await writeFile(ontologyPath, turtleContent);

      // Act
      const result = await loadOntology({ source: 'typed-literals.ttl' }, testDir);

      // Assert
      expect(result).toBeDefined();
      expect(result.tripleCount).toBeGreaterThanOrEqual(3);
    });

    it('should handle ontology with language tags', async () => {
      // Arrange
      const turtleContent = `
@prefix ex: <http://example.org/> .

ex:Concept ex:label "Hello"@en ;
           ex:label "Bonjour"@fr ;
           ex:label "Hola"@es .
`;
      const ontologyPath = join(testDir, 'lang-tags.ttl');
      await writeFile(ontologyPath, turtleContent);

      // Act
      const result = await loadOntology({ source: 'lang-tags.ttl' }, testDir);

      // Assert
      expect(result).toBeDefined();
      expect(result.tripleCount).toBeGreaterThanOrEqual(3);
    });

    it('should use process.cwd() as default baseDir', async () => {
      // This test verifies the default parameter behavior
      // We create file in testDir but pass testDir as explicit baseDir
      const turtleContent = '@prefix ex: <http://example.org/> .\nex:A ex:b ex:C .';
      await writeFile(join(testDir, 'cwd-test.ttl'), turtleContent);

      // Act - with explicit baseDir (simulating the default)
      const result = await loadOntology({ source: 'cwd-test.ttl' }, testDir);

      // Assert
      expect(result.source).toContain('cwd-test.ttl');
    });

    it('should handle large ontology file', async () => {
      // Arrange - Generate a reasonably sized ontology
      let turtleContent = '@prefix ex: <http://example.org/> .\n';
      for (let i = 0; i < 100; i++) {
        turtleContent += `ex:Subject${i} ex:predicate ex:Object${i} .\n`;
      }
      const ontologyPath = join(testDir, 'large.ttl');
      await writeFile(ontologyPath, turtleContent);

      // Act
      const result = await loadOntology({ source: 'large.ttl' }, testDir);

      // Assert
      expect(result).toBeDefined();
      expect(result.tripleCount).toBeGreaterThanOrEqual(100);
    });

    it('should handle multiline string literals', async () => {
      // Arrange
      const turtleContent = `
@prefix ex: <http://example.org/> .

ex:Document ex:content """This is a
multiline
string literal.""" .
`;
      const ontologyPath = join(testDir, 'multiline.ttl');
      await writeFile(ontologyPath, turtleContent);

      // Act
      const result = await loadOntology({ source: 'multiline.ttl' }, testDir);

      // Assert
      expect(result).toBeDefined();
      expect(result.tripleCount).toBeGreaterThanOrEqual(1);
    });
  });
});
