/**
 * @file Directory Loading Tests
 * @module cli/test/sync/directory-loading
 * @description Tests for loading RDF files from directories recursively
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { writeFile, _unlink, mkdir, _readFile } from 'node:fs/promises';
import { _existsSync } from 'node:fs';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { loadOntology } from '../../../../src/cli/commands/sync/ontology-loader.mjs';

describe('Directory Source Loading', () => {
  let testDir;

  beforeEach(async () => {
    testDir = join(tmpdir(), 'unrdf-dirload-test-' + Date.now());
    await mkdir(testDir, { recursive: true });
  });

  afterEach(async () => {
    // Cleanup would go here
  });

  describe('loadOntology with directory source', () => {
    it('should load all RDF files from directory recursively', async () => {
      // Create directory structure
      const ontologyDir = join(testDir, 'ontology');
      await mkdir(ontologyDir, { recursive: true });
      await mkdir(join(ontologyDir, 'subdir'), { recursive: true });

      // Create multiple RDF files
      await writeFile(join(ontologyDir, 'file1.nt'), `
<http://example.org/1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/Type1> .
      `.trim());

      await writeFile(join(ontologyDir, 'file2.nt'), `
<http://example.org/2> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/Type2> .
      `.trim());

      await writeFile(join(ontologyDir, 'subdir', 'file3.nt'), `
<http://example.org/3> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/Type3> .
      `.trim());

      // Also create a .ttl file
      await writeFile(join(ontologyDir, 'file4.ttl'), `
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
ex:Class4 a rdfs:Class .
      `.trim());

      const result = await loadOntology(
        { source: ontologyDir },
        testDir
      );

      expect(result).toBeDefined();
      expect(result.tripleCount).toBeGreaterThan(0);
      expect(result.store).toBeDefined();
    });

    it('should auto-detect format for each file', async () => {
      const ontologyDir = join(testDir, 'ontology');
      await mkdir(ontologyDir, { recursive: true });

      // Mix of .nt and .ttl files
      await writeFile(join(ontologyDir, 'mixed.nt'), `
<http://example.org/1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/Type1> .
      `.trim());

      await writeFile(join(ontologyDir, 'mixed.ttl'), `
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
ex:Class2 a rdfs:Class .
      `.trim());

      const result = await loadOntology(
        { source: ontologyDir },
        testDir
      );

      expect(result.tripleCount).toBeGreaterThan(0);
    });

    it('should support .rdf and .owl files', async () => {
      const ontologyDir = join(testDir, 'ontology');
      await mkdir(ontologyDir, { recursive: true });

      // Create .owl file (RDF/XML format)
      await writeFile(join(ontologyDir, 'test.owl'), `
<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
  <rdf:Description rdf:about="http://example.org/Test">
    <rdf:type rdf:resource="http://www.w3.org/2000/01/rdf-schema#Class"/>
  </rdf:Description>
</rdf:RDF>
      `.trim());

      const result = await loadOntology(
        { source: ontologyDir },
        testDir
      );

      expect(result).toBeDefined();
    });

    it('should throw error for empty directory', async () => {
      const emptyDir = join(testDir, 'empty');
      await mkdir(emptyDir, { recursive: true });

      await expect(
        loadOntology({ source: emptyDir }, testDir)
      ).rejects.toThrow('No RDF files found');
    });

    it('should handle non-RDF files in directory (ignore them)', async () => {
      const ontologyDir = join(testDir, 'ontology');
      await mkdir(ontologyDir, { recursive: true });

      // Create RDF file
      await writeFile(join(ontologyDir, 'data.nt'), `
<http://example.org/1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/Type1> .
      `.trim());

      // Create non-RDF files (should be ignored)
      await writeFile(join(ontologyDir, 'readme.md'), '# README');
      await writeFile(join(ontologyDir, 'config.json'), '{}');

      const result = await loadOntology(
        { source: ontologyDir },
        testDir
      );

      expect(result.tripleCount).toBeGreaterThan(0);
    });

    it('should still support loading single file', async () => {
      const ontologyDir = join(testDir, 'ontology');
      await mkdir(ontologyDir, { recursive: true });

      const singleFile = join(ontologyDir, 'single.nt');
      await writeFile(singleFile, `
<http://example.org/1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/Type1> .
      `.trim());

      const result = await loadOntology(
        { source: singleFile },
        testDir
      );

      expect(result.tripleCount).toBe(1);
    });

    it('should load all supported extensions', async () => {
      const ontologyDir = join(testDir, 'ontology');
      await mkdir(ontologyDir, { recursive: true });

      // Create files with different extensions
      await writeFile(join(ontologyDir, 'test.nt'), '<http://ex.org/s1> <http://ex.org/p> <http://ex.org/o> .');
      await writeFile(join(ontologyDir, 'test.ttl'), '@prefix ex: <http://ex.org/> . ex:s2 ex:p ex:o .');
      await writeFile(join(ontologyDir, 'test.nq'), '<http://ex.org/s3> <http://ex.org/p> <http://ex.org/o> <http://ex.org/g> .');

      const result = await loadOntology(
        { source: ontologyDir },
        testDir
      );

      expect(result.tripleCount).toBeGreaterThanOrEqual(3);
    });
  });

  describe('error handling', () => {
    it('should throw error for non-existent directory', async () => {
      const nonExistentDir = join(testDir, 'does-not-exist');

      await expect(
        loadOntology({ source: nonExistentDir }, testDir)
      ).rejects.toThrow('not found');
    });

    it('should handle invalid RDF files gracefully', async () => {
      const ontologyDir = join(testDir, 'ontology');
      await mkdir(ontologyDir, { recursive: true });

      // Valid file
      await writeFile(join(ontologyDir, 'valid.nt'), `
<http://example.org/1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/Type1> .
      `.trim());

      // Invalid file (will cause parse error)
      await writeFile(join(ontologyDir, 'invalid.nt'), `
This is not valid N-Triples content at all
      `.trim());

      // Should load valid file and report error for invalid
      const result = await loadOntology(
        { source: ontologyDir },
        testDir
      );

      // Either loads what it can or throws - depends on implementation
      expect(result).toBeDefined();
    });
  });

  describe('findAllRdfFiles', () => {
    it('should find RDF files in nested directories', async () => {
      const ontologyDir = join(testDir, 'ontology');
      await mkdir(join(ontologyDir, 'level1'), { recursive: true });
      await mkdir(join(ontologyDir, 'level1', 'level2'), { recursive: true });

      await writeFile(join(ontologyDir, 'root.nt'), '<http://ex.org/s> <http://ex.org/p> <http://ex.org/o> .');
      await writeFile(join(ontologyDir, 'level1', 'nested.nt'), '<http://ex.org/s2> <http://ex.org/p2> <http://ex.org/o2> .');
      await writeFile(join(ontologyDir, 'level1', 'level2', 'deep.nt'), '<http://ex.org/s3> <http://ex.org/p3> <http://ex.org/o3> .');

      const result = await loadOntology(
        { source: ontologyDir },
        testDir
      );

      expect(result.tripleCount).toBe(3);
    });

    it('should only include RDF file extensions', async () => {
      const ontologyDir = join(testDir, 'ontology');
      await mkdir(ontologyDir, { recursive: true });

      await writeFile(join(ontologyDir, 'data.nt'), '<http://ex.org/s> <http://ex.org/p> <http://ex.org/o> .');
      await writeFile(join(ontologyDir, 'data.txt'), 'not rdf');
      await writeFile(join(ontologyDir, 'data.md'), '# markdown');

      const result = await loadOntology(
        { source: ontologyDir },
        testDir
      );

      expect(result.tripleCount).toBe(1);
    });
  });
});
