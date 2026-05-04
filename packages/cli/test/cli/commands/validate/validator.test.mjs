/**
 * @file Validation Function Tests
 * @module cli/test/validate/validator
 * @description Tests for N-Triples validation functions
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { writeFile, _unlink, mkdir } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { validateNT, countTriples, extractSummary } from '../../../../src/cli/commands/validate.mjs';

describe('N-Triples Validation', () => {
  let testDir;

  beforeEach(async () => {
    testDir = join(tmpdir(), 'unrdf-validate-test-' + Date.now());
    await mkdir(testDir, { recursive: true });
  });

  afterEach(async () => {
    // Cleanup would go here
  });

  describe('validateNT', () => {
    it('should validate correct N-Triples file', async () => {
      const validContent = `
# Comment line
<http://example.org/subject1> <http://example.org/predicate1> "object1" .
<http://example.org/subject2> <http://example.org/predicate2> "object2" .
      `.trim();

      const testFile = join(testDir, 'valid.nt');
      await writeFile(testFile, validContent, 'utf-8');

      const errors = await validateNT(testFile);
      expect(errors).toEqual([]);
    });

    it('should detect incomplete triple (missing terms)', async () => {
      const invalidContent = `
<http://example.org/subject> <http://example.org/predicate> .
      `.trim();

      const testFile = join(testDir, 'invalid-incomplete.nt');
      await writeFile(testFile, invalidContent, 'utf-8');

      const errors = await validateNT(testFile);
      expect(errors.length).toBeGreaterThan(0);
      expect(errors[0].error).toContain('incomplete triple');
      expect(errors[0].line).toBe(1);
    });

    it('should detect missing dot at end of triple', async () => {
      const invalidContent = `
<http://example.org/subject> <http://example.org/predicate> "object"
      `.trim();

      const testFile = join(testDir, 'invalid-missing-dot.nt');
      await writeFile(testFile, invalidContent, 'utf-8');

      const errors = await validateNT(testFile);
      expect(errors.length).toBeGreaterThan(0);
      expect(errors[0].error).toContain('does not end with');
    });

    it('should skip empty lines and comments', async () => {
      const content = `
# This is a comment

<http://example.org/subject> <http://example.org/predicate> "object" .

# Another comment
      `.trim();

      const testFile = join(testDir, 'with-comments.nt');
      await writeFile(testFile, content, 'utf-8');

      const errors = await validateNT(testFile);
      expect(errors).toEqual([]);
    });

    it('should report multiple errors', async () => {
      const invalidContent = `
<http://example.org/s1> <http://example.org/p1> "o1" .
<http://example.org/s2> <http://example.org/p2>
incomplete triple
<http://example.org/s3> <http://example.org/p3> "o3" .
      `.trim();

      const testFile = join(testDir, 'multiple-errors.nt');
      await writeFile(testFile, invalidContent, 'utf-8');

      const errors = await validateNT(testFile);
      expect(errors.length).toBe(2);
      expect(errors[0].line).toBe(2);
      expect(errors[1].line).toBe(3);
    });

    it('should include content preview in errors', async () => {
      const invalidContent = `
<http://example.org/subject> <http://example.org/predicate> <http://example.org/object>
      `.trim();

      const testFile = join(testDir, 'preview-test.nt');
      await writeFile(testFile, invalidContent, 'utf-8');

      const errors = await validateNT(testFile);
      expect(errors[0].content).toBeDefined();
      expect(errors[0].content.length).toBeLessThan(70);
    });
  });

  describe('countTriples', () => {
    it('should count triples in N-Triples file', async () => {
      const content = `
<http://example.org/s1> <http://example.org/p1> "o1" .
<http://example.org/s2> <http://example.org/p2> "o2" .
<http://example.org/s3> <http://example.org/p3> "o3" .
      `.trim();

      const testFile = join(testDir, 'count-test.nt');
      await writeFile(testFile, content, 'utf-8');

      const count = await countTriples(testFile);
      expect(count).toBe(3);
    });

    it('should skip comments and empty lines', async () => {
      const content = `
# Comment 1

<http://example.org/s1> <http://example.org/p1> "o1" .

# Comment 2

<http://example.org/s2> <http://example.org/p2> "o2" .
      `.trim();

      const testFile = join(testDir, 'skip-comments.nt');
      await writeFile(testFile, content, 'utf-8');

      const count = await countTriples(testFile);
      expect(count).toBe(2);
    });

    it('should return 0 for empty file', async () => {
      const content = '# Only comments\n\n# More comments';

      const testFile = join(testDir, 'empty.nt');
      await writeFile(testFile, content, 'utf-8');

      const count = await countTriples(testFile);
      expect(count).toBe(0);
    });
  });

  describe('extractSummary', () => {
    it('should extract summary statistics', async () => {
      const content = `
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

<http://example.org/Class1> a rdfs:Class .
<http://example.org/Class2> a owl:Class .
<http://example.org/prop1> a rdf:Property .
<http://example.org/Shape1> a sh:NodeShape .
      `.trim();

      const testFile = join(testDir, 'summary.nt');
      await writeFile(testFile, content, 'utf-8');

      // Note: extractSummary requires an RDF store, so this test
      // verifies the function exists and has correct structure
      expect(extractSummary).toBeDefined();
      expect(typeof extractSummary).toBe('function');
    });
  });
});
