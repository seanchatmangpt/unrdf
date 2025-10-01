/**
 * @fileoverview Tests for CLI prefix management commands
 * Testing prefix listing, expansion, and shrinking
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  execCLI,
  createCLITestContext,
  assertSuccess,
  assertOutputContains
} from './test-helpers.mjs';

describe('CLI: prefix commands', () => {
  let ctx;

  beforeEach(() => {
    ctx = createCLITestContext();
  });

  afterEach(async () => {
    await ctx.cleanup();
  });

  describe('List prefixes (Critical Path)', () => {
    it('should list default prefixes', async () => {
      const result = await execCLI(['prefix', 'list']);

      assertSuccess(result);
      assertOutputContains(result.stdout, 'Known prefixes:');
      assertOutputContains(result.stdout, 'foaf:');
      assertOutputContains(result.stdout, 'schema:');
      assertOutputContains(result.stdout, 'rdf:');
      assertOutputContains(result.stdout, 'rdfs:');
      assertOutputContains(result.stdout, 'owl:');
    });

    it('should display prefix namespaces', async () => {
      const result = await execCLI(['prefix', 'list']);

      assertSuccess(result);
      assertOutputContains(result.stdout, 'http://xmlns.com/foaf/0.1/');
      assertOutputContains(result.stdout, 'https://schema.org/');
      assertOutputContains(result.stdout, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#');
    });
  });

  describe('Expand CURIEs (Critical Path)', () => {
    it('should expand foaf CURIE', async () => {
      const result = await execCLI(['prefix', 'expand', 'foaf:Person']);

      assertSuccess(result);
      assertOutputContains(result.stdout, 'http://xmlns.com/foaf/0.1/Person');
    });

    it('should expand schema CURIE', async () => {
      const result = await execCLI(['prefix', 'expand', 'schema:name']);

      assertSuccess(result);
      assertOutputContains(result.stdout, 'https://schema.org/name');
    });

    it('should expand rdf CURIE', async () => {
      const result = await execCLI(['prefix', 'expand', 'rdf:type']);

      assertSuccess(result);
      assertOutputContains(result.stdout, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
    });

    it('should expand rdfs CURIE', async () => {
      const result = await execCLI(['prefix', 'expand', 'rdfs:label']);

      assertSuccess(result);
      assertOutputContains(result.stdout, 'http://www.w3.org/2000/01/rdf-schema#label');
    });

    it('should expand owl CURIE', async () => {
      const result = await execCLI(['prefix', 'expand', 'owl:Class']);

      assertSuccess(result);
      assertOutputContains(result.stdout, 'http://www.w3.org/2002/07/owl#Class');
    });

    it('should handle unknown prefix gracefully', async () => {
      const result = await execCLI(['prefix', 'expand', 'unknown:term']);

      assertSuccess(result);
      // Should return the original or indicate unknown
      expect(result.stdout).toBeTruthy();
    });
  });

  describe('Shrink IRIs (Critical Path)', () => {
    it('should shrink foaf IRI', async () => {
      const result = await execCLI(['prefix', 'shrink', 'http://xmlns.com/foaf/0.1/Person']);

      assertSuccess(result);
      assertOutputContains(result.stdout, 'foaf:Person');
    });

    it('should shrink schema IRI', async () => {
      const result = await execCLI(['prefix', 'shrink', 'https://schema.org/name']);

      assertSuccess(result);
      assertOutputContains(result.stdout, 'schema:name');
    });

    it('should shrink rdf IRI', async () => {
      const result = await execCLI(['prefix', 'shrink', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type']);

      assertSuccess(result);
      assertOutputContains(result.stdout, 'rdf:type');
    });

    it('should handle IRI without known prefix', async () => {
      const result = await execCLI(['prefix', 'shrink', 'http://unknown.org/term']);

      assertSuccess(result);
      // Should return original IRI or indicate no prefix match
      expect(result.stdout).toBeTruthy();
    });
  });

  describe('Round-trip conversion', () => {
    it('should expand then shrink to original CURIE', async () => {
      const expandResult = await execCLI(['prefix', 'expand', 'foaf:Person']);
      assertSuccess(expandResult);

      // Extract expanded IRI from output
      const iriMatch = expandResult.stdout.match(/http[^\s]+/);
      expect(iriMatch).toBeTruthy();

      const iri = iriMatch[0];
      const shrinkResult = await execCLI(['prefix', 'shrink', iri]);
      assertSuccess(shrinkResult);

      assertOutputContains(shrinkResult.stdout, 'foaf:Person');
    });
  });

  describe('Edge cases', () => {
    it('should handle CURIEs with special characters', async () => {
      const result = await execCLI(['prefix', 'expand', 'foaf:knows_1']);

      assertSuccess(result);
      expect(result.stdout).toContain('http://xmlns.com/foaf/0.1/');
    });

    it('should handle IRIs with fragments', async () => {
      const result = await execCLI(['prefix', 'shrink', 'http://xmlns.com/foaf/0.1/Person#definition']);

      assertSuccess(result);
      expect(result.stdout).toBeTruthy();
    });

    it('should handle IRIs with query parameters', async () => {
      const result = await execCLI(['prefix', 'shrink', 'http://xmlns.com/foaf/0.1/Person?version=0.1']);

      assertSuccess(result);
      expect(result.stdout).toBeTruthy();
    });
  });

  describe('Performance', () => {
    it('should expand CURIEs quickly', async () => {
      const curies = [
        'foaf:Person',
        'schema:name',
        'rdf:type',
        'rdfs:label',
        'owl:Class'
      ];

      const start = Date.now();

      for (const curie of curies) {
        const result = await execCLI(['prefix', 'expand', curie]);
        assertSuccess(result);
      }

      const duration = Date.now() - start;

      // Performance target: 5 expansions in under 500ms
      expect(duration).toBeLessThan(500);
    });

    it('should shrink IRIs quickly', async () => {
      const iris = [
        'http://xmlns.com/foaf/0.1/Person',
        'https://schema.org/name',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'http://www.w3.org/2000/01/rdf-schema#label',
        'http://www.w3.org/2002/07/owl#Class'
      ];

      const start = Date.now();

      for (const iri of iris) {
        const result = await execCLI(['prefix', 'shrink', iri]);
        assertSuccess(result);
      }

      const duration = Date.now() - start;

      // Performance target: 5 shrinks in under 500ms
      expect(duration).toBeLessThan(500);
    });
  });
});
