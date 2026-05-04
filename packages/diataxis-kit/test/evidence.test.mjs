/**
 * @file evidence.test.mjs
 * @description Tests for evidence collection module
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { collectEvidence, hashEvidence } from '../src/evidence.mjs';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const fixturesDir = join(__dirname, 'fixtures', 'test-package');

describe('evidence.mjs', () => {
  describe('collectEvidence', () => {
    it('should return valid EvidenceSnapshot structure', async () => {
      const packageJson = {
        name: 'test-package',
        version: '1.0.0',
        keywords: ['test', 'example'],
        bin: { 'test-cli': './bin/cli.js' },
        exports: { '.': './src/index.mjs' }
      };

      const evidence = await collectEvidence(fixturesDir, packageJson);

      // Verify structure
      assert.ok(evidence);
      assert.ok(typeof evidence.readmeContent === 'string' || evidence.readmeContent === null);
      assert.ok(Array.isArray(evidence.readmeHeadings));
      assert.ok(Array.isArray(evidence.examplesFiles));
      assert.ok(typeof evidence.examplesSnippets === 'object');
      assert.ok(Array.isArray(evidence.docsFiles));
      assert.ok(typeof evidence.docsSnippets === 'object');
      assert.ok(Array.isArray(evidence.srcFiles));
      assert.ok(typeof evidence.testFileCount === 'number');
      assert.ok(typeof evidence.binEntries === 'object');
      assert.ok(typeof evidence.exportSurface === 'object');
      assert.ok(Array.isArray(evidence.keywords));
      assert.ok(typeof evidence.hasLicense === 'boolean');
      assert.ok(typeof evidence.hasTsConfig === 'boolean');
      assert.ok(typeof evidence.fingerprint === 'string');
      assert.equal(evidence.fingerprint.length, 64); // SHA256 hex = 64 chars
    });

    it('should extract package.json fields correctly', async () => {
      const packageJson = {
        keywords: ['rdf', 'semantic'],
        bin: { 'my-cli': './bin/cli.js' },
        exports: { '.': './index.js' }
      };

      const evidence = await collectEvidence(fixturesDir, packageJson);

      assert.deepEqual(evidence.keywords, ['rdf', 'semantic']);
      assert.deepEqual(evidence.binEntries, { 'my-cli': './bin/cli.js' });
      assert.deepEqual(evidence.exportSurface, { '.': './index.js' });
    });

    it('should handle missing directories gracefully', async () => {
      const packageJson = { name: 'test' };
      const evidence = await collectEvidence('/tmp/nonexistent-package-dir', packageJson);

      assert.equal(evidence.readmeContent, null);
      assert.deepEqual(evidence.readmeHeadings, []);
      assert.deepEqual(evidence.examplesFiles, []);
      assert.deepEqual(evidence.docsFiles, []);
      assert.deepEqual(evidence.srcFiles, []);
      assert.equal(evidence.testFileCount, 0);
    });

    it('should throw on invalid package.json', async () => {
      await assert.rejects(
        async () => await collectEvidence('/tmp', null),
        { message: /Invalid package.json/ }
      );
    });

    it('should compute deterministic fingerprint', async () => {
      const packageJson = { name: 'test', keywords: ['a', 'b'] };
      const evidence1 = await collectEvidence(fixturesDir, packageJson);
      const evidence2 = await collectEvidence(fixturesDir, packageJson);

      assert.equal(evidence1.fingerprint, evidence2.fingerprint);
    });
  });

  describe('hashEvidence', () => {
    it('should produce deterministic hash', () => {
      const evidence = {
        readmeContent: 'test',
        readmeHeadings: ['Heading 1'],
        examplesFiles: ['example.js'],
        examplesSnippets: { 'example.js': 'code' },
        docsFiles: ['doc.md'],
        docsSnippets: { 'doc.md': 'docs' },
        srcFiles: ['index.js'],
        testFileCount: 5,
        binEntries: {},
        exportSurface: {},
        keywords: ['test'],
        hasLicense: true,
        hasTsConfig: false,
        fingerprint: 'abc123'
      };

      const hash1 = hashEvidence(evidence);
      const hash2 = hashEvidence(evidence);

      assert.equal(hash1, hash2);
      assert.equal(hash1.length, 64); // SHA256 hex
    });

    it('should produce different hashes for different evidence', () => {
      const evidence1 = {
        readmeContent: 'test1',
        readmeHeadings: [],
        examplesFiles: [],
        examplesSnippets: {},
        docsFiles: [],
        docsSnippets: {},
        srcFiles: [],
        testFileCount: 0,
        binEntries: {},
        exportSurface: {},
        keywords: [],
        hasLicense: false,
        hasTsConfig: false,
        fingerprint: 'abc'
      };

      const evidence2 = {
        ...evidence1,
        readmeContent: 'test2'
      };

      const hash1 = hashEvidence(evidence1);
      const hash2 = hashEvidence(evidence2);

      assert.notEqual(hash1, hash2);
    });
  });
});
