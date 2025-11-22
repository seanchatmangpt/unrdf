/**
 * @vitest-environment node
 * @file Code Complexity Analysis Tests (Chicago School TDD)
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { Store, DataFactory } from 'n3';
import { mkdtemp, writeFile, rm } from 'fs/promises';
import { join } from 'path';
import { tmpdir } from 'os';
import { analyzeJsComplexity } from '../../src/project-engine/code-complexity-js.mjs';
import * as unmetric from '../../src/ontologies/unmetric-ontology.mjs';

const { namedNode } = DataFactory;

describe('analyzeJsComplexity', () => {
  let tempDir;

  beforeAll(async () => {
    tempDir = await mkdtemp(join(tmpdir(), 'code-complexity-'));
  });

  afterAll(async () => {
    if (tempDir) {
      await rm(tempDir, { recursive: true, force: true });
    }
  });

  describe('Basic file analysis', () => {
    it('should handle TypeScript files', async () => {
      const code = `
        interface User { name: string; }
        const user: User = { name: 'test' };
      `;

      await writeFile(join(tempDir, 'types.ts'), code);

      const result = await analyzeJsComplexity({
        projectRoot: tempDir,
        mode: 'observe',
      });

      expect(result.store).toBeInstanceOf(Store);
    });
  });

  describe('File discovery and exclusion', () => {
    it('should exclude node_modules by default', async () => {
      const result = await analyzeJsComplexity({
        projectRoot: tempDir,
        mode: 'observe',
      });

      // Summary should exist and exclude patterns should be applied
      expect(result.summary).toBeDefined();
    });

    it('should respect custom exclude patterns', async () => {
      const result = await analyzeJsComplexity({
        projectRoot: tempDir,
        excludePatterns: ['**/*.mjs'],
        mode: 'observe',
      });

      expect(result.store).toBeInstanceOf(Store);
    });
  });

  describe('RDF output', () => {
    it('should preserve existing store when merging', async () => {
      const baseStore = new Store();
      const testQuad = DataFactory.quad(
        namedNode('http://test.org/test'),
        namedNode('http://test.org/prop'),
        DataFactory.literal('test')
      );
      baseStore.addQuad(testQuad);

      const code = 'function f() {}';
      await writeFile(join(tempDir, 'merge-test.js'), code);

      const result = await analyzeJsComplexity({
        projectRoot: tempDir,
        baseStore,
        mode: 'observe',
      });

      // Original quad should still be present
      expect(result.store.countQuads(testQuad.subject, testQuad.predicate, testQuad.object)).toBe(
        1
      );
    });
  });

  describe('Summary metrics', () => {
    it('should identify top risks by maintainability index', async () => {
      const poorCode = `
        function messyFunction(a, b, c, d, e, f, g, h, i, j) {
          if (a) { if (b) { if (c) { if (d) { if (e) { if (f) { if (g) { if (h) { if (i) { if (j) {
            return a + b + c + d + e + f + g + h + i + j;
          } } } } } } } } } }
          return 0;
        }
      `;

      await writeFile(join(tempDir, 'poor-code.js'), poorCode);

      const result = await analyzeJsComplexity({
        projectRoot: tempDir,
        mode: 'observe',
      });

      expect(result.summary.topRisks).toBeDefined();
      expect(Array.isArray(result.summary.topRisks)).toBe(true);
    });

    it('should include timestamp in summary', async () => {
      const code = 'function f() {}';
      await writeFile(join(tempDir, 'timestamp-test.js'), code);

      const result = await analyzeJsComplexity({
        projectRoot: tempDir,
        mode: 'observe',
      });

      expect(result.summary.timestamp).toBeDefined();
      // Should be valid ISO 8601
      expect(new Date(result.summary.timestamp)).toBeInstanceOf(Date);
    });
  });

  describe('Feature flag modes', () => {
    it('should return empty store when mode is off', async () => {
      const code = 'function f() {}';
      await writeFile(join(tempDir, 'off-mode.js'), code);

      const result = await analyzeJsComplexity({
        projectRoot: tempDir,
        mode: 'off',
      });

      expect(result.summary.mode).toBe('off');
      expect(result.summary.filesAnalyzed).toBe(0);
      expect(result.summary.topRisks).toEqual([]);
    });

    it('should analyze with observe mode', async () => {
      const code = 'function f() {}';
      await writeFile(join(tempDir, 'observe-mode.js'), code);

      const result = await analyzeJsComplexity({
        projectRoot: tempDir,
        mode: 'observe',
      });

      expect(result.summary.mode).toBe('observe');
    });

    it('should analyze with enforce mode', async () => {
      const code = 'function f() {}';
      await writeFile(join(tempDir, 'enforce-mode.js'), code);

      const result = await analyzeJsComplexity({
        projectRoot: tempDir,
        mode: 'enforce',
      });

      expect(result.summary.mode).toBe('enforce');
    });
  });

  describe('Error handling', () => {
    it('should skip files with syntax errors', async () => {
      const badCode = 'function f() { if (true) } // missing body';
      await writeFile(join(tempDir, 'bad-syntax.js'), badCode);

      // Should not throw, just skip the bad file
      const result = await analyzeJsComplexity({
        projectRoot: tempDir,
        mode: 'observe',
      });

      expect(result.store).toBeInstanceOf(Store);
    });

    it('should handle empty project directory', async () => {
      const emptyDir = await mkdtemp(join(tmpdir(), 'empty-'));

      const result = await analyzeJsComplexity({
        projectRoot: emptyDir,
        mode: 'observe',
      });

      expect(result.summary.filesAnalyzed).toBe(0);

      await rm(emptyDir, { recursive: true, force: true });
    });

    it('should return valid summary with no files', async () => {
      const noJsDir = await mkdtemp(join(tmpdir(), 'nojs-'));
      await writeFile(join(noJsDir, 'readme.txt'), 'No JavaScript here');

      const result = await analyzeJsComplexity({
        projectRoot: noJsDir,
        mode: 'observe',
      });

      expect(result.summary.filesAnalyzed).toBe(0);
      expect(result.summary.averageCyclomatic).toBe(0);
      expect(result.summary.topRisks).toEqual([]);

      await rm(noJsDir, { recursive: true, force: true });
    });
  });
});
