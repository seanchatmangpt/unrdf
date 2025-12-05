/**
 * @file Hotspot analyzer tests - 80/20 consolidated
 * @vitest-environment node
 */

import { describe, it, expect } from 'vitest';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { analyzeHotspots, scoreFeature } from '../../packages/project-engine/hotspot-analyzer.mjs';

const { namedNode, literal } = dataFactory;
const NS = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  xsd: 'http://www.w3.org/2001/XMLSchema#',
  fs: 'http://example.org/unrdf/filesystem#',
  proj: 'http://example.org/unrdf/project#',
};

function createProjectStore(config = {}) {
  const store = createStore();
  const { features = {}, baseIri = 'http://example.org/unrdf/project#' } = config;
  const projectIri = namedNode(`${baseIri}project`);
  store.addQuad(projectIri, namedNode(`${NS.rdf}type`), namedNode(`${NS.proj}Project`));

  for (const [featureName, featureConfig] of Object.entries(features)) {
    const featureIri = namedNode(`${baseIri}feature/${encodeURIComponent(featureName)}`);
    store.addQuad(featureIri, namedNode(`${NS.rdf}type`), namedNode(`${NS.proj}Feature`));
    store.addQuad(featureIri, namedNode(`${NS.rdfs}label`), literal(featureName));
    store.addQuad(projectIri, namedNode(`${NS.proj}hasFeature`), featureIri);

    const { files = [], byteSizes = {}, roles = {} } = featureConfig;
    for (const filePath of files) {
      const fileIri = namedNode(`http://example.org/unrdf/fs#${encodeURIComponent(filePath)}`);
      store.addQuad(fileIri, namedNode(`${NS.fs}relativePath`), literal(filePath));
      store.addQuad(fileIri, namedNode(`${NS.proj}belongsToFeature`), featureIri);
      store.addQuad(
        fileIri,
        namedNode(`${NS.fs}byteSize`),
        literal(byteSizes[filePath] || 2000, namedNode(`${NS.xsd}integer`))
      );
      if (roles[filePath])
        store.addQuad(fileIri, namedNode(`${NS.proj}roleString`), literal(roles[filePath]));
    }
  }
  return store;
}

describe('hotspot-analyzer', () => {
  describe('analyzeHotspots', () => {
    it('returns empty for empty store', () => {
      const result = analyzeHotspots({ projectStore: createStore() });
      expect(result.hotspots).toHaveLength(0);
      expect(result.summary).toBe('No high-risk features identified');
    });

    it('scores features based on complexity', () => {
      const projectStore = createProjectStore({
        features: {
          small: {
            files: ['a.mjs', 'a.test.mjs'],
            roles: { 'a.test.mjs': 'Test' },
          },
          large: { files: Array.from({ length: 40 }, (_, i) => `f${i}.mjs`) },
        },
      });
      const result = analyzeHotspots({ projectStore });

      expect(result.hotspots[0].feature).toBe('large');
      expect(result.hotspots[0].risk).toBe('HIGH');
      expect(result.hotspots[0].score).toBeGreaterThan(70);
    });

    it('lowers score for well-tested features', () => {
      const files = [];
      const roles = {};
      for (let i = 0; i < 25; i++) files.push(`src${i}.mjs`);
      for (let i = 0; i < 20; i++) {
        const testFile = `test${i}.test.mjs`;
        files.push(testFile);
        roles[testFile] = 'Test';
      }
      const projectStore = createProjectStore({
        features: { 'well-tested': { files, roles } },
      });
      const result = analyzeHotspots({ projectStore });

      expect(result.hotspots[0].metrics.testCount).toBe(20);
      expect(result.hotspots[0].score).toBeLessThan(80);
    });

    it('sorts by score descending and identifies top risks', () => {
      const projectStore = createProjectStore({
        features: {
          checkout: {
            files: Array.from({ length: 35 }, (_, i) => `checkout${i}.mjs`),
          },
          utils: {
            files: ['util.mjs', 'util.test.mjs'],
            roles: { 'util.test.mjs': 'Test' },
          },
        },
      });
      const result = analyzeHotspots({ projectStore });

      expect(result.hotspots[0].feature).toBe('checkout');
      expect(result.topRisks[0].feature).toBe('checkout');
    });
  });

  describe('scoreFeature', () => {
    it('calculates score based on metrics', () => {
      expect(
        scoreFeature('empty', {
          fileCount: 0,
          lineCount: 0,
          testCount: 0,
          testCoverage: 100,
          dependencies: 0,
        })
      ).toBe(0);
      expect(
        scoreFeature('risky', {
          fileCount: 40,
          lineCount: 8000,
          testCount: 0,
          testCoverage: 0,
          dependencies: 15,
        })
      ).toBe(100);
    });

    it('weights test coverage heavily (40%)', () => {
      const base = { fileCount: 20, lineCount: 2000, dependencies: 5 };
      const zeroCoverage = scoreFeature('zero', {
        ...base,
        testCount: 0,
        testCoverage: 0,
      });
      const fullCoverage = scoreFeature('full', {
        ...base,
        testCount: 20,
        testCoverage: 100,
      });
      expect(zeroCoverage - fullCoverage).toBeGreaterThanOrEqual(35);
    });

    it('validates metrics with Zod', () => {
      expect(() =>
        scoreFeature('invalid', {
          fileCount: -1,
          lineCount: 0,
          testCount: 0,
          testCoverage: 0,
          dependencies: 0,
        })
      ).toThrow();
    });
  });
});
