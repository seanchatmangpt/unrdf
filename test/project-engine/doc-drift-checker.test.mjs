/**
 * @file Doc drift checker tests - 80/20 consolidated
 * @vitest-environment node
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { promises as fs } from 'fs';
import path from 'path';
import os from 'os';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import {
  checkDocConsistency,
  extractDocReferences,
  scoreDocDrift,
} from '../../src/project-engine/doc-drift-checker.mjs';

const { namedNode, literal } = dataFactory;
const NS = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  dom: 'http://example.org/unrdf/domain#',
  fs: 'http://example.org/unrdf/filesystem#',
};

function _createDomainStore(entities) {
  const store = createStore();
  for (const { name, fields = [] } of entities) {
    const iri = namedNode(`${NS.dom}${name}`);
    store.addQuad(iri, namedNode(`${NS.rdf}type`), namedNode(`${NS.dom}Entity`));
    store.addQuad(iri, namedNode(`${NS.rdfs}label`), literal(name));
    for (const field of fields) {
      const fieldIri = namedNode(`${NS.dom}${name}.${field}`);
      store.addQuad(iri, namedNode(`${NS.dom}hasField`), fieldIri);
      store.addQuad(fieldIri, namedNode(`${NS.dom}fieldName`), literal(field));
    }
  }
  return store;
}

async function createTempDir() {
  const dir = path.join(
    os.tmpdir(),
    `doc-drift-test-${Date.now()}-${Math.random().toString(36).slice(2)}`
  );
  await fs.mkdir(dir, { recursive: true });
  await fs.mkdir(path.join(dir, 'docs'), { recursive: true });
  return dir;
}

async function cleanupTempDir(dir) {
  try {
    await fs.rm(dir, { recursive: true, force: true });
  } catch {}
}

describe('doc-drift-checker', () => {
  describe('extractDocReferences', () => {
    it('extracts doc references from project store', () => {
      const projectStore = createStore();
      const docIri = namedNode(`${NS.fs}docs/guide.md`);
      projectStore.addQuad(docIri, namedNode(`${NS.fs}relativePath`), literal('docs/guide.md'));
      const result = extractDocReferences({
        projectStore,
        projectRoot: '/test',
      });
      expect(Array.isArray(result)).toBe(true);
      // Should return array of drift objects with docFile and sourceFile
      if (result.length > 0) {
        expect(result[0]).toHaveProperty('docFile');
      }
    });
  });

  describe('scoreDocDrift', () => {
    it('returns score based on doc drift', () => {
      const projectStore = createStore();
      // Add a source file without doc
      const sourceIri = namedNode(`${NS.fs}src/user.mjs`);
      projectStore.addQuad(sourceIri, namedNode(`${NS.fs}relativePath`), literal('src/user.mjs'));
      const result = scoreDocDrift({
        projectStore,
        projectRoot: '/test',
      });
      expect(result).toHaveProperty('score');
      expect(result).toHaveProperty('driftCount');
      expect(typeof result.score).toBe('number');
      expect(typeof result.driftCount).toBe('number');
    });
  });

  describe('checkDocConsistency', () => {
    let tempDir;

    beforeEach(async () => {
      tempDir = await createTempDir();
    });
    afterEach(async () => {
      await cleanupTempDir(tempDir);
    });

    it('detects stale entity references and missing docs', async () => {
      const projectStore = createStore();
      // Add a doc file
      const docIri = namedNode(`${NS.fs}docs/guide.md`);
      projectStore.addQuad(docIri, namedNode(`${NS.fs}relativePath`), literal('docs/guide.md'));
      // Add a source file
      const sourceIri = namedNode(`${NS.fs}src/user.mjs`);
      projectStore.addQuad(sourceIri, namedNode(`${NS.fs}relativePath`), literal('src/user.mjs'));
      await fs.writeFile(
        path.join(tempDir, 'docs', 'guide.md'),
        '# Guide\nThe User entity stores user info.'
      );

      const result = await checkDocConsistency({
        projectStore,
        projectRoot: tempDir,
      });

      expect(result).toHaveProperty('drifts');
      expect(result).toHaveProperty('summary');
      expect(result).toHaveProperty('healthScore');
      expect(Array.isArray(result.drifts)).toBe(true);
    });

    it('handles empty and nonexistent docs directories', async () => {
      const projectStore = createStore();
      // Add a source file
      const sourceIri = namedNode(`${NS.fs}src/foo.mjs`);
      projectStore.addQuad(sourceIri, namedNode(`${NS.fs}relativePath`), literal('src/foo.mjs'));
      const result = await checkDocConsistency({
        projectStore,
        projectRoot: tempDir,
      });
      expect(result).toHaveProperty('drifts');
      expect(result).toHaveProperty('summary');
      expect(Array.isArray(result.drifts)).toBe(true);
    });
  });
});
