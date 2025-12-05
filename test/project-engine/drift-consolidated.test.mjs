/**
 * @file Drift Snapshot Consolidated Tests - 80/20 principle
 * @vitest-environment node
 *
 * Consolidated tests covering 80% of drift detection functionality.
 */

import { describe, it, expect } from 'vitest';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import {
  createStructureSnapshot,
  computeDrift,
  createEmptyBaseline,
} from '../../packages/project-engine/drift-snapshot.mjs';

const { namedNode, literal } = dataFactory;

const NS = {
  fs: 'http://example.org/unrdf/filesystem#',
  proj: 'http://example.org/unrdf/project#',
};

function createMockFsStore() {
  const store = createStore();
  const files = ['src/index.mjs', 'src/user.mjs', 'test/user.test.mjs'];
  for (const filePath of files) {
    const fileIri = namedNode(`${NS.fs}${encodeURIComponent(filePath)}`);
    store.addQuad(fileIri, namedNode(`${NS.fs}relativePath`), literal(filePath));
  }
  return store;
}

describe('drift-snapshot consolidated - 80/20 core', () => {
  // ===== Core Snapshot (80% of value) =====
  it('creates structure snapshot with hash and receipt', () => {
    const fsStore = createMockFsStore();
    const { snapshotStore, receipt } = createStructureSnapshot(fsStore);

    expect(receipt.hash).toBeDefined();
    expect(receipt.hash.length).toBe(32);
    expect(receipt.createdAt).toBeDefined();
    expect(receipt.summary).toBeDefined();
    expect(snapshotStore.size).toBeGreaterThan(0);
  });

  // ===== Core Drift Detection (80% of value) =====
  it('computes drift between current and baseline', () => {
    const currentStore = createMockFsStore();
    const baselineStore = createMockFsStore();

    const drift = computeDrift(currentStore, baselineStore);

    expect(drift).toBeDefined();
    expect(drift.ontologyDiff).toBeDefined();
    expect(drift.summary).toBeDefined();
    expect(drift.driftSeverity).toBeDefined();
    expect(['none', 'minor', 'major']).toContain(drift.driftSeverity);
  });

  it('creates empty baseline for new projects', () => {
    const baseline = createEmptyBaseline();

    // createEmptyBaseline returns a Store
    expect(baseline).toBeDefined();
    expect(baseline.size).toBe(0);
  });
});
