/**
 * @file Domain Inference Consolidated Tests - 80/20 principle
 * @vitest-environment node
 *
 * Consolidated tests covering 80% of domain inference functionality.
 * Merges: domain-infer, domain-infer-integration
 */

import { describe, it, expect } from 'vitest';
import { createStore } from '@unrdf/oxigraph';
import { inferDomainModel, DomainModelLens } from '../../src/project-engine/domain-infer.mjs';

const { namedNode, literal } = DataFactory;

const NS = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  dom: 'http://example.org/unrdf/domain#',
  fs: 'http://example.org/unrdf/filesystem#',
};

function createMockFsStore(paths) {
  const store = createStore();
  for (const filePath of paths) {
    const fileIri = namedNode(`${NS.fs}${encodeURIComponent(filePath)}`);
    store.addQuad(fileIri, namedNode(`${NS.fs}relativePath`), literal(filePath));
  }
  return store;
}

describe('domain-infer consolidated - 80/20 core', () => {
  // ===== Core Inference (80% of value) =====
  it('infers domain model from filesystem store', async () => {
    const fsStore = createMockFsStore(['src/schemas/user.ts', 'src/schemas/product.ts']);

    const { store, summary } = await inferDomainModel({
      fsStore,
      stackProfile: { hasZod: true, hasTypescript: true },
    });

    expect(store).toBeDefined();
    expect(summary).toHaveProperty('entityCount');
    expect(summary).toHaveProperty('fieldCount');
    expect(summary).toHaveProperty('relationshipCount');
    expect(typeof summary.entityCount).toBe('number');
  });

  it('returns empty result for empty store', async () => {
    const fsStore = createStore();
    const { store, summary } = await inferDomainModel({ fsStore });

    expect(summary.entityCount).toBe(0);
    expect(summary.fieldCount).toBe(0);
    expect(store.size).toBe(0);
  });

  it('respects stack profile configuration', async () => {
    const fsStore = createMockFsStore(['src/types/user.ts']);

    const { store, summary } = await inferDomainModel({
      fsStore,
      stackProfile: {
        hasZod: false,
        hasTypescript: true,
        sourceRoot: 'src',
      },
    });

    expect(store).toBeDefined();
    expect(summary).toHaveProperty('entityCount');
  });

  // ===== Domain Model Lens (20% of value) =====
  it('detects entity changes via DomainModelLens', () => {
    const triple = {
      subject: `${NS.dom}User`,
      predicate: `${NS.rdf}type`,
      object: `${NS.dom}Entity`,
    };

    const change = DomainModelLens(triple, 'added');
    expect(change).not.toBeNull();
    expect(change.kind).toBe('EntityAdded');
    expect(change.entity).toBe(`${NS.dom}User`);
  });
});
