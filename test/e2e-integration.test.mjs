#!/usr/bin/env node
/**
 * AUTONOMIC HYPER INTELLIGENCE: End-to-End Integration Test
 *
 * Tests REAL implementations with NO MOCKS to eliminate false positives.
 * This is the ultimate truth test - if it passes here, it works in production.
 */

import { createDarkMatterCore, parseTurtle, defineHook } from '../src/knowledge-engine/index.mjs';
import { UnrdfDataFactory } from '../packages/core/src/rdf/n3-justified-only.mjs';
import { describe, it, expect } from 'vitest';

const { namedNode, literal, quad } = UnrdfDataFactory;

describe('E2E Integration Tests (NO MOCKS)', () => {
  it('should create and initialize DarkMatterCore', async () => {
    const system = await createDarkMatterCore();
    expect(system.initialized).toBe(true);
    await system.cleanup();
  });

  it('should parse Turtle with real N3.js', async () => {
    const ttl = '@prefix ex: <http://example.org/> . ex:alice ex:knows ex:bob .';
    const store = await parseTurtle(ttl);
    expect(store.size).toBe(1);
  });

  it('should execute transaction with real TransactionManager', async () => {
    const system = await createDarkMatterCore();
    const result = await system.executeTransaction({
      additions: [
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Alice')
        ),
      ],
      removals: [],
      actor: 'test',
    });

    expect(result.receipt.committed).toBe(true);
    await system.cleanup();
  });

  it('should execute SPARQL query with real Comunica', async () => {
    const system = await createDarkMatterCore();

    await system.executeTransaction({
      additions: [
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Alice')
        ),
      ],
      removals: [],
      actor: 'test',
    });

    const results = await system.query({
      query: 'SELECT ?name WHERE { ?person <http://xmlns.com/foaf/0.1/name> ?name }',
      type: 'sparql-select',
    });

    expect(Array.isArray(results)).toBe(true);
    expect(results.length).toBeGreaterThan(0);

    await system.cleanup();
  });

  it('should define hook with real schema validation', async () => {
    const hook = defineHook({
      meta: { name: 'test-hook', description: 'Test' },
      when: { kind: 'sparql-ask', query: 'ASK { ?s ?p ?o }' },
      run: async () => {},
    });

    expect(hook.meta.name).toBe('test-hook');
  });

  it('should handle complete README Quick Start workflow', async () => {
    // Exact code from README.md Quick Start
    const system = await createDarkMatterCore();

    await system.executeTransaction({
      additions: [
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Alice')
        ),
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/knows'),
          namedNode('http://example.org/bob')
        ),
      ],
      removals: [],
      actor: 'system',
    });

    const results = await system.query({
      query: 'SELECT ?name WHERE { ?person <http://xmlns.com/foaf/0.1/name> ?name }',
      type: 'sparql-select',
    });

    expect(results).toEqual([{ name: 'Alice' }]);

    await system.cleanup();
  });
});
