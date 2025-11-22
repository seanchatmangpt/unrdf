import { describe, it, expect } from 'vitest';

describe('query contract', () => {
  it('SELECT query returns results array and changes with data', async () => {
    const { parseTurtle, query } = await import('../../src/knowledge-engine/index.mjs');
    const ttl = `@prefix ex: <http://example.org/> . ex:a ex:p ex:b . ex:b ex:p ex:c .`;
    const store = await parseTurtle(ttl, 'http://example.org/');
    const q1 = 'SELECT * WHERE { ?s ?p ?o }';
    const r1 = await query(store, q1);
    expect(Array.isArray(r1)).toBe(true);
    expect(r1.length).toBeGreaterThan(0);

    const q2 = 'SELECT * WHERE { ?s <http://example.org/p> ?o }';
    const r2 = await query(store, q2);
    expect(r2.length).toBeLessThanOrEqual(r1.length);
  });
});
