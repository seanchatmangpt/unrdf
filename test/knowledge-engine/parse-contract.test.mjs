import { describe, it, expect } from 'vitest';

describe('parseTurtle contract', () => {
  it('parses non-empty TTL into a store with >0 size', async () => {
    const { parseTurtle } = await import('../../src/knowledge-engine/index.mjs');
    const ttl = `@prefix ex: <http://example.org/> . ex:a ex:p ex:b .`;
    const store = await parseTurtle(ttl, 'http://example.org/');
    expect(store).toBeTruthy();
    expect(typeof store.size).toBe('number');
    expect(store.size).toBeGreaterThan(0);
  });

  it('different inputs yield different sizes when triples differ', async () => {
    const { parseTurtle } = await import('../../src/knowledge-engine/index.mjs');
    const ttl1 = `@prefix ex: <http://example.org/> . ex:a ex:p ex:b .`;
    const ttl2 = `@prefix ex: <http://example.org/> . ex:a ex:p ex:b . ex:b ex:p ex:c .`;
    const s1 = await parseTurtle(ttl1, 'http://example.org/');
    const s2 = await parseTurtle(ttl2, 'http://example.org/');
    expect(s2.size).toBeGreaterThan(s1.size);
  });
});


