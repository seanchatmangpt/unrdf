/**
 * @file Behavioural tests for the public exports of @unrdf/composables.
 * Real Oxigraph store, real unctx context, real Vue reactivity - no mocks.
 */

import { describe, it, expect } from 'vitest';
import {
  createStoreContext,
  initStore,
  setStoreContext,
  storeContext,
  useStoreContext,
  useGraph,
  useGraphProjection,
  useDeltaStream,
} from '../src/index.mjs';

const EX = 'http://example.org/';

function sampleQuads(ctx) {
  const alice = ctx.namedNode(`${EX}alice`);
  const bob = ctx.namedNode(`${EX}bob`);
  const name = ctx.namedNode(`${EX}name`);
  const knows = ctx.namedNode(`${EX}knows`);
  return [
    ctx.quad(alice, name, ctx.literal('Alice')),
    ctx.quad(bob, name, ctx.literal('Bob')),
    ctx.quad(alice, knows, bob),
  ];
}

describe('createStoreContext', () => {
  it('starts empty and reports zeroed stats', () => {
    const ctx = createStoreContext();
    expect(ctx.stats()).toEqual({ quads: 0, subjects: 0, predicates: 0, objects: 0, graphs: 0 });
  });

  it('adds quads and reports accurate stats; add() is chainable', () => {
    const ctx = createStoreContext();
    const [a, b, c] = sampleQuads(ctx);
    expect(ctx.add(a, b).add(c)).toBe(ctx);

    const stats = ctx.stats();
    expect(stats.quads).toBe(3);
    expect(stats.subjects).toBe(2); // alice, bob
    expect(stats.predicates).toBe(2); // name, knows
    expect(stats.objects).toBe(3); // "Alice", "Bob", bob
  });

  it('accepts initial quads', () => {
    const seed = createStoreContext();
    const ctx = createStoreContext(sampleQuads(seed));
    expect(ctx.stats().quads).toBe(3);
  });

  it('removes and clears', () => {
    const ctx = createStoreContext();
    const quads = sampleQuads(ctx);
    ctx.add(...quads);
    ctx.remove(quads[0]);
    expect(ctx.stats().quads).toBe(2);
    ctx.clear();
    expect(ctx.stats().quads).toBe(0);
  });

  it('rejects invalid inputs with TypeError', () => {
    expect(() => createStoreContext('nope')).toThrow(TypeError);
    expect(() => createStoreContext([], 'nope')).toThrow(TypeError);
    const ctx = createStoreContext();
    expect(() => ctx.add(null)).toThrow(/null or undefined/);
    expect(() => ctx.add({ foo: 1 })).toThrow(/termType/);
    expect(() => ctx.remove(undefined)).toThrow(TypeError);
    expect(() => ctx.namedNode(5)).toThrow(TypeError);
    expect(() => ctx.literal(5)).toThrow(TypeError);
    expect(() => ctx.blankNode(5)).toThrow(TypeError);
    expect(() => ctx.quad(null, null, null)).toThrow(/subject, predicate, and object/);
  });

  it('term factories produce correctly typed RDF/JS terms', () => {
    const ctx = createStoreContext();
    expect(ctx.namedNode(`${EX}x`)).toMatchObject({ termType: 'NamedNode', value: `${EX}x` });
    expect(ctx.literal('5', ctx.namedNode('http://www.w3.org/2001/XMLSchema#integer'))).toMatchObject({
      termType: 'Literal',
      value: '5',
    });
    expect(ctx.blankNode('b1')).toMatchObject({ termType: 'BlankNode', value: 'b1' });
    const q = ctx.quad(ctx.namedNode(`${EX}s`), ctx.namedNode(`${EX}p`), ctx.literal('o'));
    expect(q.graph.termType).toBe('DefaultGraph');
  });

  it('runs SELECT queries against the store', async () => {
    const ctx = createStoreContext();
    ctx.add(...sampleQuads(ctx));
    const rows = await ctx.query(`SELECT ?n WHERE { ?s <${EX}name> ?n } ORDER BY ?n`);
    expect(rows.map(r => r.get('n').value)).toEqual(['Alice', 'Bob']);
  });

  it('runs ASK queries', async () => {
    const ctx = createStoreContext();
    ctx.add(...sampleQuads(ctx));
    expect(await ctx.query(`ASK { <${EX}alice> <${EX}knows> <${EX}bob> }`)).toBe(true);
    expect(await ctx.query(`ASK { <${EX}bob> <${EX}knows> <${EX}alice> }`)).toBe(false);
  });

  it('applies SPARQL UPDATE operations to the store', async () => {
    const ctx = createStoreContext();
    const result = await ctx.query(`INSERT DATA { <${EX}s> <${EX}p> "v" }`);
    expect(result).toEqual({ type: 'update', ok: true });
    expect(ctx.stats().quads).toBe(1);
    await ctx.query(`DELETE WHERE { <${EX}s> <${EX}p> ?o }`);
    expect(ctx.stats().quads).toBe(0);
  });

  it('rejects empty and unclassifiable queries; wraps engine errors', async () => {
    const ctx = createStoreContext();
    await expect(ctx.query('')).rejects.toThrow(/non-empty SPARQL/);
    await expect(ctx.query('   ')).rejects.toThrow(/non-empty SPARQL/);
    await expect(ctx.query('hello world')).rejects.toThrow(/unknown query type/);
    await expect(ctx.query('SELECT ?s WHERE {')).rejects.toThrow(/Query failed/);
  });

  it('serializes to N-Quads and Turtle and rejects other formats', () => {
    const ctx = createStoreContext();
    ctx.add(...sampleQuads(ctx));
    const nq = ctx.serialize({ format: 'N-Quads' });
    expect(nq.trim().split('\n')).toHaveLength(3);
    expect(nq).toContain(`<${EX}alice> <${EX}name> "Alice"`);

    const ttl = ctx.serialize();
    expect(ttl).toContain('Alice');
    expect(() => ctx.serialize({ format: 'RDF/XML' })).toThrow(/Unsupported serialization format/);
    expect(() => ctx.serialize('bad')).toThrow(TypeError);
  });

  it('canonical hash is deterministic and independent of blank node labels', async () => {
    const build = label => {
      const ctx = createStoreContext();
      const b = ctx.blankNode(label);
      ctx.add(
        ctx.quad(b, ctx.namedNode(`${EX}p`), ctx.literal('x')),
        ctx.quad(ctx.namedNode(`${EX}s`), ctx.namedNode(`${EX}q`), b)
      );
      return ctx;
    };
    const h1 = await build('one').hash();
    const h2 = await build('two').hash();
    expect(h1).toMatch(/^[0-9a-f]{64}$/);
    expect(h1).toBe(h2);

    const other = createStoreContext();
    other.add(other.quad(other.namedNode(`${EX}s`), other.namedNode(`${EX}p`), other.literal('y')));
    expect(await other.hash()).not.toBe(h1);
  });

  it('canonicalize() emits N-Quads and reports a metric', async () => {
    const ctx = createStoreContext();
    ctx.add(...sampleQuads(ctx));
    const metrics = [];
    const canonical = await ctx.canonicalize({ onMetric: (name, data) => metrics.push([name, data]) });
    expect(canonical.trim().split('\n')).toHaveLength(3);
    expect(metrics).toHaveLength(1);
    expect(metrics[0][0]).toBe('canonicalization');
    expect(metrics[0][1].size).toBeGreaterThan(0);
  });

  it('isIsomorphic distinguishes equal from different graphs', async () => {
    const a = createStoreContext();
    a.add(...sampleQuads(a));
    const b = createStoreContext();
    b.add(...sampleQuads(b));
    const c = createStoreContext();
    c.add(sampleQuads(c)[0]);

    expect(await a.isIsomorphic(a.store, b.store)).toBe(true);
    expect(await a.isIsomorphic(a.store, c.store)).toBe(false);
    await expect(a.isIsomorphic(a.store)).rejects.toThrow(TypeError);
  });
});

describe('store context propagation (unctx)', () => {
  it('useStoreContext throws outside an initialised context', () => {
    expect(() => useStoreContext()).toThrow();
  });

  it('initStore exposes one shared context across async boundaries', async () => {
    const run = initStore([], {});
    const stats = await run(async () => {
      const ctx = useStoreContext();
      ctx.add(ctx.quad(ctx.namedNode(`${EX}s`), ctx.namedNode(`${EX}p`), ctx.literal('o')));
      await new Promise(resolve => setTimeout(resolve, 5));
      return useStoreContext().stats();
    });
    expect(stats.quads).toBe(1);
  });

  it('setStoreContext installs a context for the current execution scope', () => {
    const ctx = setStoreContext();
    try {
      expect(useStoreContext()).toBe(ctx);
    } finally {
      storeContext.unset();
    }
  });
});

describe('useGraph / useGraphProjection', () => {
  it('useGraph returns the active context', async () => {
    const run = initStore();
    const same = await run(() => useGraph() === useStoreContext());
    expect(same).toBe(true);
  });

  it('useGraphProjection is a lazily computed projection of the graph context', async () => {
    const run = initStore();
    await run(() => {
      const projection = useGraphProjection(g => g.stats().quads);
      expect(projection.value).toBe(0);
    });
  });

  it('useGraphProjection requires a function', async () => {
    const run = initStore();
    await run(() => {
      expect(() => useGraphProjection('nope')).toThrow(TypeError);
    });
  });
});

describe('useDeltaStream', () => {
  it('appends deltas and exposes them read-only', () => {
    const { deltas, append, clear } = useDeltaStream([{ id: 0 }]);
    expect(deltas.value).toEqual([{ id: 0 }]);

    const d = { id: 1, added: [], removed: [] };
    expect(append(d)).toBe(d);
    expect(deltas.value).toHaveLength(2);

    clear();
    expect(deltas.value).toEqual([]);
  });

  it('does not mutate the seed array', () => {
    const seed = [{ id: 0 }];
    const { append } = useDeltaStream(seed);
    append({ id: 1 });
    expect(seed).toHaveLength(1);
  });

  it('rejects malformed input', () => {
    expect(() => useDeltaStream('x')).toThrow(TypeError);
    const { append } = useDeltaStream();
    expect(() => append(null)).toThrow(TypeError);
    expect(() => append('str')).toThrow(TypeError);
  });

  it('blocks direct writes to the exposed readonly ref value', () => {
    const { deltas } = useDeltaStream();
    // Vue readonly proxies ignore writes (and warn) instead of mutating
    deltas.value.push({ id: 9 });
    expect(deltas.value).toHaveLength(0);
  });
});
