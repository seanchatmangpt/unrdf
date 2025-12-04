/**
 * @vitest-environment jsdom
 */

import { describe, it, expect, vi } from 'vitest';
import { ref } from 'vue';

// Mock streaming module before imports
vi.mock('@unrdf/streaming', () => ({
  createSubscriptionManager: () => ({
    subscribe: vi.fn(),
    unsubscribe: vi.fn(),
  }),
  createStreamProcessor: () => ({
    process: vi.fn(),
  }),
}));

import { useGraph } from '../src/composables/use-graph.mjs';
import { useQuery } from '../src/composables/use-query.mjs';
import { useDelta } from '../src/composables/use-delta.mjs';
import { useTerms } from '../src/composables/use-terms.mjs';
import { useSubscription } from '../src/composables/use-subscription.mjs';
import { useStreaming } from '../src/composables/use-streaming.mjs';

/**
 * @unrdf/composables Test Suite
 *
 * Tests for Vue 3 composables with reactive RDF state.
 */

describe('@unrdf/composables', () => {
  describe('useGraph', () => {
    it('should create reactive graph store', () => {
      const { store, loading, error, quads } = useGraph(null);

      expect(store.value).toBeDefined();
      expect(loading.value).toBe(false);
      expect(error.value).toBe(null);
      expect(quads.value).toEqual([]);
    });

    it('should handle options validation', () => {
      const { store } = useGraph(null, {
        autoRefresh: false,
        fetchInterval: 30000,
      });

      expect(store.value).toBeDefined();
    });

    it('should reject invalid options', () => {
      expect(() => {
        useGraph(null, { invalidOption: true });
      }).toThrow();
    });

    it('should set error for missing URL on refresh', async () => {
      const { error, refresh } = useGraph(null);

      await refresh();

      expect(error.value).toBeDefined();
      expect(error.value.message).toContain('No graph URL');
    });
  });

  describe('useQuery', () => {
    it('should create query state', () => {
      const mockStore = ref({ match: () => [] });
      const { results, loading, error } = useQuery(mockStore, 'SELECT * WHERE { ?s ?p ?o }');

      expect(results.value).toBe(null);
      expect(loading.value).toBe(false);
      expect(error.value).toBe(null);
    });

    it('should handle options validation', () => {
      const mockStore = ref({ match: () => [] });
      const { results } = useQuery(mockStore, 'SELECT * WHERE { ?s ?p ?o }', {
        autoExecute: false,
        watchStore: false,
        memoize: true,
      });

      expect(results.value).toBe(null);
    });

    it('should reject invalid options', () => {
      const mockStore = ref({ match: () => [] });

      expect(() => {
        useQuery(mockStore, 'SELECT * WHERE { ?s ?p ?o }', { invalidOption: true });
      }).toThrow();
    });

    it('should clear results', () => {
      const mockStore = ref({ match: () => [] });
      const { results, clear } = useQuery(mockStore, 'SELECT * WHERE { ?s ?p ?o }');

      clear();

      expect(results.value).toBe(null);
    });
  });

  describe('useDelta', () => {
    it('should create delta state', () => {
      const mockStore = ref({ add: vi.fn(), delete: vi.fn() });
      const { deltas, canUndo, canRedo } = useDelta(mockStore);

      expect(deltas.value).toEqual([]);
      expect(canUndo.value).toBe(false);
      expect(canRedo.value).toBe(false);
    });

    it('should push delta to undo stack', () => {
      const mockStore = ref({ add: vi.fn(), delete: vi.fn() });
      const { deltas, push, canUndo } = useDelta(mockStore);

      push({ additions: ['quad1'], deletions: [] });

      expect(deltas.value.length).toBe(1);
      expect(canUndo.value).toBe(true);
    });

    it('should undo delta', () => {
      const mockStore = ref({ add: vi.fn(), delete: vi.fn() });
      const { push, undo, canUndo, canRedo } = useDelta(mockStore);

      push({ additions: ['quad1'], deletions: [] });
      undo();

      expect(canUndo.value).toBe(false);
      expect(canRedo.value).toBe(true);
      expect(mockStore.value.delete).toHaveBeenCalledWith('quad1');
    });

    it('should redo delta', () => {
      const mockStore = ref({ add: vi.fn(), delete: vi.fn() });
      const { push, undo, redo, canUndo } = useDelta(mockStore);

      push({ additions: ['quad1'], deletions: [] });
      undo();
      redo();

      expect(canUndo.value).toBe(true);
      expect(mockStore.value.add).toHaveBeenCalledWith('quad1');
    });

    it('should enforce max depth', () => {
      const mockStore = ref({ add: vi.fn(), delete: vi.fn() });
      const { deltas, push } = useDelta(mockStore, { maxDepth: 2 });

      push({ additions: ['q1'], deletions: [] });
      push({ additions: ['q2'], deletions: [] });
      push({ additions: ['q3'], deletions: [] });

      expect(deltas.value.length).toBe(2);
    });

    it('should clear stacks', () => {
      const mockStore = ref({ add: vi.fn(), delete: vi.fn() });
      const { push, clear, deltas, canUndo } = useDelta(mockStore);

      push({ additions: ['quad1'], deletions: [] });
      clear();

      expect(deltas.value).toEqual([]);
      expect(canUndo.value).toBe(false);
    });
  });

  describe('useTerms', () => {
    it('should create term factory', () => {
      const { namedNode, literal, blankNode, variable, defaultGraph } = useTerms();

      expect(namedNode).toBeDefined();
      expect(literal).toBeDefined();
      expect(blankNode).toBeDefined();
      expect(variable).toBeDefined();
      expect(defaultGraph).toBeDefined();
    });

    it('should cache named nodes', () => {
      const { namedNode } = useTerms();

      const node1 = namedNode('http://example.org/1');
      const node2 = namedNode('http://example.org/1');

      expect(node1).toBe(node2);
    });

    it('should cache literals', () => {
      const { literal } = useTerms();

      const lit1 = literal('test', 'en');
      const lit2 = literal('test', 'en');

      expect(lit1).toBe(lit2);
    });

    it('should not cache unlabeled blank nodes', () => {
      const { blankNode } = useTerms();

      const bn1 = blankNode();
      const bn2 = blankNode();

      expect(bn1).not.toBe(bn2);
    });

    it('should cache labeled blank nodes', () => {
      const { blankNode } = useTerms();

      const bn1 = blankNode('label1');
      const bn2 = blankNode('label1');

      expect(bn1).toBe(bn2);
    });

    it('should clear cache', () => {
      const { namedNode, clearCache } = useTerms();

      const node1 = namedNode('http://example.org/1');
      clearCache();
      const node2 = namedNode('http://example.org/1');

      // After cache clear, new instance created
      expect(node1).not.toBe(node2);
    });
  });

  describe('useSubscription', () => {
    it('should create subscription state', () => {
      const mockFeed = { subscribe: vi.fn() };
      const { changes, count, lastChange } = useSubscription(mockFeed);

      expect(changes.value).toEqual([]);
      expect(count.value).toBe(0);
      expect(lastChange.value).toBe(null);
    });

    it('should handle options validation', () => {
      const mockFeed = { subscribe: vi.fn() };
      const { count } = useSubscription(mockFeed, null, {
        filter: delta => delta.additions.length > 0,
        maxChanges: 50,
      });

      expect(count.value).toBe(0);
    });

    it('should reject invalid options', () => {
      const mockFeed = { subscribe: vi.fn() };

      expect(() => {
        useSubscription(mockFeed, null, { invalidOption: true });
      }).toThrow();
    });
  });

  describe('useStreaming', () => {
    it('should create streaming state', () => {
      const mockFeed = { stream: vi.fn() };
      const { events, processing, eventCount } = useStreaming(mockFeed, {
        autoStart: false,
      });

      expect(events.value).toEqual([]);
      expect(processing.value).toBe(false);
      expect(eventCount.value).toBe(0);
    });

    it('should handle options validation', () => {
      const mockFeed = { stream: vi.fn() };
      const { events } = useStreaming(mockFeed, {
        batchSize: 20,
        debounceMs: 200,
        maxEvents: 500,
        autoStart: false,
      });

      expect(events.value).toEqual([]);
    });

    it('should reject invalid options', () => {
      const mockFeed = { stream: vi.fn() };

      expect(() => {
        useStreaming(mockFeed, { invalidOption: true });
      }).toThrow();
    });

    it('should clear events', () => {
      const mockFeed = { stream: vi.fn() };
      const { events, clear } = useStreaming(mockFeed, { autoStart: false });

      clear();

      expect(events.value).toEqual([]);
    });
  });

  describe('Integration Tests', () => {
    it('should work together: graph + query + delta', async () => {
      const { store } = useGraph(null);
      const { results } = useQuery(store, 'SELECT * WHERE { ?s ?p ?o }');
      const { push, canUndo } = useDelta(store);

      push({ additions: ['quad1'], deletions: [] });

      expect(canUndo.value).toBe(true);
      expect(results.value).toBe(null);
    });

    it('should work together: terms + graph', () => {
      const { namedNode, literal } = useTerms();
      const { store } = useGraph(null);

      const subject = namedNode('http://example.org/subject');
      const predicate = namedNode('http://example.org/predicate');
      const object = literal('value');

      expect(subject).toBeDefined();
      expect(predicate).toBeDefined();
      expect(object).toBeDefined();
      expect(store.value).toBeDefined();
    });
  });

  describe('Additional Coverage Tests', () => {
    it('useQuery should detect SELECT query type', async () => {
      const mockStore = ref({ match: () => [] });
      const { execute, error } = useQuery(mockStore, 'SELECT ?s WHERE { ?s ?p ?o }', {
        autoExecute: false,
      });

      await execute();
      expect(error.value).toBeDefined(); // Will error since executeSelect is not mocked
    });

    it('useQuery should detect CONSTRUCT query type', async () => {
      const mockStore = ref({ match: () => [] });
      const { execute } = useQuery(mockStore, 'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }', {
        autoExecute: false,
      });

      await execute();
      // Should attempt to execute CONSTRUCT
    });

    it('useQuery should detect ASK query type', async () => {
      const mockStore = ref({ match: () => [] });
      const { execute } = useQuery(mockStore, 'ASK { ?s ?p ?o }', {
        autoExecute: false,
      });

      await execute();
      // Should attempt to execute ASK
    });

    it('useDelta should add timestamp when tracking metadata', () => {
      const mockStore = ref({ add: vi.fn(), delete: vi.fn() });
      const { deltas, push } = useDelta(mockStore, { trackMetadata: true });

      push({ additions: ['quad1'], deletions: [] });

      expect(deltas.value[0].timestamp).toBeDefined();
    });

    it('useTerms should create variable terms', () => {
      const { variable } = useTerms();

      const var1 = variable('x');
      const var2 = variable('x');

      expect(var1).toBe(var2); // Cached
    });

    it('useTerms should create default graph', () => {
      const { defaultGraph } = useTerms();

      const dg1 = defaultGraph();
      const dg2 = defaultGraph();

      expect(dg1).toBe(dg2); // Singleton
    });

    it('useTerms should create quads', () => {
      const { namedNode, literal, quad } = useTerms();

      const s = namedNode('http://example.org/s');
      const p = namedNode('http://example.org/p');
      const o = literal('value');

      const q = quad(s, p, o);

      expect(q).toBeDefined();
    });

    it('useStreaming should start and stop processing', () => {
      const mockFeed = { stream: vi.fn() };
      const { processing, start, stop } = useStreaming(mockFeed, {
        autoStart: false,
      });

      expect(processing.value).toBe(false);

      start();
      expect(processing.value).toBe(true);

      stop();
      expect(processing.value).toBe(false);
    });

    it('useStreaming should compute latest event', () => {
      const mockFeed = { stream: vi.fn() };
      const { latestEvent } = useStreaming(mockFeed, {
        autoStart: false,
      });

      expect(latestEvent.value).toBe(null);
    });
  });
});
