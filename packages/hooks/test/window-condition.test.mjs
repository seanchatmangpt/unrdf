/**
 * Window Condition Tests
 *
 * Tests sliding window functionality:
 * - Time-based windows
 * - Event-based windows
 * - Aggregations (count, sum, avg, min, max)
 * - Window sliding behavior
 * - Rate limiting scenarios
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { createStore } from '@unrdf/oxigraph';
import { evaluateCondition, SlidingWindow } from '../src/index.mjs';

describe('SlidingWindow Class', () => {
  it('creates a time-based window', () => {
    const window = new SlidingWindow(1000, 1000, true);
    expect(window.size).toBe(1000);
    expect(window.slideAmount).toBe(1000);
    expect(window.timeWindow).toBe(true);
    expect(window.length()).toBe(0);
  });

  it('creates an event-based window', () => {
    const window = new SlidingWindow(5, 5, false);
    expect(window.size).toBe(5);
    expect(window.timeWindow).toBe(false);
  });

  it('adds events to window', () => {
    const window = new SlidingWindow(10000, 10000, true);
    window.add(10);
    window.add(20);
    window.add(30);
    expect(window.length()).toBe(3);
  });

  it('retrieves window contents', () => {
    const window = new SlidingWindow(10000, 10000, true);
    window.add(10);
    window.add(20);
    const contents = window.getWindow();
    expect(contents).toHaveLength(2);
    expect(contents[0].value).toBe(10);
    expect(contents[1].value).toBe(20);
  });

  it('maintains event-based window of fixed size', () => {
    const window = new SlidingWindow(3, 3, false);
    window.add(10);
    window.add(20);
    window.add(30);
    window.add(40);
    window.add(50);

    // Should only keep last 3 events
    expect(window.length()).toBe(3);
    const contents = window.getWindow();
    expect(contents[0].value).toBe(30);
    expect(contents[1].value).toBe(40);
    expect(contents[2].value).toBe(50);
  });

  it('clears window state', () => {
    const window = new SlidingWindow(10000, 10000, true);
    window.add(10);
    window.add(20);
    expect(window.length()).toBe(2);

    window.clear();
    expect(window.length()).toBe(0);
  });
});

describe('Window Condition Evaluation', () => {
  let store;
  let options;

  beforeEach(() => {
    store = createStore();
    options = { windowState: new Map() };
  });

  it('evaluates count aggregation in window', async () => {
    // Add some data to store
    await store.load(
      `
      <http://example.org/tx1> <http://example.org/amount> 100 .
      <http://example.org/tx2> <http://example.org/amount> 200 .
      <http://example.org/tx3> <http://example.org/amount> 300 .
      `,
      { format: 'text/turtle' }
    );

    const condition = {
      id: 'test-count-window',
      kind: 'window',
      spec: {
        size: 5000, // 5 second window
        aggregate: 'count',
        query: 'SELECT ?amount WHERE { ?s <http://example.org/amount> ?amount }',
      },
    };

    const result = await evaluateCondition(condition, store, options);
    expect(result).toBe(true); // Should have content in window
  });

  it('evaluates sum aggregation in window', async () => {
    await store.load(
      `
      <http://example.org/tx1> <http://example.org/amount> "100"^^<http://www.w3.org/2001/XMLSchema#decimal> .
      <http://example.org/tx2> <http://example.org/amount> "200"^^<http://www.w3.org/2001/XMLSchema#decimal> .
      `,
      { format: 'text/turtle' }
    );

    const condition = {
      id: 'test-sum-window',
      kind: 'window',
      spec: {
        size: 5000,
        aggregate: 'sum',
        query: 'SELECT ?amount WHERE { ?s <http://example.org/amount> ?amount }',
      },
    };

    const result = await evaluateCondition(condition, store, options);
    expect(result).toBe(true);
  });

  it('evaluates average aggregation in window', async () => {
    await store.load(
      `
      <http://example.org/tx1> <http://example.org/value> "10"^^<http://www.w3.org/2001/XMLSchema#decimal> .
      <http://example.org/tx2> <http://example.org/value> "20"^^<http://www.w3.org/2001/XMLSchema#decimal> .
      <http://example.org/tx3> <http://example.org/value> "30"^^<http://www.w3.org/2001/XMLSchema#decimal> .
      `,
      { format: 'text/turtle' }
    );

    const condition = {
      id: 'test-avg-window',
      kind: 'window',
      spec: {
        size: 5000,
        aggregate: 'avg',
        query: 'SELECT ?value WHERE { ?s <http://example.org/value> ?value }',
      },
    };

    const result = await evaluateCondition(condition, store, options);
    expect(result).toBe(true);
  });

  it('evaluates min aggregation in window', async () => {
    await store.load(
      `
      <http://example.org/tx1> <http://example.org/score> "50"^^<http://www.w3.org/2001/XMLSchema#decimal> .
      <http://example.org/tx2> <http://example.org/score> "100"^^<http://www.w3.org/2001/XMLSchema#decimal> .
      `,
      { format: 'text/turtle' }
    );

    const condition = {
      id: 'test-min-window',
      kind: 'window',
      spec: {
        size: 5000,
        aggregate: 'min',
        query: 'SELECT ?score WHERE { ?s <http://example.org/score> ?score }',
      },
    };

    const result = await evaluateCondition(condition, store, options);
    expect(result).toBe(true);
  });

  it('evaluates max aggregation in window', async () => {
    await store.load(
      `
      <http://example.org/tx1> <http://example.org/value> "10"^^<http://www.w3.org/2001/XMLSchema#decimal> .
      <http://example.org/tx2> <http://example.org/value> "100"^^<http://www.w3.org/2001/XMLSchema#decimal> .
      `,
      { format: 'text/turtle' }
    );

    const condition = {
      id: 'test-max-window',
      kind: 'window',
      spec: {
        size: 5000,
        aggregate: 'max',
        query: 'SELECT ?value WHERE { ?s <http://example.org/value> ?value }',
      },
    };

    const result = await evaluateCondition(condition, store, options);
    expect(result).toBe(true);
  });

  it('returns false for empty window', async () => {
    const condition = {
      id: 'test-empty-window',
      kind: 'window',
      spec: {
        size: 5000,
        aggregate: 'count',
        query: 'SELECT ?x WHERE { ?x <http://example.org/nonexistent> ?y }',
      },
    };

    const result = await evaluateCondition(condition, store, options);
    expect(result).toBe(false);
  });

  it('handles maxMatches threshold for rate limiting', async () => {
    await store.load(
      `
      <http://example.org/tx1> <http://example.org/type> <http://example.org/Trade> .
      <http://example.org/tx2> <http://example.org/type> <http://example.org/Trade> .
      `,
      { format: 'text/turtle' }
    );

    const condition = {
      id: 'test-rate-limit',
      kind: 'window',
      spec: {
        size: 5000,
        aggregate: 'count',
        maxMatches: 5, // Allow up to 5 trades
        query: 'SELECT ?tx WHERE { ?tx <http://example.org/type> <http://example.org/Trade> }',
      },
    };

    const result = await evaluateCondition(condition, store, options);
    expect(result).toBe(true); // 2 trades <= 5 allowed
  });

  it('fails when exceeding maxMatches', async () => {
    await store.load(
      `
      <http://example.org/tx1> <http://example.org/type> <http://example.org/Trade> .
      <http://example.org/tx2> <http://example.org/type> <http://example.org/Trade> .
      <http://example.org/tx3> <http://example.org/type> <http://example.org/Trade> .
      `,
      { format: 'text/turtle' }
    );

    const condition = {
      id: 'test-rate-exceed',
      kind: 'window',
      spec: {
        size: 5000,
        aggregate: 'count',
        maxMatches: 2, // Only allow 2 trades
        query: 'SELECT ?tx WHERE { ?tx <http://example.org/type> <http://example.org/Trade> }',
      },
    };

    const result = await evaluateCondition(condition, store, options);
    expect(result).toBe(false); // 3 trades > 2 allowed
  });

  it('throws error when spec is missing', async () => {
    const condition = {
      id: 'test-no-spec',
      kind: 'window',
    };

    await expect(evaluateCondition(condition, store, options)).rejects.toThrow(
      'Window condition requires a spec property'
    );
  });

  it('throws error when size is invalid', async () => {
    const condition = {
      id: 'test-invalid-size',
      kind: 'window',
      spec: {
        size: 0,
        aggregate: 'count',
        query: 'SELECT ?x WHERE { ?x ?p ?o }',
      },
    };

    await expect(evaluateCondition(condition, store, options)).rejects.toThrow(
      'Window condition spec.size must be positive'
    );
  });

  it('throws error when query is missing', async () => {
    const condition = {
      id: 'test-no-query',
      kind: 'window',
      spec: {
        size: 5000,
        aggregate: 'count',
      },
    };

    await expect(evaluateCondition(condition, store, options)).rejects.toThrow(
      'Window condition requires a query property'
    );
  });

  it('handles non-numeric query results gracefully', async () => {
    await store.load(
      `
      <http://example.org/resource1> <http://example.org/name> "Alice" .
      <http://example.org/resource2> <http://example.org/name> "Bob" .
      `,
      { format: 'text/turtle' }
    );

    const condition = {
      id: 'test-non-numeric',
      kind: 'window',
      spec: {
        size: 5000,
        aggregate: 'count',
        query: 'SELECT ?name WHERE { ?s <http://example.org/name> ?name }',
      },
    };

    // Should not throw, should handle gracefully
    const result = await evaluateCondition(condition, store, options);
    expect(result).toBe(false); // Non-numeric values are filtered out
  });
});

describe('Window Condition - Sliding Behavior', () => {
  let store;
  let options;

  beforeEach(() => {
    store = createStore();
    options = { windowState: new Map() };
  });

  it('supports custom slide parameter for overlapping windows', async () => {
    await store.load(
      `
      <http://example.org/tx1> <http://example.org/amount> "100"^^<http://www.w3.org/2001/XMLSchema#decimal> .
      `,
      { format: 'text/turtle' }
    );

    const condition = {
      id: 'test-slide-param',
      kind: 'window',
      spec: {
        size: 5000,
        slide: 2500, // Slide every 2.5 seconds (50% overlap)
        aggregate: 'count',
        query: 'SELECT ?amount WHERE { ?s <http://example.org/amount> ?amount }',
      },
    };

    const result = await evaluateCondition(condition, store, options);
    expect(result).toBe(true);

    // Verify window state has the custom slide value
    const windowState = options.windowState.get('test-slide-param');
    expect(windowState.slideAmount).toBe(2500);
  });

  it('defaults slide to size for tumbling windows', async () => {
    await store.load(
      `
      <http://example.org/tx1> <http://example.org/amount> "100"^^<http://www.w3.org/2001/XMLSchema#decimal> .
      `,
      { format: 'text/turtle' }
    );

    const condition = {
      id: 'test-tumbling-window',
      kind: 'window',
      spec: {
        size: 5000,
        // No slide parameter - should default to size
        aggregate: 'count',
        query: 'SELECT ?amount WHERE { ?s <http://example.org/amount> ?amount }',
      },
    };

    await evaluateCondition(condition, store, options);

    const windowState = options.windowState.get('test-tumbling-window');
    expect(windowState.slideAmount).toBe(5000);
  });
});
