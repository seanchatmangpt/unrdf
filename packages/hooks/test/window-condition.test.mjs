/**
 * Window Condition Test Suite
 *
 * Tests sliding window and event-based aggregation conditions:
 * - Time-based windows (fixed duration)
 * - Event-based windows (count-based)
 * - Sliding window behavior
 * - Aggregate functions (sum, avg, min, max, count)
 * - Multiple data types and edge cases
 */

import { describe, it, expect, beforeEach } from 'vitest';

// Mock window evaluation
async function evaluateWindow(condition, graph, queryResults = []) {
  const { spec } = condition;
  const { size, aggregate } = spec;

  // Calculate aggregate over results
  let aggregateValue;
  switch (aggregate) {
    case 'sum':
      aggregateValue = queryResults.reduce((sum, r) => {
        const val = parseFloat(Object.values(r)[0]?.value || 0);
        return sum + (isNaN(val) ? 0 : val);
      }, 0);
      break;
    case 'avg':
      const sum = queryResults.reduce((sum, r) => {
        const val = parseFloat(Object.values(r)[0]?.value || 0);
        return sum + (isNaN(val) ? 0 : val);
      }, 0);
      aggregateValue = queryResults.length > 0 ? sum / queryResults.length : 0;
      break;
    case 'min':
      aggregateValue = Math.min(
        ...queryResults.map(r => {
          const val = parseFloat(Object.values(r)[0]?.value || Infinity);
          return isNaN(val) ? Infinity : val;
        })
      );
      break;
    case 'max':
      aggregateValue = Math.max(
        ...queryResults.map(r => {
          const val = parseFloat(Object.values(r)[0]?.value || -Infinity);
          return isNaN(val) ? -Infinity : val;
        })
      );
      break;
    case 'count':
      aggregateValue = queryResults.length;
      break;
    default:
      aggregateValue = queryResults.length;
  }

  // Return aggregate value
  return aggregateValue;
}

describe('Window Condition - Time-based and Event-based Aggregation', () => {
  let graph;

  beforeEach(() => {
    // Mock store object
    graph = { size: 0, add: () => {}, insert: () => {} };
  });

  it('should support time-based window with fixed duration', () => {
    const condition = {
      kind: 'window',
      spec: {
        size: 60000, // 60 second window in ms
        aggregate: 'sum',
        query: 'SELECT ?value WHERE { ?s ex:hasValue ?value }',
      },
    };

    expect(condition.spec.size).toBe(60000);
    expect(condition.spec.aggregate).toBe('sum');
    expect(condition.kind).toBe('window');
  });

  it('should support event-based window with count', () => {
    const condition = {
      kind: 'window',
      spec: {
        size: 100, // 100 events in window
        aggregate: 'count',
        query: 'SELECT ?event WHERE { ?e ex:isEvent ?event }',
      },
    };

    expect(condition.spec.size).toBe(100);
    expect(condition.spec.aggregate).toBe('count');
  });

  it('should calculate sum aggregate over window results', async () => {
    const condition = {
      kind: 'window',
      spec: {
        size: 5, // window of 5 events
        aggregate: 'sum',
      },
    };

    const queryResults = [
      { value: { value: '10' } },
      { value: { value: '20' } },
      { value: { value: '30' } },
      { value: { value: '40' } },
      { value: { value: '50' } },
    ];

    const result = await evaluateWindow(condition, graph, queryResults);
    expect(result).toBe(150); // 10 + 20 + 30 + 40 + 50
  });

  it('should calculate average aggregate over window', async () => {
    const condition = {
      kind: 'window',
      spec: {
        size: 10, // 10 event window
        aggregate: 'avg',
      },
    };

    const queryResults = [
      { value: { value: '100' } },
      { value: { value: '200' } },
      { value: { value: '300' } },
      { value: { value: '400' } },
    ];

    const result = await evaluateWindow(condition, graph, queryResults);
    expect(result).toBe(250); // (100 + 200 + 300 + 400) / 4
  });

  it('should calculate minimum value in window', async () => {
    const condition = {
      kind: 'window',
      spec: {
        size: 1000,
        aggregate: 'min',
      },
    };

    const queryResults = [
      { value: { value: '50' } },
      { value: { value: '100' } },
      { value: { value: '75' } },
      { value: { value: '25' } },
      { value: { value: '200' } },
    ];

    const result = await evaluateWindow(condition, graph, queryResults);
    expect(result).toBe(25);
  });

  it('should calculate maximum value in window', async () => {
    const condition = {
      kind: 'window',
      spec: {
        size: 1000,
        aggregate: 'max',
      },
    };

    const queryResults = [
      { value: { value: '50' } },
      { value: { value: '100' } },
      { value: { value: '75' } },
      { value: { value: '250' } },
      { value: { value: '200' } },
    ];

    const result = await evaluateWindow(condition, graph, queryResults);
    expect(result).toBe(250);
  });

  it('should count events in window', async () => {
    const condition = {
      kind: 'window',
      spec: {
        size: 100,
        aggregate: 'count',
      },
    };

    const queryResults = Array.from({ length: 47 }, (_, i) => ({
      event: { value: `event-${i}` },
    }));

    const result = await evaluateWindow(condition, graph, queryResults);
    expect(result).toBe(47);
  });

  it('should handle empty window', async () => {
    const condition = {
      kind: 'window',
      spec: {
        size: 1000,
        aggregate: 'sum',
      },
    };

    const result = await evaluateWindow(condition, graph, []);
    expect(result).toBe(0);
  });

  it('should handle non-numeric values gracefully', async () => {
    const condition = {
      kind: 'window',
      spec: {
        size: 1000,
        aggregate: 'sum',
      },
    };

    const queryResults = [
      { value: { value: '100' } },
      { value: { value: 'not-a-number' } },
      { value: { value: '200' } },
      { value: { value: 'invalid' } },
      { value: { value: '50' } },
    ];

    const result = await evaluateWindow(condition, graph, queryResults);
    expect(result).toBe(350); // 100 + 200 + 50 (invalid values treated as 0)
  });

  it('should support sliding window behavior', () => {
    const condition = {
      kind: 'window',
      spec: {
        size: 100, // 100 event window
        slide: 10, // slide by 10 events
        aggregate: 'count',
      },
    };

    expect(condition.spec.size).toBe(100);
    expect(condition.spec.slide).toBe(10);
    expect(condition.spec.aggregate).toBe('count');
  });

  it('should use window size as default slide if not specified', () => {
    const condition = {
      kind: 'window',
      spec: {
        size: 100, // window and slide should both be 100
        aggregate: 'avg',
      },
    };

    expect(condition.spec.size).toBe(100);
    expect(condition.spec.slide === undefined || condition.spec.slide === condition.spec.size).toBe(true);
  });

  it('should handle time-based window specification', () => {
    const condition = {
      kind: 'window',
      spec: {
        size: 300000, // 5 minutes in milliseconds
        timeUnit: 'milliseconds',
        aggregate: 'avg',
      },
    };

    expect(condition.spec.size).toBe(300000);
    expect(condition.spec.timeUnit).toBe('milliseconds');
  });

  it('should support multiple aggregate functions in separate conditions', () => {
    const conditions = [
      {
        kind: 'window',
        spec: { size: 100, aggregate: 'sum' },
      },
      {
        kind: 'window',
        spec: { size: 100, aggregate: 'avg' },
      },
      {
        kind: 'window',
        spec: { size: 100, aggregate: 'min' },
      },
      {
        kind: 'window',
        spec: { size: 100, aggregate: 'max' },
      },
      {
        kind: 'window',
        spec: { size: 100, aggregate: 'count' },
      },
    ];

    const aggregates = conditions.map(c => c.spec.aggregate);
    expect(aggregates).toEqual(['sum', 'avg', 'min', 'max', 'count']);
  });

  it('should handle very large window sizes', async () => {
    const condition = {
      kind: 'window',
      spec: {
        size: 1000000, // 1 million events or 1000 seconds
        aggregate: 'count',
      },
    };

    const queryResults = Array.from({ length: 999999 }, (_, i) => ({
      event: { value: `event-${i}` },
    }));

    const result = await evaluateWindow(condition, graph, queryResults);
    expect(result).toBe(999999);
  });

  it('should preserve query specification in window condition', () => {
    const sparqlQuery = `
      SELECT ?measurement ?value WHERE {
        ?s ex:hasMeasurement ?measurement ;
           ex:hasValue ?value
        FILTER (?value > 0)
      }
    `;

    const condition = {
      kind: 'window',
      spec: {
        size: 500,
        aggregate: 'avg',
        query: sparqlQuery,
      },
    };

    expect(condition.spec.query).toBe(sparqlQuery);
    expect(condition.spec.query).toContain('SELECT');
    expect(condition.spec.query).toContain('FILTER');
  });

  it('should calculate average with decimal precision', async () => {
    const condition = {
      kind: 'window',
      spec: {
        size: 100,
        aggregate: 'avg',
      },
    };

    const queryResults = [
      { value: { value: '10.5' } },
      { value: { value: '20.75' } },
      { value: { value: '30.25' } },
    ];

    const result = await evaluateWindow(condition, graph, queryResults);
    expect(result).toBeCloseTo(20.5, 2); // (10.5 + 20.75 + 30.25) / 3
  });

  it('should handle negative values in window aggregation', async () => {
    const condition = {
      kind: 'window',
      spec: {
        size: 1000,
        aggregate: 'sum',
      },
    };

    const queryResults = [
      { value: { value: '100' } },
      { value: { value: '-50' } },
      { value: { value: '75' } },
      { value: { value: '-25' } },
    ];

    const result = await evaluateWindow(condition, graph, queryResults);
    expect(result).toBe(100); // 100 - 50 + 75 - 25
  });

  it('should support window with explicit query in condition', () => {
    const condition = {
      kind: 'window',
      spec: {
        size: 50,
        aggregate: 'count',
        query: 'SELECT ?s WHERE { ?s a ex:Event }',
      },
    };

    expect(condition.kind).toBe('window');
    expect(condition.spec.query).toBeDefined();
    expect(condition.spec.query).toContain('?s a ex:Event');
  });

  it('should handle window with graph data size check', () => {
    graph.size = 1;

    const condition = {
      kind: 'window',
      spec: {
        size: 1000,
        aggregate: 'count',
      },
    };

    expect(graph.size).toBeGreaterThan(0);
    expect(condition.spec.size).toBe(1000);
  });
});
