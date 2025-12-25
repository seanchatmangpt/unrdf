/**
 * @fileoverview Tests for HDIT coordinate generation
 */

import { describe, it, expect } from 'vitest';
import {
  coordsForEvent,
  batchCoordsForEvents,
  createUniverseContext,
} from '../../src/hdit/coords.mjs';
import {
  D_BROWSER,
  D_LIGHT,
  ONTOLOGY_AXES,
} from '../../src/hdit/constants.mjs';

describe('coordsForEvent', () => {
  it('should generate Float32Array of correct dimension', () => {
    const event = {
      type: 'CREATE',
      timestamp: Date.now() * 1e6,
      vectorClock: { nodeId: 'node-1', counters: { 'node-1': '5' } },
      payload: { description: 'Test event' },
      mutations: [],
    };

    const coords = coordsForEvent(event, {}, D_BROWSER);

    expect(coords instanceof Float32Array).toBe(true);
    expect(coords.length).toBe(D_BROWSER);
  });

  it('should generate different coordinates for different event types', () => {
    const baseEvent = {
      timestamp: Date.now() * 1e6,
      vectorClock: { nodeId: 'node-1', counters: { 'node-1': '5' } },
      payload: {},
      mutations: [],
    };

    const createCoords = coordsForEvent({ ...baseEvent, type: 'CREATE' });
    const updateCoords = coordsForEvent({ ...baseEvent, type: 'UPDATE' });
    const deleteCoords = coordsForEvent({ ...baseEvent, type: 'DELETE' });

    // Event type axis should differ
    expect(createCoords[ONTOLOGY_AXES.EVENT_TYPE]).not.toBe(updateCoords[ONTOLOGY_AXES.EVENT_TYPE]);
    expect(updateCoords[ONTOLOGY_AXES.EVENT_TYPE]).not.toBe(deleteCoords[ONTOLOGY_AXES.EVENT_TYPE]);
  });

  it('should normalize time correctly with universe context', () => {
    const minTime = 1000000;
    const maxTime = 2000000;
    const midTime = 1500000;

    const universe = { minTime, maxTime };

    const earlyEvent = {
      type: 'CREATE',
      timestamp: minTime,
      vectorClock: { nodeId: 'node-1', counters: {} },
      payload: {},
    };

    const midEvent = { ...earlyEvent, timestamp: midTime };
    const lateEvent = { ...earlyEvent, timestamp: maxTime };

    const earlyCoords = coordsForEvent(earlyEvent, universe);
    const midCoords = coordsForEvent(midEvent, universe);
    const lateCoords = coordsForEvent(lateEvent, universe);

    // Time axis should be normalized [0, 1]
    expect(earlyCoords[ONTOLOGY_AXES.TIME] >= 0).toBe(true);
    expect(earlyCoords[ONTOLOGY_AXES.TIME] <= 1).toBe(true);
    expect(midCoords[ONTOLOGY_AXES.TIME] >= earlyCoords[ONTOLOGY_AXES.TIME]).toBe(true);
    expect(lateCoords[ONTOLOGY_AXES.TIME] >= midCoords[ONTOLOGY_AXES.TIME]).toBe(true);
  });

  it('should encode mutation count correctly', () => {
    const baseEvent = {
      type: 'UPDATE',
      timestamp: Date.now() * 1e6,
      vectorClock: { nodeId: 'node-1', counters: {} },
      payload: {},
    };

    const noMutations = coordsForEvent({ ...baseEvent, mutations: [] });
    const someMutations = coordsForEvent({
      ...baseEvent,
      mutations: Array(10).fill({ type: 'add' }),
    });

    // More mutations should have higher mutation count coordinate
    expect(someMutations[ONTOLOGY_AXES.MUTATION_COUNT] > noMutations[ONTOLOGY_AXES.MUTATION_COUNT]).toBe(true);
  });

  it('should encode causality depth from vector clock', () => {
    const lowCausality = {
      type: 'CREATE',
      timestamp: Date.now() * 1e6,
      vectorClock: { nodeId: 'node-1', counters: { 'node-1': '2' } },
      payload: {},
    };

    const highCausality = {
      ...lowCausality,
      vectorClock: {
        nodeId: 'node-1',
        counters: { 'node-1': '100', 'node-2': '50', 'node-3': '75' },
      },
    };

    const lowCoords = coordsForEvent(lowCausality);
    const highCoords = coordsForEvent(highCausality);

    // Higher causality depth should have higher coordinate
    expect(highCoords[ONTOLOGY_AXES.CAUSALITY_DEPTH] > lowCoords[ONTOLOGY_AXES.CAUSALITY_DEPTH]).toBe(true);
  });

  it('should handle minimal event structure', () => {
    const minimalEvent = {
      type: 'CREATE',
      timestamp: Date.now() * 1e6,
      vectorClock: { nodeId: 'node-1', counters: {} },
      payload: {},
    };

    const coords = coordsForEvent(minimalEvent);

    expect(coords instanceof Float32Array).toBe(true);
    expect(coords.length).toBe(D_BROWSER);

    // All coordinates should be finite
    for (let i = 0; i < coords.length; i++) {
      expect(Number.isFinite(coords[i])).toBe(true);
    }
  });

  it('should be deterministic for same input', () => {
    const event = {
      type: 'CREATE',
      timestamp: 1234567890,
      vectorClock: { nodeId: 'node-test', counters: { 'node-test': '42' } },
      payload: { description: 'Deterministic test' },
    };

    const coords1 = coordsForEvent(event);
    const coords2 = coordsForEvent(event);

    // Should produce identical coordinates
    for (let i = 0; i < coords1.length; i++) {
      expect(coords1[i]).toBe(coords2[i]);
    }
  });
});

describe('batchCoordsForEvents', () => {
  it('should generate coordinates for multiple events', () => {
    const events = [
      {
        type: 'CREATE',
        timestamp: 1000000,
        vectorClock: { nodeId: 'node-1', counters: {} },
        payload: {},
      },
      {
        type: 'UPDATE',
        timestamp: 2000000,
        vectorClock: { nodeId: 'node-2', counters: {} },
        payload: {},
      },
      {
        type: 'DELETE',
        timestamp: 3000000,
        vectorClock: { nodeId: 'node-3', counters: {} },
        payload: {},
      },
    ];

    const coordsArray = batchCoordsForEvents(events, {}, D_LIGHT);

    expect(coordsArray.length).toBe(3);
    coordsArray.forEach(coords => {
      expect(coords instanceof Float32Array).toBe(true);
      expect(coords.length).toBe(D_LIGHT);
    });
  });

  it('should auto-compute universe context', () => {
    const events = [
      {
        type: 'CREATE',
        timestamp: 1000000,
        vectorClock: { nodeId: 'node-1', counters: {} },
        payload: {},
      },
      {
        type: 'UPDATE',
        timestamp: 5000000,
        vectorClock: { nodeId: 'node-2', counters: {} },
        payload: {},
      },
    ];

    const coordsArray = batchCoordsForEvents(events);

    // First event should have low time coordinate, last should have high
    expect(coordsArray[0][ONTOLOGY_AXES.TIME] < 0.5).toBe(true);
    expect(coordsArray[1][ONTOLOGY_AXES.TIME] > 0.5).toBe(true);
  });
});

describe('createUniverseContext', () => {
  it('should compute min/max time from events', () => {
    const events = [
      {
        type: 'CREATE',
        timestamp: 1000000,
        vectorClock: { nodeId: 'node-1', counters: {} },
        payload: {},
      },
      {
        type: 'UPDATE',
        timestamp: 3000000,
        vectorClock: { nodeId: 'node-2', counters: {} },
        payload: {},
      },
      {
        type: 'DELETE',
        timestamp: 2000000,
        vectorClock: { nodeId: 'node-3', counters: {} },
        payload: {},
      },
    ];

    const context = createUniverseContext(events);

    expect(context.minTime).toBe(1000000);
    expect(context.maxTime).toBe(3000000);
  });

  it('should build entity frequency map', () => {
    const events = [
      {
        type: 'CREATE',
        timestamp: 1000000,
        vectorClock: { nodeId: 'node-1', counters: {} },
        payload: {},
        mutations: [
          { type: 'add', subject: 'alice', predicate: 'type', object: 'User' },
        ],
      },
      {
        type: 'UPDATE',
        timestamp: 2000000,
        vectorClock: { nodeId: 'node-2', counters: {} },
        payload: {},
        mutations: [
          { type: 'add', subject: 'alice', predicate: 'name', object: 'Alice' },
          { type: 'add', subject: 'bob', predicate: 'type', object: 'User' },
        ],
      },
    ];

    const context = createUniverseContext(events);

    expect(context.entityFrequency.get('alice')).toBe(2);
    expect(context.entityFrequency.get('bob')).toBe(1);
  });
});
