import { describe, test, expect } from 'vitest';
import {
  createDeltaSyncReducer,
  DeltaSyncState,
  DeltaSyncActions,
} from '../../src/core/patterns/delta-sync-reducer.mjs';

describe('DeltaSyncReducer - Pattern Tests', () => {
  test('creates reducer with initial state', () => {
    const { reducer, initialState } = createDeltaSyncReducer();

    expect(initialState.connection).toBe(DeltaSyncState.DISCONNECTED);
    expect(initialState.shard).toBeNull();
    expect(initialState.vectorClock).toBeNull();
    expect(initialState.pendingDeltas).toEqual([]);
    expect(initialState.error).toBeNull();
  });

  test('handles CONNECT action', () => {
    const { reducer, initialState, actions } = createDeltaSyncReducer();

    const newState = reducer(initialState, actions.connect());

    expect(newState.connection).toBe(DeltaSyncState.CONNECTING);
    expect(newState.error).toBeNull();
  });

  test('handles CONNECTED action', () => {
    const { reducer, initialState, actions } = createDeltaSyncReducer();

    const vectorClock = { node1: 5, node2: 3 };
    const newState = reducer(initialState, actions.connected(vectorClock));

    expect(newState.connection).toBe(DeltaSyncState.CONNECTED);
    expect(newState.vectorClock).toEqual(vectorClock);
  });

  test('handles DISCONNECT action', () => {
    const { reducer, initialState, actions } = createDeltaSyncReducer();
    const connectedState = { ...initialState, connection: DeltaSyncState.CONNECTED };

    const newState = reducer(connectedState, actions.disconnect());

    expect(newState.connection).toBe(DeltaSyncState.DISCONNECTED);
  });

  test('handles SET_SHARD action', () => {
    const { reducer, initialState, actions } = createDeltaSyncReducer();

    const shard = {
      id: 'shard-1',
      quads: [{ subject: 'A', predicate: 'B', object: 'C' }],
      vector_clock: { node: 1 },
    };

    const newState = reducer(initialState, actions.setShard(shard));

    expect(newState.shard).toEqual(shard);
    expect(newState.vectorClock).toEqual(shard.vector_clock);
    expect(newState.connection).toBe(DeltaSyncState.CONNECTED);
  });

  test('handles APPLY_DELTA action', () => {
    const { reducer, initialState, actions } = createDeltaSyncReducer();

    const shard = {
      id: 'shard-1',
      quads: [{ subject: 'A', predicate: 'label', object: 'A-value' }],
    };
    const connectedState = { ...initialState, shard, connection: DeltaSyncState.CONNECTED };

    const delta = {
      id: 'delta-1',
      operations: [
        {
          type: 'add',
          subject: { value: 'B' },
          predicate: { value: 'label' },
          object: { value: 'B-value' },
        },
      ],
    };

    const newState = reducer(connectedState, actions.applyDelta(delta));

    expect(newState.connection).toBe(DeltaSyncState.SYNCING);
    expect(newState.shard.quads.length).toBe(2);
    expect(newState.shard.quads[1].subject.value).toBe('B');
  });

  test('handles QUEUE_DELTA action', () => {
    const { reducer, initialState, actions } = createDeltaSyncReducer();

    const delta1 = { id: 'delta-1' };
    const delta2 = { id: 'delta-2' };

    let state = reducer(initialState, actions.queueDelta(delta1));
    state = reducer(state, actions.queueDelta(delta2));

    expect(state.pendingDeltas).toHaveLength(2);
    expect(state.pendingDeltas[0]).toEqual(delta1);
    expect(state.pendingDeltas[1]).toEqual(delta2);
  });

  test('handles DELTA_ACK action', () => {
    const { reducer, initialState, actions } = createDeltaSyncReducer();

    const delta = { id: 'delta-1' };
    let state = reducer(initialState, actions.queueDelta(delta));

    const vectorClock = { node: 2 };
    state = reducer(state, actions.deltaAck('delta-1', vectorClock));

    expect(state.pendingDeltas).toHaveLength(0);
    expect(state.vectorClock).toEqual(vectorClock);
    expect(state.connection).toBe(DeltaSyncState.CONNECTED);
  });

  test('handles DELTA_REJECT action', () => {
    const { reducer, initialState, actions } = createDeltaSyncReducer();

    const delta = { id: 'delta-1' };
    let state = reducer(initialState, actions.queueDelta(delta));

    state = reducer(state, actions.deltaReject('delta-1', 'Validation failed'));

    expect(state.pendingDeltas).toHaveLength(0);
    expect(state.error).toBe('Validation failed');
    expect(state.connection).toBe(DeltaSyncState.ERROR);
  });

  test('handles ADD_EVENT action', () => {
    const { reducer, initialState, actions } = createDeltaSyncReducer();

    const event1 = { type: 'CONNECTED', timestamp: Date.now() };
    const event2 = { type: 'DELTA', deltaId: '1' };

    let state = reducer(initialState, actions.addEvent(event1));
    state = reducer(state, actions.addEvent(event2));

    expect(state.events).toHaveLength(2);
    expect(state.events[0]).toEqual(event2); // Most recent first
    expect(state.events[1]).toEqual(event1);
  });

  test('keeps only last 100 events', () => {
    const { reducer, initialState, actions } = createDeltaSyncReducer();

    let state = initialState;

    // Add 150 events
    for (let i = 0; i < 150; i++) {
      state = reducer(state, actions.addEvent({ type: 'EVENT', id: i }));
    }

    expect(state.events.length).toBe(100);
    expect(state.events[0].id).toBe(149); // Most recent
    expect(state.events[99].id).toBe(50); // Oldest (50-149 kept)
  });

  test('handles ERROR action', () => {
    const { reducer, initialState, actions } = createDeltaSyncReducer();

    const newState = reducer(initialState, actions.error('Connection lost'));

    expect(newState.connection).toBe(DeltaSyncState.ERROR);
    expect(newState.error).toBe('Connection lost');
  });

  test('applies custom delta logic', () => {
    const customApplyDelta = (quads, delta) => {
      // Custom logic: always replace, not merge
      return delta.operations || [];
    };

    const { reducer, initialState, actions } = createDeltaSyncReducer({
      applyDeltaToQuads: customApplyDelta,
    });

    const shard = { quads: [{ id: 1 }, { id: 2 }] };
    const state = { ...initialState, shard };

    const delta = { operations: [{ id: 3 }, { id: 4 }] };
    const newState = reducer(state, actions.applyDelta(delta));

    // Custom logic replaced instead of merged
    expect(newState.shard.quads).toEqual([{ id: 3 }, { id: 4 }]);
  });

  test('returns valid action creators from createDeltaSyncReducer', () => {
    const { actions } = createDeltaSyncReducer();

    expect(typeof actions.connect).toBe('function');
    expect(typeof actions.connected).toBe('function');
    expect(typeof actions.setShard).toBe('function');
    expect(typeof actions.applyDelta).toBe('function');
    expect(typeof actions.queueDelta).toBe('function');
    expect(typeof actions.deltaAck).toBe('function');
    expect(typeof actions.deltaReject).toBe('function');
  });
});
