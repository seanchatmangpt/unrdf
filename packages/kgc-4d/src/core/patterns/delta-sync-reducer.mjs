/**
 * Delta Sync Reducer - Generic State Sync Pattern
 *
 * Framework-agnostic reducer for managing client-side state with delta operations.
 * Handles optimistic updates, pending deltas, vector clock tracking, and conflict resolution.
 *
 * Usage (React):
 * ```javascript
 * const reducer = createDeltaSyncReducer();
 * const [state, dispatch] = useReducer(reducer.reducer, reducer.initialState);
 * dispatch(reducer.actions.QUEUE_DELTA({ id: 'delta-1', operations: [...] }));
 * ```
 */

export const DeltaSyncState = {
  DISCONNECTED: 'disconnected',
  CONNECTING: 'connecting',
  CONNECTED: 'connected',
  SYNCING: 'syncing',
  ERROR: 'error',
};

export const DeltaSyncActions = {
  CONNECT: 'CONNECT',
  CONNECTED: 'CONNECTED',
  DISCONNECT: 'DISCONNECT',
  ERROR: 'ERROR',
  SET_SHARD: 'SET_SHARD',
  APPLY_DELTA: 'APPLY_DELTA',
  QUEUE_DELTA: 'QUEUE_DELTA',
  DELTA_ACK: 'DELTA_ACK',
  DELTA_REJECT: 'DELTA_REJECT',
  ADD_EVENT: 'ADD_EVENT',
};

/**
 * Create delta reducer and utilities
 * @param {Object} options
 * @param {Function} [options.applyDeltaToQuads] - Custom delta application logic
 * @returns {Object} { reducer, initialState, actions }
 */
export function createDeltaSyncReducer(options = {}) {
  const { applyDeltaToQuads = defaultApplyDeltaToQuads } = options;

  const initialState = {
    connection: DeltaSyncState.DISCONNECTED,
    shard: null,
    vectorClock: null,
    events: [],
    pendingDeltas: [],
    error: null,
    stats: null,
  };

  /**
   * Reducer function for state updates
   */
  function reducer(state, action) {
    switch (action.type) {
      case DeltaSyncActions.CONNECT:
        return { ...state, connection: DeltaSyncState.CONNECTING, error: null };

      case DeltaSyncActions.CONNECTED:
        return {
          ...state,
          connection: DeltaSyncState.CONNECTED,
          vectorClock: action.vectorClock,
        };

      case DeltaSyncActions.DISCONNECT:
        return { ...state, connection: DeltaSyncState.DISCONNECTED };

      case DeltaSyncActions.ERROR:
        return { ...state, connection: DeltaSyncState.ERROR, error: action.error };

      case DeltaSyncActions.SET_SHARD:
        return {
          ...state,
          shard: action.shard,
          vectorClock: action.shard.vector_clock,
          connection: DeltaSyncState.CONNECTED,
        };

      case DeltaSyncActions.APPLY_DELTA:
        // Apply delta to local shard (optimistic update)
        const updatedQuads = applyDeltaToQuads(state.shard?.quads || [], action.delta);
        return {
          ...state,
          shard: state.shard ? { ...state.shard, quads: updatedQuads } : null,
          connection: DeltaSyncState.SYNCING,
        };

      case DeltaSyncActions.QUEUE_DELTA:
        return {
          ...state,
          pendingDeltas: [...state.pendingDeltas, action.delta],
        };

      case DeltaSyncActions.DELTA_ACK:
        return {
          ...state,
          pendingDeltas: state.pendingDeltas.filter((d) => d.id !== action.deltaId),
          vectorClock: action.vectorClock,
          connection: DeltaSyncState.CONNECTED,
        };

      case DeltaSyncActions.DELTA_REJECT:
        // On reject, remove pending delta and mark error
        return {
          ...state,
          pendingDeltas: state.pendingDeltas.filter((d) => d.id !== action.deltaId),
          error: action.reason,
          connection: DeltaSyncState.ERROR,
        };

      case DeltaSyncActions.ADD_EVENT:
        return {
          ...state,
          events: [action.event, ...state.events].slice(0, 100), // Keep last 100
        };

      default:
        return state;
    }
  }

  /**
   * Create action creators
   */
  const actions = {
    connect: () => ({ type: DeltaSyncActions.CONNECT }),
    connected: (vectorClock) => ({ type: DeltaSyncActions.CONNECTED, vectorClock }),
    disconnect: () => ({ type: DeltaSyncActions.DISCONNECT }),
    error: (error) => ({ type: DeltaSyncActions.ERROR, error }),
    setShard: (shard) => ({ type: DeltaSyncActions.SET_SHARD, shard }),
    applyDelta: (delta) => ({ type: DeltaSyncActions.APPLY_DELTA, delta }),
    queueDelta: (delta) => ({ type: DeltaSyncActions.QUEUE_DELTA, delta }),
    deltaAck: (deltaId, vectorClock) => ({
      type: DeltaSyncActions.DELTA_ACK,
      deltaId,
      vectorClock,
    }),
    deltaReject: (deltaId, reason) => ({
      type: DeltaSyncActions.DELTA_REJECT,
      deltaId,
      reason,
    }),
    addEvent: (event) => ({ type: DeltaSyncActions.ADD_EVENT, event }),
  };

  return {
    reducer,
    initialState,
    actions,
    DeltaSyncState,
    DeltaSyncActions,
  };
}

/**
 * Default delta application logic (RDF quads)
 * @param {Array} quads - Current quad array
 * @param {Object} delta - Delta with operations
 * @returns {Array} Updated quads
 */
function defaultApplyDeltaToQuads(quads, delta) {
  let result = [...quads];

  for (const op of delta.operations || []) {
    if (op.type === 'add') {
      // Add new quad
      result.push({
        subject: op.subject,
        predicate: op.predicate,
        object: op.object,
        graph: op.graph || {
          value: 'http://kgc.io/Universe',
          termType: 'NamedNode',
        },
      });
    } else if (op.type === 'delete') {
      // Remove matching quad
      result = result.filter(
        (q) =>
          !(
            q.subject.value === op.subject.value &&
            q.predicate.value === op.predicate.value &&
            q.object.value === op.object.value
          )
      );
    }
  }

  return result;
}
