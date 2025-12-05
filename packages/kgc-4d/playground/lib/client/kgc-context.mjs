'use client';

/**
 * KGC-4D Client Context - Connection and State Management
 *
 * Provides:
 * - Connection state (connected/disconnected/syncing)
 * - Shard cache (current view of Universe)
 * - Delta submission with optimistic updates
 * - Real-time subscription via SSE
 */

import { createContext, useContext, useReducer, useCallback, useEffect, useRef } from 'react';

// Connection states
export const ConnectionState = {
  DISCONNECTED: 'disconnected',
  CONNECTING: 'connecting',
  CONNECTED: 'connected',
  SYNCING: 'syncing',
  ERROR: 'error',
};

// Initial state
const initialState = {
  connection: ConnectionState.DISCONNECTED,
  shard: null,
  vectorClock: null,
  events: [],
  pendingDeltas: [],
  error: null,
  stats: null,
};

// Action types
const Actions = {
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
  SET_STATS: 'SET_STATS',
};

// Reducer
function kgcReducer(state, action) {
  switch (action.type) {
    case Actions.CONNECT:
      return { ...state, connection: ConnectionState.CONNECTING, error: null };

    case Actions.CONNECTED:
      return {
        ...state,
        connection: ConnectionState.CONNECTED,
        vectorClock: action.vectorClock,
      };

    case Actions.DISCONNECT:
      return { ...state, connection: ConnectionState.DISCONNECTED };

    case Actions.ERROR:
      return { ...state, connection: ConnectionState.ERROR, error: action.error };

    case Actions.SET_SHARD:
      return {
        ...state,
        shard: action.shard,
        vectorClock: action.shard.vector_clock,
        connection: ConnectionState.CONNECTED,
      };

    case Actions.APPLY_DELTA:
      // Apply delta to local Shard (optimistic update)
      const updatedQuads = applyDeltaToQuads(state.shard?.quads || [], action.delta);
      return {
        ...state,
        shard: state.shard ? { ...state.shard, quads: updatedQuads } : null,
        connection: ConnectionState.SYNCING,
      };

    case Actions.QUEUE_DELTA:
      return {
        ...state,
        pendingDeltas: [...state.pendingDeltas, action.delta],
      };

    case Actions.DELTA_ACK:
      return {
        ...state,
        pendingDeltas: state.pendingDeltas.filter((d) => d.id !== action.deltaId),
        vectorClock: action.vectorClock,
        connection: ConnectionState.CONNECTED,
      };

    case Actions.DELTA_REJECT:
      // Rollback: remove pending delta and potentially refresh Shard
      return {
        ...state,
        pendingDeltas: state.pendingDeltas.filter((d) => d.id !== action.deltaId),
        error: action.reason,
        connection: ConnectionState.ERROR,
      };

    case Actions.ADD_EVENT:
      return {
        ...state,
        events: [action.event, ...state.events].slice(0, 100), // Keep last 100 events
      };

    case Actions.SET_STATS:
      return { ...state, stats: action.stats };

    default:
      return state;
  }
}

// Helper: Apply delta operations to quads array
function applyDeltaToQuads(quads, delta) {
  let result = [...quads];

  for (const op of delta.operations || []) {
    if (op.type === 'add') {
      // Add new quad
      result.push({
        subject: op.subject,
        predicate: op.predicate,
        object: op.object,
        graph: op.graph || { value: 'http://kgc.io/Universe', termType: 'NamedNode' },
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

// Context
const KGCContext = createContext(null);

/**
 * KGC Provider Component
 */
export function KGCProvider({ children, autoConnect = false, query = {} }) {
  const [state, dispatch] = useReducer(kgcReducer, initialState);
  const eventSourceRef = useRef(null);
  const reconnectTimeoutRef = useRef(null);

  // Connect to Tether (SSE)
  const connect = useCallback(
    (subscriptionQuery = query) => {
      dispatch({ type: Actions.CONNECT });

      // Build SSE URL with query parameters
      const params = new URLSearchParams();
      if (subscriptionQuery.subject) params.set('subject', subscriptionQuery.subject);
      if (subscriptionQuery.predicate) params.set('predicate', subscriptionQuery.predicate);
      if (subscriptionQuery.type) params.set('type', subscriptionQuery.type);
      if (subscriptionQuery.belongsTo) params.set('belongsTo', subscriptionQuery.belongsTo);

      const url = `/api/tether?${params.toString()}`;

      // Close existing connection
      if (eventSourceRef.current) {
        eventSourceRef.current.close();
      }

      // Create new EventSource
      const eventSource = new EventSource(url);
      eventSourceRef.current = eventSource;

      eventSource.addEventListener('connected', (e) => {
        const data = JSON.parse(e.data);
        dispatch({ type: Actions.CONNECTED, vectorClock: data.vector_clock });
        dispatch({ type: Actions.ADD_EVENT, event: { type: 'CONNECTED', ...data } });
      });

      eventSource.addEventListener('shard', (e) => {
        const data = JSON.parse(e.data);
        dispatch({ type: Actions.SET_SHARD, shard: data });
        dispatch({ type: Actions.ADD_EVENT, event: { type: 'SHARD', quad_count: data.quad_count } });
      });

      eventSource.addEventListener('delta', (e) => {
        const data = JSON.parse(e.data);
        dispatch({ type: Actions.APPLY_DELTA, delta: data.delta });
        dispatch({ type: Actions.ADD_EVENT, event: { type: 'DELTA', delta: data.delta } });
      });

      eventSource.addEventListener('heartbeat', (e) => {
        const data = JSON.parse(e.data);
        dispatch({ type: Actions.ADD_EVENT, event: { type: 'HEARTBEAT', ...data } });
      });

      eventSource.addEventListener('error', (e) => {
        console.error('[KGC] SSE error:', e);
        dispatch({ type: Actions.ERROR, error: 'Connection lost' });

        // Auto-reconnect after 5 seconds
        reconnectTimeoutRef.current = setTimeout(() => {
          connect(subscriptionQuery);
        }, 5000);
      });

      eventSource.onerror = () => {
        dispatch({ type: Actions.ERROR, error: 'Connection failed' });
      };
    },
    [query]
  );

  // Disconnect from Tether
  const disconnect = useCallback(() => {
    if (eventSourceRef.current) {
      eventSourceRef.current.close();
      eventSourceRef.current = null;
    }
    if (reconnectTimeoutRef.current) {
      clearTimeout(reconnectTimeoutRef.current);
    }
    dispatch({ type: Actions.DISCONNECT });
  }, []);

  // Submit delta with optimistic update
  const submitDelta = useCallback(
    async (operations, options = {}) => {
      const deltaId = crypto.randomUUID();
      const delta = {
        id: deltaId,
        operations,
        client_vector_clock: state.vectorClock,
        source: 'browser',
      };

      // Optimistic update
      dispatch({ type: Actions.QUEUE_DELTA, delta });
      dispatch({ type: Actions.APPLY_DELTA, delta });

      try {
        const response = await fetch('/api/delta', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify(delta),
        });

        const result = await response.json();

        if (result.status === 'ACK') {
          dispatch({
            type: Actions.DELTA_ACK,
            deltaId,
            vectorClock: result.vector_clock,
          });
          dispatch({ type: Actions.ADD_EVENT, event: { type: 'ACK', ...result } });
          return { success: true, result };
        } else {
          dispatch({
            type: Actions.DELTA_REJECT,
            deltaId,
            reason: result.reason,
          });
          dispatch({ type: Actions.ADD_EVENT, event: { type: 'REJECT', ...result } });

          // Refresh Shard to rollback
          await refreshShard();
          return { success: false, error: result.reason };
        }
      } catch (error) {
        dispatch({
          type: Actions.DELTA_REJECT,
          deltaId,
          reason: error.message,
        });
        return { success: false, error: error.message };
      }
    },
    [state.vectorClock]
  );

  // Refresh Shard from server
  const refreshShard = useCallback(async () => {
    try {
      const params = new URLSearchParams();
      if (query.subject) params.set('subject', query.subject);
      if (query.type) params.set('type', query.type);
      if (query.belongsTo) params.set('belongsTo', query.belongsTo);

      const response = await fetch(`/api/shard?${params.toString()}`);
      const data = await response.json();

      if (data.success) {
        dispatch({ type: Actions.SET_SHARD, shard: data.shard });
      }
    } catch (error) {
      console.error('[KGC] Refresh error:', error);
    }
  }, [query]);

  // Fetch Universe stats
  const fetchStats = useCallback(async () => {
    try {
      const response = await fetch('/api/shard?stats=true');
      const stats = await response.json();
      dispatch({ type: Actions.SET_STATS, stats });
      return stats;
    } catch (error) {
      console.error('[KGC] Stats error:', error);
      return null;
    }
  }, []);

  // Auto-connect on mount
  useEffect(() => {
    if (autoConnect) {
      connect();
    }

    return () => {
      disconnect();
    };
  }, [autoConnect, connect, disconnect]);

  const value = {
    // State
    ...state,

    // Actions
    connect,
    disconnect,
    submitDelta,
    refreshShard,
    fetchStats,

    // Helpers
    isConnected: state.connection === ConnectionState.CONNECTED,
    isSyncing: state.connection === ConnectionState.SYNCING,
    hasError: state.connection === ConnectionState.ERROR,
  };

  return <KGCContext.Provider value={value}>{children}</KGCContext.Provider>;
}

/**
 * Hook to access KGC context
 */
export function useKGC() {
  const context = useContext(KGCContext);
  if (!context) {
    throw new Error('useKGC must be used within a KGCProvider');
  }
  return context;
}

export { KGCContext };
