'use client';

/**
 * KGC-4D Client Context - Connection and State Management
 *
 * Provides:
 * - Connection state (connected/disconnected/syncing)
 * - Shard cache (current view of Universe)
 * - Delta submission with optimistic updates
 * - Real-time subscription via SSE
 *
 * Uses DeltaSyncReducer pattern from @unrdf/kgc-4d for reusable state management
 */

import { createContext, useContext, useReducer, useCallback, useEffect, useRef } from 'react';
import {
  createDeltaSyncReducer,
  DeltaSyncState,
} from '@unrdf/kgc-4d';

// Re-export for convenience
export const ConnectionState = DeltaSyncState;

// Initialize reducer pattern
const { reducer: kgcReducer, initialState, actions: createActions } = createDeltaSyncReducer();

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
      dispatch(createActions.connect());

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
        dispatch(createActions.connected(data.vector_clock));
        dispatch(createActions.addEvent({ type: 'CONNECTED', ...data }));
      });

      eventSource.addEventListener('shard', (e) => {
        const data = JSON.parse(e.data);
        dispatch(createActions.setShard(data));
        dispatch(createActions.addEvent({ type: 'SHARD', quad_count: data.quad_count }));
      });

      eventSource.addEventListener('delta', (e) => {
        const data = JSON.parse(e.data);
        dispatch(createActions.applyDelta(data.delta));
        dispatch(createActions.addEvent({ type: 'DELTA', delta: data.delta }));
      });

      eventSource.addEventListener('heartbeat', (e) => {
        const data = JSON.parse(e.data);
        dispatch(createActions.addEvent({ type: 'HEARTBEAT', ...data }));
      });

      eventSource.addEventListener('error', (e) => {
        console.error('[KGC] SSE error:', e);
        dispatch(createActions.error('Connection lost'));

        // Auto-reconnect after 5 seconds
        reconnectTimeoutRef.current = setTimeout(() => {
          connect(subscriptionQuery);
        }, 5000);
      });

      eventSource.onerror = () => {
        dispatch(createActions.error('Connection failed'));
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
    dispatch(createActions.disconnect());
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
      dispatch(createActions.queueDelta(delta));
      dispatch(createActions.applyDelta(delta));

      try {
        const response = await fetch('/api/delta', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify(delta),
        });

        const result = await response.json();

        if (result.status === 'ACK') {
          dispatch(createActions.deltaAck(deltaId, result.vector_clock));
          dispatch(createActions.addEvent({ type: 'ACK', ...result }));
          return { success: true, result };
        } else {
          dispatch(createActions.deltaReject(deltaId, result.reason));
          dispatch(createActions.addEvent({ type: 'REJECT', ...result }));

          // Refresh Shard to rollback
          await refreshShard();
          return { success: false, error: result.reason };
        }
      } catch (error) {
        dispatch(createActions.deltaReject(deltaId, error.message));
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
        dispatch(createActions.setShard(data.shard));
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
      // Stats are read-only, no dispatch needed
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
