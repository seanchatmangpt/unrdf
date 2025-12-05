'use client';

/**
 * KGC-4D Client Hooks
 *
 * React hooks for working with Shards and Deltas
 */

import { useState, useCallback, useMemo, useEffect } from 'react';
import { useKGC } from './kgc-context.mjs';

/**
 * useShard - Subscribe to a filtered view of the Universe
 *
 * @param {Object} options
 * @param {string} [options.subject] - Filter by subject
 * @param {string} [options.type] - Filter by rdf:type
 * @param {string} [options.belongsTo] - Filter by belongsTo
 * @returns {Object} { quads, loading, error, refresh }
 */
export function useShard(options = {}) {
  const { shard, connection, error, refreshShard } = useKGC();

  const quads = useMemo(() => {
    if (!shard?.quads) return [];

    let filtered = shard.quads;

    // Client-side filtering for more specific queries
    if (options.subject) {
      filtered = filtered.filter((q) => q.subject.value === options.subject);
    }

    if (options.type) {
      // Find subjects of the requested type
      const typeQuads = shard.quads.filter(
        (q) =>
          q.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' &&
          q.object.value === options.type
      );
      const typedSubjects = new Set(typeQuads.map((q) => q.subject.value));
      filtered = filtered.filter((q) => typedSubjects.has(q.subject.value));
    }

    return filtered;
  }, [shard?.quads, options.subject, options.type]);

  return {
    quads,
    loading: connection === 'connecting' || connection === 'syncing',
    error,
    refresh: refreshShard,
    metadata: shard
      ? {
          id: shard.id,
          t_ns: shard.t_ns,
          timestamp: shard.timestamp_iso,
          total_quads: shard.quad_count,
        }
      : null,
  };
}

/**
 * useEntity - Get all properties of a specific entity
 *
 * @param {string} entityUri - The entity IRI
 * @returns {Object} { properties, loading, update }
 */
export function useEntity(entityUri) {
  const { shard, submitDelta, connection } = useKGC();

  const properties = useMemo(() => {
    if (!shard?.quads || !entityUri) return {};

    const entityQuads = shard.quads.filter((q) => q.subject.value === entityUri);

    const props = {};
    for (const q of entityQuads) {
      const predName = q.predicate.value.split('/').pop().split('#').pop();
      props[predName] = {
        value: q.object.value,
        termType: q.object.termType,
        datatype: q.object.datatype,
        language: q.object.language,
        predicate: q.predicate.value,
      };
    }

    return props;
  }, [shard?.quads, entityUri]);

  const update = useCallback(
    async (predicate, newValue, options = {}) => {
      // Find current value
      const currentQuad = shard?.quads?.find(
        (q) => q.subject.value === entityUri && q.predicate.value === predicate
      );

      const operations = [];

      // Delete old value
      if (currentQuad) {
        operations.push({
          type: 'delete',
          subject: { value: entityUri, termType: 'NamedNode' },
          predicate: { value: predicate, termType: 'NamedNode' },
          object: currentQuad.object,
        });
      }

      // Add new value
      operations.push({
        type: 'add',
        subject: { value: entityUri, termType: 'NamedNode' },
        predicate: { value: predicate, termType: 'NamedNode' },
        object: {
          value: newValue,
          termType: options.termType || 'Literal',
          datatype: options.datatype,
          language: options.language,
        },
      });

      return submitDelta(operations);
    },
    [entityUri, shard?.quads, submitDelta]
  );

  return {
    properties,
    loading: connection === 'connecting' || connection === 'syncing',
    update,
  };
}

/**
 * useEntities - Get all entities of a specific type
 *
 * @param {string} typeUri - The rdf:type IRI
 * @returns {Object} { entities, loading }
 */
export function useEntities(typeUri) {
  const { shard, connection } = useKGC();

  const entities = useMemo(() => {
    if (!shard?.quads || !typeUri) return [];

    // Find all subjects with the given type
    const typeQuads = shard.quads.filter(
      (q) =>
        q.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' &&
        q.object.value === typeUri
    );

    const entityUris = typeQuads.map((q) => q.subject.value);

    // Build entity objects
    return entityUris.map((uri) => {
      const entityQuads = shard.quads.filter((q) => q.subject.value === uri);
      const props = { uri };

      for (const q of entityQuads) {
        const predName = q.predicate.value.split('/').pop().split('#').pop();
        props[predName] = q.object.value;
      }

      return props;
    });
  }, [shard?.quads, typeUri]);

  return {
    entities,
    loading: connection === 'connecting' || connection === 'syncing',
    count: entities.length,
  };
}

/**
 * useDelta - Manage delta submission with optimistic updates
 *
 * @returns {Object} { submit, pending, lastResult }
 */
export function useDelta() {
  const { submitDelta, pendingDeltas } = useKGC();
  const [lastResult, setLastResult] = useState(null);

  const submit = useCallback(
    async (operations) => {
      const result = await submitDelta(operations);
      setLastResult(result);
      return result;
    },
    [submitDelta]
  );

  return {
    submit,
    pending: pendingDeltas,
    hasPending: pendingDeltas.length > 0,
    lastResult,
  };
}

/**
 * useVectorClock - Access vector clock state
 *
 * @returns {Object} Vector clock info
 */
export function useVectorClock() {
  const { vectorClock } = useKGC();

  return {
    clock: vectorClock,
    nodeId: vectorClock?.nodeId,
    counters: vectorClock?.counters,
    formatted: vectorClock
      ? Object.entries(vectorClock.counters || {})
          .map(([k, v]) => `${k}:${v}`)
          .join(', ')
      : 'N/A',
  };
}

/**
 * useConnection - Connection state management
 *
 * @returns {Object} Connection info and controls
 */
export function useConnection() {
  const { connection, error, connect, disconnect, isConnected, isSyncing, hasError } = useKGC();

  return {
    state: connection,
    error,
    connect,
    disconnect,
    isConnected,
    isSyncing,
    hasError,
  };
}

/**
 * useEventLog - Access event log for debugging
 *
 * @param {number} limit - Maximum events to return
 * @returns {Object} { events, clear }
 */
export function useEventLog(limit = 50) {
  const { events } = useKGC();

  return {
    events: events.slice(0, limit),
    count: events.length,
  };
}

/**
 * useUniverseStats - Fetch and cache Universe statistics
 *
 * @returns {Object} { stats, loading, refresh }
 */
export function useUniverseStats() {
  const { stats, fetchStats } = useKGC();
  const [loading, setLoading] = useState(false);

  const refresh = useCallback(async () => {
    setLoading(true);
    await fetchStats();
    setLoading(false);
  }, [fetchStats]);

  useEffect(() => {
    refresh();
  }, []);

  return {
    stats,
    loading,
    refresh,
  };
}
