/**
 * @file playground/hooks-showcase/lib/unrdf-kgc-bridge.mjs
 * @description Bridge UNRDF React Hooks API to KGC-4D backend
 *
 * This adapter provides the clean UNRDF React hooks API (useKnowledgeEngine, useTriples, etc.)
 * while using KGC-4D's Shard/Universe/Tether architecture as the backend.
 *
 * Architecture:
 * - KGCProvider (from @unrdf/kgc-4d) manages connection to Universe via SSE
 * - UNRDF hooks are adapters that translate to KGC-4D's useShard/useEntity/useDelta
 * - Quad format is compatible between systems
 */

'use client';

import { useMemo, useCallback, useState, useEffect } from 'react';
import { useKGC, useShard, useEntity, useEntities, useDelta } from '../../../packages/kgc-4d/playground/lib/client/hooks.mjs';

/**
 * useKnowledgeEngine - Core hook for RDF operations
 *
 * Provides UNRDF-compatible API backed by KGC-4D
 *
 * @returns {Object} Knowledge engine interface
 */
export function useKnowledgeEngine() {
  const { shard, connection, error, refreshShard } = useKGC();
  const { submit: submitDelta } = useDelta();

  const ready = connection === 'connected';
  const isLoading = connection === 'connecting' || connection === 'syncing';

  /**
   * Add triples to the knowledge graph
   */
  const addTriples = useCallback(async (triples) => {
    const operations = triples.map(triple => ({
      type: 'add',
      subject: { value: triple.subject, termType: 'NamedNode' },
      predicate: { value: triple.predicate, termType: 'NamedNode' },
      object: triple.object.startsWith('http')
        ? { value: triple.object, termType: 'NamedNode' }
        : { value: triple.object, termType: 'Literal' }
    }));

    const result = await submitDelta(operations);
    return result;
  }, [submitDelta]);

  /**
   * Delete triples from the knowledge graph
   */
  const deleteTriples = useCallback(async (triples) => {
    const operations = triples.map(triple => ({
      type: 'delete',
      subject: { value: triple.subject, termType: 'NamedNode' },
      predicate: { value: triple.predicate, termType: 'NamedNode' },
      object: triple.object.startsWith('http')
        ? { value: triple.object, termType: 'NamedNode' }
        : { value: triple.object, termType: 'Literal' }
    }));

    const result = await submitDelta(operations);
    return result;
  }, [submitDelta]);

  /**
   * Execute SPARQL query (simplified - uses client-side filtering)
   */
  const query = useCallback(async (sparql) => {
    // For demo purposes, we'll do simple pattern matching
    // In production, this would send SPARQL to server

    console.warn('SPARQL queries not yet implemented - using pattern matching');

    // Return all quads for now
    return shard?.quads || [];
  }, [shard]);

  return {
    // State
    ready,
    isLoading,
    error,

    // Core operations
    addTriples,
    deleteTriples,
    query,

    // Utility
    refresh: refreshShard,

    // Metadata
    quadCount: shard?.quad_count || 0,
    shardId: shard?.id,
    timestamp: shard?.timestamp_iso
  };
}

/**
 * useTriples - Query triples by pattern
 *
 * @param {Object} pattern - Triple pattern filter
 * @param {string} [pattern.subject] - Subject filter
 * @param {string} [pattern.predicate] - Predicate filter
 * @param {string} [pattern.object] - Object filter
 * @returns {Object} Query result
 */
export function useTriples(pattern = {}) {
  const { quads, loading } = useShard({
    subject: pattern.subject,
    // KGC-4D uses 'type' instead of predicate/object filtering
  });

  // Convert quads to UNRDF triple format
  const data = useMemo(() => {
    if (!quads) return [];

    let filtered = quads;

    // Client-side filtering for predicate/object
    if (pattern.predicate) {
      filtered = filtered.filter(q => q.predicate.value === pattern.predicate);
    }
    if (pattern.object) {
      filtered = filtered.filter(q => q.object.value === pattern.object);
    }

    // Convert to simpler triple format
    return filtered.map(q => ({
      subject: q.subject.value,
      predicate: q.predicate.value,
      object: q.object.value,
      termType: q.object.termType,
      datatype: q.object.datatype,
      language: q.object.language
    }));
  }, [quads, pattern.predicate, pattern.object]);

  return {
    data,
    isLoading: loading,
    error: null,
    refetch: () => {}, // KGC-4D auto-refreshes via SSE
    count: data.length
  };
}

/**
 * useChangeFeed - Subscribe to real-time graph changes
 *
 * KGC-4D provides this via SSE (Tether) automatically
 *
 * @param {Object} options - Change feed options
 * @returns {Array} Recent changes
 */
export function useChangeFeed(options = {}) {
  const { events } = useKGC();

  // Filter events to delta operations
  const changes = useMemo(() => {
    return events
      .filter(e => e.type === 'DELTA')
      .flatMap(e => e.delta?.operations || [])
      .map(op => ({
        type: op.type, // 'add' or 'delete'
        triple: {
          subject: op.subject.value,
          predicate: op.predicate.value,
          object: op.object.value
        },
        timestamp: Date.now(), // KGC-4D doesn't timestamp individual ops
        source: 'tether'
      }));
  }, [events]);

  // Apply user filters
  const filtered = useMemo(() => {
    if (!options.filter) return changes;
    return changes.filter(options.filter);
  }, [changes, options.filter]);

  return filtered;
}

/**
 * useSPARQLQuery - Execute SPARQL SELECT queries (NOW WITH REAL BACKEND)
 *
 * Sends SPARQL query to /api/sparql endpoint for server-side execution
 *
 * @param {string} sparql - SPARQL query string
 * @param {Object} options - Query options
 * @returns {Object} Query result
 */
export function useSPARQLQuery(sparql, options = {}) {
  const [data, setData] = useState(null);
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState(null);

  const executeQuery = useCallback(async () => {
    if (!sparql) return;

    setIsLoading(true);
    setError(null);

    try {
      const response = await fetch('/api/sparql', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          query: sparql,
          variables: options.variables || {}
        })
      });

      const result = await response.json();

      if (result.success) {
        setData(result.results);
      } else {
        setError(new Error(result.error));
      }
    } catch (err) {
      setError(err);
    } finally {
      setIsLoading(false);
    }
  }, [sparql, options.variables]);

  useEffect(() => {
    if (options.enabled !== false) {
      executeQuery();
    }
  }, [sparql, executeQuery, options.enabled]);

  return {
    data,
    isLoading,
    error,
    refetch: executeQuery
  };
}

/**
 * useGraphs - List available named graphs
 *
 * @returns {Object} Graphs list
 */
export function useGraphs() {
  const { shard } = useKGC();

  const data = useMemo(() => {
    if (!shard?.quads) return [];

    // Extract unique graphs
    const graphs = new Set();
    shard.quads.forEach(q => {
      if (q.graph) graphs.add(q.graph.value);
    });

    return Array.from(graphs);
  }, [shard]);

  return {
    data,
    isLoading: false,
    error: null,
    count: data.length
  };
}

/**
 * useErrorBoundary - Error handling
 *
 * @param {Object} options - Error boundary options
 * @returns {Object} Error state
 */
export function useErrorBoundary(options = {}) {
  const { error } = useKGC();

  return {
    error,
    hasError: !!error,
    clearError: () => {},
    reset: () => {}
  };
}

/**
 * useQueryAnalyzer - Query performance analysis
 *
 * @returns {Object} Analyzer interface
 */
export function useQueryAnalyzer() {
  const { stats } = useKGC();

  return {
    analyze: (query) => {
      return {
        estimatedTime: 0,
        complexity: 'simple',
        suggestions: []
      };
    },
    stats: stats || {}
  };
}

/**
 * useDarkMatterCore - Query optimization
 *
 * @returns {Object} Optimizer interface
 */
export function useDarkMatterCore() {
  return {
    optimize: (query) => query,
    analyze: (query) => ({ optimized: query }),
    enabled: true
  };
}
