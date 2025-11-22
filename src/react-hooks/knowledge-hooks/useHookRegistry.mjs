/**
 * @fileoverview useHookRegistry - Hook for accessing global hook registry
 * @module react-hooks/knowledge-hooks/useHookRegistry
 *
 * @description
 * Hook for accessing and querying the global knowledge hook registry.
 *
 * @example
 * ```jsx
 * import { useHookRegistry } from 'unrdf/react-hooks';
 *
 * function HookList() {
 *   const { hooks, getByType, search } = useHookRegistry();
 *
 *   const validationHooks = getByType('validation');
 *
 *   return (
 *     <ul>
 *       {hooks.map(hook => (
 *         <li key={hook.meta.name}>{hook.meta.name}</li>
 *       ))}
 *     </ul>
 *   );
 * }
 * ```
 */

import { useMemo, useCallback } from 'react';
import { useKnowledgeEngineContext } from '../context/useKnowledgeEngineContext.mjs';

/**
 * Hook for accessing global hook registry
 *
 * @returns {Object} Registry access and query operations
 */
export function useHookRegistry() {
  const { engine } = useKnowledgeEngineContext();

  /**
   * Get all registered hooks
   */
  const hooks = useMemo(() => {
    if (!engine || !engine.getKnowledgeHooks) {
      return [];
    }
    return engine.getKnowledgeHooks();
  }, [engine]);

  /**
   * Get hooks by type
   */
  const getByType = useCallback(
    type => {
      return hooks.filter(hook => hook.meta?.type === type || hook.when?.event?.includes(type));
    },
    [hooks]
  );

  /**
   * Get hooks by event
   */
  const getByEvent = useCallback(
    event => {
      return hooks.filter(hook => hook.when?.event === event || hook.when?.event?.includes(event));
    },
    [hooks]
  );

  /**
   * Search hooks by name or description
   */
  const search = useCallback(
    query => {
      const lowerQuery = query.toLowerCase();
      return hooks.filter(
        hook =>
          hook.meta?.name?.toLowerCase().includes(lowerQuery) ||
          hook.meta?.description?.toLowerCase().includes(lowerQuery)
      );
    },
    [hooks]
  );

  /**
   * Get hook by exact name
   */
  const getByName = useCallback(
    name => {
      return hooks.find(hook => hook.meta?.name === name);
    },
    [hooks]
  );

  /**
   * Get hooks by author
   */
  const getByAuthor = useCallback(
    author => {
      return hooks.filter(hook => hook.meta?.author === author);
    },
    [hooks]
  );

  /**
   * Get hooks by version
   */
  const getByVersion = useCallback(
    version => {
      return hooks.filter(hook => hook.meta?.version === version);
    },
    [hooks]
  );

  /**
   * Get registry statistics
   */
  const stats = useMemo(() => {
    const types = new Set();
    const events = new Set();
    const authors = new Set();

    for (const hook of hooks) {
      if (hook.meta?.type) types.add(hook.meta.type);
      if (hook.when?.event) events.add(hook.when.event);
      if (hook.meta?.author) authors.add(hook.meta.author);
    }

    return {
      total: hooks.length,
      types: Array.from(types),
      events: Array.from(events),
      authors: Array.from(authors),
      typeCount: types.size,
      eventCount: events.size,
      authorCount: authors.size,
    };
  }, [hooks]);

  return {
    hooks,
    getByType,
    getByEvent,
    getByName,
    getByAuthor,
    getByVersion,
    search,
    stats,
  };
}
