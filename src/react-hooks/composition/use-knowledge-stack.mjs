/**
 * @file use-knowledge-stack.mjs
 * @description Pre-configured hook composition for common use cases
 *
 * Innovation: Combines multiple hooks into cohesive "stacks" for specific patterns,
 * reducing boilerplate and ensuring best practices are followed automatically.
 */

import { useState, useCallback, useMemo, useEffect } from 'react';
import { useKnowledgeEngine } from '../core/index.mjs';
import { useChangeFeed } from '../streaming/use-change-feed.mjs';
import { useErrorBoundary } from '../error-recovery/use-error-boundary.mjs';
import { useRecovery } from '../error-recovery/use-recovery.mjs';

/**
 * @typedef {'basic' | 'realtime' | 'resilient' | 'full'} StackPreset
 */

/**
 * @typedef {Object} KnowledgeStackConfig
 * @property {StackPreset} [preset='basic'] - Pre-configured stack preset
 * @property {boolean} [enableRealtime=false] - Enable real-time updates
 * @property {boolean} [enableRecovery=false] - Enable automatic retry on failure
 * @property {boolean} [enableErrorBoundary=true] - Enable error boundary
 * @property {number} [maxRetries=3] - Max retries for recovery
 * @property {number} [retryDelay=1000] - Retry delay in ms
 * @property {string[]} [operations=['insert', 'delete']] - Change feed operations
 * @property {Function} [onError] - Error callback
 */

/**
 * Pre-configured hook composition for common knowledge graph patterns
 *
 * @param {KnowledgeStackConfig} config - Stack configuration
 * @returns {Object} Combined hook interface
 *
 * @example
 * // Basic usage - just queries
 * const { query, data, loading } = useKnowledgeStack({ preset: 'basic' });
 *
 * @example
 * // Real-time dashboard
 * const { query, data, changes, startLive } = useKnowledgeStack({ preset: 'realtime' });
 *
 * @example
 * // Production-resilient app
 * const { query, data, executeWithRecovery, hasError } = useKnowledgeStack({ preset: 'resilient' });
 *
 * @example
 * // Full-featured app
 * const stack = useKnowledgeStack({ preset: 'full' });
 */
export function useKnowledgeStack(config = {}) {
  const preset = config.preset || 'basic';

  // Resolve preset to feature flags
  const features = useMemo(() => resolvePreset(preset, config), [preset, config]);

  // Core knowledge engine - always enabled
  const engine = useKnowledgeEngine();

  // Conditional hooks based on features
  const changeFeed = useChangeFeed({
    operations: config.operations || ['insert', 'delete'],
    batchSize: config.batchSize || 10
  });

  const errorBoundary = useErrorBoundary({
    onError: config.onError
  });

  const recovery = useRecovery({
    maxRetries: config.maxRetries || 3,
    retryDelay: config.retryDelay || 1000
  });

  // Composed query with optional recovery
  const resilientQuery = useCallback(async (sparql) => {
    if (features.enableRecovery) {
      return recovery.executeWithRecovery(() => engine.query(sparql));
    }
    return engine.query(sparql);
  }, [engine, recovery, features.enableRecovery]);

  // Composed insert with optional recovery
  const resilientInsert = useCallback(async (quads) => {
    if (features.enableRecovery) {
      return recovery.executeWithRecovery(() => engine.insert(quads));
    }
    return engine.insert(quads);
  }, [engine, recovery, features.enableRecovery]);

  // Start real-time mode
  const startLive = useCallback(() => {
    if (features.enableRealtime) {
      changeFeed.start();
    }
  }, [changeFeed, features.enableRealtime]);

  // Stop real-time mode
  const stopLive = useCallback(() => {
    if (features.enableRealtime) {
      changeFeed.stop();
    }
  }, [changeFeed, features.enableRealtime]);

  // Cleanup on unmount
  useEffect(() => {
    return () => {
      if (features.enableRealtime) {
        changeFeed.stop();
      }
    };
  }, [features.enableRealtime]);

  // Build return object based on features
  const stack = useMemo(() => {
    const base = {
      // Core operations
      query: resilientQuery,
      insert: resilientInsert,
      delete: engine.delete,
      data: engine.data,
      loading: engine.loading,
      error: errorBoundary.hasError ? errorBoundary.error : engine.error,

      // Stack metadata
      preset,
      features
    };

    // Add real-time features
    if (features.enableRealtime) {
      Object.assign(base, {
        changes: changeFeed.changes,
        startLive,
        stopLive,
        isLive: changeFeed.stats?.isActive || false,
        liveStats: changeFeed.stats
      });
    }

    // Add recovery features
    if (features.enableRecovery) {
      Object.assign(base, {
        retryCount: recovery.retryCount,
        isRecovering: recovery.isRecovering,
        lastError: recovery.lastError
      });
    }

    // Add error boundary features
    if (features.enableErrorBoundary) {
      Object.assign(base, {
        hasError: errorBoundary.hasError,
        resetError: errorBoundary.resetError,
        captureError: errorBoundary.captureError
      });
    }

    return base;
  }, [
    engine, changeFeed, errorBoundary, recovery,
    resilientQuery, resilientInsert, startLive, stopLive,
    preset, features
  ]);

  return stack;
}

/**
 * Resolve preset name to feature flags
 * @param {StackPreset} preset - Preset name
 * @param {KnowledgeStackConfig} config - Override config
 * @returns {Object} Feature flags
 */
function resolvePreset(preset, config) {
  const presets = {
    // Basic: Just queries, minimal overhead
    basic: {
      enableRealtime: false,
      enableRecovery: false,
      enableErrorBoundary: true
    },

    // Realtime: Live updates for dashboards
    realtime: {
      enableRealtime: true,
      enableRecovery: false,
      enableErrorBoundary: true
    },

    // Resilient: Auto-retry for unreliable networks
    resilient: {
      enableRealtime: false,
      enableRecovery: true,
      enableErrorBoundary: true
    },

    // Full: Everything enabled
    full: {
      enableRealtime: true,
      enableRecovery: true,
      enableErrorBoundary: true
    }
  };

  const base = presets[preset] || presets.basic;

  // Allow config overrides
  return {
    enableRealtime: config.enableRealtime ?? base.enableRealtime,
    enableRecovery: config.enableRecovery ?? base.enableRecovery,
    enableErrorBoundary: config.enableErrorBoundary ?? base.enableErrorBoundary
  };
}

/**
 * Pre-built stack for CRUD applications
 * @param {Object} config - Configuration
 * @returns {Object} CRUD stack
 */
export function useCRUDStack(config = {}) {
  return useKnowledgeStack({
    ...config,
    preset: 'basic',
    enableErrorBoundary: true
  });
}

/**
 * Pre-built stack for real-time dashboards
 * @param {Object} config - Configuration
 * @returns {Object} Dashboard stack
 */
export function useDashboardStack(config = {}) {
  const stack = useKnowledgeStack({
    ...config,
    preset: 'realtime',
    operations: ['insert', 'delete', 'update']
  });

  // Auto-start live mode
  useEffect(() => {
    stack.startLive?.();
    return () => stack.stopLive?.();
  }, []);

  return stack;
}

/**
 * Pre-built stack for production apps with resilience
 * @param {Object} config - Configuration
 * @returns {Object} Production stack
 */
export function useProductionStack(config = {}) {
  return useKnowledgeStack({
    ...config,
    preset: 'full',
    maxRetries: 5,
    retryDelay: 2000
  });
}

export default useKnowledgeStack;
