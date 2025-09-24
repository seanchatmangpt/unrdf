/**
 * @fileoverview useMetrics composable - performance metrics and monitoring with context
 * 
 * This composable provides performance monitoring and metrics collection.
 * It enables observability for RDF operations and composable performance.
 * Now uses unctx for global metrics management.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { useStoreContext } from "../context/index.mjs";

/**
 * Create a metrics composable
 * 
 * @param {Object} [options] - Metrics options
 * @param {boolean} [options.enabled=true] - Enable metrics collection
 * @param {number} [options.maxHistory=1000] - Maximum history size
 * @returns {Object} Metrics interface
 * 
 * @example
 * // Initialize store context first
 * const runApp = initStore();
 * 
 * runApp(() => {
 *   const metrics = useMetrics();
 * 
 *   // Wrap a function with metrics
 *   const wrappedFn = metrics.wrap('my-operation', async () => {
 *     // Your operation here
 *     return result;
 *   });
 * 
 *   // Get performance data
 *   const lastMetric = metrics.last();
 *   const timeline = metrics.timeline();
 * 
 *   // Create a timer
 *   const timer = metrics.timer('complex-operation');
 *   // ... do work ...
 *   timer.end();
 * });
 * 
 * @throws {Error} If store context is not initialized
 */
export function useMetrics(options = {}) {
  // Get the engine from context
  const storeContext = useStoreContext();
  const engine = storeContext.engine;
  
  const {
    enabled = true,
    maxHistory = 1000
  } = options || {};

  // Internal storage
  const history = [];
  const timers = new Map();

  return {
    /**
     * Wrap a function with metrics collection
     * @param {string} label - Label for the operation
     * @param {Function} fn - Function to wrap
     * @returns {Function} Wrapped function
     */
    wrap(label, fn) {
      if (fn == undefined || typeof fn !== "function") {
        throw new Error("[useMetrics] Function is required");
      }
      
      if (!enabled) {
        return fn;
      }

      return async (...args) => {
        const start = performance.now();
        let error = null;
        let result = null;

        try {
          result = await fn(...args);
          return result;
        } catch (error_) {
          error = error_;
          throw error_;
        } finally {
          const end = performance.now();
          const duration = end - start;

          this._record({
            label,
            start,
            end,
            duration,
            error: error ? {
              message: error.message,
              stack: error.stack,
              name: error.name
            } : null,
            args: args.length,
            result: result !== null,
            success: error === null
          });
        }
      };
    },

    /**
     * Create a timer for manual timing
     * @param {string} label - Label for the timer
     * @returns {Object} Timer object
     */
    timer(label) {
      if (!enabled) {
        return {
          end: () => null,
          elapsed: () => 0
        };
      }

      const start = performance.now();
      const timerId = `${label}-${start}`;
      
      // Add to timers map
      timers.set(timerId, true);

      const self = this; // Capture reference to metrics object

      return {
        /**
         * End the timer and record the metric
         * @returns {Object} Metric object
         */
        end() {
          if (!timers.has(timerId)) {
            return null; // Already ended
          }
          
          const end = performance.now();
          const duration = end - start;

          const metric = {
            label,
            start,
            end,
            duration,
            error: null,
            args: 0,
            result: true,
            success: true
          };

          self._record(metric);
          timers.delete(timerId);
          return metric;
        },

        /**
         * Get elapsed time without ending the timer
         * @returns {number} Elapsed time in milliseconds
         */
        elapsed() {
          return performance.now() - start;
        }
      };
    },

    /**
     * Record a custom metric
     * @param {string} label - Label for the metric
     * @param {number} value - Metric value
     * @param {Object} [metadata] - Additional metadata
     */
    record(label, value, metadata = {}) {
      if (!enabled) {
        return;
      }

      const now = performance.now();
      this._record({
        label,
        start: now,
        end: now,
        duration: value,
        error: null,
        args: 0,
        result: true,
        ...metadata
      });
    },

    /**
     * Get the last recorded metric
     * @returns {Object|null} Last metric or null
     */
    last() {
      return history.length > 0 ? history.at(-1) : null;
    },

    /**
     * Get all metrics in chronological order
     * @returns {Array} Array of metrics
     */
    timeline() {
      return [...history];
    },

    /**
     * Get metrics filtered by label
     * @param {string} label - Label to filter by
     * @returns {Array} Filtered metrics
     */
    byLabel(label) {
      return history.filter(metric => metric.label === label);
    },

    /**
     * Get summary statistics for a label
     * @param {string} label - Label to summarize
     * @returns {Object} Summary statistics
     */
    summary(label) {
      const metrics = this.byLabel(label);
      
      if (metrics.length === 0) {
        return {
          count: 0,
          total: 0,
          average: 0,
          min: 0,
          max: 0,
          errors: 0
        };
      }

      const durations = metrics.map(m => m.duration);
      const errors = metrics.filter(m => m.error).length;

      return {
        count: metrics.length,
        total: durations.reduce((sum, d) => sum + d, 0),
        average: durations.reduce((sum, d) => sum + d, 0) / durations.length,
        min: Math.min(...durations),
        max: Math.max(...durations),
        errors,
        errorRate: errors / metrics.length
      };
    },

    /**
     * Clear all metrics
     * @returns {Object} This composable instance
     */
    clear() {
      history.length = 0;
      timers.clear();
      return this;
    },

    /**
     * Get metrics in a specific time range
     * @param {number} startTime - Start time (performance.now())
     * @param {number} endTime - End time (performance.now())
     * @returns {Array} Metrics in time range
     */
    inRange(startTime, endTime) {
      return history.filter(metric => 
        metric.start >= startTime && metric.end <= endTime
      );
    },

    /**
     * Export metrics as JSON
     * @returns {string} JSON string of metrics
     */
    export() {
      return JSON.stringify({
        timestamp: new Date().toISOString(),
        count: history.length,
        metrics: history
      }, null, 2);
    },

    /**
     * Record a metric internally
     * @param {Object} metric - Metric to record
     * @private
     */
    _record(metric) {
      if (!enabled) {
        return;
      }

      history.push(metric);

      // Maintain history size limit
      if (history.length > maxHistory) {
        history.shift();
      }
    }
  };
}
