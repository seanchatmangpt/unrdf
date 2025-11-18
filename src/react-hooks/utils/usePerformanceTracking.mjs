/**
 * @fileoverview usePerformanceTracking - Hook for performance metrics
 * @module react-hooks/utils/usePerformanceTracking
 */

import { useState, useCallback } from 'react';

export function usePerformanceTracking() {
  const [metrics, setMetrics] = useState([]);

  const track = useCallback((name, fn) => {
    return async (...args) => {
      const start = performance.now();
      try {
        const result = await fn(...args);
        const duration = performance.now() - start;
        setMetrics(prev => [...prev, { name, duration, timestamp: Date.now() }]);
        return result;
      } catch (err) {
        const duration = performance.now() - start;
        setMetrics(prev => [...prev, { name, duration, error: err, timestamp: Date.now() }]);
        throw err;
      }
    };
  }, []);

  const clear = useCallback(() => setMetrics([]), []);

  return { metrics, track, clear };
}
