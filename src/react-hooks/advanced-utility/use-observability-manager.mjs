/**
 * @file use-observability-manager.mjs
 * @description React hook for comprehensive observability and monitoring
 */

import { useState, useCallback, useEffect, useRef } from 'react';
import { useKnowledgeEngineContext } from '../core/use-knowledge-engine-context.mjs';

/**
 * Hook for managing observability: tracing, metrics, logging
 *
 * @param {Object} config - Observability configuration
 * @param {boolean} [config.enableTracing=true] - Enable distributed tracing
 * @param {boolean} [config.enableMetrics=true] - Enable metrics collection
 * @returns {Object} Observability state and operations
 *
 * @example
 * const {
 *   startTrace,
 *   recordMetric,
 *   traces,
 *   metrics,
 *   dashboard
 * } = useObservabilityManager({
 *   enableTracing: true,
 *   enableMetrics: true
 * });
 */
export function useObservabilityManager(_config = {}) {
  const { _engine } = useKnowledgeEngineContext();
  const [traces, setTraces] = useState([]);
  const [metrics, setMetrics] = useState({});
  const [dashboard, setDashboard] = useState(null);
  const [loading, _setLoading] = useState(false);
  const [error, _setError] = useState(null);
  const activeTracesRef = useRef(new Map());

  const startTrace = useCallback(operation => {
    const traceId = `trace-${Date.now()}-${Math.random().toString(36).slice(2, 11)}`;
    const trace = {
      id: traceId,
      operation,
      startTime: performance.now(),
      spans: [],
    };

    activeTracesRef.current.set(traceId, trace);
    return traceId;
  }, []);

  const endTrace = useCallback(traceId => {
    const trace = activeTracesRef.current.get(traceId);
    if (!trace) return;

    trace.endTime = performance.now();
    trace.duration = trace.endTime - trace.startTime;

    setTraces(prev => [...prev, trace]);
    activeTracesRef.current.delete(traceId);

    return trace;
  }, []);

  const addSpan = useCallback((traceId, spanName) => {
    const trace = activeTracesRef.current.get(traceId);
    if (!trace) return;

    const span = {
      name: spanName,
      startTime: performance.now(),
      endTime: null,
      duration: null,
    };

    trace.spans.push(span);
    return span;
  }, []);

  const recordMetric = useCallback((metricName, value) => {
    setMetrics(prev => ({
      ...prev,
      [metricName]: {
        value,
        timestamp: new Date().toISOString(),
      },
    }));
  }, []);

  useEffect(() => {
    const updateDashboard = () => {
      setDashboard({
        activeTraces: activeTracesRef.current.size,
        totalTraces: traces.length,
        avgDuration:
          traces.length > 0 ? traces.reduce((sum, t) => sum + t.duration, 0) / traces.length : 0,
        metricsCount: Object.keys(metrics).length,
        timestamp: new Date().toISOString(),
      });
    };

    const interval = setInterval(updateDashboard, 1000);
    return () => clearInterval(interval);
  }, [traces, metrics]);

  return {
    startTrace,
    endTrace,
    addSpan,
    recordMetric,
    traces,
    metrics,
    dashboard,
    loading,
    error,
  };
}
