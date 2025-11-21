/**
 * @file use-anomaly-detector.mjs
 * @description React hook for detecting anomalies in graph data
 */

import { useState, useCallback, useEffect } from 'react';
import { useKnowledgeEngineContext } from '../core/use-knowledge-engine-context.mjs';

/**
 * Hook for detecting anomalies and outliers in RDF graphs
 *
 * @param {Object} config - Anomaly detector configuration
 * @param {number} [config.threshold=2.5] - Standard deviations for anomaly
 * @param {Function} [config.onAnomaly] - Callback when anomaly detected
 * @returns {Object} Anomaly detector state and operations
 *
 * @example
 * const {
 *   detectAnomalies,
 *   anomalies,
 *   patterns,
 *   analyze
 * } = useAnomalyDetector({
 *   threshold: 2.5,
 *   onAnomaly: (anomaly) => {
 *     console.warn('Anomaly detected:', anomaly);
 *   }
 * });
 */
export function useAnomalyDetector(config = {}) {
  const { engine } = useKnowledgeEngineContext();
  const [anomalies, setAnomalies] = useState([]);
  const [patterns, setPatterns] = useState({});
  const [stats, setStats] = useState({ total: 0, anomalies: 0 });
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  const detectAnomalies = useCallback(async (predicate) => {
    try {
      setLoading(true);

      const result = await engine.query(`
        SELECT ?s ?o WHERE {
          ?s <${predicate}> ?o
        }
      `);

      const values = result.map(b => parseFloat(b.o.value)).filter(v => !isNaN(v));

      if (values.length === 0) {
        setLoading(false);
        return [];
      }

      const mean = values.reduce((sum, v) => sum + v, 0) / values.length;
      const stdDev = Math.sqrt(
        values.reduce((sum, v) => sum + Math.pow(v - mean, 2), 0) / values.length
      );

      const threshold = config.threshold || 2.5;
      const detected = result
        .map((b, i) => ({
          subject: b.s.value,
          value: values[i],
          zScore: Math.abs((values[i] - mean) / stdDev),
          isAnomaly: Math.abs((values[i] - mean) / stdDev) > threshold
        }))
        .filter(item => item.isAnomaly);

      setAnomalies(prev => [...prev, ...detected]);
      setStats({ total: values.length, anomalies: detected.length });

      detected.forEach(anomaly => config.onAnomaly?.(anomaly));

      setLoading(false);
      return detected;
    } catch (err) {
      setError(err);
      setLoading(false);
      throw err;
    }
  }, [engine, config]);

  const analyzePatterns = useCallback(async () => {
    try {
      setLoading(true);

      const result = await engine.query(`
        SELECT ?p (COUNT(?s) as ?count) WHERE {
          ?s ?p ?o
        }
        GROUP BY ?p
        ORDER BY DESC(?count)
      `);

      const patternData = {};
      result.forEach(b => {
        patternData[b.p.value] = parseInt(b.count.value);
      });

      setPatterns(patternData);
      setLoading(false);
      return patternData;
    } catch (err) {
      setError(err);
      setLoading(false);
      throw err;
    }
  }, [engine]);

  return { detectAnomalies, analyzePatterns, anomalies, patterns, stats, loading, error };
}
