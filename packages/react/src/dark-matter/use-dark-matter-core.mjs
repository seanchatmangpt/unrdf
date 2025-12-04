/**
 * @file use-dark-matter-core.mjs
 * @description React hook for 80/20 analysis - identifying critical code paths
 * @since 3.2.0
 */

import { useState, useCallback, useEffect, useRef } from 'react';
import { useKnowledgeEngineContext } from '../core/use-knowledge-engine-context.mjs';

/**
 * Hook for analyzing and identifying "dark matter" code - the 80% that delivers 20% value
 * Focus optimization efforts on the critical 20% that delivers 80% of value
 *
 * @since 3.2.0
 * @param {Object} config - Dark matter analysis configuration
 * @param {string[]} [config.targets] - Specific queries/operations to analyze
 * @param {number} [config.sampleSize=1000] - Number of operations to sample
 * @param {boolean} [config.autoAnalyze=true] - Auto-analyze on changes
 * @param {Function} [config.onCriticalPathFound] - Callback when critical path identified
 * @returns {Object} Analysis state and operations
 * @throws {Error} When dark matter analyzer not initialized
 * @throws {Error} When analyze is called with insufficient samples
 * @throws {Error} When focusOnCritical called with unknown operationId
 * @performance Analysis is CPU-intensive - increase sampleSize for accuracy at cost of time.
 *   Pareto chart data generation is synchronous. Disable autoAnalyze for manual control.
 *
 * @example
 * // Automatic 80/20 analysis
 * const {
 *   analysis,
 *   criticalPaths,
 *   darkMatter,
 *   analyze,
 *   optimizationSuggestions
 * } = useDarkMatterCore({
 *   targets: ['products', 'orders'],
 *   sampleSize: 1000,
 *   onCriticalPathFound: (path) => {
 *     console.log('Critical path:', path, 'delivers', path.valuePercentage, '% of value');
 *   }
 * });
 *
 * @example
 * // Get Pareto chart visualization data
 * const { getParetoChartData, focusOnCritical } = useDarkMatterCore();
 * const chartData = getParetoChartData();
 * const topOp = focusOnCritical(criticalPaths[0].id);
 */
export function useDarkMatterCore(config = {}) {
  const { engine } = useKnowledgeEngineContext();
  const [_analyzer, setAnalyzer] = useState(null);
  const [analysis, setAnalysis] = useState({
    criticalPaths: [],
    darkMatter: [],
    valueDistribution: {
      critical: 0, // 20% of code
      important: 0, // 30% of code
      standard: 0, // 30% of code
      dark: 0, // 20% of code - candidates for removal
    },
    paretoScore: 0, // How close to ideal 80/20
  });
  const [criticalPaths, setCriticalPaths] = useState([]);
  const [darkMatter, setDarkMatter] = useState([]);
  const [optimizationSuggestions, setOptimizationSuggestions] = useState([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);
  const analyzerRef = useRef(null);
  const samplesRef = useRef([]);

  // Initialize dark matter analyzer
  useEffect(() => {
    let mounted = true;

    async function initializeAnalyzer() {
      try {
        setLoading(true);

        // Import analyzer module
        const { DarkMatterAnalyzer } = await import(
          '../../knowledge-engine/dark-matter/analyzer.mjs'
        );

        // Create analyzer
        const dmAnalyzer = new DarkMatterAnalyzer({
          engine,
          sampleSize: config.sampleSize || 1000,
          onSample: (sample) => {
            if (!mounted) return;
            samplesRef.current.push(sample);
          },
        });

        if (!mounted) return;

        analyzerRef.current = dmAnalyzer;
        setAnalyzer(dmAnalyzer);

        // Auto-analyze if configured
        if (config.autoAnalyze) {
          await analyze();
        }

        setLoading(false);
      } catch (err) {
        if (!mounted) return;
        setError(err);
        setLoading(false);
      }
    }

    initializeAnalyzer();

    return () => {
      mounted = false;
    };
  }, [engine]);

  // Perform 80/20 analysis
  const analyze = useCallback(
    async (options = {}) => {
      if (!analyzerRef.current) {
        throw new Error('Dark matter analyzer not initialized');
      }

      try {
        setLoading(true);
        setError(null);

        const result = await analyzerRef.current.analyze({
          targets: options.targets || config.targets,
          threshold: options.threshold || 0.8, // 80% value threshold
          minSamples: options.minSamples || 100,
        });

        // Identify critical paths (20% that delivers 80%)
        const critical = result.operations
          .filter((op) => op.cumulativeValue <= 0.8)
          .sort((a, b) => b.value - a.value);

        // Identify dark matter (80% that delivers 20%)
        const dark = result.operations
          .filter((op) => op.cumulativeValue > 0.8)
          .sort((a, b) => a.value - b.value);

        // Calculate Pareto score (how close to ideal 80/20)
        const paretoScore = calculateParetoScore(critical, dark);

        // Generate optimization suggestions
        const suggestions = generateOptimizationSuggestions(critical, dark, result);

        const analysisResult = {
          criticalPaths: critical,
          darkMatter: dark,
          valueDistribution: {
            critical: critical.reduce((sum, op) => sum + op.value, 0),
            important: result.operations
              .filter((op) => op.cumulativeValue > 0.5 && op.cumulativeValue <= 0.8)
              .reduce((sum, op) => sum + op.value, 0),
            standard: result.operations
              .filter((op) => op.cumulativeValue > 0.8 && op.cumulativeValue <= 0.95)
              .reduce((sum, op) => sum + op.value, 0),
            dark: dark.reduce((sum, op) => sum + op.value, 0),
          },
          paretoScore,
          timestamp: new Date().toISOString(),
        };

        setAnalysis(analysisResult);
        setCriticalPaths(critical);
        setDarkMatter(dark);
        setOptimizationSuggestions(suggestions);

        // Notify critical paths
        critical.forEach((path) => {
          config.onCriticalPathFound?.({
            ...path,
            valuePercentage: Math.round(path.value * 100),
          });
        });

        setLoading(false);
        return analysisResult;
      } catch (err) {
        setError(err);
        setLoading(false);
        throw err;
      }
    },
    [config]
  );

  // Calculate Pareto score (0-100, higher is better)
  function calculateParetoScore(critical, dark) {
    if (critical.length === 0 || dark.length === 0) return 0;

    const totalOps = critical.length + dark.length;
    const criticalPercent = critical.length / totalOps;
    const criticalValue = critical.reduce((sum, op) => sum + op.value, 0);

    // Ideal: 20% of operations deliver 80% of value
    const idealCriticalPercent = 0.2;
    const idealCriticalValue = 0.8;

    const percentDiff = Math.abs(criticalPercent - idealCriticalPercent);
    const valueDiff = Math.abs(criticalValue - idealCriticalValue);

    // Score based on how close to ideal (100 = perfect 80/20)
    return Math.max(0, Math.round(100 - (percentDiff * 200 + valueDiff * 100)));
  }

  // Generate optimization suggestions
  function generateOptimizationSuggestions(critical, dark, _result) {
    const suggestions = [];

    // Suggestion 1: Cache critical paths
    critical.slice(0, 5).forEach((op) => {
      suggestions.push({
        type: 'cache',
        priority: 'high',
        operation: op.id,
        reason: `This operation delivers ${Math.round(op.value * 100)}% of value. Caching will provide maximum impact.`,
        estimatedGain: `${Math.round(op.executionTime * 0.9)}ms saved per call`,
        implementation: 'createCachedHook',
      });
    });

    // Suggestion 2: Remove or lazy-load dark matter
    dark.slice(0, 3).forEach((op) => {
      if (op.value < 0.01) {
        // Less than 1% value
        suggestions.push({
          type: 'remove',
          priority: 'medium',
          operation: op.id,
          reason: `This operation delivers only ${Math.round(op.value * 100)}% of value. Consider removing or lazy-loading.`,
          estimatedGain: `${op.codeSize || 0} bytes bundle reduction`,
          implementation: 'lazy import or feature flag',
        });
      }
    });

    // Suggestion 3: Optimize query patterns
    const slowCritical = critical.filter((op) => op.executionTime > 100);
    slowCritical.forEach((op) => {
      suggestions.push({
        type: 'optimize',
        priority: 'critical',
        operation: op.id,
        reason: `High-value operation (${Math.round(op.value * 100)}%) is slow (${op.executionTime}ms). Optimization has maximum ROI.`,
        estimatedGain: `${Math.round(op.executionTime * 0.5)}ms improvement possible`,
        implementation: 'useQueryAnalyzer + useCriticalPath',
      });
    });

    return suggestions.sort((a, b) => {
      const priorityOrder = { critical: 0, high: 1, medium: 2, low: 3 };
      return priorityOrder[a.priority] - priorityOrder[b.priority];
    });
  }

  // Get operations by value percentile
  const getOperationsByPercentile = useCallback(
    (percentile) => {
      const threshold = percentile / 100;
      return analysis.criticalPaths.filter((op) => op.cumulativeValue <= threshold);
    },
    [analysis]
  );

  // Get dark matter by removal priority
  const getDarkMatterByPriority = useCallback(() => {
    return darkMatter
      .filter((op) => op.value < 0.05) // Less than 5% value
      .sort((a, b) => a.value - b.value);
  }, [darkMatter]);

  // Focus optimization on critical path
  const focusOnCritical = useCallback(
    (operationId) => {
      const operation = criticalPaths.find((op) => op.id === operationId);
      if (!operation) {
        throw new Error(`Operation ${operationId} not found in critical paths`);
      }

      return {
        operation,
        suggestions: optimizationSuggestions.filter((s) => s.operation === operationId),
        valueImpact: Math.round(operation.value * 100),
        optimizationPriority: operation.cumulativeValue <= 0.2 ? 'critical' : 'high',
      };
    },
    [criticalPaths, optimizationSuggestions]
  );

  // Get Pareto chart data
  const getParetoChartData = useCallback(() => {
    const ops = [...criticalPaths, ...darkMatter].sort((a, b) => b.value - a.value);

    let cumulative = 0;
    return ops.map((op) => {
      cumulative += op.value;
      return {
        id: op.id,
        value: Math.round(op.value * 100),
        cumulativeValue: Math.round(cumulative * 100),
        category: cumulative <= 0.8 ? 'critical' : 'dark',
      };
    });
  }, [criticalPaths, darkMatter]);

  return {
    analysis,
    criticalPaths,
    darkMatter,
    optimizationSuggestions,
    loading,
    error,
    analyze,
    getOperationsByPercentile,
    getDarkMatterByPriority,
    focusOnCritical,
    getParetoChartData,
  };
}
