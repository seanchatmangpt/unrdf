/**
 * @file use-critical-path.mjs
 * @description React hook for critical path analysis and bottleneck identification
 */

import { useState, useCallback, useEffect, useRef } from 'react';
import { useDarkMatterCore } from './use-dark-matter-core.mjs';

/**
 * Hook for analyzing critical execution paths and identifying bottlenecks
 * Focus optimization on the path that delivers maximum value
 *
 * @param {Object} config - Critical path configuration
 * @param {string[]} [config.operations] - Operations to trace
 * @param {boolean} [config.autoTrace=true] - Auto-trace critical paths
 * @param {Function} [config.onBottleneck] - Callback when bottleneck found
 * @returns {Object} Critical path state and operations
 *
 * @example
 * const {
 *   criticalPath,
 *   bottlenecks,
 *   traceOperation,
 *   optimizePath,
 *   visualization
 * } = useCriticalPath({
 *   operations: ['query', 'render', 'network'],
 *   onBottleneck: (bottleneck) => {
 *     console.warn('Bottleneck:', bottleneck.operation, bottleneck.impact);
 *   }
 * });
 */
export function useCriticalPath(config = {}) {
  const darkMatter = useDarkMatterCore({
    targets: config.operations,
    autoAnalyze: config.autoTrace !== false
  });

  const [criticalPath, setCriticalPath] = useState(null);
  const [bottlenecks, setBottlenecks] = useState([]);
  const [traces, setTraces] = useState([]);
  const [visualization, setVisualization] = useState(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);
  const tracerRef = useRef(null);

  // Initialize critical path tracer
  useEffect(() => {
    let mounted = true;

    async function initializeTracer() {
      try {
        setLoading(true);

        // Import tracer module
        const { CriticalPathTracer } = await import(
          '../../knowledge-engine/dark-matter/critical-path-tracer.mjs'
        );

        // Create tracer
        const tracer = new CriticalPathTracer({
          operations: config.operations,
          onBottleneck: (bottleneck) => {
            if (!mounted) return;
            setBottlenecks(prev => [...prev, {
              ...bottleneck,
              timestamp: new Date().toISOString()
            }]);
            config.onBottleneck?.(bottleneck);
          }
        });

        if (!mounted) return;

        tracerRef.current = tracer;
        setLoading(false);
      } catch (err) {
        if (!mounted) return;
        setError(err);
        setLoading(false);
      }
    }

    initializeTracer();

    return () => {
      mounted = false;
    };
  }, [config.operations]);

  // Analyze dark matter data for critical path
  useEffect(() => {
    if (!darkMatter.analysis || !darkMatter.criticalPaths.length) return;

    // Build critical path from dark matter analysis
    const path = buildCriticalPath(darkMatter.criticalPaths);
    setCriticalPath(path);

    // Identify bottlenecks
    const pathBottlenecks = identifyBottlenecks(path, darkMatter.criticalPaths);
    setBottlenecks(pathBottlenecks);

    // Generate visualization data
    const viz = generateVisualization(path, darkMatter);
    setVisualization(viz);
  }, [darkMatter.analysis, darkMatter.criticalPaths]);

  // Build critical path from operations
  function buildCriticalPath(operations) {
    // Sort by cumulative value (operations that contribute to 80% value)
    const critical = operations
      .filter(op => op.cumulativeValue <= 0.8)
      .sort((a, b) => a.cumulativeValue - b.cumulativeValue);

    if (critical.length === 0) return null;

    // Build dependency graph
    const nodes = critical.map(op => ({
      id: op.id,
      value: op.value,
      executionTime: op.executionTime,
      frequency: op.frequency,
      dependencies: op.dependencies || []
    }));

    // Calculate path length (sum of execution times)
    const totalTime = nodes.reduce((sum, node) => sum + node.executionTime, 0);

    // Find longest path (critical path)
    const longestPath = findLongestPath(nodes);

    return {
      operations: critical,
      nodes,
      longestPath,
      totalTime,
      totalValue: critical.reduce((sum, op) => sum + op.value, 0),
      averageValue: critical.reduce((sum, op) => sum + op.value, 0) / critical.length
    };
  }

  // Find longest path in dependency graph
  function findLongestPath(nodes) {
    const paths = [];
    const visited = new Set();

    function dfs(node, path, time) {
      visited.add(node.id);
      path.push(node);

      if (node.dependencies.length === 0) {
        paths.push({
          path: [...path],
          time,
          value: path.reduce((sum, n) => sum + n.value, 0)
        });
      } else {
        for (const depId of node.dependencies) {
          const depNode = nodes.find(n => n.id === depId);
          if (depNode && !visited.has(depId)) {
            dfs(depNode, path, time + depNode.executionTime);
          }
        }
      }

      visited.delete(node.id);
      path.pop();
    }

    // Start DFS from each node
    nodes.forEach(node => {
      if (!visited.has(node.id)) {
        dfs(node, [], node.executionTime);
      }
    });

    // Return longest path by time
    return paths.reduce((longest, current) =>
      current.time > longest.time ? current : longest
    , { path: [], time: 0, value: 0 });
  }

  // Identify bottlenecks in critical path
  function identifyBottlenecks(path, operations) {
    if (!path) return [];

    const bottlenecks = [];

    // Bottleneck 1: Slowest operations
    operations
      .filter(op => op.executionTime > 100) // Slower than 100ms
      .sort((a, b) => b.executionTime - a.executionTime)
      .slice(0, 3)
      .forEach(op => {
        bottlenecks.push({
          id: `bottleneck-slow-${op.id}`,
          type: 'slow-operation',
          severity: 'high',
          operation: op.id,
          impact: Math.round(op.value * 100),
          executionTime: op.executionTime,
          reason: `Operation takes ${op.executionTime}ms and delivers ${Math.round(op.value * 100)}% of value`,
          suggestions: [
            'Add caching',
            'Optimize query',
            'Implement request batching'
          ]
        });
      });

    // Bottleneck 2: High-frequency operations
    operations
      .filter(op => op.frequency > 100)
      .sort((a, b) => b.frequency - a.frequency)
      .slice(0, 3)
      .forEach(op => {
        bottlenecks.push({
          id: `bottleneck-freq-${op.id}`,
          type: 'high-frequency',
          severity: 'medium',
          operation: op.id,
          impact: Math.round(op.value * 100),
          frequency: op.frequency,
          reason: `Operation called ${op.frequency} times, causing overhead`,
          suggestions: [
            'Implement debouncing',
            'Add memoization',
            'Batch requests'
          ]
        });
      });

    // Bottleneck 3: Operations with high cumulative cost
    operations
      .map(op => ({
        ...op,
        cumulativeCost: op.executionTime * op.frequency
      }))
      .sort((a, b) => b.cumulativeCost - a.cumulativeCost)
      .slice(0, 3)
      .forEach(op => {
        bottlenecks.push({
          id: `bottleneck-cumulative-${op.id}`,
          type: 'cumulative-cost',
          severity: 'high',
          operation: op.id,
          impact: Math.round(op.value * 100),
          cumulativeCost: op.cumulativeCost,
          reason: `Total cost ${op.cumulativeCost}ms (${op.executionTime}ms × ${op.frequency} calls)`,
          suggestions: [
            'Reduce call frequency',
            'Optimize execution time',
            'Cache results'
          ]
        });
      });

    return bottlenecks.sort((a, b) => b.impact - a.impact);
  }

  // Generate visualization data
  function generateVisualization(path, dm) {
    if (!path) return null;

    return {
      type: 'critical-path-graph',
      nodes: path.nodes.map(node => ({
        id: node.id,
        label: node.id,
        value: Math.round(node.value * 100),
        time: node.executionTime,
        frequency: node.frequency,
        category: node.value > 0.2 ? 'critical' : 'important'
      })),
      edges: path.nodes.flatMap(node =>
        node.dependencies.map(depId => ({
          source: node.id,
          target: depId,
          weight: node.executionTime
        }))
      ),
      longestPath: path.longestPath.path.map(n => n.id),
      stats: {
        totalTime: path.totalTime,
        totalValue: Math.round(path.totalValue * 100),
        operationCount: path.operations.length,
        paretoScore: dm.analysis.paretoScore
      }
    };
  }

  // Trace a specific operation
  const traceOperation = useCallback(async (operationId) => {
    if (!tracerRef.current) {
      throw new Error('Critical path tracer not initialized');
    }

    try {
      setLoading(true);
      setError(null);

      const trace = await tracerRef.current.trace(operationId);

      setTraces(prev => [...prev, {
        ...trace,
        timestamp: new Date().toISOString()
      }]);

      setLoading(false);
      return trace;
    } catch (err) {
      setError(err);
      setLoading(false);
      throw err;
    }
  }, []);

  // Optimize critical path
  const optimizePath = useCallback(async (options = {}) => {
    if (!criticalPath) {
      throw new Error('No critical path identified');
    }

    try {
      setLoading(true);
      setError(null);

      const optimizations = [];

      // Strategy 1: Cache top operations
      const topOps = criticalPath.operations
        .sort((a, b) => b.value - a.value)
        .slice(0, 5);

      topOps.forEach(op => {
        optimizations.push({
          operation: op.id,
          strategy: 'cache',
          estimatedGain: `${Math.round(op.executionTime * 0.8)}ms per call`,
          implementation: 'createCachedHook with 5min TTL'
        });
      });

      // Strategy 2: Parallelize independent operations
      const independentOps = criticalPath.operations.filter(op =>
        !op.dependencies || op.dependencies.length === 0
      );

      if (independentOps.length > 1) {
        optimizations.push({
          operations: independentOps.map(o => o.id),
          strategy: 'parallelize',
          estimatedGain: `${Math.round(criticalPath.totalTime * 0.4)}ms total`,
          implementation: 'Promise.all or concurrent hooks'
        });
      }

      // Strategy 3: Optimize bottlenecks
      bottlenecks.slice(0, 3).forEach(bottleneck => {
        optimizations.push({
          operation: bottleneck.operation,
          strategy: 'optimize-bottleneck',
          estimatedGain: `${Math.round(bottleneck.impact)}% value improvement`,
          implementation: bottleneck.suggestions[0]
        });
      });

      setLoading(false);
      return { optimizations, criticalPath };
    } catch (err) {
      setError(err);
      setLoading(false);
      throw err;
    }
  }, [criticalPath, bottlenecks]);

  // Get bottleneck by severity
  const getBottlenecksBySeverity = useCallback((severity) => {
    return bottlenecks.filter(b => b.severity === severity);
  }, [bottlenecks]);

  // Get operation impact
  const getOperationImpact = useCallback((operationId) => {
    if (!criticalPath) return null;

    const operation = criticalPath.operations.find(op => op.id === operationId);
    if (!operation) return null;

    return {
      operation: operationId,
      value: Math.round(operation.value * 100),
      executionTime: operation.executionTime,
      frequency: operation.frequency,
      cumulativeValue: Math.round(operation.cumulativeValue * 100),
      isBottleneck: bottlenecks.some(b => b.operation === operationId),
      inLongestPath: criticalPath.longestPath.path.some(n => n.id === operationId)
    };
  }, [criticalPath, bottlenecks]);

  return {
    criticalPath,
    bottlenecks,
    traces,
    visualization,
    loading,
    error,
    traceOperation,
    optimizePath,
    getBottlenecksBySeverity,
    getOperationImpact
  };
}
