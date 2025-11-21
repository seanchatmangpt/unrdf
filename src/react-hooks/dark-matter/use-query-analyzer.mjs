/**
 * @file use-query-analyzer.mjs
 * @description React hook for SPARQL query optimization analysis
 */

import { useState, useCallback, useEffect, useRef } from 'react';
import { useKnowledgeEngineContext } from '../core/use-knowledge-engine-context.mjs';

/**
 * Hook for analyzing and optimizing SPARQL queries
 * Identifies slow queries, suggests optimizations, and tracks query patterns
 *
 * @param {Object} config - Query analyzer configuration
 * @param {boolean} [config.autoOptimize=false] - Auto-apply safe optimizations
 * @param {number} [config.slowThreshold=100] - Slow query threshold (ms)
 * @param {Function} [config.onSlowQuery] - Callback for slow queries
 * @returns {Object} Analyzer state and operations
 *
 * @example
 * const {
 *   analyzeQuery,
 *   optimizeQuery,
 *   slowQueries,
 *   suggestions,
 *   queryStats
 * } = useQueryAnalyzer({
 *   slowThreshold: 100,
 *   onSlowQuery: (query) => {
 *     console.warn('Slow query detected:', query.sparql, query.executionTime);
 *   }
 * });
 */
export function useQueryAnalyzer(config = {}) {
  const { engine } = useKnowledgeEngineContext();
  const [analyzer, setAnalyzer] = useState(null);
  const [slowQueries, setSlowQueries] = useState([]);
  const [suggestions, setSuggestions] = useState([]);
  const [queryStats, setQueryStats] = useState({
    totalQueries: 0,
    avgExecutionTime: 0,
    slowQueryCount: 0,
    cachedQueryCount: 0,
    optimizedQueryCount: 0
  });
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);
  const analyzerRef = useRef(null);
  const queryHistoryRef = useRef([]);

  // Initialize query analyzer
  useEffect(() => {
    let mounted = true;

    async function initializeAnalyzer() {
      try {
        setLoading(true);

        // Import analyzer module
        const { QueryAnalyzer } = await import(
          '../../knowledge-engine/dark-matter/query-analyzer.mjs'
        );

        // Create analyzer
        const qAnalyzer = new QueryAnalyzer({
          engine,
          slowThreshold: config.slowThreshold || 100,
          onSlowQuery: (query) => {
            if (!mounted) return;
            setSlowQueries(prev => [...prev, {
              ...query,
              timestamp: new Date().toISOString()
            }]);
            config.onSlowQuery?.(query);
          }
        });

        if (!mounted) return;

        analyzerRef.current = qAnalyzer;
        setAnalyzer(qAnalyzer);
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
  }, [engine, config.slowThreshold]);

  // Analyze a SPARQL query
  const analyzeQuery = useCallback(async (sparql, options = {}) => {
    if (!analyzerRef.current) {
      throw new Error('Query analyzer not initialized');
    }

    try {
      setLoading(true);
      setError(null);

      const analysis = await analyzerRef.current.analyze(sparql, {
        explain: options.explain !== false,
        profile: options.profile !== false
      });

      // Add to history
      queryHistoryRef.current.push({
        sparql,
        analysis,
        timestamp: new Date().toISOString()
      });

      // Update stats
      setQueryStats(prev => {
        const total = prev.totalQueries + 1;
        const avgTime = ((prev.avgExecutionTime * prev.totalQueries) + analysis.executionTime) / total;
        return {
          totalQueries: total,
          avgExecutionTime: Math.round(avgTime),
          slowQueryCount: analysis.isSlow ? prev.slowQueryCount + 1 : prev.slowQueryCount,
          cachedQueryCount: analysis.cached ? prev.cachedQueryCount + 1 : prev.cachedQueryCount,
          optimizedQueryCount: prev.optimizedQueryCount
        };
      });

      // Generate suggestions
      if (analysis.issues?.length > 0) {
        const newSuggestions = generateSuggestions(sparql, analysis);
        setSuggestions(prev => [...prev, ...newSuggestions]);
      }

      setLoading(false);
      return analysis;
    } catch (err) {
      setError(err);
      setLoading(false);
      throw err;
    }
  }, []);

  // Generate optimization suggestions
  function generateSuggestions(sparql, analysis) {
    const suggestions = [];

    // Issue: Missing INDEX
    if (analysis.issues?.includes('MISSING_INDEX')) {
      suggestions.push({
        type: 'index',
        severity: 'high',
        query: sparql,
        reason: 'Query performs full table scan. Add index for significant speedup.',
        suggestion: `Create index on predicates: ${analysis.predicates?.join(', ')}`,
        estimatedGain: '10-100x faster',
        autoApplicable: false
      });
    }

    // Issue: Cartesian product
    if (analysis.issues?.includes('CARTESIAN_PRODUCT')) {
      suggestions.push({
        type: 'join',
        severity: 'critical',
        query: sparql,
        reason: 'Query contains Cartesian product. Add join conditions to prevent exponential results.',
        suggestion: 'Add connecting triple patterns between unconnected variables',
        estimatedGain: '100-1000x faster',
        autoApplicable: false
      });
    }

    // Issue: Missing LIMIT
    if (analysis.issues?.includes('MISSING_LIMIT')) {
      suggestions.push({
        type: 'limit',
        severity: 'medium',
        query: sparql,
        reason: 'Query may return many results. Add LIMIT to improve performance.',
        suggestion: 'Add LIMIT clause (e.g., LIMIT 100)',
        estimatedGain: '2-10x faster',
        autoApplicable: true,
        optimizedQuery: sparql + '\nLIMIT 100'
      });
    }

    // Issue: Expensive OPTIONAL
    if (analysis.issues?.includes('EXPENSIVE_OPTIONAL')) {
      suggestions.push({
        type: 'optional',
        severity: 'medium',
        query: sparql,
        reason: 'OPTIONAL clause is expensive. Consider making it required or moving to separate query.',
        suggestion: 'Replace OPTIONAL with required pattern or use UNION',
        estimatedGain: '2-5x faster',
        autoApplicable: false
      });
    }

    // Issue: Complex FILTER
    if (analysis.issues?.includes('COMPLEX_FILTER')) {
      suggestions.push({
        type: 'filter',
        severity: 'low',
        query: sparql,
        reason: 'Complex FILTER expression. Consider moving to triple patterns where possible.',
        suggestion: 'Replace FILTER with more specific triple patterns',
        estimatedGain: '1.5-3x faster',
        autoApplicable: false
      });
    }

    return suggestions;
  }

  // Optimize a SPARQL query
  const optimizeQuery = useCallback(async (sparql, options = {}) => {
    if (!analyzerRef.current) {
      throw new Error('Query analyzer not initialized');
    }

    try {
      setLoading(true);
      setError(null);

      // Analyze first
      const analysis = await analyzeQuery(sparql);

      // Apply optimizations
      let optimized = sparql;

      // Auto-apply safe optimizations
      if (config.autoOptimize || options.autoApply) {
        const autoApplicable = suggestions.filter(s =>
          s.autoApplicable && s.query === sparql
        );

        for (const suggestion of autoApplicable) {
          if (suggestion.optimizedQuery) {
            optimized = suggestion.optimizedQuery;
          }
        }
      }

      // Manual optimization strategies
      if (options.strategies) {
        for (const strategy of options.strategies) {
          switch (strategy) {
            case 'add-limit':
              if (!optimized.includes('LIMIT')) {
                optimized += '\nLIMIT 100';
              }
              break;
            case 'add-index-hints':
              // Add index hints based on analysis
              if (analysis.predicates?.length > 0) {
                optimized = `# INDEX HINT: ${analysis.predicates.join(', ')}\n${optimized}`;
              }
              break;
            case 'reorder-triples':
              // Reorder triple patterns for better execution plan
              optimized = await analyzerRef.current.reorderTriples(optimized);
              break;
          }
        }
      }

      // Update stats
      setQueryStats(prev => ({
        ...prev,
        optimizedQueryCount: prev.optimizedQueryCount + 1
      }));

      setLoading(false);

      return {
        original: sparql,
        optimized,
        analysis,
        suggestions: suggestions.filter(s => s.query === sparql),
        estimatedGain: calculateEstimatedGain(analysis, suggestions)
      };
    } catch (err) {
      setError(err);
      setLoading(false);
      throw err;
    }
  }, [analyzeQuery, suggestions, config.autoOptimize]);

  // Calculate estimated performance gain
  function calculateEstimatedGain(analysis, querySuggestions) {
    const relevantSuggestions = querySuggestions.filter(s => s.query === analysis.query);
    if (relevantSuggestions.length === 0) return '1x';

    // Parse gain estimates (e.g., "10-100x" -> 55x average)
    const gains = relevantSuggestions.map(s => {
      const match = s.estimatedGain.match(/(\d+)-(\d+)x/);
      if (match) {
        return (parseInt(match[1]) + parseInt(match[2])) / 2;
      }
      const singleMatch = s.estimatedGain.match(/(\d+)x/);
      return singleMatch ? parseInt(singleMatch[1]) : 1;
    });

    const totalGain = gains.reduce((product, gain) => product * Math.sqrt(gain), 1);
    return `${Math.round(totalGain)}x`;
  }

  // Get top slow queries
  const getTopSlowQueries = useCallback((count = 10) => {
    return slowQueries
      .sort((a, b) => b.executionTime - a.executionTime)
      .slice(0, count);
  }, [slowQueries]);

  // Get suggestions by severity
  const getSuggestionsBySeverity = useCallback((severity) => {
    return suggestions.filter(s => s.severity === severity);
  }, [suggestions]);

  // Get query pattern analysis
  const getQueryPatternAnalysis = useCallback(() => {
    const patterns = {
      select: 0,
      construct: 0,
      ask: 0,
      describe: 0
    };

    const features = {
      optional: 0,
      union: 0,
      filter: 0,
      orderBy: 0,
      limit: 0,
      groupBy: 0
    };

    queryHistoryRef.current.forEach(({ sparql }) => {
      const upper = sparql.toUpperCase();

      if (upper.includes('SELECT')) patterns.select++;
      if (upper.includes('CONSTRUCT')) patterns.construct++;
      if (upper.includes('ASK')) patterns.ask++;
      if (upper.includes('DESCRIBE')) patterns.describe++;

      if (upper.includes('OPTIONAL')) features.optional++;
      if (upper.includes('UNION')) features.union++;
      if (upper.includes('FILTER')) features.filter++;
      if (upper.includes('ORDER BY')) features.orderBy++;
      if (upper.includes('LIMIT')) features.limit++;
      if (upper.includes('GROUP BY')) features.groupBy++;
    });

    return { patterns, features };
  }, []);

  // Clear history
  const clear = useCallback(() => {
    setSlowQueries([]);
    setSuggestions([]);
    queryHistoryRef.current = [];
    setQueryStats({
      totalQueries: 0,
      avgExecutionTime: 0,
      slowQueryCount: 0,
      cachedQueryCount: 0,
      optimizedQueryCount: 0
    });
  }, []);

  return {
    analyzeQuery,
    optimizeQuery,
    slowQueries,
    suggestions,
    queryStats,
    loading,
    error,
    getTopSlowQueries,
    getSuggestionsBySeverity,
    getQueryPatternAnalysis,
    clear
  };
}
