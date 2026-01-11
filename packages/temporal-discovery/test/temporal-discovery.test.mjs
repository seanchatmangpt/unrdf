/**
 * @file Comprehensive tests for temporal discovery
 * @description Tests for all temporal discovery algorithms
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  apriori,
  receiptsToTransactions,
  mineReceiptPatterns,
} from '../src/pattern-miner.mjs';
import {
  detectAnomalies,
  batchDetectAnomalies,
  filterBySeverity,
} from '../src/anomaly-detector.mjs';
import {
  analyzeTrends,
  getSmoothedSeries,
  findStrongestTrend,
} from '../src/trend-analyzer.mjs';
import {
  findCorrelation,
  findMultipleCorrelations,
  filterByStrength,
} from '../src/correlation-finder.mjs';
import {
  detectChangepoints,
  findMostSignificantChangepoint,
  segmentTimeSeries,
} from '../src/changepoint-detector.mjs';
import {
  TemporalDiscoveryEngine,
  createDiscoveryEngine,
} from '../src/temporal-discovery-engine.mjs';

describe('Pattern Mining - Apriori Algorithm', () => {
  it('should mine frequent itemsets with minimum support', () => {
    const transactions = [
      new Set(['A', 'B', 'C']),
      new Set(['A', 'B']),
      new Set(['A', 'C']),
      new Set(['B', 'C']),
      new Set(['A', 'B', 'C']),
    ];

    const patterns = apriori(transactions, { minSupport: 0.4 });

    expect(patterns).toBeDefined();
    expect(patterns.length).toBeGreaterThan(0);

    const itemA = patterns.find((p) => p.items.length === 1 && p.items[0] === 'A');
    expect(itemA).toBeDefined();
    expect(itemA.support).toBeGreaterThanOrEqual(0.4);
  });

  it('should respect maxPatternLength parameter', () => {
    const transactions = [
      new Set(['A', 'B', 'C', 'D']),
      new Set(['A', 'B', 'C', 'D']),
      new Set(['A', 'B', 'C', 'D']),
    ];

    const patterns = apriori(transactions, {
      minSupport: 0.5,
      maxPatternLength: 2,
    });

    const maxLength = Math.max(...patterns.map((p) => p.items.length));
    expect(maxLength).toBeLessThanOrEqual(2);
  });

  it('should return empty array for no transactions', () => {
    const patterns = apriori([], { minSupport: 0.1 });
    expect(patterns).toEqual([]);
  });

  it('should convert receipts to transactions', () => {
    const receipts = [
      { operation: 'insert', entityType: 'Triple', author: 'user1' },
      { operation: 'delete', entityType: 'Triple', author: 'user1' },
    ];

    const transactions = receiptsToTransactions(receipts);

    expect(transactions).toHaveLength(2);
    expect(transactions[0]).toBeInstanceOf(Set);
    expect(transactions[0].has('op:insert')).toBe(true);
    expect(transactions[0].has('type:Triple')).toBe(true);
  });

  it('should mine patterns from receipts', () => {
    const receipts = Array.from({ length: 100 }, (_, i) => ({
      timestamp: 1000 + i * 1000,
      operation: i % 2 === 0 ? 'insert' : 'delete',
      entityType: 'Triple',
      author: i % 3 === 0 ? 'user1' : 'user2',
    }));

    const patterns = mineReceiptPatterns(receipts, { minSupport: 0.3 });

    expect(patterns.length).toBeGreaterThan(0);
    const hasTriplePattern = patterns.some((p) => p.items.includes('type:Triple'));
    expect(hasTriplePattern).toBe(true);
  });
});

describe('Anomaly Detection - Z-Score', () => {
  it('should detect anomalies using z-score method', () => {
    const series = {
      name: 'temperature',
      data: [
        { timestamp: 1000, value: 20 },
        { timestamp: 2000, value: 21 },
        { timestamp: 3000, value: 22 },
        { timestamp: 4000, value: 20 },
        { timestamp: 5000, value: 21 },
        { timestamp: 6000, value: 100 },
        { timestamp: 7000, value: 20 },
        { timestamp: 8000, value: 21 },
        { timestamp: 9000, value: 22 },
        { timestamp: 10000, value: 20 },
      ],
    };

    const anomalies = detectAnomalies(series, { threshold: 3.0, windowSize: 5 });

    expect(anomalies.length).toBeGreaterThan(0);

    const highAnomaly = anomalies.find((a) => a.value === 100);
    expect(highAnomaly).toBeDefined();
    expect(Math.abs(highAnomaly.zScore)).toBeGreaterThanOrEqual(3.0);
  });

  it('should detect anomalies using MAD method', () => {
    const series = {
      name: 'metric',
      data: [
        { timestamp: 1000, value: 10 },
        { timestamp: 2000, value: 11 },
        { timestamp: 3000, value: 10 },
        { timestamp: 4000, value: 11 },
        { timestamp: 5000, value: 10 },
        { timestamp: 6000, value: 50 },
        { timestamp: 7000, value: 10 },
        { timestamp: 8000, value: 11 },
      ],
    };

    const anomalies = detectAnomalies(series, {
      threshold: 3.0,
      method: 'mad',
      windowSize: 5,
    });

    expect(anomalies.length).toBeGreaterThan(0);
  });

  it('should classify anomaly severity correctly', () => {
    const series = {
      name: 'metric',
      data: [
        { timestamp: 1000, value: 10 },
        { timestamp: 2000, value: 10 },
        { timestamp: 3000, value: 10 },
        { timestamp: 4000, value: 10 },
        { timestamp: 5000, value: 10 },
        { timestamp: 6000, value: 100 },
        { timestamp: 7000, value: 10 },
        { timestamp: 8000, value: 10 },
      ],
    };

    const anomalies = detectAnomalies(series, { threshold: 2.0, windowSize: 4 });

    expect(anomalies.length).toBeGreaterThan(0);
    const severities = anomalies.map((a) => a.severity);
    expect(severities.some((s) => ['medium', 'high', 'critical'].includes(s))).toBe(
      true
    );
  });

  it('should batch detect anomalies for multiple series', () => {
    const series1 = {
      name: 'series1',
      data: [
        { timestamp: 1000, value: 10 },
        { timestamp: 2000, value: 100 },
      ],
    };

    const series2 = {
      name: 'series2',
      data: [
        { timestamp: 1000, value: 5 },
        { timestamp: 2000, value: 50 },
      ],
    };

    const results = batchDetectAnomalies([series1, series2], { threshold: 2.0 });

    expect(results.size).toBe(2);
    expect(results.has('series1')).toBe(true);
    expect(results.has('series2')).toBe(true);
  });

  it('should filter anomalies by severity', () => {
    const anomalies = [
      { timestamp: 1000, value: 10, zScore: 3.5, severity: 'medium' },
      { timestamp: 2000, value: 20, zScore: 5.0, severity: 'critical' },
      { timestamp: 3000, value: 15, zScore: 2.5, severity: 'low' },
    ];

    const critical = filterBySeverity(anomalies, ['critical', 'high']);

    expect(critical).toHaveLength(1);
    expect(critical[0].severity).toBe('critical');
  });
});

describe('Trend Analysis', () => {
  it('should detect increasing trend', () => {
    const series = {
      name: 'growth',
      data: Array.from({ length: 20 }, (_, i) => ({
        timestamp: 1000 + i * 1000,
        value: 10 + i * 2,
      })),
    };

    const trends = analyzeTrends(series, { windowSize: 10 });

    expect(trends.length).toBeGreaterThan(0);
    const increasing = trends.filter((t) => t.direction === 'increasing');
    expect(increasing.length).toBeGreaterThan(0);
  });

  it('should detect decreasing trend', () => {
    const series = {
      name: 'decline',
      data: Array.from({ length: 20 }, (_, i) => ({
        timestamp: 1000 + i * 1000,
        value: 100 - i * 3,
      })),
    };

    const trends = analyzeTrends(series, { windowSize: 10 });

    const decreasing = trends.filter((t) => t.direction === 'decreasing');
    expect(decreasing.length).toBeGreaterThan(0);
  });

  it('should detect stable trend', () => {
    const series = {
      name: 'stable',
      data: Array.from({ length: 20 }, (_, i) => ({
        timestamp: 1000 + i * 1000,
        value: 50 + (Math.random() - 0.5) * 0.1,
      })),
    };

    const trends = analyzeTrends(series, { windowSize: 10, volatilityThreshold: 0.5 });

    const stable = trends.filter((t) => t.direction === 'stable');
    expect(stable.length).toBeGreaterThan(0);
  });

  it('should smooth time series with exponential moving average', () => {
    const series = {
      name: 'noisy',
      data: [
        { timestamp: 1000, value: 10 },
        { timestamp: 2000, value: 15 },
        { timestamp: 3000, value: 8 },
        { timestamp: 4000, value: 12 },
      ],
    };

    const smoothed = getSmoothedSeries(series, 0.3);

    expect(smoothed.name).toContain('smoothed');
    expect(smoothed.data).toHaveLength(series.data.length);
  });

  it('should find strongest trend', () => {
    const series = {
      name: 'mixed',
      data: Array.from({ length: 30 }, (_, i) => ({
        timestamp: 1000 + i * 1000,
        value: 10 + i * 2 + Math.random() * 5,
      })),
    };

    const strongest = findStrongestTrend(series, { windowSize: 10 });

    expect(strongest).toBeDefined();
    expect(strongest.strength).toBeGreaterThan(0);
  });
});

describe('Correlation Analysis', () => {
  it('should find positive correlation between series', () => {
    const series1 = {
      name: 'series1',
      data: Array.from({ length: 20 }, (_, i) => ({
        timestamp: 1000 + i * 1000,
        value: i * 2,
      })),
    };

    const series2 = {
      name: 'series2',
      data: Array.from({ length: 20 }, (_, i) => ({
        timestamp: 1000 + i * 1000,
        value: i * 2 + 5,
      })),
    };

    const correlation = findCorrelation(series1, series2);

    expect(correlation).toBeDefined();
    expect(correlation.coefficient).toBeGreaterThan(0.9);
    expect(correlation.direction).toBe('positive');
    expect(correlation.strength).toBe('very_strong');
  });

  it('should find negative correlation between series', () => {
    const series1 = {
      name: 'series1',
      data: Array.from({ length: 20 }, (_, i) => ({
        timestamp: 1000 + i * 1000,
        value: i * 2,
      })),
    };

    const series2 = {
      name: 'series2',
      data: Array.from({ length: 20 }, (_, i) => ({
        timestamp: 1000 + i * 1000,
        value: 40 - i * 2,
      })),
    };

    const correlation = findCorrelation(series1, series2);

    expect(correlation).toBeDefined();
    expect(correlation.coefficient).toBeLessThan(-0.9);
    expect(correlation.direction).toBe('negative');
  });

  it('should return null for insufficient overlap', () => {
    const series1 = {
      name: 'series1',
      data: [
        { timestamp: 1000, value: 10 },
        { timestamp: 2000, value: 20 },
      ],
    };

    const series2 = {
      name: 'series2',
      data: [
        { timestamp: 3000, value: 30 },
        { timestamp: 4000, value: 40 },
      ],
    };

    const correlation = findCorrelation(series1, series2, { minOverlap: 5 });

    expect(correlation).toBeNull();
  });

  it('should find multiple correlations', () => {
    const series1 = {
      name: 's1',
      data: Array.from({ length: 15 }, (_, i) => ({
        timestamp: 1000 + i * 1000,
        value: i,
      })),
    };

    const series2 = {
      name: 's2',
      data: Array.from({ length: 15 }, (_, i) => ({
        timestamp: 1000 + i * 1000,
        value: i + 5,
      })),
    };

    const series3 = {
      name: 's3',
      data: Array.from({ length: 15 }, (_, i) => ({
        timestamp: 1000 + i * 1000,
        value: 20 - i,
      })),
    };

    const correlations = findMultipleCorrelations([series1, series2, series3]);

    expect(correlations.length).toBeGreaterThan(0);
  });

  it('should filter correlations by strength', () => {
    const correlations = [
      {
        series1: 's1',
        series2: 's2',
        coefficient: 0.9,
        strength: 'very_strong',
        direction: 'positive',
      },
      {
        series1: 's1',
        series2: 's3',
        coefficient: 0.3,
        strength: 'weak',
        direction: 'positive',
      },
    ];

    const strong = filterByStrength(correlations, ['strong', 'very_strong']);

    expect(strong).toHaveLength(1);
    expect(strong[0].strength).toBe('very_strong');
  });
});

describe('Changepoint Detection - PELT', () => {
  it('should detect changepoint in step function', () => {
    const series = {
      name: 'step',
      data: [
        ...Array.from({ length: 10 }, (_, i) => ({
          timestamp: 1000 + i * 1000,
          value: 10,
        })),
        ...Array.from({ length: 10 }, (_, i) => ({
          timestamp: 11000 + i * 1000,
          value: 20,
        })),
      ],
    };

    const changepoints = detectChangepoints(series, { penalty: 1.0 });

    expect(changepoints.length).toBeGreaterThan(0);
    expect(changepoints[0].magnitude).toBeGreaterThan(5);
  });

  it('should detect multiple changepoints', () => {
    const series = {
      name: 'multi',
      data: [
        ...Array.from({ length: 10 }, (_, i) => ({
          timestamp: 1000 + i * 1000,
          value: 10,
        })),
        ...Array.from({ length: 10 }, (_, i) => ({
          timestamp: 11000 + i * 1000,
          value: 20,
        })),
        ...Array.from({ length: 10 }, (_, i) => ({
          timestamp: 21000 + i * 1000,
          value: 30,
        })),
      ],
    };

    const changepoints = detectChangepoints(series, { penalty: 2.0, maxChangepoints: 5 });

    expect(changepoints.length).toBeGreaterThan(1);
  });

  it('should find most significant changepoint', () => {
    const series = {
      name: 'step',
      data: [
        ...Array.from({ length: 10 }, (_, i) => ({
          timestamp: 1000 + i * 1000,
          value: 10,
        })),
        ...Array.from({ length: 10 }, (_, i) => ({
          timestamp: 11000 + i * 1000,
          value: 50,
        })),
      ],
    };

    const mostSignificant = findMostSignificantChangepoint(series);

    expect(mostSignificant).toBeDefined();
    expect(mostSignificant.magnitude).toBeGreaterThan(30);
  });

  it('should segment time series at changepoints', () => {
    const series = {
      name: 'data',
      data: [
        ...Array.from({ length: 10 }, (_, i) => ({
          timestamp: 1000 + i * 1000,
          value: 10,
        })),
        ...Array.from({ length: 10 }, (_, i) => ({
          timestamp: 11000 + i * 1000,
          value: 20,
        })),
      ],
    };

    const changepoints = detectChangepoints(series, { penalty: 1.0 });
    const segments = segmentTimeSeries(series, changepoints);

    expect(segments.length).toBeGreaterThan(1);
    expect(segments[0].data.length).toBeGreaterThan(0);
  });
});

describe('Temporal Discovery Engine', () => {
  let engine;

  beforeEach(() => {
    engine = new TemporalDiscoveryEngine({
      enablePatternMining: false,
      enableAnomalyDetection: true,
      enableTrendAnalysis: true,
      enableCorrelationAnalysis: true,
      enableChangepointDetection: true,
    });
  });

  it('should create engine with default options', () => {
    const defaultEngine = createDiscoveryEngine();
    expect(defaultEngine).toBeInstanceOf(TemporalDiscoveryEngine);
  });

  it('should run full discovery on time series', () => {
    const series = {
      name: 'test',
      data: Array.from({ length: 30 }, (_, i) => ({
        timestamp: 1000 + i * 1000,
        value: 10 + i + (i > 15 ? 20 : 0) + Math.random() * 2,
      })),
    };

    const results = engine.discover(series);

    expect(results).toBeDefined();
    expect(results.metadata).toBeDefined();
    expect(results.metadata.executionTimeMs).toBeGreaterThan(0);
    expect(results.metadata.dataPointsProcessed).toBe(30);
  });

  it('should discover anomalies', () => {
    const series = {
      name: 'anomaly-test',
      data: [
        { timestamp: 1000, value: 10 },
        { timestamp: 2000, value: 11 },
        { timestamp: 3000, value: 10 },
        { timestamp: 4000, value: 11 },
        { timestamp: 5000, value: 10 },
        { timestamp: 6000, value: 100 },
        { timestamp: 7000, value: 10 },
        { timestamp: 8000, value: 11 },
      ],
    };

    const anomalies = engine.discoverAnomalies(series);

    expect(anomalies.length).toBeGreaterThan(0);
  });

  it('should discover trends', () => {
    const series = {
      name: 'trend-test',
      data: Array.from({ length: 20 }, (_, i) => ({
        timestamp: 1000 + i * 1000,
        value: 10 + i * 2,
      })),
    };

    const trends = engine.discoverTrends(series);

    expect(trends.length).toBeGreaterThan(0);
  });

  it('should discover changepoints', () => {
    const series = {
      name: 'cp-test',
      data: [
        ...Array.from({ length: 10 }, (_, i) => ({
          timestamp: 1000 + i * 1000,
          value: 10,
        })),
        ...Array.from({ length: 10 }, (_, i) => ({
          timestamp: 11000 + i * 1000,
          value: 30,
        })),
      ],
    };

    const changepoints = engine.discoverChangepoints(series);

    expect(changepoints.length).toBeGreaterThan(0);
  });

  it('should generate summary statistics', () => {
    const series = {
      name: 'summary-test',
      data: Array.from({ length: 30 }, (_, i) => ({
        timestamp: 1000 + i * 1000,
        value: 10 + i + (i === 15 ? 50 : 0),
      })),
    };

    const results = engine.discover(series);
    const summary = engine.getSummary(results);

    expect(summary).toBeDefined();
    expect(summary.executionTimeMs).toBeGreaterThan(0);
    expect(summary.algorithmsRun).toBeInstanceOf(Array);
  });

  it('should handle multiple time series', () => {
    const series1 = {
      name: 's1',
      data: Array.from({ length: 20 }, (_, i) => ({
        timestamp: 1000 + i * 1000,
        value: i,
      })),
    };

    const series2 = {
      name: 's2',
      data: Array.from({ length: 20 }, (_, i) => ({
        timestamp: 1000 + i * 1000,
        value: i + 5,
      })),
    };

    const results = engine.discover([series1, series2]);

    expect(results).toBeDefined();
    expect(results.correlations).toBeDefined();
    expect(results.correlations.length).toBeGreaterThan(0);
  });

  it('should discover from receipts', () => {
    const engineWithPatterns = new TemporalDiscoveryEngine({
      enablePatternMining: true,
    });

    const receipts = Array.from({ length: 50 }, (_, i) => ({
      timestamp: 1000 + i * 1000,
      operation: i % 2 === 0 ? 'insert' : 'delete',
      entityType: 'Triple',
    }));

    const results = engineWithPatterns.discoverFromReceipts(receipts);

    expect(results).toBeDefined();
    expect(results.patterns).toBeDefined();
    expect(results.metadata.dataPointsProcessed).toBe(50);
  });
});

describe('Performance Benchmarks', () => {
  it('should mine patterns in <1s for 10K receipts', () => {
    const receipts = Array.from({ length: 10000 }, (_, i) => ({
      timestamp: 1000 + i,
      operation: i % 3 === 0 ? 'insert' : i % 3 === 1 ? 'delete' : 'update',
      entityType: i % 2 === 0 ? 'Triple' : 'Graph',
      author: `user${i % 10}`,
    }));

    const startTime = performance.now();
    const patterns = mineReceiptPatterns(receipts, { minSupport: 0.01 });
    const endTime = performance.now();

    expect(endTime - startTime).toBeLessThan(1000);
    expect(patterns.length).toBeGreaterThan(0);
  });

  it('should detect anomalies in <100ms for 1K data points', () => {
    const series = {
      name: 'perf-test',
      data: Array.from({ length: 1000 }, (_, i) => ({
        timestamp: 1000 + i,
        value: 10 + Math.random() * 5 + (i === 500 ? 100 : 0),
      })),
    };

    const startTime = performance.now();
    const anomalies = detectAnomalies(series, { threshold: 3.0, windowSize: 50 });
    const endTime = performance.now();

    expect(endTime - startTime).toBeLessThan(100);
    expect(anomalies.length).toBeGreaterThan(0);
  });

  it('should analyze trends in <50ms', () => {
    const series = {
      name: 'trend-perf',
      data: Array.from({ length: 100 }, (_, i) => ({
        timestamp: 1000 + i * 1000,
        value: 10 + i * 0.5,
      })),
    };

    const startTime = performance.now();
    const trends = analyzeTrends(series, { windowSize: 10 });
    const endTime = performance.now();

    expect(endTime - startTime).toBeLessThan(50);
    expect(trends.length).toBeGreaterThan(0);
  });

  it('should detect changepoints in <3s for 1K points', () => {
    const series = {
      name: 'cp-perf',
      data: [
        ...Array.from({ length: 500 }, (_, i) => ({
          timestamp: 1000 + i,
          value: 10,
        })),
        ...Array.from({ length: 500 }, (_, i) => ({
          timestamp: 1500 + i,
          value: 30,
        })),
      ],
    };

    const startTime = performance.now();
    const changepoints = detectChangepoints(series, { penalty: 3.0 });
    const endTime = performance.now();

    expect(endTime - startTime).toBeLessThan(3000);
    expect(changepoints.length).toBeGreaterThan(0);
  });
});
