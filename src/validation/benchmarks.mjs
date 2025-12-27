/**
 * @fileoverview Benchmarks - SLA definitions and performance baseline management
 *
 * **Purpose**:
 * - Define performance SLAs for different package types
 * - Compare against baselines for regression detection
 * - Track performance trends over time
 *
 * **Package Types**:
 * - library: Shared code packages
 * - application: Full applications
 * - cli: Command-line tools
 * - web: Web applications/components
 * - api: API services
 *
 * @module validation/benchmarks
 */

import { readFile, writeFile, access, mkdir } from 'node:fs/promises';
import { join, dirname } from 'node:path';

/**
 * Default SLA definitions by package type
 */
export const DEFAULT_SLAS = {
  library: {
    name: 'Library Package',
    metrics: {
      bundleSize: {
        unit: 'KB',
        warn: 100,
        fail: 500,
        description: 'Estimated bundle size after minification'
      },
      testDuration: {
        unit: 'ms',
        warn: 3000,
        fail: 10000,
        description: 'Test suite execution time'
      },
      testCoverage: {
        unit: '%',
        warn: 80,
        fail: 70,
        direction: 'higher-better',
        description: 'Code coverage percentage'
      },
      startupTime: {
        unit: 'ms',
        warn: 500,
        fail: 2000,
        description: 'Module import/require time'
      },
      memoryUsage: {
        unit: 'MB',
        warn: 50,
        fail: 100,
        description: 'Memory usage at rest'
      },
      dependencyCount: {
        unit: 'count',
        warn: 20,
        fail: 50,
        description: 'Number of production dependencies'
      }
    }
  },

  application: {
    name: 'Application Package',
    metrics: {
      bundleSize: {
        unit: 'KB',
        warn: 500,
        fail: 2000,
        description: 'Total application bundle size'
      },
      testDuration: {
        unit: 'ms',
        warn: 10000,
        fail: 60000,
        description: 'Full test suite execution'
      },
      testCoverage: {
        unit: '%',
        warn: 70,
        fail: 60,
        direction: 'higher-better',
        description: 'Code coverage percentage'
      },
      startupTime: {
        unit: 'ms',
        warn: 3000,
        fail: 10000,
        description: 'Application startup time'
      },
      memoryUsage: {
        unit: 'MB',
        warn: 256,
        fail: 512,
        description: 'Memory usage at rest'
      },
      dependencyCount: {
        unit: 'count',
        warn: 100,
        fail: 200,
        description: 'Number of production dependencies'
      }
    }
  },

  cli: {
    name: 'CLI Package',
    metrics: {
      bundleSize: {
        unit: 'KB',
        warn: 200,
        fail: 1000,
        description: 'CLI bundle size'
      },
      testDuration: {
        unit: 'ms',
        warn: 5000,
        fail: 30000,
        description: 'Test suite execution'
      },
      testCoverage: {
        unit: '%',
        warn: 75,
        fail: 60,
        direction: 'higher-better',
        description: 'Code coverage percentage'
      },
      startupTime: {
        unit: 'ms',
        warn: 500,
        fail: 2000,
        description: 'CLI startup time'
      },
      memoryUsage: {
        unit: 'MB',
        warn: 100,
        fail: 256,
        description: 'Memory usage during execution'
      },
      dependencyCount: {
        unit: 'count',
        warn: 30,
        fail: 80,
        description: 'Number of production dependencies'
      }
    }
  },

  web: {
    name: 'Web Package',
    metrics: {
      bundleSize: {
        unit: 'KB',
        warn: 300,
        fail: 1000,
        description: 'JavaScript bundle size'
      },
      testDuration: {
        unit: 'ms',
        warn: 10000,
        fail: 60000,
        description: 'Test suite execution'
      },
      testCoverage: {
        unit: '%',
        warn: 70,
        fail: 50,
        direction: 'higher-better',
        description: 'Code coverage percentage'
      },
      firstContentfulPaint: {
        unit: 'ms',
        warn: 2000,
        fail: 4000,
        description: 'First Contentful Paint (estimated)'
      },
      memoryUsage: {
        unit: 'MB',
        warn: 100,
        fail: 300,
        description: 'Browser memory usage'
      },
      dependencyCount: {
        unit: 'count',
        warn: 50,
        fail: 150,
        description: 'Number of production dependencies'
      }
    }
  },

  api: {
    name: 'API Package',
    metrics: {
      bundleSize: {
        unit: 'KB',
        warn: 200,
        fail: 500,
        description: 'API bundle size'
      },
      testDuration: {
        unit: 'ms',
        warn: 10000,
        fail: 60000,
        description: 'Test suite execution'
      },
      testCoverage: {
        unit: '%',
        warn: 80,
        fail: 70,
        direction: 'higher-better',
        description: 'Code coverage percentage'
      },
      responseTime: {
        unit: 'ms',
        warn: 100,
        fail: 500,
        description: 'Average API response time'
      },
      memoryUsage: {
        unit: 'MB',
        warn: 128,
        fail: 512,
        description: 'Memory usage under load'
      },
      dependencyCount: {
        unit: 'count',
        warn: 40,
        fail: 100,
        description: 'Number of production dependencies'
      }
    }
  }
};

/**
 * Benchmark result status
 */
export const BenchmarkStatus = {
  PASS: 'pass',
  WARN: 'warn',
  FAIL: 'fail',
  SKIP: 'skip'
};

/**
 * Benchmark Manager - Manages SLAs, baselines, and comparisons
 *
 * @class BenchmarkManager
 */
export class BenchmarkManager {
  /**
   * Create a new Benchmark Manager
   *
   * @param {Object} [options] - Manager options
   * @param {string} [options.baselinePath] - Path to store baselines
   * @param {Object} [options.customSLAs] - Custom SLA overrides
   */
  constructor(options = {}) {
    this.baselinePath = options.baselinePath || '.benchmarks';
    this.customSLAs = options.customSLAs || {};
    this.results = [];
  }

  /**
   * Get SLA for a package type
   *
   * @param {string} packageType - Package type
   * @returns {Object} SLA definition
   */
  getSLA(packageType) {
    const baseSLA = DEFAULT_SLAS[packageType] || DEFAULT_SLAS.library;

    // Merge with custom SLAs
    if (this.customSLAs[packageType]) {
      return {
        ...baseSLA,
        metrics: {
          ...baseSLA.metrics,
          ...this.customSLAs[packageType].metrics
        }
      };
    }

    return baseSLA;
  }

  /**
   * Evaluate a metric against SLA
   *
   * @param {string} packageType - Package type
   * @param {string} metricName - Metric name
   * @param {number} value - Measured value
   * @returns {Object} Evaluation result
   */
  evaluateMetric(packageType, metricName, value) {
    const sla = this.getSLA(packageType);
    const metric = sla.metrics[metricName];

    if (!metric) {
      return {
        status: BenchmarkStatus.SKIP,
        message: `Unknown metric: ${metricName}`
      };
    }

    const isHigherBetter = metric.direction === 'higher-better';

    let status;
    let threshold;

    if (isHigherBetter) {
      if (value >= metric.warn) {
        status = BenchmarkStatus.PASS;
      } else if (value >= metric.fail) {
        status = BenchmarkStatus.WARN;
        threshold = metric.warn;
      } else {
        status = BenchmarkStatus.FAIL;
        threshold = metric.fail;
      }
    } else {
      if (value <= metric.warn) {
        status = BenchmarkStatus.PASS;
      } else if (value <= metric.fail) {
        status = BenchmarkStatus.WARN;
        threshold = metric.warn;
      } else {
        status = BenchmarkStatus.FAIL;
        threshold = metric.fail;
      }
    }

    return {
      metric: metricName,
      value,
      unit: metric.unit,
      status,
      threshold,
      description: metric.description
    };
  }

  /**
   * Evaluate all metrics for a package
   *
   * @param {string} packageType - Package type
   * @param {Object} metrics - Measured metrics
   * @returns {Object} Evaluation results
   */
  evaluateAll(packageType, metrics) {
    const sla = this.getSLA(packageType);
    const results = [];
    let passed = 0;
    let warned = 0;
    let failed = 0;

    for (const [name, value] of Object.entries(metrics)) {
      if (value !== null && value !== undefined) {
        const result = this.evaluateMetric(packageType, name, value);
        results.push(result);

        switch (result.status) {
          case BenchmarkStatus.PASS: passed++; break;
          case BenchmarkStatus.WARN: warned++; break;
          case BenchmarkStatus.FAIL: failed++; break;
        }
      }
    }

    const overall = failed > 0 ? BenchmarkStatus.FAIL
      : warned > 0 ? BenchmarkStatus.WARN
      : BenchmarkStatus.PASS;

    return {
      packageType,
      slaName: sla.name,
      overall,
      passed,
      warned,
      failed,
      total: results.length,
      results
    };
  }

  /**
   * Load baseline for a package
   *
   * @param {string} packageName - Package name
   * @returns {Promise<Object|null>} Baseline data
   */
  async loadBaseline(packageName) {
    try {
      const safeName = packageName.replace(/[@\/]/g, '_');
      const path = join(this.baselinePath, `${safeName}.json`);
      const content = await readFile(path, 'utf-8');
      return JSON.parse(content);
    } catch {
      return null;
    }
  }

  /**
   * Save baseline for a package
   *
   * @param {string} packageName - Package name
   * @param {Object} metrics - Metric values
   * @returns {Promise<void>}
   */
  async saveBaseline(packageName, metrics) {
    const safeName = packageName.replace(/[@\/]/g, '_');
    const path = join(this.baselinePath, `${safeName}.json`);

    // Ensure directory exists
    try {
      await mkdir(dirname(path), { recursive: true });
    } catch {
      // Directory might exist
    }

    const baseline = {
      packageName,
      timestamp: new Date().toISOString(),
      metrics
    };

    await writeFile(path, JSON.stringify(baseline, null, 2));
  }

  /**
   * Compare metrics against baseline
   *
   * @param {string} packageName - Package name
   * @param {Object} currentMetrics - Current metric values
   * @returns {Promise<Object>} Comparison results
   */
  async compareToBaseline(packageName, currentMetrics) {
    const baseline = await this.loadBaseline(packageName);

    if (!baseline) {
      return {
        hasBaseline: false,
        message: 'No baseline available for comparison'
      };
    }

    const comparisons = [];
    let regressions = 0;
    let improvements = 0;

    for (const [name, currentValue] of Object.entries(currentMetrics)) {
      const baselineValue = baseline.metrics[name];

      if (baselineValue !== undefined && currentValue !== undefined) {
        const diff = currentValue - baselineValue;
        const percentChange = baselineValue !== 0
          ? ((diff / baselineValue) * 100).toFixed(2)
          : 0;

        // Determine if this is an improvement or regression
        // For most metrics, lower is better (except coverage)
        const isHigherBetter = name.includes('coverage') || name.includes('Coverage');
        const isRegression = isHigherBetter
          ? currentValue < baselineValue
          : currentValue > baselineValue;

        if (isRegression && Math.abs(parseFloat(percentChange)) > 5) {
          regressions++;
        } else if (!isRegression && Math.abs(parseFloat(percentChange)) > 5) {
          improvements++;
        }

        comparisons.push({
          metric: name,
          baseline: baselineValue,
          current: currentValue,
          diff,
          percentChange: `${percentChange}%`,
          trend: isRegression ? 'regression' : diff === 0 ? 'stable' : 'improvement'
        });
      }
    }

    return {
      hasBaseline: true,
      baselineDate: baseline.timestamp,
      regressions,
      improvements,
      comparisons
    };
  }

  /**
   * Detect performance regressions
   *
   * @param {string} packageName - Package name
   * @param {string} packageType - Package type
   * @param {Object} currentMetrics - Current metrics
   * @returns {Promise<Object>} Regression detection result
   */
  async detectRegressions(packageName, packageType, currentMetrics) {
    const comparison = await this.compareToBaseline(packageName, currentMetrics);
    const slaEval = this.evaluateAll(packageType, currentMetrics);

    const regressions = [];

    // Check for SLA violations
    for (const result of slaEval.results) {
      if (result.status === BenchmarkStatus.FAIL) {
        regressions.push({
          metric: result.metric,
          type: 'sla-violation',
          severity: 'high',
          message: `${result.metric} (${result.value}${result.unit}) exceeds SLA threshold (${result.threshold}${result.unit})`
        });
      }
    }

    // Check for baseline regressions
    if (comparison.hasBaseline) {
      for (const comp of comparison.comparisons) {
        if (comp.trend === 'regression') {
          const pct = parseFloat(comp.percentChange);
          if (Math.abs(pct) > 20) {
            regressions.push({
              metric: comp.metric,
              type: 'baseline-regression',
              severity: 'high',
              message: `${comp.metric} regressed by ${comp.percentChange} from baseline`
            });
          } else if (Math.abs(pct) > 10) {
            regressions.push({
              metric: comp.metric,
              type: 'baseline-regression',
              severity: 'medium',
              message: `${comp.metric} regressed by ${comp.percentChange} from baseline`
            });
          }
        }
      }
    }

    return {
      hasRegressions: regressions.length > 0,
      count: regressions.length,
      highSeverity: regressions.filter(r => r.severity === 'high').length,
      mediumSeverity: regressions.filter(r => r.severity === 'medium').length,
      regressions
    };
  }

  /**
   * Generate benchmark report
   *
   * @param {string} packageName - Package name
   * @param {string} packageType - Package type
   * @param {Object} metrics - Measured metrics
   * @returns {Promise<string>} Formatted report
   */
  async generateReport(packageName, packageType, metrics) {
    const slaEval = this.evaluateAll(packageType, metrics);
    const comparison = await this.compareToBaseline(packageName, metrics);
    const regressions = await this.detectRegressions(packageName, packageType, metrics);

    const lines = [
      `# Benchmark Report: ${packageName}`,
      '',
      `**Package Type**: ${slaEval.slaName}`,
      `**Evaluated**: ${new Date().toISOString()}`,
      `**Overall Status**: ${slaEval.overall.toUpperCase()}`,
      '',
      '## SLA Compliance',
      '',
      `- Passed: ${slaEval.passed}`,
      `- Warned: ${slaEval.warned}`,
      `- Failed: ${slaEval.failed}`,
      '',
      '| Metric | Value | Threshold | Status |',
      '|--------|-------|-----------|--------|'
    ];

    for (const result of slaEval.results) {
      const thresholdStr = result.threshold ? `${result.threshold}${result.unit}` : '-';
      lines.push(`| ${result.metric} | ${result.value}${result.unit} | ${thresholdStr} | ${result.status.toUpperCase()} |`);
    }

    lines.push('');

    if (comparison.hasBaseline) {
      lines.push('## Baseline Comparison');
      lines.push('');
      lines.push(`Baseline from: ${comparison.baselineDate}`);
      lines.push(`Improvements: ${comparison.improvements} | Regressions: ${comparison.regressions}`);
      lines.push('');
      lines.push('| Metric | Baseline | Current | Change | Trend |');
      lines.push('|--------|----------|---------|--------|-------|');

      for (const comp of comparison.comparisons) {
        const emoji = comp.trend === 'improvement' ? 'UP' : comp.trend === 'regression' ? 'DOWN' : '-';
        lines.push(`| ${comp.metric} | ${comp.baseline} | ${comp.current} | ${comp.percentChange} | ${emoji} |`);
      }

      lines.push('');
    }

    if (regressions.hasRegressions) {
      lines.push('## Regressions Detected');
      lines.push('');

      for (const reg of regressions.regressions) {
        lines.push(`- **[${reg.severity.toUpperCase()}]** ${reg.message}`);
      }

      lines.push('');
    }

    return lines.join('\n');
  }
}

/**
 * Create a benchmark manager with default settings
 *
 * @param {Object} [options] - Manager options
 * @returns {BenchmarkManager} Manager instance
 */
export function createBenchmarkManager(options = {}) {
  return new BenchmarkManager(options);
}

/**
 * Quick SLA check for a package
 *
 * @param {string} packageType - Package type
 * @param {Object} metrics - Measured metrics
 * @returns {Object} SLA evaluation
 */
export function checkSLA(packageType, metrics) {
  const manager = new BenchmarkManager();
  return manager.evaluateAll(packageType, metrics);
}

export default BenchmarkManager;
