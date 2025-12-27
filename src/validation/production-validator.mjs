/**
 * @fileoverview Production Validator - Comprehensive production readiness gate
 *
 * This module orchestrates all production readiness checks for packages in the
 * UNRDF monorepo. Every package must pass this gate before being considered
 * production-ready.
 *
 * **Validation Categories**:
 * 1. Code Quality - JSDoc, linting, complexity
 * 2. Testing - Coverage, pass rate, performance
 * 3. Dependencies - Circularity, versions, maintenance
 * 4. Security - Credentials, injection, OWASP
 * 5. Documentation - Coverage, examples
 * 6. Performance - Benchmarks, memory
 * 7. Accessibility - a11y compliance
 * 8. Compatibility - Node version, runtime support
 *
 * **Score Calculation**:
 * - Each category has a weight (importance)
 * - Each check returns pass/warn/fail with score 0-100
 * - Final score is weighted average
 * - Production-ready threshold: 95%
 *
 * @module validation/production-validator
 */

import { z } from 'zod';
import { codeQualityCheck } from './checks/code-quality-check.mjs';
import { testCheck } from './checks/test-check.mjs';
import { dependencyCheck } from './checks/dependency-check.mjs';
import { securityCheck } from './checks/security-check.mjs';
import { documentationCheck } from './checks/documentation-check.mjs';
import { performanceCheck } from './checks/performance-check.mjs';
import { accessibilityCheck } from './checks/accessibility-check.mjs';
import { compatibilityCheck } from './checks/compatibility-check.mjs';
import { blake3 } from 'hash-wasm';

/**
 * Check result schema
 */
export const CheckResultSchema = z.object({
  name: z.string(),
  category: z.string(),
  passed: z.boolean(),
  score: z.number().min(0).max(100),
  status: z.enum(['pass', 'warn', 'fail']),
  warnings: z.array(z.string()),
  failures: z.array(z.string()),
  remediation: z.array(z.string()),
  duration: z.number(),
  timestamp: z.string().datetime(),
  details: z.record(z.any()).optional()
});

/**
 * Category weight schema
 */
export const CategoryWeightSchema = z.object({
  codeQuality: z.number().default(20),
  testing: z.number().default(25),
  dependencies: z.number().default(15),
  security: z.number().default(20),
  documentation: z.number().default(5),
  performance: z.number().default(10),
  accessibility: z.number().default(2.5),
  compatibility: z.number().default(2.5)
});

/**
 * Validation config schema
 */
export const ValidationConfigSchema = z.object({
  weights: CategoryWeightSchema.optional(),
  thresholds: z.object({
    productionReady: z.number().default(95),
    warning: z.number().default(80),
    critical: z.number().default(60)
  }).optional(),
  skipChecks: z.array(z.string()).optional(),
  packageType: z.enum(['library', 'application', 'cli', 'web', 'api']).optional(),
  strict: z.boolean().default(true),
  timeout: z.number().default(30000),
  parallel: z.boolean().default(true)
});

/**
 * Production readiness receipt schema
 */
export const ProductionReceiptSchema = z.object({
  packageName: z.string(),
  packagePath: z.string(),
  version: z.string(),
  timestamp: z.string().datetime(),
  overallScore: z.number().min(0).max(100),
  productionReady: z.boolean(),
  checkResults: z.array(CheckResultSchema),
  categoryScores: z.record(z.number()),
  topBlockers: z.array(z.object({
    category: z.string(),
    issue: z.string(),
    severity: z.enum(['critical', 'high', 'medium', 'low']),
    remediation: z.string()
  })),
  exemptions: z.array(z.object({
    check: z.string(),
    reason: z.string(),
    approvedBy: z.string(),
    expiresAt: z.string().datetime().optional()
  })).optional(),
  receiptHash: z.string(),
  toolchainVersion: z.object({
    node: z.string(),
    validator: z.string()
  })
});

/**
 * Default category weights (must sum to 100)
 */
export const DEFAULT_WEIGHTS = {
  codeQuality: 20,
  testing: 25,
  dependencies: 15,
  security: 20,
  documentation: 5,
  performance: 10,
  accessibility: 2.5,
  compatibility: 2.5
};

/**
 * Default thresholds
 */
export const DEFAULT_THRESHOLDS = {
  productionReady: 95,
  warning: 80,
  critical: 60
};

/**
 * Validator version for receipt tracking
 */
export const VALIDATOR_VERSION = '1.0.0';

/**
 * Production Validator - Main orchestrator for production readiness checks
 *
 * @class ProductionValidator
 * @example
 * const validator = new ProductionValidator({ strict: true });
 * const receipt = await validator.validate('/path/to/package');
 * if (receipt.productionReady) {
 *   console.log('Package is production ready!');
 * } else {
 *   console.log('Blockers:', receipt.topBlockers);
 * }
 */
export class ProductionValidator {
  /**
   * Create a new Production Validator
   *
   * @param {Object} [config] - Validator configuration
   * @param {Object} [config.weights] - Category weights (must sum to 100)
   * @param {Object} [config.thresholds] - Score thresholds
   * @param {Array<string>} [config.skipChecks] - Checks to skip
   * @param {string} [config.packageType] - Package type hint
   * @param {boolean} [config.strict=true] - Strict mode
   * @param {number} [config.timeout=30000] - Timeout per check (ms)
   * @param {boolean} [config.parallel=true] - Run checks in parallel
   */
  constructor(config = {}) {
    const validated = ValidationConfigSchema.parse(config);
    this.config = {
      weights: { ...DEFAULT_WEIGHTS, ...validated.weights },
      thresholds: { ...DEFAULT_THRESHOLDS, ...validated.thresholds },
      skipChecks: validated.skipChecks || [],
      packageType: validated.packageType,
      strict: validated.strict,
      timeout: validated.timeout,
      parallel: validated.parallel
    };

    // Validate weights sum to 100
    const weightSum = Object.values(this.config.weights).reduce((a, b) => a + b, 0);
    if (Math.abs(weightSum - 100) > 0.01) {
      throw new Error(`Category weights must sum to 100, got ${weightSum}`);
    }

    // Initialize check registry
    this.checks = this._initializeChecks();

    // Statistics
    this.stats = {
      packagesValidated: 0,
      productionReady: 0,
      notReady: 0,
      averageScore: 0,
      totalDuration: 0
    };

    // Receipt history
    this.receiptHistory = [];
  }

  /**
   * Initialize all check modules
   *
   * @returns {Object} Check registry
   * @private
   */
  _initializeChecks() {
    return {
      codeQuality: {
        name: 'Code Quality',
        category: 'codeQuality',
        check: codeQualityCheck,
        weight: this.config.weights.codeQuality
      },
      testing: {
        name: 'Testing',
        category: 'testing',
        check: testCheck,
        weight: this.config.weights.testing
      },
      dependencies: {
        name: 'Dependencies',
        category: 'dependencies',
        check: dependencyCheck,
        weight: this.config.weights.dependencies
      },
      security: {
        name: 'Security',
        category: 'security',
        check: securityCheck,
        weight: this.config.weights.security
      },
      documentation: {
        name: 'Documentation',
        category: 'documentation',
        check: documentationCheck,
        weight: this.config.weights.documentation
      },
      performance: {
        name: 'Performance',
        category: 'performance',
        check: performanceCheck,
        weight: this.config.weights.performance
      },
      accessibility: {
        name: 'Accessibility',
        category: 'accessibility',
        check: accessibilityCheck,
        weight: this.config.weights.accessibility
      },
      compatibility: {
        name: 'Compatibility',
        category: 'compatibility',
        check: compatibilityCheck,
        weight: this.config.weights.compatibility
      }
    };
  }

  /**
   * Validate a package for production readiness
   *
   * @param {string} packagePath - Path to package directory
   * @param {Object} [options] - Validation options
   * @returns {Promise<Object>} Production readiness receipt
   */
  async validate(packagePath, options = {}) {
    const startTime = Date.now();
    const timestamp = new Date().toISOString();

    // Load package.json
    const packageInfo = await this._loadPackageInfo(packagePath);

    // Determine package type
    const packageType = options.packageType ||
      this.config.packageType ||
      this._inferPackageType(packageInfo);

    // Run all checks
    const checkResults = await this._runAllChecks(packagePath, {
      ...options,
      packageInfo,
      packageType
    });

    // Calculate category scores
    const categoryScores = this._calculateCategoryScores(checkResults);

    // Calculate overall score
    const overallScore = this._calculateOverallScore(categoryScores);

    // Determine production readiness
    const productionReady = overallScore >= this.config.thresholds.productionReady;

    // Identify top blockers
    const topBlockers = this._identifyBlockers(checkResults);

    // Generate receipt
    const receipt = await this._generateReceipt({
      packageName: packageInfo.name || 'unknown',
      packagePath,
      version: packageInfo.version || '0.0.0',
      timestamp,
      overallScore,
      productionReady,
      checkResults,
      categoryScores,
      topBlockers,
      duration: Date.now() - startTime
    });

    // Update stats
    this._updateStats(receipt);

    // Store in history
    this.receiptHistory.push(receipt);

    return receipt;
  }

  /**
   * Validate multiple packages
   *
   * @param {Array<string>} packagePaths - Paths to validate
   * @param {Object} [options] - Validation options
   * @returns {Promise<Object>} Batch validation results
   */
  async validateBatch(packagePaths, options = {}) {
    const results = [];
    const startTime = Date.now();

    for (const packagePath of packagePaths) {
      try {
        const receipt = await this.validate(packagePath, options);
        results.push(receipt);
      } catch (error) {
        results.push({
          packagePath,
          error: error.message,
          productionReady: false,
          overallScore: 0
        });
      }
    }

    const successfulResults = results.filter(r => !r.error);
    const averageScore = successfulResults.length > 0
      ? successfulResults.reduce((sum, r) => sum + r.overallScore, 0) / successfulResults.length
      : 0;

    return {
      timestamp: new Date().toISOString(),
      packagesValidated: packagePaths.length,
      packagesReady: results.filter(r => r.productionReady).length,
      packagesNotReady: results.filter(r => !r.productionReady).length,
      packagesErrored: results.filter(r => r.error).length,
      averageScore,
      duration: Date.now() - startTime,
      results
    };
  }

  /**
   * Run all enabled checks
   *
   * @param {string} packagePath - Package path
   * @param {Object} options - Check options
   * @returns {Promise<Array>} Check results
   * @private
   */
  async _runAllChecks(packagePath, options) {
    const results = [];
    const enabledChecks = Object.entries(this.checks)
      .filter(([key]) => !this.config.skipChecks.includes(key));

    if (this.config.parallel) {
      // Run checks in parallel
      const checkPromises = enabledChecks.map(async ([key, checkDef]) => {
        return this._runSingleCheck(checkDef, packagePath, options);
      });

      const parallelResults = await Promise.allSettled(checkPromises);

      for (let i = 0; i < parallelResults.length; i++) {
        const result = parallelResults[i];
        const [key, checkDef] = enabledChecks[i];

        if (result.status === 'fulfilled') {
          results.push(result.value);
        } else {
          results.push(this._createErrorResult(checkDef, result.reason));
        }
      }
    } else {
      // Run checks sequentially
      for (const [key, checkDef] of enabledChecks) {
        try {
          const result = await this._runSingleCheck(checkDef, packagePath, options);
          results.push(result);
        } catch (error) {
          results.push(this._createErrorResult(checkDef, error));
        }
      }
    }

    return results;
  }

  /**
   * Run a single check with timeout
   *
   * @param {Object} checkDef - Check definition
   * @param {string} packagePath - Package path
   * @param {Object} options - Check options
   * @returns {Promise<Object>} Check result
   * @private
   */
  async _runSingleCheck(checkDef, packagePath, options) {
    const startTime = Date.now();

    const timeoutPromise = new Promise((_, reject) => {
      setTimeout(() => reject(new Error('Check timeout')), this.config.timeout);
    });

    try {
      const result = await Promise.race([
        checkDef.check(packagePath, options),
        timeoutPromise
      ]);

      return {
        ...result,
        name: checkDef.name,
        category: checkDef.category,
        duration: Date.now() - startTime,
        timestamp: new Date().toISOString()
      };
    } catch (error) {
      throw error;
    }
  }

  /**
   * Create error result for failed check
   *
   * @param {Object} checkDef - Check definition
   * @param {Error} error - Error that occurred
   * @returns {Object} Error result
   * @private
   */
  _createErrorResult(checkDef, error) {
    return {
      name: checkDef.name,
      category: checkDef.category,
      passed: false,
      score: 0,
      status: 'fail',
      warnings: [],
      failures: [`Check failed: ${error.message}`],
      remediation: ['Fix check execution error and retry'],
      duration: 0,
      timestamp: new Date().toISOString(),
      details: { error: error.message }
    };
  }

  /**
   * Calculate category scores from check results
   *
   * @param {Array} checkResults - Check results
   * @returns {Object} Category scores
   * @private
   */
  _calculateCategoryScores(checkResults) {
    const categoryScores = {};

    for (const result of checkResults) {
      categoryScores[result.category] = result.score;
    }

    // Add zero scores for missing categories
    for (const category of Object.keys(this.checks)) {
      if (!(category in categoryScores)) {
        categoryScores[category] = 0;
      }
    }

    return categoryScores;
  }

  /**
   * Calculate overall weighted score
   *
   * @param {Object} categoryScores - Category scores
   * @returns {number} Overall score (0-100)
   * @private
   */
  _calculateOverallScore(categoryScores) {
    let totalWeight = 0;
    let weightedSum = 0;

    for (const [category, score] of Object.entries(categoryScores)) {
      const weight = this.config.weights[category] || 0;
      weightedSum += score * weight;
      totalWeight += weight;
    }

    if (totalWeight === 0) return 0;
    return Math.round((weightedSum / totalWeight) * 100) / 100;
  }

  /**
   * Identify top blockers from check results
   *
   * @param {Array} checkResults - Check results
   * @returns {Array} Top blockers
   * @private
   */
  _identifyBlockers(checkResults) {
    const blockers = [];

    for (const result of checkResults) {
      // Add failures as blockers
      for (const failure of result.failures) {
        blockers.push({
          category: result.category,
          issue: failure,
          severity: result.score < 50 ? 'critical' : 'high',
          remediation: result.remediation[0] || 'Address the issue'
        });
      }

      // Add warnings as medium severity
      for (const warning of result.warnings) {
        if (result.score < 80) {
          blockers.push({
            category: result.category,
            issue: warning,
            severity: 'medium',
            remediation: result.remediation[0] || 'Consider addressing'
          });
        }
      }
    }

    // Sort by severity and limit
    const severityOrder = { critical: 0, high: 1, medium: 2, low: 3 };
    blockers.sort((a, b) => severityOrder[a.severity] - severityOrder[b.severity]);

    return blockers.slice(0, 10);
  }

  /**
   * Load package.json info
   *
   * @param {string} packagePath - Package path
   * @returns {Promise<Object>} Package info
   * @private
   */
  async _loadPackageInfo(packagePath) {
    try {
      const { readFile } = await import('node:fs/promises');
      const { join } = await import('node:path');

      const packageJsonPath = join(packagePath, 'package.json');
      const content = await readFile(packageJsonPath, 'utf-8');
      return JSON.parse(content);
    } catch (error) {
      return { name: 'unknown', version: '0.0.0' };
    }
  }

  /**
   * Infer package type from package.json
   *
   * @param {Object} packageInfo - Package info
   * @returns {string} Package type
   * @private
   */
  _inferPackageType(packageInfo) {
    if (packageInfo.bin) return 'cli';
    if (packageInfo.dependencies?.['next'] || packageInfo.dependencies?.['react']) return 'web';
    if (packageInfo.dependencies?.['express'] || packageInfo.dependencies?.['fastify']) return 'api';
    if (packageInfo.main && !packageInfo.bin) return 'library';
    return 'library';
  }

  /**
   * Generate production readiness receipt
   *
   * @param {Object} data - Receipt data
   * @returns {Promise<Object>} Receipt with hash
   * @private
   */
  async _generateReceipt(data) {
    const receipt = {
      packageName: data.packageName,
      packagePath: data.packagePath,
      version: data.version,
      timestamp: data.timestamp,
      overallScore: data.overallScore,
      productionReady: data.productionReady,
      checkResults: data.checkResults,
      categoryScores: data.categoryScores,
      topBlockers: data.topBlockers,
      toolchainVersion: {
        node: process.version,
        validator: VALIDATOR_VERSION
      },
      exemptions: data.exemptions || []
    };

    // Compute receipt hash
    const canonical = JSON.stringify(receipt, Object.keys(receipt).sort());
    const receiptHash = await blake3(canonical);

    return {
      ...receipt,
      receiptHash
    };
  }

  /**
   * Update validator statistics
   *
   * @param {Object} receipt - Validation receipt
   * @private
   */
  _updateStats(receipt) {
    this.stats.packagesValidated++;
    if (receipt.productionReady) {
      this.stats.productionReady++;
    } else {
      this.stats.notReady++;
    }

    // Running average
    const n = this.stats.packagesValidated;
    this.stats.averageScore =
      ((n - 1) * this.stats.averageScore + receipt.overallScore) / n;

    this.stats.totalDuration += receipt.checkResults.reduce(
      (sum, r) => sum + (r.duration || 0), 0
    );
  }

  /**
   * Get validator statistics
   *
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      ...this.stats,
      readyRate: this.stats.packagesValidated > 0
        ? `${((this.stats.productionReady / this.stats.packagesValidated) * 100).toFixed(2)}%`
        : 'N/A'
    };
  }

  /**
   * Get receipt history
   *
   * @param {number} [limit=100] - Number of receipts to return
   * @returns {Array} Recent receipts
   */
  getReceiptHistory(limit = 100) {
    return this.receiptHistory.slice(-limit);
  }

  /**
   * Clear receipt history
   */
  clearHistory() {
    this.receiptHistory = [];
  }

  /**
   * Add exemption for a specific check
   *
   * @param {string} check - Check name to exempt
   * @param {Object} exemption - Exemption details
   * @returns {void}
   */
  addExemption(check, exemption) {
    if (!this.exemptions) {
      this.exemptions = [];
    }

    this.exemptions.push({
      check,
      reason: exemption.reason,
      approvedBy: exemption.approvedBy,
      expiresAt: exemption.expiresAt
    });
  }

  /**
   * Generate production readiness report
   *
   * @param {Object} receipt - Validation receipt
   * @returns {string} Formatted report
   */
  generateReport(receipt) {
    const lines = [
      '# Production Readiness Report',
      '',
      `**Package**: ${receipt.packageName}@${receipt.version}`,
      `**Path**: ${receipt.packagePath}`,
      `**Validated**: ${receipt.timestamp}`,
      '',
      '## Summary',
      '',
      `- **Overall Score**: ${receipt.overallScore}/100`,
      `- **Status**: ${receipt.productionReady ? 'PRODUCTION READY' : 'NOT READY'}`,
      `- **Receipt Hash**: ${receipt.receiptHash.substring(0, 16)}...`,
      ''
    ];

    // Category breakdown
    lines.push('## Category Scores');
    lines.push('');
    lines.push('| Category | Score | Status |');
    lines.push('|----------|-------|--------|');

    for (const [category, score] of Object.entries(receipt.categoryScores)) {
      const status = score >= 95 ? 'PASS' : score >= 80 ? 'WARN' : 'FAIL';
      lines.push(`| ${category} | ${score}/100 | ${status} |`);
    }

    lines.push('');

    // Top blockers
    if (receipt.topBlockers.length > 0) {
      lines.push('## Top Blockers');
      lines.push('');

      for (const blocker of receipt.topBlockers) {
        lines.push(`### [${blocker.severity.toUpperCase()}] ${blocker.category}`);
        lines.push('');
        lines.push(`**Issue**: ${blocker.issue}`);
        lines.push(`**Remediation**: ${blocker.remediation}`);
        lines.push('');
      }
    } else {
      lines.push('## No Blockers');
      lines.push('');
      lines.push('All checks passed with no significant issues.');
      lines.push('');
    }

    // Check details
    lines.push('## Check Details');
    lines.push('');

    for (const result of receipt.checkResults) {
      lines.push(`### ${result.name}`);
      lines.push('');
      lines.push(`- **Score**: ${result.score}/100`);
      lines.push(`- **Status**: ${result.status.toUpperCase()}`);
      lines.push(`- **Duration**: ${result.duration}ms`);
      lines.push('');

      if (result.failures.length > 0) {
        lines.push('**Failures**:');
        for (const failure of result.failures) {
          lines.push(`- ${failure}`);
        }
        lines.push('');
      }

      if (result.warnings.length > 0) {
        lines.push('**Warnings**:');
        for (const warning of result.warnings) {
          lines.push(`- ${warning}`);
        }
        lines.push('');
      }

      if (result.remediation.length > 0) {
        lines.push('**Remediation Steps**:');
        for (const step of result.remediation) {
          lines.push(`1. ${step}`);
        }
        lines.push('');
      }
    }

    return lines.join('\n');
  }
}

/**
 * Create a production validator with default config
 *
 * @param {Object} [config] - Optional configuration
 * @returns {ProductionValidator} Validator instance
 */
export function createProductionValidator(config = {}) {
  return new ProductionValidator(config);
}

/**
 * Validate a single package with default settings
 *
 * @param {string} packagePath - Package path
 * @param {Object} [options] - Validation options
 * @returns {Promise<Object>} Validation receipt
 */
export async function validatePackage(packagePath, options = {}) {
  const validator = new ProductionValidator(options);
  return validator.validate(packagePath, options);
}

/**
 * Validate multiple packages with default settings
 *
 * @param {Array<string>} packagePaths - Package paths
 * @param {Object} [options] - Validation options
 * @returns {Promise<Object>} Batch validation results
 */
export async function validatePackages(packagePaths, options = {}) {
  const validator = new ProductionValidator(options);
  return validator.validateBatch(packagePaths, options);
}

/**
 * Quick check if a package is production ready
 *
 * @param {string} packagePath - Package path
 * @param {Object} [options] - Validation options
 * @returns {Promise<boolean>} True if production ready
 */
export async function isProductionReady(packagePath, options = {}) {
  const validator = new ProductionValidator(options);
  const receipt = await validator.validate(packagePath, options);
  return receipt.productionReady;
}
