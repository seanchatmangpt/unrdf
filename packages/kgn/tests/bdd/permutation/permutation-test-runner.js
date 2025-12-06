/**
 * Permutation Test Runner for Byte-Identical Verification
 * Ensures deterministic output across all input variations
 */
import crypto from 'crypto';
import { TemplateEngine } from '../../../src/engine/template-engine.js';
import { createTestDataFactory } from '../fixtures/test-data-factory.js';
import { GoldenTestValidator } from '../golden/golden-validator.js';

export class PermutationTestRunner {
  constructor(options = {}) {
    this.templateEngine = new TemplateEngine(options.engineConfig);
    this.goldenValidator = new GoldenTestValidator(options.goldenConfig);
    this.iterations = options.iterations || 10;
    this.parallelRuns = options.parallelRuns || 5;
    this.tolerance = options.tolerance || 0; // 0 for byte-identical
    this.results = new Map();
  }

  /**
   * Run permutation tests for template determinism
   */
  async runPermutationTests(testCases) {
    const results = {};

    for (const [testName, testCase] of Object.entries(testCases)) {
      console.log(`Running permutation test: ${testName}`);
      results[testName] = await this.runSinglePermutationTest(testCase);
    }

    return this.generatePermutationReport(results);
  }

  /**
   * Run a single permutation test case
   */
  async runSinglePermutationTest(testCase) {
    const { template, dataVariations, options = {} } = testCase;
    const permutationResults = {};

    // Test each data variation
    for (const [variationName, data] of Object.entries(dataVariations)) {
      const variationResult = await this.testDataVariation(
        template,
        data,
        variationName,
        options
      );
      permutationResults[variationName] = variationResult;
    }

    // Test determinism across multiple runs
    const determinismResult = await this.testDeterminism(
      template,
      dataVariations.base || Object.values(dataVariations)[0],
      options
    );

    return {
      permutations: permutationResults,
      determinism: determinismResult,
      summary: this.summarizePermutationResults(permutationResults, determinismResult)
    };
  }

  /**
   * Test a specific data variation
   */
  async testDataVariation(template, data, variationName, options) {
    const renders = [];
    const startTime = Date.now();

    // Perform multiple renders
    for (let i = 0; i < this.iterations; i++) {
      try {
        const output = await this.templateEngine.render(template, data, options);
        const hash = this.generateContentHash(output);

        renders.push({
          iteration: i,
          output,
          hash,
          timestamp: Date.now(),
          size: Buffer.byteLength(output, 'utf8')
        });
      } catch (error) {
        renders.push({
          iteration: i,
          error: error.message,
          timestamp: Date.now()
        });
      }
    }

    const endTime = Date.now();

    return this.analyzeVariationResults(renders, {
      variationName,
      totalTime: endTime - startTime,
      template,
      data
    });
  }

  /**
   * Test determinism across parallel executions
   */
  async testDeterminism(template, data, options) {
    const promises = [];

    // Create parallel render promises
    for (let i = 0; i < this.parallelRuns; i++) {
      promises.push(
        this.executeParallelRender(template, data, options, i)
      );
    }

    const parallelResults = await Promise.all(promises);

    return this.analyzeDeterminismResults(parallelResults);
  }

  /**
   * Execute a single parallel render
   */
  async executeParallelRender(template, data, options, runId) {
    const startTime = process.hrtime.bigint();

    try {
      const output = await this.templateEngine.render(template, data, options);
      const endTime = process.hrtime.bigint();

      return {
        runId,
        output,
        hash: this.generateContentHash(output),
        executionTime: Number(endTime - startTime) / 1000000, // Convert to ms
        success: true,
        size: Buffer.byteLength(output, 'utf8')
      };
    } catch (error) {
      const endTime = process.hrtime.bigint();

      return {
        runId,
        error: error.message,
        executionTime: Number(endTime - startTime) / 1000000,
        success: false
      };
    }
  }

  /**
   * Analyze variation results for consistency
   */
  analyzeVariationResults(renders, metadata) {
    const successfulRenders = renders.filter(r => !r.error);
    const failedRenders = renders.filter(r => r.error);

    if (successfulRenders.length === 0) {
      return {
        success: false,
        errorRate: 1.0,
        errors: failedRenders.map(r => r.error),
        metadata
      };
    }

    // Check hash consistency
    const uniqueHashes = [...new Set(successfulRenders.map(r => r.hash))];
    const isDeterministic = uniqueHashes.length === 1;

    // Calculate statistics
    const executionTimes = successfulRenders.map(r => r.timestamp - metadata.startTime || 0);
    const sizes = successfulRenders.map(r => r.size);

    return {
      success: true,
      deterministic: isDeterministic,
      uniqueHashes: uniqueHashes.length,
      primaryHash: uniqueHashes[0],
      iterations: successfulRenders.length,
      errorRate: failedRenders.length / renders.length,
      statistics: {
        averageExecutionTime: this.average(executionTimes),
        minExecutionTime: Math.min(...executionTimes),
        maxExecutionTime: Math.max(...executionTimes),
        averageSize: this.average(sizes),
        sizeVariation: Math.max(...sizes) - Math.min(...sizes)
      },
      errors: failedRenders.map(r => r.error),
      metadata
    };
  }

  /**
   * Analyze determinism across parallel executions
   */
  analyzeDeterminismResults(parallelResults) {
    const successfulResults = parallelResults.filter(r => r.success);
    const failedResults = parallelResults.filter(r => !r.success);

    if (successfulResults.length === 0) {
      return {
        deterministic: false,
        parallelConsistency: false,
        errorRate: 1.0,
        errors: failedResults.map(r => r.error)
      };
    }

    // Check hash consistency across parallel runs
    const uniqueHashes = [...new Set(successfulResults.map(r => r.hash))];
    const isDeterministic = uniqueHashes.length === 1;

    // Check execution time consistency
    const executionTimes = successfulResults.map(r => r.executionTime);
    const timeVariance = this.variance(executionTimes);
    const timeStandardDeviation = Math.sqrt(timeVariance);

    return {
      deterministic: isDeterministic,
      parallelConsistency: true,
      uniqueHashes: uniqueHashes.length,
      primaryHash: uniqueHashes[0],
      parallelRuns: successfulResults.length,
      errorRate: failedResults.length / parallelResults.length,
      performance: {
        averageExecutionTime: this.average(executionTimes),
        executionTimeVariance: timeVariance,
        executionTimeStdDev: timeStandardDeviation,
        minExecutionTime: Math.min(...executionTimes),
        maxExecutionTime: Math.max(...executionTimes)
      },
      errors: failedResults.map(r => r.error)
    };
  }

  /**
   * Generate comprehensive permutation report
   */
  generatePermutationReport(allResults) {
    const totalTests = Object.keys(allResults).length;
    const successfulTests = Object.values(allResults).filter(r =>
      r.summary.success && r.summary.deterministic
    ).length;

    const report = {
      summary: {
        totalTests,
        successfulTests,
        deterministicTests: successfulTests,
        successRate: successfulTests / totalTests,
        timestamp: new Date().toISOString(),
        testConfiguration: {
          iterations: this.iterations,
          parallelRuns: this.parallelRuns,
          tolerance: this.tolerance
        }
      },
      testResults: allResults,
      recommendations: this.generateRecommendations(allResults)
    };

    return report;
  }

  /**
   * Summarize permutation results
   */
  summarizePermutationResults(permutationResults, determinismResult) {
    const permutationCount = Object.keys(permutationResults).length;
    const deterministicPermutations = Object.values(permutationResults)
      .filter(r => r.deterministic).length;

    return {
      success: deterministicPermutations === permutationCount && determinismResult.deterministic,
      deterministic: determinismResult.deterministic,
      permutationCount,
      deterministicPermutations,
      parallelConsistency: determinismResult.parallelConsistency,
      overallErrorRate: this.calculateOverallErrorRate(permutationResults, determinismResult),
      primaryHash: determinismResult.primaryHash
    };
  }

  /**
   * Calculate overall error rate
   */
  calculateOverallErrorRate(permutationResults, determinismResult) {
    const permutationErrors = Object.values(permutationResults)
      .reduce((sum, result) => sum + result.errorRate, 0);

    const avgPermutationErrorRate = permutationErrors / Object.keys(permutationResults).length;
    const determinismErrorRate = determinismResult.errorRate || 0;

    return (avgPermutationErrorRate + determinismErrorRate) / 2;
  }

  /**
   * Generate recommendations based on test results
   */
  generateRecommendations(allResults) {
    const recommendations = [];

    Object.entries(allResults).forEach(([testName, result]) => {
      if (!result.summary.deterministic) {
        recommendations.push({
          test: testName,
          severity: 'high',
          issue: 'Non-deterministic output detected',
          suggestion: 'Review template for random values, timestamps, or UUID generation'
        });
      }

      if (result.summary.overallErrorRate > 0.1) {
        recommendations.push({
          test: testName,
          severity: 'medium',
          issue: `High error rate: ${(result.summary.overallErrorRate * 100).toFixed(1)}%`,
          suggestion: 'Investigate template syntax and data compatibility'
        });
      }

      if (!result.summary.parallelConsistency) {
        recommendations.push({
          test: testName,
          severity: 'medium',
          issue: 'Parallel execution inconsistency',
          suggestion: 'Check for race conditions or shared state issues'
        });
      }
    });

    return recommendations;
  }

  /**
   * Generate content hash for comparison
   */
  generateContentHash(content) {
    return crypto.createHash('sha256').update(content, 'utf8').digest('hex');
  }

  /**
   * Calculate average of array
   */
  average(arr) {
    return arr.length > 0 ? arr.reduce((sum, val) => sum + val, 0) / arr.length : 0;
  }

  /**
   * Calculate variance of array
   */
  variance(arr) {
    if (arr.length === 0) return 0;
    const avg = this.average(arr);
    return arr.reduce((sum, val) => sum + Math.pow(val - avg, 2), 0) / arr.length;
  }

  /**
   * Export results for analysis
   */
  async exportResults(results, outputPath) {
    const exportData = {
      ...results,
      exportedAt: new Date().toISOString(),
      nodeVersion: process.version,
      platform: process.platform
    };

    if (outputPath) {
      await fs.writeFile(outputPath, JSON.stringify(exportData, null, 2));
    }

    return exportData;
  }
}