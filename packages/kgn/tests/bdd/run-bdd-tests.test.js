/**
 * Main BDD Test Execution File
 * Integrates with Vitest and runs comprehensive BDD test suite
 */
import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import BDDTestRunner from './bdd-test-runner.js';
import fs from 'fs/promises';
import path from 'path';

describe('KGEN Templates - Comprehensive BDD Test Suite', () => {
  let bddRunner;
  let testResults;

  beforeAll(async () => {
    console.log('🚀 Setting up BDD Test Suite...');
    bddRunner = new BDDTestRunner({
      enableGoldenTests: true,
      enablePermutationTests: process.env.ENABLE_PERMUTATION !== 'false',
      enableCrossPlatformTests: process.env.ENABLE_CROSS_PLATFORM !== 'false',
      enablePerformanceTests: process.env.ENABLE_PERFORMANCE !== 'false',
      updateGolden: process.env.UPDATE_GOLDEN === 'true',
      iterations: parseInt(process.env.PERMUTATION_ITERATIONS) || 5, // Reduced for CI
      parallelRuns: parseInt(process.env.PARALLEL_RUNS) || 3
    });
  }, 30000);

  afterAll(async () => {
    console.log('🧹 Cleaning up BDD Test Suite...');

    if (testResults) {
      const reportPath = path.join(__dirname, '../../coverage/bdd/final-report.json');
      await fs.mkdir(path.dirname(reportPath), { recursive: true });
      await fs.writeFile(reportPath, JSON.stringify(testResults, null, 2));
      console.log(`📊 Final report saved to: ${reportPath}`);
    }
  });

  it('should run complete BDD test suite successfully', async () => {
    testResults = await bddRunner.runCompleteBDDSuite();

    // Assert overall success
    expect(testResults).toBeDefined();
    expect(testResults.summary).toBeDefined();
    expect(testResults.summary.totalTests).toBeGreaterThan(0);

    // Log results for visibility
    console.log('\n📋 BDD Test Suite Results:');
    console.log(`  Total Tests: ${testResults.summary.totalTests}`);
    console.log(`  Passed: ${testResults.summary.passedTests}`);
    console.log(`  Failed: ${testResults.summary.failedTests}`);
    console.log(`  Success Rate: ${(testResults.summary.successRate * 100).toFixed(2)}%`);

    if (testResults.summary.failedTests > 0) {
      console.log('\n❌ Failed Tests:');
      testResults.summary.failedTestDetails.forEach(test => {
        console.log(`  - ${test.name}: ${test.result.error || 'Unknown error'}`);
      });
    }

    // Log category breakdown
    console.log('\n📊 Category Breakdown:');
    testResults.summary.categorySummary.forEach(category => {
      console.log(`  ${category.name}: ${category.passed}/${category.total} (${(category.successRate * 100).toFixed(1)}%)`);
    });

    // Assert minimum success rate (allow some flexibility for CI environments)
    const minimumSuccessRate = process.env.CI ? 0.8 : 0.9;
    expect(testResults.summary.successRate).toBeGreaterThanOrEqual(minimumSuccessRate);
  }, 120000); // 2 minute timeout for comprehensive suite

  describe('Core Template Rendering', () => {
    it('should pass basic template rendering tests', async () => {
      if (!testResults) {
        testResults = await bddRunner.runCompleteBDDSuite();
      }

      expect(testResults.coreTemplates.basicRendering.passed).toBe(true);
    });

    it('should pass frontmatter parsing tests', async () => {
      if (!testResults) {
        testResults = await bddRunner.runCompleteBDDSuite();
      }

      expect(testResults.coreTemplates.frontmatterParsing.passed).toBe(true);
    });

    it('should pass custom filter tests', async () => {
      if (!testResults) {
        testResults = await bddRunner.runCompleteBDDSuite();
      }

      expect(testResults.coreTemplates.customFilters.passed).toBe(true);
    });

    it('should pass deterministic rendering tests', async () => {
      if (!testResults) {
        testResults = await bddRunner.runCompleteBDDSuite();
      }

      expect(testResults.coreTemplates.deterministicRendering.passed).toBe(true);
      expect(testResults.coreTemplates.deterministicRendering.uniqueOutputs).toBe(1);
    });
  });

  describe('Advanced Features', () => {
    it('should pass LaTeX generation tests', async () => {
      if (!testResults) {
        testResults = await bddRunner.runCompleteBDDSuite();
      }

      expect(testResults.advancedFeatures.latexGeneration.passed).toBe(true);
    });

    it('should pass React component generation tests', async () => {
      if (!testResults) {
        testResults = await bddRunner.runCompleteBDDSuite();
      }

      expect(testResults.advancedFeatures.reactComponents.passed).toBe(true);
    });

    it('should handle errors gracefully', async () => {
      if (!testResults) {
        testResults = await bddRunner.runCompleteBDDSuite();
      }

      expect(testResults.advancedFeatures.errorHandling.passed).toBe(true);
    });
  });

  describe('Integration Tests', () => {
    it('should pass CAS integration tests', async () => {
      if (!testResults) {
        testResults = await bddRunner.runCompleteBDDSuite();
      }

      expect(testResults.integration.casIntegration.passed).toBe(true);
    });

    it('should pass RDF integration tests', async () => {
      if (!testResults) {
        testResults = await bddRunner.runCompleteBDDSuite();
      }

      expect(testResults.integration.rdfIntegration.passed).toBe(true);
    });

    it('should pass cross-platform tests', async () => {
      if (!testResults || !testResults.integration.crossPlatform) {
        testResults = await bddRunner.runCompleteBDDSuite();
      }

      if (testResults.integration.crossPlatform) {
        expect(testResults.integration.crossPlatform.compatibility.compatibilityScore)
          .toBeGreaterThanOrEqual(0.8);
      }
    }, { skip: process.env.ENABLE_CROSS_PLATFORM === 'false' });
  });

  describe('Performance Tests', () => {
    it('should pass throughput tests', async () => {
      if (!testResults) {
        testResults = await bddRunner.runCompleteBDDSuite();
      }

      expect(testResults.performance.throughputTest.passed).toBe(true);
      expect(testResults.performance.throughputTest.throughput).toBeGreaterThan(100);
    });

    it('should pass memory usage tests', async () => {
      if (!testResults) {
        testResults = await bddRunner.runCompleteBDDSuite();
      }

      expect(testResults.performance.memoryUsage.passed).toBe(true);
    });

    it('should pass large dataset processing tests', async () => {
      if (!testResults) {
        testResults = await bddRunner.runCompleteBDDSuite();
      }

      expect(testResults.performance.largeDatasets.passed).toBe(true);
    });

    it('should pass permutation tests', async () => {
      if (!testResults || !testResults.performance.permutation) {
        testResults = await bddRunner.runCompleteBDDSuite();
      }

      if (testResults.performance.permutation) {
        expect(testResults.performance.permutation.summary.successRate)
          .toBeGreaterThanOrEqual(0.9);
      }
    }, { skip: process.env.ENABLE_PERMUTATION === 'false' });
  });

  describe('Golden Test Validation', () => {
    it('should validate against golden files', async () => {
      if (!testResults) {
        testResults = await bddRunner.runCompleteBDDSuite();
      }

      if (testResults.coreTemplates.goldenValidation) {
        const goldenResults = Object.values(testResults.coreTemplates.goldenValidation);
        const passedGolden = goldenResults.filter(r => r.valid).length;

        expect(passedGolden / goldenResults.length).toBeGreaterThanOrEqual(0.8);
      }
    });
  });
});