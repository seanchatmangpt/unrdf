/**
 * @file Testing and Quality Assurance Tests
 * 
 * Tests for coverage gaps, integration test failures, performance test limitations,
 * security test coverage, and user acceptance testing.
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { KnowledgeHookManager } from '../../../src/knowledge-engine/knowledge-hook-manager.mjs';
import { defineHook } from '../../../src/knowledge-engine/define-hook.mjs';
import { Store } from 'n3';
import { promises as fs } from 'fs';
import { join } from 'path';

// Mock condition evaluator at module level
vi.mock('../../../src/knowledge-engine/condition-evaluator.mjs', () => ({
  evaluateCondition: vi.fn(),
  createConditionEvaluator: vi.fn().mockReturnValue({
    evaluate: vi.fn()
  }),
  validateCondition: vi.fn().mockReturnValue({ valid: true })
}));

describe('Testing and Quality Assurance', () => {
  let manager;
  let tempDir;
  let testCoverageIssues;
  let integrationTestResults;
  let performanceTestResults;
  let securityTestResults;
  let uatResults;

  beforeEach(async () => {
    // Create temporary directory for test files
    tempDir = join(process.cwd(), 'test-temp-testing-qa');
    await fs.mkdir(tempDir, { recursive: true });
    
    // Initialize tracking arrays
    testCoverageIssues = [];
    integrationTestResults = [];
    performanceTestResults = [];
    securityTestResults = [];
    uatResults = [];
    
    manager = new KnowledgeHookManager({
      basePath: tempDir,
      enableKnowledgeHooks: true
    });

    // Configure mock to always return true for conditions
    const { evaluateCondition } = await import('../../../src/knowledge-engine/condition-evaluator.mjs');
    vi.mocked(evaluateCondition).mockResolvedValue(true);
  });

  afterEach(async () => {
    // Clean up temporary directory
    await fs.rm(tempDir, { recursive: true, force: true });
  });

  describe('Test Coverage Gaps', () => {
    it('should detect test coverage gaps and missing test cases', async () => {
      const coverageHook = defineHook({
        meta: {
          name: 'testing:coverage-gaps',
          description: 'Detect test coverage gaps and missing test cases'
        },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: 'file://hooks/testing/coverage-gaps.ask.rq',
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3'
          }
        },
        run: async (event) => {
          // Simulate test coverage analysis
          const coverage = event.payload.coverage || {};
          const files = event.payload.files || [];
          const threshold = event.payload.threshold || 80;
          
          let coverageGaps = [];
          
          files.forEach(file => {
            const fileCoverage = coverage[file.name] || 0;
            
            if (fileCoverage < threshold) {
              coverageGaps.push({
                file: file.name,
                coverage: fileCoverage,
                threshold,
                gap: threshold - fileCoverage,
                severity: fileCoverage < 50 ? 'HIGH' : fileCoverage < 70 ? 'MEDIUM' : 'LOW'
              });
            }
            
            // Check for uncovered lines
            const uncoveredLines = file.uncoveredLines || [];
            if (uncoveredLines.length > 0) {
              coverageGaps.push({
                file: file.name,
                type: 'UNCOVERED_LINES',
                lines: uncoveredLines,
                count: uncoveredLines.length,
                severity: 'MEDIUM'
              });
            }
            
            // Check for missing edge cases
            const missingEdgeCases = file.missingEdgeCases || [];
            if (missingEdgeCases.length > 0) {
              coverageGaps.push({
                file: file.name,
                type: 'MISSING_EDGE_CASES',
                cases: missingEdgeCases,
                count: missingEdgeCases.length,
                severity: 'HIGH'
              });
            }
          });
          
          if (coverageGaps.length > 0) {
            testCoverageIssues.push(...coverageGaps);
          }
          
          return {
            result: { 
              totalFiles: files.length,
              coverageGaps: coverageGaps.length,
              averageCoverage: Object.values(coverage).reduce((a, b) => a + b, 0) / Object.keys(coverage).length,
              threshold
            },
            status: 'coverage-analysis-complete'
          };
        }
      });

      manager.addKnowledgeHook(coverageHook);

      const event = {
        name: 'coverage-analysis',
        payload: {
          coverage: {
            'user-service.js': 85,
            'payment-service.js': 45,
            'auth-service.js': 70
          },
          files: [
            { 
              name: 'user-service.js', 
              uncoveredLines: [45, 67],
              missingEdgeCases: ['invalid email format']
            },
            { 
              name: 'payment-service.js', 
              uncoveredLines: [12, 34, 56, 78],
              missingEdgeCases: ['network timeout', 'invalid card', 'insufficient funds']
            },
            { 
              name: 'auth-service.js', 
              uncoveredLines: [23],
              missingEdgeCases: ['expired token']
            }
          ],
          threshold: 80
        },
        context: {
          graph: new Store(),
          timestamp: new Date()
        }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      
      // Should detect coverage gaps
      expect(results[0].success).toBe(true);
      expect(results[0].runResult?.result?.totalFiles).toBe(3);
      expect(results[0].runResult?.result?.coverageGaps).toBe(6);
      expect(results[0].runResult?.result?.averageCoverage).toBe(66.67);
      expect(testCoverageIssues.length).toBe(6);
    });
  });

  describe('Integration Test Failures', () => {
    it('should detect and analyze integration test failures', async () => {
      const integrationHook = defineHook({
        meta: {
          name: 'testing:integration-failures',
          description: 'Detect and analyze integration test failures'
        },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: 'file://hooks/testing/integration-failures.ask.rq',
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3'
          }
        },
        run: async (event) => {
          // Simulate integration test analysis
          const tests = event.payload.tests || [];
          const services = event.payload.services || [];
          
          let failures = [];
          let serviceIssues = [];
          
          tests.forEach(test => {
            if (test.status === 'FAILED') {
              failures.push({
                test: test.name,
                error: test.error,
                duration: test.duration,
                services: test.services,
                failureType: test.failureType || 'UNKNOWN'
              });
              
              // Analyze service-specific issues
              test.services.forEach(serviceName => {
                const service = services.find(s => s.name === serviceName);
                if (service && service.status !== 'HEALTHY') {
                  serviceIssues.push({
                    service: serviceName,
                    status: service.status,
                    error: service.error,
                    test: test.name
                  });
                }
              });
            }
          });
          
          if (failures.length > 0) {
            integrationTestResults.push(...failures);
          }
          
          if (serviceIssues.length > 0) {
            integrationTestResults.push(...serviceIssues);
          }
          
          return {
            result: { 
              totalTests: tests.length,
              failedTests: failures.length,
              serviceIssues: serviceIssues.length,
              failureRate: (failures.length / tests.length) * 100
            },
            status: 'integration-analysis-complete'
          };
        }
      });

      manager.addKnowledgeHook(integrationHook);

      const event = {
        name: 'integration-analysis',
        payload: {
          tests: [
            {
              name: 'user-payment-flow',
              status: 'FAILED',
              error: 'Payment service timeout',
              duration: 5000,
              services: ['user-service', 'payment-service'],
              failureType: 'TIMEOUT'
            },
            {
              name: 'auth-integration',
              status: 'PASSED',
              duration: 1200,
              services: ['auth-service', 'user-service']
            },
            {
              name: 'order-processing',
              status: 'FAILED',
              error: 'Database connection failed',
              duration: 3000,
              services: ['order-service', 'database'],
              failureType: 'CONNECTION_ERROR'
            }
          ],
          services: [
            { name: 'user-service', status: 'HEALTHY' },
            { name: 'payment-service', status: 'UNHEALTHY', error: 'High latency' },
            { name: 'auth-service', status: 'HEALTHY' },
            { name: 'order-service', status: 'HEALTHY' },
            { name: 'database', status: 'UNHEALTHY', error: 'Connection pool exhausted' }
          ]
        },
        context: {
          graph: new Store(),
          timestamp: new Date()
        }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      
      // Should detect integration test failures
      expect(results[0].success).toBe(true);
      expect(results[0].runResult?.result?.totalTests).toBe(3);
      expect(results[0].runResult?.result?.failedTests).toBe(2);
      expect(results[0].runResult?.result?.serviceIssues).toBe(2);
      expect(results[0].runResult?.result?.failureRate).toBe(66.67);
      expect(integrationTestResults.length).toBe(4);
    });
  });

  describe('Performance Test Limitations', () => {
    it('should identify performance test limitations and bottlenecks', async () => {
      const performanceHook = defineHook({
        meta: {
          name: 'testing:performance-limitations',
          description: 'Identify performance test limitations and bottlenecks'
        },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: 'file://hooks/testing/performance-limitations.ask.rq',
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3'
          }
        },
        run: async (event) => {
          // Simulate performance test analysis
          const tests = event.payload.tests || [];
          const metrics = event.payload.metrics || {};
          const thresholds = event.payload.thresholds || {};
          
          let limitations = [];
          let bottlenecks = [];
          
          tests.forEach(test => {
            const responseTime = test.responseTime || 0;
            const throughput = test.throughput || 0;
            const errorRate = test.errorRate || 0;
            
            // Check response time thresholds
            if (responseTime > thresholds.responseTime) {
              limitations.push({
                test: test.name,
                metric: 'RESPONSE_TIME',
                value: responseTime,
                threshold: thresholds.responseTime,
                severity: 'HIGH'
              });
            }
            
            // Check throughput thresholds
            if (throughput < thresholds.throughput) {
              limitations.push({
                test: test.name,
                metric: 'THROUGHPUT',
                value: throughput,
                threshold: thresholds.throughput,
                severity: 'MEDIUM'
              });
            }
            
            // Check error rate thresholds
            if (errorRate > thresholds.errorRate) {
              limitations.push({
                test: test.name,
                metric: 'ERROR_RATE',
                value: errorRate,
                threshold: thresholds.errorRate,
                severity: 'CRITICAL'
              });
            }
            
            // Identify bottlenecks
            if (test.bottlenecks) {
              test.bottlenecks.forEach(bottleneck => {
                bottlenecks.push({
                  test: test.name,
                  component: bottleneck.component,
                  type: bottleneck.type,
                  impact: bottleneck.impact,
                  recommendation: bottleneck.recommendation
                });
              });
            }
          });
          
          if (limitations.length > 0) {
            performanceTestResults.push(...limitations);
          }
          
          if (bottlenecks.length > 0) {
            performanceTestResults.push(...bottlenecks);
          }
          
          return {
            result: { 
              totalTests: tests.length,
              limitations: limitations.length,
              bottlenecks: bottlenecks.length,
              overallPerformance: limitations.length === 0 ? 'PASS' : 'FAIL'
            },
            status: 'performance-analysis-complete'
          };
        }
      });

      manager.addKnowledgeHook(performanceHook);

      const event = {
        name: 'performance-analysis',
        payload: {
          tests: [
            {
              name: 'user-login',
              responseTime: 2500,
              throughput: 100,
              errorRate: 0.05,
              bottlenecks: [
                {
                  component: 'database',
                  type: 'QUERY_SLOW',
                  impact: 'HIGH',
                  recommendation: 'Add database indexes'
                }
              ]
            },
            {
              name: 'payment-processing',
              responseTime: 800,
              throughput: 50,
              errorRate: 0.02,
              bottlenecks: [
                {
                  component: 'external-api',
                  type: 'NETWORK_LATENCY',
                  impact: 'MEDIUM',
                  recommendation: 'Implement caching'
                }
              ]
            }
          ],
          thresholds: {
            responseTime: 1000,
            throughput: 200,
            errorRate: 0.01
          }
        },
        context: {
          graph: new Store(),
          timestamp: new Date()
        }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      
      // Should detect performance limitations
      expect(results[0].success).toBe(true);
      expect(results[0].runResult?.result?.totalTests).toBe(2);
      expect(results[0].runResult?.result?.limitations).toBe(3);
      expect(results[0].runResult?.result?.bottlenecks).toBe(2);
      expect(results[0].runResult?.result?.overallPerformance).toBe('FAIL');
      expect(performanceTestResults.length).toBe(5);
    });
  });

  describe('Security Test Coverage', () => {
    it('should assess security test coverage and identify gaps', async () => {
      const securityHook = defineHook({
        meta: {
          name: 'testing:security-coverage',
          description: 'Assess security test coverage and identify gaps'
        },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: 'file://hooks/testing/security-coverage.ask.rq',
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3'
          }
        },
        run: async (event) => {
          // Simulate security test analysis
          const tests = event.payload.tests || [];
          const vulnerabilities = event.payload.vulnerabilities || [];
          const coverage = event.payload.coverage || {};
          
          let securityGaps = [];
          let testFailures = [];
          
          // Check for missing security test categories
          const requiredCategories = ['AUTHENTICATION', 'AUTHORIZATION', 'INPUT_VALIDATION', 'ENCRYPTION', 'SESSION_MANAGEMENT'];
          const coveredCategories = tests.map(t => t.category).filter(Boolean);
          
          requiredCategories.forEach(category => {
            if (!coveredCategories.includes(category)) {
              securityGaps.push({
                type: 'MISSING_CATEGORY',
                category,
                severity: 'HIGH',
                description: `No tests for ${category} security`
              });
            }
          });
          
          // Analyze test results
          tests.forEach(test => {
            if (test.status === 'FAILED') {
              testFailures.push({
                test: test.name,
                category: test.category,
                vulnerability: test.vulnerability,
                severity: test.severity,
                description: test.description
              });
            }
          });
          
          // Check for known vulnerabilities not covered by tests
          vulnerabilities.forEach(vuln => {
            const hasTest = tests.some(t => t.vulnerability === vuln.type);
            if (!hasTest) {
              securityGaps.push({
                type: 'UNCOVERED_VULNERABILITY',
                vulnerability: vuln.type,
                severity: vuln.severity,
                description: `No test coverage for ${vuln.type} vulnerability`
              });
            }
          });
          
          if (securityGaps.length > 0) {
            securityTestResults.push(...securityGaps);
          }
          
          if (testFailures.length > 0) {
            securityTestResults.push(...testFailures);
          }
          
          return {
            result: { 
              totalTests: tests.length,
              securityGaps: securityGaps.length,
              testFailures: testFailures.length,
              coverageScore: (coveredCategories.length / requiredCategories.length) * 100
            },
            status: 'security-analysis-complete'
          };
        }
      });

      manager.addKnowledgeHook(securityHook);

      const event = {
        name: 'security-analysis',
        payload: {
          tests: [
            {
              name: 'sql-injection-test',
              category: 'INPUT_VALIDATION',
              status: 'PASSED',
              vulnerability: 'SQL_INJECTION'
            },
            {
              name: 'xss-test',
              category: 'INPUT_VALIDATION',
              status: 'FAILED',
              vulnerability: 'XSS',
              severity: 'HIGH',
              description: 'XSS vulnerability detected'
            },
            {
              name: 'auth-bypass-test',
              category: 'AUTHENTICATION',
              status: 'PASSED',
              vulnerability: 'AUTH_BYPASS'
            }
          ],
          vulnerabilities: [
            { type: 'SQL_INJECTION', severity: 'HIGH' },
            { type: 'XSS', severity: 'MEDIUM' },
            { type: 'CSRF', severity: 'MEDIUM' },
            { type: 'AUTH_BYPASS', severity: 'CRITICAL' }
          ],
          coverage: {
            'INPUT_VALIDATION': 80,
            'AUTHENTICATION': 60,
            'AUTHORIZATION': 0,
            'ENCRYPTION': 0,
            'SESSION_MANAGEMENT': 0
          }
        },
        context: {
          graph: new Store(),
          timestamp: new Date()
        }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      
      // Should detect security test gaps
      expect(results[0].success).toBe(true);
      expect(results[0].runResult?.result?.totalTests).toBe(3);
      expect(results[0].runResult?.result?.securityGaps).toBe(4);
      expect(results[0].runResult?.result?.testFailures).toBe(1);
      expect(results[0].runResult?.result?.coverageScore).toBe(40);
      expect(securityTestResults.length).toBe(5);
    });
  });

  describe('User Acceptance Testing', () => {
    it('should analyze user acceptance testing results and feedback', async () => {
      const uatHook = defineHook({
        meta: {
          name: 'testing:user-acceptance',
          description: 'Analyze user acceptance testing results and feedback'
        },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: 'file://hooks/testing/user-acceptance.ask.rq',
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3'
          }
        },
        run: async (event) => {
          // Simulate UAT analysis
          const tests = event.payload.tests || [];
          const feedback = event.payload.feedback || [];
          const scenarios = event.payload.scenarios || [];
          
          let uatIssues = [];
          let feedbackAnalysis = [];
          
          // Analyze test results
          tests.forEach(test => {
            if (test.status === 'FAILED') {
              uatIssues.push({
                test: test.name,
                scenario: test.scenario,
                user: test.user,
                issue: test.issue,
                severity: test.severity,
                category: test.category
              });
            }
          });
          
          // Analyze user feedback
          feedback.forEach(fb => {
            if (fb.rating < 4) { // Rating below 4/5
              feedbackAnalysis.push({
                user: fb.user,
                rating: fb.rating,
                comments: fb.comments,
                category: fb.category,
                priority: fb.rating < 2 ? 'HIGH' : 'MEDIUM'
              });
            }
          });
          
          // Check scenario coverage
          const coveredScenarios = tests.map(t => t.scenario).filter(Boolean);
          const totalScenarios = scenarios.length;
          const uncoveredScenarios = scenarios.filter(s => !coveredScenarios.includes(s.name));
          
          if (uncoveredScenarios.length > 0) {
            uatIssues.push({
              type: 'UNCOVERED_SCENARIOS',
              scenarios: uncoveredScenarios,
              count: uncoveredScenarios.length,
              severity: 'MEDIUM'
            });
          }
          
          if (uatIssues.length > 0) {
            uatResults.push(...uatIssues);
          }
          
          if (feedbackAnalysis.length > 0) {
            uatResults.push(...feedbackAnalysis);
          }
          
          return {
            result: { 
              totalTests: tests.length,
              failedTests: tests.filter(t => t.status === 'FAILED').length,
              totalFeedback: feedback.length,
              negativeFeedback: feedbackAnalysis.length,
              scenarioCoverage: ((totalScenarios - uncoveredScenarios.length) / totalScenarios) * 100
            },
            status: 'uat-analysis-complete'
          };
        }
      });

      manager.addKnowledgeHook(uatHook);

      const event = {
        name: 'uat-analysis',
        payload: {
          tests: [
            {
              name: 'user-registration',
              scenario: 'new-user-signup',
              user: 'user1',
              status: 'PASSED'
            },
            {
              name: 'payment-flow',
              scenario: 'credit-card-payment',
              user: 'user2',
              status: 'FAILED',
              issue: 'Payment form validation error',
              severity: 'HIGH',
              category: 'FUNCTIONALITY'
            },
            {
              name: 'mobile-responsive',
              scenario: 'mobile-navigation',
              user: 'user3',
              status: 'FAILED',
              issue: 'Navigation menu not accessible on mobile',
              severity: 'MEDIUM',
              category: 'USABILITY'
            }
          ],
          feedback: [
            {
              user: 'user1',
              rating: 5,
              comments: 'Great user experience',
              category: 'OVERALL'
            },
            {
              user: 'user2',
              rating: 2,
              comments: 'Payment process is confusing',
              category: 'FUNCTIONALITY'
            },
            {
              user: 'user3',
              rating: 3,
              comments: 'Mobile version needs improvement',
              category: 'USABILITY'
            }
          ],
          scenarios: [
            { name: 'new-user-signup' },
            { name: 'credit-card-payment' },
            { name: 'mobile-navigation' },
            { name: 'password-reset' },
            { name: 'profile-update' }
          ]
        },
        context: {
          graph: new Store(),
          timestamp: new Date()
        }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      
      // Should detect UAT issues
      expect(results[0].success).toBe(true);
      expect(results[0].runResult?.result?.totalTests).toBe(3);
      expect(results[0].runResult?.result?.failedTests).toBe(2);
      expect(results[0].runResult?.result?.totalFeedback).toBe(3);
      expect(results[0].runResult?.result?.negativeFeedback).toBe(2);
      expect(results[0].runResult?.result?.scenarioCoverage).toBe(60);
      expect(uatResults.length).toBe(4);
    });
  });
});
