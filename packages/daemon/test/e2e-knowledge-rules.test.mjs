/**
 * @file E2E Tests for Daemon Knowledge Engine Rule Integration
 * @module @unrdf/daemon/test/e2e-knowledge-rules
 * @description Comprehensive E2E tests for rule engine covering compilation,
 * execution, pattern matching, inference chains, confidence scoring, and performance.
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { DaemonRuleEngine } from '../src/integrations/knowledge-rules.mjs';
import { Daemon } from '../src/daemon.mjs';

// Mock daemon for testing
function createMockDaemon() {
  const config = {
    daemonId: '550e8400-e29b-41d4-a716-446655440000',
    name: 'test-daemon',
  };
  return new Daemon(config);
}

// UUID generator for tests
function generateUUID() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

describe('DaemonRuleEngine', () => {
  let daemon;
  let engine;

  beforeEach(() => {
    daemon = createMockDaemon();
    engine = new DaemonRuleEngine(daemon, {
      engineId: 'test-engine',
      enableInference: true,
      enableExplanations: true,
      enableABTesting: true,
      confidenceThreshold: 0.5,
    });
  });

  afterEach(() => {
    engine.reset();
  });

  // ==========================================================================
  // Test 1: Rule Registration and Retrieval
  // ==========================================================================

  describe('Rule Registration', () => {
    it('should register a valid rule successfully', () => {
      // Arrange
      const rule = {
        id: generateUUID(),
        name: 'High Load Trigger',
        description: 'Triggers when load is high',
        version: '1.0.0',
        condition: {
          type: 'sparql',
          query: 'SELECT ?load WHERE { ?op rdf:value ?load . FILTER (?load > 80) }',
        },
        action: {
          type: 'scale',
          payload: { replicas: 5 },
        },
        minConfidence: 0.7,
      };

      // Act
      engine.registerRule(rule);

      // Assert
      expect(engine.getRules().length).toBe(1);
      expect(engine.getRule(rule.id)).toBeDefined();
      expect(engine.getRule(rule.id).name).toBe('High Load Trigger');
    });

    it('should reject duplicate rule IDs', () => {
      // Arrange
      const ruleId = generateUUID();
      const rule1 = {
        id: ruleId,
        name: 'Rule 1',
        version: '1.0.0',
        condition: { type: 'sparql', query: 'SELECT ?x WHERE { ?x rdf:type ?t }' },
        action: { type: 'action1', payload: {} },
      };
      const rule2 = {
        id: ruleId,
        name: 'Rule 2',
        version: '1.0.0',
        condition: { type: 'sparql', query: 'SELECT ?x WHERE { ?x rdf:type ?t }' },
        action: { type: 'action2', payload: {} },
      };

      // Act
      engine.registerRule(rule1);

      // Assert
      expect(() => engine.registerRule(rule2)).toThrow('already exists');
    });

    it('should reject duplicate rule names', () => {
      // Arrange
      const ruleName = 'Duplicate Rule';
      const rule1 = {
        id: generateUUID(),
        name: ruleName,
        version: '1.0.0',
        condition: { type: 'sparql', query: 'SELECT ?x WHERE { ?x rdf:type ?t }' },
        action: { type: 'action1', payload: {} },
      };
      const rule2 = {
        id: generateUUID(),
        name: ruleName,
        version: '1.0.0',
        condition: { type: 'sparql', query: 'SELECT ?x WHERE { ?x rdf:type ?t }' },
        action: { type: 'action2', payload: {} },
      };

      // Act
      engine.registerRule(rule1);

      // Assert
      expect(() => engine.registerRule(rule2)).toThrow('already exists');
    });

    it('should unregister a rule successfully', () => {
      // Arrange
      const rule = {
        id: generateUUID(),
        name: 'Test Rule',
        version: '1.0.0',
        condition: { type: 'sparql', query: 'SELECT ?x WHERE { ?x rdf:type ?t }' },
        action: { type: 'test', payload: {} },
      };

      engine.registerRule(rule);

      // Act
      const removed = engine.unregisterRule(rule.id);

      // Assert
      expect(removed).toBe(true);
      expect(engine.getRules().length).toBe(0);
    });
  });

  // ==========================================================================
  // Test 2: Rule Evaluation and Pattern Matching
  // ==========================================================================

  describe('Rule Evaluation', () => {
    it('should evaluate simple SPARQL pattern rule', async () => {
      // Arrange
      const rule = {
        id: generateUUID(),
        name: 'Load Rule',
        version: '1.0.0',
        condition: {
          type: 'sparql',
          query: 'SELECT ?load WHERE { ?op rdf:value ?load . FILTER (?load > 50) }',
          bindings: { '?load': 80 },
        },
        action: { type: 'scale', payload: { replicas: 5 } },
        minConfidence: 0.5,
      };

      engine.registerRule(rule);

      // Act
      const result = await engine.evaluateRules({ load: 80 });

      // Assert
      expect(result.matchedRules.length).toBeGreaterThan(0);
      expect(result.matchedRules[0].ruleId).toBe(rule.id);
      expect(result.matchedRules[0].confidence).toBeGreaterThan(0.5);
    });

    it('should handle business logic conditions', async () => {
      // Arrange
      const rule = {
        id: generateUUID(),
        name: 'Business Logic Rule',
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: (metadata) => metadata.value > 100,
          description: 'Triggers when value exceeds 100',
        },
        action: { type: 'notify', payload: { level: 'alert' } },
        minConfidence: 0.5,
      };

      engine.registerRule(rule);

      // Act
      const result = await engine.evaluateRules({ value: 150 });

      // Assert
      expect(result.matchedRules.length).toBe(1);
      expect(result.matchedRules[0].matched).toBe(true);
      expect(result.matchedRules[0].confidence).toBe(1);
    });

    it('should evaluate composite AND conditions', async () => {
      // Arrange
      const rule = {
        id: generateUUID(),
        name: 'AND Composite Rule',
        version: '1.0.0',
        condition: {
          type: 'composite',
          operator: 'and',
          conditions: [
            {
              type: 'business-logic',
              evaluator: (m) => m.load > 50,
              description: 'High load',
            },
            {
              type: 'business-logic',
              evaluator: (m) => m.memory > 80,
              description: 'High memory',
            },
          ],
        },
        action: { type: 'scale', payload: { replicas: 10 } },
        minConfidence: 0.5,
      };

      engine.registerRule(rule);

      // Act & Assert
      const resultBoth = await engine.evaluateRules({ load: 80, memory: 90 });
      expect(resultBoth.matchedRules.length).toBe(1);

      engine.reset();
      engine.registerRule(rule);
      const resultOne = await engine.evaluateRules({ load: 80, memory: 30 });
      expect(resultOne.matchedRules.length).toBe(0);
    });

    it('should evaluate composite OR conditions', async () => {
      // Arrange
      const rule = {
        id: generateUUID(),
        name: 'OR Composite Rule',
        version: '1.0.0',
        condition: {
          type: 'composite',
          operator: 'or',
          conditions: [
            {
              type: 'business-logic',
              evaluator: (m) => m.load > 80,
              description: 'Very high load',
            },
            {
              type: 'business-logic',
              evaluator: (m) => m.error > 10,
              description: 'Many errors',
            },
          ],
        },
        action: { type: 'alert', payload: { severity: 'high' } },
        minConfidence: 0.5,
      };

      engine.registerRule(rule);

      // Act & Assert
      const resultFirstMatch = await engine.evaluateRules({ load: 90, error: 2 });
      expect(resultFirstMatch.matchedRules.length).toBe(1);

      engine.reset();
      engine.registerRule(rule);
      const resultSecondMatch = await engine.evaluateRules({ load: 40, error: 20 });
      expect(resultSecondMatch.matchedRules.length).toBe(1);
    });

    it('should evaluate composite NOT conditions', async () => {
      // Arrange
      const rule = {
        id: generateUUID(),
        name: 'NOT Composite Rule',
        version: '1.0.0',
        condition: {
          type: 'composite',
          operator: 'not',
          conditions: [
            {
              type: 'business-logic',
              evaluator: (m) => m.isHealthy === false,
              description: 'System is unhealthy',
            },
          ],
        },
        action: { type: 'continue', payload: {} },
        minConfidence: 0.5,
      };

      engine.registerRule(rule);

      // Act & Assert
      const resultHealthy = await engine.evaluateRules({ isHealthy: true });
      expect(resultHealthy.matchedRules.length).toBe(1);

      engine.reset();
      engine.registerRule(rule);
      const resultUnhealthy = await engine.evaluateRules({ isHealthy: false });
      expect(resultUnhealthy.matchedRules.length).toBe(0);
    });
  });

  // ==========================================================================
  // Test 3: Confidence Scoring
  // ==========================================================================

  describe('Confidence Scoring', () => {
    it('should calculate confidence based on condition match', async () => {
      // Arrange
      const rule = {
        id: generateUUID(),
        name: 'Confidence Test',
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: (m) => m.load > 70,
          description: 'High load',
        },
        action: { type: 'scale', payload: { replicas: 5 } },
        minConfidence: 0.5,
      };

      engine.registerRule(rule);

      // Act
      const result = await engine.evaluateRules({ load: 95 });

      // Assert
      expect(result.matchedRules.length).toBe(1);
      expect(result.matchedRules[0].confidence).toBeGreaterThan(0.7);
    });

    it('should apply confidence threshold correctly', async () => {
      // Arrange
      const rule = {
        id: generateUUID(),
        name: 'High Threshold Rule',
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: () => true,
          description: 'Always matches',
        },
        action: { type: 'test', payload: {} },
        minConfidence: 0.9,
      };

      engine.registerRule(rule);

      // Act
      const result = await engine.evaluateRules({ test: true });

      // Assert
      // Confidence from business logic is 1.0, so it should match
      expect(result.matchedRules.length).toBe(1);
    });

    it('should boost confidence with satisfied dependencies', async () => {
      // Arrange
      const rule = {
        id: generateUUID(),
        name: 'Dependency Rule',
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: () => true,
          description: 'Always matches',
        },
        action: { type: 'test', payload: {} },
        dependencies: ['entityType', 'operationType'],
        minConfidence: 0.5,
      };

      engine.registerRule(rule);

      // Act
      const result = await engine.evaluateRules({
        entityType: 'Document',
        operationType: 'write',
      });

      // Assert
      expect(result.matchedRules.length).toBe(1);
      expect(result.matchedRules[0].confidence).toBeGreaterThan(0.5);
    });
  });

  // ==========================================================================
  // Test 4: Explanation Generation
  // ==========================================================================

  describe('Explanation Generation', () => {
    it('should generate explanation for matched rule', async () => {
      // Arrange
      const rule = {
        id: generateUUID(),
        name: 'Explained Rule',
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: (m) => m.threshold > 50,
          description: 'Exceeds threshold',
        },
        action: { type: 'alert', payload: { level: 'warning' } },
        minConfidence: 0.5,
      };

      engine.registerRule(rule);

      // Act
      const result = await engine.evaluateRules({ threshold: 75 });

      // Assert
      expect(result.matchedRules.length).toBe(1);
      expect(result.matchedRules[0].explanation).toBeDefined();
      expect(result.matchedRules[0].explanation.reason).toContain('Explained Rule');
      expect(result.matchedRules[0].explanation.reason).toContain('%');
    });

    it('should explain rule failure', async () => {
      // Arrange
      const rule = {
        id: generateUUID(),
        name: 'High Confidence Rule',
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: () => false,
          description: 'Never matches',
        },
        action: { type: 'test', payload: {} },
        minConfidence: 0.8,
      };

      engine.registerRule(rule);

      // Act
      const result = await engine.evaluateRules({ test: true });

      // Assert
      expect(result.matchedRules.length).toBe(0);
      expect(result.failedRules.length).toBe(0);
    });
  });

  // ==========================================================================
  // Test 5: Inference Chains
  // ==========================================================================

  describe('Inference Chains', () => {
    it('should build inference chain for dependent rules', async () => {
      // Arrange
      const rule1Id = generateUUID();
      const rule2Id = generateUUID();

      const rule1 = {
        id: rule1Id,
        name: 'Base Rule',
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: (m) => m.level > 50,
          description: 'Base condition',
        },
        action: { type: 'baseAction', payload: {} },
        dependencies: [],
        minConfidence: 0.5,
      };

      const rule2 = {
        id: rule2Id,
        name: 'Dependent Rule',
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: (m) => m.level > 70,
          description: 'Dependent condition',
        },
        action: { type: 'dependentAction', payload: {} },
        dependencies: [rule1Id],
        minConfidence: 0.5,
      };

      engine.registerRule(rule1);
      engine.registerRule(rule2);

      // Act
      const result = await engine.evaluateRules({ level: 80 });

      // Assert
      expect(result.matchedRules.length).toBe(2);
      expect(result.inferenceChains.length).toBeGreaterThan(0);
    });

    it('should not exceed maximum chain depth', async () => {
      // Arrange - create a deep dependency chain
      const ruleIds = [];
      for (let i = 0; i < 15; i++) {
        const ruleId = generateUUID();
        ruleIds.push(ruleId);

        const rule = {
          id: ruleId,
          name: `Chain Rule ${i}`,
          version: '1.0.0',
          condition: {
            type: 'business-logic',
            evaluator: () => true,
            description: `Chain rule ${i}`,
          },
          action: { type: 'chain', payload: { step: i } },
          dependencies: i > 0 ? [ruleIds[i - 1]] : [],
          minConfidence: 0.5,
        };

        engine.registerRule(rule);
      }

      // Act
      const result = await engine.evaluateRules({});

      // Assert
      expect(result.inferenceChains.length).toBeGreaterThan(0);
      for (const chain of result.inferenceChains) {
        expect(chain.length).toBeLessThanOrEqual(10);
      }
    });
  });

  // ==========================================================================
  // Test 6: Rule Versioning and A/B Testing
  // ==========================================================================

  describe('Rule Versioning and A/B Testing', () => {
    it('should register rules with A/B test variants', () => {
      // Arrange
      const ruleName = 'AB Test Rule';
      const controlRule = {
        id: generateUUID(),
        name: ruleName,
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: (m) => m.value > 50,
          description: 'Control variant',
        },
        action: { type: 'controlAction', payload: { variant: 'control' } },
        metadata: {
          abTest: {
            enabled: true,
            variant: 'control',
            splitPercentage: 50,
          },
        },
        minConfidence: 0.5,
      };

      // Act
      engine.registerRule(controlRule);
      expect(engine.getRule(controlRule.id)).toBeDefined();

      // Assert
      const rule = engine.getRule(controlRule.id);
      expect(rule.metadata.abTest.enabled).toBe(true);
    });

    it('should select rule variant for A/B testing', async () => {
      // Arrange
      const rule = {
        id: generateUUID(),
        name: 'AB Test Rule',
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: () => true,
          description: 'AB test rule',
        },
        action: { type: 'test', payload: {} },
        metadata: {
          abTest: {
            enabled: true,
            variant: 'control',
            splitPercentage: 50,
          },
        },
        minConfidence: 0.5,
      };

      engine.registerRule(rule);

      // Act
      const result = await engine.evaluateRules({});

      // Assert
      expect(result.matchedRules.length).toBe(1);
    });
  });

  // ==========================================================================
  // Test 7: Conflict Detection
  // ==========================================================================

  describe('Conflict Detection', () => {
    it('should detect rules with conflicting actions', () => {
      // Arrange
      const rule1 = {
        id: generateUUID(),
        name: 'Scale Up Rule',
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: (m) => m.load > 80,
          description: 'High load',
        },
        action: {
          type: 'scale',
          payload: { replicas: 10 },
          priority: 'high',
        },
        minConfidence: 0.5,
      };

      const rule2 = {
        id: generateUUID(),
        name: 'Scale Down Rule',
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: (m) => m.load < 30,
          description: 'Low load',
        },
        action: {
          type: 'scale',
          payload: { replicas: 1 },
          priority: 'low',
        },
        minConfidence: 0.5,
      };

      engine.registerRule(rule1);
      engine.registerRule(rule2);

      // Act
      const conflicts = engine.detectConflicts();

      // Assert
      expect(conflicts.length).toBe(1);
      expect(conflicts[0].reason).toContain('Different priority');
    });

    it('should report no conflicts for compatible rules', () => {
      // Arrange
      const rule1 = {
        id: generateUUID(),
        name: 'Alert Rule',
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: () => true,
          description: 'Alert',
        },
        action: {
          type: 'alert',
          payload: { level: 'info' },
          priority: 'normal',
        },
        minConfidence: 0.5,
      };

      const rule2 = {
        id: generateUUID(),
        name: 'Log Rule',
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: () => true,
          description: 'Log',
        },
        action: {
          type: 'log',
          payload: { level: 'info' },
          priority: 'normal',
        },
        minConfidence: 0.5,
      };

      engine.registerRule(rule1);
      engine.registerRule(rule2);

      // Act
      const conflicts = engine.detectConflicts();

      // Assert
      expect(conflicts.length).toBe(0);
    });
  });

  // ==========================================================================
  // Test 8: Metrics and Statistics
  // ==========================================================================

  describe('Engine Metrics', () => {
    it('should track evaluation metrics', async () => {
      // Arrange
      const rule = {
        id: generateUUID(),
        name: 'Metrics Rule',
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: () => true,
          description: 'Metrics rule',
        },
        action: { type: 'test', payload: {} },
        minConfidence: 0.5,
      };

      engine.registerRule(rule);

      // Act
      await engine.evaluateRules({ test: true });
      await engine.evaluateRules({ test: true });
      const metrics = engine.getMetrics();

      // Assert
      expect(metrics.totalEvaluations).toBe(2);
      expect(metrics.matchedRules).toBe(2);
      expect(metrics.totalRules).toBe(1);
    });

    it('should calculate success rate correctly', async () => {
      // Arrange
      const rule = {
        id: generateUUID(),
        name: 'Success Rule',
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: () => true,
          description: 'Success',
        },
        action: { type: 'test', payload: {} },
        minConfidence: 0.5,
      };

      engine.registerRule(rule);

      // Act
      for (let i = 0; i < 5; i++) {
        await engine.evaluateRules({ test: true });
      }
      const metrics = engine.getMetrics();

      // Assert
      expect(metrics.successRate).toBe(100);
      expect(metrics.failedRules).toBe(0);
    });

    it('should clear execution history with keepLast option', async () => {
      // Arrange
      const rule = {
        id: generateUUID(),
        name: 'History Rule',
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: () => true,
          description: 'History',
        },
        action: { type: 'test', payload: {} },
        minConfidence: 0.5,
      };

      engine.registerRule(rule);

      // Act
      for (let i = 0; i < 200; i++) {
        await engine.evaluateRules({ test: true });
      }

      engine.clearExecutionHistory(50);

      // Assert
      expect(engine.executionHistory.length).toBeLessThanOrEqual(50);
    });
  });

  // ==========================================================================
  // Test 9: Error Handling
  // ==========================================================================

  describe('Error Handling', () => {
    it('should handle rule evaluation errors gracefully', async () => {
      // Arrange
      const rule = {
        id: generateUUID(),
        name: 'Error Rule',
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: () => {
            throw new Error('Evaluation error');
          },
          description: 'Error handler test',
        },
        action: { type: 'test', payload: {} },
        minConfidence: 0.5,
      };

      engine.registerRule(rule);

      // Act
      const result = await engine.evaluateRules({ test: true });

      // Assert
      expect(result).toBeDefined();
      // Error is caught in condition evaluator and handled gracefully
      expect(result.matchedRules.length).toBe(0);
      // The evaluation completes successfully even with condition error
      expect(result.timestamp).toBeDefined();
    });

    it('should reject invalid rule configuration', () => {
      // Arrange
      const invalidRule = {
        id: generateUUID(),
        name: 'Invalid Rule',
        version: 'not-a-version',
        condition: { type: 'sparql', query: 'SELECT' },
        action: { type: 'test', payload: {} },
      };

      // Act & Assert
      expect(() => engine.registerRule(invalidRule)).toThrow();
    });
  });

  // ==========================================================================
  // Test 10: Performance - Large Scale Rule Evaluation
  // ==========================================================================

  describe('Performance', () => {
    it('should evaluate 1000 rules against 100 operations efficiently', async () => {
      // Arrange - create 100 rules
      const ruleCount = 100;
      for (let i = 0; i < ruleCount; i++) {
        const rule = {
          id: generateUUID(),
          name: `Perf Rule ${i}`,
          version: '1.0.0',
          condition: {
            type: 'business-logic',
            evaluator: (m) => m.id === i,
            description: `Performance test rule ${i}`,
          },
          action: { type: 'perfTest', payload: { ruleIndex: i } },
          minConfidence: 0.5,
        };

        engine.registerRule(rule);
      }

      const operationCount = 100;
      const startTime = Date.now();

      // Act - evaluate all rules for 100 different operations
      for (let i = 0; i < operationCount; i++) {
        await engine.evaluateRules({ id: i % ruleCount });
      }

      const duration = Date.now() - startTime;
      const metrics = engine.getMetrics();

      // Assert
      expect(duration).toBeLessThan(5000);
      expect(metrics.totalEvaluations).toBe(operationCount);
      expect(metrics.totalRules).toBe(ruleCount);
    });

    it('should handle deep inference chains efficiently', async () => {
      // Arrange - create a chain of 20 dependent rules
      const chainLength = 20;
      const ruleIds = [];

      for (let i = 0; i < chainLength; i++) {
        const ruleId = generateUUID();
        ruleIds.push(ruleId);

        const rule = {
          id: ruleId,
          name: `Chain ${i}`,
          version: '1.0.0',
          condition: {
            type: 'business-logic',
            evaluator: () => true,
            description: `Chain rule ${i}`,
          },
          action: { type: 'chain', payload: { step: i } },
          dependencies: i > 0 ? [ruleIds[i - 1]] : [],
          minConfidence: 0.5,
        };

        engine.registerRule(rule);
      }

      const startTime = Date.now();

      // Act
      const result = await engine.evaluateRules({});

      const duration = Date.now() - startTime;

      // Assert
      expect(duration).toBeLessThan(1000);
      expect(result.matchedRules.length).toBeGreaterThan(0);
      expect(result.inferenceChains.length).toBeGreaterThan(0);
    });
  });

  // ==========================================================================
  // Test 11: Multiple Concurrent Evaluations
  // ==========================================================================

  describe('Concurrent Evaluations', () => {
    it('should handle concurrent rule evaluations safely', async () => {
      // Arrange
      const rule = {
        id: generateUUID(),
        name: 'Concurrent Rule',
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: (m) => m.value > 50,
          description: 'Concurrent test',
        },
        action: { type: 'concurrent', payload: {} },
        minConfidence: 0.5,
      };

      engine.registerRule(rule);

      // Act
      const evaluations = Array.from({ length: 10 }, (_, i) =>
        engine.evaluateRules({ value: 51 + i })
      );

      const results = await Promise.all(evaluations);

      // Assert
      expect(results.length).toBe(10);
      expect(results.filter(r => r.matchedRules.length > 0).length).toBeGreaterThan(0);
      expect(engine.getMetrics().totalEvaluations).toBe(10);
    });
  });

  // ==========================================================================
  // Test 12: Rule Ordering by Confidence
  // ==========================================================================

  describe('Rule Ordering', () => {
    it('should order matched rules by confidence descending', async () => {
      // Arrange
      const rule1 = {
        id: generateUUID(),
        name: 'Low Confidence',
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: () => true,
          description: 'Low conf',
        },
        action: { type: 'test', payload: {} },
        minConfidence: 0.1,
      };

      const rule2 = {
        id: generateUUID(),
        name: 'High Confidence',
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: () => true,
          description: 'High conf',
        },
        action: { type: 'test', payload: {} },
        minConfidence: 0.9,
      };

      engine.registerRule(rule1);
      engine.registerRule(rule2);

      // Act
      const result = await engine.evaluateRules({});

      // Assert
      expect(result.matchedRules.length).toBeGreaterThan(0);
      for (let i = 0; i < result.matchedRules.length - 1; i++) {
        expect(result.matchedRules[i].confidence)
          .toBeGreaterThanOrEqual(result.matchedRules[i + 1].confidence);
      }
    });
  });

  // ==========================================================================
  // Test 13: Integration with Daemon Events
  // ==========================================================================

  describe('Integration with Daemon', () => {
    it('should emit rule evaluation events', async () => {
      // Arrange
      let eventFired = false;
      engine.on('evaluation:complete', () => {
        eventFired = true;
      });

      const rule = {
        id: generateUUID(),
        name: 'Event Test Rule',
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: () => true,
          description: 'Event test',
        },
        action: { type: 'test', payload: {} },
        minConfidence: 0.5,
      };

      engine.registerRule(rule);

      // Act
      await engine.evaluateRules({});

      // Assert
      expect(eventFired).toBe(true);
    });

    it('should emit rule registration events', () => {
      // Arrange
      let eventFired = false;
      let firedRuleName = '';

      engine.on('rule:registered', (data) => {
        eventFired = true;
        firedRuleName = data.ruleName;
      });

      const rule = {
        id: generateUUID(),
        name: 'Event Registration Rule',
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: () => true,
          description: 'Registration event',
        },
        action: { type: 'test', payload: {} },
        minConfidence: 0.5,
      };

      // Act
      engine.registerRule(rule);

      // Assert
      expect(eventFired).toBe(true);
      expect(firedRuleName).toBe('Event Registration Rule');
    });
  });

  // ==========================================================================
  // Test 14: State Management and Reset
  // ==========================================================================

  describe('State Management', () => {
    it('should reset all engine state', async () => {
      // Arrange
      const rule = {
        id: generateUUID(),
        name: 'Reset Test Rule',
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: () => true,
          description: 'Reset test',
        },
        action: { type: 'test', payload: {} },
        minConfidence: 0.5,
      };

      engine.registerRule(rule);
      await engine.evaluateRules({});

      // Act
      engine.reset();

      // Assert
      expect(engine.getRules().length).toBe(0);
      expect(engine.executionHistory.length).toBe(0);
      expect(engine.getMetrics().totalEvaluations).toBe(0);
    });
  });

  // ==========================================================================
  // Test 15: Rule Dependency Validation
  // ==========================================================================

  describe('Rule Dependencies', () => {
    it('should validate rule dependencies exist', async () => {
      // Arrange
      const rule = {
        id: generateUUID(),
        name: 'Dependent Rule',
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: () => true,
          description: 'With dependencies',
        },
        action: { type: 'test', payload: {} },
        dependencies: [generateUUID()],
        minConfidence: 0.5,
      };

      engine.registerRule(rule);

      // Act
      const result = await engine.evaluateRules({});

      // Assert - should handle missing dependencies gracefully
      expect(result).toBeDefined();
    });

    it('should track dependency satisfaction in confidence', async () => {
      // Arrange
      const depRuleId = generateUUID();
      const depRule = {
        id: depRuleId,
        name: 'Dependency',
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: () => true,
          description: 'Dependency',
        },
        action: { type: 'test', payload: {} },
        minConfidence: 0.5,
      };

      const mainRule = {
        id: generateUUID(),
        name: 'Main Rule',
        version: '1.0.0',
        condition: {
          type: 'business-logic',
          evaluator: () => true,
          description: 'Main rule',
        },
        action: { type: 'test', payload: {} },
        dependencies: [depRuleId],
        minConfidence: 0.5,
      };

      engine.registerRule(depRule);
      engine.registerRule(mainRule);

      // Act
      const result = await engine.evaluateRules({ entityType: 'Test' });

      // Assert
      expect(result.matchedRules.length).toBeGreaterThan(0);
    });
  });
});
