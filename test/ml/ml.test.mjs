/**
 * @file ml.test.mjs
 * @description Comprehensive tests for ML module
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  PatternRecognizer,
  QLearningAgent,
  AnomalyDetector,
  calculateReward,
} from '../../src/ml/index.mjs';

describe('PatternRecognizer', () => {
  /** @type {PatternRecognizer} */
  let recognizer;

  beforeEach(() => {
    recognizer = new PatternRecognizer({ windowSize: 3, minSupport: 2 });
  });

  describe('observation and history', () => {
    it('should record observations correctly', () => {
      recognizer.observe({
        artifacts: ['a1', 'a2'],
        driftValue: 10,
        normalized: 0.5,
        epoch: 1,
        timestamp: Date.now(),
      });

      expect(recognizer.history).toHaveLength(1);
      expect(recognizer.driftSeriesIndex).toBe(1);
    });

    it('should track artifact frequency', () => {
      recognizer.observe({
        artifacts: ['a1'],
        driftValue: 5,
        normalized: 0.25,
        epoch: 1,
        timestamp: Date.now(),
      });

      recognizer.observe({
        artifacts: ['a1', 'a2'],
        driftValue: 3,
        normalized: 0.15,
        epoch: 2,
        timestamp: Date.now(),
      });

      expect(recognizer.artifactFrequency.get('a1')).toBe(2);
      expect(recognizer.artifactFrequency.get('a2')).toBe(1);
    });

    it('should expand drift array when needed', () => {
      const initialSize = recognizer.driftSeries.length;

      // Add more than initial capacity
      for (let i = 0; i < initialSize + 10; i++) {
        recognizer.observe({
          artifacts: [`a${i}`],
          driftValue: i,
          normalized: i / 100,
          epoch: i,
          timestamp: Date.now(),
        });
      }

      expect(recognizer.driftSeries.length).toBeGreaterThan(initialSize);
      expect(recognizer.driftSeriesIndex).toBe(initialSize + 10);
    });
  });

  describe('pattern extraction', () => {
    it('should extract patterns from window', () => {
      // Add observations to create a pattern
      for (let i = 1; i <= 5; i++) {
        recognizer.observe({
          artifacts: ['a1', 'a2'],
          driftValue: 10 - i,
          normalized: (10 - i) / 10,
          epoch: i,
          timestamp: Date.now() + i * 1000,
        });
      }

      const patterns = recognizer.getPatterns({ onlyFrequent: false });
      expect(patterns.length).toBeGreaterThan(0);
    });

    it('should filter patterns by minimum support', () => {
      // Create pattern with low frequency
      recognizer.observe({
        artifacts: ['a1'],
        driftValue: 5,
        normalized: 0.5,
        epoch: 1,
        timestamp: Date.now(),
      });

      const frequentOnly = recognizer.getPatterns({ onlyFrequent: true });
      const all = recognizer.getPatterns({ onlyFrequent: false });

      expect(frequentOnly.length).toBeLessThanOrEqual(all.length);
    });
  });

  describe('convergence prediction', () => {
    it('should predict convergence time based on patterns', () => {
      // Create decreasing drift pattern
      for (let i = 1; i <= 10; i++) {
        recognizer.observe({
          artifacts: ['a1'],
          driftValue: 10 / i,
          normalized: 1 / i,
          epoch: i,
          timestamp: Date.now() + i * 1000,
        });
      }

      const prediction = recognizer.predictConvergence();

      expect(prediction.estimatedEpochs).toBeGreaterThan(0);
      expect(prediction.confidence).toBeGreaterThanOrEqual(0);
      expect(prediction.confidence).toBeLessThanOrEqual(1);
    });

    it('should have low confidence with insufficient data', () => {
      recognizer.observe({
        artifacts: ['a1'],
        driftValue: 5,
        normalized: 0.5,
        epoch: 1,
        timestamp: Date.now(),
      });

      const prediction = recognizer.predictConvergence();
      expect(prediction.confidence).toBeLessThan(0.3);
    });
  });

  describe('compression strategy', () => {
    it('should suggest aggressive deduplication for low entropy', () => {
      // Add many identical artifacts (low entropy)
      for (let i = 0; i < 10; i++) {
        recognizer.observe({
          artifacts: ['a1', 'a1', 'a1'],
          driftValue: 5,
          normalized: 0.5,
          epoch: i,
          timestamp: Date.now() + i * 1000,
        });
      }

      const strategy = recognizer.suggestCompressionStrategy();
      expect(strategy.strategy).toBe('aggressive-deduplication');
      expect(strategy.expectedRatio).toBeLessThan(0.5);
    });

    it('should suggest frequency-based for high frequency', () => {
      // Add varied but high frequency artifacts
      for (let i = 0; i < 20; i++) {
        recognizer.observe({
          artifacts: ['a1', 'a2'],
          driftValue: 5 + i % 3,
          normalized: 0.5,
          epoch: i,
          timestamp: Date.now() + i * 1000,
        });
      }

      const strategy = recognizer.suggestCompressionStrategy();
      expect(['frequency-based', 'aggressive-deduplication']).toContain(strategy.strategy);
    });
  });

  describe('summary and reset', () => {
    it('should provide accurate summary', () => {
      for (let i = 0; i < 5; i++) {
        recognizer.observe({
          artifacts: ['a1'],
          driftValue: 10 - i,
          normalized: (10 - i) / 10,
          epoch: i,
          timestamp: Date.now() + i * 1000,
        });
      }

      const summary = recognizer.getSummary();

      expect(summary.totalObservations).toBe(5);
      expect(summary.driftSeriesLength).toBe(5);
      expect(summary.averageDrift).toBeGreaterThan(0);
    });

    it('should reset state completely', () => {
      recognizer.observe({
        artifacts: ['a1'],
        driftValue: 5,
        normalized: 0.5,
        epoch: 1,
        timestamp: Date.now(),
      });

      recognizer.reset();

      expect(recognizer.history).toHaveLength(0);
      expect(recognizer.driftSeriesIndex).toBe(0);
      expect(recognizer.patterns.size).toBe(0);
    });
  });
});

describe('QLearningAgent', () => {
  /** @type {QLearningAgent} */
  let agent;

  const actions = [
    { id: 'aggressive', name: 'Aggressive Compression' },
    { id: 'balanced', name: 'Balanced' },
    { id: 'conservative', name: 'Conservative' },
  ];

  beforeEach(() => {
    agent = new QLearningAgent({
      actions,
      epsilon: 0.2,
      learningRate: 0.1,
      discountFactor: 0.9,
    });
  });

  describe('initialization', () => {
    it('should initialize with given actions', () => {
      expect(agent.actions).toHaveLength(3);
      expect(agent.actionIds).toContain('aggressive');
    });

    it('should initialize Q-table to zeros', () => {
      const allZero = Array.from(agent.qTable).every(v => v === 0);
      expect(allZero).toBe(true);
    });

    it('should throw error with no actions', () => {
      expect(() => new QLearningAgent({ actions: [] })).toThrow();
    });
  });

  describe('state encoding', () => {
    it('should convert state to index correctly', () => {
      const state1 = { driftLevel: 'high', epochCount: 'early', budgetUsed: 'low' };
      const state2 = { driftLevel: 'high', epochCount: 'early', budgetUsed: 'low' };
      const state3 = { driftLevel: 'low', epochCount: 'late', budgetUsed: 'high' };

      const idx1 = agent._stateToIndex(state1);
      const idx2 = agent._stateToIndex(state2);
      const idx3 = agent._stateToIndex(state3);

      expect(idx1).toBe(idx2);
      expect(idx1).not.toBe(idx3);
    });

    it('should throw error for invalid state', () => {
      const invalidState = { driftLevel: 'invalid', epochCount: 'early', budgetUsed: 'low' };
      expect(() => agent._stateToIndex(invalidState)).toThrow();
    });
  });

  describe('action selection', () => {
    it('should select action from available actions', () => {
      const state = { driftLevel: 'high', epochCount: 'early', budgetUsed: 'low' };
      const action = agent.selectAction(state);

      expect(agent.actionIds).toContain(action);
    });

    it('should support deterministic selection', () => {
      const state = { driftLevel: 'high', epochCount: 'early', budgetUsed: 'low' };

      // Set a clear best action
      agent._setQValue(state, 'balanced', 10);

      const action = agent.selectAction(state, { explore: false });
      expect(action).toBe('balanced');
    });

    it('should explore sometimes with epsilon-greedy', () => {
      const state = { driftLevel: 'medium', epochCount: 'middle', budgetUsed: 'medium' };

      // Set best action
      agent._setQValue(state, 'conservative', 100);

      const actions = [];
      for (let i = 0; i < 50; i++) {
        actions.push(agent.selectAction(state, { explore: true }));
      }

      // Should have some exploration (not all conservative)
      const unique = new Set(actions);
      expect(unique.size).toBeGreaterThan(1);
    });
  });

  describe('Q-learning updates', () => {
    it('should update Q-values based on reward', () => {
      const state = { driftLevel: 'high', epochCount: 'early', budgetUsed: 'low' };
      const nextState = { driftLevel: 'medium', epochCount: 'early', budgetUsed: 'low' };

      const initialQ = agent._getQValue(state, 'balanced');
      agent.learn(state, 'balanced', 10, nextState, false);
      const updatedQ = agent._getQValue(state, 'balanced');

      expect(updatedQ).not.toBe(initialQ);
      expect(updatedQ).toBeGreaterThan(initialQ);
    });

    it('should handle terminal states correctly', () => {
      const state = { driftLevel: 'low', epochCount: 'late', budgetUsed: 'high' };
      const terminalState = { driftLevel: 'converged', epochCount: 'late', budgetUsed: 'high' };

      agent.learn(state, 'aggressive', 20, terminalState, true);

      const q = agent._getQValue(state, 'aggressive');
      expect(q).toBeGreaterThan(0);
    });

    it('should build experience replay buffer', () => {
      const state = { driftLevel: 'medium', epochCount: 'middle', budgetUsed: 'medium' };
      const nextState = { driftLevel: 'low', epochCount: 'middle', budgetUsed: 'medium' };

      for (let i = 0; i < 10; i++) {
        agent.learn(state, 'balanced', 5, nextState, false);
      }

      expect(agent.experienceReplay.length).toBeGreaterThan(0);
      expect(agent.experienceReplay.length).toBeLessThanOrEqual(agent.maxReplaySize);
    });
  });

  describe('epsilon decay', () => {
    it('should decay epsilon over episodes', () => {
      const initialEpsilon = agent.epsilon;

      agent.endEpisode();
      agent.endEpisode();
      agent.endEpisode();

      expect(agent.epsilon).toBeLessThan(initialEpsilon);
      expect(agent.epsilon).toBeGreaterThanOrEqual(agent.minEpsilon);
    });

    it('should not decay below minimum epsilon', () => {
      agent.epsilon = agent.minEpsilon;

      for (let i = 0; i < 100; i++) {
        agent.endEpisode();
      }

      expect(agent.epsilon).toBe(agent.minEpsilon);
    });
  });

  describe('policy extraction', () => {
    it('should generate policy for all states', () => {
      const policy = agent.getPolicy();

      const expectedStates =
        agent.driftLevels.length *
        agent.epochCounts.length *
        agent.budgetUsed.length;

      expect(policy.size).toBe(expectedStates);
    });

    it('should recommend best action per state', () => {
      const state = { driftLevel: 'high', epochCount: 'early', budgetUsed: 'low' };

      // Train agent
      agent._setQValue(state, 'aggressive', 100);

      const policy = agent.getPolicy();
      const stateKey = 'high-early-low';

      expect(policy.get(stateKey)).toBe('aggressive');
    });
  });

  describe('persistence', () => {
    it('should export Q-table', () => {
      const state = { driftLevel: 'medium', epochCount: 'middle', budgetUsed: 'medium' };
      agent._setQValue(state, 'balanced', 42);

      const exported = agent.exportQTable();

      expect(exported.qTable).toBeInstanceOf(Array);
      expect(exported.episodeCount).toBe(agent.episodeCount);
      expect(exported.qTable).toContain(42);
    });

    it('should import Q-table', () => {
      const state = { driftLevel: 'low', epochCount: 'late', budgetUsed: 'high' };
      const exported = agent.exportQTable();

      const newAgent = new QLearningAgent({ actions });
      newAgent.importQTable(exported);

      const originalQ = agent._getQValue(state, 'conservative');
      const importedQ = newAgent._getQValue(state, 'conservative');

      expect(importedQ).toBe(originalQ);
    });
  });

  describe('summary and statistics', () => {
    it('should track action statistics', () => {
      const state = { driftLevel: 'high', epochCount: 'early', budgetUsed: 'low' };
      const nextState = { driftLevel: 'medium', epochCount: 'early', budgetUsed: 'low' };

      agent.learn(state, 'aggressive', 10, nextState, false);
      agent.learn(state, 'aggressive', 5, nextState, false);

      const summary = agent.getSummary();
      const aggressiveStats = summary.actionStats.find(s => s.actionId === 'aggressive');

      expect(aggressiveStats.selectionCount).toBe(2);
      expect(aggressiveStats.avgReward).toBe(7.5);
    });

    it('should reset all state', () => {
      const state = { driftLevel: 'high', epochCount: 'early', budgetUsed: 'low' };
      agent.learn(state, 'balanced', 10, state, false);

      agent.reset();

      expect(agent.episodeCount).toBe(0);
      expect(agent.totalReward).toBe(0);
      expect(agent.experienceReplay).toHaveLength(0);
    });
  });
});

describe('calculateReward', () => {
  it('should reward fast convergence', () => {
    const fastReward = calculateReward({
      convergenceTime: 2,
      compressionRatio: 0.5,
    });

    const slowReward = calculateReward({
      convergenceTime: 10,
      compressionRatio: 0.5,
    });

    expect(fastReward).toBeGreaterThan(slowReward);
  });

  it('should reward high compression', () => {
    const highCompressionReward = calculateReward({
      convergenceTime: 5,
      compressionRatio: 0.9,
    });

    const lowCompressionReward = calculateReward({
      convergenceTime: 5,
      compressionRatio: 0.1,
    });

    expect(highCompressionReward).toBeGreaterThan(lowCompressionReward);
  });

  it('should penalize high budget usage', () => {
    const lowBudgetReward = calculateReward({
      convergenceTime: 5,
      compressionRatio: 0.5,
      budgetUsed: 0.1,
    });

    const highBudgetReward = calculateReward({
      convergenceTime: 5,
      compressionRatio: 0.5,
      budgetUsed: 0.9,
    });

    expect(lowBudgetReward).toBeGreaterThan(highBudgetReward);
  });
});

describe('AnomalyDetector', () => {
  /** @type {AnomalyDetector} */
  let detector;

  beforeEach(() => {
    detector = new AnomalyDetector({
      zScoreThreshold: 2.0,
      iqrMultiplier: 1.5,
      windowSize: 5,
    });
  });

  describe('observation tracking', () => {
    it('should record observations correctly', () => {
      detector.observe({
        epoch: 1,
        drift: 10,
        normalized: 0.5,
        timestamp: Date.now(),
      });

      expect(detector.observations).toHaveLength(1);
      expect(detector.driftIndex).toBe(1);
    });

    it('should expand arrays when needed', () => {
      const initialSize = detector.driftValues.length;

      for (let i = 0; i < initialSize + 10; i++) {
        detector.observe({
          epoch: i,
          drift: i,
          normalized: i / 100,
          timestamp: Date.now() + i * 1000,
        });
      }

      expect(detector.driftValues.length).toBeGreaterThan(initialSize);
    });
  });

  describe('statistical anomaly detection', () => {
    it('should detect Z-score outliers', () => {
      // Add normal observations
      for (let i = 0; i < 10; i++) {
        detector.observe({
          epoch: i,
          drift: 5,
          normalized: 0.5,
          timestamp: Date.now() + i * 1000,
        });
      }

      // Add outlier
      detector.observe({
        epoch: 10,
        drift: 50,
        normalized: 5.0,
        timestamp: Date.now() + 10000,
      });

      const anomalies = detector.detect({ useZScore: true, useIQR: false });

      expect(anomalies.length).toBeGreaterThan(0);
      expect(anomalies[0].type).toBe('statistical-outlier');
    });

    it('should detect IQR outliers', () => {
      // Add observations with outlier
      const values = [0.1, 0.15, 0.12, 0.14, 0.13, 0.11, 0.16, 0.12, 5.0];

      values.forEach((val, i) => {
        detector.observe({
          epoch: i,
          drift: val * 10,
          normalized: val,
          timestamp: Date.now() + i * 1000,
        });
      });

      const anomalies = detector.detect({ useZScore: false, useIQR: true });

      expect(anomalies.some(a => a.type === 'statistical-outlier')).toBe(true);
    });

    it('should not detect anomalies in normal data', () => {
      // Add normal, consistent observations
      for (let i = 0; i < 10; i++) {
        detector.observe({
          epoch: i,
          drift: 5 + Math.random() * 0.5,
          normalized: 0.5 + Math.random() * 0.05,
          timestamp: Date.now() + i * 1000,
        });
      }

      const anomalies = detector.detect();

      expect(anomalies).toHaveLength(0);
    });
  });

  describe('pattern anomaly detection', () => {
    it('should detect drift spikes', () => {
      // Normal drift
      for (let i = 0; i < 5; i++) {
        detector.observe({
          epoch: i,
          drift: 5,
          normalized: 0.1,
          timestamp: Date.now() + i * 1000,
        });
      }

      // Sudden spike
      detector.observe({
        epoch: 5,
        drift: 50,
        normalized: 0.5,
        timestamp: Date.now() + 5000,
      });

      const anomalies = detector.detect({ detectPatterns: true });

      expect(anomalies.some(a => a.type === 'drift-spike')).toBe(true);
    });

    it('should detect drift plateau', () => {
      // Non-decreasing drift over window
      for (let i = 0; i < 7; i++) {
        detector.observe({
          epoch: i,
          drift: 10,
          normalized: 0.1,
          timestamp: Date.now() + i * 1000,
        });
      }

      const anomalies = detector.detect({ detectPatterns: true });

      expect(anomalies.some(a => a.type === 'drift-plateau')).toBe(true);
    });
  });

  describe('guard violation prediction', () => {
    it('should predict guard violation for increasing drift', () => {
      // Increasing drift trend
      for (let i = 0; i < 10; i++) {
        detector.observe({
          epoch: i,
          drift: i * 0.5,
          normalized: i * 0.05,
          timestamp: Date.now() + i * 1000,
        });
      }

      const prediction = detector.predictGuardViolation({
        name: 'max-drift',
        driftThreshold: 0.8,
      });

      expect(prediction.risk).toBeGreaterThan(0);
      expect(prediction.confidence).toBeGreaterThan(0);
    });

    it('should detect immediate violation', () => {
      detector.observe({
        epoch: 1,
        drift: 100,
        normalized: 1.5,
        timestamp: Date.now(),
      });

      const prediction = detector.predictGuardViolation({
        name: 'max-drift',
        driftThreshold: 1.0,
      });

      expect(prediction.willViolate).toBe(true);
      expect(prediction.risk).toBe(1.0);
      expect(prediction.estimatedEpochs).toBe(0);
    });

    it('should have low confidence with insufficient data', () => {
      detector.observe({
        epoch: 1,
        drift: 5,
        normalized: 0.5,
        timestamp: Date.now(),
      });

      const prediction = detector.predictGuardViolation({
        name: 'test',
        driftThreshold: 1.0,
      });

      expect(prediction.confidence).toBeLessThan(0.3);
    });
  });

  describe('summary and reset', () => {
    it('should provide accurate summary', () => {
      for (let i = 0; i < 5; i++) {
        detector.observe({
          epoch: i,
          drift: 10 - i,
          normalized: (10 - i) / 10,
          timestamp: Date.now() + i * 1000,
        });
      }

      const summary = detector.getSummary();

      expect(summary.totalObservations).toBe(5);
      expect(summary.statistics.mean).toBeGreaterThan(0);
    });

    it('should reset state completely', () => {
      detector.observe({
        epoch: 1,
        drift: 5,
        normalized: 0.5,
        timestamp: Date.now(),
      });

      detector.reset();

      expect(detector.observations).toHaveLength(0);
      expect(detector.driftIndex).toBe(0);
      expect(detector.detectedAnomalies.size).toBe(0);
    });

    it('should clear anomalies without resetting observations', () => {
      detector.observe({
        epoch: 1,
        drift: 5,
        normalized: 0.5,
        timestamp: Date.now(),
      });

      detector.detectedAnomalies.set(1, {
        type: 'drift-spike',
        severity: 0.5,
        description: 'Test',
        epoch: 1,
        value: 0.5,
        threshold: 0.3,
        suggestions: [],
      });

      detector.clearAnomalies();

      expect(detector.observations).toHaveLength(1);
      expect(detector.detectedAnomalies.size).toBe(0);
    });
  });
});
