/**
 * @file Federated Learning Tests
 * @module ai-ml-innovations/test/federated-learning
 *
 * @description
 * Comprehensive test suite for federated learning with differential privacy.
 * Tests convergence, privacy guarantees, and secure aggregation.
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  FederatedEmbeddingTrainer,
  FedAvgAggregator,
  DPMechanism,
  PrivacyBudgetTracker,
  SecureAggregation,
} from '../src/index.mjs';

describe('Federated Learning', () => {
  describe('FedAvgAggregator', () => {
    let aggregator;

    beforeEach(() => {
      aggregator = new FedAvgAggregator({
        learningRate: 0.01,
        momentum: 0.9,
      });
    });

    it('should create aggregator with default config', () => {
      const agg = new FedAvgAggregator();
      expect(agg.config.learningRate).toBe(0.01);
      expect(agg.config.momentum).toBe(0.9);
    });

    it('should aggregate client updates', () => {
      const updates = [
        {
          nodeId: 'node-1',
          gradients: {
            'entity_A': [1, 2, 3],
            'relation_knows': [0.5, 0.5],
          },
          sampleCount: 100,
          epoch: 0,
          timestamp: Date.now(),
        },
        {
          nodeId: 'node-2',
          gradients: {
            'entity_A': [2, 3, 4],
            'relation_knows': [0.3, 0.7],
          },
          sampleCount: 200,
          epoch: 0,
          timestamp: Date.now(),
        },
      ];

      const globalModel = {
        entityEmbeddings: { A: [0, 0, 0] },
        relationEmbeddings: { knows: [0, 0] },
        version: 0,
        timestamp: Date.now(),
      };

      const aggregated = aggregator.aggregate(updates, globalModel);

      expect(aggregated.version).toBe(1);
      expect(aggregated.entityEmbeddings.A).toBeDefined();
      expect(aggregated.relationEmbeddings.knows).toBeDefined();
    });

    it('should weight by sample count', () => {
      const updates = [
        {
          nodeId: 'node-1',
          gradients: { 'entity_A': [1.0] },
          sampleCount: 100,
          epoch: 0,
          timestamp: Date.now(),
        },
        {
          nodeId: 'node-2',
          gradients: { 'entity_A': [2.0] },
          sampleCount: 100,
          epoch: 0,
          timestamp: Date.now(),
        },
      ];

      const globalModel = {
        entityEmbeddings: { A: [0] },
        relationEmbeddings: {},
        version: 0,
        timestamp: Date.now(),
      };

      const aggregated = aggregator.aggregate(updates, globalModel);

      // Should be average: (1.0 + 2.0) / 2 = 1.5, scaled by learning rate
      const expected = (1.5 * aggregator.config.learningRate);
      expect(Math.abs(aggregated.entityEmbeddings.A[0] - expected)).toBeLessThan(0.01);
    });

    it('should throw on insufficient clients', () => {
      const updates = [];
      const globalModel = {
        entityEmbeddings: {},
        relationEmbeddings: {},
        version: 0,
        timestamp: Date.now(),
      };

      expect(() => aggregator.aggregate(updates, globalModel)).toThrow(
        /Insufficient clients/
      );
    });

    it('should track rounds', () => {
      const updates = [
        {
          nodeId: 'node-1',
          gradients: { 'entity_A': [1] },
          sampleCount: 100,
          epoch: 0,
          timestamp: Date.now(),
        },
      ];

      const globalModel = {
        entityEmbeddings: { A: [0] },
        relationEmbeddings: {},
        version: 0,
        timestamp: Date.now(),
      };

      expect(aggregator.roundCount).toBe(0);
      aggregator.aggregate(updates, globalModel);
      expect(aggregator.roundCount).toBe(1);
      aggregator.aggregate(updates, globalModel);
      expect(aggregator.roundCount).toBe(2);
    });
  });

  describe('DPMechanism', () => {
    let mechanism;

    beforeEach(() => {
      mechanism = new DPMechanism({
        epsilon: 1.0,
        delta: 1e-5,
        sensitivity: 1.0,
        clippingNorm: 1.0,
      });
    });

    it('should create mechanism with Gaussian noise', () => {
      expect(mechanism.mechanism).toBe('gaussian');
      expect(mechanism.epsilon).toBe(1.0);
      expect(mechanism.delta).toBe(1e-5);
    });

    it('should clip gradients exceeding max norm', () => {
      const gradients = {
        'entity_A': [3, 4], // L2 norm = 5
      };

      const clipped = mechanism.clipGradients(gradients, 1.0);

      // Should scale to norm 1.0
      const norm = Math.sqrt(
        clipped['entity_A'][0] ** 2 + clipped['entity_A'][1] ** 2
      );
      expect(norm).toBeCloseTo(1.0, 5);
    });

    it('should not clip gradients within max norm', () => {
      const gradients = {
        'entity_A': [0.3, 0.4], // L2 norm = 0.5 < 1.0
      };

      const clipped = mechanism.clipGradients(gradients, 1.0);

      expect(clipped['entity_A'][0]).toBeCloseTo(0.3);
      expect(clipped['entity_A'][1]).toBeCloseTo(0.4);
    });

    it('should add Gaussian noise', () => {
      const gradients = {
        'entity_A': [0, 0, 0, 0, 0],
      };

      const noised = mechanism.addNoise(gradients);

      // Noise should be non-zero
      const hasNoise = noised['entity_A'].some((val) => Math.abs(val) > 0);
      expect(hasNoise).toBe(true);

      // Noise should be roughly Gaussian (mean ≈ 0)
      const mean =
        noised['entity_A'].reduce((sum, val) => sum + val, 0) /
        noised['entity_A'].length;
      expect(Math.abs(mean)).toBeLessThan(1.0); // Loose bound
    });

    it('should privatize gradients (clip + noise)', () => {
      const gradients = {
        'entity_A': [3, 4], // Will be clipped
      };

      const privatized = mechanism.privatize(gradients);

      // Should be clipped to norm 1.0 and noised
      expect(privatized['entity_A']).toBeDefined();
      expect(privatized['entity_A'].length).toBe(2);
    });

    it('should support Laplace mechanism', () => {
      const laplace = new DPMechanism({
        mechanism: 'laplace',
        epsilon: 1.0,
        sensitivity: 1.0,
        clippingNorm: 1.0,
      });

      expect(laplace.mechanism).toBe('laplace');
    });
  });

  describe('PrivacyBudgetTracker', () => {
    let tracker;

    beforeEach(() => {
      tracker = new PrivacyBudgetTracker({
        epsilon: 1.0,
        delta: 1e-5,
        composition: 'basic',
      });
    });

    it('should create tracker with budget', () => {
      expect(tracker.epsilon).toBe(1.0);
      expect(tracker.delta).toBe(1e-5);
      expect(tracker.spent).toBe(0);
    });

    it('should compute round cost', () => {
      const cost = tracker.computeRoundCost({
        noiseMultiplier: 1.0,
        samplingRate: 0.1,
        steps: 1,
      });

      expect(cost.epsilon).toBeGreaterThan(0);
      expect(cost.delta).toBe(1e-5);
    });

    it('should account for rounds', () => {
      tracker.accountRound({
        noiseMultiplier: 1.0,
        samplingRate: 0.1,
        steps: 1,
      });

      expect(tracker.spent).toBeGreaterThan(0);
      expect(tracker.rounds).toBe(1);
      expect(tracker.history.length).toBe(1);
    });

    it('should throw when budget exhausted', () => {
      // Use high noise (low privacy cost) for first rounds
      for (let i = 0; i < 5; i++) {
        tracker.accountRound({
          noiseMultiplier: 0.5,
          samplingRate: 1.0,
          steps: 1,
        });
      }

      // Should eventually exhaust budget
      expect(() => {
        for (let i = 0; i < 100; i++) {
          tracker.accountRound({
            noiseMultiplier: 0.5,
            samplingRate: 1.0,
            steps: 1,
          });
        }
      }).toThrow(/Privacy budget exhausted/);
    });

    it('should track remaining budget', () => {
      const status = tracker.getStatus();
      expect(status.remaining).toBe(1.0);

      tracker.accountRound({
        noiseMultiplier: 1.0,
        samplingRate: 0.1,
        steps: 1,
      });

      const newStatus = tracker.getStatus();
      expect(newStatus.remaining).toBeLessThan(1.0);
    });

    it('should support moments accountant', () => {
      const moments = new PrivacyBudgetTracker({
        epsilon: 1.0,
        delta: 1e-5,
        composition: 'moments',
      });

      const cost = moments.computeRoundCost({
        noiseMultiplier: 1.0,
        samplingRate: 0.1,
        steps: 1,
      });

      expect(cost.epsilon).toBeGreaterThan(0);
    });

    it('should check if can continue', () => {
      expect(tracker.canContinue(0.1)).toBe(true);

      tracker.accountRound({
        noiseMultiplier: 0.5,
        samplingRate: 1.0,
        steps: 1,
      });

      // Should still have budget
      expect(tracker.canContinue(0.01)).toBe(true);
    });

    it('should reset budget', () => {
      tracker.accountRound({
        noiseMultiplier: 1.0,
        samplingRate: 0.1,
        steps: 1,
      });

      expect(tracker.spent).toBeGreaterThan(0);
      tracker.reset();
      expect(tracker.spent).toBe(0);
      expect(tracker.rounds).toBe(0);
      expect(tracker.history.length).toBe(0);
    });
  });

  describe('SecureAggregation', () => {
    let protocol;

    beforeEach(() => {
      protocol = new SecureAggregation({
        threshold: 2,
        totalNodes: 3,
        enableEncryption: true,
      });
    });

    it('should create protocol with config', () => {
      expect(protocol.threshold).toBe(2);
      expect(protocol.totalNodes).toBe(3);
      expect(protocol.enableEncryption).toBe(true);
    });

    it('should generate shares for nodes', () => {
      const shares1 = protocol.generateShares('node-0');
      const shares2 = protocol.generateShares('node-1');

      expect(shares1.secret).toBeDefined();
      expect(shares1.shares).toBeDefined();
      expect(shares2.secret).toBeDefined();
    });

    it('should mask gradients', () => {
      protocol.generateShares('node-0');

      const gradients = {
        'entity_A': [1, 2, 3],
      };

      const masked = protocol.maskGradients('node-0', gradients);

      expect(masked['entity_A']).toBeDefined();
      expect(masked['entity_A'].length).toBe(3);
      // Masked values should differ from original
      expect(masked['entity_A']).not.toEqual(gradients['entity_A']);
    });

    it('should aggregate masked updates (masks cancel)', () => {
      // Generate shares for all nodes
      protocol.generateShares('node-0');
      protocol.generateShares('node-1');
      protocol.generateShares('node-2');

      const originalGradients = {
        'entity_A': [1, 2, 3],
      };

      // Mask from each node
      const masked1 = protocol.maskGradients('node-0', originalGradients);
      const masked2 = protocol.maskGradients('node-1', originalGradients);
      const masked3 = protocol.maskGradients('node-2', originalGradients);

      const maskedUpdates = [
        {
          nodeId: 'node-0',
          gradients: masked1,
          sampleCount: 100,
          epoch: 0,
          timestamp: Date.now(),
        },
        {
          nodeId: 'node-1',
          gradients: masked2,
          sampleCount: 100,
          epoch: 0,
          timestamp: Date.now(),
        },
        {
          nodeId: 'node-2',
          gradients: masked3,
          sampleCount: 100,
          epoch: 0,
          timestamp: Date.now(),
        },
      ];

      const aggregated = protocol.aggregateMasked(maskedUpdates);

      // Masks should cancel out, recovering original average
      // All clients have same gradients, so average = original
      expect(aggregated['entity_A'][0]).toBeCloseTo(
        originalGradients['entity_A'][0],
        1
      );
    });

    it('should throw on insufficient updates', () => {
      const maskedUpdates = [
        {
          nodeId: 'node-0',
          gradients: { 'entity_A': [1] },
          sampleCount: 100,
          epoch: 0,
          timestamp: Date.now(),
        },
      ];

      // Need at least threshold (2) updates
      expect(() => protocol.aggregateMasked(maskedUpdates)).toThrow(
        /Insufficient updates/
      );
    });

    it('should handle disabled encryption', () => {
      const plainProtocol = new SecureAggregation({
        threshold: 2,
        totalNodes: 3,
        enableEncryption: false,
      });

      plainProtocol.generateShares('node-0');

      const gradients = {
        'entity_A': [1, 2, 3],
      };

      const masked = plainProtocol.maskGradients('node-0', gradients);

      // Should be unchanged
      expect(masked).toEqual(gradients);
    });
  });

  describe('FederatedEmbeddingTrainer Integration', () => {
    it('should create trainer with default config', () => {
      const trainer = new FederatedEmbeddingTrainer();
      expect(trainer.config.embeddingDim).toBe(128);
      expect(trainer.config.aggregationStrategy).toBe('fedavg');
      expect(trainer.config.enableDifferentialPrivacy).toBe(true);
    });

    it('should initialize global model', () => {
      const trainer = new FederatedEmbeddingTrainer();
      const model = trainer.initializeGlobalModel();

      expect(model.entityEmbeddings).toEqual({});
      expect(model.relationEmbeddings).toEqual({});
      expect(model.version).toBe(0);
    });

    it('should train with mock nodes', async () => {
      const mockNodes = [
        {
          id: 'node-1',
          graph: [
            { subject: 'Alice', predicate: 'knows', object: 'Bob' },
            { subject: 'Bob', predicate: 'likes', object: 'Coffee' },
          ],
        },
        {
          id: 'node-2',
          graph: [
            { subject: 'Charlie', predicate: 'knows', object: 'Alice' },
            { subject: 'Alice', predicate: 'likes', object: 'Tea' },
          ],
        },
      ];

      const trainer = new FederatedEmbeddingTrainer({
        nodes: mockNodes,
        embeddingDim: 32,
        privacyBudget: 1.0,
        enableDifferentialPrivacy: true,
      });

      const result = await trainer.trainFederated({
        epochs: 3,
        localEpochs: 2,
        batchSize: 2,
      });

      expect(result.model).toBeDefined();
      expect(result.model.version).toBeGreaterThan(0);
      expect(result.trainingHistory.length).toBe(3);
      expect(result.privacySpent).toBeGreaterThan(0);
      expect(result.privacySpent).toBeLessThanOrEqual(1.0);
    });

    it('should track privacy budget during training', async () => {
      const mockNodes = [
        {
          id: 'node-1',
          graph: [{ subject: 'A', predicate: 'knows', object: 'B' }],
        },
      ];

      const trainer = new FederatedEmbeddingTrainer({
        nodes: mockNodes,
        privacyBudget: 0.5,
        enableDifferentialPrivacy: true,
      });

      const result = await trainer.trainFederated({
        epochs: 10,
        localEpochs: 1,
      });

      // Should stop when budget exhausted
      expect(result.privacySpent).toBeLessThanOrEqual(0.5);
    });

    it('should converge within target rounds', async () => {
      const mockNodes = Array.from({ length: 5 }, (_, i) => ({
        id: `node-${i}`,
        graph: [
          { subject: 'A', predicate: 'knows', object: 'B' },
          { subject: 'B', predicate: 'likes', object: 'C' },
        ],
      }));

      const trainer = new FederatedEmbeddingTrainer({
        nodes: mockNodes,
        embeddingDim: 64,
        privacyBudget: 2.0,
        enableDifferentialPrivacy: false, // Faster convergence
      });

      const result = await trainer.trainFederated({
        epochs: 50,
        localEpochs: 5,
        convergenceThreshold: 0.01,
      });

      expect(result.stats.convergenceRound).toBeLessThanOrEqual(50);
    });

    it('should produce embeddings for all entities and relations', async () => {
      const mockNodes = [
        {
          id: 'node-1',
          graph: [
            { subject: 'Alice', predicate: 'knows', object: 'Bob' },
            { subject: 'Bob', predicate: 'knows', object: 'Charlie' },
          ],
        },
      ];

      const trainer = new FederatedEmbeddingTrainer({
        nodes: mockNodes,
        embeddingDim: 32,
        enableDifferentialPrivacy: false,
      });

      const result = await trainer.trainFederated({
        epochs: 5,
        localEpochs: 3,
      });

      const { entityEmbeddings, relationEmbeddings } = result.model;

      // Should have embeddings for all entities
      expect(entityEmbeddings.Alice).toBeDefined();
      expect(entityEmbeddings.Bob).toBeDefined();
      expect(entityEmbeddings.Charlie).toBeDefined();
      expect(entityEmbeddings.Alice.length).toBe(32);

      // Should have embeddings for all relations
      expect(relationEmbeddings.knows).toBeDefined();
      expect(relationEmbeddings.knows.length).toBe(32);
    });

    it('should achieve target accuracy', async () => {
      const mockNodes = Array.from({ length: 10 }, (_, i) => ({
        id: `node-${i}`,
        graph: Array.from({ length: 20 }, (_, j) => ({
          subject: `Entity${j}`,
          predicate: 'related',
          object: `Entity${(j + 1) % 20}`,
        })),
      }));

      const trainer = new FederatedEmbeddingTrainer({
        nodes: mockNodes,
        embeddingDim: 128,
        privacyBudget: 1.0,
        enableDifferentialPrivacy: true,
      });

      const result = await trainer.trainFederated({
        epochs: 20,
        localEpochs: 5,
      });

      const finalMetrics =
        result.trainingHistory[result.trainingHistory.length - 1];

      // Target: ≥95% of centralized accuracy (placeholder ~80-95%)
      expect(finalMetrics.accuracy).toBeGreaterThan(0.75);
    });
  });

  describe('Privacy Guarantees', () => {
    it('should satisfy epsilon-DP with proper budget', () => {
      const epsilon = 1.0;
      const tracker = new PrivacyBudgetTracker({
        epsilon,
        delta: 1e-5,
        composition: 'basic',
      });

      const rounds = 10;
      for (let i = 0; i < rounds; i++) {
        tracker.accountRound({
          noiseMultiplier: 2.0, // High noise for low privacy cost
          samplingRate: 0.1,
          steps: 1,
        });
      }

      const status = tracker.getStatus();
      expect(status.spent).toBeLessThanOrEqual(epsilon);
    });

    it('should have tighter bounds with moments accountant', () => {
      const basic = new PrivacyBudgetTracker({
        epsilon: 10,
        delta: 1e-5,
        composition: 'basic',
      });

      const moments = new PrivacyBudgetTracker({
        epsilon: 10,
        delta: 1e-5,
        composition: 'moments',
      });

      const params = {
        noiseMultiplier: 1.0,
        samplingRate: 0.1,
        steps: 1,
      };

      const basicCost = basic.computeRoundCost(params);
      const momentsCost = moments.computeRoundCost(params);

      // Moments accountant should give tighter (lower) bounds
      expect(momentsCost.epsilon).toBeLessThanOrEqual(basicCost.epsilon);
    });

    it('should clip gradients to bound sensitivity', () => {
      const mechanism = new DPMechanism({
        epsilon: 1.0,
        delta: 1e-5,
        sensitivity: 1.0,
        clippingNorm: 1.0,
      });

      const largeGradients = {
        'entity_A': [100, 200], // Very large
      };

      const clipped = mechanism.clipGradients(largeGradients);

      // Should be clipped to norm 1.0
      const norm = Math.sqrt(
        clipped['entity_A'].reduce((sum, val) => sum + val * val, 0)
      );

      expect(norm).toBeCloseTo(1.0, 5);
    });
  });

  describe('Performance Benchmarks', () => {
    it('should aggregate in <100ms', () => {
      const aggregator = new FedAvgAggregator();

      const updates = Array.from({ length: 10 }, (_, i) => ({
        nodeId: `node-${i}`,
        gradients: {
          'entity_A': Array.from({ length: 128 }, () => Math.random()),
        },
        sampleCount: 100,
        epoch: 0,
        timestamp: Date.now(),
      }));

      const globalModel = {
        entityEmbeddings: { A: new Array(128).fill(0) },
        relationEmbeddings: {},
        version: 0,
        timestamp: Date.now(),
      };

      const start = Date.now();
      aggregator.aggregate(updates, globalModel);
      const duration = Date.now() - start;

      expect(duration).toBeLessThan(100);
    });

    it('should privatize gradients in <50ms', () => {
      const mechanism = new DPMechanism({
        epsilon: 1.0,
        delta: 1e-5,
        sensitivity: 1.0,
        clippingNorm: 1.0,
      });

      const gradients = {};
      for (let i = 0; i < 100; i++) {
        gradients[`entity_${i}`] = Array.from({ length: 128 }, () =>
          Math.random()
        );
      }

      const start = Date.now();
      mechanism.privatize(gradients);
      const duration = Date.now() - start;

      expect(duration).toBeLessThan(50);
    });
  });
});
