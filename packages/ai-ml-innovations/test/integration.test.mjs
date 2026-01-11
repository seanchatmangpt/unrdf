/**
 * @file Integration tests for AI/ML Innovations
 * @module ai-ml-innovations/test
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  TemporalGraphNeuralNetwork,
  NeuralSymbolicReasoner,
  FederatedEmbeddingTrainer,
} from '../src/index.mjs';

describe('AI/ML Innovations Integration Tests', () => {
  describe('Temporal Graph Neural Network', () => {
    let tgnn;

    beforeEach(() => {
      tgnn = new TemporalGraphNeuralNetwork({
        embeddingDim: 64, // Smaller for faster tests
        temporalWindow: 5,
        aggregation: 'attention',
        attentionHeads: 2,
      });
    });

    it('should initialize with correct configuration', () => {
      expect(tgnn.config.embeddingDim).toBe(64);
      expect(tgnn.config.temporalWindow).toBe(5);
      expect(tgnn.config.aggregation).toBe('attention');
      expect(tgnn.config.attentionHeads).toBe(2);
    });

    it('should train on temporal snapshots', async () => {
      // Create mock temporal snapshots
      const snapshots = [];
      for (let i = 0; i < 3; i++) {
        snapshots.push({
          timestamp: Date.now() + i * 1000,
          receiptId: `receipt_${i}`,
          graph: createMockGraph(5), // 5 triples
        });
      }

      const results = await tgnn.train(snapshots, { epochs: 10 });

      expect(results.nodeEmbeddings).toBeGreaterThan(0);
      expect(results.edgeEmbeddings).toBeGreaterThan(0);
      expect(results.temporalWindow).toBe(5);
    });

    it('should predict future links', async () => {
      // Setup: train on snapshots
      const snapshots = [];
      for (let i = 0; i < 3; i++) {
        snapshots.push({
          timestamp: Date.now() + i * 1000,
          receiptId: `receipt_${i}`,
          graph: createMockGraph(5),
        });
      }

      await tgnn.train(snapshots, { epochs: 5 });

      // Add temporal history
      const nodeId = 'http://example.org/entity1';
      tgnn.temporalHistory.set(nodeId, snapshots);

      const predictions = await tgnn.predictFutureLinks(nodeId, 5, { topK: 3 });

      expect(predictions).toBeInstanceOf(Array);
      expect(predictions.length).toBeLessThanOrEqual(3);

      if (predictions.length > 0) {
        expect(predictions[0]).toHaveProperty('subject');
        expect(predictions[0]).toHaveProperty('predicate');
        expect(predictions[0]).toHaveProperty('object');
        expect(predictions[0]).toHaveProperty('score');
        expect(predictions[0].score).toBeGreaterThanOrEqual(0);
        expect(predictions[0].score).toBeLessThanOrEqual(1);
      }
    });

    it('should aggregate temporal features with attention', async () => {
      const snapshots = [];
      for (let i = 0; i < 3; i++) {
        snapshots.push({
          timestamp: Date.now() + i * 1000,
          receiptId: `receipt_${i}`,
          graph: createMockGraph(5),
        });
      }

      await tgnn.train(snapshots, { epochs: 5 });

      const nodeId = 'http://example.org/entity1';
      const aggregated = await tgnn.aggregateTemporalFeatures(nodeId, snapshots);

      expect(aggregated).toBeInstanceOf(Array);
      expect(aggregated.length).toBe(64); // embeddingDim
    });

    it('should track statistics', async () => {
      const stats = tgnn.getStats();

      expect(stats).toHaveProperty('predictions');
      expect(stats).toHaveProperty('aggregations');
      expect(stats).toHaveProperty('trainingSessions');
      expect(stats).toHaveProperty('avgPredictionTime');
      expect(stats).toHaveProperty('config');
    });
  });

  describe('Neural-Symbolic Reasoner', () => {
    let reasoner;

    beforeEach(() => {
      reasoner = new NeuralSymbolicReasoner({
        embeddingDim: 64,
        symbolicWeight: 0.6,
        neuralWeight: 0.4,
        minConfidence: 0.5,
      });
    });

    it('should initialize with correct configuration', () => {
      expect(reasoner.config.embeddingDim).toBe(64);
      expect(reasoner.config.symbolicWeight).toBe(0.6);
      expect(reasoner.config.neuralWeight).toBe(0.4);
      expect(reasoner.config.minConfidence).toBe(0.5);
    });

    it('should learn rule embeddings from SHACL shapes', async () => {
      const shaclShapes = [
        {
          id: 'rule1',
          name: 'Employment Rule',
          description: 'If person works at company, they are employed by company',
          conditions: [
            {
              subject: '?person',
              predicate: 'http://example.org/worksAt',
              object: '?company',
            },
          ],
          conclusion: {
            subject: '?person',
            predicate: 'http://example.org/employedBy',
            object: '?company',
          },
        },
      ];

      const results = await reasoner.learnRuleEmbeddings(shaclShapes);

      expect(results.rulesLearned).toBe(1);
      expect(results.embeddingDim).toBe(64);
      expect(reasoner.ruleEmbeddings.size).toBe(1);
    });

    it('should perform hybrid inference', async () => {
      // Learn rules first
      const shaclShapes = [
        {
          id: 'rule1',
          name: 'Test Rule',
          conditions: [
            {
              subject: 'http://example.org/Alice',
              predicate: 'http://example.org/knows',
              object: 'http://example.org/Bob',
            },
          ],
          conclusion: {
            subject: 'http://example.org/Alice',
            predicate: 'http://example.org/friendOf',
            object: 'http://example.org/Bob',
          },
        },
      ];

      await reasoner.learnRuleEmbeddings(shaclShapes);

      // Test inference
      const triple = {
        subject: 'http://example.org/Alice',
        predicate: 'http://example.org/knows',
        object: 'http://example.org/Bob',
      };

      const results = await reasoner.infer(triple);

      expect(results).toBeInstanceOf(Array);
      results.forEach((result) => {
        expect(result).toHaveProperty('triple');
        expect(result).toHaveProperty('rule');
        expect(result).toHaveProperty('confidence');
        expect(result).toHaveProperty('method');
        expect(result.method).toMatch(/symbolic|neural|hybrid/);
      });
    });

    it('should fuse symbolic and neural inferences', () => {
      const symbolic = [
        {
          triple: { subject: 'A', predicate: 'p', object: 'B' },
          rule: { id: 'r1', name: 'Rule 1', conditions: [], conclusion: {} },
          confidence: 1.0,
        },
      ];

      const neural = [
        {
          triple: { subject: 'A', predicate: 'p', object: 'C' },
          rule: { id: 'r2', name: 'Rule 2', conditions: [], conclusion: {} },
          confidence: 0.8,
        },
      ];

      const fused = reasoner.fuseInferences(symbolic, neural);

      expect(fused.length).toBe(2);
      expect(fused[0].confidence).toBeGreaterThanOrEqual(0);
      expect(fused[0].confidence).toBeLessThanOrEqual(1);
    });

    it('should track statistics', () => {
      const stats = reasoner.getStats();

      expect(stats).toHaveProperty('symbolicInferences');
      expect(stats).toHaveProperty('neuralInferences');
      expect(stats).toHaveProperty('hybridInferences');
      expect(stats).toHaveProperty('cacheSize');
      expect(stats).toHaveProperty('config');
    });
  });

  describe('Federated Embedding Trainer', () => {
    let trainer;
    let mockNodes;

    beforeEach(() => {
      mockNodes = createMockFederatedNodes(3);

      trainer = new FederatedEmbeddingTrainer({
        nodes: mockNodes,
        embeddingDim: 32,
        aggregationStrategy: 'fedavg',
        privacyBudget: 1.0,
        enableDifferentialPrivacy: true,
      });
    });

    it('should initialize with correct configuration', () => {
      expect(trainer.config.embeddingDim).toBe(32);
      expect(trainer.config.aggregationStrategy).toBe('fedavg');
      expect(trainer.config.privacyBudget).toBe(1.0);
      expect(trainer.config.enableDifferentialPrivacy).toBe(true);
    });

    it('should train federated embeddings', async () => {
      const results = await trainer.trainFederated({
        epochs: 3,
        localEpochs: 2,
        batchSize: 4,
      });

      expect(results).toHaveProperty('model');
      expect(results).toHaveProperty('trainingHistory');
      expect(results).toHaveProperty('stats');
      expect(results).toHaveProperty('privacySpent');

      expect(results.model.version).toBeGreaterThan(0);
      expect(results.trainingHistory.length).toBe(3);
      expect(results.privacySpent).toBeLessThanOrEqual(results.stats.privacyBudget);
    });

    it('should train local node', async () => {
      const globalModel = trainer.initializeGlobalModel();
      const node = mockNodes[0];

      const update = await trainer.trainLocalNode(node, globalModel, 2, 4);

      expect(update).toHaveProperty('nodeId');
      expect(update).toHaveProperty('gradients');
      expect(update).toHaveProperty('sampleCount');
      expect(update).toHaveProperty('timestamp');
      expect(update.nodeId).toBe(node.id);
    });

    it('should aggregate updates using FedAvg', async () => {
      const globalModel = trainer.initializeGlobalModel();

      const updates = await Promise.all(
        mockNodes.map((node) => trainer.trainLocalNode(node, globalModel, 1, 4))
      );

      const aggregated = await trainer.aggregateUpdates(updates, 0);

      expect(aggregated).toHaveProperty('entityEmbeddings');
      expect(aggregated).toHaveProperty('relationEmbeddings');
      expect(aggregated).toHaveProperty('version');
      expect(aggregated.version).toBe(1);
    });

    it('should respect privacy budget', async () => {
      const results = await trainer.trainFederated({
        epochs: 2,
        localEpochs: 1,
        batchSize: 4,
      });

      expect(results.privacySpent).toBeLessThanOrEqual(trainer.config.privacyBudget);
    });

    it('should track statistics', () => {
      const stats = trainer.getStats();

      expect(stats).toHaveProperty('rounds');
      expect(stats).toHaveProperty('totalUpdates');
      expect(stats).toHaveProperty('privacySpent');
      expect(stats).toHaveProperty('privacyBudget');
      expect(stats).toHaveProperty('privacyRemaining');
    });
  });
});

// Helper functions

function createMockGraph(numTriples) {
  const graph = [];

  for (let i = 0; i < numTriples; i++) {
    graph.push({
      subject: { value: `http://example.org/entity${i}` },
      predicate: { value: `http://example.org/relatedTo` },
      object: { value: `http://example.org/entity${(i + 1) % numTriples}` },
    });
  }

  // Add iterator
  graph[Symbol.iterator] = function* () {
    for (const triple of this) {
      yield triple;
    }
  };

  return graph;
}

function createMockFederatedNodes(numNodes) {
  const nodes = [];

  for (let i = 0; i < numNodes; i++) {
    nodes.push({
      id: `node_${i}`,
      graph: createMockGraph(5 + i), // Different sizes
    });
  }

  return nodes;
}
