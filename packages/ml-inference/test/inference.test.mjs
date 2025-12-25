/**
 * @file ML Inference Tests
 * @description Comprehensive tests for ONNX inference pipeline
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { createONNXRunner } from '../src/runtime/onnx-runner.mjs';
import { createStreamingInferencePipeline } from '../src/pipeline/streaming-inference.mjs';
import { createModelRegistry } from '../src/registry/model-registry.mjs';

// Mock ONNX runner for tests
class MockONNXRunner {
  constructor() {
    this.session = null;
    this.inputNames = ['input'];
    this.outputNames = ['output'];
  }

  async loadModel() {
    this.session = { loaded: true };
    return 5.0;
  }

  async infer(inputs) {
    if (!this.session) throw new Error('Model not loaded');
    return {
      output: {
        data: new Float32Array([0.5, 0.3, 0.2]),
        size: 3,
      },
    };
  }

  async inferBatch(batchInputs) {
    if (!this.session) throw new Error('Model not loaded');
    return batchInputs.map(() => ({
      output: new Float32Array([0.5, 0.3, 0.2]),
    }));
  }

  getMetadata() {
    return {
      inputs: this.inputNames,
      outputs: this.outputNames,
    };
  }

  getMetrics() {
    return {
      totalInferences: 0,
      avgLatencyMs: 0,
      throughputPerSec: 0,
    };
  }

  async dispose() {
    this.session = null;
  }
}

describe('ML Inference Pipeline', () => {
  describe('StreamingInferencePipeline', () => {
    let runner;
    let pipeline;

    beforeEach(() => {
      runner = new MockONNXRunner();
      pipeline = createStreamingInferencePipeline(runner, {
        batchSize: 4,
        batchTimeoutMs: 100,
        maxQueueSize: 100,
      });
    });

    afterEach(async () => {
      await pipeline?.destroy();
    });

    it('should create pipeline instance', () => {
      expect(pipeline).toBeDefined();
      expect(pipeline.buffer).toEqual([]);
    });

    it('should process single item', async () => {
      const results = [];
      pipeline.subscribe(batch => {
        results.push(...batch);
      });

      await pipeline.process({
        id: 'test-1',
        features: [1, 2, 3],
      });

      await pipeline.flush();

      expect(results.length).toBeGreaterThan(0);
      expect(results[0].id).toBe('test-1');
    });

    it('should batch items correctly', async () => {
      const batches = [];
      pipeline.subscribe(batch => {
        batches.push(batch);
      });

      // Add 8 items (should create 2 batches of 4)
      for (let i = 0; i < 8; i++) {
        await pipeline.process({
          id: `item-${i}`,
          features: [i, i + 1, i + 2],
        });
      }

      await pipeline.flush();

      expect(batches.length).toBeGreaterThanOrEqual(1);
      const totalProcessed = batches.reduce((sum, b) => sum + b.length, 0);
      expect(totalProcessed).toBe(8);
    });

    it('should track metrics', async () => {
      pipeline.subscribe(() => {});

      for (let i = 0; i < 5; i++) {
        await pipeline.process({
          id: `item-${i}`,
          features: [i],
        });
      }

      await pipeline.flush();

      const metrics = pipeline.getMetrics();
      expect(metrics.totalProcessed).toBe(5);
      expect(metrics.totalBatches).toBeGreaterThan(0);
    });

    it('should pause and resume', async () => {
      pipeline.pause();
      expect(pipeline.isPaused).toBe(true);

      pipeline.resume();
      expect(pipeline.isPaused).toBe(false);
    });
  });

  describe('ModelRegistry', () => {
    let registry;

    beforeEach(() => {
      registry = createModelRegistry();
    });

    afterEach(async () => {
      await registry?.destroy();
    });

    it('should create registry instance', () => {
      expect(registry).toBeDefined();
      expect(registry.models.size).toBe(0);
    });

    it('should register model', async () => {
      const mockRunner = new MockONNXRunner();
      await mockRunner.loadModel();

      registry.models.set('v1.0', {
        runner: mockRunner,
        metadata: {
          name: 'test-model',
          version: 'v1.0',
          createdAt: Date.now(),
        },
        stats: {
          loadTimeMs: 5.0,
          deployedAt: Date.now(),
          inferences: 0,
          errors: 0,
        },
      });

      expect(registry.models.size).toBe(1);
      const model = registry.getModel('v1.0');
      expect(model).toBeDefined();
      expect(model.metadata.name).toBe('test-model');
    });

    it('should list models', async () => {
      const mockRunner1 = new MockONNXRunner();
      const mockRunner2 = new MockONNXRunner();

      registry.models.set('v1.0', {
        runner: mockRunner1,
        metadata: { name: 'model1', version: 'v1.0', createdAt: Date.now() },
        stats: { loadTimeMs: 5, deployedAt: Date.now(), inferences: 0, errors: 0 },
      });

      registry.models.set('v2.0', {
        runner: mockRunner2,
        metadata: { name: 'model2', version: 'v2.0', createdAt: Date.now() },
        stats: { loadTimeMs: 6, deployedAt: Date.now(), inferences: 0, errors: 0 },
      });

      const models = registry.listModels();
      expect(models.length).toBe(2);
    });

    it('should deploy and promote canary', () => {
      const mockRunner1 = new MockONNXRunner();
      const mockRunner2 = new MockONNXRunner();

      registry.models.set('v1.0', {
        runner: mockRunner1,
        metadata: { name: 'model', version: 'v1.0', createdAt: Date.now() },
        stats: { loadTimeMs: 5, deployedAt: Date.now(), inferences: 0, errors: 0 },
      });

      registry.models.set('v2.0', {
        runner: mockRunner2,
        metadata: { name: 'model', version: 'v2.0', createdAt: Date.now() },
        stats: { loadTimeMs: 5, deployedAt: Date.now(), inferences: 0, errors: 0 },
      });

      registry.activeVersion = 'v1.0';

      // Deploy canary
      registry.deploy('v2.0', 'canary', 10);
      expect(registry.canaryVersion).toBe('v2.0');
      expect(registry.canaryTrafficPercent).toBe(10);

      // Promote
      registry.promoteCanary();
      expect(registry.activeVersion).toBe('v2.0');
      expect(registry.canaryVersion).toBeNull();
    });
  });
});
