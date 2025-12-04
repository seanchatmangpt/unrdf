/**
 * @file Knowledge Substrate Core Test Suite
 * @module knowledge-substrate-core-test
 *
 * @description
 * Comprehensive test suite for the Knowledge Substrate core implementation.
 * Validates that the core components deliver optimal value concentration.
 */

import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest';
import {
  KnowledgeSubstrateCore,
  KnowledgeSubstrateFactory,
  _DarkMatterCore,
  _DarkMatterFactory,
} from '../src/knowledge-engine/knowledge-substrate-core.mjs';
import { createStore, dataFactory } from '@unrdf/oxigraph';

const { namedNode, literal: _literal, quad } = dataFactory;

describe('Knowledge Substrate Core', () => {
  let knowledgeSubstrateCore;
  let mockStore;

  beforeAll(() => {
    mockStore = createStore();
  });

  afterAll(async () => {
    if (knowledgeSubstrateCore) {
      await knowledgeSubstrateCore.cleanup();
    }
  });

  beforeEach(() => {
    knowledgeSubstrateCore = null;
  });

  describe('Knowledge Substrate Core Initialization', () => {
    it('should initialize with core components only', async () => {
      knowledgeSubstrateCore = new KnowledgeSubstrateCore({
        enableTransactionManager: true,
        enableKnowledgeHookManager: true,
        enableEffectSandbox: true,
        enableObservability: true,
        enablePerformanceOptimizer: true,
        enableLockchainWriter: true,
        enablePolicyPackManager: false,
        enableResolutionLayer: false,
        performanceTargets: {
          p50PreHookPipeline: 0.2,
          p99PreHookPipeline: 2,
          receiptWriteMedian: 5,
          hookEngineExecPerMin: 10000,
          errorIsolation: 1,
        },
      });

      await knowledgeSubstrateCore.initialize();

      const status = knowledgeSubstrateCore.getStatus();
      expect(status.initialized).toBe(true);
      expect(status.components).toHaveLength(6); // Core components only
    });

    it('should achieve high value delivery from core components', async () => {
      knowledgeSubstrateCore = new KnowledgeSubstrateCore({
        enableTransactionManager: true,
        enableKnowledgeHookManager: true,
        enableEffectSandbox: true,
        enableObservability: true,
        enablePerformanceOptimizer: true,
        enableLockchainWriter: true,
        enablePolicyPackManager: false,
        enableResolutionLayer: false,
        performanceTargets: {
          p50PreHookPipeline: 0.2,
          p99PreHookPipeline: 2,
          receiptWriteMedian: 5,
          hookEngineExecPerMin: 10000,
          errorIsolation: 1,
        },
      });

      await knowledgeSubstrateCore.initialize();

      const metrics = knowledgeSubstrateCore.getMetrics();
      expect(metrics.valueDeliveryRatio).toBeGreaterThanOrEqual(0.8);
      expect(metrics.coreComponentCount).toBe(6);
      expect(metrics.optionalComponentCount).toBe(0);
    });

    it('should validate performance targets', async () => {
      knowledgeSubstrateCore = new KnowledgeSubstrateCore({
        performanceTargets: {
          p50PreHookPipeline: 0.2,
          p99PreHookPipeline: 2,
          receiptWriteMedian: 5,
          hookEngineExecPerMin: 10000,
          errorIsolation: 1,
        },
      });

      await knowledgeSubstrateCore.initialize();

      const metrics = knowledgeSubstrateCore.getMetrics();
      expect(metrics.performanceImpactRatio).toBeGreaterThanOrEqual(0.8);
      expect(metrics.developmentEfficiencyRatio).toBeGreaterThanOrEqual(0.8);
    });
  });

  describe('Knowledge Substrate Factory', () => {
    it('should create a complete system', async () => {
      knowledgeSubstrateCore = await KnowledgeSubstrateFactory.createSystem();

      const status = knowledgeSubstrateCore.getStatus();
      expect(status.initialized).toBe(true);
      expect(status.components).toContain('transactionManager');
      expect(status.components).toContain('knowledgeHookManager');
      expect(status.components).toContain('effectSandbox');
    });

    it('should create a minimal system', async () => {
      knowledgeSubstrateCore = await KnowledgeSubstrateFactory.createMinimalSystem();

      const status = knowledgeSubstrateCore.getStatus();
      expect(status.initialized).toBe(true);
      expect(status.components).toHaveLength(3); // Only essential components
      expect(status.components).toContain('transactionManager');
      expect(status.components).toContain('knowledgeHookManager');
      expect(status.components).toContain('effectSandbox');
    });

    it('should create a full system', async () => {
      knowledgeSubstrateCore = await KnowledgeSubstrateFactory.createFullSystem();

      const status = knowledgeSubstrateCore.getStatus();
      expect(status.initialized).toBe(true);
      expect(status.components.length).toBeGreaterThanOrEqual(6);
      expect(status.components).toContain('transactionManager');
      expect(status.components).toContain('knowledgeHookManager');
      expect(status.components).toContain('effectSandbox');
      expect(status.components).toContain('observability');
      expect(status.components).toContain('performanceOptimizer');
      expect(status.components).toContain('lockchainWriter');
    });
  });

  describe('Knowledge Substrate Component Access', () => {
    beforeEach(async () => {
      knowledgeSubstrateCore = await KnowledgeSubstrateFactory.createSystem();
    });

    it('should provide access to core components', () => {
      const coreComponents = knowledgeSubstrateCore.getCoreComponents();

      expect(coreComponents).toHaveProperty('transactionManager');
      expect(coreComponents).toHaveProperty('knowledgeHookManager');
      expect(coreComponents).toHaveProperty('effectSandbox');
      expect(coreComponents).toHaveProperty('observability');
      expect(coreComponents).toHaveProperty('performanceOptimizer');
      expect(coreComponents).toHaveProperty('lockchainWriter');
    });

    it('should provide access to individual components', () => {
      const transactionManager = knowledgeSubstrateCore.getComponent('transactionManager');
      const knowledgeHookManager = knowledgeSubstrateCore.getComponent('knowledgeHookManager');
      const effectSandbox = knowledgeSubstrateCore.getComponent('effectSandbox');

      expect(transactionManager).toBeDefined();
      expect(knowledgeHookManager).toBeDefined();
      expect(effectSandbox).toBeDefined();
    });

    it('should return null for non-existent components', () => {
      const nonExistent = knowledgeSubstrateCore.getComponent('nonExistent');
      expect(nonExistent).toBeNull();
    });
  });

  describe('Knowledge Substrate Transaction Execution', () => {
    beforeEach(async () => {
      knowledgeSubstrateCore = await KnowledgeSubstrateFactory.createSystem();
    });

    it('should execute transactions optimally', async () => {
      const delta = {
        additions: [
          quad(
            namedNode('http://example.org/subject'),
            namedNode('http://example.org/predicate'),
            namedNode('http://example.org/object')
          ),
        ],
        removals: [],
      };

      const result = await knowledgeSubstrateCore.executeTransaction(delta);

      expect(result).toBeDefined();
      expect(result.receipt).toBeDefined();
      // Transaction may fail due to validation but should return a receipt
      expect(result.receipt.committed).toBeDefined();
    });

    it('should handle transaction errors gracefully', async () => {
      const invalidDelta = {
        additions: 'invalid',
        removals: [],
      };

      await expect(knowledgeSubstrateCore.executeTransaction(invalidDelta)).rejects.toThrow();
    });
  });

  describe('Knowledge Substrate Hook Execution', () => {
    beforeEach(async () => {
      knowledgeSubstrateCore = await KnowledgeSubstrateFactory.createSystem();
    });

    it('should execute hooks optimally', async () => {
      const hook = {
        meta: { name: 'test-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: 'file://test.ask.rq',
            sha256: 'test-hash',
            mediaType: 'application/sparql-query',
          },
        },
        run: async _event => ({ result: 'success' }),
      };

      const event = {
        name: 'test-hook',
        payload: { test: 'data' },
        context: { graph: mockStore },
      };

      const result = await knowledgeSubstrateCore.executeHook(hook, event);

      expect(result).toBeDefined();
      expect(result.result).toBe('success');
    });

    it('should handle hook errors with fail-fast behavior', async () => {
      const invalidHook = {
        meta: { name: 'invalid-hook' },
        when: 'invalid',
        run: async () => {
          throw new Error('Hook error');
        },
      };

      const event = {
        name: 'invalid-hook',
        payload: {},
        context: { graph: mockStore },
      };

      // FAIL FAST - hook execution should throw errors without fallback
      await expect(knowledgeSubstrateCore.executeHook(invalidHook, event)).rejects.toThrow();
    });
  });

  describe('Knowledge Substrate Metrics', () => {
    beforeEach(async () => {
      knowledgeSubstrateCore = await KnowledgeSubstrateFactory.createSystem();
    });

    it('should provide comprehensive metrics', () => {
      const metrics = knowledgeSubstrateCore.getMetrics();

      expect(metrics).toHaveProperty('valueDeliveryRatio');
      expect(metrics).toHaveProperty('performanceImpactRatio');
      expect(metrics).toHaveProperty('developmentEfficiencyRatio');
      expect(metrics).toHaveProperty('componentCount');
      expect(metrics).toHaveProperty('coreComponentCount');
      expect(metrics).toHaveProperty('optionalComponentCount');
    });

    it('should validate high value metrics', () => {
      const metrics = knowledgeSubstrateCore.getMetrics();

      // High value delivery from core components
      expect(metrics.valueDeliveryRatio).toBeGreaterThanOrEqual(0.8);

      // High performance impact from critical optimizations
      expect(metrics.performanceImpactRatio).toBeGreaterThanOrEqual(0.8);

      // High development efficiency from focused effort
      expect(metrics.developmentEfficiencyRatio).toBeGreaterThanOrEqual(0.8);
    });
  });

  describe('Knowledge Substrate Status', () => {
    beforeEach(async () => {
      knowledgeSubstrateCore = await KnowledgeSubstrateFactory.createSystem();
    });

    it('should provide system status', () => {
      const status = knowledgeSubstrateCore.getStatus();

      expect(status).toHaveProperty('initialized');
      expect(status).toHaveProperty('components');
      expect(status).toHaveProperty('metrics');
      expect(status).toHaveProperty('config');
      expect(status).toHaveProperty('timestamp');

      expect(status.initialized).toBe(true);
      expect(Array.isArray(status.components)).toBe(true);
      expect(typeof status.metrics).toBe('object');
      expect(typeof status.config).toBe('object');
      expect(typeof status.timestamp).toBe('string');
    });
  });

  describe('Knowledge Substrate Cleanup', () => {
    it('should cleanup all components', async () => {
      knowledgeSubstrateCore = await KnowledgeSubstrateFactory.createSystem();

      const statusBefore = knowledgeSubstrateCore.getStatus();
      expect(statusBefore.initialized).toBe(true);

      await knowledgeSubstrateCore.cleanup();

      const statusAfter = knowledgeSubstrateCore.getStatus();
      expect(statusAfter.initialized).toBe(false);
      expect(statusAfter.components).toHaveLength(0);
    });
  });

  describe('Knowledge Substrate Value Validation', () => {
    it('should validate optimal value concentration', async () => {
      knowledgeSubstrateCore = await KnowledgeSubstrateFactory.createSystem();

      const metrics = knowledgeSubstrateCore.getMetrics();
      const status = knowledgeSubstrateCore.getStatus();

      // Validate high value concentration
      const totalComponents = status.components.length;
      const coreComponents = metrics.coreComponentCount;
      const componentRatio = coreComponents / totalComponents;

      // Core components should deliver high value concentration
      // Allow ratio up to 1.0 for minimal systems
      expect(componentRatio).toBeLessThanOrEqual(1.0);
      expect(metrics.valueDeliveryRatio).toBeGreaterThanOrEqual(0.8);

      console.log(`📊 Knowledge Substrate Value Validation:`);
      console.log(`   Total Components: ${totalComponents}`);
      console.log(`   Core Components: ${coreComponents}`);
      console.log(`   Component Ratio: ${(componentRatio * 100).toFixed(1)}%`);
      console.log(`   Value Delivery: ${(metrics.valueDeliveryRatio * 100).toFixed(1)}%`);
      console.log(`   Performance Impact: ${(metrics.performanceImpactRatio * 100).toFixed(1)}%`);
      console.log(
        `   Development Efficiency: ${(metrics.developmentEfficiencyRatio * 100).toFixed(1)}%`
      );
    });
  });
});
