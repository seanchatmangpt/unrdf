/**
 * @file Knowledge Hooks Self-Play Autonomics Loop - Unit Tests
 * @module @unrdf/daemon/test/knowledge-self-play
 */

import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { randomUUID } from 'crypto';
import { KnowledgeSelfPlayLoop, createKnowledgeSelfPlayLoop } from '../src/knowledge-self-play.mjs';

/**
 * Mock store with quad tracking
 */
function createMockStore() {
  const quads = [];
  return {
    add: (quad) => {
      quads.push(quad);
    },
    getQuads: () => [...quads],
    reset: () => {
      quads.length = 0;
    },
  };
}

/**
 * Mock engine that returns customizable receipts
 */
function createMockEngine(options = {}) {
  const { hashes = [] } = options;
  let callCount = 0;

  return {
    execute: vi.fn(async (_store, _delta, _config) => {
      let hash;
      if (hashes.length > 0) {
        // Use provided hashes array
        hash = hashes[callCount] || hashes[hashes.length - 1];
      } else {
        // Default: each call increments hash
        hash = `hash-${callCount}`;
      }

      // Previous hash is input_hash (from prior iteration or initial)
      const prevHash = callCount === 0 ? 'initial-hash' : hashes[callCount - 1] || `hash-${callCount - 1}`;

      const result = {
        receipt: {
          input_hash: prevHash,
          output_hash: hash,
          receiptHash: `receipt-${randomUUID()}`,
          previousReceiptHash: callCount === 0 ? null : `receipt-${callCount - 1}`,
          executedHooks: 1,
        },
        executionResults: [{ hookId: 'hook-1', fired: true }],
      };

      callCount++;
      return result;
    }),
    reset: () => {
      callCount = 0;
    },
  };
}

describe('KnowledgeSelfPlayLoop', () => {
  let store;
  let engine;
  let loop;

  beforeEach(() => {
    store = createMockStore();
    engine = createMockEngine();
  });

  afterEach(() => {
    vi.clearAllMocks();
  });

  describe('constructor', () => {
    it('creates instance with required parameters', () => {
      loop = new KnowledgeSelfPlayLoop({ store, engine });
      expect(loop).toBeDefined();
      expect(loop.getEpisodeId()).toMatch(/^[0-9a-f-]+$/);
    });

    it('throws error if store is missing', () => {
      expect(() => new KnowledgeSelfPlayLoop({ engine })).toThrow('store is required');
    });

    it('throws error if engine is missing', () => {
      expect(() => new KnowledgeSelfPlayLoop({ store })).toThrow('engine is required');
    });

    it('throws error if engine lacks execute method', () => {
      expect(() => new KnowledgeSelfPlayLoop({ store, engine: {} })).toThrow(
        'engine must have execute method'
      );
    });

    it('accepts custom maxIterations', () => {
      loop = new KnowledgeSelfPlayLoop({ store, engine, maxIterations: 20 });
      expect(loop).toBeDefined();
    });

    it('accepts custom triggerType', () => {
      loop = new KnowledgeSelfPlayLoop({
        store,
        engine,
        triggerType: 'debug',
      });
      expect(loop).toBeDefined();
    });
  });

  describe('step()', () => {
    beforeEach(() => {
      loop = new KnowledgeSelfPlayLoop({ store, engine });
    });

    it('executes engine.execute() with correct parameters', async () => {
      await loop.step({});
      expect(engine.execute).toHaveBeenCalledWith(
        store,
        {},
        expect.objectContaining({
          trigger: 'continuous-improvement',
        })
      );
    });

    it('returns receipt, feedback, and storeChanged flag', async () => {
      const result = await loop.step({});
      expect(result).toHaveProperty('receipt');
      expect(result).toHaveProperty('feedback');
      expect(result).toHaveProperty('storeChanged');
      expect(result).toHaveProperty('hooksExecuted');
    });

    it('detects convergence when input_hash equals output_hash', async () => {
      const stableEngine = createMockEngine({ hashes: ['v1', 'v1'] });
      loop = new KnowledgeSelfPlayLoop({ store, engine: stableEngine });

      // First step: initial → v1 (changed)
      await loop.step({});
      // Second step: v1 → v1 (no change, converged)
      const result = await loop.step({});
      expect(result.storeChanged).toBe(false);
    });

    it('detects progress when input_hash differs from output_hash', async () => {
      const result = await loop.step({});
      expect(result.storeChanged).toBe(true);
    });

    it('calculates feedback as 0.1 * hooksExecuted when store changed', async () => {
      const result = await loop.step({});
      expect(result.feedback).toBe(0.1);
    });

    it('calculates feedback as 0 when store did not change (convergence)', async () => {
      const stableEngine = createMockEngine({ hashes: ['v1', 'v1'] });
      loop = new KnowledgeSelfPlayLoop({ store, engine: stableEngine });

      // First step: initial → v1 (changed)
      await loop.step({});
      // Second step: v1 → v1 (no change, feedback=0)
      const result = await loop.step({});
      expect(result.feedback).toBe(0);
    });

    it('accumulates step history', async () => {
      await loop.step({});
      await loop.step({});
      const history = loop.getHistory();
      expect(history).toHaveLength(2);
      expect(history[0]).toHaveProperty('receipt');
      expect(history[1]).toHaveProperty('receipt');
    });
  });

  describe('run()', () => {
    beforeEach(() => {
      loop = new KnowledgeSelfPlayLoop({ store, engine, maxIterations: 5 });
    });

    it('returns episode result with episodeId', async () => {
      const result = await loop.run({});
      expect(result).toHaveProperty('episodeId');
      expect(result.episodeId).toMatch(/^[0-9a-f-]+$/);
    });

    it('returns receipts array from iterations', async () => {
      const result = await loop.run({});
      expect(result).toHaveProperty('receipts');
      expect(Array.isArray(result.receipts)).toBe(true);
      expect(result.receipts.length).toBeGreaterThan(0);
    });

    it('returns totalFeedback sum', async () => {
      const result = await loop.run({});
      expect(result).toHaveProperty('totalFeedback');
      expect(typeof result.totalFeedback).toBe('number');
    });

    it('terminates early when convergence is detected', async () => {
      const hashes = ['hash-1', 'hash-1', 'hash-1'];
      const quickConvergeEngine = createMockEngine({ hashes, shouldAlwaysChange: false });
      loop = new KnowledgeSelfPlayLoop({ store, engine: quickConvergeEngine });

      const result = await loop.run({});
      expect(result.converged).toBe(true);
      expect(result.iterations).toBeLessThan(5);
    });

    it('respects maxIterations limit', async () => {
      loop = new KnowledgeSelfPlayLoop({ store, engine, maxIterations: 3 });
      const result = await loop.run({});
      expect(result.iterations).toBeLessThanOrEqual(3);
    });

    it('includes startTime and endTime', async () => {
      const result = await loop.run({});
      expect(result).toHaveProperty('startTime');
      expect(result).toHaveProperty('endTime');
      expect(result.endTime).toBeGreaterThanOrEqual(result.startTime);
    });

    it('includes avgFeedbackPerIteration', async () => {
      const result = await loop.run({});
      expect(result).toHaveProperty('avgFeedbackPerIteration');
      expect(typeof result.avgFeedbackPerIteration).toBe('number');
    });

    it('threads previousReceipt through delta to next iteration', async () => {
      await loop.run({});
      const calls = engine.execute.mock.calls;
      expect(calls.length).toBeGreaterThan(1);
      // Second call should have previousReceipt in delta
      const secondCall = calls[1];
      expect(secondCall[1]).toHaveProperty('previousReceipt');
    });
  });

  describe('materializeEpisodeRDF()', () => {
    beforeEach(() => {
      loop = new KnowledgeSelfPlayLoop({ store, engine });
    });

    it('returns array of quads', async () => {
      const runResult = await loop.run({});
      const quads = loop.materializeEpisodeRDF(runResult);
      expect(Array.isArray(quads)).toBe(true);
      expect(quads.length).toBeGreaterThan(0);
    });

    it('creates quads with subject, predicate, object', async () => {
      const runResult = await loop.run({});
      const quads = loop.materializeEpisodeRDF(runResult);
      expect(quads.every((q) => q.subject && q.predicate && q.object)).toBe(true);
    });

    it('creates episode type triple', async () => {
      const runResult = await loop.run({});
      const quads = loop.materializeEpisodeRDF(runResult);
      const typeQuad = quads.find(
        (q) => q.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'
      );
      expect(typeQuad).toBeDefined();
      expect(typeQuad.object.value).toBe('urn:unrdf:SelfPlayEpisode');
    });

    it('includes episodeId quad', async () => {
      const runResult = await loop.run({});
      const quads = loop.materializeEpisodeRDF(runResult);
      const idQuad = quads.find((q) => q.predicate.value === 'urn:unrdf:episodeId');
      expect(idQuad).toBeDefined();
      expect(idQuad.object.value).toBe(runResult.episodeId);
    });

    it('includes iterations quad with correct value', async () => {
      const runResult = await loop.run({});
      const quads = loop.materializeEpisodeRDF(runResult);
      const iterQuad = quads.find((q) => q.predicate.value === 'urn:unrdf:iterations');
      expect(iterQuad).toBeDefined();
      expect(iterQuad.object.value).toBe(String(runResult.iterations));
    });

    it('includes converged quad as boolean', async () => {
      const runResult = await loop.run({});
      const quads = loop.materializeEpisodeRDF(runResult);
      const convQuad = quads.find((q) => q.predicate.value === 'urn:unrdf:converged');
      expect(convQuad).toBeDefined();
      expect(['true', 'false']).toContain(convQuad.object.value);
    });

    it('includes totalFeedback quad', async () => {
      const runResult = await loop.run({});
      const quads = loop.materializeEpisodeRDF(runResult);
      const feedQuad = quads.find((q) => q.predicate.value === 'urn:unrdf:totalFeedback');
      expect(feedQuad).toBeDefined();
    });

    it('links receipts via hasReceipt predicates', async () => {
      const runResult = await loop.run({});
      const quads = loop.materializeEpisodeRDF(runResult);
      const receiptQuads = quads.filter((q) => q.predicate.value === 'urn:unrdf:hasReceipt');
      expect(receiptQuads.length).toBe(runResult.receipts.length);
    });

    it('adds quads to store', async () => {
      const runResult = await loop.run({});
      store.reset();
      loop.materializeEpisodeRDF(runResult);
      const storeQuads = store.getQuads();
      expect(storeQuads.length).toBeGreaterThan(0);
    });

    it('handles store.add() errors gracefully', async () => {
      const failingStore = {
        add: vi.fn(() => {
          throw new Error('Store add failed');
        }),
      };
      loop = new KnowledgeSelfPlayLoop({ store: failingStore, engine });
      const runResult = await loop.run({});
      expect(() => loop.materializeEpisodeRDF(runResult)).not.toThrow();
    });
  });

  describe('getHistory()', () => {
    beforeEach(() => {
      loop = new KnowledgeSelfPlayLoop({ store, engine });
    });

    it('returns array of step records', async () => {
      await loop.step({});
      const history = loop.getHistory();
      expect(Array.isArray(history)).toBe(true);
    });

    it('includes receipt, feedback, storeChanged for each step', async () => {
      await loop.step({});
      await loop.step({});
      const history = loop.getHistory();
      expect(history[0]).toHaveProperty('receipt');
      expect(history[0]).toHaveProperty('feedback');
      expect(history[0]).toHaveProperty('storeChanged');
      expect(history[0]).toHaveProperty('hooksExecuted');
    });

    it('returns a copy (not reference to private state)', async () => {
      await loop.step({});
      const history1 = loop.getHistory();
      const history2 = loop.getHistory();
      expect(history1).not.toBe(history2);
      expect(history1).toEqual(history2);
    });
  });

  describe('getEpisodeId()', () => {
    beforeEach(() => {
      loop = new KnowledgeSelfPlayLoop({ store, engine });
    });

    it('returns UUID string', () => {
      const episodeId = loop.getEpisodeId();
      expect(typeof episodeId).toBe('string');
      expect(episodeId).toMatch(/^[0-9a-f-]+$/);
    });

    it('same across all method calls', () => {
      const id1 = loop.getEpisodeId();
      const id2 = loop.getEpisodeId();
      expect(id1).toBe(id2);
    });
  });

  describe('getMetrics()', () => {
    beforeEach(() => {
      loop = new KnowledgeSelfPlayLoop({ store, engine });
    });

    it('returns metrics object with required fields', async () => {
      await loop.step({});
      const metrics = loop.getMetrics();
      expect(metrics).toHaveProperty('episodeId');
      expect(metrics).toHaveProperty('totalSteps');
      expect(metrics).toHaveProperty('totalFeedback');
      expect(metrics).toHaveProperty('stepsConverged');
      expect(metrics).toHaveProperty('hooksExecutedTotal');
      expect(metrics).toHaveProperty('avgHooksPerStep');
    });

    it('counts converged steps correctly', async () => {
      const stableEngine = createMockEngine({ hashes: ['v1', 'v1', 'v1'] });
      loop = new KnowledgeSelfPlayLoop({ store, engine: stableEngine });
      // Step 0: initial → v1 (changed)
      await loop.step({});
      // Step 1: v1 → v1 (no change, converged)
      await loop.step({});
      const metrics = loop.getMetrics();
      expect(metrics.stepsConverged).toBe(1);
    });

    it('calculates avgHooksPerStep', async () => {
      await loop.step({});
      await loop.step({});
      const metrics = loop.getMetrics();
      expect(metrics.avgHooksPerStep).toBe(metrics.hooksExecutedTotal / metrics.totalSteps);
    });
  });

  describe('createKnowledgeSelfPlayLoop factory', () => {
    it('creates instance with factory function', () => {
      loop = createKnowledgeSelfPlayLoop(store, engine);
      expect(loop).toBeInstanceOf(KnowledgeSelfPlayLoop);
    });

    it('passes options to constructor', () => {
      loop = createKnowledgeSelfPlayLoop(store, engine, { maxIterations: 15 });
      expect(loop).toBeDefined();
    });
  });

  describe('integration: convergence to stable state', () => {
    it('stops looping when no more mutations occur', async () => {
      const hashes = ['v1', 'v2', 'v2'];
      const engine2 = createMockEngine({ hashes });
      loop = new KnowledgeSelfPlayLoop({ store, engine: engine2, maxIterations: 10 });

      const result = await loop.run({});
      expect(result.converged).toBe(true);
      // Three calls: initial→v1 (change), v1→v2 (change), v2→v2 (no change, break)
      expect(result.iterations).toBe(3);
    });

    it('accumulates correct total feedback over multiple iterations', async () => {
      const hashes = ['v1', 'v2', 'v3', 'v3'];
      const engine2 = createMockEngine({ hashes });
      loop = new KnowledgeSelfPlayLoop({ store, engine: engine2 });

      const result = await loop.run({});
      // Feedback from changes: initial→v1 (0.1), v1→v2 (0.1), v2→v3 (0.1), v3→v3 (0=converged)
      const expectedFeedback = 0.1 + 0.1 + 0.1;
      expect(result.totalFeedback).toBe(expectedFeedback);
    });
  });
});
