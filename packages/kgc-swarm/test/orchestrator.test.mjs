/**
 * @file orchestrator.test.mjs
 * @description Tests for KGCSwarmOrchestrator
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { KGCSwarmOrchestrator, createOrchestrator } from '../src/orchestrator.mjs';

describe('KGCSwarmOrchestrator', () => {
  let orchestrator;

  beforeEach(() => {
    orchestrator = new KGCSwarmOrchestrator({
      budget: {
        maxTime: 5000, // 5 seconds
        maxSteps: 50,
        maxBytes: 10 * 1024 * 1024, // 10MB
      },
    });
  });

  describe('constructor', () => {
    it('should create orchestrator with default options', () => {
      const orch = new KGCSwarmOrchestrator();
      expect(orch).toBeDefined();
      expect(orch.budget).toBeDefined();
      expect(orch.tokenGenerator).toBeDefined();
      expect(orch.state.currentEpoch).toBe(0);
    });

    it('should create orchestrator with custom budget', () => {
      const budget = { maxTime: 10000, maxSteps: 100, maxBytes: 1024 };
      const orch = new KGCSwarmOrchestrator({ budget });

      expect(orch.budget.maxTime).toBe(10000);
      expect(orch.budget.maxSteps).toBe(100);
      expect(orch.budget.maxBytes).toBe(1024);
    });

    it('should create orchestrator without RDF store', () => {
      const orch = new KGCSwarmOrchestrator({ storeObservations: false });
      expect(orch.store).toBeNull();
    });
  });

  describe('run(σ, κ)', () => {
    it('should execute main loop and generate observations', async () => {
      const result = await orchestrator.run(
        { seed: 42, context: 'test' },
        { temperature: 0.7, maxTokens: 5 },
        { shouldStop: (state) => state.currentEpoch >= 3 }
      );

      expect(result.epochs).toBe(3);
      expect(result.totalSteps).toBeGreaterThan(0);
      expect(result.observations).toBeGreaterThan(0);
      expect(result.stopReason).toBe('Custom stop condition met');
      expect(orchestrator.getCurrentEpoch()).toBe(3);
    });

    it('should respect maxSteps budget', async () => {
      const orch = new KGCSwarmOrchestrator({
        budget: { maxTime: 10000, maxSteps: 10, maxBytes: 1024 * 1024 },
      });

      const result = await orch.run(
        { seed: 123 },
        { temperature: 0.7, maxTokens: 20 }
      );

      expect(result.stopReason).toContain('Steps budget exceeded');
    });

    it('should respect maxTime budget', async () => {
      const orch = new KGCSwarmOrchestrator({
        budget: { maxTime: 100, maxSteps: 10000, maxBytes: 1024 * 1024 },
      });

      // Give it time to hit budget
      const result = await orch.run(
        { seed: 456 },
        { temperature: 0.7, maxTokens: 5 },
        { shouldStop: (state) => state.currentEpoch >= 100 }
      );

      expect(result.stopReason).toContain('budget exceeded');
    });

    it('should call onEpoch callback', async () => {
      const epochs = [];

      await orchestrator.run(
        { seed: 789 },
        { temperature: 0.7, maxTokens: 5 },
        {
          onEpoch: (τ, observations) => {
            epochs.push({ epoch: τ, obsCount: observations.length });
          },
          shouldStop: (state) => state.currentEpoch >= 2,
        }
      );

      expect(epochs.length).toBe(2);
      expect(epochs[0].epoch).toBe(1);
      expect(epochs[1].epoch).toBe(2);
      epochs.forEach(e => {
        expect(e.obsCount).toBeGreaterThan(0);
      });
    });

    it('should accumulate observations in O_τ', async () => {
      await orchestrator.run(
        { seed: 42 },
        { temperature: 0.7, maxTokens: 5 },
        { shouldStop: (state) => state.currentEpoch >= 2 }
      );

      const allObs = orchestrator.getObservations();
      expect(allObs.length).toBeGreaterThan(0);

      const epoch0Obs = orchestrator.getObservations(0);
      const epoch1Obs = orchestrator.getObservations(1);

      expect(epoch0Obs.length).toBeGreaterThan(0);
      expect(epoch1Obs.length).toBeGreaterThan(0);

      epoch0Obs.forEach(obs => {
        expect(obs.epoch).toBe(0);
        expect(obs).toHaveProperty('type');
        expect(obs).toHaveProperty('data');
        expect(obs).toHaveProperty('timestamp');
      });
    });
  });

  describe('stop()', () => {
    it('should stop orchestrator', () => {
      orchestrator.stop('Test stop');

      const state = orchestrator.getState();
      expect(state.stopped).toBe(true);
      expect(state.stopReason).toBe('Test stop');
    });
  });

  describe('getState()', () => {
    it('should return orchestrator state', () => {
      const state = orchestrator.getState();

      expect(state).toHaveProperty('currentEpoch');
      expect(state).toHaveProperty('totalSteps');
      expect(state).toHaveProperty('startTime');
      expect(state).toHaveProperty('stopped');
      expect(state.currentEpoch).toBe(0);
      expect(state.stopped).toBe(false);
    });
  });

  describe('getObservations()', () => {
    it('should return all observations', async () => {
      await orchestrator.run(
        { seed: 42 },
        { temperature: 0.7, maxTokens: 3 },
        { shouldStop: (state) => state.currentEpoch >= 1 }
      );

      const obs = orchestrator.getObservations();
      expect(Array.isArray(obs)).toBe(true);
      expect(obs.length).toBeGreaterThan(0);
    });

    it('should filter observations by epoch', async () => {
      await orchestrator.run(
        { seed: 42 },
        { temperature: 0.7, maxTokens: 3 },
        { shouldStop: (state) => state.currentEpoch >= 2 }
      );

      const epoch0Obs = orchestrator.getObservations(0);
      const epoch1Obs = orchestrator.getObservations(1);

      expect(epoch0Obs.every(obs => obs.epoch === 0)).toBe(true);
      expect(epoch1Obs.every(obs => obs.epoch === 1)).toBe(true);
    });
  });

  describe('RDF store integration', () => {
    it('should store observations in RDF store', async () => {
      await orchestrator.run(
        { seed: 42 },
        { temperature: 0.7, maxTokens: 3 },
        { shouldStop: (state) => state.currentEpoch >= 1 }
      );

      const store = orchestrator.getStore();
      expect(store).not.toBeNull();
      expect(store.size).toBeGreaterThan(0);
    });

    it('should not create RDF store when disabled', () => {
      const orch = new KGCSwarmOrchestrator({ storeObservations: false });
      const store = orch.getStore();
      expect(store).toBeNull();
    });
  });

  describe('createOrchestrator', () => {
    it('should create KGCSwarmOrchestrator instance', () => {
      const orch = createOrchestrator({ budget: { maxSteps: 10 } });
      expect(orch).toBeInstanceOf(KGCSwarmOrchestrator);
      expect(orch.budget.maxSteps).toBe(10);
    });
  });
});
