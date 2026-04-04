/**
 * @file Ensemble Groq Provider Tests
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { EnsembleGroqProvider } from '../src/providers/ensemble-groq.mjs';

describe('EnsembleGroqProvider', () => {
  let mockBaseProvider;
  let ensemble;

  beforeEach(() => {
    // Mock base provider
    mockBaseProvider = {
      generateText: vi.fn(async (input) => ({
        text: `Response to: ${input.prompt}`,
      })),
    };

    ensemble = new EnsembleGroqProvider(mockBaseProvider);
  });

  it('should create ensemble with default specialists', () => {
    expect(ensemble.specialists).toHaveLength(3);
    expect(ensemble.specialists[0].role).toBe('hypothesis-generator');
    expect(ensemble.specialists[1].role).toBe('validator');
    expect(ensemble.specialists[2].role).toBe('refiner');
  });

  it('should create ensemble with custom specialists', () => {
    const custom = [{ role: 'custom', prefix: 'Custom: ' }];
    const customEnsemble = new EnsembleGroqProvider(mockBaseProvider, {
      specialists: custom,
    });

    expect(customEnsemble.specialists).toEqual(custom);
  });

  it('should run all specialists in parallel', async () => {
    const result = await ensemble.generateText({
      prompt: 'Test prompt',
      maxTokens: 100,
    });

    // All 3 specialists should be called
    expect(mockBaseProvider.generateText).toHaveBeenCalledTimes(3);
    expect(result.specialist_responses).toHaveLength(3);
  });

  it('should aggregate specialist responses', async () => {
    const result = await ensemble.generateText({
      prompt: 'Test prompt',
    });

    expect(result.text).toBeDefined();
    expect(result.text).toContain('hypothesis-generator');
    expect(result.text).toContain('validator');
    expect(result.text).toContain('refiner');
  });

  it('should compute confidence score', async () => {
    const result = await ensemble.generateText({
      prompt: 'Test prompt',
    });

    expect(result.confidence).toBeDefined();
    expect(typeof result.confidence).toBe('number');
    expect(result.confidence).toBeGreaterThanOrEqual(0);
    expect(result.confidence).toBeLessThanOrEqual(1);
  });

  it('should include specialist roles in response', async () => {
    const result = await ensemble.generateText({
      prompt: 'Test prompt',
    });

    expect(result.specialist_responses[0].role).toBe('hypothesis-generator');
    expect(result.specialist_responses[1].role).toBe('validator');
    expect(result.specialist_responses[2].role).toBe('refiner');
  });

  it('should allow setting custom specialists', () => {
    const newSpecialists = [
      { role: 'new1', prefix: 'New: ' },
      { role: 'new2', prefix: 'New2: ' },
    ];

    ensemble.setSpecialists(newSpecialists);
    expect(ensemble.getSpecialists()).toEqual(newSpecialists);
  });

  it('should pass through input options to base provider', async () => {
    const inputOptions = {
      prompt: 'Test prompt',
      maxTokens: 500,
      temperature: 0.7,
    };

    await ensemble.generateText(inputOptions);

    // Check that all specialists received the options
    expect(mockBaseProvider.generateText).toHaveBeenCalledWith(
      expect.objectContaining({
        maxTokens: 500,
        temperature: 0.7,
      })
    );
  });

  it('should handle empty specialist list gracefully', () => {
    const emptyEnsemble = new EnsembleGroqProvider(mockBaseProvider, {
      specialists: [],
    });

    expect(emptyEnsemble.specialists).toEqual([]);
  });

  it('should compute high confidence for similar responses', () => {
    // Mock provider that returns same response
    const uniformProvider = {
      generateText: vi.fn(async () => ({
        text: 'Same response for all',
      })),
    };

    const uniformEnsemble = new EnsembleGroqProvider(uniformProvider);

    return uniformEnsemble.generateText({ prompt: 'Test' }).then(result => {
      expect(result.confidence).toBeGreaterThan(0.5);
    });
  });

  it('should include ensemble size in response', async () => {
    const result = await ensemble.generateText({
      prompt: 'Test prompt',
    });

    expect(result.ensemble_size).toBe(3);
  });
});
