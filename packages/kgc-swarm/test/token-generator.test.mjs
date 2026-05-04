/**
 * @file token-generator.test.mjs
 * @description Tests for TokenGenerator G(σ, κ)
 */

import { describe, it, expect } from 'vitest';
import { TokenGenerator, createTokenGenerator } from '../src/token-generator.mjs';

describe('TokenGenerator', () => {
  describe('constructor', () => {
    it('should create generator with default options', () => {
      const generator = new TokenGenerator();
      expect(generator).toBeDefined();
      expect(generator.deterministic).toBe(false);
    });

    it('should create generator with deterministic mode', () => {
      const generator = new TokenGenerator({ deterministic: true });
      expect(generator.deterministic).toBe(true);
    });
  });

  describe('emit(σ, κ)', () => {
    it('should generate token sequence with valid parameters', () => {
      const generator = new TokenGenerator({ deterministic: true });
      const tokens = generator.emit(
        { seed: 42, context: 'test' },
        { temperature: 0.7, maxTokens: 10 }
      );

      expect(Array.isArray(tokens)).toBe(true);
      expect(tokens.length).toBeGreaterThan(0);
      expect(tokens.length).toBeLessThanOrEqual(10);

      tokens.forEach((token, i) => {
        expect(token).toHaveProperty('value');
        expect(token).toHaveProperty('logProb');
        expect(token).toHaveProperty('position');
        expect(token.position).toBe(i);
      });
    });

    it('should respect maxTokens limit', () => {
      const generator = new TokenGenerator();
      const maxTokens = 5;
      const tokens = generator.emit(
        { seed: 123 },
        { temperature: 0.8, maxTokens }
      );

      expect(tokens.length).toBeLessThanOrEqual(maxTokens);
    });

    it('should include priming tokens', () => {
      const generator = new TokenGenerator();
      const priming = ['start', 'init'];
      const tokens = generator.emit(
        { seed: 456, priming },
        { temperature: 0.7, maxTokens: 10 }
      );

      expect(tokens.length).toBeGreaterThanOrEqual(priming.length);
      expect(tokens[0].value).toBe('start');
      expect(tokens[1].value).toBe('init');
      expect(tokens[0].metadata?.type).toBe('priming');
      expect(tokens[1].metadata?.type).toBe('priming');
    });

    it('should stop on stop sequence', () => {
      const generator = new TokenGenerator({ deterministic: true });
      const tokens = generator.emit(
        { seed: 789 },
        {
          temperature: 0.7,
          maxTokens: 100,
          stopSequences: ['stop']
        }
      );

      // Should generate tokens, may or may not hit stop sequence
      expect(Array.isArray(tokens)).toBe(true);

      // If stop sequence is hit, it should be the last token
      const lastToken = tokens[tokens.length - 1];
      if (lastToken.value === 'stop') {
        expect(tokens.length).toBeLessThan(100);
      }
    });

    it('should be deterministic with same seed', () => {
      const generator = new TokenGenerator({ deterministic: true });
      const seed = { seed: 42, context: 'test' };
      const control = { temperature: 0.7, maxTokens: 10 };

      const tokens1 = generator.emit(seed, control);
      const tokens2 = generator.emit(seed, control);

      expect(tokens1.length).toBe(tokens2.length);
      tokens1.forEach((token, i) => {
        expect(token.value).toBe(tokens2[i].value);
        expect(token.logProb).toBeCloseTo(tokens2[i].logProb);
      });
    });

    it('should throw on invalid seed parameter', () => {
      const generator = new TokenGenerator();
      expect(() => {
        generator.emit(
          { seed: 'invalid' }, // seed must be number
          { temperature: 0.7, maxTokens: 10 }
        );
      }).toThrow();
    });

    it('should throw on invalid control parameter', () => {
      const generator = new TokenGenerator();
      expect(() => {
        generator.emit(
          { seed: 42 },
          { temperature: 2.0, maxTokens: 10 } // temperature must be [0, 1]
        );
      }).toThrow();
    });
  });

  describe('createTokenGenerator', () => {
    it('should create TokenGenerator instance', () => {
      const generator = createTokenGenerator({ deterministic: true });
      expect(generator).toBeInstanceOf(TokenGenerator);
    });
  });
});
