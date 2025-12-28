/**
 * @file token-generator.mjs
 * @description Token Generator G(σ, κ) - Emits token sequences for KGC-SWARM orchestration
 * @module @unrdf/kgc-swarm/token-generator
 */

import { z } from 'zod';

/**
 * Control parameter schema κ ∈ K
 * Discovered from tooling and swarm configuration
 * @typedef {Object} ControlParameter
 * @property {number} temperature - Sampling temperature [0.0, 1.0]
 * @property {number} topK - Top-k sampling parameter
 * @property {number} topP - Nucleus sampling parameter [0.0, 1.0]
 * @property {number} maxTokens - Maximum tokens to generate
 * @property {string[]} stopSequences - Stop sequences for generation
 * @property {Record<string, number>} [frequencyPenalty] - Token frequency penalties
 */
const ControlParameterSchema = z.object({
  temperature: z.number().min(0.0).max(1.0).default(0.7),
  topK: z.number().int().positive().default(40),
  topP: z.number().min(0.0).max(1.0).default(0.9),
  maxTokens: z.number().int().positive().default(4096),
  stopSequences: z.array(z.string()).default([]),
  frequencyPenalty: z.record(z.string(), z.number()).optional(),
});

/**
 * Seed parameter schema σ ∈ S
 * For reproducibility across swarm runs
 * @typedef {Object} SeedParameter
 * @property {number} [seed] - Random seed for deterministic generation
 * @property {string} [context] - Initial context string
 * @property {string[]} [priming] - Priming tokens
 */
const SeedParameterSchema = z.object({
  seed: z.number().int().optional(),
  context: z.string().default(''),
  priming: z.array(z.string()).default([]),
});

/**
 * Token schema
 * @typedef {Object} Token
 * @property {string} value - Token string value
 * @property {number} logProb - Log probability of token
 * @property {number} position - Position in sequence
 * @property {Record<string, any>} [metadata] - Additional token metadata
 */
const TokenSchema = z.object({
  value: z.string(),
  logProb: z.number(),
  position: z.number().int().nonnegative(),
  metadata: z.record(z.string(), z.any()).optional(),
});

/**
 * TokenGenerator class - Pure function implementation of G(σ, κ)
 * Generates token sequences with control and seed parameters
 *
 * @class TokenGenerator
 * @example
 * ```javascript
 * const generator = new TokenGenerator();
 * const tokens = generator.emit(
 *   { seed: 42, context: 'Initial state' },
 *   { temperature: 0.8, maxTokens: 100 }
 * );
 * console.log(tokens); // Array of Token objects
 * ```
 */
export class TokenGenerator {
  /**
   * Creates a new TokenGenerator instance
   * @param {Object} [options] - Configuration options
   * @param {boolean} [options.deterministic=false] - Use deterministic generation
   */
  constructor(options = {}) {
    /** @type {boolean} */
    this.deterministic = options.deterministic ?? false;

    /** @type {Map<number, Function>} */
    this.rngCache = new Map();
  }

  /**
   * Emit token sequence G(σ, κ) → t₁...t_n
   * Pure function: same inputs always produce same outputs (when deterministic)
   *
   * @param {SeedParameter} σ - Seed parameter for reproducibility
   * @param {ControlParameter} κ - Control parameter from tooling
   * @returns {Token[]} Array of tokens t₁...t_n
   * @throws {Error} If validation fails
   *
   * @example
   * ```javascript
   * const tokens = generator.emit(
   *   { seed: 123, context: 'Start' },
   *   { temperature: 0.7, maxTokens: 50 }
   * );
   * ```
   */
  emit(σ, κ) {
    // Validate inputs
    const seed = SeedParameterSchema.parse(σ);
    const control = ControlParameterSchema.parse(κ);

    // Initialize RNG if seed provided
    const rng = seed.seed !== undefined
      ? this._getSeededRng(seed.seed)
      : Math.random;

    /** @type {Token[]} */
    const tokens = [];

    // Add priming tokens if provided
    for (let i = 0; i < seed.priming.length; i++) {
      tokens.push({
        value: seed.priming[i],
        logProb: 0.0, // Priming tokens have fixed probability
        position: i,
        metadata: { type: 'priming' },
      });
    }

    // Generate tokens up to maxTokens
    let position = tokens.length;
    let currentContext = seed.context;

    while (position < control.maxTokens) {
      const token = this._sampleToken(
        currentContext,
        control,
        rng,
        position
      );

      tokens.push(token);

      // Check stop sequences
      if (control.stopSequences.includes(token.value)) {
        break;
      }

      // Update context
      currentContext += token.value;
      position++;
    }

    return tokens;
  }

  /**
   * Sample a single token using control parameters
   * @private
   * @param {string} context - Current generation context
   * @param {ControlParameter} κ - Control parameters
   * @param {Function} rng - Random number generator
   * @param {number} position - Token position
   * @returns {Token} Generated token
   */
  _sampleToken(context, κ, rng, position) {
    // Simplified token sampling (placeholder for real LLM integration)
    // In production, this would call actual token generation model

    const vocab = this._getVocabulary();
    const probabilities = this._computeProbabilities(
      context,
      vocab,
      κ.temperature
    );

    // Apply top-k filtering
    const topKProbs = this._applyTopK(probabilities, κ.topK);

    // Apply nucleus (top-p) filtering
    const nucleusProbs = this._applyTopP(topKProbs, κ.topP);

    // Sample from filtered distribution
    const sampledIndex = this._categoricalSample(nucleusProbs, rng);
    const token = vocab[sampledIndex];
    const logProb = Math.log(nucleusProbs[sampledIndex]);

    return {
      value: token,
      logProb,
      position,
      metadata: {
        temperature: κ.temperature,
        context_length: context.length,
      },
    };
  }

  /**
   * Get vocabulary for token generation
   * @private
   * @returns {string[]} Token vocabulary
   */
  _getVocabulary() {
    // Simplified vocabulary for demonstration
    // In production, use actual tokenizer vocabulary
    return [
      'agent', 'task', 'execute', 'complete', 'start', 'stop',
      'observe', 'plan', 'reflect', 'coordinate', 'emit', 'receive',
      'state', 'action', 'reward', 'policy', 'value', 'gradient',
    ];
  }

  /**
   * Compute token probabilities with temperature scaling
   * @private
   * @param {string} context - Generation context
   * @param {string[]} vocab - Vocabulary
   * @param {number} temperature - Temperature parameter
   * @returns {number[]} Probability distribution
   */
  _computeProbabilities(context, vocab, temperature) {
    // Simplified: uniform distribution with temperature
    // In production, use actual language model
    const logits = vocab.map(() => Math.random());

    // Apply temperature scaling
    const scaledLogits = logits.map(l => l / temperature);

    // Softmax
    const expLogits = scaledLogits.map(l => Math.exp(l));
    const sum = expLogits.reduce((a, b) => a + b, 0);

    return expLogits.map(e => e / sum);
  }

  /**
   * Apply top-k filtering to probability distribution
   * @private
   * @param {number[]} probs - Probability distribution
   * @param {number} k - Top-k parameter
   * @returns {number[]} Filtered probabilities
   */
  _applyTopK(probs, k) {
    const indexed = probs.map((p, i) => ({ p, i }));
    indexed.sort((a, b) => b.p - a.p);

    const topK = indexed.slice(0, k);
    const filtered = new Array(probs.length).fill(0);

    const sum = topK.reduce((acc, { p }) => acc + p, 0);
    topK.forEach(({ p, i }) => {
      filtered[i] = p / sum; // Renormalize
    });

    return filtered;
  }

  /**
   * Apply nucleus (top-p) filtering to probability distribution
   * @private
   * @param {number[]} probs - Probability distribution
   * @param {number} p - Nucleus parameter
   * @returns {number[]} Filtered probabilities
   */
  _applyTopP(probs, p) {
    const indexed = probs.map((prob, i) => ({ p: prob, i }));
    indexed.sort((a, b) => b.p - a.p);

    let cumSum = 0;
    const nucleus = [];

    for (const item of indexed) {
      cumSum += item.p;
      nucleus.push(item);
      if (cumSum >= p) break;
    }

    const filtered = new Array(probs.length).fill(0);
    const sum = nucleus.reduce((acc, { p: prob }) => acc + prob, 0);

    nucleus.forEach(({ p: prob, i }) => {
      filtered[i] = prob / sum; // Renormalize
    });

    return filtered;
  }

  /**
   * Sample from categorical distribution
   * @private
   * @param {number[]} probs - Probability distribution
   * @param {Function} rng - Random number generator
   * @returns {number} Sampled index
   */
  _categoricalSample(probs, rng) {
    const sample = rng();
    let cumSum = 0;

    for (let i = 0; i < probs.length; i++) {
      cumSum += probs[i];
      if (sample <= cumSum) {
        return i;
      }
    }

    return probs.length - 1; // Fallback
  }

  /**
   * Get seeded random number generator
   * Uses Linear Congruential Generator (LCG) for determinism
   * @private
   * @param {number} seed - Random seed
   * @returns {Function} RNG function
   */
  _getSeededRng(seed) {
    // Don't cache - create fresh RNG each time for true determinism
    // Each emit() call should produce identical results with same seed

    // LCG parameters (Numerical Recipes)
    const a = 1664525;
    const c = 1013904223;
    const m = Math.pow(2, 32);

    let state = seed;

    const rng = () => {
      state = (a * state + c) % m;
      return state / m;
    };

    return rng;
  }
}

/**
 * Create a new TokenGenerator instance
 * @param {Object} [options] - Configuration options
 * @returns {TokenGenerator} TokenGenerator instance
 */
export function createTokenGenerator(options = {}) {
  return new TokenGenerator(options);
}

// Export schemas for validation
export {
  ControlParameterSchema,
  SeedParameterSchema,
  TokenSchema,
};
