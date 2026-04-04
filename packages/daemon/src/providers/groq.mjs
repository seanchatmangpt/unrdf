/**
 * @file Groq Language Model Provider for UNRDF Daemon
 * @module @unrdf/daemon/providers/groq
 * @description Integrates Groq API with the daemon for LLM-powered operations
 */

import { groq, createGroq } from '@ai-sdk/groq';
import { z } from 'zod';

/**
 * @typedef {Object} GroqProviderConfig
 * @property {string} [apiKey] - Groq API key (defaults to GROQ_API_KEY env var)
 * @property {string} [baseURL] - Custom Groq API base URL
 * @property {string} [model='openai/gpt-oss-20b'] - Default Groq model ID
 * @property {('parsed'|'raw'|'hidden')} [reasoningFormat] - Reasoning format for reasoning models
 * @property {('low'|'medium'|'high'|'none'|'default')} [reasoningEffort] - Reasoning effort level
 * @property {boolean} [structuredOutputs=true] - Enable structured outputs
 * @property {boolean} [strictJsonSchema=true] - Use strict JSON schema validation
 * @property {boolean} [parallelToolCalls=true] - Enable parallel tool calling
 * @property {('on_demand'|'performance'|'flex'|'auto')} [serviceTier='on_demand'] - Service tier for requests
 * @property {number} [timeout=30000] - Request timeout in milliseconds
 */

/**
 * Groq provider configuration schema
 */
export const GroqProviderConfigSchema = z.object({
  apiKey: z.string().optional().describe('Groq API key (defaults to GROQ_API_KEY env var)'),
  baseURL: z.string().url().optional().describe('Custom Groq API base URL'),
  model: z.string().default('openai/gpt-oss-20b').describe('Default Groq model ID'),
  reasoningFormat: z.enum(['parsed', 'raw', 'hidden']).optional().describe('Reasoning format for reasoning models'),
  reasoningEffort: z.enum(['low', 'medium', 'high', 'none', 'default']).optional().describe('Reasoning effort level'),
  structuredOutputs: z.boolean().default(true).describe('Enable structured outputs'),
  strictJsonSchema: z.boolean().default(true).describe('Use strict JSON schema validation'),
  parallelToolCalls: z.boolean().default(true).describe('Enable parallel tool calling'),
  serviceTier: z.enum(['on_demand', 'performance', 'flex', 'auto']).default('on_demand').describe('Service tier for requests'),
  timeout: z.number().default(30000).describe('Request timeout in milliseconds'),
});

/**
 * Groq provider instance manager
 */
class GroqProvider {
  #config;
  #instance;

  /**
   * @param {Partial<GroqProviderConfig>} [config] - Configuration options
   */
  constructor(config = {}) {
    const validated = GroqProviderConfigSchema.parse(config);
    this.#config = validated;

    // Create provider instance with custom config if needed
    if (config.apiKey || config.baseURL) {
      this.#instance = createGroq({
        apiKey: config.apiKey,
        baseURL: config.baseURL,
      });
    } else {
      this.#instance = groq;
    }
  }

  /**
   * Get the configured model instance
   * @param {string} [modelId] - Model ID to use (defaults to configured model)
   * @returns {*} Groq model instance
   */
  getModel(modelId) {
    const id = modelId || this.#config.model;
    return this.#instance(id);
  }

  /**
   * Get default model instance
   * @returns {*} Default Groq model instance
   */
  getDefaultModel() {
    return this.#instance(this.#config.model);
  }

  /**
   * Get provider options for AI SDK
   * @returns {Object} Provider options object
   */
  getProviderOptions() {
    return {
      reasoningFormat: this.#config.reasoningFormat,
      reasoningEffort: this.#config.reasoningEffort,
      structuredOutputs: this.#config.structuredOutputs,
      strictJsonSchema: this.#config.strictJsonSchema,
      parallelToolCalls: this.#config.parallelToolCalls,
      serviceTier: this.#config.serviceTier,
    };
  }

  /**
   * Get configuration
   * @returns {GroqProviderConfig} Configuration object
   */
  getConfig() {
    return { ...this.#config };
  }
}

// Global instance
let _groqProvider = null;

/**
 * Initialize Groq provider (call once at daemon startup)
 * @param {Partial<GroqProviderConfig>} [config] - Configuration options
 * @returns {GroqProvider} Initialized provider instance
 */
export function initializeGroqProvider(config = {}) {
  _groqProvider = new GroqProvider(config);
  return _groqProvider;
}

/**
 * Get or create Groq provider instance
 * @returns {GroqProvider} Groq provider instance
 */
export function getGroqProvider() {
  if (!_groqProvider) {
    _groqProvider = new GroqProvider();
  }
  return _groqProvider;
}

/**
 * Create a new Groq provider instance with custom config
 * @param {Partial<GroqProviderConfig>} [config] - Custom configuration
 * @returns {GroqProvider} New provider instance
 */
export function createGroqProvider(config = {}) {
  return new GroqProvider(config);
}

/**
 * Export default provider for convenience
 */
export { groq };
