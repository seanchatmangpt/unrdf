/**
 * DETERMINISTIC Template Engine - Complete Exports
 *
 * 4-Stage Pipeline: plan → render → post → attest
 * 100% Deterministic guarantees with cryptographic attestation
 */

// Import classes first so they can be used in factory functions
import { TemplateEngine, EnhancedTemplateEngine as _EnhancedTemplateEngine } from './template-engine.js';
import { DeterministicRenderer } from './renderer.js';
import { DeterministicPipeline } from './pipeline.js';

// Re-export for external use
export { TemplateEngine, EnhancedTemplateEngine } from './template-engine.js';
export { DeterministicRenderer } from './renderer.js';
export { DeterministicPipeline } from './pipeline.js';

// Factory for deterministic engine with complete pipeline
/**
 *
 */
export function createDeterministicEngine(options = {}) {
  const pipeline = new DeterministicPipeline(options);

  return {
    pipeline,
    renderer: pipeline.renderer,

    async render(template, data, opts = {}) {
      return await pipeline.execute(template, data, opts);
    },

    async verifyDeterminism(template, data, iterations = 3) {
      return await pipeline.verifyDeterminism(template, data, iterations);
    },

    async renderBatch(batch, opts = {}) {
      return await pipeline.executeBatch(batch, opts);
    },

    getStats() { return pipeline.getStats(); },
    resetStats() { pipeline.resetStats(); }
  };
}

export default {
  TemplateEngine,
  DeterministicRenderer,
  DeterministicPipeline,
  createDeterministicEngine
};