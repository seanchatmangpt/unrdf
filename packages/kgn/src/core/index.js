/**
 * KGEN Core Template Engine - Native implementation without nunjucks
 *
 * Pipeline: plan → render → post → attest
 * Features: Deterministic rendering, custom filters, attestation support
 */

// Import class first so it can be used in factory function
import { KGenTemplateEngine } from './kgen-engine.js';

// Export all classes
export { KGenTemplateEngine } from './kgen-engine.js';
export { KGenParser } from './parser.js';
export { KGenFilters } from './filters.js';
export { KGenRenderer } from './renderer.js';
export { KGenPostProcessor } from './post-processor.js';
export { KGenAttestor } from './attestor.js';

// Convenience factory function
/**
 *
 */
export function createKGenEngine(options = {}) {
  return new KGenTemplateEngine(options);
}

// Default export
export default KGenTemplateEngine;