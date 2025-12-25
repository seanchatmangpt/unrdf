/**
 * @kgn/templates - Deterministic Nunjucks Template System
 *
 * Enhanced with KGEN Injection Operations System:
 * - Nunjucks integration with custom filters
 * - Frontmatter parsing (YAML)
 * - Variable extraction and validation
 * - Deterministic rendering
 * - Template linting for determinism
 * - Atomic, idempotent injection operations (NEW)
 * - Marker-based targeting and rollback capabilities (NEW)
 */

// Import classes for local use in factory functions
import { TemplateEngine, EnhancedTemplateEngine } from './engine/template-engine.js';
import { TemplateInheritanceEngine } from './inheritance/index.js';
import { enhanceKgenWithInjection } from './injection/integration.js';
import { initializeInjection } from './injection/api.js';

// Re-export for external consumers
export { TemplateEngine, EnhancedTemplateEngine } from './engine/template-engine.js';
export { TemplateInheritanceEngine } from './inheritance/index.js';
export { createCustomFilters } from './filters/index.js';
export { FrontmatterParser } from './parser/frontmatter.js';
export { VariableExtractor } from './parser/variables.js';
export { TemplateLinter } from './linter/determinism.js';
export { DeterministicRenderer } from './renderer/deterministic.js';

// Base template system
export * as BaseTemplates from './base/index.js';
export { KGenTemplateBase } from './base/template-base.js';

// Template packs
export * as NextJSTemplates from './templates/nextjs/index.js';
export * as OfficeTemplates from './templates/office/index.js';
export * as LaTeXTemplates from './templates/latex/index.js';

// Re-export commonly used types and utilities
export {
  renderTemplate,
  validateTemplate,
  extractVariables,
  lintTemplate
} from './utils/template-utils.js';

// INJECTION SYSTEM EXPORTS (NEW)
// Main injection API
export {
  inject,
  dryRun,
  undo,
  initializeInjection,
  getOperationHistory,
  batchInject,
  validateInjectionConfig,
  getInjectionStatus,
  clearCaches
} from './injection/api.js';

// Integration helpers
export {
  enhanceKgenWithInjection,
  createInjectionTemplateLoader,
  injectionValidators
} from './injection/integration.js';

// Core injection classes (for advanced usage)
export {
  InjectionEngine,
  AtomicWriter,
  IdempotencyManager,
  TargetResolver,
  ValidationEngine,
  RollbackManager
} from './injection/index.js';

// Constants and types
export {
  INJECTION_MODES,
  ERROR_CODES,
  DEFAULT_CONFIG as INJECTION_CONFIG
} from './injection/constants.js';

// Enhanced engine factory with inheritance and injection support
export function createEngine(options = {}) {
  // Use EnhancedTemplateEngine by default for inheritance support
  const engineClass = options.useBasicEngine ? TemplateEngine : EnhancedTemplateEngine;
  const engine = new engineClass(options);

  // Enhance with injection capabilities by default
  if (options.enableInjection !== false) {
    return enhanceKgenWithInjection(engine, options.injection || {});
  }

  return engine;
}

// Inheritance-specific engine factory
export function createInheritanceEngine(options = {}) {
  return new TemplateInheritanceEngine(options);
}

// Enhanced template engine factory (inheritance + all features)
export function createEnhancedEngine(options = {}) {
  return new EnhancedTemplateEngine({
    enableInheritance: true,
    deterministicMode: true,
    enableCache: true,
    ...options
  });
}

// Injection-specific engine factory
export function createInjectionEngine(injectionConfig = {}) {
  return initializeInjection(injectionConfig);
}