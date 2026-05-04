/**
 * KGEN Injection API
 *
 * Main API for injection operations. Provides a clean interface
 * for the template engine to perform atomic, idempotent injections.
 */

import { InjectionEngine } from './injection-engine.js';
import { DEFAULT_CONFIG } from './constants.js';

// Global injection engine instance
let globalInjectionEngine = null;

/**
 * Initialize injection system with configuration
 */
export function initializeInjection(config = {}) {
  globalInjectionEngine = new InjectionEngine({ ...DEFAULT_CONFIG, ...config });
  return globalInjectionEngine;
}

/**
 * Main injection function - atomic, idempotent, deterministic
 */
export async function inject(templateConfig, content, variables = {}, options = {}) {
  const engine = getInjectionEngine(options.config);

  try {
    return await engine.inject(templateConfig, content, variables);
  } catch (error) {
    // Enhance error with context
    error.templateConfig = templateConfig;
    error.variables = variables;
    throw error;
  }
}

/**
 * Perform dry run to see what would be injected
 */
export async function dryRun(templateConfig, content, variables = {}, options = {}) {
  const engine = getInjectionEngine(options.config);
  return await engine.dryRun(templateConfig, content, variables);
}

/**
 * Undo a previous injection operation
 */
export async function undo(operationId, options = {}) {
  const engine = getInjectionEngine(options.config);
  return await engine.undo(operationId);
}

/**
 * Get injection operation history
 */
export function getOperationHistory(options = {}) {
  const engine = getInjectionEngine(options.config);
  return engine.getOperationHistory();
}

/**
 * Batch injection operations
 */
export async function batchInject(operations, options = {}) {
  const engine = getInjectionEngine(options.config);
  const results = [];
  const errors = [];

  for (const operation of operations) {
    try {
      const result = await engine.inject(
        operation.templateConfig,
        operation.content,
        operation.variables || {}
      );
      results.push({ ...result, operation });
    } catch (error) {
      errors.push({ error, operation });

      // Stop on first error unless continueOnError is true
      if (!options.continueOnError) {
        break;
      }
    }
  }

  return {
    results,
    errors,
    totalOperations: operations.length,
    successfulOperations: results.length,
    failedOperations: errors.length
  };
}

/**
 * Validate injection configuration without executing
 */
export async function validateInjectionConfig(templateConfig, variables = {}, options = {}) {
  const engine = getInjectionEngine(options.config);

  try {
    // This would use internal validation methods
    const targets = await engine.targetResolver.resolveTargets(templateConfig, variables);
    const validationResults = await Promise.all(
      targets.map(target => engine.validationEngine.validateTarget(target))
    );

    return {
      valid: validationResults.every(r => r.valid),
      targets: targets.length,
      validationResults,
      errors: validationResults.flatMap(r => r.errors),
      warnings: validationResults.flatMap(r => r.warnings)
    };
  } catch (error) {
    return {
      valid: false,
      error: error.message,
      targets: 0
    };
  }
}

/**
 * Get injection system status and metrics
 */
export function getInjectionStatus(options = {}) {
  const engine = getInjectionEngine(options.config);

  return {
    initialized: !!engine,
    config: engine.config,
    activeOperations: engine.activeOperations.size,
    operationHistory: engine.operationHistory.length,
    cacheStats: {
      idempotency: engine.idempotencyManager.contentCache.size,
      validation: engine.validationEngine.validationCache.size
    }
  };
}

/**
 * Clear all caches (useful for testing or memory management)
 */
export function clearCaches(options = {}) {
  const engine = getInjectionEngine(options.config);

  engine.idempotencyManager.clearCache();
  engine.validationEngine.clearCache();

  return {
    cleared: true,
    timestamp: Date.now()
  };
}

/**
 * Helper function to get or create injection engine
 */
function getInjectionEngine(config) {
  if (config) {
    // Create temporary engine with custom config
    return new InjectionEngine(config);
  }

  if (!globalInjectionEngine) {
    // Create default engine
    globalInjectionEngine = new InjectionEngine(DEFAULT_CONFIG);
  }

  return globalInjectionEngine;
}

/**
 * Export engine class for advanced usage
 */
export { InjectionEngine };

/**
 * Template integration helper - processes template with injection support
 */
export async function processTemplate(template, data, options = {}) {
  const { frontmatter, content } = parseTemplate(template);

  // Check if this template has injection configuration
  if (frontmatter.inject) {
    return await inject(frontmatter, content, data, options);
  }

  // Regular template processing would go here
  throw new Error('Regular template processing not implemented - injection only');
}

/**
 * Simple frontmatter parser for templates
 */
function parseTemplate(template) {
  const frontmatterMatch = template.match(/^---\n([\s\S]*?)\n---\n([\s\S]*)$/);

  if (!frontmatterMatch) {
    return {
      frontmatter: {},
      content: template
    };
  }

  try {
    // Simple YAML-like parsing - in production use proper YAML parser
    const frontmatterText = frontmatterMatch[1];
    const frontmatter = parseFrontmatter(frontmatterText);
    const content = frontmatterMatch[2];

    return { frontmatter, content };
  } catch (error) {
    throw new Error(`Failed to parse template frontmatter: ${error.message}`);
  }
}

/**
 * Simple frontmatter parser (YAML-like)
 */
function parseFrontmatter(text) {
  const result = {};
  const lines = text.split('\n');

  for (const line of lines) {
    const trimmed = line.trim();
    if (!trimmed || trimmed.startsWith('#')) continue;

    const colonIndex = trimmed.indexOf(':');
    if (colonIndex === -1) continue;

    const key = trimmed.substring(0, colonIndex).trim();
    const value = trimmed.substring(colonIndex + 1).trim();

    // Simple value parsing
    if (value === 'true') {
      result[key] = true;
    } else if (value === 'false') {
      result[key] = false;
    } else if (/^\d+$/.test(value)) {
      result[key] = parseInt(value);
    } else if (value.startsWith('"') && value.endsWith('"')) {
      result[key] = value.slice(1, -1);
    } else if (value.startsWith("'") && value.endsWith("'")) {
      result[key] = value.slice(1, -1);
    } else {
      result[key] = value;
    }
  }

  return result;
}

/**
 * Export all injection modes for reference
 */
export { INJECTION_MODES, ERROR_CODES } from './constants.js';