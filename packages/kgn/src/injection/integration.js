/**
 * KGEN Injection Integration
 *
 * Integrates injection capabilities with the existing KGEN template engine.
 * Provides seamless injection support for template processing.
 */

import { inject, dryRun, processTemplate as _processTemplate, initializeInjection } from './api.js';
import { DEFAULT_CONFIG, INJECTION_MODES } from './constants.js';

/**
 * Enhance KGEN engine with injection capabilities
 */
export function enhanceKgenWithInjection(kgenEngine, injectionConfig = {}) {
  const config = { ...DEFAULT_CONFIG, ...injectionConfig };

  // Initialize injection system
  const injectionEngine = initializeInjection(config);

  // Store original methods
  const originalRender = kgenEngine.render;
  const originalRenderString = kgenEngine.renderString;

  /**
   * Enhanced render method with injection support
   */
  kgenEngine.render = async function(templatePath, data = {}, options = {}) {
    // First, try to read and parse the template
    const template = await this.getTemplate(templatePath);

    // Check if template has injection configuration in frontmatter
    if (template.frontmatter && template.frontmatter.inject) {
      return await processInjectionTemplate(template, data, options, injectionEngine);
    }

    // Fall back to original render for non-injection templates
    return originalRender.call(this, templatePath, data, options);
  };

  /**
   * Enhanced renderString with injection support
   */
  kgenEngine.renderString = async function(templateString, data = {}, options = {}) {
    // Parse template string for frontmatter
    const parsed = parseTemplateString(templateString);

    if (parsed.frontmatter && parsed.frontmatter.inject) {
      return await processInjectionTemplate(parsed, data, options, injectionEngine);
    }

    // Fall back to original renderString
    return originalRenderString.call(this, templateString, data, options);
  };

  /**
   * Add injection-specific methods to the engine
   */
  kgenEngine.inject = async function(templateConfig, content, variables = {}) {
    return await inject(templateConfig, content, variables, { config });
  };

  kgenEngine.dryRunInjection = async function(templateConfig, content, variables = {}) {
    return await dryRun(templateConfig, content, variables, { config });
  };

  kgenEngine.getInjectionHistory = function() {
    return injectionEngine.getOperationHistory();
  };

  kgenEngine.undoInjection = async function(operationId) {
    return await injectionEngine.undo(operationId);
  };

  /**
   * Batch template processing with injection support
   */
  kgenEngine.processBatch = async function(templates, globalData = {}, options = {}) {
    const results = [];
    const errors = [];

    for (const template of templates) {
      try {
        const templateData = { ...globalData, ...(template.data || {}) };
        let result;

        if (template.content) {
          result = await this.renderString(template.content, templateData, options);
        } else if (template.path) {
          result = await this.render(template.path, templateData, options);
        } else {
          throw new Error('Template must have either content or path');
        }

        results.push({
          template,
          result,
          success: true
        });

      } catch (error) {
        errors.push({
          template,
          error: error.message,
          success: false
        });

        if (!options.continueOnError) {
          break;
        }
      }
    }

    return {
      results,
      errors,
      total: templates.length,
      successful: results.length,
      failed: errors.length
    };
  };

  return kgenEngine;
}

/**
 * Process injection template
 */
async function processInjectionTemplate(template, data, options, injectionEngine) {
  const { frontmatter, content } = template;

  // Render the template content first
  const renderedContent = await renderTemplateContent(content, data, options);

  // Perform injection
  const injectionResult = await injectionEngine.inject(frontmatter, renderedContent, data);

  return {
    injectionResult,
    operationId: injectionResult.operationId,
    success: injectionResult.success,
    skipped: injectionResult.skipped,
    targets: injectionResult.targets,
    content: renderedContent
  };
}

/**
 * Render template content (without injection)
 */
async function renderTemplateContent(content, data, _options) {
  // Simple variable interpolation - in production use proper template engine
  return content.replace(/\{\{(\w+)\}\}/g, (match, variable) => {
    return data[variable] || match;
  });
}

/**
 * Parse template string for frontmatter
 */
function parseTemplateString(templateString) {
  const frontmatterMatch = templateString.match(/^---\n([\s\S]*?)\n---\n([\s\S]*)$/);

  if (!frontmatterMatch) {
    return {
      frontmatter: {},
      content: templateString
    };
  }

  const frontmatter = parseFrontmatter(frontmatterMatch[1]);
  const content = frontmatterMatch[2];

  return { frontmatter, content };
}

/**
 * Simple frontmatter parser
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
    let value = trimmed.substring(colonIndex + 1).trim();

    // Parse arrays
    if (value.startsWith('[') && value.endsWith(']')) {
      value = value.slice(1, -1).split(',').map(v => v.trim().replace(/['"]/g, ''));
    }
    // Parse booleans
    else if (value === 'true') {
      value = true;
    } else if (value === 'false') {
      value = false;
    }
    // Parse numbers
    else if (/^\d+$/.test(value)) {
      value = parseInt(value);
    }
    // Remove quotes
    else if ((value.startsWith('"') && value.endsWith('"')) ||
             (value.startsWith("'") && value.endsWith("'"))) {
      value = value.slice(1, -1);
    }

    result[key] = value;
  }

  return result;
}

/**
 * Create injection-aware template loader
 */
export function createInjectionTemplateLoader(baseLoader, _injectionConfig = {}) {
  return {
    ...baseLoader,

    async loadTemplate(templatePath, options = {}) {
      const template = await baseLoader.loadTemplate(templatePath, options);

      // Parse for injection configuration
      const parsed = parseTemplateString(template.content || template);

      return {
        ...template,
        frontmatter: parsed.frontmatter,
        content: parsed.content,
        hasInjection: !!(parsed.frontmatter && parsed.frontmatter.inject),
        injectionMode: parsed.frontmatter?.mode || INJECTION_MODES.APPEND
      };
    }
  };
}

/**
 * Validation helpers for injection templates
 */
export const injectionValidators = {
  /**
   * Validate injection template configuration
   */
  validateTemplate(template) {
    const errors = [];

    if (!template.frontmatter) {
      return { valid: true, errors: [] }; // Non-injection template
    }

    const fm = template.frontmatter;

    if (fm.inject && !fm.to) {
      errors.push('Injection templates must specify "to" target');
    }

    if (fm.mode && !Object.values(INJECTION_MODES).includes(fm.mode)) {
      errors.push(`Invalid injection mode: ${fm.mode}`);
    }

    if (fm.mode === INJECTION_MODES.BEFORE && !fm.target) {
      errors.push('Before mode requires "target" pattern');
    }

    if (fm.mode === INJECTION_MODES.AFTER && !fm.target) {
      errors.push('After mode requires "target" pattern');
    }

    if (fm.mode === INJECTION_MODES.REPLACE && !fm.target) {
      errors.push('Replace mode requires "target" pattern');
    }

    if (fm.mode === INJECTION_MODES.LINE_AT && typeof fm.lineNumber !== 'number') {
      errors.push('LineAt mode requires "lineNumber" as number');
    }

    return {
      valid: errors.length === 0,
      errors
    };
  },

  /**
   * Validate injection context data
   */
  validateData(data, template) {
    const errors = [];
    const fm = template.frontmatter;

    if (!fm || !fm.inject) {
      return { valid: true, errors: [] };
    }

    // Check for required variables in template content
    const requiredVars = extractTemplateVariables(template.content);

    for (const variable of requiredVars) {
      if (!(variable in data)) {
        errors.push(`Required template variable missing: ${variable}`);
      }
    }

    return {
      valid: errors.length === 0,
      errors
    };
  }
};

/**
 * Extract template variables from content
 */
function extractTemplateVariables(content) {
  const variables = new Set();
  const matches = content.matchAll(/\{\{(\w+)\}\}/g);

  for (const match of matches) {
    variables.add(match[1]);
  }

  return Array.from(variables);
}

/**
 * Export integration utilities
 */
export {
  inject,
  dryRun,
  initializeInjection,
  INJECTION_MODES,
  DEFAULT_CONFIG
};