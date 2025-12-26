/**
 * KGEN Native Template Engine - Deterministic template processing without nunjucks
 *
 * Pipeline: plan → render → post → attest
 * Supports: Variables {{ var }}, conditionals {% if %}, loops {% for %}, filters {{ var | filter }}
 * Deterministic: All operations produce stable, reproducible output
 */

import crypto from 'crypto';
import { KGenParser } from './parser.js';
import { KGenFilters } from './filters.js';
import { KGenRenderer } from './renderer.js';
import { KGenPostProcessor } from './post-processor.js';
import { KGenAttestor } from './attestor.js';

/**
 *
 */
export class KGenTemplateEngine {
  /**
   *
   */
  constructor(options = {}) {
    this.options = {
      strictMode: options.strictMode !== false,
      deterministicMode: options.deterministicMode !== false,
      staticBuildTime: options.staticBuildTime || '2024-01-01T00:00:00.000Z',
      maxDepth: options.maxDepth || 10,
      enableIncludes: options.enableIncludes !== false,
      enableAttestation: options.enableAttestation !== false,
      ...options
    };

    // Initialize pipeline components
    this.parser = new KGenParser(this.options);
    this.filters = new KGenFilters(this.options);
    this.renderer = new KGenRenderer(this.options);
    this.postProcessor = new KGenPostProcessor(this.options);
    this.attestor = new KGenAttestor(this.options);

    // Register core filters
    this.registerCoreFilters();
  }

  /**
   * PHASE 1: PLAN - Parse and analyze template
   */
  async plan(template, context = {}) {
    try {
      const parseResult = await this.parser.parse(template);

      const plan = {
        success: true,
        template: parseResult.template,
        frontmatter: parseResult.frontmatter,
        variables: parseResult.variables,
        expressions: parseResult.expressions,
        includes: parseResult.includes,
        complexity: this.calculateComplexity(parseResult),
        hash: this.hashContent(template),
        contextHash: this.hashContent(JSON.stringify(context)),
        timestamp: this.options.deterministicMode ? this.options.staticBuildTime : new Date().toISOString()
      };

      // Validate context against required variables
      if (this.options.strictMode) {
        this.validateContext(plan.variables, context, plan.frontmatter);
      }

      return plan;
    } catch (error) {
      return {
        success: false,
        error: error.message,
        phase: 'plan',
        timestamp: this.options.deterministicMode ? this.options.staticBuildTime : new Date().toISOString()
      };
    }
  }

  /**
   * PHASE 2: RENDER - Execute template with context
   */
  async render(plan, context = {}) {
    if (!plan.success) {
      return { success: false, error: 'Invalid plan provided', phase: 'render' };
    }

    try {
      // Merge frontmatter with context
      const mergedContext = {
        ...plan.frontmatter,
        ...context,
        __kgen: {
          renderTime: this.options.deterministicMode ? this.options.staticBuildTime : new Date().toISOString(),
          templateHash: plan.hash,
          contextHash: plan.contextHash,
          deterministicMode: this.options.deterministicMode
        }
      };

      // Execute rendering
      const renderResult = await this.renderer.render(plan.template, mergedContext, {
        variables: plan.variables,
        expressions: plan.expressions,
        filters: this.filters
      });

      return {
        success: true,
        content: renderResult.content,
        context: mergedContext,
        metadata: {
          ...renderResult.metadata,
          phase: 'render',
          renderTime: mergedContext.__kgen.renderTime
        }
      };
    } catch (error) {
      return {
        success: false,
        error: error.message,
        phase: 'render',
        context: context
      };
    }
  }

  /**
   * PHASE 3: POST - Post-process rendered content
   */
  async post(renderResult) {
    if (!renderResult.success) {
      return renderResult;
    }

    try {
      const postResult = await this.postProcessor.process(renderResult.content, {
        normalizeWhitespace: true,
        trimLines: true,
        ensureFinalNewline: true,
        deterministicMode: this.options.deterministicMode
      });

      return {
        ...renderResult,
        content: postResult.content,
        metadata: {
          ...renderResult.metadata,
          post: postResult.metadata,
          phase: 'post'
        }
      };
    } catch (error) {
      return {
        success: false,
        error: error.message,
        phase: 'post',
        originalResult: renderResult
      };
    }
  }

  /**
   * PHASE 4: ATTEST - Generate attestation of deterministic output
   */
  async attest(postResult) {
    if (!postResult.success || !this.options.enableAttestation) {
      return postResult;
    }

    try {
      const attestation = await this.attestor.attest(postResult.content, {
        templateHash: postResult.metadata?.renderTime ?
          this.hashContent(postResult.metadata.renderTime) : undefined,
        contextHash: postResult.context ?
          this.hashContent(JSON.stringify(postResult.context)) : undefined,
        deterministicMode: this.options.deterministicMode
      });

      return {
        ...postResult,
        attestation,
        metadata: {
          ...postResult.metadata,
          attestation: attestation.metadata,
          phase: 'attest'
        }
      };
    } catch (error) {
      return {
        success: false,
        error: error.message,
        phase: 'attest',
        originalResult: postResult
      };
    }
  }

  /**
   * Complete pipeline: plan → render → post → attest
   */
  async execute(template, context = {}) {
    const plan = await this.plan(template, context);
    const renderResult = await this.render(plan, context);
    const postResult = await this.post(renderResult);
    const finalResult = await this.attest(postResult);

    return finalResult;
  }

  /**
   * Simple render method for basic use cases
   */
  async renderTemplate(template, context = {}) {
    // Use a simplified pipeline without post-processing for simple rendering
    const plan = await this.plan(template, context);
    if (!plan.success) return '';

    const renderResult = await this.render(plan, context);
    if (!renderResult.success) return '';

    // Return content without post-processing to avoid final newlines
    return renderResult.content || '';
  }

  /**
   * Register core filters
   */
  registerCoreFilters() {
    // Text filters
    this.filters.register('upper', (str) => String(str || '').toUpperCase());
    this.filters.register('lower', (str) => String(str || '').toLowerCase());
    this.filters.register('trim', (str) => String(str || '').trim());
    this.filters.register('replace', (str, search, replace) =>
      String(str || '').replace(new RegExp(search, 'g'), replace));
    this.filters.register('split', (str, separator) =>
      String(str || '').split(separator || ''));
    this.filters.register('join', (arr, separator) =>
      Array.isArray(arr) ? arr.join(separator || '') : arr);
    this.filters.register('slice', (str, start, end) =>
      String(str || '').slice(start, end));

    // Data filters
    this.filters.register('default', (value, defaultValue) =>
      (value === null || value === undefined || value === '') ? defaultValue : value);
    this.filters.register('unique', (arr) =>
      Array.isArray(arr) ? [...new Set(arr)] : arr);
    this.filters.register('sort', (arr, key) => {
      if (!Array.isArray(arr)) return arr;
      return [...arr].sort((a, b) => {
        const aVal = key ? a[key] : a;
        const bVal = key ? b[key] : b;
        return aVal > bVal ? 1 : aVal < bVal ? -1 : 0;
      });
    });
    this.filters.register('groupby', (arr, key) => {
      if (!Array.isArray(arr)) return {};
      return arr.reduce((groups, item) => {
        const groupKey = typeof item === 'object' ? item[key] : item;
        groups[groupKey] = groups[groupKey] || [];
        groups[groupKey].push(item);
        return groups;
      }, {});
    });
    this.filters.register('map', (arr, key) => {
      if (!Array.isArray(arr)) return arr;
      return arr.map(item => typeof item === 'object' ? item[key] : item);
    });
    this.filters.register('sum', (arr, key) => {
      if (!Array.isArray(arr)) return 0;
      return arr.reduce((sum, item) => {
        const val = key ? item[key] : item;
        return sum + (Number(val) || 0);
      }, 0);
    });
    this.filters.register('count', (arr) => Array.isArray(arr) ? arr.length : 0);

    // Format filters
    this.filters.register('json', (obj, indent) => {
      try {
        return JSON.stringify(obj, null, indent || 0);
      } catch (e) {
        return '{}';
      }
    });
    this.filters.register('md', (str) => {
      // Basic markdown escaping
      return String(str || '').replace(/[*_`]/g, '\\$&');
    });
    this.filters.register('csv', (arr) => {
      if (!Array.isArray(arr)) return '';
      return arr.map(item =>
        typeof item === 'object' ? JSON.stringify(item) : String(item)
      ).join(',');
    });
  }

  /**
   * Calculate template complexity score
   */
  calculateComplexity(parseResult) {
    const { variables = [], expressions = [], includes = [] } = parseResult;
    return variables.length + expressions.length * 2 + includes.length * 3;
  }

  /**
   * Validate context has required variables
   */
  validateContext(variables, context, frontmatter) {
    const missing = [];
    const available = new Set([
      ...Object.keys(context || {}),
      ...Object.keys(frontmatter || {}),
      '__kgen'
    ]);

    // Common loop variables that are typically not required
    const loopVars = new Set(['item', 'index', 'key', 'value', 'loop']);

    variables.forEach(varName => {
      if (!available.has(varName) && !loopVars.has(varName)) {
        missing.push(varName);
      }
    });

    if (missing.length > 0) {
      throw new Error(`Missing required variables: ${missing.join(', ')}`);
    }
  }

  /**
   * Generate deterministic content hash
   */
  hashContent(content) {
    return crypto.createHash('sha256').update(String(content || ''), 'utf8').digest('hex');
  }

  /**
   * Get engine statistics
   */
  getStats() {
    return {
      ...this.options,
      filterCount: this.filters.getFilterCount(),
      version: '2.0.0-kgen-native'
    };
  }

  /**
   * Verify deterministic behavior across multiple runs
   */
  async verifyDeterminism(template, context, iterations = 3) {
    const results = [];
    const hashes = new Set();

    for (let i = 0; i < iterations; i++) {
      const result = await this.execute(template, context);
      if (result.success) {
        const hash = this.hashContent(result.content);
        results.push({ iteration: i + 1, hash, success: true });
        hashes.add(hash);
      } else {
        results.push({ iteration: i + 1, success: false, error: result.error });
      }
    }

    return {
      isDeterministic: hashes.size === 1,
      iterations,
      successfulRuns: results.filter(r => r.success).length,
      uniqueOutputs: hashes.size,
      results: results.slice(0, 2) // Show first 2 for comparison
    };
  }
}

export default KGenTemplateEngine;