/**
 * @file Core Tera-Compatible Template Engine
 * @module @unrdf/tera-engine/engine
 * @description Pure JavaScript template engine with Tera-compatible syntax
 */

import { validateContext, validateOptions, validateEngineConfig } from './schemas.mjs';

/**
 * Escapes HTML special characters
 * @param {string} str - String to escape
 * @returns {string} Escaped string
 */
function escapeHtml(str) {
  if (typeof str !== 'string') return String(str);
  const map = {
    '&': '&amp;',
    '<': '&lt;',
    '>': '&gt;',
    '"': '&quot;',
    "'": '&#39;',
  };
  return str.replace(/[&<>"']/g, m => map[m]);
}

/**
 * Resolves variable path in context (e.g., "user.name.first")
 * @param {Record<string, any>} context - Template context
 * @param {string} path - Variable path
 * @param {boolean} strict - Throw on undefined
 * @returns {any} Resolved value
 */
function resolveVariable(context, path, strict = false) {
  const parts = path.trim().split('.');
  let value = context;

  for (const part of parts) {
    if (value === null || value === undefined) {
      if (strict) {
        throw new Error(`Undefined variable: ${path}`);
      }
      return '';
    }
    value = value[part];
  }

  return value ?? (strict ? (() => { throw new Error(`Undefined variable: ${path}`); })() : '');
}

/**
 * Applies filter to value
 * @param {any} value - Input value
 * @param {string} filterName - Filter name
 * @param {any} arg - Filter argument
 * @param {Record<string, Function>} filters - Filter registry
 * @returns {any} Filtered value
 */
function applyFilter(value, filterName, arg, filters) {
  const filter = filters[filterName];
  if (!filter) {
    throw new Error(`Unknown filter: ${filterName}`);
  }
  return filter(value, arg);
}


/**
 * Evaluates expression with filters
 * @param {string} expr - Expression string
 * @param {Record<string, any>} context - Template context
 * @param {Record<string, Function>} filters - Filter registry
 * @param {boolean} strict - Strict variable mode
 * @returns {any} Evaluated result
 */
function evaluateExpression(expr, context, filters, strict) {
  const parts = expr.split('|').map(s => s.trim());
  let value = resolveVariable(context, parts[0], strict);

  // Apply filters
  for (let i = 1; i < parts.length; i++) {
    const match = parts[i].match(/^(\w+)(?:\((.+?)\))?$/);
    if (!match) {
      throw new Error(`Invalid filter syntax: ${parts[i]}`);
    }
    const [, name, arg] = match;
    value = applyFilter(value, name, arg ? JSON.parse(arg) : undefined, filters);
  }

  return value;
}

/**
 * Tera-compatible template engine
 */
export class TeraEngine {
  /**
   * Creates a new TeraEngine instance
   * @param {Object} [config] - Engine configuration
   * @param {Record<string, Function>} [config.filters] - Custom filters
   * @param {Object} [config.options] - Default template options
   */
  constructor(config = {}) {
    const validConfig = validateEngineConfig(config);
    // Create a new object to avoid sharing filter references
    this.filters = { ...(validConfig.filters || {}) };
    this.defaultOptions = validConfig.options || {};
    this.templateCache = new Map();
    this.config = validConfig;
  }

  /**
   * Registers a custom filter
   * @param {string} name - Filter name
   * @param {Function} fn - Filter function
   */
  registerFilter(name, fn) {
    if (typeof fn !== 'function') {
      throw new Error(`Filter must be a function: ${name}`);
    }
    this.filters[name] = fn;
  }

  /**
   * Renders a template string
   * @param {string} template - Template string
   * @param {Record<string, any>} context - Template context
   * @param {Object} [options] - Template options
   * @returns {string} Rendered output
   */
  render(template, context, options = {}) {
    const validContext = validateContext(context);
    const validOptions = validateOptions({ ...this.defaultOptions, ...options });

    return this._compile(template, validContext, validOptions);
  }

  /**
   * Compiles and renders template
   * @private
   * @param {string} template - Template string
   * @param {Record<string, any>} context - Template context
   * @param {Object} options - Template options
   * @returns {string} Rendered output
   */
  _compile(template, context, options) {
    let output = template;

    // Process for loops: {% for item in items %}...{% endfor %}
    output = this._processForLoops(output, context, options);

    // Process if statements: {% if condition %}...{% endif %}
    output = this._processIfStatements(output, context, options);

    // Process variable interpolation: {{ variable }}
    output = this._processVariables(output, context, options);

    // Process comments: {# comment #}
    output = output.replace(/\{#.*?#\}/gs, '');

    // Trim blocks if enabled (remove trailing newlines after blocks)
    if (options.trimBlocks) {
      output = output.replace(/\{%[^%]*%\}\n/g, match => match.slice(0, -1));
    }

    // Lstrip blocks if enabled (remove leading whitespace before blocks)
    if (options.lstripBlocks) {
      output = output.replace(/^\s+(?=\{%)/gm, '');
    }

    return output;
  }

  /**
   * Process for loops
   * @private
   */
  _processForLoops(template, context, options) {
    let result = template;
    let lastResult = '';

    // Process loops recursively until no more loops found
    while (result !== lastResult) {
      lastResult = result;
      const forRegex = /\{%\s*for\s+(\w+)\s+in\s+([\w.]+)\s*%\}(.*?)\{%\s*endfor\s*%\}/s;
      const match = result.match(forRegex);

      if (!match) break;

      const [fullMatch, itemVar, arrayPath, body] = match;
      const array = resolveVariable(context, arrayPath, options.strictVariables);

      if (!Array.isArray(array)) {
        if (options.strictVariables) {
          throw new Error(`Expected array for loop: ${arrayPath}`);
        }
        result = result.replace(fullMatch, '');
        continue;
      }

      const replacement = array.map((item, index) => {
        const loopContext = {
          ...context,
          [itemVar]: item,
          loop: {
            index: index,
            index0: index,
            index1: index + 1,
            first: index === 0,
            last: index === array.length - 1,
            length: array.length,
          },
        };
        return this._compile(body, loopContext, options);
      }).join('');

      result = result.replace(fullMatch, replacement);
    }

    return result;
  }

  /**
   * Process if statements
   * @private
   */
  _processIfStatements(template, context, options) {
    let result = template;
    let lastResult = '';

    // Process if statements recursively until no more if statements found
    while (result !== lastResult) {
      lastResult = result;
      const ifRegex = /\{%\s*if\s+([\w.]+)\s*%\}(.*?)(?:\{%\s*else\s*%\}(.*?))?\{%\s*endif\s*%\}/s;
      const match = result.match(ifRegex);

      if (!match) break;

      const [fullMatch, condition, thenBranch, elseBranch = ''] = match;
      const value = resolveVariable(context, condition, false);
      const isTruthy = Boolean(value);

      const branch = isTruthy ? thenBranch : elseBranch;
      const replacement = this._compile(branch, context, options);

      result = result.replace(fullMatch, replacement);
    }

    return result;
  }

  /**
   * Process variable interpolation
   * @private
   */
  _processVariables(template, context, options) {
    const varRegex = /\{\{\s*([\w.|\s()\d,'"]+?)\s*\}\}/g;

    return template.replace(varRegex, (match, expr) => {
      let value;
      try {
        value = evaluateExpression(expr, context, this.filters, options.strictVariables);
      } catch (error) {
        if (options.strictVariables) {
          throw error;
        }
        // Return empty string for non-strict mode
        return '';
      }

      // Auto-escape HTML if enabled
      if (options.autoescape && typeof value === 'string') {
        value = escapeHtml(value);
      }

      return String(value ?? '');
    });
  }
}

/**
 * Creates a new Tera engine instance
 * @param {Object} [config] - Engine configuration
 * @returns {TeraEngine} Template engine instance
 * @example
 * const engine = createTeraEngine({ filters: { upper: s => s.toUpperCase() } });
 * const output = engine.render('Hello {{ name | upper }}!', { name: 'world' });
 */
export function createTeraEngine(config = {}) {
  return new TeraEngine(config);
}

/**
 * Quick render helper
 * @param {string} template - Template string
 * @param {Record<string, any>} context - Template context
 * @param {Object} [options] - Template options
 * @returns {string} Rendered output
 * @example
 * const output = renderTemplate('Hello {{ name }}!', { name: 'World' });
 */
export function renderTemplate(template, context, options = {}) {
  const engine = new TeraEngine({ options });
  return engine.render(template, context);
}
