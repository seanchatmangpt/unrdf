/**
 * @file KGEN Template Renderer - Execute template logic without nunjucks
 * @module @unrdf/kgn/core/renderer
 *
 * @description
 * Handles:
 * - Variable interpolation: {{ variable }}
 * - Filter application: {{ variable | filter }}
 * - Conditional rendering: {% if %}...{% endif %}
 * - Loop rendering: {% for %}...{% endfor %}
 * - Include processing: {% include %}
 */

/**
 * KGEN Template Renderer class
 */
export class KGenRenderer {
  /**
   * Create a new KGenRenderer instance
   * @param {Object} [options={}] - Renderer configuration options
   * @param {number} [options.maxDepth=10] - Maximum rendering depth
   * @param {boolean} [options.enableIncludes=true] - Enable include processing
   * @param {boolean} [options.strictMode=true] - Enable strict error handling
   * @param {boolean} [options.deterministicMode=true] - Enable deterministic mode
   */
  constructor(options = {}) {
    this.options = {
      maxDepth: options.maxDepth || 10,
      enableIncludes: options.enableIncludes !== false,
      strictMode: options.strictMode !== false,
      deterministicMode: options.deterministicMode !== false,
      ...options
    };

    // Rendering patterns
    this.patterns = {
      variable: /\{\{\s*([^}]+)\s*\}\}/g,
      expression: /\{\%\s*([^%]+)\s*\%\}/g,
      comment: /\{#\s*([^#]+)\s*#\}/g
    };

    this.renderDepth = 0;
  }

  /**
   * Render template with context
   * @param {string} template - Template content to render
   * @param {Object} context - Context data for template variables
   * @param {Object} [options={}] - Rendering options
   * @param {Object} options.filters - Filter instance (required)
   * @returns {Promise<Object>} Render result with content and metadata
   */
  async render(template, context, options = {}) {
    this.renderDepth = 0;

    const { filters } = options;

    if (!filters) {
      throw new Error('Filters instance required for rendering');
    }

    try {
      const result = await this.processTemplate(template, context, filters);

      return {
        content: result,
        metadata: {
          renderTime: this.options.deterministicMode ? '2024-01-01T00:00:00.000Z' : new Date().toISOString(),
          maxDepthReached: this.renderDepth,
          deterministicMode: this.options.deterministicMode
        }
      };
    } catch (error) {
      throw new Error(`Rendering failed: ${error.message}`);
    }
  }

  /**
   * Process template content recursively
   * @param {string} template - Template content
   * @param {Object} context - Context data
   * @param {Object} filters - Filter instance
   * @param {number} [depth=0] - Current recursion depth
   * @returns {Promise<string>} Processed template content
   */
  async processTemplate(template, context, filters, depth = 0) {
    if (depth > this.options.maxDepth) {
      throw new Error(`Maximum rendering depth ${this.options.maxDepth} exceeded`);
    }

    this.renderDepth = Math.max(this.renderDepth, depth);

    let processed = template;

    // Remove comments first
    processed = this.removeComments(processed);

    // Process expressions (conditionals, loops, etc.)
    processed = await this.processExpressions(processed, context, filters, depth);

    // Process variables and filters
    processed = this.processVariables(processed, context, filters);

    return processed;
  }

  /**
   * Remove template comments
   * @param {string} template - Template content
   * @returns {string} Template without comments
   */
  removeComments(template) {
    return template.replace(this.patterns.comment, '');
  }

  /**
   * Process template expressions (if, for, etc.)
   * @param {string} template - Template content
   * @param {Object} context - Context data
   * @param {Object} filters - Filter instance
   * @param {number} depth - Current recursion depth
   * @returns {Promise<string>} Processed template
   */
  async processExpressions(template, context, filters, depth) {
    let processed = template;

    // Process conditionals and loops (nesting-aware, so inner blocks see loop scope)
    processed = await this.processBlocks(processed, context, filters, depth);

    // Process includes
    if (this.options.enableIncludes) {
      processed = await this.processIncludes(processed, context, filters, depth);
    }

    return processed;
  }

  /**
   * Locate the top-level {% if %} / {% for %} blocks of a template.
   * Nesting is tracked with a depth stack so blocks of either kind can contain
   * each other; inner blocks are rendered later, in their own (loop) scope.
   * @param {string} template - Template content
   * @returns {Array<Object>} Top-level blocks in source order
   */
  findBlocks(template) {
    const tagPattern = /\{\%\s*(if|for|else|endif|endfor)\b([^%]*)\%\}/g;
    const blocks = [];
    const stack = [];
    let m;

    while ((m = tagPattern.exec(template)) !== null) {
      const [tag, keyword, rest] = m;
      const tagEnd = m.index + tag.length;

      if (keyword === 'if' || keyword === 'for') {
        stack.push({ kind: keyword, expr: rest.trim(), start: m.index, bodyStart: tagEnd, elseStart: -1, elseBodyStart: -1 });
      } else if (keyword === 'else') {
        const top = stack[stack.length - 1];
        if (top && top.kind === 'if' && top.elseStart === -1) {
          top.elseStart = m.index;
          top.elseBodyStart = tagEnd;
        }
      } else {
        const kind = keyword === 'endif' ? 'if' : 'for';
        const top = stack[stack.length - 1];
        if (top && top.kind === kind) {
          stack.pop();
          if (stack.length === 0) {
            blocks.push({
              ...top,
              end: tagEnd,
              bodyEnd: top.elseStart === -1 ? m.index : top.elseStart,
              elseBodyEnd: top.elseStart === -1 ? -1 : m.index
            });
          }
        }
      }
    }

    return blocks;
  }

  /**
   * Process conditional and loop blocks (outermost first, recursively)
   * @param {string} template - Template content
   * @param {Object} context - Context data
   * @param {Object} filters - Filter instance
   * @param {number} depth - Current recursion depth
   * @returns {Promise<string>} Processed template with blocks expanded
   */
  async processBlocks(template, context, filters, depth) {
    const blocks = this.findBlocks(template);
    let out = '';
    let cursor = 0;

    for (const block of blocks) {
      out += template.slice(cursor, block.start);
      cursor = block.end;

      const errorPrefix = block.kind === 'if' ? 'Conditional evaluation failed' : 'Loop processing failed';
      try {
        out += block.kind === 'if'
          ? await this.renderIfBlock(template, block, context, filters, depth)
          : await this.renderForBlock(template, block, context, filters, depth);
      } catch (error) {
        if (this.options.strictMode) {
          throw new Error(`${errorPrefix}: ${error.message}`);
        }
        // Replace with empty string in non-strict mode
      }
    }

    return out + template.slice(cursor);
  }

  /**
   * Render one {% if %} block
   * @private
   */
  async renderIfBlock(template, block, context, filters, depth) {
    if (this.evaluateCondition(block.expr, context)) {
      return this.processTemplate(template.slice(block.bodyStart, block.bodyEnd), context, filters, depth + 1);
    }
    if (block.elseStart !== -1) {
      return this.processTemplate(template.slice(block.elseBodyStart, block.elseBodyEnd), context, filters, depth + 1);
    }
    return '';
  }

  /**
   * Render one {% for %} block
   * @private
   */
  async renderForBlock(template, block, context, filters, depth) {
    const header = block.expr.match(/^(\w+)\s+in\s+([\s\S]+)$/);
    if (!header) {
      throw new Error(`Invalid for syntax: ${block.expr}`);
    }
    const [, itemVar, arrayExpr] = header;
    const body = template.slice(block.bodyStart, block.bodyEnd);
    const value = this.evaluateForSource(arrayExpr.trim(), context, filters);
    const array = Array.isArray(value) ? value : (value ? [value] : []);

    let replacement = '';
    for (let index = 0; index < array.length; index++) {
      const loopContext = {
        ...context,
        [itemVar]: array[index],
        loop: {
          index: index,
          index0: index,
          index1: index + 1,
          first: index === 0,
          last: index === array.length - 1,
          length: array.length,
          revindex: array.length - index,
          revindex0: array.length - index - 1
        }
      };
      replacement += await this.processTemplate(body, loopContext, filters, depth + 1);
    }
    return replacement;
  }

  /**
   * Evaluate the source of a for loop: a path or a (parenthesised) filter pipeline
   * @private
   */
  evaluateForSource(expr, context, filters) {
    let text = expr;
    while (text.startsWith('(') && text.endsWith(')')) {
      text = text.slice(1, -1).trim();
    }
    return this.evaluateFilterChain(text, context, filters);
  }

  /**
   * Process include expressions
   * @param {string} template - Template content
   * @param {Object} context - Context data
   * @param {Object} filters - Filter instance
   * @param {number} depth - Current recursion depth
   * @returns {Promise<string>} Processed template with includes resolved
   */
  async processIncludes(template, context, filters, depth) {
    const includePattern = /\{\%\s*include\s+['"]([^'"]+)['"]\s*\%\}/g;

    let match;
    let processed = template;

    // Process includes (simplified - no actual file loading for security)
    while ((match = includePattern.exec(processed)) !== null) {
      const [fullMatch, includePath] = match;

      if (this.options.strictMode) {
        throw new Error(`Include processing not implemented: ${includePath}`);
      }

      // In non-strict mode, replace with comment
      const replacement = `<!-- Include: ${includePath} -->`;
      processed = processed.replace(fullMatch, replacement);
    }

    return processed;
  }

  /**
   * Evaluate `path | filter1(args) | filter2 arg` against the context
   * @param {string} trimmed - Expression text
   * @param {Object} context - Context data
   * @param {Object} filters - Filter instance
   * @returns {*} Resulting value
   */
  evaluateFilterChain(trimmed, context, filters) {

    // Check for filters: {{ variable | filter1 | filter2 }}
    const parts = trimmed.split('|').map(p => p.trim());
    let value = this.evaluateExpression(parts[0], context);

    // Apply filters in sequence
    for (let i = 1; i < parts.length; i++) {
      const filterExpr = parts[i].trim();

      // Parse filter with parentheses: filter(arg1, arg2) or filter arg1 arg2
      let filterName, filterArgs = [];

      if (filterExpr.includes('(')) {
        // Handle filter(arg1, arg2) syntax
        const match = filterExpr.match(/^([a-zA-Z_][a-zA-Z0-9_]*)\((.*)\)$/);
        if (match) {
          filterName = match[1];
          const argsStr = match[2].trim();
          if (argsStr) {
            // Split arguments by comma, respecting quotes
            filterArgs = this.parseFilterArguments(argsStr);
          }
        } else {
          throw new Error(`Invalid filter syntax: ${filterExpr}`);
        }
      } else {
        // Handle filter arg1 arg2 syntax
        const parts = filterExpr.split(/\s+/);
        filterName = parts[0];
        filterArgs = parts.slice(1);
      }

      // Parse filter arguments
      const parsedArgs = filterArgs.map(arg => this.parseArgument(arg, context));

      value = filters.apply(filterName, value, ...parsedArgs);
    }

    return value;
  }

  /**
   * Process variable interpolations and filters
   * @param {string} template - Template content
   * @param {Object} context - Context data
   * @param {Object} filters - Filter instance
   * @returns {string} Template with variables interpolated
   */
  processVariables(template, context, filters) {
    return template.replace(this.patterns.variable, (match, expression) => {
      try {
        const value = this.evaluateFilterChain(expression.trim(), context, filters);
        return String(value !== null && value !== undefined ? value : '');
      } catch (error) {
        if (this.options.strictMode) {
          throw new Error(`Variable processing failed: ${error.message}`);
        }
        return match; // Return original expression on error
      }
    });
  }

  /**
   * Evaluate condition expression
   * @param {string} condition - Condition expression to evaluate
   * @param {Object} context - Context data
   * @returns {boolean} Evaluation result
   */
  evaluateCondition(condition, context) {
    // Simple condition evaluation
    // Supports: variable, variable == value, variable != value, !variable

    if (condition.includes('==')) {
      const [left, right] = condition.split('==').map(s => s.trim());
      return this.evaluateExpression(left, context) == this.parseValue(right, context);
    }

    if (condition.includes('!=')) {
      const [left, right] = condition.split('!=').map(s => s.trim());
      return this.evaluateExpression(left, context) != this.parseValue(right, context);
    }

    if (/^not\s+/.test(condition)) {
      return !this.isTruthy(this.evaluateExpression(condition.replace(/^not\s+/, '').trim(), context));
    }

    if (condition.startsWith('!')) {
      const expr = condition.substring(1).trim();
      return !this.isTruthy(this.evaluateExpression(expr, context));
    }

    // Simple truthiness check
    return this.isTruthy(this.evaluateExpression(condition, context));
  }

  /**
   * Evaluate expression to get value from context
   * @param {string} expr - Expression to evaluate
   * @param {Object} context - Context data
   * @returns {*} Evaluated value
   */
  evaluateExpression(expr, context) {
    if (!expr) return '';

    // Handle literals
    if (expr.startsWith('"') && expr.endsWith('"')) {
      return expr.slice(1, -1);
    }
    if (expr.startsWith("'") && expr.endsWith("'")) {
      return expr.slice(1, -1);
    }

    // Handle numbers
    if (/^-?\d+(\.\d+)?$/.test(expr)) {
      return parseFloat(expr);
    }

    // Handle booleans
    if (expr === 'true') return true;
    if (expr === 'false') return false;
    if (expr === 'null') return null;

    // Handle object property access
    const parts = expr.replace(/\[(\d+)\]/g, '.$1').split('.');
    let value = context;

    for (const part of parts) {
      if (value === null || value === undefined) return '';
      value = value[part];
    }

    return value !== undefined ? value : '';
  }

  /**
   * Parse filter arguments from a comma-separated string, respecting quotes
   * @param {string} argsStr - Arguments string
   * @returns {Array<string>} Parsed arguments
   */
  parseFilterArguments(argsStr) {
    const args = [];
    let current = '';
    let inQuotes = false;
    let quoteChar = null;

    for (let i = 0; i < argsStr.length; i++) {
      const char = argsStr[i];

      if ((char === '"' || char === "'") && !inQuotes) {
        inQuotes = true;
        quoteChar = char;
        current += char;
      } else if (char === quoteChar && inQuotes) {
        inQuotes = false;
        quoteChar = null;
        current += char;
      } else if (char === ',' && !inQuotes) {
        args.push(current.trim());
        current = '';
      } else {
        current += char;
      }
    }

    if (current.trim()) {
      args.push(current.trim());
    }

    return args;
  }

  /**
   * Parse argument value (string, number, or variable reference)
   * @param {string} arg - Argument to parse
   * @param {Object} context - Context data
   * @returns {*} Parsed value
   */
  parseArgument(arg, context) {
    // Handle quoted strings
    if ((arg.startsWith('"') && arg.endsWith('"')) || (arg.startsWith("'") && arg.endsWith("'"))) {
      return arg.slice(1, -1);
    }

    // Handle numbers
    if (/^-?\d+(\.\d+)?$/.test(arg)) {
      return parseFloat(arg);
    }

    // Handle booleans
    if (arg === 'true') return true;
    if (arg === 'false') return false;
    if (arg === 'null') return null;

    // Handle variable reference
    return this.evaluateExpression(arg, context);
  }

  /**
   * Parse value with context substitution
   * @param {string} value - Value to parse
   * @param {Object} context - Context data
   * @returns {*} Parsed value
   */
  parseValue(value, context) {
    return this.parseArgument(value, context);
  }

  /**
   * Check if value is truthy in template context
   * @param {*} value - Value to check
   * @returns {boolean} True if value is truthy
   */
  isTruthy(value) {
    if (value === null || value === undefined) return false;
    if (value === '') return false;
    if (value === 0) return false;
    if (value === false) return false;
    if (Array.isArray(value) && value.length === 0) return false;
    if (typeof value === 'object' && Object.keys(value).length === 0) return false;

    return true;
  }

  /**
   * Get renderer statistics
   * @returns {Object} Renderer configuration and statistics
   */
  getStats() {
    return {
      ...this.options,
      maxDepthReached: this.renderDepth,
      supportedExpressions: ['if/else/endif', 'for/endfor', 'include', 'variables', 'filters']
    };
  }
}

export default KGenRenderer;