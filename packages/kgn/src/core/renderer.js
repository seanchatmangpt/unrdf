/**
 * KGEN Template Renderer - Execute template logic without nunjucks
 *
 * Handles:
 * - Variable interpolation: {{ variable }}
 * - Filter application: {{ variable | filter }}
 * - Conditional rendering: {% if %}...{% endif %}
 * - Loop rendering: {% for %}...{% endfor %}
 * - Include processing: {% include %}
 */

export class KGenRenderer {
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
   */
  removeComments(template) {
    return template.replace(this.patterns.comment, '');
  }

  /**
   * Process template expressions (if, for, etc.)
   */
  async processExpressions(template, context, filters, depth) {
    let processed = template;

    // Process conditionals
    processed = await this.processConditionals(processed, context, filters, depth);

    // Process loops
    processed = await this.processLoops(processed, context, filters, depth);

    // Process includes
    if (this.options.enableIncludes) {
      processed = await this.processIncludes(processed, context, filters, depth);
    }

    return processed;
  }

  /**
   * Process conditional expressions
   */
  async processConditionals(template, context, filters, depth) {
    const ifPattern = /\{\%\s*if\s+([^%]+)\s*\%\}([\s\S]*?)(\{\%\s*else\s*\%\}([\s\S]*?))?\{\%\s*endif\s*\%\}/g;

    let match;
    let processed = template;

    // Process from end to start to avoid position shifts
    const matches = [];
    while ((match = ifPattern.exec(template)) !== null) {
      matches.push(match);
    }

    for (let i = matches.length - 1; i >= 0; i--) {
      match = matches[i];
      const [fullMatch, condition, ifContent, elseBlock, elseContent] = match;

      try {
        const conditionResult = this.evaluateCondition(condition.trim(), context);
        let replacement;

        if (conditionResult) {
          replacement = await this.processTemplate(ifContent, context, filters, depth + 1);
        } else if (elseContent !== undefined) {
          replacement = await this.processTemplate(elseContent, context, filters, depth + 1);
        } else {
          replacement = '';
        }

        processed = processed.substring(0, match.index) + replacement + processed.substring(match.index + fullMatch.length);
      } catch (error) {
        if (this.options.strictMode) {
          throw new Error(`Conditional evaluation failed: ${error.message}`);
        }
        // Replace with empty string in non-strict mode
        const replacement = '';
        processed = processed.substring(0, match.index) + replacement + processed.substring(match.index + fullMatch.length);
      }
    }

    return processed;
  }

  /**
   * Process loop expressions
   */
  async processLoops(template, context, filters, depth) {
    const forPattern = /\{\%\s*for\s+(\w+)\s+in\s+([^%]+)\s*\%\}([\s\S]*?)\{\%\s*endfor\s*\%\}/g;

    let match;
    let processed = template;

    // Process from end to start to avoid position shifts
    const matches = [];
    while ((match = forPattern.exec(template)) !== null) {
      matches.push(match);
    }

    for (let i = matches.length - 1; i >= 0; i--) {
      match = matches[i];
      const [fullMatch, itemVar, arrayExpr, loopContent] = match;

      try {
        const array = this.evaluateExpression(arrayExpr.trim(), context);
        let replacement = '';

        if (Array.isArray(array)) {
          for (let index = 0; index < array.length; index++) {
            const item = array[index];

            // Create loop context
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
                revindex: array.length - index,
                revindex0: array.length - index - 1
              }
            };

            const loopResult = await this.processTemplate(loopContent, loopContext, filters, depth + 1);
            replacement += loopResult;
          }
        } else if (array) {
          // Handle single item as array of one
          const loopContext = {
            ...context,
            [itemVar]: array,
            loop: {
              index: 0,
              index0: 0,
              index1: 1,
              first: true,
              last: true,
              length: 1,
              revindex: 1,
              revindex0: 0
            }
          };

          replacement = await this.processTemplate(loopContent, loopContext, filters, depth + 1);
        }

        processed = processed.substring(0, match.index) + replacement + processed.substring(match.index + fullMatch.length);
      } catch (error) {
        if (this.options.strictMode) {
          throw new Error(`Loop processing failed: ${error.message}`);
        }
        // Replace with empty string in non-strict mode
        const replacement = '';
        processed = processed.substring(0, match.index) + replacement + processed.substring(match.index + fullMatch.length);
      }
    }

    return processed;
  }

  /**
   * Process include expressions
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
   * Process variable interpolations and filters
   */
  processVariables(template, context, filters) {
    return template.replace(this.patterns.variable, (match, expression) => {
      try {
        const trimmed = expression.trim();

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

    if (condition.startsWith('!')) {
      const expr = condition.substring(1).trim();
      return !this.isTruthy(this.evaluateExpression(expr, context));
    }

    // Simple truthiness check
    return this.isTruthy(this.evaluateExpression(condition, context));
  }

  /**
   * Evaluate expression to get value from context
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
    const parts = expr.split('.');
    let value = context;

    for (const part of parts) {
      if (value === null || value === undefined) return '';
      value = value[part];
    }

    return value !== undefined ? value : '';
  }

  /**
   * Parse filter arguments from a comma-separated string, respecting quotes
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
   */
  parseValue(value, context) {
    return this.parseArgument(value, context);
  }

  /**
   * Check if value is truthy in template context
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