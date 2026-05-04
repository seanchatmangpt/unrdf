/**
 * @file KGEN Template Parser - Parse template syntax without nunjucks
 * @module @unrdf/kgn/core/parser
 *
 * @description
 * Supports:
 * - Variables: {{ variable }}
 * - Filters: {{ variable | filter }}
 * - Conditions: {% if condition %}...{% endif %}
 * - Loops: {% for item in items %}...{% endfor %}
 * - Comments: {# comment #}
 * - Frontmatter: YAML header
 */

// Use existing frontmatter parser if available, fallback to basic parsing

/**
 * KGEN Template Parser class
 */
export class KGenParser {
  /**
   * Create a new KGenParser instance
   * @param {Object} [options={}] - Parser configuration options
   * @param {number} [options.maxDepth=10] - Maximum nesting depth for templates
   * @param {boolean} [options.enableIncludes=true] - Enable include statement processing
   * @param {boolean} [options.strictMode=true] - Enable strict syntax validation
   */
  constructor(options = {}) {
    this.options = {
      maxDepth: options.maxDepth || 10,
      enableIncludes: options.enableIncludes !== false,
      strictMode: options.strictMode !== false,
      ...options
    };

    // Template syntax patterns
    this.patterns = {
      variable: /\{\{\s*([^}]+)\s*\}\}/g,
      expression: /\{\%\s*([^%]+)\s*\%\}/g,
      comment: /\{#\s*([^#]+)\s*#\}/g,
      frontmatter: /^---\n([\s\S]*?)\n---\n([\s\S]*)$/
    };
  }

  /**
   * Parse template content into structured format
   * @param {string} template - Template content to parse
   * @returns {Promise<Object>} Parse result with template structure
   * @returns {string} return.template - Parsed template content
   * @returns {Object} return.frontmatter - Extracted frontmatter data
   * @returns {Array<string>} return.variables - Extracted variable names
   * @returns {Array<Object>} return.expressions - Extracted expressions
   * @returns {Array<Object>} return.includes - Include statements
   * @returns {Array<Object>} return.comments - Template comments
   * @returns {Object} return.structure - Structural analysis
   * @throws {Error} When parse error occurs
   */
  async parse(template) {
    try {
      // Extract frontmatter if present
      const { frontmatter, content } = this.parseFrontmatter(template);

      // Parse template structure
      const parseResult = {
        template: content,
        frontmatter: frontmatter || {},
        variables: this.extractVariables(content),
        expressions: this.extractExpressions(content),
        includes: this.extractIncludes(content),
        comments: this.extractComments(content),
        structure: this.analyzeStructure(content)
      };

      // Validate syntax
      if (this.options.strictMode) {
        this.validateSyntax(parseResult);
      }

      return parseResult;
    } catch (error) {
      throw new Error(`Parse error: ${error.message}`);
    }
  }

  /**
   * Parse frontmatter from template
   * @param {string} template - Template content with optional frontmatter
   * @returns {Object} Frontmatter parse result
   * @returns {Object} return.frontmatter - Parsed frontmatter data
   * @returns {string} return.content - Template content without frontmatter
   */
  parseFrontmatter(template) {
    const match = template.match(this.patterns.frontmatter);

    if (match) {
      try {
        // Try to parse YAML frontmatter
        const yamlContent = match[1];
        const templateContent = match[2];

        // Basic YAML parsing (simplified)
        const frontmatter = this.parseBasicYAML(yamlContent);

        return {
          frontmatter,
          content: templateContent
        };
      } catch (error) {
        // Fallback to basic parsing
        return {
          frontmatter: {},
          content: template
        };
      }
    }

    return {
      frontmatter: {},
      content: template
    };
  }

  /**
   * Basic YAML parser for frontmatter (simplified)
   * @param {string} yamlContent - YAML content to parse
   * @returns {Object} Parsed YAML data as JavaScript object
   */
  parseBasicYAML(yamlContent) {
    const result = {};
    const lines = yamlContent.split('\n');

    for (const line of lines) {
      const trimmed = line.trim();
      if (!trimmed || trimmed.startsWith('#')) continue;

      const colonIndex = trimmed.indexOf(':');
      if (colonIndex === -1) continue;

      const key = trimmed.substring(0, colonIndex).trim();
      let value = trimmed.substring(colonIndex + 1).trim();

      // Parse basic value types
      if (value.startsWith('"') && value.endsWith('"')) {
        value = value.slice(1, -1);
      } else if (value.startsWith("'") && value.endsWith("'")) {
        value = value.slice(1, -1);
      } else if (value === 'true') {
        value = true;
      } else if (value === 'false') {
        value = false;
      } else if (value === 'null') {
        value = null;
      } else if (/^-?\d+(\.\d+)?$/.test(value)) {
        value = parseFloat(value);
      }

      result[key] = value;
    }

    return result;
  }

  /**
   * Extract variables from template content
   * @param {string} content - Template content to analyze
   * @returns {Array<string>} Array of required variable names
   */
  extractVariables(content) {
    const variables = new Set();
    const optionalVariables = new Set();
    let match;

    // Reset regex state
    this.patterns.variable.lastIndex = 0;

    while ((match = this.patterns.variable.exec(content)) !== null) {
      const variableExpr = match[1].trim();

      // Handle filtered variables: {{ variable | filter }}
      const parts = variableExpr.split('|');
      const [variableName] = parts[0].trim().split('.');

      // Skip loop variables and built-ins
      if (!this.isBuiltinVariable(variableName)) {
        variables.add(variableName);

        // Check if variable has default filter (makes it optional)
        if (parts.length > 1) {
          const filters = parts.slice(1);
          for (const filterExpr of filters) {
            const filterName = filterExpr.trim().split(/[\s(]/)[0];
            if (filterName === 'default') {
              optionalVariables.add(variableName);
              break;
            }
          }
        }
      }
    }

    // Return only required variables (excluding optional ones)
    const required = Array.from(variables).filter(v => !optionalVariables.has(v));
    return required;
  }

  /**
   * Extract expressions (conditionals, loops, etc.)
   * @param {string} content - Template content to analyze
   * @returns {Array<Object>} Array of expression objects with type, expression, position, raw
   */
  extractExpressions(content) {
    const expressions = [];
    let match;

    // Reset regex state
    this.patterns.expression.lastIndex = 0;

    while ((match = this.patterns.expression.exec(content)) !== null) {
      const expr = match[1].trim();
      const type = this.getExpressionType(expr);

      expressions.push({
        type,
        expression: expr,
        position: match.index,
        raw: match[0]
      });
    }

    return expressions;
  }

  /**
   * Extract include statements
   * @param {string} content - Template content to analyze
   * @returns {Array<Object>} Array of include objects with path, position, raw
   */
  extractIncludes(content) {
    const includes = [];
    const expressions = this.extractExpressions(content);

    expressions.forEach(expr => {
      if (expr.type === 'include') {
        const pathMatch = expr.expression.match(/include\s+['"]([^'"]+)['"]/);
        if (pathMatch) {
          includes.push({
            path: pathMatch[1],
            position: expr.position,
            raw: expr.raw
          });
        }
      }
    });

    return includes;
  }

  /**
   * Extract comments
   * @param {string} content - Template content to analyze
   * @returns {Array<Object>} Array of comment objects with text, position, raw
   */
  extractComments(content) {
    const comments = [];
    let match;

    // Reset regex state
    this.patterns.comment.lastIndex = 0;

    while ((match = this.patterns.comment.exec(content)) !== null) {
      comments.push({
        text: match[1].trim(),
        position: match.index,
        raw: match[0]
      });
    }

    return comments;
  }

  /**
   * Analyze template structure
   * @param {string} content - Template content to analyze
   * @returns {Object} Structure analysis with conditionals, loops, includes, blocks, macros, depth counts
   */
  analyzeStructure(content) {
    const expressions = this.extractExpressions(content);

    const structure = {
      conditionals: 0,
      loops: 0,
      includes: 0,
      blocks: 0,
      macros: 0,
      depth: 0
    };

    const stack = [];

    expressions.forEach(expr => {
      switch (expr.type) {
        case 'if':
          structure.conditionals++;
          stack.push('if');
          break;
        case 'elif':
        case 'else':
          // Don't increment depth for elif/else
          break;
        case 'endif':
          if (stack.length > 0 && stack[stack.length - 1] === 'if') {
            stack.pop();
          }
          break;
        case 'for':
          structure.loops++;
          stack.push('for');
          break;
        case 'endfor':
          if (stack.length > 0 && stack[stack.length - 1] === 'for') {
            stack.pop();
          }
          break;
        case 'include':
          structure.includes++;
          break;
        case 'block':
          structure.blocks++;
          stack.push('block');
          break;
        case 'endblock':
          if (stack.length > 0 && stack[stack.length - 1] === 'block') {
            stack.pop();
          }
          break;
        case 'macro':
          structure.macros++;
          break;
      }

      // Track maximum nesting depth
      structure.depth = Math.max(structure.depth, stack.length);
    });

    return structure;
  }

  /**
   * Get expression type from expression string
   * @param {string} expr - Expression string to classify
   * @returns {string} Expression type (if, else, elif, endif, for, endfor, include, block, endblock, macro, endmacro, set, unknown)
   */
  getExpressionType(expr) {
    const trimmed = expr.trim().toLowerCase();

    if (trimmed.startsWith('if ')) return 'if';
    if (trimmed === 'else') return 'else';
    if (trimmed.startsWith('elif ')) return 'elif';
    if (trimmed === 'endif') return 'endif';
    if (trimmed.startsWith('for ')) return 'for';
    if (trimmed === 'endfor') return 'endfor';
    if (trimmed.startsWith('include ')) return 'include';
    if (trimmed.startsWith('block ')) return 'block';
    if (trimmed === 'endblock') return 'endblock';
    if (trimmed.startsWith('macro ')) return 'macro';
    if (trimmed === 'endmacro') return 'endmacro';
    if (trimmed.startsWith('set ')) return 'set';

    return 'unknown';
  }

  /**
   * Check if variable is a built-in
   * @param {string} name - Variable name to check
   * @returns {boolean} True if variable is a built-in
   */
  isBuiltinVariable(name) {
    const builtins = new Set([
      'loop', 'super', '__kgen', 'true', 'false', 'null', 'undefined'
    ]);

    return builtins.has(name) || name.startsWith('__');
  }

  /**
   * Validate template syntax
   * @param {Object} parseResult - Parse result to validate
   * @param {Array<Object>} parseResult.expressions - Expressions to validate
   * @param {Object} parseResult.structure - Structure to validate
   * @throws {Error} When syntax validation fails
   */
  validateSyntax(parseResult) {
    const { expressions } = parseResult;
    const errors = [];
    const stack = [];

    // Check for balanced expressions
    expressions.forEach((expr, index) => {
      switch (expr.type) {
        case 'if':
          stack.push({ type: 'if', index });
          break;
        case 'endif':
          if (stack.length === 0 || stack[stack.length - 1].type !== 'if') {
            errors.push(`Unmatched {% endif %} at position ${expr.position}`);
          } else {
            stack.pop();
          }
          break;
        case 'for':
          stack.push({ type: 'for', index });
          break;
        case 'endfor':
          if (stack.length === 0 || stack[stack.length - 1].type !== 'for') {
            errors.push(`Unmatched {% endfor %} at position ${expr.position}`);
          } else {
            stack.pop();
          }
          break;
        case 'block':
          stack.push({ type: 'block', index });
          break;
        case 'endblock':
          if (stack.length === 0 || stack[stack.length - 1].type !== 'block') {
            errors.push(`Unmatched {% endblock %} at position ${expr.position}`);
          } else {
            stack.pop();
          }
          break;
      }
    });

    // Check for unclosed expressions
    stack.forEach(unclosed => {
      const expr = expressions[unclosed.index];
      errors.push(`Unclosed {% ${expr.type} %} at position ${expr.position}`);
    });

    // Check depth limits
    if (parseResult.structure.depth > this.options.maxDepth) {
      errors.push(`Template nesting depth ${parseResult.structure.depth} exceeds maximum ${this.options.maxDepth}`);
    }

    if (errors.length > 0) {
      throw new Error(`Syntax validation failed:\n${errors.join('\n')}`);
    }
  }

  /**
   * Check if template is deterministic
   * @param {Object} parseResult - Parse result to check
   * @param {Array<string>} parseResult.variables - Variables to check
   * @param {Array<Object>} parseResult.expressions - Expressions to check
   * @returns {Object} Result with deterministic boolean and reasons array
   */
  isDeterministic(parseResult) {
    const { variables, expressions } = parseResult;

    // Check for non-deterministic variables
    const nonDeterministicVars = variables.filter(name =>
      ['now', 'random', 'uuid'].includes(name)
    );

    if (nonDeterministicVars.length > 0) {
      return {
        deterministic: false,
        reasons: [`Non-deterministic variables: ${nonDeterministicVars.join(', ')}`]
      };
    }

    // Check for non-deterministic expressions
    const nonDeterministicExpressions = expressions.filter(expr =>
      /\b(now|random|uuid)\b/.test(expr.expression)
    );

    if (nonDeterministicExpressions.length > 0) {
      return {
        deterministic: false,
        reasons: [`Non-deterministic expressions found`]
      };
    }

    return { deterministic: true, reasons: [] };
  }

  /**
   * Get parser statistics
   * @returns {Object} Parser configuration and supported patterns
   */
  getStats() {
    return {
      ...this.options,
      supportedPatterns: Object.keys(this.patterns)
    };
  }
}

export default KGenParser;