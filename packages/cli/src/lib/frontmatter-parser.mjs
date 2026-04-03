/**
 * @file Template frontmatter YAML parser with gray-matter
 * @module @unrdf/cli/lib/frontmatter-parser
 *
 * Parses YAML frontmatter from .njk template files using gray-matter.
 * Supports directives: to:, inject:, before:, after:, append:, prepend:, lineAt:,
 * skipIf:, skip_if:, rdf:, turtle:, turtleData:, sparql:, unless_exists:, eof_last:, chmod:, force:, sh:
 */

import matter from 'gray-matter';

/**
 * Parse YAML frontmatter + body from template file content.
 * Expects format:
 *   ---
 *   to: path/{{ name }}.html
 *   inject: true
 *   ---
 *   <template body>
 *
 * @param {string} fileContent
 * @returns {{frontmatter: Object, body: string}}
 */
export function parseFrontmatter(fileContent) {
  const { data: frontmatter, content: body } = matter(fileContent);
  return { frontmatter, body };
}

/**
 * Determine the file operation mode from frontmatter directives.
 * Returns { mode: 'write'|'append'|'prepend'|'lineAt'|'inject'|'before'|'after', ...details }
 * Ported from kgen-core FrontmatterParser
 * @param {Object} frontmatter
 * @returns {Object}
 */
export function getOperationMode(frontmatter) {
  if (!frontmatter) return { mode: 'write' };

  // inject: true with anchor
  if (frontmatter.inject) {
    if (frontmatter.before) {
      return { mode: 'before', anchor: frontmatter.before };
    }
    if (frontmatter.after) {
      return { mode: 'after', anchor: frontmatter.after };
    }
    // inject: true without anchor = append
    return { mode: 'inject' };
  }

  // lineAt: N
  if (frontmatter.lineAt) {
    return { mode: 'lineAt', line: frontmatter.lineAt };
  }

  // append: true
  if (frontmatter.append) {
    return { mode: 'append' };
  }

  // prepend: true
  if (frontmatter.prepend) {
    return { mode: 'prepend' };
  }

  return { mode: 'write' };
}

/**
 * Evaluate a skip condition against template variables.
 * Supports: 'var', '!var', 'var==value', 'var!=value'
 * Also supports 'skip_if' as snake_case alias for 'skipIf'
 * Ported from kgen-core FrontmatterParser
 * @param {Object} frontmatter
 * @param {Object} variables - Template context variables
 * @returns {boolean} true if should skip
 */
export function shouldSkip(frontmatter, variables) {
  if (!frontmatter) return false;

  // Support both skipIf (camelCase) and skip_if (snake_case Hygen)
  const skipExpr = frontmatter.skipIf || frontmatter.skip_if;
  if (!skipExpr) return false;

  const expr = skipExpr.trim();

  // Check for 'var==value' or 'var!=value'
  const eqMatch = expr.match(/^(\w+)(==|!=)(.+)$/);
  if (eqMatch) {
    const [, varName, op, expectedValue] = eqMatch;
    const varValue = variables[varName];
    const matches = String(varValue) === String(expectedValue).trim();
    return op === '==' ? matches : !matches;
  }

  // Check for '!var' (negation)
  if (expr.startsWith('!')) {
    const varName = expr.slice(1).trim();
    return !Boolean(variables[varName]);
  }

  // Check for bare 'var' (truthy)
  const varName = expr;
  return !Boolean(variables[varName]);
}

/**
 * FrontmatterParser class for advanced frontmatter operations.
 */
export class FrontmatterParser {
  /**
   * @param {Object} [options]
   * @param {boolean} [options.strict=false] - Throw on parse errors
   * @param {boolean} [options.allowEmpty=true] - Allow empty frontmatter
   */
  constructor(options = {}) {
    this.strict = options.strict || false;
    this.allowEmpty = options.allowEmpty !== false;
  }

  /**
   * Parse file content into frontmatter + body
   * @param {string} fileContent
   * @returns {{frontmatter: Object, body: string}}
   */
  parse(fileContent) {
    return parseFrontmatter(fileContent);
  }

  /**
   * Validate frontmatter against required/optional fields
   * @param {Object} frontmatter
   * @returns {{valid: boolean, errors: string[], warnings: string[]}}
   */
  validate(frontmatter) {
    const errors = [];
    const warnings = [];

    if (!frontmatter) {
      if (!this.allowEmpty) {
        errors.push('Frontmatter is empty');
      }
      return { valid: errors.length === 0, errors, warnings };
    }

    // Validate operation mode
    const mode = getOperationMode(frontmatter);
    if (mode.mode === 'before' || mode.mode === 'after') {
      if (!mode.anchor) {
        errors.push(`${mode.mode} directive requires anchor pattern`);
      }
    }

    if (mode.mode === 'lineAt') {
      if (typeof mode.line !== 'number' || mode.line < 1) {
        errors.push('lineAt must be a positive number');
      }
    }

    // Validate mutually exclusive directives
    const hasInject = frontmatter.inject;
    const hasAppend = frontmatter.append;
    const hasPrepend = frontmatter.prepend;
    const hasLineAt = frontmatter.lineAt;
    const modeCount = [hasInject, hasAppend, hasPrepend, hasLineAt].filter(Boolean).length;
    if (modeCount > 1) {
      errors.push('Cannot specify multiple operation modes (inject, append, prepend, lineAt)');
    }

    // Warn about missing recommended fields
    const recommended = ['name', 'description', 'type'];
    recommended.forEach(field => {
      if (!(field in frontmatter)) {
        warnings.push(`Recommended field missing: ${field}`);
      }
    });

    return { valid: errors.length === 0, errors, warnings };
  }

  /**
   * Merge frontmatter with defaults
   * @param {Object} frontmatter
   * @param {Object} [defaults]
   * @returns {Object}
   */
  mergeWithDefaults(frontmatter, defaults = {}) {
    return {
      name: frontmatter.name || defaults.name || 'Untitled Template',
      description: frontmatter.description || defaults.description || '',
      type: frontmatter.type || defaults.type || 'file',
      to: frontmatter.to,
      inject: frontmatter.inject,
      skipIf: frontmatter.skipIf || frontmatter.skip_if,
      rdf: frontmatter.rdf,
      sparql: frontmatter.sparql,
      ...Object.fromEntries(
        Object.entries(frontmatter).filter(
          ([key]) =>
            ![
              'name',
              'description',
              'type',
              'to',
              'inject',
              'skipIf',
              'skip_if',
              'rdf',
              'sparql',
              'before',
              'after',
              'append',
              'prepend',
              'lineAt',
              'turtle',
              'turtleData',
              'unless_exists',
              'eof_last',
              'chmod',
              'force',
              'sh',
            ].includes(key)
        )
      ),
      ...frontmatter,
    };
  }

  /**
   * Stringify frontmatter back to YAML
   * @param {Object} frontmatter
   * @param {string} [body]
   * @returns {string}
   */
  stringify(frontmatter, body = '') {
    try {
      return matter.stringify(body, frontmatter);
    } catch (error) {
      throw new Error(`Failed to stringify frontmatter: ${error.message}`);
    }
  }

  /**
   * Get parser statistics
   * @returns {Object}
   */
  getStats() {
    return {
      strict: this.strict,
      allowEmpty: this.allowEmpty,
    };
  }
}

export default FrontmatterParser;
