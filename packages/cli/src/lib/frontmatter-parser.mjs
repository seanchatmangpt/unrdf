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
 * Frontmatter field schema — defines allowed keys, types, and constraints.
 */
export const FRONTMATTER_SCHEMA = {
  /** Fields that must be present */
  required: ['to'],

  /** All recognized frontmatter keys grouped by category */
  allowed: [
    'to',
    'mode',
    'inject',
    'append',
    'prepend',
    'lineAt',
    'before',
    'after',
    'rdf',
    'turtle',
    'turtleData',
    'sparql',
    'skipIf',
    'skip_if',
    'unless_exists',
    'eof_last',
    'chmod',
    'force',
    'sh',
    'sh_ignore_exit',
    'from',
    'requires',
    'description',
    'name',
    'type',
    'variables',
    'filters',
    // OStar template extensions
    'category',
    'author',
  ],

  /** Type expectations per key */
  types: {
    to: 'string',
    mode: { type: 'string', enum: ['overwrite', 'append', 'skip_existing', 'prepend'] },
    inject: 'boolean',
    append: 'boolean',
    prepend: 'boolean',
    lineAt: 'number',
    before: 'string',
    after: 'string',
    rdf: 'string',
    sparql: 'string',
    skipIf: 'string',
    skip_if: 'string',
    unless_exists: 'boolean',
    eof_last: 'boolean',
    chmod: 'string',
    force: 'boolean',
    sh: 'string',
    sh_ignore_exit: 'boolean',
    from: 'string',
    requires: { type: 'oneOf', values: ['string', 'array'] },
    description: 'string',
    name: 'string',
    type: 'string',
    variables: 'object',
    filters: 'object',
  },

  /** Groups of keys that are mutually exclusive */
  exclusive: [
    ['inject', 'append', 'prepend', 'lineAt'],
    ['skipIf', 'skip_if'],
  ],

  /** Dependency rules: if key is present, companion must also be present */
  requires: [
    { key: 'before', companion: 'inject', message: '"before" requires "inject: true"' },
    { key: 'after', companion: 'inject', message: '"after" requires "inject: true"' },
  ],
};

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
 * Supports: 'var', '!var', 'var==value', 'var!=value', '/regex/' (Hygen parity)
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

  // Check for regex pattern: /pattern/flags
  const regexMatch = expr.match(/^\/(.+)\/([gimsuy]*)$/);
  if (regexMatch) {
    const pattern = regexMatch[1];
    const flags = regexMatch[2];
    try {
      const regex = new RegExp(pattern, flags);
      // Check all variable values for regex match
      for (const val of Object.values(variables)) {
        if (regex.test(String(val))) return true;
      }
      return false;
    } catch {
      // Invalid regex — fall through to legacy equality
    }
  }

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
   * Validate frontmatter against full schema (required, allowed, types, exclusivity, combinations)
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

    // 1. Required fields
    for (const field of FRONTMATTER_SCHEMA.required) {
      if (!(field in frontmatter) || frontmatter[field] == null) {
        errors.push(`Required field missing: "${field}"`);
      }
    }

    // 2. Unknown fields
    for (const key of Object.keys(frontmatter)) {
      if (!FRONTMATTER_SCHEMA.allowed.includes(key)) {
        errors.push(`Unknown frontmatter key: "${key}"`);
      }
    }

    // 3. Type checking
    for (const [key, schema] of Object.entries(FRONTMATTER_SCHEMA.types)) {
      if (!(key in frontmatter)) continue;
      const value = frontmatter[key];

      if (schema.type === 'oneOf') {
        // Special handling for 'requires' field which can be array
        const actualType = Array.isArray(value) ? 'array' : typeof value;
        if (!schema.values.includes(actualType)) {
          errors.push(
            `"${key}" must be one of types: ${schema.values.join(' | ')}, got ${actualType}`
          );
        }
      } else if (schema.enum) {
        if (typeof value !== schema.type || !schema.enum.includes(value)) {
          errors.push(`"${key}" must be one of: ${schema.enum.join(', ')}, got "${value}"`);
        }
      } else {
        if (typeof value !== schema) {
          errors.push(`"${key}" must be ${schema}, got ${typeof value}`);
        }
      }
    }

    // 4. Mutually exclusive groups
    for (const group of FRONTMATTER_SCHEMA.exclusive) {
      const present = group.filter(k => frontmatter[k]);
      if (present.length > 1) {
        errors.push(
          `Cannot specify both ${present.map(k => `"${k}"`).join(' and ')} — they are mutually exclusive`
        );
      }
    }

    // 5. Dependency rules
    for (const rule of FRONTMATTER_SCHEMA.requires) {
      if (rule.key in frontmatter && !(rule.companion in frontmatter)) {
        errors.push(rule.message);
      }
    }

    // 6. Warn about missing recommended fields
    const recommended = ['name', 'description', 'type'];
    for (const field of recommended) {
      if (!(field in frontmatter)) {
        warnings.push(`Recommended field missing: ${field}`);
      }
    }

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
