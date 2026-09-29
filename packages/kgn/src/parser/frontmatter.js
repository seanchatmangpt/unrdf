/**
 * Frontmatter Parser - YAML frontmatter extraction and parsing
 * Migrated from ~/unjucks with enhanced error handling
 */

import matter from 'gray-matter';
import { parse as parseYAML, stringify as stringifyYAML } from 'yaml';

/**
 *
 */
export class FrontmatterParser {
  /**
   *
   */
  constructor(options = {}) {
    this.strict = options.strict !== false;
    this.allowEmpty = options.allowEmpty !== false;
  }

  /**
   * Parse frontmatter from template content
   */
  parse(content) {
    try {
      const result = matter(content, {
        engines: {
          yaml: {
            parse: parseYAML,
            stringify: stringifyYAML
          }
        }
      });

      return {
        frontmatter: result.data || {},
        content: result.content || '',
        isEmpty: result.isEmpty,
        excerpt: result.excerpt
      };

    } catch (error) {
      if (this.strict) {
        throw new Error(`Frontmatter parsing failed: ${error.message}`);
      }

      // Return content as-is if parsing fails in non-strict mode
      return {
        frontmatter: {},
        content: content,
        isEmpty: true,
        parseError: error.message
      };
    }
  }

  /**
   * Extract only frontmatter without parsing content
   */
  extractFrontmatter(content) {
    try {
      const result = matter(content);
      return {
        success: true,
        frontmatter: result.data || {},
        isEmpty: result.isEmpty
      };
    } catch (error) {
      return {
        success: false,
        error: error.message,
        frontmatter: {}
      };
    }
  }

  /**
   * Validate frontmatter structure
   */
  validate(frontmatter, schema = {}) {
    const errors = [];
    const warnings = [];

    // Check required fields
    if (schema.required) {
      schema.required.forEach(field => {
        if (!(field in frontmatter)) {
          errors.push(`Missing required field: ${field}`);
        }
      });
    }

    // Check field types
    if (schema.types) {
      Object.entries(schema.types).forEach(([field, expectedType]) => {
        if (field in frontmatter) {
          const actualType = typeof frontmatter[field];
          if (actualType !== expectedType) {
            errors.push(`Field '${field}' should be ${expectedType}, got ${actualType}`);
          }
        }
      });
    }

    // Check allowed values
    if (schema.enum) {
      Object.entries(schema.enum).forEach(([field, allowedValues]) => {
        if (field in frontmatter) {
          if (!allowedValues.includes(frontmatter[field])) {
            errors.push(`Field '${field}' must be one of: ${allowedValues.join(', ')}`);
          }
        }
      });
    }

    // Template-specific validations
    this.validateTemplateFields(frontmatter, errors, warnings);

    return {
      valid: errors.length === 0,
      errors,
      warnings
    };
  }

  /**
   * Validate template-specific frontmatter fields
   */
  validateTemplateFields(frontmatter, errors, warnings) {
    // Check for common template metadata
    const recommendedFields = ['name', 'description', 'version'];
    recommendedFields.forEach(field => {
      if (!(field in frontmatter)) {
        warnings.push(`Recommended field missing: ${field}`);
      }
    });

    // Validate variables definition
    if (frontmatter.variables) {
      if (typeof frontmatter.variables !== 'object') {
        errors.push('variables field should be an object');
      } else {
        Object.entries(frontmatter.variables).forEach(([varName, varDesc]) => {
          if (typeof varDesc !== 'string' && typeof varDesc !== 'object') {
            warnings.push(`Variable '${varName}' should have a description`);
          }
        });
      }
    }

    // Validate template type
    if (frontmatter.type) {
      const validTypes = ['component', 'page', 'service', 'model', 'config', 'documentation'];
      if (!validTypes.includes(frontmatter.type)) {
        warnings.push(`Unknown template type: ${frontmatter.type}`);
      }
    }

    // Validate output configuration
    if (frontmatter.output) {
      if (typeof frontmatter.output === 'string') {
        // Simple path - OK
      } else if (typeof frontmatter.output === 'object') {
        // Complex output config - validate structure
        if (frontmatter.output.path && typeof frontmatter.output.path !== 'string') {
          errors.push('output.path must be a string');
        }
        if (frontmatter.output.mode && !['write', 'append', 'inject'].includes(frontmatter.output.mode)) {
          errors.push('output.mode must be one of: write, append, inject');
        }
      } else {
        errors.push('output must be a string path or object configuration');
      }
    }
  }

  /**
   * Merge frontmatter with defaults
   */
  mergeWithDefaults(frontmatter, defaults = {}) {
    return {
      // Template metadata defaults
      name: frontmatter.name || defaults.name || 'Untitled Template',
      description: frontmatter.description || defaults.description || '',
      version: frontmatter.version || defaults.version || '1.0.0',
      author: frontmatter.author || defaults.author || '',
      
      // Template configuration
      type: frontmatter.type || defaults.type || 'component',
      category: frontmatter.category || defaults.category || 'general',
      tags: frontmatter.tags || defaults.tags || [],
      
      // Output configuration
      output: frontmatter.output || defaults.output,
      
      // Variable definitions
      variables: {
        ...defaults.variables,
        ...frontmatter.variables
      },
      
      // Custom fields
      ...Object.fromEntries(
        Object.entries(frontmatter).filter(([key]) => 
          !['name', 'description', 'version', 'author', 'type', 'category', 'tags', 'output', 'variables'].includes(key)
        )
      )
    };
  }

  /**
   * Convert frontmatter back to YAML string
   */
  stringify(frontmatter, content = '') {
    try {
      const yamlString = stringifyYAML(frontmatter);
      return `---\n${yamlString}---\n${content}`;
    } catch (error) {
      throw new Error(`Failed to stringify frontmatter: ${error.message}`);
    }
  }

  /**
   * Get parser statistics
   */
  getStats() {
    return {
      strict: this.strict,
      allowEmpty: this.allowEmpty
    };
  }
}

export default FrontmatterParser;