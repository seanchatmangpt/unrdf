/**
 * Meta-Template Engine - Templates that generate templates
 * @module @unrdf/codegen/meta-template-engine
 * @description
 * Innovation: Self-modifying template system for pattern replication
 *
 * Pattern 2 from Code Generation Research
 */

import { z } from 'zod';
import { createHash } from 'crypto';

const MetaTemplateOptionsSchema = z.object({
  maxDepth: z.number().int().positive().default(3),
  cacheTemplates: z.boolean().default(true),
  validateOutput: z.boolean().default(true),
});

/**
 * Meta-Template Engine that generates new templates from patterns
 */
export class MetaTemplateEngine {
  /**
   * @param {Object} renderer - Template renderer instance
   * @param {Object} options - Engine options
   */
  constructor(renderer, options = {}) {
    this.renderer = renderer;
    this.options = MetaTemplateOptionsSchema.parse(options);
    this.generatedTemplates = new Map();
    this.generationHistory = [];
  }

  /**
   * Generate new template from meta-template pattern
   * @param {string} metaTemplate - Meta-template content
   * @param {Object} context - Context data for meta-template
   * @returns {Promise<Object>} Generated template result
   */
  async generateTemplate(metaTemplate, context) {
    const startTime = Date.now();

    // Validate context
    if (!context.templateName) {
      throw new Error('context.templateName is required');
    }

    // Render meta-template to produce new template
    const { content: newTemplate } = await this.renderer.render(
      metaTemplate,
      context,
      { filters: this.createMetaFilters() }
    );

    // Validate generated template
    if (this.options.validateOutput) {
      this.validateTemplate(newTemplate);
    }

    // Generate template ID
    const templateId = context.templateName;
    const templateHash = this.hash(newTemplate);

    // Cache template
    if (this.options.cacheTemplates) {
      this.generatedTemplates.set(templateId, {
        template: newTemplate,
        hash: templateHash,
        context,
        generatedAt: new Date().toISOString(),
      });
    }

    // Record generation
    this.generationHistory.push({
      templateId,
      hash: templateHash,
      timestamp: new Date().toISOString(),
      duration: Date.now() - startTime,
    });

    return {
      templateId,
      template: newTemplate,
      hash: templateHash,
      metadata: {
        generatedAt: new Date().toISOString(),
        fromContext: context,
        linesGenerated: newTemplate.split('\n').length,
      },
    };
  }

  /**
   * Render previously generated template
   * @param {string} templateId - Template identifier
   * @param {Object} data - Data for template rendering
   * @returns {Promise<Object>} Render result
   */
  async renderGenerated(templateId, data) {
    const cached = this.generatedTemplates.get(templateId);
    if (!cached) {
      throw new Error(`Template ${templateId} not found. Generate it first.`);
    }

    return this.renderer.render(
      cached.template,
      data,
      { filters: this.createMetaFilters() }
    );
  }

  /**
   * Generate template hierarchy (template that generates templates)
   * @param {string} rootMetaTemplate - Root meta-template
   * @param {Array} contexts - Array of context objects
   * @param {number} depth - Current depth (for recursion limit)
   * @returns {Promise<Array>} Array of generated templates
   */
  async generateHierarchy(rootMetaTemplate, contexts, depth = 0) {
    if (depth >= this.options.maxDepth) {
      throw new Error(`Maximum meta-template depth ${this.options.maxDepth} exceeded`);
    }

    const generated = [];

    for (const context of contexts) {
      const result = await this.generateTemplate(rootMetaTemplate, context);
      generated.push(result);

      // Check if generated template is itself a meta-template
      if (context.isMeta && context.childContexts) {
        const children = await this.generateHierarchy(
          result.template,
          context.childContexts,
          depth + 1
        );
        generated.push(...children);
      }
    }

    return generated;
  }

  /**
   * Create custom filters for meta-template processing
   * @returns {Object} Filter functions
   */
  createMetaFilters() {
    return {
      // Template-specific filters
      templateName: (str) => {
        return str.replace(/[^a-zA-Z0-9-]/g, '-').toLowerCase();
      },

      escapeTemplate: (str) => {
        // Escape template syntax for nested templates
        return str
          .replace(/\{\{/g, '\\{\\{')
          .replace(/\}\}/g, '\\}\\}')
          .replace(/\{%/g, '\\{%')
          .replace(/%\}/g, '%\\}');
      },

      unescapeTemplate: (str) => {
        return str
          .replace(/\\\{\\\{/g, '{{')
          .replace(/\\\}\\\}/g, '}}')
          .replace(/\\\{%/g, '{%')
          .replace(/%\\\}/g, '%}');
      },

      indent: (str, spaces = 2) => {
        const indent = ' '.repeat(spaces);
        return str.split('\n').map(line => indent + line).join('\n');
      },

      dedent: (str) => {
        const lines = str.split('\n');
        const minIndent = Math.min(
          ...lines
            .filter(line => line.trim().length > 0)
            .map(line => line.match(/^\s*/)?.[0].length || 0)
        );
        return lines.map(line => line.slice(minIndent)).join('\n');
      },

      hash: (content) => this.hash(content),
    };
  }

  /**
   * Validate generated template syntax
   * @param {string} template - Template to validate
   * @throws {Error} If template is invalid
   */
  validateTemplate(template) {
    // Check for balanced braces
    const openBraces = (template.match(/\{\{/g) || []).length;
    const closeBraces = (template.match(/\}\}/g) || []).length;

    if (openBraces !== closeBraces) {
      throw new Error(`Unbalanced {{ }} in generated template`);
    }

    // Check for balanced blocks
    const openBlocks = (template.match(/\{%\s*(?:if|for)/g) || []).length;
    const closeBlocks = (template.match(/\{%\s*end(?:if|for)/g) || []).length;

    if (openBlocks !== closeBlocks) {
      throw new Error(`Unbalanced {% %} blocks in generated template`);
    }
  }

  /**
   * Hash content for template ID generation
   * @param {string} content - Content to hash
   * @returns {string} Hash string
   */
  hash(content) {
    return createHash('sha256').update(content).digest('hex').substring(0, 16);
  }

  /**
   * Get all generated templates
   * @returns {Array} Array of template metadata
   */
  getGeneratedTemplates() {
    return Array.from(this.generatedTemplates.entries()).map(([id, data]) => ({
      id,
      hash: data.hash,
      generatedAt: data.generatedAt,
      linesGenerated: data.template.split('\n').length,
    }));
  }

  /**
   * Get generation statistics
   * @returns {Object} Statistics object
   */
  getStats() {
    return {
      totalGenerated: this.generatedTemplates.size,
      generationHistory: this.generationHistory.length,
      cacheSize: this.generatedTemplates.size,
      options: this.options,
    };
  }

  /**
   * Clear template cache
   */
  clearCache() {
    this.generatedTemplates.clear();
  }
}

/**
 * Create CRUD operations template from entity pattern
 * @param {Object} engine - MetaTemplateEngine instance
 * @param {Object} entityConfig - Entity configuration
 * @returns {Promise<Object>} Generated CRUD template
 */
export async function generateCRUDTemplate(engine, entityConfig) {
  const crudMetaTemplate = `
{# CRUD Operations Template Generator #}
/**
 * CRUD operations for {{ entityName }}
 * Generated: {{ timestamp }}
 */

import { z } from 'zod';
import { {{ entityName }}Schema } from './{{ entityName | lower }}.schema.mjs';

{% for operation in operations %}
/**
 * {{ operation | capitalize }} {{ entityName }}
 */
export async function {{ operation }}{{ entityName }}(
  {%- if operation === 'create' -%}
  data: z.infer<typeof {{ entityName }}Schema>
  {%- elif operation === 'read' -%}
  id: string
  {%- elif operation === 'update' -%}
  id: string, data: Partial<z.infer<typeof {{ entityName }}Schema>>
  {%- elif operation === 'delete' -%}
  id: string
  {%- endif -%}
): Promise<{% if operation !== 'delete' %}z.infer<typeof {{ entityName }}Schema>{% else %}void{% endif %}> {
  // Implementation for {{ operation }}
  {% if operation === 'create' %}
  const validated = {{ entityName }}Schema.parse(data);
  // Insert into store
  return validated;
  {% elif operation === 'read' %}
  // Query by ID
  const result = await store.findById(id);
  return {{ entityName }}Schema.parse(result);
  {% elif operation === 'update' %}
  // Update by ID
  const result = await store.update(id, data);
  return {{ entityName }}Schema.parse(result);
  {% elif operation === 'delete' %}
  // Delete by ID
  await store.delete(id);
  {% endif %}
}

{% endfor %}

export default {
  {% for operation in operations -%}
  {{ operation }}: {{ operation }}{{ entityName }}{{ ',' if not loop.last else '' }}
  {% endfor %}
};
  `.trim();

  return engine.generateTemplate(crudMetaTemplate, {
    templateName: `${entityConfig.entityName.toLowerCase()}-crud`,
    entityName: entityConfig.entityName,
    operations: entityConfig.operations || ['create', 'read', 'update', 'delete'],
    timestamp: new Date().toISOString(),
  });
}

/**
 * Create test template generator
 * @param {Object} engine - MetaTemplateEngine instance
 * @param {Object} testConfig - Test configuration
 * @returns {Promise<Object>} Generated test template
 */
export async function generateTestTemplate(engine, testConfig) {
  const testMetaTemplate = `
{# Test Template Generator #}
/**
 * Tests for {{ moduleName }}
 * Generated: {{ timestamp }}
 */

import { describe, it, expect } from 'vitest';
import {{ testConfig.imports | join(', ') }} from './{{ moduleName }}.mjs';

describe('{{ moduleName }}', () => {
  {% for testCase in testCases %}
  describe('{{ testCase.describe }}', () => {
    {% for test in testCase.tests %}
    it('{{ test.should }}', async () => {
      // Arrange
      {{ test.arrange | indent(6) }}

      // Act
      {{ test.act | indent(6) }}

      // Assert
      {{ test.assert | indent(6) }}
    });
    {% endfor %}
  });
  {% endfor %}
});
  `.trim();

  return engine.generateTemplate(testMetaTemplate, {
    templateName: `${testConfig.moduleName}-test`,
    ...testConfig,
    timestamp: new Date().toISOString(),
  });
}

export default MetaTemplateEngine;
