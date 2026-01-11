/**
 * Tests for Meta-Template Engine
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { MetaTemplateEngine, generateCRUDTemplate, generateTestTemplate } from '../src/meta-template-engine.mjs';

// Mock renderer for testing
class MockRenderer {
  async render(template, context, options) {
    // Simple variable replacement
    let result = template;

    // Replace {{ variable }}
    result = result.replace(/\{\{\s*(\w+)\s*\}\}/g, (match, name) => {
      return context[name] || '';
    });

    // Replace {{ variable | filter }}
    result = result.replace(/\{\{\s*(\w+)\s*\|\s*(\w+)\s*\}\}/g, (match, name, filter) => {
      const value = context[name] || '';
      if (filter === 'lower') return String(value).toLowerCase();
      if (filter === 'capitalize') return String(value).charAt(0).toUpperCase() + String(value).slice(1);
      return value;
    });

    // Handle loops: {% for item in items %}...{% endfor %}
    result = result.replace(/\{%\s*for\s+(\w+)\s+in\s+(\w+)\s*%\}([\s\S]*?)\{%\s*endfor\s*%\}/g,
      (match, itemVar, arrayName, loopContent) => {
        const array = context[arrayName] || [];
        return array.map((item, index) => {
          let itemContent = loopContent;
          itemContent = itemContent.replace(/\{\{\s*(\w+)\s*\}\}/g, (m, prop) => {
            if (prop === itemVar) return item;
            if (prop === 'loop.last') return index === array.length - 1 ? 'true' : '';
            return context[prop] || '';
          });
          return itemContent;
        }).join('\n');
      }
    );

    // Handle conditionals: {% if condition %}...{% endif %}
    result = result.replace(/\{%\s*if\s+(\w+)\s*%\}([\s\S]*?)\{%\s*endif\s*%\}/g,
      (match, condVar, content) => {
        return context[condVar] ? content : '';
      }
    );

    // Remove comments: {# comment #}
    result = result.replace(/\{#[\s\S]*?#\}/g, '');

    return {
      content: result,
      metadata: {
        renderTime: new Date().toISOString(),
      },
    };
  }
}

describe('MetaTemplateEngine', () => {
  let engine;
  let renderer;

  beforeEach(() => {
    renderer = new MockRenderer();
    engine = new MetaTemplateEngine(renderer);
  });

  it('should generate template from meta-template', async () => {
    const metaTemplate = `
/**
 * Generated: {{ entityName }}
 */
export const {{ entityName }}Schema = z.object({});
    `.trim();

    const result = await engine.generateTemplate(metaTemplate, {
      templateName: 'user-schema',
      entityName: 'User',
    });

    expect(result.templateId).toBe('user-schema');
    expect(result.template).toContain('export const UserSchema');
    expect(result.hash).toBeDefined();
    expect(result.metadata.generatedAt).toBeDefined();
  });

  it('should cache generated templates', async () => {
    const metaTemplate = 'Template: {{ name }}';

    await engine.generateTemplate(metaTemplate, {
      templateName: 'test-template',
      name: 'Test',
    });

    const cached = engine.generatedTemplates.get('test-template');
    expect(cached).toBeDefined();
    expect(cached.template).toContain('Template: Test');
  });

  it('should render generated template with data', async () => {
    const metaTemplate = 'Hello {{ name }}';

    await engine.generateTemplate(metaTemplate, {
      templateName: 'greeting',
      name: 'placeholder',
    });

    const result = await engine.renderGenerated('greeting', {
      name: 'World',
    });

    expect(result.content).toContain('Hello World');
  });

  it('should throw error for unknown template', async () => {
    await expect(
      engine.renderGenerated('nonexistent', {})
    ).rejects.toThrow('Template nonexistent not found');
  });

  it('should validate template syntax', () => {
    // Unbalanced braces
    expect(() => {
      engine.validateTemplate('{{ open');
    }).toThrow('Unbalanced');

    // Balanced braces
    expect(() => {
      engine.validateTemplate('{{ valid }}');
    }).not.toThrow();
  });

  it('should generate template hierarchy', async () => {
    const rootTemplate = 'Root: {{ name }}';

    const contexts = [
      { templateName: 'template-1', name: 'First' },
      { templateName: 'template-2', name: 'Second' },
    ];

    const results = await engine.generateHierarchy(rootTemplate, contexts);

    expect(results).toHaveLength(2);
    expect(results[0].templateId).toBe('template-1');
    expect(results[1].templateId).toBe('template-2');
  });

  it('should enforce max depth limit', async () => {
    const metaTemplate = 'Nested template';

    const contexts = [
      {
        templateName: 'level-1',
        isMeta: true,
        childContexts: [
          {
            templateName: 'level-2',
            isMeta: true,
            childContexts: [
              {
                templateName: 'level-3',
                isMeta: true,
                childContexts: [
                  { templateName: 'level-4' },
                ],
              },
            ],
          },
        ],
      },
    ];

    await expect(
      engine.generateHierarchy(metaTemplate, contexts)
    ).rejects.toThrow('Maximum meta-template depth');
  });

  it('should provide generation statistics', async () => {
    const metaTemplate = 'Test';

    await engine.generateTemplate(metaTemplate, {
      templateName: 'test-1',
    });

    await engine.generateTemplate(metaTemplate, {
      templateName: 'test-2',
    });

    const stats = engine.getStats();

    expect(stats.totalGenerated).toBe(2);
    expect(stats.generationHistory).toBe(2);
  });

  it('should clear template cache', async () => {
    const metaTemplate = 'Test';

    await engine.generateTemplate(metaTemplate, {
      templateName: 'test',
    });

    expect(engine.generatedTemplates.size).toBe(1);

    engine.clearCache();

    expect(engine.generatedTemplates.size).toBe(0);
  });

  it('should generate CRUD template', async () => {
    const result = await generateCRUDTemplate(engine, {
      entityName: 'Product',
      operations: ['create', 'read', 'update', 'delete'],
    });

    expect(result.template).toContain('createProduct');
    expect(result.template).toContain('readProduct');
    expect(result.template).toContain('updateProduct');
    expect(result.template).toContain('deleteProduct');
  });

  it('should generate test template', async () => {
    const result = await generateTestTemplate(engine, {
      moduleName: 'calculator',
      imports: ['add', 'subtract'],
      testCases: [
        {
          describe: 'add',
          tests: [
            {
              should: 'add two numbers',
              arrange: 'const a = 1, b = 2',
              act: 'const result = add(a, b)',
              assert: 'expect(result).toBe(3)',
            },
          ],
        },
      ],
    });

    expect(result.template).toContain('describe(\'calculator\'');
    expect(result.template).toContain('it(\'add two numbers\'');
  });

  it('should create deterministic hashes', () => {
    const content = 'test content';
    const hash1 = engine.hash(content);
    const hash2 = engine.hash(content);

    expect(hash1).toBe(hash2);
    expect(hash1).toHaveLength(16);
  });

  it('should track generation history', async () => {
    const metaTemplate = 'Test';

    await engine.generateTemplate(metaTemplate, {
      templateName: 'test-1',
    });

    expect(engine.generationHistory).toHaveLength(1);
    expect(engine.generationHistory[0].templateId).toBe('test-1');
    expect(engine.generationHistory[0].duration).toBeGreaterThanOrEqual(0);
  });
});
