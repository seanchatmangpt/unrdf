/**
 * Tests for Meta-Template Engine
 */

import { describe, it, expect, beforeEach } from 'vitest';
import nunjucks from 'nunjucks';
import { MetaTemplateEngine, generateCRUDTemplate, generateTestTemplate } from '../src/meta-template-engine.mjs';

// Real Nunjucks renderer (the engine's templates use Nunjucks syntax: elif, ===, ternaries, filters)
class NunjucksRenderer {
  async render(template, context, options = {}) {
    const env = new nunjucks.Environment(null, { autoescape: false });
    for (const [name, fn] of Object.entries(options.filters || {})) {
      env.addFilter(name, fn);
    }
    return {
      content: env.renderString(template, context),
      metadata: { renderTime: new Date().toISOString() },
    };
  }
}

describe('MetaTemplateEngine', () => {
  let engine;
  let renderer;

  beforeEach(() => {
    renderer = new NunjucksRenderer();
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
    // Stage 1 renders the meta-template; {% raw %} keeps {{ name }} as a stage-2 placeholder
    const metaTemplate = 'Hello {% raw %}{{ name }}{% endraw %}';

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
