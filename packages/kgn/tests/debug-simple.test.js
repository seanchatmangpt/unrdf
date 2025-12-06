// Debug tests for KGEN Template Engine
import { describe, it, expect, beforeEach } from 'vitest';
import { KGenTemplateEngine } from '../src/core/kgen-engine.js';

describe('Debug KGEN Engine', () => {
  let engine;

  beforeEach(() => {
    engine = new KGenTemplateEngine({
      deterministicMode: true,
      strictMode: false,
      enableAttestation: false
    });
  });

  it('should render simple variable', async () => {
    const template = '{{ name }}';
    const context = { name: 'test' };

    const result = await engine.renderTemplate(template, context);
    console.log('Simple variable result:', JSON.stringify(result));
    expect(result).toBe('test');
  });

  it('should handle missing variable gracefully', async () => {
    const template = '{{ missing }}';
    const context = {};

    const result = await engine.renderTemplate(template, context);
    console.log('Missing variable result:', JSON.stringify(result));
    expect(result).toBe('');
  });

  it('should apply upper filter', async () => {
    const template = '{{ name | upper }}';
    const context = { name: 'test' };

    const result = await engine.renderTemplate(template, context);
    console.log('Upper filter result:', JSON.stringify(result));
    expect(result).toBe('TEST');
  });

  it('should handle simple conditional', async () => {
    const template = '{% if show %}visible{% endif %}';
    const context = { show: true };

    const result = await engine.renderTemplate(template, context);
    console.log('Conditional result:', JSON.stringify(result));
    expect(result).toBe('visible');
  });

  it('should handle simple loop', async () => {
    const template = '{% for item in items %}{{ item }}{% endfor %}';
    const context = { items: ['a', 'b'] };

    const result = await engine.renderTemplate(template, context);
    console.log('Loop result:', JSON.stringify(result));
    expect(result).toBe('ab');
  });

  it('should debug full pipeline', async () => {
    const template = '{{ name }}';
    const context = { name: 'debug' };

    console.log('=== DEBUGGING FULL PIPELINE ===');

    const plan = await engine.plan(template, context);
    console.log('Plan result:', JSON.stringify(plan, null, 2));

    if (plan.success) {
      const renderResult = await engine.render(plan, context);
      console.log('Render result:', JSON.stringify(renderResult, null, 2));

      if (renderResult.success) {
        const postResult = await engine.post(renderResult);
        console.log('Post result:', JSON.stringify(postResult, null, 2));

        expect(postResult.success).toBe(true);
      }
    }
  });
});