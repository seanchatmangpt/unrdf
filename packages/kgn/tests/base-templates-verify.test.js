/**
 * Basic verification test for KGEN Base Templates
 */

import { describe, it, expect } from 'vitest';

describe('KGEN Base Templates Verification', () => {
  it('should import base template classes without errors', async () => {
    // Test that all base template classes can be imported
    const { KGenTemplateBase } = await import('../src/base/template-base.js');
    const { KGenMacroTemplates } = await import('../src/base/macro-templates.js');
    const { KGenFilterTemplates } = await import('../src/base/filter-templates.js');
    const { KGenSHACLTemplates } = await import('../src/base/shacl-templates.js');
    const { KGenInjectionTargets } = await import('../src/base/injection-targets.js');

    expect(KGenTemplateBase).toBeDefined();
    expect(KGenMacroTemplates).toBeDefined();
    expect(KGenFilterTemplates).toBeDefined();
    expect(KGenSHACLTemplates).toBeDefined();
    expect(KGenInjectionTargets).toBeDefined();
  });

  it('should create template base instance', async () => {
    const { KGenTemplateBase } = await import('../src/base/template-base.js');

    const base = new KGenTemplateBase({
      deterministicMode: true,
      staticBuildTime: '2024-01-01T00:00:00.000Z'
    });

    expect(base).toBeDefined();
    expect(base.options.deterministicMode).toBe(true);
    expect(base.templates.size).toBeGreaterThan(0);
    expect(base.macros.size).toBeGreaterThan(0);
  });

  it('should have registered base templates', async () => {
    const { KGenTemplateBase } = await import('../src/base/template-base.js');

    const base = new KGenTemplateBase();
    const templateNames = Array.from(base.templates.keys());

    expect(templateNames).toContain('base.layout');
    expect(templateNames).toContain('base.component');
    expect(templateNames).toContain('base.module');
  });

  it('should export base templates from main index', async () => {
    const { BaseTemplates } = await import('../src/index.js');

    expect(BaseTemplates).toBeDefined();
    expect(BaseTemplates.KGenTemplateBase).toBeDefined();
    expect(BaseTemplates.createTemplateSystem).toBeDefined();
  });

  it('should get template statistics', async () => {
    const { KGenTemplateBase } = await import('../src/base/template-base.js');

    const base = new KGenTemplateBase();
    const stats = base.getStats();

    expect(stats).toBeDefined();
    expect(stats.templates).toBeGreaterThan(0);
    expect(stats.blocks).toBeGreaterThan(-1);
    expect(stats.macros).toBeGreaterThan(0);
    expect(stats.options).toBeDefined();
  });
});