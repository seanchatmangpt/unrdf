/**
 * Standalone verification test for KGEN Base Templates (avoiding inheritance issues)
 */

import { describe, it, expect } from 'vitest';

describe('KGEN Base Templates Standalone Verification', () => {
  it('should import and instantiate macro templates', async () => {
    const { KGenMacroTemplates } = await import('../src/base/macro-templates.js');

    const macros = new KGenMacroTemplates({
      deterministicMode: true,
      staticBuildTime: '2024-01-01T00:00:00.000Z'
    });

    expect(macros).toBeDefined();
    expect(macros.getMacroNames()).toContain('document_header');
    expect(macros.getMacroNames()).toContain('js_class');
    expect(macros.getMacroNames().length).toBeGreaterThan(5);
  });

  it('should import and instantiate filter templates', async () => {
    const { KGenFilterTemplates } = await import('../src/base/filter-templates.js');

    const filters = new KGenFilterTemplates({
      deterministicMode: true,
      staticBuildTime: '2024-01-01T00:00:00.000Z'
    });

    expect(filters).toBeDefined();
    expect(filters.getTemplateNames()).toContain('basic_filter');
    expect(filters.getTemplateNames()).toContain('text_filter');
  });

  it('should import and instantiate SHACL templates', async () => {
    const { KGenSHACLTemplates } = await import('../src/base/shacl-templates.js');

    const shacl = new KGenSHACLTemplates({
      deterministicMode: true,
      staticBuildTime: '2024-01-01T00:00:00.000Z'
    });

    expect(shacl).toBeDefined();
    expect(shacl.getTemplateNames()).toContain('basic_node_shape');
    expect(shacl.getTemplateNames()).toContain('template_shape');
  });

  it('should import and instantiate injection targets', async () => {
    const { KGenInjectionTargets } = await import('../src/base/injection-targets.js');

    const targets = new KGenInjectionTargets({
      deterministicMode: true,
      staticBuildTime: '2024-01-01T00:00:00.000Z'
    });

    expect(targets).toBeDefined();
    expect(targets.getTemplateNames()).toContain('marker_target');
    expect(targets.getTemplateNames()).toContain('function_target');
  });

  it('should generate macro library content', async () => {
    const { KGenMacroTemplates } = await import('../src/base/macro-templates.js');

    const macros = new KGenMacroTemplates();
    const library = macros.generateMacroLibrary(['document_header', 'js_function']);

    expect(library).toContain('KGEN Macro Library');
    expect(library).toContain('document_header');
    expect(library).toContain('js_function');
  });

  it('should export macros in correct format', async () => {
    const { KGenMacroTemplates } = await import('../src/base/macro-templates.js');

    const macros = new KGenMacroTemplates();
    const exported = macros.exportMacros();

    expect(exported.format).toBe('kgen');
    expect(exported.namespace).toBe('kgen');
    expect(exported.macros).toBeDefined();
    expect(Object.keys(exported.macros).length).toBeGreaterThan(0);
  });

  it('should validate macro syntax', async () => {
    const { KGenMacroTemplates } = await import('../src/base/macro-templates.js');

    const macros = new KGenMacroTemplates();
    const validation = macros.validateMacro('document_header');

    expect(validation.valid).toBe(true);
    expect(validation.errors).toEqual([]);
  });

  it('should generate filter code from template', async () => {
    const { KGenFilterTemplates } = await import('../src/base/filter-templates.js');

    const filters = new KGenFilterTemplates();
    const generated = filters.generateFilter('basic_filter', {
      filterName: 'testFilter',
      filterDescription: 'Test filter description',
      defaultValue: 'null'
    });

    expect(generated.name).toBe('testFilter');
    expect(generated.description).toContain('Test filter description');
    expect(generated.code).toContain('testFilter');
  });

  it('should generate SHACL shape from template', async () => {
    const { KGenSHACLTemplates } = await import('../src/base/shacl-templates.js');

    const shacl = new KGenSHACLTemplates();
    const shape = shacl.generateShape('basic_node_shape', {
      shapeName: 'TestShape',
      targetClass: 'TestClass',
      description: 'Test shape description'
    });

    expect(shape.name).toBe('TestShapeShape');
    expect(shape.content).toContain('TestShapeShape');
    expect(shape.content).toContain('TestClass');
  });

  it('should generate injection target from template', async () => {
    const { KGenInjectionTargets } = await import('../src/base/injection-targets.js');

    const targets = new KGenInjectionTargets();
    const target = targets.generateTarget('marker_target', {
      markerName: 'TEST_SECTION',
      injectionContent: 'console.log("injected");'
    });

    expect(target.name).toBe('MarkerBasedTarget');
    expect(target.content).toContain('TEST_SECTION');
    expect(target.content).toContain('console.log("injected");');
  });
});