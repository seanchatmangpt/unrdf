/**
 * @file Open-Ontologies MCP Tools Tests
 * @description Unit tests for open-ontologies CLI wrapper functionality
 */

import { describe, it, expect, vi } from 'vitest';
import { ontoRegistry, getOntoCommand, getOntoMetadata, getToolsByPhase } from '../open-ontologies-registry.mjs';

// Mock the helpers to avoid actual CLI calls
vi.mock('../open-ontologies-helpers.mjs', () => ({
  runOntoCommand: vi.fn(() => Promise.resolve({ success: true })),
  validateOntoInstallation: vi.fn(() => Promise.resolve(true)),
  ensureDataDir: vi.fn(() => Promise.resolve('/tmp/test-onto')),
  formatOntoError: (error) => `Error: ${error.message}`,
  createOntoResponse: (result) => ({
    content: [{ type: 'text', text: JSON.stringify(result) }],
    isError: false,
  }),
  createOntoErrorResponse: (error) => ({
    content: [{ type: 'text', text: `Error: ${error.message}` }],
    isError: true,
  }),
}));

describe('open-ontologies-registry', () => {
  it('should export registry with all 15 tools', () => {
    expect(Object.keys(ontoRegistry)).toHaveLength(15);
  });

  it('should include Phase 1 core tools', () => {
    const coreTools = getToolsByPhase('core');
    expect(coreTools).toEqual([
      'onto_validate',
      'onto_stats',
      'onto_query',
      'onto_load',
      'onto_marketplace',
    ]);
  });

  it('should include Phase 2 advanced tools', () => {
    const advancedTools = getToolsByPhase('advanced');
    expect(advancedTools).toEqual([
      'onto_reason',
      'onto_shacl',
      'onto_save',
      'onto_clear',
      'onto_convert',
    ]);
  });

  it('should include Phase 3 expert tools', () => {
    const expertTools = getToolsByPhase('expert');
    expect(expertTools).toEqual([
      'onto_align',
      'onto_drift',
      'onto_plan',
      'onto_apply',
      'onto_version',
    ]);
  });

  it('should map tool names to CLI commands', () => {
    expect(getOntoCommand('onto_validate')).toEqual(['validate']);
    expect(getOntoCommand('onto_stats')).toEqual(['stats']);
    expect(getOntoCommand('onto_query')).toEqual(['query']);
    expect(getOntoCommand('onto_load')).toEqual(['load']);
    expect(getOntoCommand('onto_marketplace')).toEqual(['marketplace']);
  });

  it('should map advanced tool names to CLI commands', () => {
    expect(getOntoCommand('onto_reason')).toEqual(['reason']);
    expect(getOntoCommand('onto_shacl')).toEqual(['shacl']);
    expect(getOntoCommand('onto_save')).toEqual(['save']);
    expect(getOntoCommand('onto_clear')).toEqual(['clear']);
    expect(getOntoCommand('onto_convert')).toEqual(['convert']);
  });

  it('should map expert tool names to CLI commands', () => {
    expect(getOntoCommand('onto_align')).toEqual(['align']);
    expect(getOntoCommand('onto_drift')).toEqual(['drift']);
    expect(getOntoCommand('onto_plan')).toEqual(['plan']);
    expect(getOntoCommand('onto_apply')).toEqual(['apply']);
    expect(getOntoCommand('onto_version')).toEqual(['version']);
  });

  it('should provide metadata for each tool', () => {
    const validateMeta = getOntoMetadata('onto_validate');
    expect(validateMeta).toMatchObject({
      name: 'onto_validate',
      phase: 'core',
      description: expect.stringContaining('Validate RDF/OWL syntax'),
    });
    expect(validateMeta.examples).toHaveLength(2);
    expect(validateMeta.examples[0]).toContain('onto_validate');
  });

  it('should provide metadata for advanced tools', () => {
    const reasonMeta = getOntoMetadata('onto_reason');
    expect(reasonMeta).toMatchObject({
      name: 'onto_reason',
      phase: 'advanced',
      description: expect.stringContaining('reasoning'),
    });
  });

  it('should provide metadata for expert tools', () => {
    const alignMeta = getOntoMetadata('onto_align');
    expect(alignMeta).toMatchObject({
      name: 'onto_align',
      phase: 'expert',
      description: expect.stringContaining('alignment'),
    });
  });

  it('should return null for unknown tool', () => {
    expect(getOntoCommand('unknown_tool')).toBeNull();
    expect(getOntoMetadata('unknown_tool')).toBeNull();
  });

  it('should export ONTO_BINARY and ONTO_DATA_DIR constants', async () => {
    const { ONTO_BINARY, ONTO_DATA_DIR } = await import('../open-ontologies-registry.mjs');
    expect(ONTO_BINARY).toBeDefined();
    expect(ONTO_BINARY).toContain('open-ontologies');
    expect(ONTO_DATA_DIR).toBeDefined();
    expect(ONTO_DATA_DIR).toContain('.open-ontologies');
  });
});

describe('open-ontologies registry metadata structure', () => {
  it('should have all required metadata fields for core tools', () => {
    const coreTools = getToolsByPhase('core');

    for (const toolName of coreTools) {
      const meta = getOntoMetadata(toolName);
      expect(meta).toHaveProperty('name');
      expect(meta).toHaveProperty('phase', 'core');
      expect(meta).toHaveProperty('description');
      expect(meta).toHaveProperty('examples');
      expect(meta.examples).toBeInstanceOf(Array);
      expect(meta.examples.length).toBeGreaterThan(0);
    }
  });

  it('should have all required metadata fields for advanced tools', () => {
    const advancedTools = getToolsByPhase('advanced');

    for (const toolName of advancedTools) {
      const meta = getOntoMetadata(toolName);
      expect(meta).toHaveProperty('name');
      expect(meta).toHaveProperty('phase', 'advanced');
      expect(meta).toHaveProperty('description');
      expect(meta).toHaveProperty('examples');
      expect(meta.examples).toBeInstanceOf(Array);
      expect(meta.examples.length).toBeGreaterThan(0);
    }
  });

  it('should have all required metadata fields for expert tools', () => {
    const expertTools = getToolsByPhase('expert');

    for (const toolName of expertTools) {
      const meta = getOntoMetadata(toolName);
      expect(meta).toHaveProperty('name');
      expect(meta).toHaveProperty('phase', 'expert');
      expect(meta).toHaveProperty('description');
      expect(meta).toHaveProperty('examples');
      expect(meta.examples).toBeInstanceOf(Array);
      expect(meta.examples.length).toBeGreaterThan(0);
    }
  });

  it('should have unique tool names across all phases', () => {
    const allToolNames = [
      ...getToolsByPhase('core'),
      ...getToolsByPhase('advanced'),
      ...getToolsByPhase('expert'),
    ];

    const uniqueNames = new Set(allToolNames);
    expect(uniqueNames.size).toBe(allToolNames.length);
  });
});
