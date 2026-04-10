/**
 * @file Sync Orchestrator Tests
 * @module cli/test/sync/orchestrator
 * @description Tests for the main orchestrator that coordinates config, ontology, SPARQL, and template rendering
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';

// Mock all dependencies before importing orchestrator
vi.mock('fs/promises', () => ({
  mkdir: vi.fn().mockResolvedValue(undefined),
  writeFile: vi.fn().mockResolvedValue(undefined),
  access: vi.fn().mockResolvedValue(undefined),
}));

vi.mock('../../src/cli/commands/sync/config-parser.mjs', () => ({
  parseConfig: vi.fn(),
}));

vi.mock('../../src/cli/commands/sync/ontology-loader.mjs', () => ({
  loadOntology: vi.fn(),
}));

vi.mock('../../src/cli/commands/sync/sparql-executor.mjs', () => ({
  executeSparqlQuery: vi.fn(),
}));

vi.mock('../../src/cli/commands/sync/template-renderer.mjs', () => ({
  renderTemplate: vi.fn(),
}));

import { mkdir, writeFile } from 'fs/promises';
import { parseConfig } from '../../src/cli/commands/sync/config-parser.mjs';
import { loadOntology } from '../../src/cli/commands/sync/ontology-loader.mjs';
import { executeSparqlQuery } from '../../src/cli/commands/sync/sparql-executor.mjs';
import { renderTemplate } from '../../src/cli/commands/sync/template-renderer.mjs';
import { runSync } from '../../src/cli/commands/sync/orchestrator.mjs';

describe('Sync Orchestrator', () => {
  let originalLog, originalError, logOutput, errorOutput;
  let mockStore;

  beforeEach(() => {
    logOutput = [];
    errorOutput = [];
    originalLog = console.log;
    originalError = console.error;
    console.log = vi.fn((...args) => logOutput.push(args.join(' ')));
    console.error = vi.fn((...args) => errorOutput.push(args.join(' ')));
    mockStore = { size: 100, query: vi.fn().mockReturnValue([]) };
    vi.clearAllMocks();

    parseConfig.mockResolvedValue({
      project: { name: 'test-project', version: '1.0.0' },
      ontology: { source: '/test/schema.ttl', format: 'turtle' },
      generation: {
        output_dir: '/test/output',
        rules: [
          {
            name: 'generate-types',
            query: 'SELECT ?class WHERE { ?class a owl:Class }',
            template: '/test/templates/types.njk',
            output_file: 'types.mjs',
            enabled: true,
          },
          {
            name: 'generate-schemas',
            query: 'SELECT ?schema WHERE { ?schema a rdfs:Schema }',
            template: '/test/templates/schemas.njk',
            output_file: 'schemas.mjs',
            enabled: true,
          },
        ],
      },
    });

    loadOntology.mockResolvedValue({
      store: mockStore,
      tripleCount: 150,
      prefixes: { rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#' },
    });

    executeSparqlQuery.mockResolvedValue([
      { '?class': 'http://example.org/Person' },
      { '?class': 'http://example.org/Organization' },
    ]);

    renderTemplate.mockResolvedValue({
      content: '// Generated code\nexport const types = {};',
      outputPath: 'types.mjs',
    });
  });

  afterEach(() => {
    console.log = originalLog;
    console.error = originalError;
  });

  it('should process config and generate files successfully with full metrics', async () => {
    const options = { config: '/test/unrdf.toml' };
    const result = await runSync(options);

    expect(result.success).toBe(true);
    expect(result.metrics.rulesProcessed).toBe(2);
    expect(result.metrics.filesGenerated).toBe(2);
    expect(result.metrics.errors).toBe(0);
    expect(result.metrics.totalTriples).toBe(150);
    expect(result.totalDuration).toBeGreaterThanOrEqual(0);

    expect(parseConfig).toHaveBeenCalledWith('/test/unrdf.toml');
    expect(loadOntology).toHaveBeenCalled();
    expect(executeSparqlQuery).toHaveBeenCalledTimes(2);
    expect(renderTemplate).toHaveBeenCalledTimes(2);
    expect(mkdir).toHaveBeenCalled();
    expect(writeFile).toHaveBeenCalledTimes(2);

    const output = logOutput.join('\n');
    expect(output).toContain('UNRDF Sync');
    expect(output).toContain('150');
    expect(output).toContain('Sync complete');
  });

  it('should handle config errors, individual rule failures, and template errors', async () => {
    // Test 1: config file missing
    parseConfig.mockRejectedValue(new Error('Configuration file not found'));
    let result = await runSync({ config: '/missing/unrdf.toml' });
    expect(result.success).toBe(false);
    expect(result.error).toContain('Configuration file not found');

    // Reset mocks for remaining tests
    parseConfig.mockResolvedValue({
      project: { name: 'test-project' },
      ontology: { source: '/test/schema.ttl', format: 'turtle' },
      generation: {
        output_dir: '/test/output',
        rules: [
          {
            name: 'rule1',
            query: 'SELECT ?x WHERE { ?x a ?y }',
            template: '/test/t.njk',
            output_file: 'o.mjs',
            enabled: true,
          },
          {
            name: 'rule2',
            query: 'SELECT ?x WHERE { ?x a ?y }',
            template: '/test/t.njk',
            output_file: 'o2.mjs',
            enabled: true,
          },
        ],
      },
    });
    loadOntology.mockResolvedValue({ store: mockStore, tripleCount: 0, prefixes: {} });
    renderTemplate.mockResolvedValue({ content: 'x', outputPath: 'o.mjs' });

    // Test 2: individual rule failure (first fails, second succeeds)
    executeSparqlQuery
      .mockRejectedValueOnce(new Error('SPARQL syntax error'))
      .mockResolvedValueOnce([{ '?schema': 'http://example.org/Schema1' }]);

    result = await runSync({ config: '/test/unrdf.toml' });
    expect(result.success).toBe(false);
    expect(result.metrics.rulesProcessed).toBe(2);
    expect(result.metrics.filesGenerated).toBe(1);
    expect(result.metrics.errors).toBe(1);
    expect(result.results[0].status).toBe('error');
    expect(result.results[1].status).toBe('success');

    // Test 3: template rendering failures
    renderTemplate.mockRejectedValue(new Error('Template not found'));
    result = await runSync({ config: '/test/unrdf.toml' });
    expect(result.success).toBe(false);
    expect(result.metrics.errors).toBe(2);
    expect(result.results.every(r => r.status === 'error')).toBe(true);
  });

  it('should handle dry-run, rule filtering, disabled rules, and empty rules', async () => {
    // Test 1: dry-run mode
    let result = await runSync({ config: '/test/unrdf.toml', dryRun: true });
    expect(result.success).toBe(true);
    expect(result.metrics.filesGenerated).toBe(0);
    expect(result.metrics.filesSkipped).toBe(2);
    expect(mkdir).not.toHaveBeenCalled();
    expect(writeFile).not.toHaveBeenCalled();
    const dryRunOutput = logOutput.join('\n');
    expect(dryRunOutput).toContain('[DRY RUN]');

    // Test 2: rule filter
    result = await runSync({ config: '/test/unrdf.toml', rule: 'generate-types' });
    expect(result.metrics.rulesProcessed).toBe(1);
    expect(result.metrics.filesGenerated).toBe(1);
    expect(result.results[0].rule).toBe('generate-types');

    // Test 3: non-existent rule
    result = await runSync({ config: '/test/unrdf.toml', rule: 'non-existent-rule' });
    expect(result.metrics.rulesProcessed).toBe(0);

    // Test 4: disabled rules
    parseConfig.mockResolvedValue({
      project: { name: 'test' },
      ontology: { source: '/test/schema.ttl' },
      generation: {
        output_dir: '/test/output',
        rules: [
          { name: 'enabled-rule', query: 'Q', template: '/t.njk', output_file: 'e.mjs', enabled: true },
          { name: 'disabled-rule', query: 'Q', template: '/t.njk', output_file: 'd.mjs', enabled: false },
        ],
      },
    });
    result = await runSync({ config: '/test/unrdf.toml' });
    expect(result.metrics.rulesProcessed).toBe(1);
    expect(result.results[0].rule).toBe('enabled-rule');

    // Test 5: empty rules
    parseConfig.mockResolvedValue({
      project: { name: 'empty' },
      ontology: { source: '/test/schema.ttl' },
      generation: { output_dir: '/test/output', rules: [] },
    });
    result = await runSync({ config: '/test/unrdf.toml' });
    expect(result.metrics.rulesProcessed).toBe(0);
  });
});
