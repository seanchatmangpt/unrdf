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
    // Capture console output
    logOutput = [];
    errorOutput = [];
    originalLog = console.log;
    originalError = console.error;

    console.log = vi.fn((...args) => {
      logOutput.push(args.join(' '));
    });

    console.error = vi.fn((...args) => {
      errorOutput.push(args.join(' '));
    });

    // Setup default mock store
    mockStore = {
      size: 100,
      query: vi.fn().mockReturnValue([]),
    };

    // Reset all mocks
    vi.clearAllMocks();

    // Setup default mock implementations
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

  describe('runSync()', () => {
    it('should process config and generate files successfully', async () => {
      // Arrange
      const options = { config: '/test/ggen.toml' };

      // Act
      const result = await runSync(options);

      // Assert
      expect(result.success).toBe(true);
      expect(result.metrics.rulesProcessed).toBe(2);
      expect(result.metrics.filesGenerated).toBe(2);
      expect(result.metrics.errors).toBe(0);

      // Verify dependencies were called
      expect(parseConfig).toHaveBeenCalledWith('/test/ggen.toml');
      expect(loadOntology).toHaveBeenCalled();
      expect(executeSparqlQuery).toHaveBeenCalledTimes(2);
      expect(renderTemplate).toHaveBeenCalledTimes(2);
      expect(mkdir).toHaveBeenCalled();
      expect(writeFile).toHaveBeenCalledTimes(2);

      // Verify output messages
      const output = logOutput.join('\n');
      expect(output).toContain('UNRDF Sync');
      expect(output).toContain('Loading configuration');
      expect(output).toContain('Loading ontology');
      expect(output).toContain('150');
      expect(output).toContain('Sync complete');
    });

    it('should not write files in dry-run mode', async () => {
      // Arrange
      const options = { config: '/test/ggen.toml', dryRun: true };

      // Act
      const result = await runSync(options);

      // Assert
      expect(result.success).toBe(true);
      expect(result.metrics.rulesProcessed).toBe(2);
      expect(result.metrics.filesGenerated).toBe(0);
      expect(result.metrics.filesSkipped).toBe(2);

      // Verify mkdir and writeFile were NOT called
      expect(mkdir).not.toHaveBeenCalled();
      expect(writeFile).not.toHaveBeenCalled();

      // Verify dry-run message in output
      const output = logOutput.join('\n');
      expect(output).toContain('[DRY RUN]');
      expect(output).toContain('Would write');

      // Verify results have dry-run status
      expect(result.results.every(r => r.status === 'dry-run')).toBe(true);
    });

    it('should output detailed information in verbose mode', async () => {
      // Arrange
      const options = { config: '/test/ggen.toml', verbose: true };

      // Act
      const result = await runSync(options);

      // Assert
      expect(result.success).toBe(true);

      // Verify verbose output
      const output = logOutput.join('\n');
      expect(output).toContain('Config:');
      expect(output).toContain('/test/ggen.toml');
      expect(output).toContain('Project:');
      expect(output).toContain('test-project');
      expect(output).toContain('Rule:');
      expect(output).toContain('generate-types');
      expect(output).toContain('Query returned');
      expect(output).toContain('2 results');
    });

    it('should process only the specified rule when filter is provided', async () => {
      // Arrange
      const options = { config: '/test/ggen.toml', rule: 'generate-types' };

      // Act
      const result = await runSync(options);

      // Assert
      expect(result.success).toBe(true);
      expect(result.metrics.rulesProcessed).toBe(1);
      expect(result.metrics.filesGenerated).toBe(1);

      // Verify only one rule was processed
      expect(executeSparqlQuery).toHaveBeenCalledTimes(1);
      expect(renderTemplate).toHaveBeenCalledTimes(1);
      expect(writeFile).toHaveBeenCalledTimes(1);

      // Verify correct rule was processed
      expect(result.results).toHaveLength(1);
      expect(result.results[0].rule).toBe('generate-types');
    });

    it('should handle non-existent rule filter gracefully', async () => {
      // Arrange
      const options = { config: '/test/ggen.toml', rule: 'non-existent-rule' };

      // Act
      const result = await runSync(options);

      // Assert
      expect(result.success).toBe(true);
      expect(result.metrics.rulesProcessed).toBe(0);
      expect(result.metrics.filesGenerated).toBe(0);

      // Verify no SPARQL queries or templates were processed
      expect(executeSparqlQuery).not.toHaveBeenCalled();
      expect(renderTemplate).not.toHaveBeenCalled();

      // Verify output indicates no rules
      const output = logOutput.join('\n');
      expect(output).toContain('No rules to process');
    });

    it('should skip disabled rules when no filter is provided', async () => {
      // Arrange
      parseConfig.mockResolvedValue({
        project: { name: 'test-project' },
        ontology: { source: '/test/schema.ttl', format: 'turtle' },
        generation: {
          output_dir: '/test/output',
          rules: [
            {
              name: 'enabled-rule',
              query: 'SELECT ?x WHERE { ?x a ?y }',
              template: '/test/enabled.njk',
              output_file: 'enabled.mjs',
              enabled: true,
            },
            {
              name: 'disabled-rule',
              query: 'SELECT ?x WHERE { ?x a ?y }',
              template: '/test/disabled.njk',
              output_file: 'disabled.mjs',
              enabled: false,
            },
          ],
        },
      });

      const options = { config: '/test/ggen.toml' };

      // Act
      const result = await runSync(options);

      // Assert
      expect(result.metrics.rulesProcessed).toBe(1);
      expect(result.results).toHaveLength(1);
      expect(result.results[0].rule).toBe('enabled-rule');
    });
  });

  describe('error handling', () => {
    it('should return error when config file is missing', async () => {
      // Arrange
      parseConfig.mockRejectedValue(new Error('Configuration file not found: /missing/ggen.toml'));
      const options = { config: '/missing/ggen.toml' };

      // Act
      const result = await runSync(options);

      // Assert
      expect(result.success).toBe(false);
      expect(result.error).toContain('Configuration file not found');

      // Verify error output
      const errors = errorOutput.join('\n');
      expect(errors).toContain('Sync failed');
      expect(errors).toContain('Configuration file not found');
    });

    it('should return error when ontology loading fails', async () => {
      // Arrange
      loadOntology.mockRejectedValue(new Error('Ontology file not found: /test/schema.ttl'));
      const options = { config: '/test/ggen.toml' };

      // Act
      const result = await runSync(options);

      // Assert
      expect(result.success).toBe(false);
      expect(result.error).toContain('Ontology file not found');
    });

    it('should continue processing and track errors for individual rule failures', async () => {
      // Arrange - first rule fails, second succeeds
      executeSparqlQuery
        .mockRejectedValueOnce(new Error('SPARQL syntax error'))
        .mockResolvedValueOnce([{ '?schema': 'http://example.org/Schema1' }]);

      const options = { config: '/test/ggen.toml' };

      // Act
      const result = await runSync(options);

      // Assert
      expect(result.success).toBe(false); // Has errors
      expect(result.metrics.rulesProcessed).toBe(2);
      expect(result.metrics.filesGenerated).toBe(1);
      expect(result.metrics.errors).toBe(1);

      // Verify error was recorded
      expect(result.results[0].status).toBe('error');
      expect(result.results[0].error).toContain('SPARQL');
      expect(result.results[1].status).toBe('success');

      // Verify error output
      const output = logOutput.join('\n');
      expect(output).toContain('ERR');
      expect(output).toContain('SPARQL');
    });

    it('should handle template rendering failures', async () => {
      // Arrange
      renderTemplate.mockRejectedValue(new Error('Template not found'));
      const options = { config: '/test/ggen.toml' };

      // Act
      const result = await runSync(options);

      // Assert
      expect(result.success).toBe(false);
      expect(result.metrics.errors).toBe(2);
      expect(result.results.every(r => r.status === 'error')).toBe(true);
    });

    it('should show stack trace in verbose mode on error', async () => {
      // Arrange
      const errorWithStack = new Error('Test error');
      errorWithStack.stack = 'Error: Test error\n    at TestFunction (test.mjs:10:5)';
      parseConfig.mockRejectedValue(errorWithStack);

      const options = { config: '/test/ggen.toml', verbose: true };

      // Act
      await runSync(options);

      // Assert
      const errors = errorOutput.join('\n');
      expect(errors).toContain('at TestFunction');
    });
  });

  describe('metrics tracking', () => {
    it('should track rulesProcessed count correctly', async () => {
      // Arrange
      const options = { config: '/test/ggen.toml' };

      // Act
      const result = await runSync(options);

      // Assert
      expect(result.metrics.rulesProcessed).toBe(2);

      // Verify output shows rules processed
      const output = logOutput.join('\n');
      expect(output).toContain('Rules processed: 2');
    });

    it('should track filesGenerated count correctly', async () => {
      // Arrange
      const options = { config: '/test/ggen.toml' };

      // Act
      const result = await runSync(options);

      // Assert
      expect(result.metrics.filesGenerated).toBe(2);

      // Verify output shows files generated
      const output = logOutput.join('\n');
      expect(output).toContain('Files generated: 2');
    });

    it('should track totalBytes generated', async () => {
      // Arrange
      renderTemplate.mockResolvedValue({
        content: 'x'.repeat(1000), // 1000 bytes
        outputPath: 'test.mjs',
      });
      const options = { config: '/test/ggen.toml' };

      // Act
      const result = await runSync(options);

      // Assert
      expect(result.metrics.totalBytes).toBe(2000); // 2 rules * 1000 bytes
      expect(result.results[0].bytes).toBe(1000);
      expect(result.results[1].bytes).toBe(1000);
    });

    it('should track totalTriples from ontology', async () => {
      // Arrange
      loadOntology.mockResolvedValue({
        store: mockStore,
        tripleCount: 500,
        prefixes: {},
      });
      const options = { config: '/test/ggen.toml' };

      // Act
      const result = await runSync(options);

      // Assert
      expect(result.metrics.totalTriples).toBe(500);

      // Verify output shows triple count
      const output = logOutput.join('\n');
      expect(output).toContain('500');
      expect(output).toContain('triples');
    });

    it('should track filesSkipped in dry-run mode', async () => {
      // Arrange
      const options = { config: '/test/ggen.toml', dryRun: true };

      // Act
      const result = await runSync(options);

      // Assert
      expect(result.metrics.filesSkipped).toBe(2);
      expect(result.metrics.filesGenerated).toBe(0);
    });

    it('should track errors count correctly', async () => {
      // Arrange - both rules fail
      executeSparqlQuery.mockRejectedValue(new Error('Query failed'));
      const options = { config: '/test/ggen.toml' };

      // Act
      const result = await runSync(options);

      // Assert
      expect(result.metrics.errors).toBe(2);

      // Verify error count in output
      const output = logOutput.join('\n');
      expect(output).toContain('Errors: 2');
    });

    it('should track duration for each rule', async () => {
      // Arrange
      const options = { config: '/test/ggen.toml' };

      // Act
      const result = await runSync(options);

      // Assert
      expect(result.results[0]).toHaveProperty('duration');
      expect(result.results[1]).toHaveProperty('duration');
      expect(typeof result.results[0].duration).toBe('number');
      expect(result.results[0].duration).toBeGreaterThanOrEqual(0);
    });

    it('should track totalDuration for entire sync', async () => {
      // Arrange
      const options = { config: '/test/ggen.toml' };

      // Act
      const result = await runSync(options);

      // Assert
      expect(result).toHaveProperty('totalDuration');
      expect(typeof result.totalDuration).toBe('number');
      expect(result.totalDuration).toBeGreaterThanOrEqual(0);

      // Verify duration in output
      const output = logOutput.join('\n');
      expect(output).toContain('Duration:');
    });
  });

  describe('output formats', () => {
    it('should output JSON when output option is json', async () => {
      // Arrange
      const options = { config: '/test/ggen.toml', output: 'json' };

      // Act
      const result = await runSync(options);

      // Assert
      expect(result.success).toBe(true);

      // Find JSON output in log
      const jsonOutput = logOutput.find(line => {
        try {
          JSON.parse(line);
          return true;
        } catch {
          return false;
        }
      });

      expect(jsonOutput).toBeDefined();
      const parsed = JSON.parse(jsonOutput);
      expect(parsed.success).toBe(true);
      expect(parsed.results).toHaveLength(2);
      expect(parsed.metrics).toHaveProperty('rulesProcessed', 2);
      expect(parsed.metrics).toHaveProperty('filesGenerated', 2);
    });

    it('should not output JSON when output option is text', async () => {
      // Arrange
      const options = { config: '/test/ggen.toml', output: 'text' };

      // Act
      await runSync(options);

      // Assert - no JSON output
      const jsonOutput = logOutput.find(line => {
        try {
          const parsed = JSON.parse(line);
          return parsed.success !== undefined && parsed.results !== undefined;
        } catch {
          return false;
        }
      });

      expect(jsonOutput).toBeUndefined();
    });
  });

  describe('edge cases', () => {
    it('should handle empty rules array', async () => {
      // Arrange
      parseConfig.mockResolvedValue({
        project: { name: 'empty-project' },
        ontology: { source: '/test/schema.ttl', format: 'turtle' },
        generation: { output_dir: '/test/output', rules: [] },
      });
      const options = { config: '/test/ggen.toml' };

      // Act
      const result = await runSync(options);

      // Assert
      expect(result.success).toBe(true);
      expect(result.metrics.rulesProcessed).toBe(0);
      expect(result.results).toHaveLength(0);

      // Verify output
      const output = logOutput.join('\n');
      expect(output).toContain('No rules to process');
    });

    it('should handle missing generation config', async () => {
      // Arrange
      parseConfig.mockResolvedValue({
        project: { name: 'minimal-project' },
        ontology: { source: '/test/schema.ttl', format: 'turtle' },
        generation: undefined,
      });
      const options = { config: '/test/ggen.toml' };

      // Act
      const result = await runSync(options);

      // Assert
      expect(result.success).toBe(true);
      expect(result.metrics.rulesProcessed).toBe(0);
    });

    it('should use default output_dir when not specified', async () => {
      // Arrange
      parseConfig.mockResolvedValue({
        project: { name: 'default-output-project' },
        ontology: { source: '/test/schema.ttl', format: 'turtle' },
        generation: {
          rules: [
            {
              name: 'test-rule',
              query: 'SELECT ?x WHERE { ?x a ?y }',
              template: '/test/template.njk',
              output_file: 'output.mjs',
              enabled: true,
            },
          ],
        },
      });

      renderTemplate.mockResolvedValue({
        content: '// test',
        outputPath: 'output.mjs',
      });

      const options = { config: '/test/ggen.toml' };

      // Act
      const result = await runSync(options);

      // Assert
      expect(result.success).toBe(true);
      // renderTemplate should receive a default output_dir
      expect(renderTemplate).toHaveBeenCalledWith(
        '/test/template.njk',
        expect.any(Array),
        expect.objectContaining({
          output_dir: expect.any(String),
        })
      );
    });

    it('should handle missing project config', async () => {
      // Arrange
      parseConfig.mockResolvedValue({
        ontology: { source: '/test/schema.ttl', format: 'turtle' },
        generation: { output_dir: '/test/output', rules: [] },
      });
      const options = { config: '/test/ggen.toml', verbose: true };

      // Act
      const result = await runSync(options);

      // Assert
      expect(result.success).toBe(true);

      // Verify verbose output handles missing project
      const output = logOutput.join('\n');
      expect(output).toContain('Project:');
      expect(output).toContain('unnamed');
    });

    it('should use outputPath from template when output_file not specified', async () => {
      // Arrange
      parseConfig.mockResolvedValue({
        project: { name: 'test' },
        ontology: { source: '/test/schema.ttl', format: 'turtle' },
        generation: {
          output_dir: '/test/output',
          rules: [
            {
              name: 'template-path-rule',
              query: 'SELECT ?x WHERE { ?x a ?y }',
              template: '/test/template.njk',
              output_file: 'fallback.mjs',
              enabled: true,
            },
          ],
        },
      });

      renderTemplate.mockResolvedValue({
        content: '// generated',
        outputPath: 'from-template.mjs', // Template specifies output path
      });

      const options = { config: '/test/ggen.toml' };

      // Act
      const result = await runSync(options);

      // Assert
      expect(result.success).toBe(true);
      // writeFile should use the path from template
      expect(writeFile).toHaveBeenCalledWith(
        expect.stringContaining('from-template.mjs'),
        expect.any(String),
        'utf-8'
      );
    });
  });
});
