/**
 * @file Config Parser Tests
 * @module cli/test/sync/config-parser
 * @description Tests for `unrdf.toml` configuration parser
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { writeFile, mkdir, rm } from 'fs/promises';
import { join } from 'path';
import { tmpdir } from 'os';
import {
  parseConfig,
  resolveConfigPaths,
  substituteEnvVars,
  validateConfig,
  createDefaultConfig,
  serializeConfig,
  findConfigFile,
  ConfigParseError,
  ConfigValidationError,
} from '../../src/cli/commands/sync/config-parser.mjs';

describe('Config Parser', () => {
  let testDir;

  beforeEach(async () => {
    testDir = join(tmpdir(), `config-parser-test-${Date.now()}`);
    await mkdir(testDir, { recursive: true });
  });

  afterEach(async () => {
    await rm(testDir, { recursive: true, force: true });
  });

  it('should parse valid config, resolve paths, auto-detect format, and parse array tables', async () => {
    // Valid config with all major sections
    const configContent = `
[project]
name = "test-project"
version = "1.0.0"

[ontology]
source = "schema/domain.ttl"

[generation]
output_dir = "src/"

[[generation.rules]]
name = "rule1"
query = "SELECT * WHERE { ?s ?p ?o }"
template = "template1.tera"
output_file = "out1.mjs"

[[generation.rules]]
name = "rule2"
query = "SELECT * WHERE { ?x a ?type }"
template = "template2.tera"
output_file = "out2.mjs"
`;
    const configPath = join(testDir, 'unrdf.toml');
    await writeFile(configPath, configContent);

    const config = await parseConfig(configPath);

    expect(config.project.name).toBe('test-project');
    expect(config.ontology.format).toBe('turtle'); // auto-detected from .ttl
    expect(config.ontology.source).toBe(join(testDir, 'schema/domain.ttl')); // resolved
    expect(config.generation.output_dir).toBe(join(testDir, 'src')); // resolved
    expect(config.generation.rules).toHaveLength(2);
    expect(config.generation.rules[0].name).toBe('rule1');
    expect(config.generation.rules[1].name).toBe('rule2');
  });

  it('should handle missing files, invalid formats, missing source, and invalid base_iri', async () => {
    // Missing file
    await expect(parseConfig(join(testDir, 'nonexistent.toml'))).rejects.toThrow('not found');

    // Invalid format
    const invalidFormatPath = join(testDir, 'bad-format.toml');
    await writeFile(invalidFormatPath, '[ontology]\nsource = "schema.ttl"\nformat = "invalid-format"');
    await expect(parseConfig(invalidFormatPath)).rejects.toThrow();

    // Missing ontology.source
    const noSourcePath = join(testDir, 'no-source.toml');
    await writeFile(noSourcePath, '[ontology]\nformat = "turtle"');
    await expect(parseConfig(noSourcePath)).rejects.toThrow();

    // Invalid base_iri
    const badUriPath = join(testDir, 'bad-uri.toml');
    await writeFile(badUriPath, '[ontology]\nsource = "schema.ttl"\nbase_iri = "not-a-valid-url"');
    await expect(parseConfig(badUriPath)).rejects.toThrow();
  });

  it('should substitute env vars, serialize/deserialize config, and validate config', async () => {
    // substituteEnvVars with ${VAR:-default} syntax
    const origEnv = process.env;
    process.env = { ...origEnv };
    try {
      process.env.PROJECT_NAME = 'my-project';
      const result = substituteEnvVars({
        project: { name: '${MISSING_VAR:-default-name}', version: '${PROJECT_NAME:-default-version}' },
        generation: { parallel: true },
      });
      expect(result.project.name).toBe('default-name');
      expect(result.project.version).toBe('my-project');
      expect(result.generation.parallel).toBe(true); // non-strings untouched
    } finally {
      process.env = origEnv;
    }

    // serializeConfig round-trip
    const config = { project: { name: 'test', version: '1.0.0' }, generation: { incremental: true } };
    const toml = serializeConfig(config);
    expect(toml).toContain('[project]');
    expect(toml).toContain('name = "test"');
    expect(toml).toContain('incremental = true');

    // validateConfig success
    const validationResult = validateConfig({ project: { name: 'test' }, ontology: { source: 'schema.ttl' } });
    expect(validationResult.success).toBe(true);

    // findConfigFile
    await writeFile(join(testDir, 'unrdf.toml'), '[project]\nname = "test"');
    const found = await findConfigFile(testDir);
    expect(found).toContain('unrdf.toml');
  });
});
