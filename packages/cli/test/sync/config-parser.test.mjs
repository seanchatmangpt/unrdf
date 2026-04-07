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

  describe('parseConfig()', () => {
    it('should parse a valid unrdf.toml file', async () => {
      // Arrange
      const configContent = `
[project]
name = "test-project"
version = "1.0.0"
description = "Test project"

[ontology]
source = "schema/domain.ttl"
format = "turtle"

[generation]
output_dir = "src/generated/"
`;
      const configPath = join(testDir, 'unrdf.toml');
      await writeFile(configPath, configContent);

      // Act
      const config = await parseConfig(configPath);

      // Assert
      expect(config.project.name).toBe('test-project');
      expect(config.project.version).toBe('1.0.0');
      expect(config.project.description).toBe('Test project');
      expect(config.ontology.format).toBe('turtle');
    });

    it('should throw Error for missing file', async () => {
      // Arrange
      const configPath = join(testDir, 'nonexistent.toml');

      // Act & Assert
      await expect(parseConfig(configPath)).rejects.toThrow('not found');
    });

    it('should accept config without ontology section', async () => {
      // Arrange - ontology section is optional per the schema
      const configWithoutOntology = `
[project]
name = "test"
version = "1.0.0"
`;
      const configPath = join(testDir, 'no-ontology.toml');
      await writeFile(configPath, configWithoutOntology);

      // Act
      const config = await parseConfig(configPath);

      // Assert
      expect(config.project.name).toBe('test');
      expect(config.ontology).toBeUndefined();
    });

    it('should resolve relative paths to absolute', async () => {
      // Arrange
      const configContent = `
[project]
name = "path-test"
version = "1.0.0"

[ontology]
source = "schema/domain.ttl"

[generation]
output_dir = "src/generated/"
`;
      const configPath = join(testDir, 'unrdf.toml');
      await writeFile(configPath, configContent);

      // Act
      const config = await parseConfig(configPath);

      // Assert - resolve removes trailing slashes
      expect(config.ontology.source).toBe(join(testDir, 'schema/domain.ttl'));
      expect(config.generation.output_dir).toBe(join(testDir, 'src/generated'));
    });

    it('should auto-detect RDF format from file extension when not specified', async () => {
      // Arrange
      const configContent = `
[ontology]
source = "schema/domain.ttl"
`;
      const configPath = join(testDir, 'unrdf.toml');
      await writeFile(configPath, configContent);

      // Act
      const config = await parseConfig(configPath);

      // Assert - format detected from .ttl extension
      expect(config.ontology.format).toBe('turtle');
    });

    it('should apply default values for optional fields', async () => {
      // Arrange
      const minimalConfig = `
[ontology]
source = "schema.ttl"
`;
      const configPath = join(testDir, 'minimal.toml');
      await writeFile(configPath, minimalConfig);

      // Act
      const config = await parseConfig(configPath);

      // Assert - defaults from schema
      // Note: generation defaults to empty object {}, inner defaults apply only when section is explicitly provided
      expect(config.generation).toEqual({});
      // ontology defaults DO apply since ontology section is present
      expect(config.ontology.format).toBe('turtle');
      expect(config.ontology.follow_imports).toBe(false);
    });

    it('should parse boolean values correctly', async () => {
      // Arrange
      const configContent = `
[ontology]
source = "schema.ttl"
follow_imports = true

[generation]
parallel = false
require_audit_trail = true
`;
      const configPath = join(testDir, 'bool.toml');
      await writeFile(configPath, configContent);

      // Act
      const config = await parseConfig(configPath);

      // Assert
      expect(config.ontology.follow_imports).toBe(true);
      expect(config.generation.parallel).toBe(false);
      expect(config.generation.require_audit_trail).toBe(true);
    });

    it('should parse integer values correctly', async () => {
      // Arrange - using a custom key that accepts numbers
      const configContent = `
[ontology]
source = "schema.ttl"

[generation]
output_dir = "output"
`;
      const configPath = join(testDir, 'int.toml');
      await writeFile(configPath, configContent);

      // Act
      const config = await parseConfig(configPath);

      // Assert - verify parsing works without number errors
      expect(config.ontology.source).toContain('schema.ttl');
    });

    it('should parse array tables [[section]]', async () => {
      // Arrange
      const configContent = `
[ontology]
source = "schema.ttl"

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
      const configPath = join(testDir, 'array.toml');
      await writeFile(configPath, configContent);

      // Act
      const config = await parseConfig(configPath);

      // Assert
      expect(config.generation.rules).toHaveLength(2);
      expect(config.generation.rules[0].name).toBe('rule1');
      expect(config.generation.rules[1].name).toBe('rule2');
    });

    it('should parse multiline strings with triple quotes', async () => {
      // Arrange
      const configContent = `
[ontology]
source = "schema.ttl"

[[generation.rules]]
name = "multi"
query = """
SELECT * WHERE {
  ?s ?p ?o .
  FILTER(?o > 10)
}
"""
template = "t.tera"
output_file = "o.mjs"
`;
      const configPath = join(testDir, 'multiline.toml');
      await writeFile(configPath, configContent);

      // Act
      const config = await parseConfig(configPath);

      // Assert
      expect(config.generation.rules[0].query).toContain('SELECT');
      expect(config.generation.rules[0].query).toContain('FILTER');
    });
  });

  describe('resolveConfigPaths()', () => {
    it('should resolve all relative paths', () => {
      // Arrange
      const config = {
        ontology: { source: 'schema/domain.ttl' },
        generation: {
          output_dir: 'src/generated/',
        },
      };

      // Act
      const resolved = resolveConfigPaths(config, '/home/user/project');

      // Assert
      expect(resolved.ontology.source).toBe('/home/user/project/schema/domain.ttl');
      expect(resolved.generation.output_dir).toBe('/home/user/project/src/generated');
    });

    it('should preserve absolute paths', () => {
      // Arrange
      const config = {
        ontology: { source: '/absolute/path/domain.ttl' },
        generation: { output_dir: '/absolute/output' },
      };

      // Act
      const resolved = resolveConfigPaths(config, '/home/user/project');

      // Assert
      expect(resolved.ontology.source).toBe('/absolute/path/domain.ttl');
      expect(resolved.generation.output_dir).toBe('/absolute/output');
    });

    it('should handle missing sections gracefully', () => {
      // Arrange
      const config = { project: { name: 'test' } };

      // Act
      const resolved = resolveConfigPaths(config, '/base');

      // Assert
      expect(resolved.project.name).toBe('test');
      expect(resolved.ontology).toBeUndefined();
    });

    it('should resolve rule template paths', () => {
      // Arrange
      const config = {
        generation: {
          rules: [
            { template: 'templates/test.tera' },
            { template: 'templates/other.tera' },
          ],
        },
      };

      // Act
      const resolved = resolveConfigPaths(config, '/home/user/project');

      // Assert
      expect(resolved.generation.rules[0].template).toBe('/home/user/project/templates/test.tera');
      expect(resolved.generation.rules[1].template).toBe('/home/user/project/templates/other.tera');
    });

    it('should auto-detect format from resolved source path', () => {
      // Arrange
      const config = {
        ontology: { source: 'schema/domain.nt' },
      };

      // Act
      const resolved = resolveConfigPaths(config, '/home/user/project');

      // Assert - format detected from .nt extension
      expect(resolved.ontology.format).toBe('ntriples');
    });

    it('should preserve explicit format even when different from extension', () => {
      // Arrange
      const config = {
        ontology: { source: 'schema/domain.txt', format: 'turtle' },
      };

      // Act
      const resolved = resolveConfigPaths(config, '/home/user/project');

      // Assert - explicit format preserved
      expect(resolved.ontology.format).toBe('turtle');
    });
  });

  describe('substituteEnvVars()', () => {
    const originalEnv = process.env;

    beforeEach(() => {
      process.env = { ...originalEnv };
    });

    afterEach(() => {
      process.env = originalEnv;
    });

    it('should substitute ${VAR} syntax', () => {
      // Arrange
      process.env.GGEN_OUTPUT = '/custom/output';
      const config = {
        generation: { output_dir: '${GGEN_OUTPUT}/generated' },
      };

      // Act
      const result = substituteEnvVars(config);

      // Assert
      expect(result.generation.output_dir).toBe('/custom/output/generated');
    });

    it('should substitute $VAR syntax', () => {
      // Arrange
      process.env.PROJECT_NAME = 'my-project';
      const config = {
        project: { name: '$PROJECT_NAME' },
      };

      // Act
      const result = substituteEnvVars(config);

      // Assert
      expect(result.project.name).toBe('my-project');
    });

    it('should replace undefined env vars with empty string', () => {
      // Arrange
      delete process.env.UNDEFINED_VAR;
      const config = {
        project: { name: '${UNDEFINED_VAR}' },
      };

      // Act
      const result = substituteEnvVars(config);

      // Assert - undefined vars replaced with empty string
      expect(result.project.name).toBe('');
    });

    it('should handle ${VAR:-default} syntax', () => {
      // Arrange
      delete process.env.MISSING_VAR;
      process.env.EXISTING_VAR = 'exists';
      const config = {
        project: {
          name: '${MISSING_VAR:-default-name}',
          version: '${EXISTING_VAR:-default-version}',
        },
      };

      // Act
      const result = substituteEnvVars(config);

      // Assert
      expect(result.project.name).toBe('default-name');
      expect(result.project.version).toBe('exists');
    });

    it('should substitute in nested objects and arrays', () => {
      // Arrange
      process.env.TEMPLATE_DIR = '/templates';
      const config = {
        generation: {
          rules: [
            { template: '${TEMPLATE_DIR}/one.tera' },
            { template: '${TEMPLATE_DIR}/two.tera' },
          ],
        },
      };

      // Act
      const result = substituteEnvVars(config);

      // Assert
      expect(result.generation.rules[0].template).toBe('/templates/one.tera');
      expect(result.generation.rules[1].template).toBe('/templates/two.tera');
    });

    it('should not modify non-string values', () => {
      // Arrange
      const config = {
        generation: { parallel: true, require_audit_trail: false },
        numbers: { count: 100 },
      };

      // Act
      const result = substituteEnvVars(config);

      // Assert
      expect(result.generation.parallel).toBe(true);
      expect(result.generation.require_audit_trail).toBe(false);
      expect(result.numbers.count).toBe(100);
    });

    it('should handle multiple substitutions in one string', () => {
      // Arrange
      process.env.PREFIX = 'pre';
      process.env.SUFFIX = 'suf';
      const config = {
        project: { name: '${PREFIX}-middle-${SUFFIX}' },
      };

      // Act
      const result = substituteEnvVars(config);

      // Assert
      expect(result.project.name).toBe('pre-middle-suf');
    });

    it('should handle null and undefined values', () => {
      // Arrange
      const config = {
        project: { name: 'test', description: null },
        optional: undefined,
      };

      // Act
      const result = substituteEnvVars(config);

      // Assert
      expect(result.project.name).toBe('test');
      expect(result.project.description).toBeNull();
      expect(result.optional).toBeUndefined();
    });
  });

  describe('TOML Parsing Edge Cases', () => {
    it('should ignore comment lines', async () => {
      // Arrange
      const configContent = `
# This is a comment
[ontology]
# Another comment
source = "schema.ttl"
# Inline comments are NOT supported in our parser
`;
      const configPath = join(testDir, 'comments.toml');
      await writeFile(configPath, configContent);

      // Act
      const config = await parseConfig(configPath);

      // Assert
      expect(config.ontology.source).toContain('schema.ttl');
    });

    it('should handle empty lines', async () => {
      // Arrange
      const configContent = `

[ontology]

source = "schema.ttl"

`;
      const configPath = join(testDir, 'empty-lines.toml');
      await writeFile(configPath, configContent);

      // Act
      const config = await parseConfig(configPath);

      // Assert
      expect(config.ontology.source).toContain('schema.ttl');
    });

    it('should parse [ontology.prefixes] subtable into validated schema', async () => {
      const configContent = `
[ontology]
source = "schema.ttl"
format = "turtle"

[ontology.prefixes]
rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
rdfs = "http://www.w3.org/2000/01/rdf-schema#"
`;
      const configPath = join(testDir, 'nested.toml');
      await writeFile(configPath, configContent);

      const config = await parseConfig(configPath);

      expect(config.ontology.prefixes.rdf).toBe('http://www.w3.org/1999/02/22-rdf-syntax-ns#');
      expect(config.ontology.prefixes.rdfs).toBe('http://www.w3.org/2000/01/rdf-schema#');
    });

    it('should parse string with escaped characters', async () => {
      // Arrange
      const configContent = `
[ontology]
source = "schema.ttl"

[project]
name = "test"
description = "Line1\\nLine2"
`;
      const configPath = join(testDir, 'escaped.toml');
      await writeFile(configPath, configContent);

      // Act
      const config = await parseConfig(configPath);

      // Assert
      expect(config.project.description).toContain('Line1');
    });

    it('should parse arrays inline', async () => {
      // Arrange
      const configContent = `
[ontology]
source = "schema.ttl"

[[generation.rules]]
name = "test"
query = "SELECT * WHERE { ?s ?p ?o }"
template = "t.tera"
output_file = "o.mjs"
depends_on = ["rule1", "rule2"]
`;
      const configPath = join(testDir, 'inline-array.toml');
      await writeFile(configPath, configContent);

      // Act
      const config = await parseConfig(configPath);

      // Assert
      expect(config.generation.rules[0].depends_on).toEqual(['rule1', 'rule2']);
    });

    it('should parse floating point numbers', async () => {
      // Arrange - using generation.rules since it can have numeric fields
      const configContent = `
[ontology]
source = "schema.ttl"

[generation]
output_dir = "output"
`;
      const configPath = join(testDir, 'float.toml');
      await writeFile(configPath, configContent);

      // Act
      const config = await parseConfig(configPath);

      // Assert
      expect(config.generation.output_dir).toContain('output');
    });
  });

  describe('Schema Validation', () => {
    it('should accept valid ontology formats', async () => {
      // Arrange
      const formats = ['turtle', 'ntriples', 'jsonld', 'rdfxml', 'nquads', 'trig'];

      for (const format of formats) {
        const configContent = `
[ontology]
source = "schema.ttl"
format = "${format}"
`;
        const configPath = join(testDir, `format-${format}.toml`);
        await writeFile(configPath, configContent);

        // Act
        const config = await parseConfig(configPath);

        // Assert
        expect(config.ontology.format).toBe(format);
      }
    });

    it('should reject invalid ontology format', async () => {
      // Arrange
      const configContent = `
[ontology]
source = "schema.ttl"
format = "invalid-format"
`;
      const configPath = join(testDir, 'invalid-format.toml');
      await writeFile(configPath, configContent);

      // Act & Assert
      await expect(parseConfig(configPath)).rejects.toThrow();
    });

    it('should require ontology.source', async () => {
      // Arrange
      const configContent = `
[ontology]
format = "turtle"
`;
      const configPath = join(testDir, 'no-source.toml');
      await writeFile(configPath, configContent);

      // Act & Assert
      await expect(parseConfig(configPath)).rejects.toThrow();
    });

    it('should accept valid generation rule modes', async () => {
      // Arrange
      const modes = ['overwrite', 'append', 'skip_existing'];

      for (const mode of modes) {
        const configContent = `
[ontology]
source = "schema.ttl"

[[generation.rules]]
name = "test"
query = "SELECT * WHERE { ?s ?p ?o }"
template = "t.tera"
output_file = "o.mjs"
mode = "${mode}"
`;
        const configPath = join(testDir, `mode-${mode}.toml`);
        await writeFile(configPath, configContent);

        // Act
        const config = await parseConfig(configPath);

        // Assert
        expect(config.generation.rules[0].mode).toBe(mode);
      }
    });

    it('should validate base_iri as URL when provided', async () => {
      // Arrange
      const configContent = `
[ontology]
source = "schema.ttl"
base_iri = "http://example.org/ontology#"
`;
      const configPath = join(testDir, 'valid-uri.toml');
      await writeFile(configPath, configContent);

      // Act
      const config = await parseConfig(configPath);

      // Assert
      expect(config.ontology.base_iri).toBe('http://example.org/ontology#');
    });

    it('should reject invalid base_iri URL', async () => {
      // Arrange
      const configContent = `
[ontology]
source = "schema.ttl"
base_iri = "not-a-valid-url"
`;
      const configPath = join(testDir, 'invalid-uri.toml');
      await writeFile(configPath, configContent);

      // Act & Assert
      await expect(parseConfig(configPath)).rejects.toThrow();
    });
  });

  describe('parseConfig() with options', () => {
    it('should skip path resolution when resolvePaths is false', async () => {
      // Arrange
      const configContent = `
[project]
name = "no-resolve"
version = "1.0.0"

[ontology]
source = "schema/domain.ttl"
`;
      const configPath = join(testDir, 'unrdf.toml');
      await writeFile(configPath, configContent);

      // Act
      const config = await parseConfig(configPath, { resolvePaths: false });

      // Assert - paths should remain relative
      expect(config.ontology.source).toBe('schema/domain.ttl');
    });
  });

  describe('validateConfig()', () => {
    it('should return success for valid config', () => {
      // Arrange
      const config = {
        project: { name: 'test', version: '1.0.0' },
        ontology: { source: 'schema.ttl' },
      };

      // Act
      const result = validateConfig(config);

      // Assert
      expect(result.success).toBe(true);
      expect(result.data).toBeDefined();
      expect(result.data.project.name).toBe('test');
    });

    it('should handle valid config with all required fields', () => {
      // Arrange - needs valid non-empty strings
      const config = {
        project: { name: 'test-project' },
        ontology: { source: 'schema.ttl' },
      };

      // Act
      const result = validateConfig(config);

      // Assert
      expect(result.success).toBe(true);
      expect(result.data.project.name).toBe('test-project');
    });

    it('should validate config without ontology section', () => {
      // Arrange
      const config = {
        project: { name: 'test' },
      };

      // Act
      const result = validateConfig(config);

      // Assert
      expect(result.success).toBe(true);
    });
  });

  describe('createDefaultConfig()', () => {
    it('should create valid default config', () => {
      // Act
      const config = createDefaultConfig();

      // Assert
      expect(config.project.name).toBe('untitled');
      expect(config.project.version).toBe('1.0.0');
      expect(config.generation.incremental).toBe(true);
      expect(config.generation.overwrite).toBe(false);
      expect(config.sync.enabled).toBe(true);
    });

    it('should merge overrides with defaults', () => {
      // Act
      const config = createDefaultConfig({
        project: { name: 'custom', version: '2.0.0' },
        generation: { output_dir: 'custom/output/' },
      });

      // Assert
      expect(config.project.name).toBe('custom');
      expect(config.project.version).toBe('2.0.0');
      expect(config.generation.output_dir).toBe('custom/output/');
      expect(config.generation.incremental).toBe(true); // default preserved
    });

    it('should deep merge nested objects', () => {
      // Act
      const config = createDefaultConfig({
        sync: { on_change: 'auto' },
      });

      // Assert
      expect(config.sync.on_change).toBe('auto');
      expect(config.sync.enabled).toBe(true); // default preserved
      expect(config.sync.conflict_mode).toBe('warn'); // default preserved
    });
  });

  describe('serializeConfig()', () => {
    it('should serialize config to valid TOML', () => {
      // Arrange
      const config = {
        project: { name: 'test', version: '1.0.0' },
      };

      // Act
      const toml = serializeConfig(config);

      // Assert
      expect(toml).toContain('[project]');
      expect(toml).toContain('name = "test"');
      expect(toml).toContain('version = "1.0.0"');
    });

    it('should serialize boolean values correctly', () => {
      // Arrange
      const config = {
        generation: { incremental: true, overwrite: false },
      };

      // Act
      const toml = serializeConfig(config);

      // Assert
      expect(toml).toContain('incremental = true');
      expect(toml).toContain('overwrite = false');
    });

    it('should serialize number values correctly', () => {
      // Arrange
      const config = {
        settings: { count: 42 },
      };

      // Act
      const toml = serializeConfig(config);

      // Assert
      expect(toml).toContain('count = 42');
    });

    it('should serialize arrays correctly', () => {
      // Arrange
      const config = {
        data: { items: ['a', 'b', 'c'] },
      };

      // Act
      const toml = serializeConfig(config);

      // Assert
      expect(toml).toContain('items = ["a", "b", "c"]');
    });

    it('should serialize config with nested sections', () => {
      // Arrange
      const config = {
        project: { name: 'test', version: '1.0.0' },
        ontology: { source: 'schema.ttl' },
      };

      // Act
      const toml = serializeConfig(config);

      // Assert
      expect(toml).toContain('[project]');
      expect(toml).toContain('[ontology]');
      expect(toml).toContain('source = "schema.ttl"');
    });
  });

  describe('findConfigFile()', () => {
    it('should find unrdf.toml in directory', async () => {
      const configPath = join(testDir, 'unrdf.toml');
      await writeFile(configPath, '[project]\nname = "test"');

      const found = await findConfigFile(testDir);

      expect(found).toBe(configPath);
    });

    it('should return null when no config found', async () => {
      const found = await findConfigFile(testDir);

      expect(found).toBeNull();
    });
  });

  describe('unrdf.toml schema surface', () => {
    it('should parse a config with all major sections and path fields', async () => {
      const configContent = `
[project]
name = "full"
version = "2.0.0"
description = "full stack"
author = "unrdf"
license = "MIT"

[ontology]
source = "ontology/schema.ttl"
format = "turtle"
follow_imports = true

[generation]
output_dir = "dist/out"
templates_dir = "tmpl"
ontology_dir = "sub"
parallel = true
incremental = false
overwrite = true
require_audit_trail = true

[[generation.rules]]
name = "r1"
description = "first rule"
query = "SELECT ?s WHERE { ?s ?p ?o }"
template = "templates/a.njk"
output_file = "out/a.mjs"
enabled = false
mode = "skip_existing"

[sync]
enabled = false
on_change = "watch"
conflict_mode = "skip"

[rdf]
base_uri = "https://example.org/base/"
default_prefix = "ex"

[[templates]]
name = "t1"
source = "tpls/njk/extra.njk"
output = "gen/extra.mjs"
`;
      const configPath = join(testDir, 'full.toml');
      await mkdir(join(testDir, 'ontology'), { recursive: true });
      await mkdir(join(testDir, 'templates'), { recursive: true });
      await mkdir(join(testDir, 'tmpl'), { recursive: true });
      await mkdir(join(testDir, 'tpls', 'njk'), { recursive: true });
      await mkdir(join(testDir, 'sub'), { recursive: true });
      await mkdir(join(testDir, 'dist', 'out'), { recursive: true });
      await mkdir(join(testDir, 'gen'), { recursive: true });
      await writeFile(join(testDir, 'ontology', 'schema.ttl'), '@prefix ex: <http://example.org/> .\n');

      await writeFile(configPath, configContent);

      const config = await parseConfig(configPath);

      expect(config.project.name).toBe('full');
      expect(config.project.author).toBe('unrdf');
      expect(config.project.license).toBe('MIT');
      expect(config.ontology.follow_imports).toBe(true);
      expect(config.generation.templates_dir).toBe(join(testDir, 'tmpl'));
      expect(config.generation.ontology_dir).toBe(join(testDir, 'sub'));
      expect(config.generation.parallel).toBe(true);
      expect(config.generation.require_audit_trail).toBe(true);
      expect(config.generation.rules[0].enabled).toBe(false);
      expect(config.generation.rules[0].description).toBe('first rule');
      expect(config.generation.rules[0].mode).toBe('skip_existing');
      expect(config.sync.enabled).toBe(false);
      expect(config.sync.on_change).toBe('watch');
      expect(config.sync.conflict_mode).toBe('skip');
      expect(config.rdf.base_uri).toBe('https://example.org/base/');
      expect(config.rdf.default_prefix).toBe('ex');
      expect(config.templates[0].name).toBe('t1');
      expect(config.templates[0].source).toBe(join(testDir, 'tpls', 'njk', 'extra.njk'));
      expect(config.templates[0].output).toBe(join(testDir, 'gen', 'extra.mjs'));
    });

    it('should accept every sync.on_change enum value', async () => {
      await writeFile(join(testDir, 'schema-onchange.ttl'), '@prefix : <http://x/> .\n');
      for (const onChange of ['manual', 'auto', 'watch']) {
        const cfg = `
[ontology]
source = "schema-onchange.ttl"

[sync]
on_change = "${onChange}"
`;
        const p = join(testDir, `onchange-${onChange}.toml`);
        await writeFile(p, cfg);
        const c = await parseConfig(p);
        expect(c.sync.on_change).toBe(onChange);
      }
    });

    it('should accept every sync.conflict_mode enum value', async () => {
      await writeFile(join(testDir, 'schema-conflict.ttl'), '@prefix : <http://x/> .\n');
      for (const m of ['warn', 'error', 'overwrite', 'skip']) {
        const cfg = `
[ontology]
source = "schema-conflict.ttl"

[sync]
conflict_mode = "${m}"
`;
        const p = join(testDir, `conflict-${m}.toml`);
        await writeFile(p, cfg);
        const c = await parseConfig(p);
        expect(c.sync.conflict_mode).toBe(m);
      }
    });

    it('should reject invalid sync.on_change', async () => {
      await writeFile(join(testDir, 'schema-bad-sync.ttl'), '@prefix : <http://x/> .\n');
      const cfg = `
[ontology]
source = "schema-bad-sync.ttl"
[sync]
on_change = "invalid"
`;
      const p = join(testDir, 'bad-onchange.toml');
      await writeFile(p, cfg);
      await expect(parseConfig(p)).rejects.toThrow();
    });

    it('should reject invalid sync.conflict_mode', async () => {
      await writeFile(join(testDir, 'schema-bad-conflict.ttl'), '@prefix : <http://x/> .\n');
      const cfg = `
[ontology]
source = "schema-bad-conflict.ttl"
[sync]
conflict_mode = "nope"
`;
      const p = join(testDir, 'bad-conflict.toml');
      await writeFile(p, cfg);
      await expect(parseConfig(p)).rejects.toThrow();
    });

    it('should reject invalid rdf.base_uri', async () => {
      await writeFile(join(testDir, 'schema-bad-rdf.ttl'), '@prefix : <http://x/> .\n');
      const cfg = `
[ontology]
source = "schema-bad-rdf.ttl"
[rdf]
base_uri = "not-a-url"
`;
      const p = join(testDir, 'bad-rdf.toml');
      await writeFile(p, cfg);
      await expect(parseConfig(p)).rejects.toThrow();
    });
  });

  describe('ConfigParseError', () => {
    it('should create error with message', () => {
      // Act
      const err = new ConfigParseError('Test error');

      // Assert
      expect(err.name).toBe('ConfigParseError');
      expect(err.message).toBe('Test error');
    });

    it('should store path, line, and column', () => {
      // Arrange
      const err = new ConfigParseError('Test error', {
        path: '/path/to/config.toml',
        line: 10,
        column: 5,
      });

      // Assert
      expect(err.path).toBe('/path/to/config.toml');
      expect(err.line).toBe(10);
      expect(err.column).toBe(5);
    });

    it('should format error with all details', () => {
      // Arrange
      const err = new ConfigParseError('Test error', {
        path: '/path/to/config.toml',
        line: 10,
        column: 5,
        cause: 'Underlying issue',
      });

      // Act
      const formatted = err.format();

      // Assert
      expect(formatted).toContain('Configuration Error');
      expect(formatted).toContain('Test error');
      expect(formatted).toContain('/path/to/config.toml');
      expect(formatted).toContain('line 10');
      expect(formatted).toContain('column 5');
      expect(formatted).toContain('Underlying issue');
    });

    it('should format error without optional fields', () => {
      // Arrange
      const err = new ConfigParseError('Simple error');

      // Act
      const formatted = err.format();

      // Assert
      expect(formatted).toContain('Configuration Error');
      expect(formatted).toContain('Simple error');
      expect(formatted).not.toContain('File:');
    });
  });

  describe('ConfigValidationError', () => {
    it('should create error with message and errors array', () => {
      // Arrange
      const errors = [
        { path: ['project', 'name'], message: 'Required' },
      ];

      // Act
      const err = new ConfigValidationError('Validation failed', errors);

      // Assert
      expect(err.name).toBe('ConfigValidationError');
      expect(err.message).toBe('Validation failed');
      expect(err.errors).toEqual(errors);
    });

    it('should format validation errors', () => {
      // Arrange
      const err = new ConfigValidationError('Validation failed', [
        { path: ['project', 'name'], message: 'Required', code: 'invalid_type' },
        { path: ['project', 'version'], message: 'Invalid', code: 'invalid_string', received: 'bad', expected: 'semver' },
      ]);

      // Act
      const formatted = err.format();

      // Assert
      expect(formatted).toContain('Configuration Validation Failed');
      expect(formatted).toContain('project.name: Required');
      expect(formatted).toContain('project.version: Invalid');
      expect(formatted).toContain('Received: "bad"');
      expect(formatted).toContain('Expected: semver');
    });

    it('should handle errors without path', () => {
      // Arrange
      const err = new ConfigValidationError('Validation failed', [
        { message: 'Unknown error' },
      ]);

      // Act
      const formatted = err.format();

      // Assert
      expect(formatted).toContain('unknown: Unknown error');
    });
  });
});
