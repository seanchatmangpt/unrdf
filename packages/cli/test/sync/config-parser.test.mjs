/**
 * @file Config Parser Tests
 * @module cli/test/sync/config-parser
 * @description Tests for ggen.toml configuration parser
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
    it('should parse a valid ggen.toml file', async () => {
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
templates_dir = "templates/"
`;
      const configPath = join(testDir, 'ggen.toml');
      await writeFile(configPath, configContent);

      // Act
      const config = await parseConfig(configPath);

      // Assert
      expect(config.project.name).toBe('test-project');
      expect(config.project.version).toBe('1.0.0');
      expect(config.project.description).toBe('Test project');
      expect(config.ontology.format).toBe('turtle');
    });

    it('should throw ConfigParseError for missing file', async () => {
      // Arrange
      const configPath = join(testDir, 'nonexistent.toml');

      // Act & Assert
      await expect(parseConfig(configPath)).rejects.toThrow(ConfigParseError);
      try {
        await parseConfig(configPath);
      } catch (err) {
        expect(err.name).toBe('ConfigParseError');
        expect(err.format()).toContain('not found');
      }
    });

    it('should throw ConfigParseError for invalid TOML syntax', async () => {
      // Arrange
      const invalidToml = `
[project
name = "broken"
`;
      const configPath = join(testDir, 'invalid.toml');
      await writeFile(configPath, invalidToml);

      // Act & Assert
      await expect(parseConfig(configPath)).rejects.toThrow(ConfigParseError);
    });

    it('should throw ConfigValidationError for invalid schema', async () => {
      // Arrange
      const invalidConfig = `
[project]
name = "test"
version = "not-a-semver"
`;
      const configPath = join(testDir, 'invalid-schema.toml');
      await writeFile(configPath, invalidConfig);

      // Act & Assert
      await expect(parseConfig(configPath)).rejects.toThrow(ConfigValidationError);
    });

    it('should apply default values for optional fields', async () => {
      // Arrange
      const minimalConfig = `
[project]
name = "minimal"
version = "1.0.0"
`;
      const configPath = join(testDir, 'minimal.toml');
      await writeFile(configPath, minimalConfig);

      // Act
      const config = await parseConfig(configPath);

      // Assert
      expect(config.generation.incremental).toBe(true);
      expect(config.generation.overwrite).toBe(false);
      expect(config.sync.enabled).toBe(true);
      expect(config.sync.on_change).toBe('manual');
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
      const configPath = join(testDir, 'ggen.toml');
      await writeFile(configPath, configContent);

      // Act
      const config = await parseConfig(configPath);

      // Assert - Note: path.resolve removes trailing slashes
      expect(config.ontology.source).toBe(join(testDir, 'schema/domain.ttl'));
      expect(config.generation.output_dir).toBe(join(testDir, 'src/generated'));
    });

    it('should skip path resolution when disabled', async () => {
      // Arrange
      const configContent = `
[project]
name = "no-resolve"
version = "1.0.0"

[ontology]
source = "schema/domain.ttl"
`;
      const configPath = join(testDir, 'ggen.toml');
      await writeFile(configPath, configContent);

      // Act
      const config = await parseConfig(configPath, { resolvePaths: false });

      // Assert
      expect(config.ontology.source).toBe('schema/domain.ttl');
    });
  });

  describe('resolveConfigPaths()', () => {
    it('should resolve all relative paths', () => {
      // Arrange
      const config = {
        ontology: { source: 'schema/domain.ttl' },
        generation: {
          output_dir: 'src/generated/',
          templates_dir: 'templates/',
          ontology_dir: 'schema/',
        },
        templates: [
          { name: 'test', source: 'templates/test.tera', output: 'out/test.md' },
        ],
      };

      // Act
      const resolved = resolveConfigPaths(config, '/home/user/project');

      // Assert - Note: path.resolve removes trailing slashes
      expect(resolved.ontology.source).toBe('/home/user/project/schema/domain.ttl');
      expect(resolved.generation.output_dir).toBe('/home/user/project/src/generated');
      expect(resolved.generation.templates_dir).toBe('/home/user/project/templates');
      expect(resolved.templates[0].source).toBe('/home/user/project/templates/test.tera');
    });

    it('should preserve absolute paths', () => {
      // Arrange
      const config = {
        ontology: { source: '/absolute/path/domain.ttl' },
        generation: { output_dir: '/absolute/output/' },
      };

      // Act
      const resolved = resolveConfigPaths(config, '/home/user/project');

      // Assert
      expect(resolved.ontology.source).toBe('/absolute/path/domain.ttl');
      expect(resolved.generation.output_dir).toBe('/absolute/output/');
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

    it('should preserve undefined env vars as-is', () => {
      // Arrange
      delete process.env.UNDEFINED_VAR;
      const config = {
        project: { name: '${UNDEFINED_VAR}' },
      };

      // Act
      const result = substituteEnvVars(config);

      // Assert
      expect(result.project.name).toBe('${UNDEFINED_VAR}');
    });

    it('should substitute in nested objects and arrays', () => {
      // Arrange
      process.env.TEMPLATE_DIR = '/templates';
      const config = {
        templates: [
          { source: '${TEMPLATE_DIR}/one.tera' },
          { source: '${TEMPLATE_DIR}/two.tera' },
        ],
      };

      // Act
      const result = substituteEnvVars(config);

      // Assert
      expect(result.templates[0].source).toBe('/templates/one.tera');
      expect(result.templates[1].source).toBe('/templates/two.tera');
    });

    it('should not modify non-string values', () => {
      // Arrange
      const config = {
        generation: { incremental: true, overwrite: false },
        output: { line_length: 100 },
      };

      // Act
      const result = substituteEnvVars(config);

      // Assert
      expect(result.generation.incremental).toBe(true);
      expect(result.output.line_length).toBe(100);
    });
  });

  describe('validateConfig()', () => {
    it('should return success for valid config', () => {
      // Arrange
      const config = {
        project: { name: 'test', version: '1.0.0' },
      };

      // Act
      const result = validateConfig(config);

      // Assert
      expect(result.success).toBe(true);
      expect(result.data).toBeDefined();
      expect(result.data.project.name).toBe('test');
    });

    it('should return errors for invalid config', () => {
      // Arrange
      const config = {
        project: { name: '', version: 'invalid' },
      };

      // Act
      const result = validateConfig(config);

      // Assert
      expect(result.success).toBe(false);
      expect(result.errors).toHaveLength(2);
      expect(result.errors.some((e) => e.path.includes('name'))).toBe(true);
      expect(result.errors.some((e) => e.path.includes('version'))).toBe(true);
    });

    it('should validate nested sections', () => {
      // Arrange
      const config = {
        project: { name: 'test', version: '1.0.0' },
        rdf: { base_uri: 'not-a-url' },
      };

      // Act
      const result = validateConfig(config);

      // Assert - validation should fail for invalid URL
      expect(result.success).toBe(false);
      expect(result.errors.some((e) => e.path.includes('base_uri') || e.message.toLowerCase().includes('url') || e.message.toLowerCase().includes('invalid'))).toBe(true);
    });
  });

  describe('createDefaultConfig()', () => {
    it('should create valid default config', () => {
      // Act
      const config = createDefaultConfig();

      // Assert
      const result = validateConfig(config);
      expect(result.success).toBe(true);
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

    it('should throw for invalid config', () => {
      // Arrange
      const invalidConfig = {
        project: { name: '', version: 'bad' },
      };

      // Act & Assert
      expect(() => serializeConfig(invalidConfig)).toThrow(ConfigValidationError);
    });
  });

  describe('findConfigFile()', () => {
    it('should find ggen.toml in directory', async () => {
      // Arrange
      const configPath = join(testDir, 'ggen.toml');
      await writeFile(configPath, '[project]\nname = "test"\nversion = "1.0.0"');

      // Act
      const found = await findConfigFile(testDir);

      // Assert
      expect(found).toBe(configPath);
    });

    it('should find .ggen.toml as fallback', async () => {
      // Arrange
      const configPath = join(testDir, '.ggen.toml');
      await writeFile(configPath, '[project]\nname = "test"\nversion = "1.0.0"');

      // Act
      const found = await findConfigFile(testDir);

      // Assert
      expect(found).toBe(configPath);
    });

    it('should return null when no config found', async () => {
      // Act
      const found = await findConfigFile(testDir);

      // Assert
      expect(found).toBeNull();
    });
  });

  describe('ConfigParseError', () => {
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
  });

  describe('ConfigValidationError', () => {
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
  });
});

describe('Schema Validation', () => {
  describe('ProjectSchema', () => {
    it('should accept valid project config', () => {
      // Arrange
      const config = {
        project: { name: 'test', version: '1.0.0-rc.1', description: 'Test' },
      };

      // Act
      const result = validateConfig(config);

      // Assert
      expect(result.success).toBe(true);
    });

    it('should reject empty name', () => {
      // Arrange
      const config = {
        project: { name: '', version: '1.0.0' },
      };

      // Act
      const result = validateConfig(config);

      // Assert
      expect(result.success).toBe(false);
    });
  });

  describe('OntologySchema', () => {
    it('should accept valid ontology formats', () => {
      // Arrange - using format names from the schema enum
      const formats = ['turtle', 'ntriples', 'jsonld', 'rdfxml'];

      for (const format of formats) {
        const config = {
          project: { name: 'test', version: '1.0.0' },
          ontology: { source: 'schema.ttl', format },
        };

        // Act
        const result = validateConfig(config);

        // Assert
        expect(result.success).toBe(true);
      }
    });
  });

  describe('SyncSchema', () => {
    it('should accept valid conflict modes', () => {
      // Arrange
      const modes = ['warn', 'error', 'overwrite', 'skip'];

      for (const mode of modes) {
        const config = {
          project: { name: 'test', version: '1.0.0' },
          sync: { conflict_mode: mode },
        };

        // Act
        const result = validateConfig(config);

        // Assert
        expect(result.success).toBe(true);
      }
    });
  });
});
