/**
 * @fileoverview Tests for CLI init command
 * Testing project initialization and scaffolding
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  execCLI,
  createCLITestContext,
  assertSuccess,
  assertOutputContains
} from './test-helpers.mjs';
import { join } from 'node:path';
import { readFile, access } from 'node:fs/promises';

describe('CLI: init command', () => {
  let ctx;

  beforeEach(() => {
    ctx = createCLITestContext();
  });

  afterEach(async () => {
    await ctx.cleanup();
  });

  describe('Project initialization (Critical Path)', () => {
    it('should create new project structure', async () => {
      const dir = await ctx.createTempDir();
      const projectName = 'test-project';

      const result = await execCLI(['init', projectName], { cwd: dir });

      assertSuccess(result);
      assertOutputContains(result.stdout, `Initializing UNRDF project: ${projectName}`);
      assertOutputContains(result.stdout, 'Project initialized');
    });

    it('should create package.json', async () => {
      const dir = await ctx.createTempDir();
      const projectName = 'test-project';

      const result = await execCLI(['init', projectName], { cwd: dir });
      assertSuccess(result);

      const packageJsonPath = join(dir, projectName, 'package.json');
      const packageJson = JSON.parse(await readFile(packageJsonPath, 'utf-8'));

      expect(packageJson.name).toBe(projectName);
      expect(packageJson.type).toBe('module');
      expect(packageJson.dependencies).toHaveProperty('unrdf');
      expect(packageJson.scripts).toHaveProperty('dev');
      expect(packageJson.scripts).toHaveProperty('build');
    });

    it('should create unrdf.config.mjs', async () => {
      const dir = await ctx.createTempDir();
      const projectName = 'test-project';

      const result = await execCLI(['init', projectName], { cwd: dir });
      assertSuccess(result);

      const configPath = join(dir, projectName, 'unrdf.config.mjs');
      const configContent = await readFile(configPath, 'utf-8');

      expect(configContent).toContain('export default');
      expect(configContent).toContain('baseIRI');
      expect(configContent).toContain('prefixes');
      expect(configContent).toContain('foaf');
      expect(configContent).toContain('schema');
    });

    it('should create sample data.ttl', async () => {
      const dir = await ctx.createTempDir();
      const projectName = 'test-project';

      const result = await execCLI(['init', projectName], { cwd: dir });
      assertSuccess(result);

      const dataPath = join(dir, projectName, 'data.ttl');
      const dataContent = await readFile(dataPath, 'utf-8');

      expect(dataContent).toContain('@prefix');
      expect(dataContent).toContain('foaf:Person');
    });

    it('should list created files', async () => {
      const dir = await ctx.createTempDir();
      const projectName = 'test-project';

      const result = await execCLI(['init', projectName], { cwd: dir });

      assertSuccess(result);
      assertOutputContains(result.stdout, 'package.json');
      assertOutputContains(result.stdout, 'unrdf.config.mjs');
      assertOutputContains(result.stdout, 'data.ttl');
    });
  });

  describe('Project structure validation', () => {
    it('should create all required files', async () => {
      const dir = await ctx.createTempDir();
      const projectName = 'test-project';

      await execCLI(['init', projectName], { cwd: dir });

      const projectDir = join(dir, projectName);

      // Check all files exist
      await access(join(projectDir, 'package.json'));
      await access(join(projectDir, 'unrdf.config.mjs'));
      await access(join(projectDir, 'data.ttl'));
    });

    it('should create valid npm package', async () => {
      const dir = await ctx.createTempDir();
      const projectName = 'my-rdf-app';

      await execCLI(['init', projectName], { cwd: dir });

      const packageJsonPath = join(dir, projectName, 'package.json');
      const packageJson = JSON.parse(await readFile(packageJsonPath, 'utf-8'));

      // Validate npm package structure
      expect(packageJson.name).toBe(projectName);
      expect(packageJson.version).toBeTruthy();
      expect(packageJson.description).toBeTruthy();
      expect(typeof packageJson.dependencies).toBe('object');
      expect(typeof packageJson.scripts).toBe('object');
    });

    it('should include development scripts', async () => {
      const dir = await ctx.createTempDir();
      const projectName = 'test-project';

      await execCLI(['init', projectName], { cwd: dir });

      const packageJsonPath = join(dir, projectName, 'package.json');
      const packageJson = JSON.parse(await readFile(packageJsonPath, 'utf-8'));

      expect(packageJson.scripts.dev).toContain('unrdf parse');
      expect(packageJson.scripts.build).toContain('unrdf convert');
    });
  });

  describe('Configuration defaults', () => {
    it('should include standard prefixes in config', async () => {
      const dir = await ctx.createTempDir();
      const projectName = 'test-project';

      await execCLI(['init', projectName], { cwd: dir });

      const configPath = join(dir, projectName, 'unrdf.config.mjs');
      const configContent = await readFile(configPath, 'utf-8');

      expect(configContent).toContain('foaf');
      expect(configContent).toContain('schema');
      expect(configContent).toContain('http://xmlns.com/foaf/0.1/');
      expect(configContent).toContain('https://schema.org/');
    });

    it('should enable validation by default', async () => {
      const dir = await ctx.createTempDir();
      const projectName = 'test-project';

      await execCLI(['init', projectName], { cwd: dir });

      const configPath = join(dir, projectName, 'unrdf.config.mjs');
      const configContent = await readFile(configPath, 'utf-8');

      expect(configContent).toContain('validation');
      expect(configContent).toContain('strict: true');
      expect(configContent).toContain('validateOnLoad: true');
    });
  });

  describe('Sample data', () => {
    it('should create parseable Turtle data', async () => {
      const dir = await ctx.createTempDir();
      const projectName = 'test-project';

      await execCLI(['init', projectName], { cwd: dir });

      const dataPath = join(dir, projectName, 'data.ttl');

      // Try to parse the generated data
      const parseResult = await execCLI(['parse', dataPath]);
      assertSuccess(parseResult);
    });

    it('should include sample persons', async () => {
      const dir = await ctx.createTempDir();
      const projectName = 'test-project';

      await execCLI(['init', projectName], { cwd: dir });

      const dataPath = join(dir, projectName, 'data.ttl');
      const dataContent = await readFile(dataPath, 'utf-8');

      expect(dataContent).toContain('foaf:Person');
      expect(dataContent).toContain('foaf:name');
      expect(dataContent).toContain('foaf:age');
    });
  });

  describe('Error handling', () => {
    it('should require project name', async () => {
      const result = await execCLI(['init']);

      // Should fail without project name
      expect(result.exitCode).not.toBe(0);
    });

    it('should handle special characters in project name', async () => {
      const dir = await ctx.createTempDir();
      const projectName = 'test-project-123';

      const result = await execCLI(['init', projectName], { cwd: dir });

      assertSuccess(result);
    });
  });

  describe('Performance', () => {
    it('should initialize project quickly', async () => {
      const dir = await ctx.createTempDir();
      const projectName = 'perf-test';

      const start = Date.now();
      const result = await execCLI(['init', projectName], { cwd: dir });
      const duration = Date.now() - start;

      assertSuccess(result);

      // Performance target: initialize project in under 1 second
      expect(duration).toBeLessThan(1000);
    });
  });
});
