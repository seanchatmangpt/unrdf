/**
 * @file template-engine.test.mjs
 * @description Tests for Tera-compatible template engine
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { TemplateEngine, createTemplateEngine } from '../src/template-engine.mjs';
import { loadConfig } from '../src/config-loader.mjs';
import { mkdirSync, rmSync, existsSync, readFileSync, writeFileSync } from 'fs';
import { resolve, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const TEST_DIR = resolve(__dirname, '.test-output');
const TEMPLATES_DIR = resolve(__dirname, '../templates');

describe('TemplateEngine', () => {
  beforeEach(() => {
    // Create test output directory
    if (!existsSync(TEST_DIR)) {
      mkdirSync(TEST_DIR, { recursive: true });
    }
  });

  afterEach(() => {
    // Clean up test output
    if (existsSync(TEST_DIR)) {
      rmSync(TEST_DIR, { recursive: true, force: true });
    }
  });

  describe('constructor', () => {
    it('should create engine with default options', () => {
      const engine = new TemplateEngine();
      expect(engine).toBeDefined();
      expect(engine.options).toBeDefined();
      expect(engine.env).toBeDefined();
    });

    it('should create engine with custom options', () => {
      const engine = new TemplateEngine({
        templatesDir: TEMPLATES_DIR,
        outputDir: TEST_DIR,
        autoescape: false,
      });
      expect(engine.options.templatesDir).toBe(TEMPLATES_DIR);
      expect(engine.options.outputDir).toBe(TEST_DIR);
      expect(engine.options.autoescape).toBe(false);
    });
  });

  describe('renderString', () => {
    it('should render simple template', () => {
      const engine = new TemplateEngine();
      const result = engine.renderString('Hello {{ name }}!', { name: 'World' });
      expect(result).toBe('Hello World!');
    });

    it('should render with filters', () => {
      const engine = new TemplateEngine();
      const result = engine.renderString('{{ text | upper }}', { text: 'hello' });
      expect(result).toBe('HELLO');
    });

    it('should render with loops', () => {
      const engine = new TemplateEngine();
      const template = '{% for item in items %}{{ item }}{% if not loop.last %}, {% endif %}{% endfor %}';
      const result = engine.renderString(template, { items: ['a', 'b', 'c'] });
      expect(result).toBe('a, b, c');
    });

    it('should render with conditionals', () => {
      const engine = new TemplateEngine();
      const template = '{% if show %}visible{% else %}hidden{% endif %}';
      expect(engine.renderString(template, { show: true })).toBe('visible');
      expect(engine.renderString(template, { show: false })).toBe('hidden');
    });
  });

  describe('render', () => {
    it('should render equation-reference template', () => {
      const engine = new TemplateEngine({
        templatesDir: TEMPLATES_DIR,
      });

      const context = {
        title: 'Test Equation',
        module: '@unrdf/test',
        version: '1.0.0',
        updated: '2026-01-18',
        description: 'Test description',
        equations: [
          {
            name: 'TestEq',
            formula: 'x + y = z',
            description: 'Simple addition',
            parameters: [
              { name: 'x', type: 'number', description: 'First number' },
              { name: 'y', type: 'number', description: 'Second number' },
            ],
            returns: { type: 'number', description: 'Sum' },
          },
        ],
        usage_example: 'const result = testEq(1, 2);',
      };

      const result = engine.render('equation-reference.tera', context);
      expect(result).toContain('# Test Equation');
      expect(result).toContain('TestEq');
      expect(result).toContain('x + y = z');
    });

    it('should render tutorial template', () => {
      const engine = new TemplateEngine({
        templatesDir: TEMPLATES_DIR,
      });

      const context = {
        title: 'Test Tutorial',
        difficulty: 'Beginner',
        duration: 10,
        learning_objectives: ['Learn A', 'Learn B'],
        steps: [
          {
            title: 'Step 1',
            description: 'First step',
            code: 'console.log("hello");',
          },
        ],
      };

      const result = engine.render('tutorial.tera', context);
      expect(result).toContain('# Test Tutorial');
      expect(result).toContain('Beginner');
      expect(result).toContain('Step 1');
    });
  });

  describe('renderToFile', () => {
    it('should render and write to file', () => {
      const engine = new TemplateEngine({
        templatesDir: TEMPLATES_DIR,
        outputDir: TEST_DIR,
      });

      const context = {
        title: 'File Test',
        module: '@unrdf/test',
        version: '1.0.0',
        updated: '2026-01-18',
        description: 'Test',
        equations: [],
        usage_example: 'test();',
      };

      const filePath = engine.renderToFile('equation-reference.tera', context, 'test.md');

      expect(existsSync(filePath)).toBe(true);
      const content = readFileSync(filePath, 'utf-8');
      expect(content).toContain('# File Test');
    });

    it('should create nested directories', () => {
      const engine = new TemplateEngine({
        templatesDir: TEMPLATES_DIR,
        outputDir: TEST_DIR,
      });

      const context = {
        title: 'Nested Test',
        module: '@unrdf/test',
        version: '1.0.0',
        updated: '2026-01-18',
        description: 'Test',
        equations: [],
        usage_example: 'test();',
      };

      const filePath = engine.renderToFile('equation-reference.tera', context, 'nested/dir/test.md');

      expect(existsSync(filePath)).toBe(true);
    });
  });

  describe('generateFromConfig', () => {
    it('should generate from TOML config', () => {
      const configPath = resolve(__dirname, '../configs/example-equation.toml');
      const engine = new TemplateEngine({
        templatesDir: TEMPLATES_DIR,
        outputDir: TEST_DIR,
      });

      const result = engine.generateFromConfig(configPath);

      expect(result.success).toBe(true);
      expect(result.outputPath).toBeDefined();
      expect(existsSync(result.outputPath)).toBe(true);
    });
  });

  describe('filters', () => {
    it('should apply string filters', () => {
      const engine = new TemplateEngine();
      expect(engine.renderString('{{ text | upper }}', { text: 'hello' })).toBe('HELLO');
      expect(engine.renderString('{{ text | lower }}', { text: 'HELLO' })).toBe('hello');
      expect(engine.renderString('{{ text | title }}', { text: 'hello world' })).toBe('Hello World');
      expect(engine.renderString('{{ text | slugify }}', { text: 'Hello World!' })).toBe('hello-world');
    });

    it('should apply array filters', () => {
      const engine = new TemplateEngine();
      expect(engine.renderString('{{ items | join(", ") }}', { items: ['a', 'b', 'c'] })).toBe('a, b, c');
      expect(engine.renderString('{{ items | first }}', { items: ['a', 'b', 'c'] })).toBe('a');
      expect(engine.renderString('{{ items | last }}', { items: ['a', 'b', 'c'] })).toBe('c');
    });

    it('should apply markdown filters', () => {
      const engine = new TemplateEngine();
      expect(engine.renderString('{{ text | bold }}', { text: 'hello' })).toBe('**hello**');
      expect(engine.renderString('{{ text | italic }}', { text: 'hello' })).toBe('*hello*');
      expect(engine.renderString('{{ text | inline_code }}', { text: 'code' })).toBe('`code`');
    });
  });

  describe('custom filters and globals', () => {
    it('should add custom filter', () => {
      const engine = new TemplateEngine();
      engine.addFilter('reverse', (str) => str.split('').reverse().join(''));

      const result = engine.renderString('{{ text | reverse }}', { text: 'hello' });
      expect(result).toBe('olleh');
    });

    it('should add custom global', () => {
      const engine = new TemplateEngine();
      engine.addGlobal('appVersion', '1.0.0');

      const result = engine.renderString('Version: {{ appVersion }}', {});
      expect(result).toBe('Version: 1.0.0');
    });
  });

  describe('createTemplateEngine factory', () => {
    it('should create engine instance', () => {
      const engine = createTemplateEngine({
        templatesDir: TEMPLATES_DIR,
        outputDir: TEST_DIR,
      });
      expect(engine).toBeInstanceOf(TemplateEngine);
    });
  });

  describe('error handling', () => {
    it('should throw on invalid template', () => {
      const engine = new TemplateEngine({ templatesDir: TEMPLATES_DIR });
      expect(() => {
        engine.render('nonexistent.tera', {});
      }).toThrow();
    });

    it('should throw on invalid context', () => {
      const engine = new TemplateEngine();
      expect(() => {
        engine.renderString('{{ value }}', null);
      }).toThrow();
    });
  });
});

describe('TemplateEngine Integration', () => {
  it('should generate complete documentation suite', () => {
    const engine = new TemplateEngine({
      templatesDir: TEMPLATES_DIR,
      outputDir: TEST_DIR,
    });

    // Generate equation reference
    const eqConfigPath = resolve(__dirname, '../configs/example-equation.toml');
    const eqResult = engine.generateFromConfig(eqConfigPath);
    expect(eqResult.success).toBe(true);

    // Generate tutorial
    const tutConfigPath = resolve(__dirname, '../configs/example-tutorial.toml');
    const tutResult = engine.generateFromConfig(tutConfigPath);
    expect(tutResult.success).toBe(true);

    // Verify both files exist
    expect(existsSync(eqResult.outputPath)).toBe(true);
    expect(existsSync(tutResult.outputPath)).toBe(true);
  });
});
