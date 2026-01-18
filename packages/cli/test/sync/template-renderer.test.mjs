/**
 * @file Template Renderer Tests
 * @module cli/commands/sync/template-renderer.test
 * @description Tests for RDF-aware template rendering with Nunjucks
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { writeFile, mkdir, rm, readFile } from 'fs/promises';
import { resolve, join } from 'path';
import { existsSync } from 'fs';
import { tmpdir } from 'os';
import {
  renderTemplate,
  createNunjucksEnvironment,
  renderWithOptions,
  DEFAULT_PREFIXES,
  TEMPLATE_EXTENSIONS,
} from '../../src/cli/commands/sync/template-renderer.mjs';

/**
 * Test fixtures directory
 */
const TEST_DIR = join(tmpdir(), `template-renderer-test-${Date.now()}`);
const TEMPLATES_DIR = join(TEST_DIR, 'templates');
const OUTPUT_DIR = join(TEST_DIR, 'output');

/**
 * Sample SPARQL results for testing
 */
const SAMPLE_SPARQL_RESULTS = [
  {
    s: { value: 'http://example.org/User' },
    p: { value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
    o: { value: 'http://www.w3.org/2002/07/owl#Class' },
  },
  {
    s: { value: 'http://example.org/name' },
    p: { value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
    o: { value: 'http://www.w3.org/2002/07/owl#DatatypeProperty' },
  },
  {
    s: { value: 'http://example.org/email' },
    p: { value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
    o: { value: 'http://www.w3.org/2002/07/owl#DatatypeProperty' },
  },
];

/**
 * Simple SPARQL results (string values)
 */
const SIMPLE_SPARQL_RESULTS = [
  { class: 'User', property: 'name', type: 'string' },
  { class: 'User', property: 'email', type: 'string' },
  { class: 'Post', property: 'title', type: 'string' },
  { class: 'Post', property: 'content', type: 'text' },
];

describe('Template Renderer', () => {
  beforeEach(async () => {
    // Create test directories
    await mkdir(TEMPLATES_DIR, { recursive: true });
    await mkdir(OUTPUT_DIR, { recursive: true });
  });

  afterEach(async () => {
    // Clean up test directories
    if (existsSync(TEST_DIR)) {
      await rm(TEST_DIR, { recursive: true, force: true });
    }
  });

  describe('renderTemplate()', () => {
    it('should render a simple template with SPARQL results', async () => {
      // Arrange
      const templatePath = join(TEMPLATES_DIR, 'simple.njk');
      const templateContent = `---
to: output/api.ts
description: Generated API file
---
// Generated API
{% for result in sparql_results %}
// Class: {{ result.class }}
{% endfor %}`;

      await writeFile(templatePath, templateContent, 'utf-8');

      // Act
      const result = await renderTemplate(templatePath, SIMPLE_SPARQL_RESULTS);

      // Assert
      expect(result.content).toContain('// Generated API');
      expect(result.content).toContain('// Class: User');
      expect(result.content).toContain('// Class: Post');
      expect(result.outputPath).toBe('output/api.ts');
      expect(result.description).toBe('Generated API file');
    });

    it('should render template with context variables', async () => {
      // Arrange
      const templatePath = join(TEMPLATES_DIR, 'context.njk');
      // Note: YAML frontmatter values with {{ }} must be quoted
      const templateContent = `---
to: "{{ projectName }}/api.ts"
---
// Project: {{ projectName }}
// Version: {{ version }}`;

      await writeFile(templatePath, templateContent, 'utf-8');

      // Act
      const result = await renderTemplate(templatePath, [], {
        projectName: 'MyApp',
        version: '1.0.0',
      });

      // Assert
      expect(result.content).toContain('// Project: MyApp');
      expect(result.content).toContain('// Version: 1.0.0');
      expect(result.outputPath).toBe('MyApp/api.ts');
    });

    it('should handle missing template file', async () => {
      // Arrange
      const templatePath = join(TEMPLATES_DIR, 'nonexistent.njk');

      // Act & Assert
      await expect(renderTemplate(templatePath, [])).rejects.toThrow('Template not found');
    });

    it('should merge frontmatter variables with context', async () => {
      // Arrange
      const templatePath = join(TEMPLATES_DIR, 'variables.njk');
      const templateContent = `---
to: output/result.txt
variables:
  defaultAuthor: System
  defaultVersion: 0.1.0
---
Author: {{ author | default(defaultAuthor) }}
Version: {{ version | default(defaultVersion) }}`;

      await writeFile(templatePath, templateContent, 'utf-8');

      // Act
      const result = await renderTemplate(templatePath, [], { author: 'John' });

      // Assert
      expect(result.content).toContain('Author: John');
      expect(result.content).toContain('Version: 0.1.0');
    });

    it('should include template metadata in result', async () => {
      // Arrange
      const templatePath = join(TEMPLATES_DIR, 'metadata.njk');
      const templateContent = `---
to: output/file.ts
description: Test file
mode: append
---
Content`;

      await writeFile(templatePath, templateContent, 'utf-8');

      // Act
      const result = await renderTemplate(templatePath, []);

      // Assert
      expect(result.mode).toBe('append');
      expect(result.frontmatter).toBeDefined();
      expect(result.frontmatter.mode).toBe('append');
    });
  });

  describe('createNunjucksEnvironment()', () => {
    it('should create environment with custom filters', () => {
      // Act
      const env = createNunjucksEnvironment();

      // Assert
      expect(env).toBeDefined();
      expect(env.renderString).toBeInstanceOf(Function);
      expect(env.addFilter).toBeInstanceOf(Function);
      expect(env.addGlobal).toBeInstanceOf(Function);
    });

    it('should render string templates', () => {
      // Arrange
      const env = createNunjucksEnvironment();
      const template = 'Hello {{ name }}!';

      // Act
      const result = env.renderString(template, { name: 'World' });

      // Assert
      expect(result).toBe('Hello World!');
    });

    it('should include localName filter', () => {
      // Arrange
      const env = createNunjucksEnvironment();

      // Act
      const result = env.renderString('{{ "http://example.org/User" | localName }}', {});

      // Assert
      expect(result).toBe('User');
    });

    it('should include namespace filter', () => {
      // Arrange
      const env = createNunjucksEnvironment();

      // Act
      const result = env.renderString('{{ "http://example.org/User" | namespace }}', {});

      // Assert
      expect(result).toBe('http://example.org/');
    });

    it('should include groupBy filter', () => {
      // Arrange
      const env = createNunjucksEnvironment();
      const template = `{% set grouped = results | groupBy('class') %}{% for key, items in grouped %}{{ key }}: {{ items | length }}{% endfor %}`;

      // Act
      const result = env.renderString(template, { results: SIMPLE_SPARQL_RESULTS });

      // Assert
      expect(result).toContain('User: 2');
      expect(result).toContain('Post: 2');
    });

    it('should include distinctValues filter', () => {
      // Arrange
      const env = createNunjucksEnvironment();
      const template = `{% set classes = results | distinctValues('class') %}{{ classes | join(', ') }}`;

      // Act
      const result = env.renderString(template, { results: SIMPLE_SPARQL_RESULTS });

      // Assert
      expect(result).toContain('User');
      expect(result).toContain('Post');
    });

    it('should include sortBy filter', () => {
      // Arrange
      const env = createNunjucksEnvironment();
      const template = `{% set sorted = results | sortBy('property') %}{% for r in sorted %}{{ r.property }},{% endfor %}`;

      // Act
      const result = env.renderString(template, { results: SIMPLE_SPARQL_RESULTS });

      // Assert
      expect(result).toBe('content,email,name,title,');
    });

    it('should include camelCase filter', () => {
      // Arrange
      const env = createNunjucksEnvironment();

      // Act
      const result = env.renderString('{{ "my-class-name" | camelCase }}', {});

      // Assert
      expect(result).toBe('myClassName');
    });

    it('should include pascalCase filter', () => {
      // Arrange
      const env = createNunjucksEnvironment();

      // Act
      const result = env.renderString('{{ "my-class-name" | pascalCase }}', {});

      // Assert
      expect(result).toBe('MyClassName');
    });

    it('should include snakeCase filter', () => {
      // Arrange
      const env = createNunjucksEnvironment();

      // Act
      const result = env.renderString('{{ "myClassName" | snakeCase }}', {});

      // Assert
      expect(result).toBe('my_class_name');
    });

    it('should include kebabCase filter', () => {
      // Arrange
      const env = createNunjucksEnvironment();

      // Act
      const result = env.renderString('{{ "myClassName" | kebabCase }}', {});

      // Assert
      expect(result).toBe('my-class-name');
    });

    it('should include zodType filter', () => {
      // Arrange
      const env = createNunjucksEnvironment();

      // Act
      const result = env.renderString('{{ "string" | zodType }}', {});

      // Assert
      expect(result).toBe('z.string()');
    });

    it('should include jsdocType filter', () => {
      // Arrange
      const env = createNunjucksEnvironment();

      // Act
      const result = env.renderString('{{ "integer" | jsdocType }}', {});

      // Assert
      expect(result).toBe('number');
    });

    it('should include keys filter', () => {
      // Arrange
      const env = createNunjucksEnvironment();

      // Act
      const result = env.renderString('{{ obj | keys | join(",") }}', { obj: { a: 1, b: 2 } });

      // Assert
      expect(result).toBe('a,b');
    });

    it('should include values filter', () => {
      // Arrange
      const env = createNunjucksEnvironment();

      // Act
      const result = env.renderString('{{ obj | values | join(",") }}', { obj: { a: 1, b: 2 } });

      // Assert
      expect(result).toBe('1,2');
    });

    it('should include indent filter', () => {
      // Arrange
      const env = createNunjucksEnvironment();

      // Act
      const result = env.renderString('{{ text | indent(4) }}', { text: 'line1\nline2' });

      // Assert
      expect(result).toBe('    line1\n    line2');
    });

    it('should include quote filter', () => {
      // Arrange
      const env = createNunjucksEnvironment();

      // Act
      const result = env.renderString('{{ "hello" | quote }}', {});

      // Assert
      expect(result).toBe('"hello"');
    });

    it('should include date filter', () => {
      // Arrange
      const env = createNunjucksEnvironment();
      const testDate = new Date('2024-01-15T10:30:45');

      // Act
      const result = env.renderString('{{ d | date("YYYY-MM-DD") }}', { d: testDate });

      // Assert
      expect(result).toBe('2024-01-15');
    });

    it('should render file templates with templateDir', async () => {
      // Arrange
      const templatePath = join(TEMPLATES_DIR, 'file-test.njk');
      await writeFile(templatePath, 'File content: {{ value }}', 'utf-8');

      const env = createNunjucksEnvironment(TEMPLATES_DIR);

      // Act
      const result = env.render('file-test.njk', { value: 42 });

      // Assert
      expect(result).toBe('File content: 42');
    });
  });

  describe('renderWithOptions()', () => {
    it('should return content in dry-run mode without writing', async () => {
      // Arrange
      const templatePath = join(TEMPLATES_DIR, 'dryrun.njk');
      const templateContent = `---
to: output.ts
---
Generated content`;

      await writeFile(templatePath, templateContent, 'utf-8');

      // Act
      const result = await renderWithOptions(templatePath, [], { dryRun: true });

      // Assert
      expect(result.content).toContain('Generated content');
      expect(result.status).toBe('dry-run');
    });

    it('should write file in non-dry-run mode', async () => {
      // Arrange
      const templatePath = join(TEMPLATES_DIR, 'write.njk');
      const templateContent = `---
to: written.ts
---
Written content`;

      await writeFile(templatePath, templateContent, 'utf-8');

      // Act
      const result = await renderWithOptions(templatePath, [], { dryRun: false, outputDir: OUTPUT_DIR });

      // Assert
      expect(result.status).toBe('success');
      const outputPath = join(OUTPUT_DIR, 'written.ts');
      expect(existsSync(outputPath)).toBe(true);

      const content = await readFile(outputPath, 'utf-8');
      expect(content).toContain('Written content');
    });

    it('should create directories if needed', async () => {
      // Arrange
      const templatePath = join(TEMPLATES_DIR, 'nested.njk');
      const templateContent = `---
to: nested/deep/file.ts
---
Nested content`;

      await writeFile(templatePath, templateContent, 'utf-8');

      // Act
      await renderWithOptions(templatePath, [], { dryRun: false, outputDir: OUTPUT_DIR });

      // Assert
      const outputPath = join(OUTPUT_DIR, 'nested', 'deep', 'file.ts');
      expect(existsSync(outputPath)).toBe(true);
    });

    it('should skip existing files with skip_existing mode', async () => {
      // Arrange
      const existingPath = join(OUTPUT_DIR, 'existing.ts');
      await writeFile(existingPath, 'Original content', 'utf-8');

      const templatePath = join(TEMPLATES_DIR, 'skip.njk');
      const templateContent = `---
to: existing.ts
mode: skip_existing
---
New content`;

      await writeFile(templatePath, templateContent, 'utf-8');

      // Act
      const result = await renderWithOptions(templatePath, [], { dryRun: false, outputDir: OUTPUT_DIR });

      // Assert
      expect(result.status).toBe('skipped');

      const content = await readFile(existingPath, 'utf-8');
      expect(content).toBe('Original content');
    });

    it('should append content with append mode', async () => {
      // Arrange
      const appendPath = join(OUTPUT_DIR, 'append.ts');
      await writeFile(appendPath, 'Line 1', 'utf-8');

      const templatePath = join(TEMPLATES_DIR, 'append.njk');
      const templateContent = `---
to: append.ts
mode: append
---
Line 2`;

      await writeFile(templatePath, templateContent, 'utf-8');

      // Act
      await renderWithOptions(templatePath, [], { dryRun: false, outputDir: OUTPUT_DIR });

      // Assert
      const content = await readFile(appendPath, 'utf-8');
      expect(content).toContain('Line 1');
      expect(content).toContain('Line 2');
    });

    it('should return finalPath in result', async () => {
      // Arrange
      const templatePath = join(TEMPLATES_DIR, 'path.njk');
      const templateContent = `---
to: result.ts
---
Content`;

      await writeFile(templatePath, templateContent, 'utf-8');

      // Act
      const result = await renderWithOptions(templatePath, [], { dryRun: false, outputDir: OUTPUT_DIR });

      // Assert
      expect(result.finalPath).toBe(resolve(OUTPUT_DIR, 'result.ts'));
    });

    it('should pass context to template', async () => {
      // Arrange
      const templatePath = join(TEMPLATES_DIR, 'context-opts.njk');
      const templateContent = `---
to: context.ts
---
Project: {{ projectName }}`;

      await writeFile(templatePath, templateContent, 'utf-8');

      // Act
      const result = await renderWithOptions(templatePath, [], {
        dryRun: true,
        context: { projectName: 'MyProject' },
      });

      // Assert
      expect(result.content).toContain('Project: MyProject');
    });
  });

  describe('Filter edge cases', () => {
    it('should handle null/undefined in localName filter', () => {
      // Arrange
      const env = createNunjucksEnvironment();

      // Act
      const result = env.renderString('{{ val | localName }}', { val: null });

      // Assert
      expect(result).toBe('');
    });

    it('should handle null/undefined in camelCase filter', () => {
      // Arrange
      const env = createNunjucksEnvironment();

      // Act
      const result = env.renderString('{{ val | camelCase }}', { val: undefined });

      // Assert
      expect(result).toBe('');
    });

    it('should handle empty arrays in groupBy filter', () => {
      // Arrange
      const env = createNunjucksEnvironment();
      const template = `{% set grouped = arr | groupBy('key') %}{{ grouped | keys | length }}`;

      // Act
      const result = env.renderString(template, { arr: [] });

      // Assert
      expect(result).toBe('0');
    });

    it('should handle empty arrays in distinctValues filter', () => {
      // Arrange
      const env = createNunjucksEnvironment();
      const template = `{% set vals = arr | distinctValues('key') %}{{ vals | length }}`;

      // Act
      const result = env.renderString(template, { arr: [] });

      // Assert
      expect(result).toBe('0');
    });

    it('should handle sortBy with desc direction', () => {
      // Arrange
      const env = createNunjucksEnvironment();
      const template = `{% set sorted = items | sortBy('n', 'desc') %}{% for i in sorted %}{{ i.n }},{% endfor %}`;

      // Act
      const result = env.renderString(template, { items: [{ n: 1 }, { n: 3 }, { n: 2 }] });

      // Assert
      expect(result).toBe('3,2,1,');
    });

    it('should handle items filter on object', () => {
      // Arrange
      const env = createNunjucksEnvironment();
      const template = `{% for k, v in obj | items %}{{ k }}={{ v }};{% endfor %}`;

      // Act
      const result = env.renderString(template, { obj: { a: 1, b: 2 } });

      // Assert
      expect(result).toContain('a=1');
      expect(result).toContain('b=2');
    });
  });

  describe('Constants', () => {
    it('should export DEFAULT_PREFIXES', () => {
      expect(DEFAULT_PREFIXES).toBeDefined();
      expect(DEFAULT_PREFIXES.rdf).toBe('http://www.w3.org/1999/02/22-rdf-syntax-ns#');
      expect(DEFAULT_PREFIXES.rdfs).toBe('http://www.w3.org/2000/01/rdf-schema#');
      expect(DEFAULT_PREFIXES.owl).toBe('http://www.w3.org/2002/07/owl#');
      expect(DEFAULT_PREFIXES.xsd).toBe('http://www.w3.org/2001/XMLSchema#');
    });

    it('should export TEMPLATE_EXTENSIONS', () => {
      expect(TEMPLATE_EXTENSIONS).toBeDefined();
      expect(TEMPLATE_EXTENSIONS).toContain('.njk');
      expect(TEMPLATE_EXTENSIONS).toContain('.tera');
      expect(TEMPLATE_EXTENSIONS).toContain('.jinja');
      expect(TEMPLATE_EXTENSIONS).toContain('.jinja2');
    });
  });
});
