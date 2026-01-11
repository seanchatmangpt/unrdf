/**
 * @file Template Renderer Tests
 * @module cli/commands/sync/template-renderer.test
 * @description Tests for RDF-aware template rendering with @unrdf/kgn
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { writeFile, mkdir, rm, readFile } from 'fs/promises';
import { resolve, join } from 'path';
import { existsSync } from 'fs';
import { tmpdir } from 'os';
import {
  renderTemplate,
  createTemplateEngine,
  renderWithOptions,
  batchRender,
  discoverTemplates,
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
      await expect(renderTemplate(templatePath, [])).rejects.toThrow('Template file not found');
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
      expect(result.templatePath).toBe(resolve(templatePath));
      expect(result.frontmatter).toBeDefined();
      expect(result.frontmatter.mode).toBe('append');
    });
  });

  describe('createTemplateEngine()', () => {
    it('should create engine with default options', async () => {
      // Act
      const engine = await createTemplateEngine();

      // Assert
      expect(engine).toBeDefined();
      expect(engine.render).toBeInstanceOf(Function);
      expect(engine.renderFile).toBeInstanceOf(Function);
      expect(engine.addFilter).toBeInstanceOf(Function);
      expect(engine.addGlobal).toBeInstanceOf(Function);
      expect(engine.prefixes).toBeDefined();
    });

    it('should render string templates', async () => {
      // Arrange
      const engine = await createTemplateEngine();
      const template = 'Hello {{ name }}!';

      // Act
      const result = engine.render(template, { name: 'World' });

      // Assert
      expect(result).toBe('Hello World!');
    });

    it('should include RDF filters', async () => {
      // Arrange
      const engine = await createTemplateEngine();

      // Act - Test expand filter
      const expandResult = engine.render('{{ "rdf:type" | expand }}', {});

      // Assert
      expect(expandResult).toContain('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
    });

    it('should include localName filter', async () => {
      // Arrange
      const engine = await createTemplateEngine();

      // Act
      const result = engine.render('{{ "http://example.org/User" | localName }}', {});

      // Assert
      expect(result).toBe('User');
    });

    it('should include groupBy filter', async () => {
      // Arrange
      const engine = await createTemplateEngine();
      const template = `{% set grouped = results | groupBy('class') %}{% for key, items in grouped %}{{ key }}: {{ items | length }}{% endfor %}`;

      // Act
      const result = engine.render(template, { results: SIMPLE_SPARQL_RESULTS });

      // Assert
      expect(result).toContain('User: 2');
      expect(result).toContain('Post: 2');
    });

    it('should include distinctValues filter', async () => {
      // Arrange
      const engine = await createTemplateEngine();
      const template = `{% set classes = results | distinctValues('class') %}{{ classes | join(', ') }}`;

      // Act
      const result = engine.render(template, { results: SIMPLE_SPARQL_RESULTS });

      // Assert
      expect(result).toContain('User');
      expect(result).toContain('Post');
    });

    it('should include sortBy filter', async () => {
      // Arrange
      const engine = await createTemplateEngine();
      const template = `{% set sorted = results | sortBy('property') %}{% for r in sorted %}{{ r.property }},{% endfor %}`;

      // Act
      const result = engine.render(template, { results: SIMPLE_SPARQL_RESULTS });

      // Assert
      expect(result).toBe('content,email,name,title,');
    });

    it('should render file templates with templateDir', async () => {
      // Arrange
      const templatePath = join(TEMPLATES_DIR, 'file-test.njk');
      await writeFile(templatePath, 'File content: {{ value }}', 'utf-8');

      const engine = await createTemplateEngine({ templateDir: TEMPLATES_DIR });

      // Act
      const result = engine.renderFile('file-test.njk', { value: 42 });

      // Assert
      expect(result).toBe('File content: 42');
    });

    it('should support custom prefixes', async () => {
      // Arrange
      const engine = await createTemplateEngine({
        prefixes: { myapp: 'http://myapp.org/' },
      });

      // Act
      const result = engine.render('{{ "myapp:User" | expand }}', {});

      // Assert
      expect(result).toBe('http://myapp.org/User');
    });

    it('should provide global functions', async () => {
      // Arrange
      const engine = await createTemplateEngine();

      // Act - Test uri global
      const uriResult = engine.render('{{ uri("rdf:type") }}', {});
      // Test literal global
      const literalResult = engine.render('{{ literal("Hello", "en") }}', {});
      // Test blankNode global
      const blankResult = engine.render('{{ blankNode("person1") }}', {});

      // Assert
      expect(uriResult).toContain('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
      expect(literalResult).toBe('"Hello"@en');
      expect(blankResult).toBe('_:person1');
    });
  });

  describe('renderWithOptions()', () => {
    it('should return content without writing in dry-run mode', async () => {
      // Arrange
      const templatePath = join(TEMPLATES_DIR, 'dryrun.njk');
      const templateContent = `---
to: ${OUTPUT_DIR}/output.ts
---
Generated content`;

      await writeFile(templatePath, templateContent, 'utf-8');

      // Act
      const result = await renderWithOptions(templatePath, [], {}, { dryRun: true });

      // Assert
      expect(result.content).toContain('Generated content');
      expect(result.written).toBe(false);
      expect(result.dryRun).toBe(true);
      expect(existsSync(join(OUTPUT_DIR, 'output.ts'))).toBe(false);
    });

    it('should write file in non-dry-run mode', async () => {
      // Arrange
      const outputPath = join(OUTPUT_DIR, 'written.ts');
      const templatePath = join(TEMPLATES_DIR, 'write.njk');
      const templateContent = `---
to: ${outputPath}
---
Written content`;

      await writeFile(templatePath, templateContent, 'utf-8');

      // Act
      const result = await renderWithOptions(templatePath, [], {}, { dryRun: false });

      // Assert
      expect(result.written).toBe(true);
      expect(result.dryRun).toBe(false);
      expect(existsSync(outputPath)).toBe(true);

      const content = await readFile(outputPath, 'utf-8');
      expect(content).toContain('Written content');
    });

    it('should create directories if needed', async () => {
      // Arrange
      const outputPath = join(OUTPUT_DIR, 'nested', 'deep', 'file.ts');
      const templatePath = join(TEMPLATES_DIR, 'nested.njk');
      const templateContent = `---
to: ${outputPath}
---
Nested content`;

      await writeFile(templatePath, templateContent, 'utf-8');

      // Act
      await renderWithOptions(templatePath, [], {}, { dryRun: false });

      // Assert
      expect(existsSync(outputPath)).toBe(true);
    });

    it('should skip existing files with skip_existing mode', async () => {
      // Arrange
      const outputPath = join(OUTPUT_DIR, 'existing.ts');
      await writeFile(outputPath, 'Original content', 'utf-8');

      const templatePath = join(TEMPLATES_DIR, 'skip.njk');
      const templateContent = `---
to: ${outputPath}
mode: skip_existing
---
New content`;

      await writeFile(templatePath, templateContent, 'utf-8');

      // Act
      const result = await renderWithOptions(templatePath, [], {}, { dryRun: false });

      // Assert
      expect(result.written).toBe(false);
      expect(result.skipped).toBe(true);

      const content = await readFile(outputPath, 'utf-8');
      expect(content).toBe('Original content');
    });

    it('should append content with append mode', async () => {
      // Arrange
      const outputPath = join(OUTPUT_DIR, 'append.ts');
      await writeFile(outputPath, 'Line 1\n', 'utf-8');

      const templatePath = join(TEMPLATES_DIR, 'append.njk');
      const templateContent = `---
to: ${outputPath}
mode: append
---
Line 2`;

      await writeFile(templatePath, templateContent, 'utf-8');

      // Act
      await renderWithOptions(templatePath, [], {}, { dryRun: false });

      // Assert
      const content = await readFile(outputPath, 'utf-8');
      expect(content).toContain('Line 1');
      expect(content).toContain('Line 2');
    });

    it('should override output path with option', async () => {
      // Arrange
      const outputPath = join(OUTPUT_DIR, 'override.ts');
      const templatePath = join(TEMPLATES_DIR, 'override.njk');
      const templateContent = `---
to: original.ts
---
Override content`;

      await writeFile(templatePath, templateContent, 'utf-8');

      // Act
      const result = await renderWithOptions(
        templatePath,
        [],
        {},
        { dryRun: false, outputPath }
      );

      // Assert
      expect(result.outputPath).toBe(resolve(outputPath));
      expect(existsSync(outputPath)).toBe(true);
    });
  });

  describe('batchRender()', () => {
    it('should render multiple templates', async () => {
      // Arrange
      const template1Path = join(TEMPLATES_DIR, 'batch1.njk');
      const template2Path = join(TEMPLATES_DIR, 'batch2.njk');

      await writeFile(template1Path, `---\nto: output1.ts\n---\nContent 1`, 'utf-8');
      await writeFile(template2Path, `---\nto: output2.ts\n---\nContent 2`, 'utf-8');

      const templates = [
        { path: template1Path, sparqlResults: [] },
        { path: template2Path, sparqlResults: [] },
      ];

      // Act
      const results = await batchRender(templates, {}, { dryRun: true });

      // Assert
      expect(results).toHaveLength(2);
      expect(results[0].content).toContain('Content 1');
      expect(results[1].content).toContain('Content 2');
    });

    it('should pass shared context to all templates', async () => {
      // Arrange
      const template1Path = join(TEMPLATES_DIR, 'shared1.njk');
      const template2Path = join(TEMPLATES_DIR, 'shared2.njk');

      await writeFile(template1Path, `Project: {{ projectName }}`, 'utf-8');
      await writeFile(template2Path, `App: {{ projectName }}`, 'utf-8');

      const templates = [
        { path: template1Path },
        { path: template2Path },
      ];

      // Act
      const results = await batchRender(
        templates,
        { projectName: 'SharedApp' },
        { dryRun: true }
      );

      // Assert
      expect(results[0].content).toContain('Project: SharedApp');
      expect(results[1].content).toContain('App: SharedApp');
    });

    it('should allow template-specific context', async () => {
      // Arrange
      const template1Path = join(TEMPLATES_DIR, 'specific1.njk');
      const template2Path = join(TEMPLATES_DIR, 'specific2.njk');

      await writeFile(template1Path, `Module: {{ moduleName }}`, 'utf-8');
      await writeFile(template2Path, `Module: {{ moduleName }}`, 'utf-8');

      const templates = [
        { path: template1Path, context: { moduleName: 'Module1' } },
        { path: template2Path, context: { moduleName: 'Module2' } },
      ];

      // Act
      const results = await batchRender(templates, {}, { dryRun: true });

      // Assert
      expect(results[0].content).toContain('Module: Module1');
      expect(results[1].content).toContain('Module: Module2');
    });
  });

  describe('discoverTemplates()', () => {
    it('should discover templates in directory', async () => {
      // Arrange
      await writeFile(join(TEMPLATES_DIR, 'model.njk'), `---\nto: model.ts\n---\nModel`, 'utf-8');
      await writeFile(join(TEMPLATES_DIR, 'api.njk'), `---\nto: api.ts\n---\nAPI`, 'utf-8');
      await writeFile(join(TEMPLATES_DIR, 'readme.md'), `# README`, 'utf-8'); // Non-template

      // Act
      const templates = await discoverTemplates(TEMPLATES_DIR);

      // Assert
      expect(templates).toHaveLength(2);
      expect(templates.map(t => t.name)).toContain('model');
      expect(templates.map(t => t.name)).toContain('api');
    });

    it('should discover templates recursively', async () => {
      // Arrange
      const subDir = join(TEMPLATES_DIR, 'components');
      await mkdir(subDir, { recursive: true });

      await writeFile(join(TEMPLATES_DIR, 'root.njk'), `Root`, 'utf-8');
      await writeFile(join(subDir, 'button.njk'), `Button`, 'utf-8');

      // Act
      const templates = await discoverTemplates(TEMPLATES_DIR, { recursive: true });

      // Assert
      expect(templates).toHaveLength(2);
      expect(templates.map(t => t.name)).toContain('root');
      expect(templates.map(t => t.name)).toContain('button');
    });

    it('should extract frontmatter metadata', async () => {
      // Arrange
      await writeFile(
        join(TEMPLATES_DIR, 'meta.njk'),
        `---\nto: output/meta.ts\ndescription: Test template\n---\nContent`,
        'utf-8'
      );

      // Act
      const templates = await discoverTemplates(TEMPLATES_DIR);

      // Assert
      expect(templates).toHaveLength(1);
      expect(templates[0].hasOutputPath).toBe(true);
      expect(templates[0].description).toBe('Test template');
      expect(templates[0].frontmatter.to).toBe('output/meta.ts');
    });

    it('should support multiple extensions', async () => {
      // Arrange
      await writeFile(join(TEMPLATES_DIR, 'nunjucks.njk'), `NJK`, 'utf-8');
      await writeFile(join(TEMPLATES_DIR, 'tera.tera'), `TERA`, 'utf-8');
      await writeFile(join(TEMPLATES_DIR, 'jinja.jinja2'), `J2`, 'utf-8');

      // Act
      const templates = await discoverTemplates(TEMPLATES_DIR);

      // Assert
      expect(templates).toHaveLength(3);
      expect(templates.map(t => t.extension)).toContain('.njk');
      expect(templates.map(t => t.extension)).toContain('.tera');
      expect(templates.map(t => t.extension)).toContain('.jinja2');
    });

    it('should handle nonexistent directory', async () => {
      // Arrange
      const nonexistentDir = join(TEST_DIR, 'nonexistent');

      // Act & Assert
      await expect(discoverTemplates(nonexistentDir)).rejects.toThrow('Template directory not found');
    });
  });

  describe('RDF filters integration', () => {
    it('should render Turtle output with toTurtle filter', async () => {
      // Arrange
      const templatePath = join(TEMPLATES_DIR, 'turtle.njk');
      const templateContent = `{% set triples = [
        { s: 'ex:User', p: 'rdf:type', o: 'owl:Class' },
        { s: 'ex:name', p: 'rdf:type', o: 'owl:DatatypeProperty' }
      ] %}{{ triples | toTurtle }}`;

      await writeFile(templatePath, templateContent, 'utf-8');

      // Act
      const result = await renderTemplate(templatePath, []);

      // Assert
      expect(result.content).toContain('@prefix');
      expect(result.content).toContain('ex:User');
      expect(result.content).toContain('rdf:type');
    });

    it('should generate SPARQL with toSparql filter', async () => {
      // Arrange
      const templatePath = join(TEMPLATES_DIR, 'sparql.njk');
      const templateContent = `{% set pattern = { s: '?s', p: 'rdf:type', o: '?type' } %}{{ pattern | toSparql({ type: 'select', limit: 10 }) }}`;

      await writeFile(templatePath, templateContent, 'utf-8');

      // Act
      const result = await renderTemplate(templatePath, []);

      // Assert
      expect(result.content).toContain('SELECT');
      expect(result.content).toContain('WHERE');
      expect(result.content).toContain('LIMIT 10');
    });

    it('should format SPARQL results as markdown table', async () => {
      // Arrange
      const templatePath = join(TEMPLATES_DIR, 'table.njk');
      const templateContent = `{{ sparql_results | sparqlTable(['class', 'property']) }}`;

      await writeFile(templatePath, templateContent, 'utf-8');

      // Act
      const result = await renderTemplate(templatePath, SIMPLE_SPARQL_RESULTS);

      // Assert
      expect(result.content).toContain('| class | property |');
      expect(result.content).toContain('| --- | --- |');
      expect(result.content).toContain('| User | name |');
    });

    it('should escape Turtle strings', async () => {
      // Arrange
      const engine = await createTemplateEngine();
      const template = '{{ value | turtleEscape }}';

      // Act
      const result = engine.render(template, { value: 'Line1\nLine2\t"quoted"' });

      // Assert
      expect(result).toBe('Line1\\nLine2\\t\\"quoted\\"');
    });

    it('should escape SPARQL strings', async () => {
      // Arrange
      const engine = await createTemplateEngine();
      const template = '{{ value | sparqlEscape }}';

      // Act
      const result = engine.render(template, { value: "It's a \"test\"\nwith newlines" });

      // Assert
      expect(result).toContain("\\'");
      expect(result).toContain('\\"');
      expect(result).toContain('\\n');
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
