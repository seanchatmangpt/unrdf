/**
 * Basic Integration Test - Manual validation of template system
 * Tests core functionality without external dependencies
 */

import { TemplateEngine } from '../src/engine/template-engine.js';
import { createCustomFilters } from '../src/filters/index.js';
import { FrontmatterParser } from '../src/parser/frontmatter.js';
import { VariableExtractor } from '../src/parser/variables.js';
import { TemplateLinter } from '../src/linter/determinism.js';
import { DeterministicRenderer } from '../src/renderer/deterministic.js';
import fs from 'fs/promises';
import path from 'path';

// Simple test framework
const tests = [];
const errors = [];

function test(name, fn) {
  tests.push({ name, fn });
}

async function runTests() {
  console.log(`🧪 Running ${tests.length} integration tests...\n`);
  
  for (const { name, fn } of tests) {
    try {
      console.log(`  ⏳ ${name}`);
      await fn();
      console.log(`  ✅ ${name}`);
    } catch (error) {
      console.log(`  ❌ ${name}: ${error.message}`);
      errors.push({ test: name, error });
    }
  }

  console.log(`\n📊 Test Results:`);
  console.log(`  • Passed: ${tests.length - errors.length}/${tests.length}`);
  console.log(`  • Failed: ${errors.length}/${tests.length}`);
  
  if (errors.length > 0) {
    console.log(`\n💥 Failures:`);
    errors.forEach(({ test, error }) => {
      console.log(`  • ${test}: ${error.message}`);
    });
    process.exit(1);
  }
  
  console.log(`\n🎉 All tests passed! Template system migration successful.`);
}

// Create temp directory for tests
const tempDir = await fs.mkdtemp(path.join(process.cwd(), 'test-templates-'));

// Test 1: Custom Filters
test('Custom filters work correctly', async () => {
  const filters = createCustomFilters({ deterministicMode: false });
  
  // String transformations
  if (filters.camelCase('hello-world') !== 'helloWorld') {
    throw new Error('camelCase filter failed');
  }
  
  if (filters.kebabCase('HelloWorld') !== 'hello-world') {
    throw new Error('kebabCase filter failed');
  }
  
  if (filters.pascalCase('hello world') !== 'HelloWorld') {
    throw new Error('pascalCase filter failed');
  }
  
  if (filters.snakeCase('Hello World') !== 'hello_world') {
    throw new Error('snakeCase filter failed');
  }

  // Hashing
  const hash1 = filters.hash('test content');
  const hash2 = filters.hash('test content');
  if (hash1 !== hash2) {
    throw new Error('hash filter not deterministic');
  }
  
  if (hash1.length !== 64) {
    throw new Error('hash filter wrong length');
  }
});

// Test 2: Frontmatter Parser
test('Frontmatter parser works correctly', async () => {
  const parser = new FrontmatterParser();
  
  const template = `---
name: Test Template
description: A test template
variables:
  title: Template title
---
# {{ title }}

Content goes here.`;

  const result = parser.parse(template);
  
  if (result.frontmatter.name !== 'Test Template') {
    throw new Error('Frontmatter name not parsed correctly');
  }
  
  if (!result.content.includes('# {{ title }}')) {
    throw new Error('Template content not extracted correctly');
  }
});

// Test 3: Variable Extractor
test('Variable extractor works correctly', async () => {
  const extractor = new VariableExtractor();
  
  const template = `# {{ title }}

{% for item in items %}
- {{ item.name }}
{% endfor %}

{% if author %}
By: {{ author }}
{% endif %}`;

  const result = extractor.extract(template);
  
  if (!result.variables.includes('title')) {
    throw new Error('title variable not extracted');
  }
  
  if (!result.variables.includes('items')) {
    throw new Error('items variable not extracted');
  }
  
  if (!result.variables.includes('author')) {
    throw new Error('author variable not extracted');
  }
});

// Test 4: Deterministic Renderer
test('Deterministic renderer produces consistent output', async () => {
  const renderer = new DeterministicRenderer({
    staticBuildTime: '2024-01-01T00:00:00.000Z'
  });
  
  // Mock Nunjucks environment
  const mockEnv = {
    renderString: (template, context) => {
      return template.replace('{{ timestamp }}', context.__deterministic.buildTime)
                   .replace('{{ title }}', context.title || '');
    }
  };
  
  const template = `Title: {{ title }}
Generated: {{ timestamp }}`;
  
  const context = { title: 'Test Page' };
  
  const result1 = await renderer.render(mockEnv, template, context);
  const result2 = await renderer.render(mockEnv, template, context);
  
  if (result1 !== result2) {
    throw new Error('Deterministic renderer not producing consistent output');
  }
  
  if (!result1.includes('2024-01-01T00:00:00.000Z')) {
    throw new Error('Deterministic renderer not using static time');
  }
});

// Test 5: Template Linter
test('Template linter detects non-deterministic operations', async () => {
  const linter = new TemplateLinter({ strict: true });
  
  const badTemplate = `---
name: Bad Template
---
Current time: {{ now }}
Random: {{ random }}`;

  const result = linter.lint(badTemplate, { name: 'Bad Template' });
  
  if (result.deterministic) {
    throw new Error('Linter should detect non-deterministic operations');
  }
  
  if (result.issues.length === 0) {
    throw new Error('Linter should report issues');
  }
  
  const errorIssues = result.issues.filter(issue => issue.severity === 'error');
  if (errorIssues.length === 0) {
    throw new Error('Linter should report error-level issues');
  }
});

// Test 6: Template Engine Integration
test('Template engine renders correctly', async () => {
  const templateContent = `---
name: Integration Test
description: Test template for integration
variables:
  greeting: Greeting text
  name: Name to greet
---
{{ greeting }}, {{ name | pascalCase }}!`;

  const templatePath = path.join(tempDir, 'integration.njk');
  await fs.writeFile(templatePath, templateContent);

  const engine = new TemplateEngine({
    templatesDir: tempDir,
    deterministicMode: true,
    strictMode: true
  });

  const result = await engine.render('integration.njk', {
    greeting: 'Hello',
    name: 'world test'
  });

  if (!result.success) {
    throw new Error(`Template rendering failed: ${result.error}`);
  }
  
  if (result.content.trim() !== 'Hello, WorldTest!') {
    throw new Error(`Unexpected output: ${result.content.trim()}`);
  }
  
  if (result.frontmatter.name !== 'Integration Test') {
    throw new Error('Frontmatter not processed correctly');
  }
  
  if (!result.variables.includes('greeting')) {
    throw new Error('Variables not extracted correctly');
  }
});

// Test 7: Deterministic Consistency
test('Template produces identical output across multiple runs', async () => {
  const templateContent = `---
name: Deterministic Test
---
Rendered at: {{ __meta.renderedAt }}
Hash: {{ content | hash }}
Content: {{ content }}`;

  const templatePath = path.join(tempDir, 'deterministic.njk');
  await fs.writeFile(templatePath, templateContent);

  const engine = new TemplateEngine({
    templatesDir: tempDir,
    deterministicMode: true,
    strictMode: true
  });

  const context = { content: 'test content' };
  
  const results = [];
  for (let i = 0; i < 3; i++) {
    const result = await engine.render('deterministic.njk', context);
    if (!result.success) {
      throw new Error(`Run ${i + 1} failed: ${result.error}`);
    }
    results.push(result.content);
  }

  // All outputs should be identical
  for (let i = 1; i < results.length; i++) {
    if (results[0] !== results[i]) {
      throw new Error(`Output differs between runs: ${results[0]} vs ${results[i]}`);
    }
  }
  
  // Should use deterministic timestamp
  if (!results[0].includes('2024-01-01T00:00:00.000Z')) {
    throw new Error('Not using deterministic timestamp');
  }
});

// Test 8: Template Linter Auto-fix
test('Template linter can auto-fix issues', async () => {
  const linter = new TemplateLinter({ strict: true });
  
  const templateWithIssues = `Current time: {{ now }}
Random value: {{ content | random }}`;

  const fixResult = linter.autoFix(templateWithIssues);
  
  if (!fixResult.modified) {
    throw new Error('Auto-fix should modify content');
  }
  
  if (fixResult.fixCount === 0) {
    throw new Error('Auto-fix should report fixes');
  }
  
  if (fixResult.fixed.includes('{{ now }}')) {
    throw new Error('Auto-fix should replace {{ now }}');
  }
});

// Run all tests
try {
  await runTests();
} catch (error) {
  console.error('Test runner failed:', error);
  process.exit(1);
} finally {
  // Cleanup
  try {
    await fs.rm(tempDir, { recursive: true, force: true });
  } catch (error) {
    console.warn('Failed to cleanup temp directory:', error.message);
  }
}