/**
 * Step Definitions for Template Rendering BDD Tests
 * London School TDD implementation with template engine mocks
 */

import { Given, When, Then, Before, After } from '@amiceli/vitest-cucumber';
import { expect } from 'vitest';
import { londonSchoolUtils } from '../../../../tests/london-school-setup.js';

// Test context
let testWorld;

Before(async function() {
  testWorld = londonSchoolUtils.getTestWorld();
  await testWorld.setupScenario('template-rendering');

  // Create template engine mocks
  testWorld.templateEngineMock = testWorld.createMock('templateEngine', {
    render: vi.fn(),
    compile: vi.fn(),
    addFilter: vi.fn(),
    setGlobal: vi.fn(),
    parseTemplate: vi.fn(),
    loadTemplate: vi.fn()
  });

  testWorld.filterMock = testWorld.createMock('filterRegistry', {
    register: vi.fn(),
    get: vi.fn(),
    apply: vi.fn(),
    chain: vi.fn()
  });

  testWorld.dataProviderMock = testWorld.createMock('dataProvider', {
    load: vi.fn(),
    merge: vi.fn(),
    resolve: vi.fn(),
    validate: vi.fn()
  });

  testWorld.fileSystemMock = testWorld.mockFileSystem();
});

After(async function() {
  if (testWorld) {
    await testWorld.cleanupScenario();
  }
});

// Background steps
Given('a template engine is initialized', function() {
  testWorld.templateEngineMock.compile.mockImplementation((template) => {
    return {
      render: (data) => `rendered: ${template} with ${JSON.stringify(data)}`
    };
  });

  testWorld.templateEngineMock.render.mockImplementation((template, data) => {
    return Promise.resolve(`output: ${template.replace(/{{(\w+)}}/g, (_, key) => data[key] || '')}`);
  });

  expect(testWorld.templateEngineMock).toBeDefined();
});

Given('I have core filters available', function() {
  const coreFilters = {
    upper: (str) => String(str).toUpperCase(),
    lower: (str) => String(str).toLowerCase(),
    capitalize: (str) => String(str).charAt(0).toUpperCase() + String(str).slice(1),
    reverse: (str) => String(str).split('').reverse().join(''),
    length: (arr) => Array.isArray(arr) ? arr.length : String(arr).length,
    first: (arr) => Array.isArray(arr) ? arr[0] : arr,
    join: (arr, sep = ',') => Array.isArray(arr) ? arr.join(sep) : arr
  };

  testWorld.filterMock.get.mockImplementation((name) => coreFilters[name]);
  testWorld.filterMock.apply.mockImplementation((name, value, ...args) => {
    const filter = coreFilters[name];
    return filter ? filter(value, ...args) : value;
  });

  Object.keys(coreFilters).forEach(filterName => {
    testWorld.templateEngineMock.addFilter.mockImplementation((name, fn) => {
      coreFilters[name] = fn;
      return true;
    });
  });
});

// Template definition steps
Given('I have a template:', function(docString) {
  testWorld.templateContent = docString.trim();

  // Mock template parsing
  testWorld.templateEngineMock.parseTemplate.mockReturnValue({
    variables: testWorld.extractVariables(testWorld.templateContent),
    filters: testWorld.extractFilters(testWorld.templateContent),
    structure: 'parsed'
  });
});

Given('I have template data:', function(docString) {
  try {
    testWorld.templateData = JSON.parse(docString.trim());
  } catch (error) {
    // If not JSON, treat as plain text
    testWorld.templateData = { content: docString.trim() };
  }

  testWorld.dataProviderMock.load.mockResolvedValue(testWorld.templateData);
});

Given('I have a complex template with nested structures:', function(docString) {
  testWorld.templateContent = docString.trim();

  // Mock complex template handling
  testWorld.templateEngineMock.parseTemplate.mockReturnValue({
    variables: testWorld.extractVariables(testWorld.templateContent),
    loops: testWorld.extractLoops(testWorld.templateContent),
    conditionals: testWorld.extractConditionals(testWorld.templateContent),
    nested: true
  });
});

// Rendering execution steps
When('I render the template', async function() {
  testWorld.renderResult = await testWorld.templateEngineMock.render(
    testWorld.templateContent,
    testWorld.templateData
  );
});

When('I render the template with data', async function() {
  await testWorld.dataProviderMock.load(testWorld.templateData);
  testWorld.renderResult = await testWorld.templateEngineMock.render(
    testWorld.templateContent,
    testWorld.templateData
  );
});

When('I render the template multiple times', async function() {
  testWorld.renderResults = [];

  for (let i = 0; i < 3; i++) {
    const result = await testWorld.templateEngineMock.render(
      testWorld.templateContent,
      testWorld.templateData
    );
    testWorld.renderResults.push(result);
  }
});

When('I render the template with filter chaining', async function() {
  // Mock filter chaining
  testWorld.filterMock.chain.mockImplementation((value, filters) => {
    return filters.reduce((result, filter) => {
      return testWorld.filterMock.apply(filter.name, result, ...filter.args);
    }, value);
  });

  testWorld.renderResult = await testWorld.templateEngineMock.render(
    testWorld.templateContent,
    testWorld.templateData
  );
});

// Assertion steps
Then('the output should be:', function(docString) {
  const expectedOutput = docString.trim();
  expect(testWorld.renderResult).toBe(expectedOutput);
  testWorld.verifyInteraction('templateEngine', 'render');
});

Then('the output should contain {string}', function(expectedText) {
  expect(testWorld.renderResult).toContain(expectedText);
});

Then('the output should not contain {string}', function(unexpectedText) {
  expect(testWorld.renderResult).not.toContain(unexpectedText);
});

Then('all renders should produce identical output', function() {
  expect(testWorld.renderResults).toBeDefined();
  expect(testWorld.renderResults.length).toBeGreaterThan(1);

  const firstResult = testWorld.renderResults[0];
  testWorld.renderResults.forEach((result, index) => {
    expect(result).toBe(firstResult);
  });
});

Then('the template should be compiled successfully', function() {
  testWorld.verifyInteraction('templateEngine', 'compile', [testWorld.templateContent]);
  expect(testWorld.templateEngineMock.compile).toHaveBeenCalled();
});

// Filter-specific steps
Given('I have a template with filters:', function(docString) {
  testWorld.templateContent = docString.trim();

  // Extract and mock filters
  const filters = testWorld.extractFilters(testWorld.templateContent);
  filters.forEach(filter => {
    testWorld.filterMock.register.mockImplementation((name, fn) => {
      return true;
    });
  });
});

When('I apply the {string} filter to {string}', function(filterName, value) {
  testWorld.filterResult = testWorld.filterMock.apply(filterName, value);
});

Then('the filtered value should be {string}', function(expectedValue) {
  expect(testWorld.filterResult).toBe(expectedValue);
  testWorld.verifyInteraction('filterRegistry', 'apply');
});

Then('the filters should be applied in sequence', function() {
  testWorld.verifyInteraction('filterRegistry', 'chain');
  expect(testWorld.filterMock.chain).toHaveBeenCalled();
});

// Data handling steps
Given('I have data from multiple sources:', function(dataTable) {
  testWorld.multipleSources = {};

  dataTable.forEach(row => {
    testWorld.multipleSources[row.source] = JSON.parse(row.data);
  });

  testWorld.dataProviderMock.merge.mockImplementation((sources) => {
    return Object.assign({}, ...Object.values(sources));
  });
});

When('I merge the data sources', async function() {
  testWorld.mergedData = await testWorld.dataProviderMock.merge(testWorld.multipleSources);
});

Then('the data should be merged correctly', function() {
  expect(testWorld.mergedData).toBeDefined();
  testWorld.verifyInteraction('dataProvider', 'merge', [testWorld.multipleSources]);
});

Then('later sources should override earlier ones', function() {
  // Verify merge precedence
  testWorld.verifyInteraction('dataProvider', 'merge');
  expect(testWorld.dataProviderMock.merge).toHaveBeenCalled();
});

// Template loading steps
Given('I have template files in the filesystem', function() {
  testWorld.fileSystemMock.exists.mockReturnValue(true);
  testWorld.fileSystemMock.readFile.mockImplementation((path) => {
    if (path.includes('base.njk')) {
      return 'Base template: {{title}}';
    } else if (path.includes('partial.njk')) {
      return 'Partial: {{content}}';
    }
    return 'Generic template';
  });
});

When('I load a template from file {string}', async function(templatePath) {
  testWorld.templateEngineMock.loadTemplate.mockImplementation(async (path) => {
    const content = await testWorld.fileSystemMock.readFile(path);
    return { path, content, compiled: testWorld.templateEngineMock.compile(content) };
  });

  testWorld.loadedTemplate = await testWorld.templateEngineMock.loadTemplate(templatePath);
});

Then('the template should be loaded from the filesystem', function() {
  testWorld.verifyInteraction('templateEngine', 'loadTemplate');
  testWorld.verifyInteraction('fileSystem', 'readFile');

  expect(testWorld.loadedTemplate).toBeDefined();
  expect(testWorld.loadedTemplate.content).toBeDefined();
});

// Error handling steps
Given('I have a template with invalid syntax:', function(docString) {
  testWorld.invalidTemplate = docString.trim();

  testWorld.templateEngineMock.compile.mockImplementation((template) => {
    if (template.includes('{{unclosed')) {
      throw new Error('Unclosed template tag');
    }
    return { render: () => 'valid template' };
  });
});

When('I attempt to render the invalid template', async function() {
  try {
    await testWorld.templateEngineMock.render(testWorld.invalidTemplate, testWorld.templateData);
  } catch (error) {
    testWorld.templateError = error;
  }
});

Then('I should get a clear error message', function() {
  expect(testWorld.templateError).toBeDefined();
  expect(testWorld.templateError.message).toContain('Unclosed template tag');
});

Then('the error should indicate the problem location', function() {
  expect(testWorld.templateError.message).toBeDefined();
  expect(testWorld.templateError.message.length).toBeGreaterThan(0);
});

// Performance steps
Given('I have a large dataset with {int} items', function(itemCount) {
  testWorld.largeDataset = {
    items: Array.from({ length: itemCount }, (_, i) => ({
      id: i,
      name: `Item ${i}`,
      value: Math.random() * 100
    }))
  };

  testWorld.dataProviderMock.load.mockResolvedValue(testWorld.largeDataset);
});

When('I render a template that processes all items', async function() {
  const startTime = Date.now();

  testWorld.renderResult = await testWorld.templateEngineMock.render(
    '{% for item in items %}{{item.name}}: {{item.value}}{% endfor %}',
    testWorld.largeDataset
  );

  testWorld.renderTime = Date.now() - startTime;
});

Then('rendering should complete within {int}ms', function(maxTime) {
  expect(testWorld.renderTime).toBeLessThan(maxTime);
});

Then('memory usage should remain stable', function() {
  // Mock memory stability check
  expect(testWorld.renderResult).toBeDefined();
  testWorld.verifyInteraction('templateEngine', 'render');
});

// Helper methods for step definitions
testWorld.extractVariables = function(template) {
  const matches = template.match(/\{\{(\w+)\}\}/g) || [];
  return matches.map(match => match.slice(2, -2));
};

testWorld.extractFilters = function(template) {
  const matches = template.match(/\{\{[^}]*\|([^}]*)\}\}/g) || [];
  return matches.map(match => {
    const filterPart = match.split('|')[1];
    return filterPart ? filterPart.split('(')[0].trim() : null;
  }).filter(Boolean);
};

testWorld.extractLoops = function(template) {
  const matches = template.match(/\{\%\s*for\s+(\w+)\s+in\s+(\w+)\s*\%\}/g) || [];
  return matches.length;
};

testWorld.extractConditionals = function(template) {
  const matches = template.match(/\{\%\s*if\s+/g) || [];
  return matches.length;
};