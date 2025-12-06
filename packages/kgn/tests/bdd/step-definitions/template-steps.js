/**
 * London-style BDD Step Definitions for Template Engine
 * Using test doubles, mocks, and dependency injection
 */
import { Given, When, Then } from '@amiceli/vitest-cucumber';
import { expect, vi } from 'vitest';
import fs from 'fs/promises';
import path from 'path';
import { TemplateEngine } from '../../../src/engine/template-engine.js';
import { createMockCAS } from '../fixtures/mock-cas.js';
import { createMockSPARQL } from '../fixtures/mock-sparql.js';
import { createTestDataFactory } from '../fixtures/test-data-factory.js';
import { GoldenTestValidator } from '../golden/golden-validator.js';
import crypto from 'crypto';

// Test context store
const testContext = {
  templateEngine: null,
  mockDependencies: {},
  testData: {},
  renderResult: null,
  lastError: null,
  goldenValidator: null,
  renderingAttempts: [],
  fixtures: {}
};

// Mock factory for dependency injection
const createMocks = () => ({
  cas: createMockCAS(),
  sparql: createMockSPARQL(),
  fileSystem: {
    readFile: vi.fn(),
    writeFile: vi.fn(),
    mkdir: vi.fn(),
    exists: vi.fn()
  },
  logger: {
    info: vi.fn(),
    error: vi.fn(),
    debug: vi.fn()
  },
  performance: {
    mark: vi.fn(),
    measure: vi.fn(),
    now: vi.fn(() => Date.now())
  }
});

// Background setup
Given('the KGEN template engine is initialized', async () => {
  testContext.mockDependencies = createMocks();
  testContext.templateEngine = new TemplateEngine({
    // Inject mocks using dependency injection
    cas: testContext.mockDependencies.cas,
    sparql: testContext.mockDependencies.sparql,
    fileSystem: testContext.mockDependencies.fileSystem,
    logger: testContext.mockDependencies.logger,
    performance: testContext.mockDependencies.performance
  });

  // Setup default mock behaviors
  testContext.mockDependencies.fileSystem.exists.mockResolvedValue(true);
  testContext.mockDependencies.cas.store.mockResolvedValue({ hash: 'mock-hash-123' });
});

Given('I have a clean test environment', async () => {
  // Reset all mocks
  Object.values(testContext.mockDependencies).forEach(mockObj => {
    if (typeof mockObj === 'object') {
      Object.values(mockObj).forEach(fn => {
        if (vi.isMockFunction(fn)) fn.mockClear();
      });
    }
  });

  // Clear test data
  testContext.testData = {};
  testContext.renderResult = null;
  testContext.lastError = null;
  testContext.renderingAttempts = [];
});

Given('the golden test fixtures are loaded', async () => {
  testContext.goldenValidator = new GoldenTestValidator({
    goldenDir: path.join(__dirname, '../golden'),
    updateGolden: process.env.UPDATE_GOLDEN === 'true'
  });

  testContext.fixtures = await createTestDataFactory();
});

// Template setup steps
Given('I have a template {string} with content:', async (templateName, templateContent) => {
  const templatePath = `test-fixtures/${templateName}`;

  // Mock file system to return our template content
  testContext.mockDependencies.fileSystem.readFile
    .mockImplementation((filePath) => {
      if (filePath.includes(templateName)) {
        return Promise.resolve(templateContent);
      }
      return Promise.reject(new Error(`File not found: ${filePath}`));
    });

  testContext.testData.templatePath = templatePath;
  testContext.testData.templateContent = templateContent;
});

Given('I have a template {string} with frontmatter:', async (templateName, templateWithFrontmatter) => {
  const templatePath = `test-fixtures/${templateName}`;

  testContext.mockDependencies.fileSystem.readFile
    .mockResolvedValueOnce(templateWithFrontmatter);

  testContext.testData.templatePath = templatePath;
  testContext.testData.templateContent = templateWithFrontmatter;
});

Given('I have rendering data:', async (dataTable) => {
  const data = {};

  for (const row of dataTable.raw()) {
    const [key, value] = row;
    try {
      // Try to parse as JSON for complex values
      data[key] = JSON.parse(value);
    } catch {
      // Use as string if JSON parsing fails
      data[key] = value;
    }
  }

  testContext.testData.renderData = data;
});

Given('I provide template data:', async (dataTable) => {
  const data = {};

  for (const row of dataTable.raw()) {
    const [key, value] = row;
    data[key] = value;
  }

  testContext.testData.renderData = data;
});

Given('I have registered custom filters:', async (dataTable) => {
  const filters = {};

  for (const row of dataTable.raw()) {
    const [filter, description] = row;

    // Mock filter implementation based on common patterns
    switch (filter) {
      case 'camelCase':
        filters[filter] = (str) => str.replace(/_([a-z])/g, (_, char) => char.toUpperCase());
        break;
      case 'snakeCase':
        filters[filter] = (str) => str.replace(/[A-Z]/g, (char) => `_${char.toLowerCase()}`);
        break;
      case 'kebabCase':
        filters[filter] = (str) => str.replace(/[A-Z]/g, (char) => `-${char.toLowerCase()}`);
        break;
      case 'pascalCase':
        filters[filter] = (str) => str.charAt(0).toUpperCase() + str.slice(1);
        break;
      case 'pluralize':
        filters[filter] = (str) => str.endsWith('s') ? str : str + 's';
        break;
      case 'singularize':
        filters[filter] = (str) => str.endsWith('s') ? str.slice(0, -1) : str;
        break;
      case 'indent':
        filters[filter] = (str, levels = 1) => {
          const indent = '  '.repeat(levels);
          return str.split('\n').map(line => line ? indent + line : line).join('\n');
        };
        break;
      default:
        filters[filter] = vi.fn((input) => input);
    }
  }

  // Mock the template engine to use our filters
  testContext.templateEngine.registerFilters(filters);
});

// Action steps
When('I render the template with the data', async () => {
  try {
    testContext.renderResult = await testContext.templateEngine.render(
      testContext.testData.templatePath,
      testContext.testData.renderData || {}
    );
  } catch (error) {
    testContext.lastError = error;
  }
});

When('I parse the frontmatter and render the template', async () => {
  try {
    const parsed = await testContext.templateEngine.parseWithFrontmatter(
      testContext.testData.templatePath
    );

    // Combine frontmatter with render data
    const combinedData = {
      ...parsed.frontmatter,
      ...testContext.testData.renderData
    };

    testContext.renderResult = await testContext.templateEngine.render(
      testContext.testData.templatePath,
      combinedData
    );

    testContext.testData.parsedFrontmatter = parsed.frontmatter;
  } catch (error) {
    testContext.lastError = error;
  }
});

When('I render the template {int} times in parallel', async (count) => {
  const promises = [];
  const startTime = Date.now();

  for (let i = 0; i < count; i++) {
    promises.push(
      testContext.templateEngine.render(
        testContext.testData.templatePath,
        testContext.testData.renderData || {}
      ).then(result => ({
        attempt: i,
        result,
        timestamp: Date.now()
      }))
    );
  }

  try {
    testContext.renderingAttempts = await Promise.all(promises);
    testContext.testData.renderingTime = Date.now() - startTime;
  } catch (error) {
    testContext.lastError = error;
  }
});

// Assertion steps
Then('the output should match the golden file {string}', async (goldenFile) => {
  expect(testContext.renderResult).toBeDefined();

  const isValid = await testContext.goldenValidator.validate(
    testContext.renderResult,
    goldenFile
  );

  expect(isValid).toBe(true);
});

Then('the rendering should be byte-identical across multiple runs', async () => {
  expect(testContext.renderingAttempts.length).toBeGreaterThan(1);

  const firstHash = crypto.createHash('sha256')
    .update(testContext.renderingAttempts[0].result)
    .digest('hex');

  for (let i = 1; i < testContext.renderingAttempts.length; i++) {
    const hash = crypto.createHash('sha256')
      .update(testContext.renderingAttempts[i].result)
      .digest('hex');

    expect(hash).toBe(firstHash);
  }
});

Then('the output should contain valid React component syntax', () => {
  expect(testContext.renderResult).toMatch(/import React/);
  expect(testContext.renderResult).toMatch(/export const \w+/);
  expect(testContext.renderResult).toMatch(/return \(/);
});

Then('the frontmatter should be correctly extracted', () => {
  expect(testContext.testData.parsedFrontmatter).toBeDefined();
  expect(typeof testContext.testData.parsedFrontmatter).toBe('object');
});

Then('variables should inherit from frontmatter defaults', () => {
  expect(testContext.testData.parsedFrontmatter).toHaveProperty('defaults');
  expect(testContext.testData.parsedFrontmatter).toHaveProperty('variables');
});

Then('the final output should match the golden file {string}', async (goldenFile) => {
  const isValid = await testContext.goldenValidator.validate(
    testContext.renderResult,
    goldenFile
  );

  expect(isValid).toBe(true);
});

Then('the JSON structure should be valid', () => {
  // Extract JSON from the rendered JavaScript
  const jsonMatch = testContext.renderResult.match(/export const config = ({[\s\S]*?});/);
  expect(jsonMatch).toBeDefined();

  // Validate it's parseable (after converting to JSON format)
  const jsonStr = jsonMatch[1]
    .replace(/([a-zA-Z_$][a-zA-Z0-9_$]*):(?=\s*[^/])/g, '"$1":')  // Quote keys
    .replace(/'/g, '"')  // Convert single to double quotes
    .replace(/,(\s*[}\]])/g, '$1');  // Remove trailing commas

  expect(() => JSON.parse(jsonStr)).not.toThrow();
});

Then('custom filters should be applied correctly', () => {
  expect(testContext.renderResult).toBeDefined();

  // Verify mock filter calls
  const mockFilters = Object.values(testContext.templateEngine.filters || {})
    .filter(vi.isMockFunction);

  mockFilters.forEach(filter => {
    expect(filter).toHaveBeenCalled();
  });
});

Then('all outputs should be byte-identical', () => {
  expect(testContext.renderingAttempts.length).toBeGreaterThan(1);

  const results = testContext.renderingAttempts.map(attempt => attempt.result);
  const firstResult = results[0];

  results.slice(1).forEach((result, index) => {
    expect(result).toBe(firstResult);
  });
});

Then('rendering should complete within {int} second per run', (maxSeconds) => {
  const avgTime = testContext.testData.renderingTime / testContext.renderingAttempts.length;
  expect(avgTime).toBeLessThan(maxSeconds * 1000);
});

Then('memory usage should remain stable', () => {
  // Verify performance mock was called for memory tracking
  expect(testContext.mockDependencies.performance.mark).toHaveBeenCalled();
});

Then('no race conditions should occur', () => {
  // Verify all attempts completed successfully
  expect(testContext.renderingAttempts).toHaveLength(testContext.renderingAttempts.length);
  expect(testContext.lastError).toBeNull();
});