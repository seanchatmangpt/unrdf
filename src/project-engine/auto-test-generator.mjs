/**
 * @file Auto Test Generator - generates test suggestions from code analysis
 * @module project-engine/auto-test-generator
 */

import { DataFactory } from 'n3';
import { z } from 'zod';

const { namedNode } = DataFactory;

const NS = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  fs: 'http://example.org/unrdf/filesystem#',
  proj: 'http://example.org/unrdf/project#',
  dom: 'http://example.org/unrdf/domain#',
};

const InferTestPatternsOptionsSchema = z.object({
  fsStore: z
    .custom(val => val && typeof val.getQuads === 'function', {
      message: 'fsStore must be an RDF store with getQuads method',
    })
    .optional(),
  projectStore: z.custom(val => val && typeof val.getQuads === 'function', {
    message: 'projectStore must be an RDF store with getQuads method',
  }),
});

const AutoTestOptionsSchema = z.object({
  projectStore: z.custom(val => val && typeof val.getQuads === 'function', {
    message: 'projectStore must be an RDF store with getQuads method',
  }),
  domainStore: z
    .custom(val => val && typeof val.getQuads === 'function', {
      message: 'domainStore must be an RDF store with getQuads method',
    })
    .optional(),
  stackProfile: z
    .object({
      testFramework: z.string().nullable().optional(),
    })
    .passthrough()
    .optional(),
});

const TestSuggestionSchema = z.object({
  file: z.string(),
  testFile: z.string(),
  testType: z.enum(['unit', 'integration', 'e2e']),
  priority: z.enum(['critical', 'high', 'medium', 'low']),
  reason: z.string(),
  suggestedTests: z.array(z.string()),
});

/**
 * Generate test suggestions based on code analysis
 * @param {Object} options
 * @param {Store} options.projectStore - Project RDF store
 * @param {Store} [options.domainStore] - Domain model store
 * @param {Object} [options.stackProfile] - Stack profile
 * @returns {{ suggestions: Array, summary: string, coverage: number }}
 */
export function generateTestSuggestions(options) {
  const validated = AutoTestOptionsSchema.parse(options);
  const { projectStore, stackProfile } = validated;

  const suggestions = [];
  const fileQuads = projectStore.getQuads(null, namedNode(`${NS.fs}relativePath`), null);

  let totalFiles = 0;
  let filesWithTests = 0;

  for (const quad of fileQuads) {
    const filePath = quad.object.value;
    if (isTestFile(filePath) || isConfigFile(filePath) || !isSourceFile(filePath)) continue;

    totalFiles++;
    const testPath = generateTestPath(filePath, stackProfile);
    const hasTest = hasExistingTest(filePath, projectStore);

    if (hasTest) {
      filesWithTests++;
      continue;
    }

    const roleQuads = projectStore.getQuads(quad.subject, namedNode(`${NS.proj}roleString`), null);
    const role = roleQuads.length > 0 ? roleQuads[0].object.value : 'Unknown';
    const { testType, priority } = determineTestType(filePath, role);
    const suggestedTests = generateSuggestedTestCases(filePath, role);

    suggestions.push({
      file: filePath,
      testFile: testPath,
      testType,
      priority,
      reason: `${role} file without test`,
      suggestedTests,
    });
  }

  const priorityOrder = { critical: 0, high: 1, medium: 2, low: 3 };
  suggestions.sort((a, b) => priorityOrder[a.priority] - priorityOrder[b.priority]);

  const coverage = totalFiles > 0 ? Math.round((filesWithTests / totalFiles) * 100) : 100;
  const summary =
    suggestions.length > 0
      ? `${suggestions.length} files need tests (${coverage}% coverage)`
      : `All files have tests (${coverage}% coverage)`;

  return { suggestions, summary, coverage };
}

function isTestFile(filePath) {
  return (
    /\.(test|spec)\.(tsx?|jsx?|mjs)$/.test(filePath) ||
    /^(test|tests|__tests__|spec)\//.test(filePath)
  );
}

function isConfigFile(filePath) {
  return /\.(config|rc)\.(tsx?|jsx?|mjs|json)$/.test(filePath);
}

function isSourceFile(filePath) {
  return /\.(tsx?|jsx?|mjs)$/.test(filePath);
}

function generateTestPath(filePath, _stackProfile) {
  const ext = filePath.match(/\.(tsx?|jsx?|mjs)$/)?.[1] || 'mjs';
  const baseName = filePath.replace(/\.(tsx?|jsx?|mjs)$/, '');
  return `${baseName}.test.${ext}`;
}

function hasExistingTest(filePath, projectStore) {
  const baseName = filePath.replace(/\.(tsx?|jsx?|mjs)$/, '');
  const testPatterns = [`${baseName}.test.`, `${baseName}.spec.`];
  const allPaths = projectStore
    .getQuads(null, namedNode(`${NS.fs}relativePath`), null)
    .map(q => q.object.value);
  return testPatterns.some(pattern => allPaths.some(p => p.includes(pattern)));
}

function determineTestType(filePath, role) {
  const roleMap = {
    Api: { testType: 'integration', priority: 'critical' },
    Route: { testType: 'integration', priority: 'critical' },
    Service: { testType: 'unit', priority: 'high' },
    Schema: { testType: 'unit', priority: 'high' },
    Component: { testType: 'unit', priority: 'medium' },
  };
  return roleMap[role] || { testType: 'unit', priority: 'medium' };
}

function generateSuggestedTestCases(filePath, role) {
  const baseName = filePath
    .split('/')
    .pop()
    ?.replace(/\.(tsx?|jsx?|mjs)$/, '');
  const tests =
    role === 'Api' || role === 'Route'
      ? [
          'should handle GET requests',
          'should handle POST requests',
          'should return 400 for invalid input',
        ]
      : ['should work correctly with valid input', 'should handle edge cases'];
  return tests.map(t => `${baseName}: ${t}`);
}

/**
 * Infer test patterns from existing test files
 * @param {Object} options
 * @param {import('n3').Store} [options.fsStore] - Filesystem RDF store
 * @param {import('n3').Store} options.projectStore - Project RDF store
 * @returns {Object} Test patterns object
 */
export function inferTestPatterns(options) {
  const validated = InferTestPatternsOptionsSchema.parse(options);
  const { fsStore, _projectStore } = validated;

  // Default patterns
  const defaultPatterns = {
    testFramework: 'vitest',
    fileExtension: 'mjs',
    testSuffix: 'test',
    assertionPatterns: ['toBe', 'toEqual', 'toBeDefined', 'toContain'],
    describeBlocks: [],
    setupTeardown: {
      hasBeforeEach: false,
      hasAfterEach: false,
      hasBeforeAll: false,
      hasAfterAll: false,
    },
    imports: [],
  };

  // If no fsStore, return defaults
  if (!fsStore) {
    return defaultPatterns;
  }

  // Find test files
  const testFileQuads = fsStore.getQuads(null, namedNode(`${NS.fs}relativePath`), null);

  const testFiles = testFileQuads
    .map(q => ({
      path: q.object.value,
      subject: q.subject,
    }))
    .filter(f => isTestFile(f.path));

  // If no test files found, return defaults
  if (testFiles.length === 0) {
    return defaultPatterns;
  }

  // Analyze first test file content
  const firstTestFile = testFiles[0];
  const contentQuads = fsStore.getQuads(firstTestFile.subject, namedNode(`${NS.fs}content`), null);

  if (contentQuads.length === 0) {
    return defaultPatterns;
  }

  const content = contentQuads[0].object.value;
  const patterns = { ...defaultPatterns };

  // Detect test framework
  if (content.includes("from 'vitest'") || content.includes('from "vitest"')) {
    patterns.testFramework = 'vitest';
  } else if (content.includes("from 'jest'") || content.includes('from "jest"')) {
    patterns.testFramework = 'jest';
  } else if (content.includes("from 'mocha'") || content.includes('from "mocha"')) {
    patterns.testFramework = 'mocha';
  }

  // Detect file extension
  const extMatch = firstTestFile.path.match(/\.(tsx?|jsx?|mjs)$/);
  if (extMatch) {
    patterns.fileExtension = extMatch[1];
  }

  // Detect test suffix
  if (firstTestFile.path.includes('.spec.')) {
    patterns.testSuffix = 'spec';
  }

  // Extract describe blocks
  const describeMatches = content.matchAll(/describe\(['"]([^'"]+)['"]/g);
  for (const match of describeMatches) {
    patterns.describeBlocks.push(match[1]);
  }

  // Detect assertion patterns
  const assertionPatterns = [
    'toBe',
    'toEqual',
    'toBeDefined',
    'toBeUndefined',
    'toBeNull',
    'toBeTruthy',
    'toBeFalsy',
    'toContain',
    'toHaveLength',
    'toThrow',
    'toMatch',
  ];
  for (const pattern of assertionPatterns) {
    if (content.includes(`.${pattern}(`)) {
      if (!patterns.assertionPatterns.includes(pattern)) {
        patterns.assertionPatterns.push(pattern);
      }
    }
  }

  // Detect setup/teardown
  patterns.setupTeardown.hasBeforeEach = content.includes('beforeEach');
  patterns.setupTeardown.hasAfterEach = content.includes('afterEach');
  patterns.setupTeardown.hasBeforeAll = content.includes('beforeAll');
  patterns.setupTeardown.hasAfterAll = content.includes('afterAll');

  // Extract imports
  const importMatches = content.matchAll(/import\s+.*?\s+from\s+['"]([^'"]+)['"]/g);
  for (const match of importMatches) {
    if (!patterns.imports.includes(match[1])) {
      patterns.imports.push(match[1]);
    }
  }

  return patterns;
}

/**
 * Generate test skeleton from entity and patterns
 * @param {Object} options
 * @param {string} options.entity - Entity name
 * @param {Object} options.existingTestPatterns - Test patterns from inferTestPatterns
 * @param {import('n3').Store} [options.domainStore] - Domain model store
 * @returns {Object} Test skeleton object
 */
export function generateTestSkeleton(options) {
  const { entity, existingTestPatterns, domainStore } = options;

  // Use provided patterns or defaults
  const patterns = existingTestPatterns || {
    testFramework: 'vitest',
    fileExtension: 'mjs',
    testSuffix: 'test',
    assertionPatterns: ['toBe', 'toEqual', 'toBeDefined'],
    describeBlocks: [],
    setupTeardown: {
      hasBeforeEach: false,
      hasAfterEach: false,
      hasBeforeAll: false,
      hasAfterAll: false,
    },
    imports: [],
  };
  const ext = patterns.fileExtension || 'mjs';
  const suffix = patterns.testSuffix || 'test';
  const entityLower = entity.charAt(0).toLowerCase() + entity.slice(1);

  const filename = `${entityLower}.${suffix}.${ext}`;

  // Generate imports
  const imports =
    patterns.imports.length > 0
      ? patterns.imports.map(imp => `import { } from '${imp}'`).join('\n')
      : `import { describe, it, expect${patterns.setupTeardown.hasBeforeEach ? ', beforeEach' : ''} } from '${patterns.testFramework || 'vitest'}'`;

  // Generate content
  let content = `${imports}\n`;
  content += `import { ${entity} } from '../../src/${entityLower}.${ext}'\n\n`;
  content += `describe('${entity}', () => {\n`;

  if (patterns.setupTeardown.hasBeforeEach) {
    content += `  let ${entityLower}\n`;
    content += `  beforeEach(() => { ${entityLower} = new ${entity}() })\n`;
  }

  content += `  describe('creation', () => {\n`;
  content += `    it('should create ${entityLower} instance', () => { expect(${entityLower || `new ${entity}()`}).toBeDefined() })\n`;
  content += `  })\n`;
  content += `})\n`;

  // Generate suggested tests
  const suggestedTests = [`should create ${entity} instance`];
  const fieldTests = [];

  // Add field tests if domainStore provided
  if (domainStore) {
    const fieldQuads = domainStore.getQuads(
      namedNode(`${NS.dom}${entity}`),
      namedNode(`${NS.dom}hasField`),
      null
    );
    for (const quad of fieldQuads) {
      const fieldName = quad.object.value.split('.').pop();
      const testName = `should have ${fieldName} property`;
      suggestedTests.push(testName);
      fieldTests.push({ name: testName, field: fieldName });
    }
  }

  // Add field tests to content
  if (fieldTests.length > 0) {
    content += `  describe('properties', () => {\n`;
    for (const fieldTest of fieldTests) {
      content += `    it('${fieldTest.name}', () => { expect(${entityLower}.${fieldTest.field}).toBeDefined() })\n`;
    }
    content += `  })\n`;
  }

  return {
    filename,
    content,
    suggestedTests,
  };
}

/**
 * Score test coverage for an entity
 * @param {Object} options
 * @param {string} options.entity - Entity name
 * @param {string[]} [options.testFiles] - Array of test file paths
 * @param {string[]} [options.sourceFiles] - Array of source file paths
 * @returns {Object} Coverage score object
 */
export function scoreTestCoverage(options) {
  const { entity, testFiles = [], sourceFiles = [] } = options;

  const entityLower = entity.charAt(0).toLowerCase() + entity.slice(1);
  const entityKebab = entity
    .replace(/([A-Z])/g, '-$1')
    .toLowerCase()
    .slice(1);

  const matchingTests = testFiles.filter(f => {
    const lower = f.toLowerCase();
    return (
      lower.includes(entityLower) ||
      lower.includes(entityKebab) ||
      lower.includes(entity.toLowerCase())
    );
  });

  // If no source files provided, assume entity needs tests if no matching tests found
  const needsTests =
    sourceFiles.length > 0 ? matchingTests.length === 0 : matchingTests.length === 0;
  // Coverage: if source files exist, 100% if any tests found, 0% otherwise
  // Cap at 100% to handle cases where multiple test files match one source file
  const coverage =
    sourceFiles.length > 0
      ? matchingTests.length > 0
        ? 100
        : 0
      : matchingTests.length > 0
        ? 100
        : 0;

  return {
    coverage,
    needsTests,
    existingTests: matchingTests,
  };
}

/**
 * Generate test factory function
 * @param {string} entity - Entity name
 * @returns {string} Factory function code
 */
export function generateTestFactory(entity) {
  const _entityLower = entity.charAt(0).toLowerCase() + entity.slice(1);
  return `function create${entity}(overrides = {}) {
  return {
    id: 'test-id',
    ...overrides,
  };
}`;
}

export { TestSuggestionSchema };
