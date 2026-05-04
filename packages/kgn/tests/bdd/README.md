# KGEN Templates - BDD Test Suite

## Overview

This comprehensive Behavior-Driven Development (BDD) test suite implements London-style TDD with extensive testing patterns for the KGEN Templates package. The suite ensures deterministic rendering, cross-platform compatibility, and performance benchmarks.

## 🎯 Key Features

- **London-style BDD**: Test-first development with dependency injection and mocking
- **Golden Tests**: Byte-identical output verification across runs
- **Permutation Tests**: Deterministic validation under various conditions
- **Cross-Platform Tests**: Consistent behavior on Linux, macOS, and Windows
- **Performance Benchmarks**: Throughput and memory usage validation
- **Integration Tests**: CAS, RDF/SPARQL, and SHACL validation

## 📁 Directory Structure

```
tests/bdd/
├── features/                    # Gherkin feature files
│   ├── core-template-rendering.feature
│   ├── advanced-template-features.feature
│   ├── cas-integration.feature
│   ├── rdf-sparql-integration.feature
│   └── shacl-validation.feature
├── step-definitions/            # London-style step implementations
│   └── template-steps.js
├── fixtures/                    # Test data and mocks
│   ├── test-data-factory.js
│   ├── mock-cas.js
│   └── mock-sparql.js
├── golden/                      # Golden test files and validator
│   ├── golden-validator.js
│   ├── component-golden.js
│   └── config-golden.js
├── permutation/                 # Permutation test runner
│   └── permutation-test-runner.js
├── cross-os/                    # Cross-platform test matrix
│   └── cross-platform-test-matrix.js
├── bdd-test-runner.js          # Main test orchestrator
├── run-bdd-tests.test.js       # Vitest integration
├── vitest.bdd.config.js        # Vitest configuration
└── README.md                   # This file
```

## 🚀 Quick Start

### Prerequisites

```bash
# Install dependencies
npm install

# Install development dependencies
npm install --save-dev vitest @amiceli/vitest-cucumber
```

### Running Tests

```bash
# Run all BDD tests
npm run test:bdd

# Run with coverage
npm run test:bdd:coverage

# Watch mode
npm run test:bdd:watch

# Run specific test categories
npm run test -- --grep "Core Template"
npm run test -- --grep "Golden Test"
npm run test -- --grep "Permutation"
npm run test -- --grep "Cross-Platform"
```

### Environment Variables

```bash
# Update golden files
UPDATE_GOLDEN=true npm run test:bdd

# Enable/disable test categories
ENABLE_PERMUTATION=false npm run test:bdd
ENABLE_CROSS_PLATFORM=false npm run test:bdd
ENABLE_PERFORMANCE=false npm run test:bdd

# Configure test parameters
PERMUTATION_ITERATIONS=20 npm run test:bdd
PARALLEL_RUNS=10 npm run test:bdd
```

## 🧪 Test Categories

### 1. Core Template Rendering Tests

Tests fundamental template engine capabilities:

- **Basic Rendering**: Variable substitution and Nunjucks syntax
- **Frontmatter Parsing**: YAML frontmatter extraction and merging
- **Custom Filters**: Filter registration and application
- **Deterministic Rendering**: Consistent output across multiple runs

Example:
```javascript
// Given/When/Then pattern
Given('I have a template "component.njk" with content:', async (content) => {
  await setupTemplate(content);
});

When('I render the template with the data', async () => {
  result = await templateEngine.render(templatePath, renderData);
});

Then('the output should match the golden file "component-golden.js"', async (goldenFile) => {
  const isValid = await goldenValidator.validate(result, goldenFile);
  expect(isValid).toBe(true);
});
```

### 2. Golden Tests

Ensures byte-identical outputs for deterministic validation:

- **Content Hashing**: SHA-256 verification of rendered outputs
- **Platform Normalization**: Line ending and path handling
- **Update Mechanism**: Automatic golden file updates when needed
- **Diff Generation**: Detailed comparisons when tests fail

### 3. Permutation Tests

Validates determinism under various conditions:

- **Multiple Iterations**: 10+ renders with identical inputs
- **Parallel Execution**: Concurrent rendering validation
- **Data Variations**: Different input combinations
- **Performance Tracking**: Execution time consistency

### 4. Cross-Platform Tests

Ensures consistent behavior across operating systems:

- **Path Handling**: Platform-specific path resolution
- **Line Endings**: Windows (CRLF) vs Unix (LF) normalization
- **File System Operations**: Directory creation and access
- **Environment Variables**: Platform-specific variable handling
- **Character Encoding**: Unicode and special character support

### 5. Integration Tests

Tests external system integrations:

- **CAS Integration**: Content-addressed storage operations
- **RDF/SPARQL**: Semantic data querying in templates
- **SHACL Validation**: Schema validation for generated RDF
- **Performance Benchmarks**: Throughput and memory usage

## 🎭 London-Style TDD Implementation

The test suite follows London-style TDD principles:

### Dependency Injection

```javascript
// Template engine with injected dependencies
const templateEngine = new TemplateEngine({
  cas: mockCAS,           // Mock CAS implementation
  sparql: mockSPARQL,     // Mock SPARQL engine
  fileSystem: mockFS,     // Mock file system
  logger: mockLogger      // Mock logger
});
```

### Test Doubles

```javascript
// Mock CAS with realistic behavior
const mockCAS = {
  store: vi.fn(async (content) => {
    const hash = generateHash(content);
    storage.set(hash, content);
    return { hash, stored: true };
  }),

  retrieve: vi.fn(async (hash) => {
    return storage.has(hash)
      ? { content: storage.get(hash), found: true }
      : { found: false };
  })
};
```

### Behavior Verification

```javascript
// Verify mock interactions
Then('CAS storage should be called with rendered content', () => {
  expect(mockCAS.store).toHaveBeenCalledWith(expectedContent, {
    createAttestation: true,
    metadata: expect.any(Object)
  });
});
```

## 📊 Performance Benchmarks

### Throughput Targets

- **Basic Templates**: > 1000 renders/second
- **Complex Templates**: > 100 renders/second
- **Large Datasets**: < 5 seconds for 10,000 items

### Memory Targets

- **Memory Growth**: < 100MB for large operations
- **Garbage Collection**: Proper cleanup after operations
- **Concurrent Operations**: Stable memory under load

### Example Performance Test

```javascript
async testRenderingThroughput() {
  const template = '{{ message }} - Item {{ index }}';
  const itemCount = 1000;
  const startTime = Date.now();

  for (let i = 0; i < itemCount; i++) {
    await templateEngine.render(template, {
      message: 'Performance test',
      index: i
    });
  }

  const duration = Date.now() - startTime;
  const throughput = itemCount / (duration / 1000);

  expect(throughput).toBeGreaterThan(100);
}
```

## 🔧 Configuration

### Vitest Configuration

The test suite uses a specialized Vitest configuration:

```javascript
// vitest.bdd.config.js
export default defineConfig({
  test: {
    environment: 'node',
    include: ['tests/bdd/**/*.test.js'],
    coverage: {
      thresholds: {
        lines: 80,
        functions: 80,
        branches: 75,
        statements: 80
      }
    },
    setupFiles: ['./tests/bdd/setup.js']
  }
});
```

### Test Modes

```javascript
// Different configurations for different test types
export const configs = {
  bdd: defineConfig({ /* standard BDD tests */ }),
  golden: defineConfig({ /* golden test specific */ }),
  permutation: defineConfig({ /* permutation tests */ }),
  crossPlatform: defineConfig({ /* cross-platform */ }),
  performance: defineConfig({ /* performance tests */ })
};
```

## 📈 Test Reports

### Coverage Reports

```bash
# Generate coverage reports
npm run test:bdd:coverage

# View HTML coverage report
open coverage/bdd/index.html
```

### Golden Test Reports

```bash
# Generate golden test report
UPDATE_GOLDEN=true npm run test:bdd

# View golden test differences
cat coverage/bdd/golden-diff-report.txt
```

### Performance Reports

```bash
# Run performance benchmarks
npm run test:bdd -- --grep "Performance"

# View performance report
cat coverage/bdd/performance-report.json
```

## 🐛 Troubleshooting

### Common Issues

1. **Golden Test Failures**
   ```bash
   # Update golden files if expected
   UPDATE_GOLDEN=true npm run test:bdd
   ```

2. **Platform-Specific Failures**
   ```bash
   # Run platform-specific tests only
   npm run test:bdd -- --grep "Cross-Platform"
   ```

3. **Performance Test Failures**
   ```bash
   # Reduce test load for slower systems
   PERMUTATION_ITERATIONS=5 npm run test:bdd
   ```

### Debug Mode

```bash
# Enable debug logging
DEBUG=kgen:* npm run test:bdd

# Run single test with detailed output
npm run test:bdd -- --grep "specific test" --reporter verbose
```

## 🤝 Contributing

### Adding New Tests

1. **Create Feature File**: Add `.feature` file in `features/`
2. **Implement Step Definitions**: Add steps in `step-definitions/`
3. **Create Golden Files**: Add expected outputs in `golden/`
4. **Update Test Data**: Add test data in `fixtures/test-data-factory.js`

### Test Guidelines

- Follow Given/When/Then structure
- Use dependency injection for external dependencies
- Create realistic test doubles
- Ensure deterministic behavior
- Add performance benchmarks for new features

### Example New Test

```javascript
// features/new-feature.feature
Feature: New Template Feature
  Scenario: Test new functionality
    Given I have a template with new feature
    When I render the template
    Then the output should be correct

// step-definitions/new-feature-steps.js
Given('I have a template with new feature', async () => {
  // Setup test
});

When('I render the template', async () => {
  // Execute action
});

Then('the output should be correct', async () => {
  // Verify result
});
```

## 📚 References

- [London vs Chicago TDD](https://softwareengineering.stackexchange.com/questions/123627/what-are-the-london-and-chicago-schools-of-tdd)
- [Vitest Documentation](https://vitest.dev/)
- [Cucumber.js](https://cucumber.io/docs/cucumber/)
- [Golden Testing](https://ro-che.info/articles/2017-12-04-golden-tests)

## 📄 License

MIT License - see LICENSE file for details.