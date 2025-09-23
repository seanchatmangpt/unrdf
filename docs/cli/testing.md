# CLI Testing

This guide covers testing CLI applications using UNRDF and the citty-test-utils framework.

## Table of Contents

- [Overview](#overview)
- [Basic Testing](#basic-testing)
- [Scenario Testing](#scenario-testing)
- [Environment Testing](#environment-testing)
- [Advanced Testing](#advanced-testing)
- [Best Practices](#best-practices)

## Overview

UNRDF provides comprehensive testing utilities for CLI applications through the [citty-test-utils](https://github.com/seanchatmangpt/citty-test-utils) framework. This allows you to:

- Test CLI commands and options
- Validate output and error handling
- Test in different environments
- Create complex test scenarios
- Automate CLI testing workflows

## Basic Testing

### Simple Command Testing

```javascript
import { runLocalCitty } from 'citty-test-utils';

// Test basic help command
const result = await runLocalCitty(['--help'], {
  cwd: './my-cli-project',
  env: { DEBUG: 'true' }
});

result
  .expectSuccess()
  .expectOutput('UNRDF Command Line Interface')
  .expectNoStderr();
```

### Testing with Options

```javascript
// Test command with options
const result = await runLocalCitty([
  'parse',
  './test-data/sample.ttl',
  '--format', 'turtle',
  '--output', './output.ttl'
], {
  cwd: './my-cli-project',
  env: { DEBUG: 'true' }
});

result
  .expectSuccess()
  .expectOutput('Parsed 4 triples successfully')
  .expectNoStderr();
```

### Error Testing

```javascript
// Test error handling
const result = await runLocalCitty([
  'parse',
  './nonexistent.ttl'
], {
  cwd: './my-cli-project',
  env: { DEBUG: 'true' }
});

result
  .expectFailure()
  .expectStderr(/Parse error/);
```

## Scenario Testing

### Multi-Step Workflows

```javascript
import { scenario } from 'citty-test-utils';

// Test complete workflow
const result = await scenario('RDF Processing Pipeline')
  .step('Parse RDF data')
  .run('parse', './test-data/sample.ttl')
  .expectSuccess()
  .expectOutput('Parsed 4 triples successfully')
  .step('Query the parsed data')
  .run('query', './test-data/sample.ttl', '--query', 'SELECT ?name WHERE { ?person foaf:name ?name }')
  .expectSuccess()
  .expectOutput('John Doe')
  .step('Validate the data')
  .run('validate', './test-data/sample.ttl', './test-data/shape.ttl')
  .expectSuccess()
  .expectOutput('Conforms: true')
  .execute('local', { cwd: './my-cli-project' });

console.log(`Workflow completed: ${result.success ? 'SUCCESS' : 'FAILED'}`);
```

### Custom Actions in Scenarios

```javascript
// Test with custom actions
const result = await scenario('Custom Workflow')
  .step('Setup test data', async ({ context }) => {
    // Create test data
    const testData = `
      @prefix ex: <http://example.org/> .
      ex:test a ex:Person .
    `;
    
    await writeFile('./test-data/custom.ttl', testData);
    context.dataCreated = true;
    
    return { success: true, message: 'Test data created' };
  })
  .step('Process custom data')
  .run('parse', './test-data/custom.ttl')
  .expectSuccess()
  .step('Cleanup', async ({ context }) => {
    if (context.dataCreated) {
      await rm('./test-data/custom.ttl', { force: true });
      return { success: true, message: 'Test data cleaned up' };
    }
    return { success: true, message: 'No cleanup needed' };
  })
  .execute('local', { cwd: './my-cli-project' });
```

### Pre-built Scenarios

```javascript
import { scenarios } from 'citty-test-utils';

// Use pre-built scenarios
const helpResult = await scenarios.help('local').execute({ cwd: './my-cli-project' });
const versionResult = await scenarios.version('local').execute({ cwd: './my-cli-project' });

console.log(`Help test: ${helpResult.success ? 'PASSED' : 'FAILED'}`);
console.log(`Version test: ${versionResult.success ? 'PASSED' : 'FAILED'}`);
```

## Environment Testing

### Local Environment Testing

```javascript
// Test in local development environment
const result = await runLocalCitty(['--help'], {
  cwd: './my-cli-project',
  env: {
    DEBUG: 'true',
    NODE_ENV: 'test'
  },
  timeout: 30000
});

result
  .expectSuccess()
  .expectOutput('UNRDF Command Line Interface');
```

### Cleanroom Environment Testing

```javascript
import { setupCleanroom, runCitty, teardownCleanroom } from 'citty-test-utils';

// Test in isolated Docker environment
await setupCleanroom({ 
  rootDir: './my-cli-project',
  nodeImage: 'node:18-alpine'
});

const result = await runCitty(['--help'], {
  env: { DEBUG: 'true' }
});

result
  .expectSuccess()
  .expectOutput('UNRDF Command Line Interface');

await teardownCleanroom();
```

### Cross-Environment Comparison

```javascript
// Compare results between environments
const localResult = await runLocalCitty(['--version'], {
  cwd: './my-cli-project',
  env: { DEBUG: 'true' }
});

await setupCleanroom({ 
  rootDir: './my-cli-project',
  nodeImage: 'node:18-alpine'
});

const cleanroomResult = await runCitty(['--version'], {
  env: { DEBUG: 'true' }
});

await teardownCleanroom();

// Compare outputs
expect(localResult.result.stdout).toBe(cleanroomResult.result.stdout);
```

## Advanced Testing

### Performance Testing

```javascript
// Test command performance
const startTime = Date.now();

const result = await runLocalCitty([
  'parse',
  './large-data.ttl'
], {
  cwd: './my-cli-project',
  env: { DEBUG: 'false' } // Disable debug for cleaner timing
});

const endTime = Date.now();
const duration = endTime - startTime;

result.expectSuccess();
expect(duration).toBeLessThan(5000); // Should complete within 5 seconds
```

### Retry and Resilience Testing

```javascript
import { testUtils } from 'citty-test-utils';

// Test flaky operations with retry
await testUtils.retry(async () => {
  const result = await runLocalCitty(['--help'], {
    cwd: './my-cli-project',
    env: { DEBUG: 'true' }
  });
  result.expectSuccess();
}, 3, 1000); // Retry 3 times with 1 second delay
```

### Output Format Testing

```javascript
// Test different output formats
const formats = ['table', 'json', 'turtle'];

for (const format of formats) {
  const result = await runLocalCitty([
    'query',
    './test-data/sample.ttl',
    '--query', 'SELECT ?name WHERE { ?person foaf:name ?name } LIMIT 1',
    '--format', format
  ], {
    cwd: './my-cli-project',
    env: { DEBUG: 'true' }
  });

  result.expectSuccess();
  
  // Validate format-specific output
  switch (format) {
    case 'json':
      expect(result.result.stdout).toMatch(/\[.*\]/);
      break;
    case 'table':
      expect(result.result.stdout).toContain('name');
      break;
    case 'turtle':
      expect(result.result.stdout).toContain('@prefix');
      break;
  }
}
```

### Error Scenario Testing

```javascript
// Test various error scenarios
const errorScenarios = [
  {
    name: 'Invalid file path',
    args: ['parse', './nonexistent.ttl'],
    expectedError: /Parse error/
  },
  {
    name: 'Invalid SPARQL query',
    args: ['query', './test-data/sample.ttl', '--query', 'INVALID SPARQL'],
    expectedError: /Query error/
  },
  {
    name: 'Missing required arguments',
    args: ['validate', './test-data/sample.ttl'],
    expectedError: /Validation error/
  }
];

for (const scenario of errorScenarios) {
  const result = await runLocalCitty(scenario.args, {
    cwd: './my-cli-project',
    env: { DEBUG: 'true' }
  });

  result
    .expectFailure()
    .expectStderr(scenario.expectedError);
  
  console.log(`✅ ${scenario.name} test passed`);
}
```

## Vitest Integration

### Test Suite Setup

```javascript
import { describe, it, beforeAll, afterAll, expect } from 'vitest';
import { 
  runLocalCitty, 
  setupCleanroom, 
  runCitty, 
  teardownCleanroom 
} from 'citty-test-utils';

describe('UNRDF CLI Tests', () => {
  beforeAll(async () => {
    // Setup test data
    await setupTestData();
  });

  afterAll(async () => {
    // Cleanup test data
    await cleanupTestData();
  });

  it('should show help information', async () => {
    const result = await runLocalCitty(['--help'], {
      cwd: './my-cli-project',
      env: { DEBUG: 'true' }
    });

    result
      .expectSuccess()
      .expectOutput('UNRDF Command Line Interface')
      .expectNoStderr();
  });

  it('should work in cleanroom environment', async () => {
    await setupCleanroom({ 
      rootDir: './my-cli-project',
      nodeImage: 'node:18-alpine'
    });

    const result = await runCitty(['--help'], {
      env: { DEBUG: 'true' }
    });

    result
      .expectSuccess()
      .expectOutput('UNRDF Command Line Interface')
      .expectNoStderr();

    await teardownCleanroom();
  });
});
```

### Test Configuration

```javascript
// vitest.config.mjs
import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    include: ['**/*.test.mjs', '**/*.spec.mjs'],
    timeout: 30000, // 30 seconds for CLI operations
    testTimeout: 30000,
    hookTimeout: 30000,
    teardownTimeout: 30000
  }
});
```

## Best Practices

### 1. Use Appropriate Test Environments

```javascript
// Use local environment for development
const localResult = await runLocalCitty(['--help'], {
  cwd: './my-cli-project',
  env: { DEBUG: 'true' }
});

// Use cleanroom environment for CI/CD
await setupCleanroom({ 
  rootDir: './my-cli-project',
  nodeImage: 'node:18-alpine'
});

const cleanroomResult = await runCitty(['--help'], {
  env: { DEBUG: 'true' }
});

await teardownCleanroom();
```

### 2. Test Error Scenarios

```javascript
// Always test error handling
const result = await runLocalCitty(['invalid-command'], {
  cwd: './my-cli-project',
  env: { DEBUG: 'true' }
});

result
  .expectFailure()
  .expectStderr(/Unknown command/);
```

### 3. Use Meaningful Test Names

```javascript
// Good test names
it('should parse Turtle data successfully', async () => {
  // test implementation
});

it('should handle invalid file paths gracefully', async () => {
  // test implementation
});

// Avoid vague test names
it('should work', async () => {
  // test implementation
});
```

### 4. Test Output Formats

```javascript
// Test different output formats
const formats = ['table', 'json', 'turtle'];

for (const format of formats) {
  it(`should output ${format} format correctly`, async () => {
    const result = await runLocalCitty([
      'query',
      './test-data/sample.ttl',
      '--query', 'SELECT ?name WHERE { ?person foaf:name ?name }',
      '--format', format
    ], {
      cwd: './my-cli-project',
      env: { DEBUG: 'true' }
    });

    result.expectSuccess();
    // Validate format-specific output
  });
}
```

### 5. Use Test Data Management

```javascript
// Setup and cleanup test data
async function setupTestData() {
  await mkdir('./test-data', { recursive: true });
  
  const testData = `
    @prefix ex: <http://example.org/> .
    ex:test a ex:Person .
  `;
  
  await writeFile('./test-data/sample.ttl', testData);
}

async function cleanupTestData() {
  await rm('./test-data', { recursive: true, force: true });
}
```

### 6. Test Performance

```javascript
// Test command performance
it('should complete within reasonable time', async () => {
  const startTime = Date.now();
  
  const result = await runLocalCitty([
    'parse',
    './test-data/sample.ttl'
  ], {
    cwd: './my-cli-project',
    env: { DEBUG: 'false' }
  });

  const endTime = Date.now();
  const duration = endTime - startTime;

  result.expectSuccess();
  expect(duration).toBeLessThan(5000);
});
```

## Common Patterns

### 1. Basic Command Testing

```javascript
const testCommand = async (args, expectedOutput) => {
  const result = await runLocalCitty(args, {
    cwd: './my-cli-project',
    env: { DEBUG: 'true' }
  });

  result
    .expectSuccess()
    .expectOutput(expectedOutput)
    .expectNoStderr();
};
```

### 2. Error Testing

```javascript
const testError = async (args, expectedError) => {
  const result = await runLocalCitty(args, {
    cwd: './my-cli-project',
    env: { DEBUG: 'true' }
  });

  result
    .expectFailure()
    .expectStderr(expectedError);
};
```

### 3. Scenario Testing

```javascript
const testScenario = async (name, steps) => {
  let scenario = scenario(name);
  
  for (const step of steps) {
    scenario = scenario.step(step.name);
    
    if (step.run) {
      scenario = scenario.run(...step.run);
    }
    
    if (step.expectSuccess) {
      scenario = scenario.expectSuccess();
    }
    
    if (step.expectOutput) {
      scenario = scenario.expectOutput(step.expectOutput);
    }
  }
  
  const result = await scenario.execute('local', { cwd: './my-cli-project' });
  return result;
};
```

## Troubleshooting

### Common Issues

1. **Test timeouts**
   ```javascript
   // Increase timeout for slow operations
   const result = await runLocalCitty(['--help'], {
     cwd: './my-cli-project',
     timeout: 60000
   });
   ```

2. **Environment issues**
   ```javascript
   // Use cleanroom environment for consistent results
   await setupCleanroom({ 
     rootDir: './my-cli-project',
     nodeImage: 'node:18-alpine'
   });
   ```

3. **File path issues**
   ```javascript
   // Use absolute paths
   const result = await runLocalCitty(['--help'], {
     cwd: path.resolve('./my-cli-project'),
     env: { DEBUG: 'true' }
   });
   ```

### Debug Mode

```javascript
// Enable debug mode for troubleshooting
const result = await runLocalCitty(['--help'], {
  cwd: './my-cli-project',
  env: { 
    DEBUG: 'true',
    UNRDF_DEBUG: 'true'
  }
});
```

## Next Steps

- Explore [CLI Examples](./examples.md) for practical usage
- Check out the [playground examples](../../playground/) for interactive learning
- Learn about [CLI Commands](./commands.md) for detailed command reference
- Try the [playground examples](../../playground/examples/cli-testing.mjs) for hands-on experience
