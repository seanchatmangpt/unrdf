# OTEL Span-Based Validation Guide

## Overview

This guide explains how to use OpenTelemetry (OTEL) spans for feature validation instead of traditional unit tests. The OTEL validation framework analyzes spans, metrics, and traces to determine if features are working correctly.

## Why OTEL Validation?

### Traditional Testing Problems
- **Isolated**: Tests run in isolation, not reflecting real usage
- **Mocked**: Dependencies are mocked, hiding integration issues
- **Static**: Tests don't capture runtime behavior
- **Maintenance**: High maintenance overhead for test code

### OTEL Validation Benefits
- **Real Usage**: Validates actual feature execution with real data
- **Integration**: Captures end-to-end behavior across components
- **Runtime**: Monitors performance, errors, and resource usage
- **Observability**: Provides insights into system behavior

## Architecture

```
Feature Execution → OTEL Spans → Validation Framework → Score/Report
     ↓                    ↓              ↓                ↓
  Real Usage         Trace Data    Span Analysis    Pass/Fail
```

## Core Components

### 1. OTELValidator
Main validation engine that:
- Executes features and collects OTEL spans
- Analyzes span attributes, status, and duration
- Validates against expected patterns
- Generates validation scores

### 2. ValidationHelpers
Utility functions for:
- Asserting span existence and attributes
- Checking span status and duration
- Validating performance metrics
- Creating custom validation rules

### 3. ValidationRunner
Orchestrates validation execution:
- Runs validation suites
- Manages parallel/sequential execution
- Generates comprehensive reports
- Handles timeouts and retries

## Usage Examples

### Basic Feature Validation

```javascript
import { createOTELValidator } from '../src/validation/index.mjs';

const validator = createOTELValidator();

const result = await validator.validateFeature('knowledge-engine', {
  expectedSpans: ['parse.turtle', 'query.sparql', 'validate.shacl'],
  requiredAttributes: ['service.name', 'operation.type'],
  performanceThresholds: {
    maxLatency: 1000,
    maxErrorRate: 0.01,
    minThroughput: 1,
    maxMemoryUsage: 50 * 1024 * 1024
  },
  validationRules: [
    // Custom validation rules
  ]
});

console.log(`Score: ${result.score}/100`);
console.log(`Passed: ${result.passed}`);
```

### Validation Suite

```javascript
import { createValidationRunner } from '../src/validation/index.mjs';

const runner = createValidationRunner({ verbose: true });

const suite = {
  name: 'my-features',
  description: 'Validation suite for my features',
  features: [
    {
      name: 'feature-1',
      description: 'First feature',
      config: {
        expectedSpans: ['feature1.operation'],
        requiredAttributes: ['input.size', 'output.size'],
        performanceThresholds: {
          maxLatency: 500,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 25 * 1024 * 1024
        },
        validationRules: []
      }
    }
  ],
  globalConfig: {
    timeout: 30000,
    retries: 1,
    parallel: false
  }
};

const report = await runner.runSuite(suite);
console.log(`Overall Score: ${report.summary.score}/100`);
```

### Custom Validation Rules

```javascript
import { createValidationHelpers } from '../src/validation/index.mjs';

const helpers = createValidationHelpers();

// Create custom validation rule
const customRule = helpers.createValidationRule(
  'custom-check',
  (spans, metrics) => {
    // Custom validation logic
    const operationSpan = spans.find(s => s.name === 'my.operation');
    return operationSpan && operationSpan.attributes.success === true;
  },
  'error'
);

// Use in feature configuration
const featureConfig = {
  // ... other config
  validationRules: [customRule]
};
```

## Validation Patterns

### 1. Span Existence Validation
Ensure required spans are created during feature execution:

```javascript
const config = {
  expectedSpans: [
    'parse.turtle',
    'query.sparql',
    'validate.shacl'
  ],
  // ... other config
};
```

### 2. Attribute Validation
Verify spans have required attributes:

```javascript
const config = {
  requiredAttributes: [
    'service.name',
    'operation.type',
    'input.size',
    'output.size'
  ],
  // ... other config
};
```

### 3. Performance Validation
Check performance metrics against thresholds:

```javascript
const config = {
  performanceThresholds: {
    maxLatency: 1000,        // 1 second
    maxErrorRate: 0.01,      // 1%
    minThroughput: 1,        // 1 operation per validation
    maxMemoryUsage: 50 * 1024 * 1024 // 50MB
  },
  // ... other config
};
```

### 4. Status Validation
Ensure spans have correct status:

```javascript
const config = {
  validationRules: [
    helpers.createSpanStatusRule('parse.turtle', 'ok'),
    helpers.createSpanStatusRule('query.sparql', 'ok'),
    helpers.createSpanStatusRule('validate.shacl', 'ok')
  ],
  // ... other config
};
```

## Running Validations

### Command Line

```bash
# Run comprehensive validation
node validation/run-all.mjs comprehensive

# Run individual suites
node validation/run-all.mjs individual

# Run specific feature validation
node validation/knowledge-engine.validation.mjs
node validation/cli.validation.mjs
```

### Programmatic

```javascript
import { runAllValidations } from './validation/run-all.mjs';

// Run comprehensive validation
const results = await runAllValidations({ mode: 'comprehensive' });

// Run individual suites
const results = await runAllValidations({ mode: 'individual' });
```

## Validation Results

### Score Calculation
Validation scores are calculated based on:
- **Span Validation (40%)**: Required spans exist with correct attributes
- **Metric Validation (40%)**: Performance metrics meet thresholds
- **Rule Validation (20%)**: Custom validation rules pass

### Passing Criteria
- **Score ≥ 80**: Feature passes validation
- **Score < 80**: Feature fails validation
- **All spans OK**: No error spans in the trace
- **Performance OK**: All metrics within thresholds

### Report Format

```javascript
{
  suite: 'knowledge-engine',
  timestamp: '2025-01-01T00:00:00.000Z',
  summary: {
    total: 6,
    passed: 5,
    failed: 1,
    skipped: 0,
    duration: 1250,
    score: 83
  },
  features: [
    {
      name: 'knowledge-engine',
      passed: true,
      score: 85,
      duration: 200,
      violations: [],
      metrics: {
        latency: 150,
        errorRate: 0,
        throughput: 1,
        memoryUsage: 25 * 1024 * 1024
      }
    }
  ],
  errors: []
}
```

## Best Practices

### 1. Define Clear Expectations
- Specify exactly which spans should be created
- Define required attributes for each span
- Set realistic performance thresholds

### 2. Use Meaningful Names
- Use descriptive span names that reflect operations
- Include context in attribute names
- Follow consistent naming conventions

### 3. Set Appropriate Thresholds
- Base thresholds on actual performance requirements
- Consider system resources and constraints
- Allow for reasonable variance

### 4. Create Comprehensive Rules
- Validate both success and failure scenarios
- Check for error conditions
- Verify data integrity

### 5. Monitor Validation Results
- Track validation scores over time
- Identify trends and patterns
- Use results to improve features

## Troubleshooting

### Common Issues

#### 1. Missing Spans
**Problem**: Expected spans not found in trace
**Solution**: 
- Check if feature is actually executing
- Verify span names match exactly
- Ensure OTEL instrumentation is enabled

#### 2. Attribute Mismatches
**Problem**: Required attributes missing or incorrect
**Solution**:
- Check attribute names and values
- Verify attribute setting in code
- Ensure attributes are set before span ends

#### 3. Performance Failures
**Problem**: Metrics exceed thresholds
**Solution**:
- Review performance requirements
- Optimize feature implementation
- Adjust thresholds if appropriate

#### 4. Validation Timeouts
**Problem**: Validation takes too long
**Solution**:
- Increase timeout values
- Optimize feature performance
- Use parallel execution

### Debugging Tips

1. **Enable Verbose Logging**
   ```javascript
   const runner = createValidationRunner({ verbose: true });
   ```

2. **Check Span Details**
   ```javascript
   console.log('Collected spans:', result.spans);
   console.log('Span attributes:', result.spans[0].attributes);
   ```

3. **Validate Individual Features**
   ```javascript
   const result = await validator.validateFeature('feature-name', config);
   console.log('Feature result:', result);
   ```

4. **Monitor OTEL Traces**
   - Use OTEL collector to view traces
   - Check span hierarchy and relationships
   - Verify span timing and duration

## Migration from Unit Tests

### Step 1: Identify Test Scenarios
- Map unit test cases to feature operations
- Identify expected behaviors and outcomes
- Define performance requirements

### Step 2: Create Validation Configuration
- Define expected spans for each operation
- Specify required attributes
- Set performance thresholds

### Step 3: Implement Validation Rules
- Convert test assertions to validation rules
- Add custom validation logic
- Handle edge cases and error conditions

### Step 4: Run and Validate
- Execute validation suite
- Compare results with unit tests
- Adjust configuration as needed

### Step 5: Integrate with CI/CD
- Add validation to build pipeline
- Set up automated reporting
- Configure failure notifications

## Conclusion

OTEL span-based validation provides a more realistic and comprehensive approach to feature validation compared to traditional unit tests. By analyzing actual runtime behavior through OpenTelemetry spans, metrics, and traces, you can ensure features work correctly in real-world conditions while gaining valuable insights into system performance and behavior.

The validation framework is designed to be flexible and extensible, allowing you to create custom validation rules and adapt to different feature requirements. With proper configuration and monitoring, OTEL validation can significantly improve the reliability and observability of your features.
