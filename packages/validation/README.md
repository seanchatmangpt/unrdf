# @unrdf/validation

![Version](https://img.shields.io/badge/version-26.4.3-blue) ![Production Ready](https://img.shields.io/badge/production-ready-green)

**OTEL Validation Framework for UNRDF**

A validation framework that uses OpenTelemetry (OTEL) spans instead of traditional unit tests to validate features. This approach provides deeper observability into feature behavior by analyzing actual execution patterns.

## Installation

```bash
pnpm add @unrdf/validation
```

## Overview

**Why OTEL-based validation?**

Traditional unit tests test isolated functions in isolation. OTEL validation tests features by running them and analyzing the resulting OpenTelemetry spans. This gives you:

- **Real execution patterns**: Validates how features actually execute
- **Observability**: Provides trace data for debugging
- **Performance metrics**: Captures latency, throughput, and memory usage
- **Structured reports**: Consistent validation reports across all features

## Core Exports

| Export | Description |
|--------|-------------|
| `createOTELValidator` | Creates an OTEL-based validator instance |
| `createValidationRunner` | Creates a validation runner for feature suites |
| `createValidationHelpers` | Helper utilities for validation operations |
| `OTELValidator` | Main validator class |
| `ValidationRunner` | Validation suite runner class |
| `ValidationResultSchema` | Zod schema for validation results |
| `FeatureValidationConfigSchema` | Zod schema for feature configurations |

## Quick Start

### Basic Validation

```javascript
import { createOTELValidator } from '@unrdf/validation';

const validator = createOTELValidator();

// Validate a single feature
const result = await validator.validateFeature('hook-execution', {
  expectedSpans: ['hook.validate-before-write'],
  requiredAttributes: ['hook.name', 'quad.subject'],
  performanceThresholds: {
    maxLatency: 1000,    // 1 second
    maxErrorRate: 0.01,  // 1% error rate
    minThroughput: 100,  // 100 operations/sec
    maxMemoryUsage: 100  // 100 MB
  }
});

console.log(result);
// {
//   passed: true,
//   score: 95,
//   metrics: { latency: 450, errorRate: 0.002, throughput: 150, memoryUsage: 85 },
//   spans: [...],
//   violations: []
// }
```

### Validation Runner for Suites

```javascript
import { createValidationRunner } from '@unrdf/validation';

const runner = createValidationRunner({
  timeout: 30000,
  retries: 0,
  verbose: true
});

const report = await runner.runSuite({
  name: 'Knowledge Hooks Validation',
  description: 'Validate all hook execution patterns',
  features: [
    {
      name: 'hook-validate-before-write',
      config: {
        expectedSpans: ['hook.validate-before-write'],
        requiredAttributes: ['hook.name', 'quad.subject'],
        performanceThresholds: {
          maxLatency: 1000,
          maxErrorRate: 0.01,
          minThroughput: 100,
          maxMemoryUsage: 100
        }
      }
    },
    {
      name: 'hook-post-write-transform',
      config: {
        expectedSpans: ['hook.post-write-transform'],
        requiredAttributes: ['hook.name'],
        performanceThresholds: {
          maxLatency: 2000,
          maxErrorRate: 0.005,
          minThroughput: 50
        }
      }
    }
  ]
});

console.log(`Suite score: ${report.summary.score}/100`);
console.log(`Features passed: ${report.summary.passed}/${report.summary.total}`);

// Report structure:
// {
//   suite: 'Knowledge Hooks Validation',
//   timestamp: '2026-04-03T12:00:00.000Z',
//   summary: {
//     total: 2,
//     passed: 2,
//     failed: 0,
//     skipped: 0,
//     duration: 2345,
//     score: 98
//   },
//   features: [...],
//   errors: []
// }
```

## Configuration

### Validator Options

```javascript
const validator = createOTELValidator({
  serviceName: 'my-validation-suite',
  enableMetrics: true,
  enableTracing: true,
  validationTimeout: 30000,
  maxSpansPerValidation: 1000,
  spanAttributes: {
    environment: 'production',
    version: '26.4.3'
  }
});
```

### Feature Configuration

```javascript
const featureConfig = {
  name: 'feature-name',
  config: {
    // Spans that MUST appear during execution
    expectedSpans: [
      'feature.start',
      'feature.process',
      'feature.complete'
    ],

    // Attributes that MUST exist on spans
    requiredAttributes: [
      'feature.name',
      'execution.duration',
      'result.status'
    ],

    // Performance thresholds
    performanceThresholds: {
      maxLatency: 5000,        // Max execution time in ms
      maxErrorRate: 0.02,      // Max error rate (0-1)
      minThroughput: 10,       // Min operations per second
      maxMemoryUsage: 200      // Max memory usage in MB
    },

    // Custom validation rules
    validationRules: [
      {
        name: 'check-error-logging',
        condition: (spans) => {
          const errorSpans = spans.filter(s =>
            s.attributes['error'] === 'true' && s.name === 'feature.error'
          );
          return errorSpans.length > 0;
        },
        severity: 'warning'
      }
    ]
  }
};
```

## Validation Results

```javascript
// Result structure
const result = {
  feature: 'hook-validate-before-write',
  passed: true,
  score: 95,                    // 0-100 score
  duration: 450,                // Execution time in ms
  metrics: {
    latency: 450,               // Average latency
    errorRate: 0.002,           // Error rate (0-1)
    throughput: 150,            // Operations/sec
    memoryUsage: 85             // Memory usage in MB
  },
  spans: [                      // Collected OTEL spans
    {
      name: 'hook.validate-before-write',
      status: 'ok',
      duration: 450,
      attributes: {
        'hook.name': 'validate-pii',
        'quad.subject': 'http://example.com/alice',
        'hook.type': 'validate-before-write',
        'validation.valid': 'true'
      }
    }
  ],
  violations: [                 // Validation failures
    'Latency exceeds threshold: 450ms > 400ms'
  ],
  timestamp: '2026-04-03T12:00:00.000Z'
};

// Check result
if (!result.passed) {
  console.error(`Validation failed: ${result.score}/100`);
  result.violations.forEach(v => console.error(`  - ${v}`));
}
```

## Use Cases

### 1. Feature Validation

Validate that features execute correctly and meet performance requirements:

```javascript
import { createOTELValidator } from '@unrdf/validation';

const validator = createOTELValidator();

const result = await validator.validateFeature('hook-execution', {
  expectedSpans: ['hook.validate-before-write', 'hook.post-write-transform'],
  requiredAttributes: ['hook.name', 'quad.subject'],
  performanceThresholds: {
    maxLatency: 1000,
    maxErrorRate: 0.01,
    minThroughput: 100,
    maxMemoryUsage: 100
  }
});

if (!result.passed) {
  console.error(`Feature validation failed: ${result.score}/100`);
  process.exit(1);
}
```

### 2. Regression Testing

Detect performance regressions before they reach production:

```javascript
import { createOTELValidator } from '@unrdf/validation';

const validator = createOTELValidator();

// Get baseline
const baseline = await validator.validateFeature('query-performance', {
  expectedSpans: ['sparql.query.select'],
  requiredAttributes: ['query.duration'],
  performanceThresholds: {
    maxLatency: 1000,  // 1 second
    minThroughput: 1000
  }
});

// Compare with current
const current = await validator.validateFeature('query-performance', {
  expectedSpans: ['sparql.query.select'],
  requiredAttributes: ['query.duration'],
  performanceThresholds: {
    maxLatency: 1000,
    minThroughput: 1000
  }
});

if (current.score < baseline.score - 10) {
  console.warn(`Performance regression detected!`);
  console.warn(`Baseline: ${baseline.score}/100, Current: ${current.score}/100`);
}
```

### 3. Automated CI Validation

Integrate validation into CI/CD pipelines:

```javascript
// validation-ci.mjs
import { createValidationRunner } from '@unrdf/validation';

const runner = createValidationRunner({
  timeout: 60000,
  retries: 2,
  verbose: false
});

async function runCIValidation() {
  try {
    const report = await runner.runSuite({
      name: 'CI Validation Suite',
      features: [
        {
          name: 'core-sparql-query',
          config: {
            expectedSpans: ['sparql.query.select'],
            requiredAttributes: ['query.duration'],
            performanceThresholds: { maxLatency: 5000, minThroughput: 50 }
          }
        },
        {
          name: 'hook-execution',
          config: {
            expectedSpans: ['hook.validate-before-write'],
            requiredAttributes: ['hook.name'],
            performanceThresholds: { maxLatency: 1000, maxErrorRate: 0.01 }
          }
        }
      ]
    });

    if (report.summary.failed > 0) {
      console.error('CI Validation FAILED');
      console.error(`Score: ${report.summary.score}/100`);
      console.error('Failed features:');
      report.features.filter(f => !f.passed).forEach(f => {
        console.error(`  - ${f.name}: ${f.score}/100`);
      });
      process.exit(1);
    } else {
      console.log('CI Validation PASSED');
      console.log(`Score: ${report.summary.score}/100`);
    }
  } catch (error) {
    console.error('CI Validation ERROR:', error);
    process.exit(1);
  }
}

runCIValidation();
```

### 4. Validation Helpers

Use helper utilities for common validation tasks:

```javascript
import { createValidationHelpers } from '@unrdf/validation';

const helpers = createValidationHelpers();

// Helper 1: Check if span exists
const hasRequiredSpan = helpers.hasSpan(result.spans, 'hook.validate-before-write');
console.log('Has required span:', hasRequiredSpan); // true

// Helper 2: Check if attributes exist
const hasRequiredAttrs = helpers.hasAttributes(result.spans, ['hook.name', 'quad.subject']);
console.log('Has required attributes:', hasRequiredAttrs); // true

// Helper 3: Check performance thresholds
const passesThresholds = helpers.passesThresholds(result.metrics, {
  maxLatency: 1000,
  maxErrorRate: 0.01,
  minThroughput: 100,
  maxMemoryUsage: 100
});
console.log('Passes thresholds:', passesThresholds); // true

// Helper 4: Calculate score
const score = helpers.calculateScore(result);
console.log('Score:', score); // 95
```

## Testing

This package uses OTEL-based validation instead of traditional unit tests. To run validation:

```bash
# Run validation suite
node validation/run-all.mjs comprehensive

# Check results
grep "Score:" validation-output.log

# Check for failures
grep "FAILED\|Error" validation-output.log
```

## API Reference

See `src/index.mjs` for complete API documentation.

### OTELValidator

```javascript
// Create validator
const validator = createOTELValidator({
  serviceName: 'validation',
  validationTimeout: 30000
});

// Validate feature
const result = await validator.validateFeature(
  'feature-name',
  config
);

// Get collected metrics
const metrics = validator.getMetrics();
```

### ValidationRunner

```javascript
// Create runner
const runner = createValidationRunner({
  timeout: 30000,
  retries: 0,
  verbose: true
});

// Run suite
const report = await runner.runSuite({
  name: 'Suite Name',
  features: [...]
});

// Get reports
const report = runner.getReport('Suite Name');
const allReports = runner.getAllReports();
```

## Architecture

**Validation Pipeline:**

```
Feature Execution
      ↓
OpenTelemetry Spans Collected
      ↓
Span Analysis (expected/required checks)
      ↓
Metrics Calculation (latency, error rate, throughput, memory)
      ↓
Validation Rules Evaluation
      ↓
Result Scoring (0-100)
      ↓
Report Generation
```

**Key Components:**

1. **OTELValidator** - Analyzes execution spans and validates against configurations
2. **ValidationRunner** - Executes feature suites and generates comprehensive reports
3. **ValidationHelpers** - Utility functions for common validation tasks
4. **Span Collector** - Collects and analyzes OTEL spans

## Dependencies

- `@opentelemetry/api` - OpenTelemetry tracing and metrics
- `zod` - Runtime validation schemas
- `@unrdf/knowledge-engine` - Core validation logic (internal dependency)

## When to Use

✅ **Use when:**

- You want to validate feature behavior, not isolated functions
- You need observability into execution patterns
- You're building CI/CD validation pipelines
- You want performance regression detection
- You need structured validation reports

❌ **Don't use when:**

- You're testing isolated unit functions (traditional unit tests are better)
- You don't use OpenTelemetry in your application
- You need fast feedback on simple code changes

## License

MIT
