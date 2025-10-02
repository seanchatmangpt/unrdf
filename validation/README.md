# OTEL Span-Based Validation Framework

## Overview

This directory contains the OpenTelemetry (OTEL) span-based validation framework that replaces traditional unit tests with span analysis and metric validation. Features are validated by analyzing OTEL spans, metrics, and traces instead of isolated test assertions.

## Quick Start

### Run All Validations
```bash
# Comprehensive validation (recommended)
node validation/run-all.mjs comprehensive

# Individual suite validation
node validation/run-all.mjs individual
```

### Run Specific Validations
```bash
# Knowledge Engine validation
node validation/knowledge-engine.validation.mjs

# CLI validation
node validation/cli.validation.mjs
```

## Framework Components

### Core Validation Engine
- **`src/validation/otel-validator.mjs`** - Main validation engine
- **`src/validation/validation-helpers.mjs`** - Validation utilities
- **`src/validation/validation-runner.mjs`** - Validation orchestration
- **`src/validation/index.mjs`** - Main entry point

### Validation Suites
- **`knowledge-engine.validation.mjs`** - Knowledge engine feature validation
- **`cli.validation.mjs`** - CLI command validation
- **`run-all.mjs`** - Comprehensive validation runner

## How It Works

1. **Feature Execution**: Features are executed and generate OTEL spans
2. **Span Collection**: Validation framework collects spans and metrics
3. **Analysis**: Spans are analyzed against expected patterns and thresholds
4. **Scoring**: Validation score is calculated (0-100)
5. **Reporting**: Results are reported with pass/fail status

## Validation Criteria

### Span Validation (40% of score)
- Required spans exist
- Spans have correct attributes
- Spans have correct status (ok/error)

### Metric Validation (40% of score)
- Latency within thresholds
- Error rate below limits
- Throughput meets minimums
- Memory usage within bounds

### Rule Validation (20% of score)
- Custom validation rules pass
- Business logic constraints satisfied
- Integration requirements met

## Passing Criteria

- **Score ≥ 80**: Feature passes validation
- **Score < 80**: Feature fails validation
- **All spans OK**: No error spans in trace
- **Performance OK**: All metrics within thresholds

## Example Output

```
🎯 Comprehensive Validation Results:
   Overall Score: 85/100
   Features: 5/6 passed
   Duration: 1250ms
   Status: ✅ PASSED

📊 Performance Summary:
   knowledge-engine:
     Latency: 150ms
     Error Rate: 0.00%
     Throughput: 1 ops
     Memory: 25.5MB
```

## Configuration

### Feature Configuration
```javascript
{
  name: 'feature-name',
  description: 'Feature description',
  config: {
    expectedSpans: ['span1', 'span2'],
    requiredAttributes: ['attr1', 'attr2'],
    performanceThresholds: {
      maxLatency: 1000,
      maxErrorRate: 0.01,
      minThroughput: 1,
      maxMemoryUsage: 50 * 1024 * 1024
    },
    validationRules: []
  }
}
```

### Global Configuration
```javascript
{
  timeout: 30000,
  retries: 1,
  parallel: false
}
```

## Benefits Over Unit Tests

### Traditional Unit Tests
- ❌ Isolated execution
- ❌ Mocked dependencies
- ❌ Static behavior
- ❌ High maintenance

### OTEL Validation
- ✅ Real usage patterns
- ✅ Integration testing
- ✅ Runtime behavior
- ✅ Observability insights

## Documentation

- **[OTEL Validation Guide](../docs/validation/OTEL-VALIDATION-GUIDE.md)** - Comprehensive guide
- **[CLAUDE.md](../CLAUDE.md)** - Updated validation protocol

## Integration

### CI/CD Pipeline
```bash
# Add to build pipeline
node validation/run-all.mjs comprehensive
if [ $? -ne 0 ]; then
  echo "Validation failed"
  exit 1
fi
```

### Monitoring
- Validation scores tracked over time
- Performance trends monitored
- Failure patterns analyzed

## Troubleshooting

### Common Issues
1. **Missing Spans**: Check feature execution and OTEL instrumentation
2. **Attribute Mismatches**: Verify attribute names and values
3. **Performance Failures**: Review thresholds and optimization
4. **Timeouts**: Increase timeout values or optimize performance

### Debug Mode
```bash
# Enable verbose logging
node validation/run-all.mjs comprehensive --verbose
```

## Contributing

When adding new features:
1. Create validation configuration
2. Define expected spans and attributes
3. Set performance thresholds
4. Add validation rules
5. Test validation suite

## Support

For issues or questions:
- Check the [OTEL Validation Guide](../docs/validation/OTEL-VALIDATION-GUIDE.md)
- Review validation configuration
- Enable debug mode for detailed output
- Check OTEL instrumentation setup
