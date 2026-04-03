---
description: Analyze OpenTelemetry spans and traces for UNRDF validation, ensuring OTEL score >= 80/100
---

# OTEL Analyst

## User Input

```text
$ARGUMENTS
```

You **MUST** consider the user input before proceeding (if not empty).

## Purpose

Analyze OpenTelemetry instrumentation in UNRDF packages, validate span hierarchies, and ensure observability score meets the >= 80/100 threshold.

## OTEL Validation Score

The OTEL score is calculated from:

| Component           | Weight | Criteria                      |
| ------------------- | ------ | ----------------------------- |
| Span Coverage       | 30%    | All operations have spans     |
| Attribute Quality   | 20%    | Semantic attributes present   |
| Error Tracking      | 20%    | Errors properly recorded      |
| Trace Propagation   | 15%    | Context flows across packages |
| Performance Metrics | 15%    | Duration recorded, no leaks   |

**Minimum Score**: 80/100

## Execution Steps

### 1. Run OTEL Validation

```bash
# Execute comprehensive validation
timeout 60s node validation/run-all.mjs comprehensive 2>&1 | tee otel-validation.log

# Extract score
grep "Score:" otel-validation.log || echo "Score not found"
```

### 2. Analyze Span Hierarchy

Expected UNRDF span hierarchy:

```
cli.command (root)
├── maturity.assess
│   ├── coverage.collect
│   ├── api.analyze
│   ├── docs.check
│   ├── security.scan
│   └── score.calculate
├── store.query
│   ├── sparql.parse
│   └── sparql.execute
└── output.format
    ├── json.serialize
    └── ttl.serialize
```

### 3. Check Span Attributes

Required attributes for maturity spans:

```javascript
// maturity.assess span
{
  'maturity.package': 'core',      // Package being assessed
  'maturity.level': 4,             // L1-L5 as integer
  'maturity.score': 87.5,          // Weighted score
  'maturity.coverage.lines': 92.3, // Line coverage %
  'maturity.api_stability': 'stable'
}
```

### 4. Validate Instrumentation Code

```bash
# Find OTEL imports
grep -r "@opentelemetry/api" packages/*/src/ --include="*.mjs" -l

# Find tracer creation
grep -r "getTracer" packages/*/src/ --include="*.mjs"

# Find span creation
grep -r "startActiveSpan\|startSpan" packages/*/src/ --include="*.mjs"
```

### 5. Check Error Recording

```bash
# Verify error attributes are set
grep -r "setStatus\|recordException" packages/*/src/ --include="*.mjs"
```

## OTEL Integration Pattern

### Correct Implementation

```javascript
// packages/cli/src/lib/maturity/otel-collector.mjs
import { trace, SpanStatusCode } from '@opentelemetry/api';

const tracer = trace.getTracer('unrdf-maturity', '1.0.0');

/**
 * Assess package with OTEL instrumentation
 * @param {string} packageName - Package to assess
 * @returns {Promise<Assessment>}
 */
export async function assessWithOtel(packageName) {
  return tracer.startActiveSpan('maturity.assess', async span => {
    try {
      span.setAttribute('maturity.package', packageName);

      const assessment = await runAssessment(packageName);

      span.setAttribute('maturity.level', assessment.level);
      span.setAttribute('maturity.score', assessment.score);
      span.setStatus({ code: SpanStatusCode.OK });

      return assessment;
    } catch (error) {
      span.setStatus({
        code: SpanStatusCode.ERROR,
        message: error.message,
      });
      span.recordException(error);
      throw error;
    } finally {
      span.end();
    }
  });
}
```

## Output Format

````markdown
## OTEL Analysis Report

**Date**: [timestamp]
**Scope**: [packages analyzed]

### Score Summary

| Component           | Score     | Weight | Contribution |
| ------------------- | --------- | ------ | ------------ |
| Span Coverage       | X/100     | 30%    | Y            |
| Attribute Quality   | X/100     | 20%    | Y            |
| Error Tracking      | X/100     | 20%    | Y            |
| Trace Propagation   | X/100     | 15%    | Y            |
| Performance Metrics | X/100     | 15%    | Y            |
| **Total**           | **X/100** | 100%   | **Y**        |

### Span Inventory

| Span Name        | Package | Attributes | Status |
| ---------------- | ------- | ---------- | ------ |
| maturity.assess  | cli     | 5          | ✅     |
| coverage.collect | cli     | 3          | ✅     |

### Issues Found

1. [CRITICAL/HIGH/MEDIUM] Description
   - Location: file:line
   - Fix: Suggested resolution

### Recommendations

- [actionable improvements]

### Verification Commands

```bash
# Re-run validation after fixes
node validation/run-all.mjs comprehensive
```
````

````

## Troubleshooting

### Missing Spans
```bash
# Check if OTEL SDK is initialized
grep -r "NodeSDK\|TracerProvider" packages/*/src/ --include="*.mjs"
````

### Low Score

```bash
# Run detailed validation
DEBUG=* node validation/run-all.mjs comprehensive 2>&1 | grep -E "WARN|ERROR|FAIL"
```

### Context Not Propagating

```bash
# Check for async context issues
grep -r "context.with\|context.active" packages/*/src/ --include="*.mjs"
```

End Command ---
