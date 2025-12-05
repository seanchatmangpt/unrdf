# @unrdf/validation

OpenTelemetry (OTEL) validation framework and observability tools for UNRDF.

**Status:** Internal use only (private package)

## Purpose

This package provides validation and observability infrastructure:

- OTEL span validation framework
- Compliance checking utilities
- Production readiness validation
- Performance monitoring hooks

## Installation

This is a private package. Use only in development/validation:

```bash
pnpm --filter @unrdf/validation install
```

## Usage

### Running Validation

```bash
# Comprehensive validation
node validation/run-all.mjs comprehensive

# Quick validation
node validation/run-all.mjs quick
```

### Using Validation in Code

```javascript
import { validateCompliance } from '@unrdf/validation';

const score = await validateCompliance({
  package: '@unrdf/core',
  checks: ['tests', 'types', 'docs']
});

console.log(`Compliance score: ${score.score}/100`);
```

## OTEL Spans

The validation framework checks:

- **Test coverage** - 80%+ passing tests
- **Type safety** - JSDoc type annotations
- **Documentation** - README and API docs
- **Performance** - No regressions
- **Compliance** - N3 library migration status

## Contents

```
src/
├── otel-validator.mjs    # OTEL span validation
├── compliance.mjs        # Compliance checking
├── performance.mjs       # Performance validation
└── index.mjs             # Main exports
```

## Truth Source

**CRITICAL:** OTEL spans are the only source of truth for validation.

Agent claims (tests pass, features work, production ready) are NOT reliable without OTEL confirmation. Always run validation before accepting claims.

## See Also

- [CLAUDE.md](../../CLAUDE.md) - Development guidelines
- [docs/audit/COMPLIANCE-SUMMARY.md](../../docs/audit/COMPLIANCE-SUMMARY.md) - Compliance details
- [LOCAL-DEVELOPMENT.md](../../docs/LOCAL-DEVELOPMENT.md) - Running validation
