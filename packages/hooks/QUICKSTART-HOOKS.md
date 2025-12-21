# @unrdf/hooks - Quick Start Guide

**80/20 Guide**: Get policy enforcement and validation running in 5 minutes.

## One-Command Demo

```bash
node examples/production-policy-chain.mjs
```

## Quick Start

### 1. Install

```bash
pnpm add @unrdf/hooks
```

### 2. Define a Hook

```javascript
import { defineHook } from '@unrdf/hooks'

defineHook('validate-pii', {
  type: 'validate-before-write',
  async check(quad) {
    // Reject PII without consent
    if (isPII(quad) && !hasConsent(quad.subject)) {
      return false
    }
    return true
  }
})
```

### 3. Execute Hook

```javascript
import { executeHook } from '@unrdf/hooks'

const isValid = await executeHook('validate-pii', quad)
if (!isValid) {
  throw new Error('PII validation failed')
}
```

### 4. Hook Chains

```javascript
// Chain multiple hooks
defineHook('data-quality', {
  type: 'transform',
  hooks: [
    'validate-schema',
    'validate-pii',
    'normalize-data',
    'audit-log'
  ]
})

// Execute chain
await executeHook('data-quality', quad)
```

## Hook Types

- **validate-before-write**: Validate before storing
- **transform**: Transform data
- **audit**: Log operations
- **notify**: Send notifications

## Use Cases

**Data Validation**:
```javascript
defineHook('schema-validation', {
  type: 'validate-before-write',
  async check(quad) {
    return validateSchema(quad)
  }
})
```

**Data Transformation**:
```javascript
defineHook('normalize', {
  type: 'transform',
  async transform(quad) {
    return normalizeQuad(quad)
  }
})
```

**Audit Logging**:
```javascript
defineHook('audit-log', {
  type: 'audit',
  async log(quad, operation) {
    auditLog.write({ quad, operation, timestamp: Date.now() })
  }
})
```

## Support

- Issues: https://github.com/unrdf/unrdf/issues
- Examples: See [examples/](./examples/)
