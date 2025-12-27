# Production Readiness Guide

This document defines the comprehensive production readiness standards for all packages in the UNRDF monorepo. Every package must pass the production readiness gate before being considered deployment-ready.

## Overview

The production readiness system evaluates packages across 8 categories:

| Category | Weight | Description |
|----------|--------|-------------|
| Code Quality | 20% | JSDoc coverage, linting, complexity |
| Testing | 25% | Coverage, pass rate, performance |
| Dependencies | 15% | Circularity, versions, maintenance |
| Security | 20% | Credentials, injection, OWASP |
| Documentation | 5% | README, API docs, examples |
| Performance | 10% | Bundle size, memory, patterns |
| Accessibility | 2.5% | A11y compliance (web packages) |
| Compatibility | 2.5% | Node version, ESM, types |

**Production Ready Threshold: 95%**

## Quick Start

```javascript
import { validatePackage, isProductionReady } from './src/validation/index.mjs';

// Quick check
const ready = await isProductionReady('/path/to/package');
console.log(ready ? 'Production Ready!' : 'Not Ready');

// Full validation
const receipt = await validatePackage('/path/to/package');
console.log(`Score: ${receipt.overallScore}/100`);
console.log(`Status: ${receipt.productionReady ? 'READY' : 'NOT READY'}`);
```

## Category Details

### 1. Code Quality (20%)

**What We Check:**
- JSDoc coverage for exported functions (target: 100%)
- No console.log in production code
- Function length (target: <100 lines)
- File length (target: <500 lines)
- Cyclomatic complexity (target: <10)
- Code smell detection

**Pass/Warn/Fail Examples:**

| Scenario | Status | Score |
|----------|--------|-------|
| 100% JSDoc, no console, all functions <100 lines | PASS | 95-100 |
| 90% JSDoc, 2 console.log calls | WARN | 80-94 |
| 50% JSDoc, many long functions | FAIL | <80 |

**Remediation:**

1. **Low JSDoc Coverage:**
   ```javascript
   // Before - Missing JSDoc
   export function processData(data) {
     return data.map(x => x * 2);
   }

   // After - Proper JSDoc
   /**
    * Process data by doubling each value
    * @param {number[]} data - Input data array
    * @returns {number[]} Doubled values
    */
   export function processData(data) {
     return data.map(x => x * 2);
   }
   ```

2. **Console Usage:**
   - Replace `console.log` with proper logging library
   - Use environment-aware logging
   - Remove debug statements before commit

3. **Long Functions:**
   - Extract helper functions
   - Use early returns
   - Apply single responsibility principle

### 2. Testing (25%)

**What We Check:**
- Test coverage (target: >=70%, recommended: 80%)
- Test pass rate (target: >=95%)
- Test suite duration (target: <5s)
- Integration test presence
- Test file naming conventions

**Pass/Warn/Fail Examples:**

| Scenario | Status | Score |
|----------|--------|-------|
| 85% coverage, 100% pass rate, <3s duration | PASS | 95-100 |
| 72% coverage, 97% pass rate | WARN | 80-94 |
| 50% coverage or <95% pass rate | FAIL | <80 |

**Remediation:**

1. **Low Coverage:**
   ```bash
   # Run coverage report
   npm run test:coverage

   # Identify uncovered lines
   # Add tests for edge cases and error paths
   ```

2. **Slow Tests:**
   - Mock external dependencies
   - Use test parallelization
   - Avoid unnecessary setup/teardown

3. **Missing Integration Tests:**
   - Create `*.integration.test.mjs` files
   - Test real scenarios end-to-end
   - Include happy path and error cases

### 3. Dependencies (15%)

**What We Check:**
- Circular dependency detection
- Unmaintained package usage
- Version specification (no wildcards)
- Lock file presence
- Misplaced dev dependencies

**Pass/Warn/Fail Examples:**

| Scenario | Status | Score |
|----------|--------|-------|
| No circular deps, all versions locked | PASS | 95-100 |
| Minor version drift, 1-2 warnings | WARN | 80-94 |
| Circular dependencies or wildcards | FAIL | <80 |

**Remediation:**

1. **Circular Dependencies:**
   ```
   // Problem: A imports B, B imports A

   // Solution: Extract shared code to C
   // A imports C
   // B imports C
   ```

2. **Wildcard Versions:**
   ```json
   // Before
   "dependencies": {
     "lodash": "*"
   }

   // After
   "dependencies": {
     "lodash": "^4.17.21"
   }
   ```

3. **Deprecated Packages:**
   - Replace `moment` with `dayjs` or `date-fns`
   - Replace `request` with `node-fetch` or native fetch
   - Check npm deprecation notices

### 4. Security (20%)

**What We Check:**
- Hardcoded credentials/secrets
- SQL/NoSQL injection patterns
- XSS vulnerability patterns
- Command injection patterns
- Insecure random usage
- TLS validation settings

**Pass/Warn/Fail Examples:**

| Scenario | Status | Score |
|----------|--------|-------|
| No secrets, input validation present | PASS | 95-100 |
| Minor insecure patterns in non-critical code | WARN | 80-94 |
| Hardcoded secrets or injection vulnerabilities | FAIL | <80 |

**Remediation:**

1. **Hardcoded Secrets:**
   ```javascript
   // Before - CRITICAL
   const API_KEY = "sk-1234567890abcdef";

   // After - Use environment variables
   const API_KEY = process.env.API_KEY;
   ```

2. **Injection Prevention:**
   ```javascript
   // Before - SQL Injection risk
   const query = `SELECT * FROM users WHERE id = ${userId}`;

   // After - Parameterized query
   const query = 'SELECT * FROM users WHERE id = $1';
   db.query(query, [userId]);
   ```

3. **Input Validation:**
   ```javascript
   import { z } from 'zod';

   const UserSchema = z.object({
     email: z.string().email(),
     age: z.number().min(0).max(150)
   });

   const validated = UserSchema.parse(input);
   ```

### 5. Documentation (5%)

**What We Check:**
- README.md existence and quality
- API documentation coverage (>=80%)
- Code examples presence
- Changelog maintenance
- Package.json completeness

**Pass/Warn/Fail Examples:**

| Scenario | Status | Score |
|----------|--------|-------|
| Full README, 100% API docs, examples | PASS | 95-100 |
| README exists, 85% API docs | WARN | 80-94 |
| No README or <80% API docs | FAIL | <80 |

**README Template:**

```markdown
# Package Name

Brief description of what this package does.

## Installation

\`\`\`bash
npm install @unrdf/package-name
\`\`\`

## Usage

\`\`\`javascript
import { feature } from '@unrdf/package-name';

// Example usage
const result = feature({ option: true });
\`\`\`

## API Reference

### `feature(options)`

Description of the function.

**Parameters:**
- `options.key` (string): Description

**Returns:** Description of return value

## License

MIT
```

### 6. Performance (10%)

**What We Check:**
- Bundle size estimation (target: <500KB for libraries)
- Performance anti-patterns
- Heavy dependency usage
- Async operation patterns
- Memory leak patterns

**Pass/Warn/Fail Examples:**

| Scenario | Status | Score |
|----------|--------|-------|
| Small bundle, no anti-patterns | PASS | 95-100 |
| Moderate bundle, 1-2 sync FS calls | WARN | 80-94 |
| Large bundle or critical anti-patterns | FAIL | <80 |

**Common Anti-Patterns:**

1. **Synchronous File Operations:**
   ```javascript
   // Before - Blocking
   const data = fs.readFileSync('file.txt');

   // After - Non-blocking
   const data = await fs.promises.readFile('file.txt');
   ```

2. **Heavy Dependencies:**
   - Replace `lodash` with `lodash-es` or native methods
   - Replace `moment` with `dayjs`
   - Consider bundle size impact before adding deps

3. **N+1 Queries:**
   ```javascript
   // Before - N+1 pattern
   for (const user of users) {
     const orders = await fetchOrders(user.id);
   }

   // After - Batch
   const orders = await fetchOrdersForUsers(users.map(u => u.id));
   ```

### 7. Accessibility (2.5%)

*Primarily for web/UI packages*

**What We Check:**
- Alt text for images
- ARIA attributes
- Keyboard navigation
- Focus management
- Semantic HTML usage

**Pass/Warn/Fail Examples:**

| Scenario | Status | Score |
|----------|--------|-------|
| All images have alt, semantic HTML | PASS | 95-100 |
| Minor ARIA issues | WARN | 80-94 |
| Missing alt text, no keyboard support | FAIL | <80 |

**Non-UI Packages:** Receive automatic 95% score with note.

### 8. Compatibility (2.5%)

**What We Check:**
- Node.js version requirements (>=18)
- ES module compatibility
- TypeScript type definitions
- Export map configuration
- Deno compatibility indicators

**Pass/Warn/Fail Examples:**

| Scenario | Status | Score |
|----------|--------|-------|
| ESM, types, exports map, Node 18+ | PASS | 95-100 |
| Missing exports map or types | WARN | 80-94 |
| Mixed ESM/CJS or no engine spec | FAIL | <80 |

**Package.json Best Practices:**

```json
{
  "name": "@unrdf/package",
  "type": "module",
  "main": "./dist/index.js",
  "types": "./dist/index.d.ts",
  "exports": {
    ".": {
      "import": "./dist/index.js",
      "types": "./dist/index.d.ts"
    }
  },
  "engines": {
    "node": ">=18.0.0"
  }
}
```

## Score Calculation

The overall score is a weighted average:

```
Overall = (CodeQuality * 0.20) + (Testing * 0.25) + (Dependencies * 0.15) +
          (Security * 0.20) + (Documentation * 0.05) + (Performance * 0.10) +
          (Accessibility * 0.025) + (Compatibility * 0.025)
```

**Thresholds:**
- **>=95**: Production Ready (PASS)
- **80-94**: Needs Attention (WARN)
- **<80**: Not Ready (FAIL)

## Exemption Process

Some checks may not apply to all packages. To request an exemption:

1. **Document the Reason:**
   ```javascript
   // In package.json
   {
     "productionReadiness": {
       "exemptions": [
         {
           "check": "accessibility",
           "reason": "Non-UI package - no web components",
           "approvedBy": "tech-lead@example.com"
         }
       ]
     }
   }
   ```

2. **Required Information:**
   - Which check to exempt
   - Clear justification
   - Approver (tech lead or above)
   - Expiration date (optional)

3. **Valid Exemption Reasons:**
   - Package type mismatch (e.g., a11y for CLI tools)
   - Intentional patterns (e.g., sync operations in CLI)
   - Third-party constraints

4. **Invalid Exemption Reasons:**
   - "Too much work to fix"
   - "Tests are hard to write"
   - "Nobody will notice"

## Integration with CI/CD

### GitHub Actions Example

```yaml
name: Production Readiness Check

on: [push, pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Setup Node
        uses: actions/setup-node@v4
        with:
          node-version: '20'

      - name: Install Dependencies
        run: pnpm install

      - name: Run Production Validation
        run: |
          node -e "
            import { validatePackage } from './src/validation/index.mjs';
            const receipt = await validatePackage('.');
            console.log(JSON.stringify(receipt, null, 2));
            if (!receipt.productionReady) {
              process.exit(1);
            }
          "
```

### Pre-commit Hook

```bash
#!/bin/sh
# .husky/pre-commit

node -e "
  import { isProductionReady } from './src/validation/index.mjs';
  if (!await isProductionReady('.')) {
    console.error('Package is not production ready. Run validation for details.');
    process.exit(1);
  }
"
```

## Batch Validation

Validate all packages in the monorepo:

```javascript
import { validatePackages } from './src/validation/index.mjs';
import { readdir } from 'node:fs/promises';
import { join } from 'node:path';

const packagesDir = './packages';
const entries = await readdir(packagesDir, { withFileTypes: true });
const packagePaths = entries
  .filter(e => e.isDirectory())
  .map(e => join(packagesDir, e.name));

const results = await validatePackages(packagePaths);

console.log(`Validated: ${results.packagesValidated}`);
console.log(`Ready: ${results.packagesReady}`);
console.log(`Not Ready: ${results.packagesNotReady}`);
console.log(`Average Score: ${results.averageScore.toFixed(1)}`);
```

## Troubleshooting

### Common Issues

1. **"No source files found"**
   - Check that source files are in the expected location
   - Ensure file extensions are `.mjs`, `.js`, or `.ts`
   - Verify `node_modules` is not being scanned

2. **"Check timeout"**
   - Default timeout is 30 seconds per check
   - Increase with `{ timeout: 60000 }` option
   - Consider optimizing test suite

3. **Low Security Score Despite No Secrets**
   - Check for false positives in string literals
   - Review regex patterns in source code
   - Add `.gitignore` for `.env` files

4. **Accessibility Check Always Passes**
   - Verify package is detected as web package
   - Check for `.jsx` or `.tsx` files
   - Ensure React/Vue/Svelte is in dependencies

### Debug Mode

```javascript
const receipt = await validatePackage('/path/to/package', {
  parallel: false,  // Run checks sequentially
  timeout: 60000    // Increase timeout
});

// Inspect individual check results
for (const result of receipt.checkResults) {
  console.log(`${result.name}: ${result.score} (${result.status})`);
  if (result.failures.length > 0) {
    console.log('  Failures:', result.failures);
  }
}
```

## Best Practices Summary

1. **Code Quality:**
   - Write JSDoc for all exports
   - Keep functions under 50 lines
   - Avoid code smells

2. **Testing:**
   - Aim for 80%+ coverage
   - Run tests in under 5 seconds
   - Include integration tests

3. **Dependencies:**
   - Lock all versions
   - Avoid circular imports
   - Review before adding new deps

4. **Security:**
   - Never hardcode secrets
   - Validate all inputs
   - Use parameterized queries

5. **Documentation:**
   - Maintain README with examples
   - Document all public APIs
   - Keep changelog updated

6. **Performance:**
   - Prefer async operations
   - Mind bundle size
   - Avoid N+1 patterns

7. **Compatibility:**
   - Use ESM
   - Specify Node version
   - Include TypeScript types

---

**Production readiness is not optional.** Every package must meet these standards before deployment.

For questions or issues, contact the platform team.
