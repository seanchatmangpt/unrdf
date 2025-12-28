# ESLint Enforcement Rules for V6 Patterns

**Version**: v6.0.0
**Purpose**: Automatically enforce L5 patterns via static analysis

---

## Overview

**Problem**: Manual pattern enforcement is error-prone. Developers may forget to inject context, validate inputs, or generate receipts.

**Solution**: ESLint rules that FAIL the build if L5 patterns are violated.

**Philosophy**: If it can be caught by static analysis, it MUST be enforced by linting.

---

## ESLint Configuration (Copy-Exact)

### 1. Root ESLint Config

Create `/home/user/unrdf/.eslintrc.cjs`:

```javascript
// .eslintrc.cjs - Root ESLint configuration
module.exports = {
  root: true,
  env: {
    node: true,
    es2022: true,
  },
  extends: [
    'eslint:recommended',
    'plugin:jsdoc/recommended',
    'prettier', // Disable formatting rules (Prettier handles it)
  ],
  plugins: [
    'jsdoc',
    'no-only-tests',
  ],
  parserOptions: {
    ecmaVersion: 2022,
    sourceType: 'module',
  },
  rules: {
    // =========================================================================
    // V6 L5 Pattern Enforcement
    // =========================================================================

    // --- Determinism Envelope ---
    'no-restricted-globals': ['error', {
      name: 'Date',
      message: '❌ Use injected TimeProvider (context.time.now()) instead of Date.now(). See: docs/v6-patterns/04-determinism-envelope.md',
    }],

    'no-restricted-syntax': ['error',
      // Ban Math.random()
      {
        selector: 'CallExpression[callee.object.name="Math"][callee.property.name="random"]',
        message: '❌ Use injected RandomProvider (context.random.random()) instead of Math.random(). See: docs/v6-patterns/04-determinism-envelope.md',
      },
      // Ban crypto.randomUUID()
      {
        selector: 'CallExpression[callee.object.name="crypto"][callee.property.name="randomUUID"]',
        message: '❌ Use injected RandomProvider (context.random.uuid()) instead of crypto.randomUUID(). See: docs/v6-patterns/04-determinism-envelope.md',
      },
      // Ban process.env access (except in environment provider)
      {
        selector: 'MemberExpression[object.object.name="process"][object.property.name="env"]',
        message: '❌ Use injected EnvironmentProvider (context.env.get()) instead of process.env. See: docs/v6-patterns/04-determinism-envelope.md',
      },
      // Ban direct store mutations (must use Delta)
      {
        selector: 'CallExpression[callee.object.name="store"][callee.property.name="add"]',
        message: '❌ Use Delta Contract (createDelta + gate.proposeDelta) instead of direct store.add(). See: docs/v6-patterns/02-delta-contract-pattern.md',
      },
      {
        selector: 'CallExpression[callee.object.name="store"][callee.property.name="delete"]',
        message: '❌ Use Delta Contract (createDelta + gate.proposeDelta) instead of direct store.delete(). See: docs/v6-patterns/02-delta-contract-pattern.md',
      },
    ],

    // --- Zod Validation Layer ---
    'jsdoc/require-jsdoc': ['error', {
      require: {
        FunctionDeclaration: true,
        MethodDefinition: true,
        ClassDeclaration: true,
        ArrowFunctionExpression: false, // Only enforce on named functions
        FunctionExpression: false,
      },
      publicOnly: true, // Only public (exported) functions
    }],

    'jsdoc/require-param': 'error',
    'jsdoc/require-param-type': 'error',
    'jsdoc/require-returns': 'error',
    'jsdoc/require-returns-type': 'error',
    'jsdoc/require-throws': 'warn', // Warn (not error) for @throws

    // --- General Code Quality ---
    'no-console': ['warn', { allow: ['warn', 'error'] }], // Allow console.warn/error
    'no-debugger': 'error',
    'no-unused-vars': ['error', { argsIgnorePattern: '^_' }], // Allow _unused
    'no-only-tests/no-only-tests': 'error', // Ban .only in tests

    // --- Async/Await ---
    'require-await': 'error', // Async functions must use await
    'no-return-await': 'error', // No redundant await in return

    // --- Error Handling ---
    'no-throw-literal': 'error', // Must throw Error objects

    // =========================================================================
    // End V6 L5 Pattern Enforcement
    // =========================================================================
  },
  overrides: [
    // Allow banned operations in specific files (providers)
    {
      files: [
        '**/determinism-context.mjs',
        '**/time-provider.mjs',
        '**/random-provider.mjs',
        '**/environment-provider.mjs',
      ],
      rules: {
        'no-restricted-globals': 'off',
        'no-restricted-syntax': 'off',
      },
    },
    // Test files: Relax some rules
    {
      files: ['**/*.test.mjs', '**/*.spec.mjs'],
      rules: {
        'jsdoc/require-jsdoc': 'off', // No JSDoc in tests
        'no-console': 'off', // Allow console in tests
      },
    },
  ],
};
```

---

## Custom ESLint Rules (Advanced)

For more sophisticated enforcement, create custom ESLint rules:

### 1. Require Receipt HOF for State Mutations

**Rule**: Functions with names matching `/^(create|update|delete|modify|mutate)/i` MUST return `{result, receipt}`.

```javascript
// eslint-plugin-unrdf/lib/rules/require-receipt.js

module.exports = {
  meta: {
    type: 'problem',
    docs: {
      description: 'Require receipt HOF for state mutations',
      category: 'V6 Patterns',
      recommended: true,
    },
    schema: [{
      type: 'object',
      properties: {
        patterns: {
          type: 'array',
          items: { type: 'string' },
        },
      },
      additionalProperties: false,
    }],
  },

  create(context) {
    const options = context.options[0] || {};
    const patterns = options.patterns || [
      /^(create|update|delete|modify|mutate)/i,
      /^(apply|execute|process|handle)/i,
    ];

    return {
      FunctionDeclaration(node) {
        const functionName = node.id.name;

        // Check if function name matches mutation patterns
        const isMutation = patterns.some(pattern => pattern.test(functionName));

        if (isMutation) {
          // Check if function returns {result, receipt}
          const returnStatements = node.body.body.filter(
            stmt => stmt.type === 'ReturnStatement'
          );

          for (const stmt of returnStatements) {
            if (!stmt.argument || stmt.argument.type !== 'ObjectExpression') {
              context.report({
                node,
                message: `Function "${functionName}" must return {result, receipt} (Receipt HOF pattern). See: docs/v6-patterns/01-receipt-hof-pattern.md`,
              });
              return;
            }

            const properties = stmt.argument.properties.map(p => p.key.name);
            if (!properties.includes('result') || !properties.includes('receipt')) {
              context.report({
                node,
                message: `Function "${functionName}" must return {result, receipt} (Receipt HOF pattern). See: docs/v6-patterns/01-receipt-hof-pattern.md`,
              });
            }
          }
        }
      },
    };
  },
};
```

**Usage** (add to `.eslintrc.cjs`):
```javascript
{
  plugins: ['unrdf'],
  rules: {
    'unrdf/require-receipt': ['error', {
      patterns: [
        /^(create|update|delete|modify|mutate)/i,
        /^(apply|execute|process|handle)/i,
      ],
    }],
  },
}
```

### 2. Require Context Parameter

**Rule**: Exported functions (except utilities) MUST have `context` parameter.

```javascript
// eslint-plugin-unrdf/lib/rules/require-context.js

module.exports = {
  meta: {
    type: 'problem',
    docs: {
      description: 'Require DeterminismContext parameter for deterministic functions',
      category: 'V6 Patterns',
      recommended: true,
    },
  },

  create(context) {
    return {
      ExportNamedDeclaration(node) {
        if (node.declaration && node.declaration.type === 'FunctionDeclaration') {
          const func = node.declaration;
          const params = func.params.map(p => p.name);

          // Check if function has 'context' parameter
          if (!params.includes('context')) {
            context.report({
              node: func,
              message: `Exported function "${func.id.name}" must have a "context" parameter (DeterminismContext). See: docs/v6-patterns/04-determinism-envelope.md`,
            });
          }
        }
      },
    };
  },
};
```

### 3. Require Zod Validation

**Rule**: Exported functions MUST call `.parse()` on inputs and outputs.

```javascript
// eslint-plugin-unrdf/lib/rules/require-zod-validation.js

module.exports = {
  meta: {
    type: 'problem',
    docs: {
      description: 'Require Zod validation on inputs and outputs',
      category: 'V6 Patterns',
      recommended: true,
    },
  },

  create(context) {
    return {
      ExportNamedDeclaration(node) {
        if (node.declaration && node.declaration.type === 'FunctionDeclaration') {
          const func = node.declaration;
          const sourceCode = context.getSourceCode();
          const functionText = sourceCode.getText(func);

          // Check for .parse() call
          if (!functionText.includes('.parse(')) {
            context.report({
              node: func,
              message: `Exported function "${func.id.name}" must validate inputs/outputs with Zod (.parse()). See: docs/v6-patterns/03-zod-validation-layer.md`,
            });
          }
        }
      },
    };
  },
};
```

---

## Package-Level ESLint Config

Each package can extend the root config:

```javascript
// packages/my-package/.eslintrc.cjs
module.exports = {
  extends: '../../.eslintrc.cjs', // Extend root config
  rules: {
    // Package-specific overrides (if needed)
  },
};
```

---

## Pre-Commit Hook (Husky)

Enforce linting BEFORE commits:

```bash
# .husky/pre-commit
#!/bin/sh
. "$(dirname "$0")/_/husky.sh"

# Run ESLint on staged files
npx lint-staged
```

```json
// package.json
{
  "lint-staged": {
    "*.mjs": [
      "eslint --fix",
      "prettier --write"
    ]
  }
}
```

---

## CI/CD Enforcement

Fail builds if linting fails:

```yaml
# .github/workflows/ci.yml
name: CI

on: [push, pull_request]

jobs:
  lint:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-node@v3
        with:
          node-version: 18
      - run: pnpm install
      - run: pnpm lint # ❌ Fail build if linting fails
      - run: pnpm test
```

---

## ESLint Rules Summary

| Rule | What it Enforces | Pattern |
|------|------------------|---------|
| `no-restricted-globals` (Date) | NO `Date.now()` | Determinism Envelope |
| `no-restricted-syntax` (Math.random) | NO `Math.random()` | Determinism Envelope |
| `no-restricted-syntax` (crypto.randomUUID) | NO `crypto.randomUUID()` | Determinism Envelope |
| `no-restricted-syntax` (process.env) | NO `process.env.*` | Determinism Envelope |
| `no-restricted-syntax` (store.add) | NO direct `store.add()` | Delta Contract |
| `jsdoc/require-jsdoc` | ALL public functions have JSDoc | Zod Validation |
| `unrdf/require-receipt` | Mutations return `{result, receipt}` | Receipt HOF |
| `unrdf/require-context` | Functions have `context` param | Determinism Envelope |
| `unrdf/require-zod-validation` | Functions call `.parse()` | Zod Validation |

---

## Testing ESLint Rules

**Good Code** (passes linting):
```javascript
/**
 * Create User - With all L5 patterns
 *
 * @param {Object} input
 * @param {DeterminismContext} context
 * @returns {{result: User, receipt: Receipt}}
 */
export function createUser(input, context) {
  const validated = CreateUserInputSchema.parse(input); // ✅ Zod
  const user = {
    id: context.random.uuid(), // ✅ Injected randomness
    createdAt: context.time.now(), // ✅ Injected time
    ...validated,
  };
  const validatedUser = UserSchema.parse(user); // ✅ Output validation
  return withReceipt(
    () => validatedUser,
    { operationName: 'createUser', actor: 'user-module', ...context }
  ); // ✅ Receipt HOF
}
```

**Bad Code** (fails linting):
```javascript
// ❌ NO JSDoc
export function createUser(input) { // ❌ No context parameter
  const user = {
    id: crypto.randomUUID(), // ❌ Banned: crypto.randomUUID
    createdAt: Date.now(), // ❌ Banned: Date.now
  };
  return user; // ❌ No {result, receipt}
}
```

**Lint Output**:
```
error: Use injected RandomProvider (context.random.uuid()) instead of crypto.randomUUID()
error: Use injected TimeProvider (context.time.now()) instead of Date.now()
error: Function "createUser" must have a "context" parameter (DeterminismContext)
error: Function "createUser" must return {result, receipt} (Receipt HOF pattern)
error: Missing JSDoc comment
```

---

## Exceptions (When to Disable Rules)

### Allowed Exceptions

Some files can violate rules (e.g., providers):

```javascript
// time-provider.mjs
// eslint-disable-next-line no-restricted-globals
export function createRealTimeProvider() {
  return {
    now() {
      return process.hrtime.bigint(); // ✅ OK: This IS the time provider
    },
  };
}
```

### Files with Automatic Exceptions

Add to ESLint config:
```javascript
overrides: [
  {
    files: [
      '**/determinism-context.mjs',
      '**/time-provider.mjs',
      '**/random-provider.mjs',
      '**/environment-provider.mjs',
    ],
    rules: {
      'no-restricted-globals': 'off',
      'no-restricted-syntax': 'off',
    },
  },
],
```

---

## Incremental Adoption

If migrating an existing codebase, use warnings instead of errors:

```javascript
// .eslintrc.cjs (during migration)
{
  rules: {
    'unrdf/require-receipt': 'warn', // Warn, don't fail build
    'unrdf/require-context': 'warn',
    'unrdf/require-zod-validation': 'warn',
  },
}
```

Then upgrade to `'error'` after fixing all violations:
```javascript
{
  rules: {
    'unrdf/require-receipt': 'error', // Now fail build
    'unrdf/require-context': 'error',
    'unrdf/require-zod-validation': 'error',
  },
}
```

---

## Recommended ESLint Plugins

Install these plugins for enhanced enforcement:

```bash
pnpm add -D eslint-plugin-jsdoc eslint-plugin-no-only-tests eslint-config-prettier
```

```javascript
// .eslintrc.cjs
{
  extends: [
    'eslint:recommended',
    'plugin:jsdoc/recommended',
    'prettier',
  ],
  plugins: [
    'jsdoc',
    'no-only-tests',
  ],
}
```

---

## Summary

**ESLint Enforcement** = Fail fast, fail early. If a pattern can be checked statically, enforce it automatically.

**Benefits**:
- Catch L5 violations BEFORE code review
- Reduce manual enforcement burden
- Ensure consistency across 47 packages
- Enable incremental migration (warn → error)

**Action Items**:
1. Copy `.eslintrc.cjs` to root
2. Install ESLint plugins
3. Add pre-commit hook (Husky)
4. Enable CI/CD linting
5. Fix all linting errors in P0+P1 packages

**Next**: Run `pnpm lint` and fix all violations. No exceptions for L5 packages.
