# How-To: Migrate a v5 Package to v6

**Time**: ~2-4 hours per package
**Difficulty**: Intermediate
**Prerequisites**: Existing v5 package, understanding of [7 breaking changes](../../MIGRATION_PLAN.md#breaking-changes-summary)

---

## Overview

This guide walks you through migrating a v5 package to v6, addressing all 7 breaking changes:

1. ✅ Store initialization (N3 → Oxigraph)
2. ✅ Receipt-driven operations
3. ✅ Zod schema validation
4. ✅ Pure ESM
5. ✅ Hook lifecycle changes
6. ✅ Federation query API
7. ✅ Streaming API changes

**Example**: We'll migrate `@unrdf/example-package` from v5.2.0 to v6.0.0.

---

## Step 1: Install Compatibility Layer

First, install `@unrdf/v6-compat` to bridge v5 and v6 APIs:

```bash
cd packages/example-package
pnpm add @unrdf/v6-compat @unrdf/oxigraph @unrdf/kgc-4d
```

---

## Step 2: Update package.json

### Before (v5):
```json
{
  "name": "@unrdf/example-package",
  "version": "5.2.0",
  "main": "dist/index.js",
  "module": "dist/index.mjs",
  "exports": {
    "require": "./dist/index.js",
    "import": "./dist/index.mjs"
  },
  "scripts": {
    "build": "tsc && rollup -c",
    "test": "jest"
  },
  "dependencies": {
    "n3": "^1.16.0"
  }
}
```

### After (v6):
```json
{
  "name": "@unrdf/example-package",
  "version": "6.0.0",
  "type": "module",
  "main": "src/index.mjs",
  "exports": {
    ".": "./src/index.mjs",
    "./adapters": "./src/adapters.mjs"
  },
  "scripts": {
    "build": "echo 'No build step for ESM'",
    "test": "node --test test/**/*.test.mjs",
    "lint": "eslint src/**/*.mjs"
  },
  "dependencies": {
    "@unrdf/oxigraph": "workspace:*",
    "@unrdf/kgc-4d": "workspace:*",
    "@unrdf/v6-compat": "workspace:*",
    "zod": "^3.22.4"
  },
  "peerDependencies": {
    "@unrdf/core": "workspace:*"
  }
}
```

**Key Changes:**
- ✅ `"type": "module"` (Pure ESM)
- ✅ Direct source exports (no build step)
- ✅ Oxigraph + KGC-4D dependencies
- ✅ Zod for validation
- ❌ Removed N3 dependency

---

## Step 3: Migrate Store Initialization

### Before (v5):
```javascript
import { Store } from 'n3';

function createGraph() {
  const store = new Store();
  return store;
}
```

### After (v6):
```javascript
import { createStore } from '@unrdf/oxigraph';
import { withReceipt } from '@unrdf/v6-core/receipts';

const createGraph = withReceipt(async function createGraph() {
  const store = await createStore();
  return store;
});

// Usage
const { value: store, receipt } = await createGraph();
console.log('Store created, receipt:', receipt.hash);
```

**Why the change?**
- Oxigraph is 10x faster than N3 for SPARQL
- `withReceipt()` wraps function to generate execution proof
- Async initialization allows for WASM loading

---

## Step 4: Add Zod Schemas

All public functions must have Zod schemas in v6.

### Before (v5):
```javascript
export function processData(data) {
  // No validation
  return data.value * 2;
}
```

### After (v6):
```javascript
import { z } from 'zod';

const ProcessDataSchema = z.object({
  value: z.number().positive(),
  metadata: z.object({
    source: z.string(),
    timestamp: z.number().int()
  }).optional()
});

export const processData = withReceipt(function processData(data) {
  const validated = ProcessDataSchema.parse(data); // Throws if invalid
  return validated.value * 2;
});

// Usage with validation
try {
  const { value, receipt } = await processData({ value: 42, metadata: { source: 'api', timestamp: Date.now() } });
  console.log('Result:', value); // 84
} catch (error) {
  console.error('Validation failed:', error.errors);
}
```

**Auto-generate schemas** from TypeScript types:
```bash
npx @unrdf/v6-compat schema-generator --input src/types.ts --output src/schemas.mjs
```

---

## Step 5: Migrate Hooks (If Applicable)

### Before (v5):
```javascript
import { defineHook } from '@unrdf/hooks';

const validateHook = defineHook({
  name: 'validate-commit',
  handler: async (context) => {
    // Validation logic
    return context.valid;
  }
});
```

### After (v6):
```javascript
import { defineHook } from '@unrdf/hooks';
import { z } from 'zod';

const HookContextSchema = z.object({
  commit: z.string(),
  files: z.array(z.string()),
  valid: z.boolean()
});

const validateHook = defineHook({
  name: 'validate-commit',
  schema: HookContextSchema,          // Required in v6
  receipt: true,                      // Generate receipt
  handler: async (context) => {
    const validated = HookContextSchema.parse(context);
    // Validation logic
    return validated.valid;
  }
});

// Activate hook explicitly
import { activateHook } from '@unrdf/hooks';
const { value, receipt } = await activateHook(validateHook, {
  commit: 'abc123',
  files: ['src/index.mjs'],
  valid: true
});
```

---

## Step 6: Migrate Federation Queries (If Applicable)

### Before (v5):
```javascript
const results = await federation.query('SELECT * WHERE { ?s ?p ?o }');
```

### After (v6):
```javascript
import { sparql } from '@unrdf/federation';

const results = await federation.query(
  sparql`SELECT * WHERE { ?s ?p ?o }`
    .timeout(5000)      // Required timeout guard
    .receipt(true)      // Generate receipt
);

// Receipt available after query
console.log('Query receipt:', results.receipt.hash);
```

**Or use compatibility adapter:**
```javascript
import { querySparql } from '@unrdf/v6-compat/adapters';

const results = await querySparql(federation, 'SELECT * WHERE { ?s ?p ?o }');
// Automatically adds timeout and receipt
```

---

## Step 7: Migrate Streaming (If Applicable)

### Before (v5 - EventEmitter):
```javascript
stream.on('data', (quad) => {
  console.log('Quad:', quad);
});
stream.on('error', (err) => {
  console.error('Error:', err);
});
stream.on('end', () => {
  console.log('Done');
});
```

### After (v6 - AsyncIterator):
```javascript
try {
  for await (const quad of stream) {
    console.log('Quad:', quad);
  }
  console.log('Done');

  // Receipt available after stream completes
  const receipt = stream.receipt();
  console.log('Stream receipt:', receipt.hash);
} catch (error) {
  console.error('Error:', error);
}
```

**Or use compatibility adapter:**
```javascript
import { streamToAsync } from '@unrdf/v6-compat/adapters';

const asyncStream = streamToAsync(stream);
for await (const quad of asyncStream) {
  console.log('Quad:', quad);
}
```

---

## Step 8: Add ESLint Rules

Add v6-specific ESLint rules to catch migration issues:

Create `.eslintrc.json`:
```json
{
  "extends": ["@unrdf/eslint-config"],
  "plugins": ["@unrdf/v6-compat"],
  "rules": {
    "@unrdf/v6-compat/no-n3-imports": "error",
    "@unrdf/v6-compat/no-workflow-run": "error",
    "@unrdf/v6-compat/require-zod-validation": "warn",
    "@unrdf/v6-compat/require-timeout": "error",
    "@unrdf/v6-compat/no-date-now": "error"
  }
}
```

Run linter:
```bash
npx eslint src/**/*.mjs
```

**Fix common issues:**
- ❌ `import { Store } from 'n3'` → ✅ `import { createStore } from '@unrdf/oxigraph'`
- ❌ `Date.now()` in business logic → ✅ Pass timestamp as parameter
- ❌ `workflow.run()` → ✅ `workflow.execute()`

---

## Step 9: Update Tests

### Before (v5 - Jest):
```javascript
describe('processData', () => {
  it('should double the value', () => {
    expect(processData({ value: 5 })).toBe(10);
  });
});
```

### After (v6 - Node Test Runner):
```javascript
import { test } from 'node:test';
import assert from 'node:assert';

test('processData should double the value and generate receipt', async () => {
  const { value, receipt } = await processData({ value: 5, metadata: { source: 'test', timestamp: 1704067200000 } });

  assert.strictEqual(value, 10);
  assert.ok(receipt.hash.startsWith('sha256:'));
  assert.strictEqual(receipt.operation, 'processData');
});
```

Run tests:
```bash
npm test
```

---

## Step 10: Verify Migration

Run the migration verification checklist:

```bash
# 1. Check for direct N3 imports (should be 0)
timeout 5s grep -r "from 'n3'" src/ --include="*.mjs" | wc -l

# 2. Check for v5 Store usage (should be 0)
timeout 5s grep -r "new Store()" src/ --include="*.mjs" | wc -l

# 3. Run tests (should pass 100%)
timeout 10s npm test

# 4. Run linter (0 errors)
timeout 5s npm run lint

# 5. Check Zod coverage (should match function count)
timeout 5s grep -r "z\.object\|z\.string" src/ --include="*.mjs" | wc -l

# 6. Verify receipts are generated
timeout 5s grep -r "withReceipt\|receipt:" src/ --include="*.mjs" | wc -l
```

**Expected Results:**
```
0  # No N3 imports
0  # No v5 Store usage
✅ All tests passed
✅ 0 linting errors
12 # Zod schemas (matches 12 public functions)
12 # Receipt-generating functions
```

---

## Step 11: Generate Migration Receipt

Prove your migration is complete with a receipt:

```bash
npx @unrdf/v6-compat migration-report --package @unrdf/example-package
```

**Output:**
```json
{
  "package": "@unrdf/example-package",
  "version": "6.0.0",
  "migrationComplete": true,
  "receipt": {
    "hash": "sha256:migration-abc123...",
    "timestamp": 1704067200000,
    "checks": {
      "noN3Imports": true,
      "noV5Store": true,
      "zodSchemas": true,
      "receiptsGenerated": true,
      "testsPass": true,
      "lintPass": true
    },
    "maturityLevel": "L3",
    "breakingChangesAddressed": [
      "store-initialization",
      "receipt-driven-operations",
      "zod-validation",
      "pure-esm",
      "hook-lifecycle",
      "federation-query",
      "streaming-api"
    ]
  }
}
```

Save this receipt to prove migration completion!

---

## Troubleshooting

### "Module not found: @unrdf/oxigraph"

**Solution**: Ensure workspace dependencies are linked:
```bash
pnpm install --force
```

### "Zod validation errors"

**Cause**: Input doesn't match schema

**Solution**: Check schema definition and input structure:
```javascript
console.log('Schema:', ProcessDataSchema.shape);
console.log('Input:', data);
```

### "Receipt hash changes between runs"

**Cause**: Non-deterministic input (e.g., `Date.now()`)

**Solution**: Pass timestamp as parameter:
```javascript
// ❌ Non-deterministic
const timestamp = Date.now();

// ✅ Deterministic
const processDataWithTime = withReceipt(function processData(data, timestamp) {
  return { ...data, timestamp };
});
```

---

## Next Steps

After migration:

1. **[How-To: Implement L5 Maturity](./04-implement-l5-maturity.md)**
   Achieve production-grade maturity

2. **[How-To: Compose Cross-Package Deltas](./02-compose-deltas.md)**
   Build complex operations from simple deltas

3. **[Explanation: Receipt Chain Verification](../explanation/02-receipt-chain-verification.md)**
   Understand receipt security model

---

## Summary Checklist

✅ Installed v6-compat
✅ Updated package.json (Pure ESM, type: module)
✅ Migrated Store → createStore (Oxigraph)
✅ Added Zod schemas to all public functions
✅ Wrapped functions with withReceipt()
✅ Migrated hooks (if applicable)
✅ Migrated federation queries (if applicable)
✅ Migrated streams (if applicable)
✅ Added ESLint rules
✅ Updated tests to Node Test Runner
✅ Verified migration (0 N3 imports, all tests pass)
✅ Generated migration receipt

**Migration Complete!** 🎉
