# @unrdf/v6-compat

**UNRDF v6 Compatibility Layer** - Migration bridge from v5 to v6.

## Features

- **API Adapters**: Wraps deprecated v5 APIs with v6 equivalents
- **Deprecation Warnings**: Clear migration hints in console
- **Receipt Generation**: Auto-wrap v5 operations with receipts
- **ESLint Rules**: Detect deprecated patterns in CI
- **Schema Generator**: Generate Zod schemas from JSDoc/TS types

## Installation

```bash
pnpm add @unrdf/v6-compat
```

## Usage

### API Adapters

```javascript
import { createStore, wrapWorkflow } from '@unrdf/v6-compat/adapters';

// v5 Store → v6 Oxigraph (with warning)
const store = await createStore();

// v5 workflow.run() → v6 workflow.execute() + receipt
const workflow = getWorkflow();
const wrapped = wrapWorkflow(workflow);
const { result, receipt } = await wrapped.execute(task);
```

### ESLint Rules

```javascript
// eslint.config.mjs
import { plugin as unrdfV6 } from '@unrdf/v6-compat/lint-rules';

export default [
  {
    plugins: { 'unrdf-v6': unrdfV6 },
    rules: {
      'unrdf-v6/no-n3-imports': 'error',
      'unrdf-v6/no-workflow-run': 'warn',
      'unrdf-v6/require-timeout': 'error',
      'unrdf-v6/no-date-now': 'error'
    }
  }
];
```

Run linter:

```bash
pnpm eslint . --fix
```

### Schema Generator

```javascript
import { parseJSDocToZod } from '@unrdf/v6-compat/schema-generator';

const jsdoc = `
  @typedef {Object} User
  @property {string} id - User ID
  @property {string} name - User name
`;

const schema = parseJSDocToZod(jsdoc);
console.log(schema);
// z.object({ id: z.string(), name: z.string() })
```

## Migration Tracking

```javascript
import { migrationTracker } from '@unrdf/v6-compat/adapters';

// ... run your app with adapters ...

// Print migration report
migrationTracker.summary();
```

Output:

```
📊 Migration Status Report
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Total deprecation warnings: 42
Unique deprecated APIs: 7
Elapsed time: 3521ms

Top deprecated APIs:
  18x new Store() from n3
  12x workflow.run(task)
  8x federation.query(string)
  3x stream.on("data")
  1x Date.now()

📖 Migration guide: /docs/v6/MIGRATION_PLAN.md
```

## License

MIT
