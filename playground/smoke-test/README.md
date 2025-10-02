# UNRDF v3.0.1 Smoke Test

Quick smoke test to verify UNRDF knowledge engine functionality.

## What This Tests

1. ✅ **Knowledge Engine Initialization** - Dark Matter 80/20 core with 6 components
2. ✅ **RDF Parsing** - Turtle format parsing with N3.js
3. ✅ **Store Operations** - Subject querying and data access
4. ✅ **Transactions** - ACID transaction execution with hooks
5. ✅ **Core Components** - Verification of all 6 loaded components
6. ✅ **Cleanup** - Resource cleanup and disposal

## Quick Start

### Using Local Version (Development)

```bash
cd playground/smoke-test
npm install
npm test
```

### Using npm Version (Production)

```bash
cd playground/smoke-test
npm run install-npm
npm test
```

## Expected Output

```
╔════════════════════════════════════════════════╗
║   UNRDF v3.0.1 Knowledge Engine Smoke Test    ║
╚════════════════════════════════════════════════╝

━━━ 1. Initialize Knowledge Engine ━━━

🌌 Initializing Dark Matter 80/20 Core...
🔧 Initializing transactionManager (25% value weight)...
✅ transactionManager initialized (contributes 25% of system value)
🔧 Initializing knowledgeHookManager (20% value weight)...
✅ knowledgeHookManager initialized (contributes 20% of system value)
🔧 Initializing effectSandbox (15% value weight)...
✅ effectSandbox initialized (contributes 15% of system value)
🔧 Initializing observability (10% value weight)...
✅ observability initialized (contributes 10% of system value)
🔧 Initializing performanceOptimizer (10% value weight)...
✅ performanceOptimizer initialized (contributes 10% of system value)
🔧 Initializing lockchainWriter (5% value weight)...
✅ lockchainWriter initialized (contributes 5% of system value)
⚡ Optimizing critical paths for 80/20 performance...
✅ Critical paths optimized for 80/20 performance
🎯 Validating 80/20 targets...
✅ Value delivery target met: 85.0% from core components
✅ Performance impact target met: 80.0% from critical optimizations
✅ Development efficiency target met: 80.0% from focused effort
✅ 80/20 targets validated successfully
✅ Dark Matter 80/20 Core initialized successfully
✅ Dark Matter 80/20 Core initialized

━━━ 2. Parse Turtle Data (N3.js) ━━━

✅ Parsed 16 RDF quads from Turtle

━━━ 3. Verify Store Operations ━━━

✅ Found 3 unique subjects in store

━━━ 4. Execute Transaction ━━━

✅ Transaction executed

━━━ 5. Verify Core Components ━━━

✅ 6 core components loaded

━━━ 6. Cleanup Resources ━━━

🧹 Cleaning up Dark Matter 80/20 Core...
✅ transactionManager cleaned up
✅ knowledgeHookManager cleaned up
✅ effectSandbox cleaned up
✅ observability cleaned up
✅ performanceOptimizer cleaned up
✅ lockchainWriter cleaned up
✅ Dark Matter 80/20 Core cleaned up
✅ Resources cleaned up

╔════════════════════════════════════════════════╗
║            🎉 ALL TESTS PASSED! 🎉            ║
║                                                ║
║  6/6 tests passed (100.0%)                    ║
║                                                ║
║  UNRDF v3.0.1 smoke test successful! 🚀       ║
╚════════════════════════════════════════════════╝
```

## Test Data

See `data.ttl` for sample RDF data:
- 2 FOAF persons (Alice, Bob)
- 1 Project with budget
- Friendship relationships
- Contact information

## Files

- `smoke-test.mjs` - Main test script
- `data.ttl` - Sample RDF data in Turtle format
- `package.json` - Dependencies and scripts
- `README.md` - This file

## Troubleshooting

### Module not found

Make sure you've installed dependencies:
```bash
npm install
```

### Tests fail

1. Check that you're using Node.js 18+
2. Verify UNRDF is installed: `npm list unrdf`
3. Try reinstalling: `npm clean-install`

### Using published package

To test against the published npm version:
```bash
npm run install-npm
npm test
```

## Next Steps

After smoke test passes:
- Review output for performance metrics
- Check OTEL spans for observability
- Explore example scripts in `/examples`
- Read full API docs in `/docs`
