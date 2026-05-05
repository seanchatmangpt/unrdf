# UNRDF latest Smoke Test

Quick smoke test to verify UNRDF knowledge engine functionality.

## What This Tests

1. ✅ **Knowledge Engine Initialization** - Dark Matter 80/20 core with 6 components
2. ✅ **RDF Parsing** - Turtle format parsing with N3.js
3. ✅ **Store Operations** - Subject querying and data access
4. ✅ **Transactions** - ACID transaction execution with hooks
5. ✅ **Core Components** - Verification of all 6 loaded components
6. ✅ **Cleanup** - Resource cleanup and disposal

## Quick Start

### Using Production Package (Recommended)

```bash
cd playground/smoke-test
pnpm install-prod
pnpm test
```

### Using Different Test Sets

```bash
# Test all README examples
pnpm test:readme

# Test engine core functionality
pnpm test:engine

# Test individual components
pnpm test:composables
pnpm test:utils
```

## Expected Output

```
╔════════════════════════════════════════════════╗
║   UNRDF latest Knowledge Engine Smoke Test    ║
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
║  UNRDF latest smoke test successful! 🚀       ║
╚════════════════════════════════════════════════╝
```

## Test Data

See `data.ttl` for sample RDF data:
- 2 FOAF persons (Alice, Bob)
- 1 Project with budget
- Friendship relationships
- Contact information

## Files

### Core Engine Tests
- `smoke-test.mjs` - Main engine test script
- `all-tests.mjs` - Runs all engine component tests
- `composables-test.mjs` - Tests composable functions
- `utils-test.mjs` - Tests utility functions

### README Example Tests  
- `01-quick-start.mjs` - Quick Start example (lines 64-100)
- `02-simple-knowledge-graph.mjs` - Simple Knowledge Graph (lines 332-377)
- `03-policy-driven-validation.mjs` - Policy-Driven Validation (lines 379-423)
- `04-cryptographic-audit-trail.mjs` - Cryptographic Audit Trail (lines 426-460)
- `05-dark-matter-optimization.mjs` - Dark Matter 80/20 Optimization (lines 268-283)
- `06-opentelemetry-observability.mjs` - OpenTelemetry Observability (lines 285-302)

### Test Infrastructure
- `run-all.mjs` - Runs all README example tests
- `data.ttl` - Sample RDF data in Turtle format
- `package.json` - Dependencies and scripts
- `README.md` - This file

## Troubleshooting

### Module not found

Make sure you've installed the production package:
```bash
pnpm install-prod
```

### Tests fail

1. Check that you're using Node.js 18+
2. Verify UNRDF is installed: `pnpm list unrdf`
3. Try reinstalling: `pnpm clean-install && pnpm install-prod`

### Package manager issues

This smoke test requires PNPM. Install it globally:
```bash
npm install -g pnpm
```

### Testing different versions

To test a specific version:
```bash
pnpm add unrdf@latest
pnpm test
```

## Next Steps

After smoke test passes:
- Review output for performance metrics
- Check OTEL spans for observability
- Explore example scripts in `/examples`
- Read full API docs in `/docs`
