# UNRDF Developer Guide

**Welcome to UNRDF development!** This guide covers the essential workflows and tools that deliver 80% of developer productivity.

## Quick Start (First Time Setup)

**One command to set up everything:**

```bash
./scripts/quick-start.sh
```

This script (saves 10-15 minutes per developer):
- ✓ Detects your environment (OS, Node, pnpm)
- ✓ Installs dependencies with frozen lockfile
- ✓ Builds all packages
- ✓ Runs validation tests
- ✓ Performs health checks

**Expected output:**
```
✓ Quick Start SUCCESSFUL

Next steps:
  1. Run 'npm run dev' to start development servers
  2. Run 'npm run test:watch' for test-driven development
  3. See DEVELOPER-GUIDE.md for full workflow documentation
```

---

## Daily Development Workflow

### 1. Starting Development

```bash
# Start all dev servers with hot reload
npm run dev

# Or use the script directly
./scripts/dev-workflow.sh dev
```

### 2. Test-Driven Development

```bash
# Watch mode for all tests
npm run test:watch

# Watch tests for specific package
npm run test:watch:pkg core

# Or via script
./scripts/dev-workflow.sh test:watch core
```

### 3. Before Committing

```bash
# Quick validation: lint + type-check + test
npm run validate

# Auto-fix linting and formatting issues
npm run fix

# Pre-commit check (what CI will run)
npm run validate:commit
```

**Expected validation time:** 30-60 seconds (if longer, investigate!)

---

## Common Tasks

### Creating a New Package

```bash
npm run new:package my-feature

# Creates structure:
# packages/my-feature/
#   ├── src/index.mjs
#   ├── test/index.test.mjs
#   ├── package.json
#   ├── build.config.ts
#   └── README.md
```

### Cleaning Build Artifacts

```bash
# Deep clean (removes all dist, node_modules, caches)
npm run clean:all

# Then reinstall
pnpm install
```

### Updating Dependencies

```bash
# Safe update with validation
npm run update:deps

# This will:
# 1. Update pnpm
# 2. Update all dependencies
# 3. Rebuild packages
# 4. Run tests to verify
```

### Health Check

```bash
npm run check:health

# Checks:
# - Node.js version
# - pnpm installation
# - Package count
# - Broken symlinks
# - Disk space
# - Build artifacts
```

---

## Debugging Tools

### Debug Tests with Node Inspector

```bash
npm run debug:test core

# Opens debugger on port 9229
# 1. Open chrome://inspect in Chrome
# 2. Click "inspect" on your Node process
# 3. Set breakpoints using DevTools
```

### View OTEL Traces

```bash
npm run trace:otel

# Shows recent validation results and failures
```

### Profile Performance

```bash
npm run profile:perf streaming

# Generates CPU profile
# Open in Chrome DevTools: Load Profile
```

### Analyze Bundle Size

```bash
npm run analyze:bundle hooks

# Shows:
# - Bundle files
# - Total size
# - File-by-file breakdown
```

### Trace Dependencies

```bash
npm run trace:deps federation

# Shows:
# - Direct dependencies
# - Dev dependencies
# - Workspace dependencies
```

### Watch Logs in Real-Time

```bash
npm run watch:logs validation-output.log

# Tail -f equivalent with auto-creation
```

---

## NPM Scripts Reference

### Development

| Script | Description | Timeout |
|--------|-------------|---------|
| `npm run dev` | Start all dev servers | - |
| `npm run test:watch` | Watch mode for tests | - |
| `npm run validate` | Lint + type-check + test | 120s |
| `npm run fix` | Auto-fix lint/format | 50s |

### Testing

| Script | Description | Timeout |
|--------|-------------|---------|
| `npm test` | Run all tests | 120s |
| `npm run test:fast` | Fast test suite | 60s |
| `npm run test:coverage` | With coverage report | 180s |
| `npm run test:core` | Test @unrdf/core only | 30s |

### Building

| Script | Description | Timeout |
|--------|-------------|---------|
| `npm run build` | Build all packages | 120s |
| `npm run clean` | Clean artifacts | 30s |

### Code Quality

| Script | Description | Timeout |
|--------|-------------|---------|
| `npm run lint` | Lint all code | 30s |
| `npm run lint:fix` | Auto-fix linting | 30s |
| `npm run format` | Format code | 20s |
| `npm run format:check` | Check formatting | 20s |

### Common Tasks

| Script | Description |
|--------|-------------|
| `npm run new:package <name>` | Scaffold new package |
| `npm run clean:all` | Deep clean all artifacts |
| `npm run update:deps` | Safe dependency update |
| `npm run check:health` | System health check |
| `npm run list:packages` | List all packages |

### Debugging

| Script | Description |
|--------|-------------|
| `npm run debug:test <pkg>` | Debug package tests |
| `npm run trace:otel` | View OTEL traces |
| `npm run profile:perf <pkg>` | CPU profiling |
| `npm run analyze:bundle <pkg>` | Bundle analysis |
| `npm run trace:deps <pkg>` | Dependency tracing |
| `npm run trace:imports <pkg>` | Import analysis |
| `npm run watch:logs [file]` | Watch logs |

---

## Project Structure

```
unrdf/
├── packages/              # Monorepo packages
│   ├── core/             # @unrdf/core - Core RDF functionality
│   ├── hooks/            # @unrdf/hooks - React hooks
│   ├── streaming/        # @unrdf/streaming - Streaming APIs
│   ├── federation/       # @unrdf/federation - Federation
│   └── ...               # 22+ packages total
├── scripts/              # Development automation
│   ├── quick-start.sh    # One-command setup
│   ├── dev-workflow.sh   # Daily workflow tasks
│   ├── common-tasks.sh   # Common operations
│   └── debug-helpers.sh  # Debugging tools
├── docs/                 # Documentation
├── validation/           # OTEL validation scripts
└── package.json          # Root workspace config
```

---

## Best Practices

### 1. Use Timeouts for All Commands

**Why:** Prevents silent hangs, catches performance regressions early.

```bash
# ✓ GOOD
timeout 5s npm test

# ✗ BAD
npm test  # No timeout = potential infinite hang
```

**Default timeout: 5 seconds.** If exceeded, investigate root cause.

### 2. Batch Operations

**Why:** Faster execution, better resource utilization.

```bash
# ✓ GOOD - Single message, parallel execution
pnpm lint && pnpm test:fast

# ✗ BAD - Sequential, separate commands
pnpm lint
pnpm test:fast
```

### 3. Validate with OTEL

**Why:** Agent claims need external validation.

```bash
node validation/run-all.mjs comprehensive
grep "Score:" validation-output.log  # MUST be ≥80/100
```

**Trust model:**
- Agent claims: 0% trust
- OTEL spans: 95% trust
- Test output: 90% trust
- "It should work": 10% trust

### 4. Read Actual Output

**Why:** Success indicators can be misleading.

```bash
# ✓ GOOD - Read full output
npm test 2>&1 | tee test-output.log
grep -E "PASS|FAIL" test-output.log

# ✗ BAD - Trust exit code only
npm test && echo "Tests passed"
```

---

## Troubleshooting

### Tests Failing After Update

```bash
# 1. Clean everything
npm run clean:all

# 2. Reinstall
pnpm install --frozen-lockfile

# 3. Rebuild
timeout 120s pnpm build

# 4. Run tests
timeout 120s pnpm test:fast
```

### Slow Build Times

```bash
# 1. Check what's taking time
time pnpm build

# 2. Profile specific package
npm run profile:perf <slow-package>

# 3. Check disk space
npm run check:health
```

### Import Errors

```bash
# 1. Check if package is built
ls packages/<package>/dist

# 2. Trace imports
npm run trace:imports <package>

# 3. Verify dependencies
npm run trace:deps <package>
```

### Memory Issues

```bash
# Run tests with memory tracking
npm run check:memory

# Increase heap size if needed
NODE_OPTIONS="--max-old-space-size=4096" pnpm test
```

---

## Code Style

### RDF/Triple Store (MANDATORY)

```javascript
// ✓ CORRECT
import { createStore, dataFactory } from '@unrdf/oxigraph';

// ✗ WRONG - Never import from N3 in app code
import { Store } from 'n3';
```

### Type Hints

**100% JSDoc coverage required:**

```javascript
/**
 * Process RDF triples
 * @param {import('@unrdf/core').Triple[]} triples - Input triples
 * @returns {Promise<number>} Number of processed triples
 */
export async function processTriples(triples) {
  // Implementation
}
```

### File Size

**Maximum 500 lines per file.** Check with:

```bash
wc -l src/**/*.mjs | sort -n | tail -10
```

---

## CI/CD Integration

### Pre-commit Hook

```bash
# Runs automatically on git commit
npm run validate:commit

# Equivalent to:
pnpm lint && pnpm test:fast
```

### Local CI Simulation

```bash
# Run exactly what CI runs
npm run validate        # Full validation
npm run test:coverage   # Coverage report
npm run build           # Production build
```

---

## Performance Expectations

| Operation | Expected Time | Timeout |
|-----------|---------------|---------|
| Quick start | 60-120s | 300s |
| Build | 30-60s | 120s |
| Fast tests | 10-30s | 60s |
| Full tests | 30-90s | 120s |
| Lint | 5-10s | 30s |
| Validation | 30-60s | 150s |

**If times exceed expectations:** Investigate with profiling tools.

---

## Getting Help

1. **Check health:** `npm run check:health`
2. **View logs:** `npm run watch:logs`
3. **Trace OTEL:** `npm run trace:otel`
4. **Profile:** `npm run profile:perf <package>`
5. **Open issue:** Include full output, environment info

---

## 80/20 Principle

**Focus on high-impact automation:**

✅ **DO:** Use scripts for repetitive tasks
✅ **DO:** Validate with actual command output
✅ **DO:** Profile when performance degrades
✅ **DO:** Use timeouts to catch hangs early

❌ **DON'T:** Over-engineer for edge cases
❌ **DON'T:** Trust claims without evidence
❌ **DON'T:** Ignore timeout violations
❌ **DON'T:** Skip validation before commit

---

## Quick Reference Card

```bash
# Setup
./scripts/quick-start.sh              # First time setup

# Daily workflow
npm run dev                           # Start dev servers
npm run test:watch                    # TDD mode
npm run validate                      # Before commit
npm run fix                           # Auto-fix issues

# Common tasks
npm run new:package <name>            # New package
npm run clean:all                     # Deep clean
npm run update:deps                   # Update deps
npm run check:health                  # Health check

# Debugging
npm run debug:test <pkg>              # Debug tests
npm run trace:otel                    # OTEL traces
npm run profile:perf <pkg>            # CPU profile
npm run analyze:bundle <pkg>          # Bundle size
```

---

**Remember:** The goal is developer productivity through focused automation. Use these tools to spend less time on setup and more time on creating value.
