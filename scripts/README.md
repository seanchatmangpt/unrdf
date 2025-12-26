# UNRDF Scripts - Developer Automation

**80/20 automation: 20% of scripts that deliver 80% of developer productivity.**

---

## Quick Reference

### Quick Start (First Time Setup)

```bash
./scripts/quick-start.sh
```

**What it does:**
- Detects environment (OS, Node, pnpm)
- Installs dependencies with frozen lockfile
- Builds all packages
- Runs validation tests
- Performs health checks

**Time saved:** 10-15 minutes per developer
**Expected duration:** 60-120 seconds

---

### Development Workflow

```bash
./scripts/dev-workflow.sh <command>
```

**Available commands:**

| Command | Description | Usage |
|---------|-------------|-------|
| `dev` | Start all dev servers with hot reload | `./scripts/dev-workflow.sh dev` |
| `test:watch [pkg]` | Test watch mode (all or specific package) | `./scripts/dev-workflow.sh test:watch core` |
| `validate` | Quick validation: lint + type-check + test | `./scripts/dev-workflow.sh validate` |
| `fix` | Auto-fix linting and formatting | `./scripts/dev-workflow.sh fix` |
| `validate:commit` | Pre-commit validation | `./scripts/dev-workflow.sh validate:commit` |
| `build:watch` | Build in watch mode (requires nodemon) | `./scripts/dev-workflow.sh build:watch` |

**Examples:**

```bash
# Start development
./scripts/dev-workflow.sh dev

# Watch tests for specific package
./scripts/dev-workflow.sh test:watch hooks

# Full validation before commit
./scripts/dev-workflow.sh validate

# Auto-fix code style
./scripts/dev-workflow.sh fix
```

---

### Common Tasks

```bash
./scripts/common-tasks.sh <command>
```

**Available commands:**

| Command | Description | Usage |
|---------|-------------|-------|
| `new:package <name>` | Scaffold a new package | `./scripts/common-tasks.sh new:package my-feature` |
| `clean:all` | Clean all build artifacts and dependencies | `./scripts/common-tasks.sh clean:all` |
| `update:deps` | Update all dependencies safely | `./scripts/common-tasks.sh update:deps` |
| `check:health` | Run system health check | `./scripts/common-tasks.sh check:health` |
| `list:packages` | List all packages in workspace | `./scripts/common-tasks.sh list:packages` |

**Examples:**

```bash
# Create new package
./scripts/common-tasks.sh new:package analytics

# Deep clean
./scripts/common-tasks.sh clean:all

# Safe dependency update (with validation)
./scripts/common-tasks.sh update:deps

# Check system health
./scripts/common-tasks.sh check:health
```

---

### Debug Helpers

```bash
./scripts/debug-helpers.sh <command>
```

**Available commands:**

| Command | Description | Usage |
|---------|-------------|-------|
| `debug:test <pkg> [file]` | Run tests with Node.js debugger | `./scripts/debug-helpers.sh debug:test core` |
| `trace:otel` | View OpenTelemetry trace results | `./scripts/debug-helpers.sh trace:otel` |
| `profile:perf <pkg>` | Generate CPU profile for package | `./scripts/debug-helpers.sh profile:perf streaming` |
| `trace:deps <pkg>` | Show dependency tree for package | `./scripts/debug-helpers.sh trace:deps federation` |
| `analyze:bundle <pkg>` | Analyze bundle size and composition | `./scripts/debug-helpers.sh analyze:bundle hooks` |
| `check:memory` | Check memory usage during tests | `./scripts/debug-helpers.sh check:memory` |
| `trace:imports <pkg>` | Show all imports in package | `./scripts/debug-helpers.sh trace:imports core` |
| `watch:logs [file]` | Watch log file in real-time | `./scripts/debug-helpers.sh watch:logs validation.log` |

**Examples:**

```bash
# Debug tests with Chrome DevTools
./scripts/debug-helpers.sh debug:test core

# View OTEL validation results
./scripts/debug-helpers.sh trace:otel

# Profile performance
./scripts/debug-helpers.sh profile:perf streaming

# Analyze bundle
./scripts/debug-helpers.sh analyze:bundle hooks

# Watch logs
./scripts/debug-helpers.sh watch:logs validation-output.log
```

---

## NPM Script Integration

All scripts are also available as npm scripts in the root `package.json`:

### Quick Start & Setup

```bash
npm run quick-start          # Run quick-start.sh
```

### Development Workflow

```bash
npm run dev                  # Start dev servers
npm run test:watch           # Test watch mode (all packages)
npm run test:watch:pkg core  # Test watch mode (specific package)
npm run validate             # Full validation
npm run fix                  # Auto-fix code style
npm run validate:commit      # Pre-commit validation
```

### Common Tasks

```bash
npm run new:package my-feature   # Scaffold new package
npm run clean:all                # Deep clean
npm run update:deps              # Update dependencies
npm run check:health             # Health check
npm run list:packages            # List all packages
```

### Debugging

```bash
npm run debug:test core          # Debug tests
npm run trace:otel               # OTEL traces
npm run profile:perf streaming   # Performance profiling
npm run analyze:bundle hooks     # Bundle analysis
npm run trace:deps federation    # Dependency tracing
npm run trace:imports core       # Import analysis
npm run watch:logs               # Watch logs
```

---

## Script Details

### quick-start.sh

**Purpose:** One-command setup for new developers

**Features:**
- Environment detection (OS, Node version, pnpm)
- Frozen lockfile installation
- Full package build
- Quick validation tests
- Health checks
- Clear success/failure messages

**Outputs:**
- `/tmp/pnpm-install.log` - Installation log
- `/tmp/pnpm-build.log` - Build log
- `/tmp/test-fast.log` - Test log

**Exit codes:**
- `0` - Success
- `1` - Failure (with detailed error list)

---

### dev-workflow.sh

**Purpose:** Daily development tasks

**Features:**
- Parallel dev server management
- Test watch modes
- Quick validation pipeline
- Auto-fix capabilities
- Pre-commit validation

**Timeouts:**
- Lint: 30s
- Build: 60s
- Tests: 120s
- Total validation: 150s

---

### common-tasks.sh

**Purpose:** Frequent operations automation

**Features:**
- Package scaffolding with templates
- Deep clean (dist, node_modules, caches)
- Safe dependency updates with validation
- System health monitoring
- Package listing

**Package template includes:**
- `package.json` with standard scripts
- `build.config.ts` for unbuild
- `src/index.mjs` with example code
- `test/index.test.mjs` with basic test
- `README.md` with usage guide

---

### debug-helpers.sh

**Purpose:** Debugging and profiling tools

**Features:**
- Node.js debugger integration
- OTEL trace visualization
- CPU profiling
- Bundle analysis
- Dependency tracing
- Import analysis
- Log watching

**Profile outputs:**
- CPU profiles: `profiles/<package>-<timestamp>.cpuprofile`
- Open in Chrome DevTools: `chrome://inspect` → Load Profile

---

## Best Practices

### 1. Always Use Timeouts

```bash
# ✓ GOOD
timeout 5s npm test

# ✗ BAD
npm test
```

**Default timeout: 5 seconds** (justified exceptions: 10-20s)

### 2. Read Actual Output

```bash
# ✓ GOOD
npm test 2>&1 | tee test-output.log
grep "PASS\|FAIL" test-output.log

# ✗ BAD
npm test && echo "Tests passed"
```

### 3. Validate with OTEL

```bash
./scripts/debug-helpers.sh trace:otel
grep "Score:" validation-output.log  # MUST be ≥80/100
```

### 4. Health Check Before Big Changes

```bash
./scripts/common-tasks.sh check:health
```

---

## Performance Expectations

| Script | Expected Duration | Timeout |
|--------|-------------------|---------|
| `quick-start.sh` | 60-120s | 300s |
| `dev-workflow.sh validate` | 30-60s | 150s |
| `common-tasks.sh update:deps` | 120-180s | 300s |
| `debug-helpers.sh profile:perf` | 10-30s | 60s |

**If timeouts fire:** Investigate root cause, don't just increase timeout.

---

## Troubleshooting

### Script Fails: Permission Denied

```bash
chmod +x scripts/*.sh
```

### Quick Start Fails at Build

```bash
# Check build log
cat /tmp/pnpm-build.log

# Clean and retry
npm run clean:all
./scripts/quick-start.sh
```

### Validation Times Out

```bash
# Profile to find bottleneck
npm run profile:perf <slow-package>

# Check health
npm run check:health
```

### OTEL Score Below 80

```bash
# View failures
./scripts/debug-helpers.sh trace:otel

# Check specific package
npm run debug:test <package>
```

---

## Adding New Scripts

**Follow the 80/20 principle:**

1. **Identify high-impact tasks** (saves >5 min/developer/day)
2. **Create focused script** (single responsibility)
3. **Add timeouts** (prevent hangs)
4. **Provide clear output** (colors, success/failure messages)
5. **Update documentation** (this file + DEVELOPER-GUIDE.md)
6. **Add npm script** (root package.json)

**Template:**

```bash
#!/usr/bin/env bash
set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

# Color output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m'

print_success() {
    echo -e "${GREEN}✓${NC} $1"
}

# Implementation
timeout 5s your_command && print_success "Task completed"
```

---

## Integration with CI/CD

### GitHub Actions

```yaml
- name: Quick Start Validation
  run: ./scripts/quick-start.sh

- name: Full Validation
  run: ./scripts/dev-workflow.sh validate

- name: Health Check
  run: ./scripts/common-tasks.sh check:health
```

### Pre-commit Hook

```bash
#!/bin/bash
./scripts/dev-workflow.sh validate:commit
```

---

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `PROJECT_ROOT` | Project root directory | Auto-detected |
| `NODE_OPTIONS` | Node.js options | - |
| `TIMEOUT` | Default command timeout | 5s |

**Example:**

```bash
# Increase memory for large operations
NODE_OPTIONS="--max-old-space-size=4096" ./scripts/quick-start.sh
```

---

## Maintenance

### Regular Tasks

1. **Weekly:** Run `npm run check:health`
2. **Before release:** Run full validation suite
3. **After updates:** Verify all scripts still work
4. **Monthly:** Review and update timeouts based on metrics

### Monitoring

```bash
# Check script execution times
time ./scripts/quick-start.sh

# Monitor OTEL scores
./scripts/debug-helpers.sh trace:otel
```

---

## Support

**Issues with scripts:**

1. Check logs in `/tmp/`
2. Run health check: `npm run check:health`
3. View OTEL traces: `npm run trace:otel`
4. Open issue with full output

**Contributing improvements:**

1. Test thoroughly
2. Follow existing patterns
3. Update documentation
4. Ensure timeouts are set

---

## Summary

**Four scripts, massive productivity boost:**

1. **quick-start.sh** - One-command setup (saves 10-15 min)
2. **dev-workflow.sh** - Daily development (saves 5-10 min/day)
3. **common-tasks.sh** - Frequent operations (saves 3-5 min/task)
4. **debug-helpers.sh** - Troubleshooting (saves 10-20 min/issue)

**Total time saved:** 30-60 minutes per developer per day

**Remember:** Focus on the 20% of automation that delivers 80% of value.
