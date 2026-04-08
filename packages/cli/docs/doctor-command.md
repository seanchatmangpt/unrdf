# Doctor CLI Reference

> Comprehensive diagnostic tool for UNRDF development environment validation, system health monitoring, and code quality assessment.

## Overview

The `unrdf doctor` command provides a unified health check system that aggregates scattered diagnostic functionality across the UNRDF monorepo. It validates developer environment, system state, code quality, and external integrations with actionable remediation steps.

## Quick Start

```bash
# Quick health check (30 seconds)
unrdf doctor

# Standard check with quality metrics (2 minutes)
unrdf doctor --mode standard

# Full diagnostics including integrations (5 minutes)
unrdf doctor --mode full

# Check specific category only
unrdf doctor --category system

# Output as JSON for CI/CD
unrdf doctor --format json

# Auto-fix safe issues
unrdf doctor --fix

# Continuous monitoring mode
unrdf doctor --watch
```

## Command Options

### --mode | -m

Check execution mode with different depth and duration.

**Values:**

- `quick` - Fast environment and system checks (30 seconds)
- `standard` - Adds code quality checks (2 minutes)
- `full` - Includes integration checks (5 minutes)

**Default:** `standard`

**Examples:**

```bash
# Quick mode for fast feedback
unrdf doctor --mode quick

# Full mode for comprehensive diagnostics
unrdf doctor --mode full
```

### --category | -c

Run checks for a specific category only.

**Values:**

- `env` - Environment checks (Node.js, pnpm, dependencies, tools)
- `system` - System state (build artifacts, daemon, MCP, RDF store, ports, disk)
- `quality` - Code quality (coverage, lint, file size, TypeScript, N3 imports, skipped tests)
- `integration` - External integrations (federation, OTEL, Docker, optional services)

**Examples:**

```bash
# Check only environment
unrdf doctor --category env

# Check only system state
unrdf doctor --category system

# Check code quality only
unrdf doctor --category quality
```

### --format | -f

Output format for different use cases.

**Values:**

- `human` - Human-readable with emoji icons and structured tables
- `json` - Machine-readable JSON for CI/CD integration
- `yaml` - YAML format for configuration files

**Default:** `human`

**Examples:**

```bash
# Human-readable output (default)
unrdf doctor

# JSON for automated parsing
unrdf doctor --format json

# YAML for config generation
unrdf doctor --format yaml
```

### --fix | -x

Attempt automatic fixes for safe, reversible operations.

**Safe operations:**

- `pnpm install --frozen-lockfile` - Fix node_modules consistency
- `pnpm build` - Fix missing build artifacts
- `pnpm lint:fix` - Fix ESLint issues

**NOT applied:**

- Destructive operations (no file deletion, no git history modification)
- Manual fixes required (file size violations, TypeScript contamination)
- External service issues (Docker, Redis, PostgreSQL)

**Example:**

```bash
# Run with auto-fix
unrdf doctor --fix
```

### --watch | -w

Continuous monitoring mode with 5-second refresh interval.

**Behavior:**

- Clears screen between updates
- Shows timestamp for each check
- Runs until interrupted with Ctrl+C

**Example:**

```bash
# Continuous monitoring
unrdf doctor --watch

# Press Ctrl+C to exit
```

## Check Categories

### Environment Checks

Validates the development environment setup.

| Check                        | Description                                      | Critical |
| ---------------------------- | ------------------------------------------------ | -------- |
| **Node.js version**          | Verifies Node.js >= 18.0.0                       | ✅       |
| **pnpm version**             | Verifies pnpm >= 7.0.0                           | ✅       |
| **node_modules consistency** | Checks for broken symlinks and missing packages  | ❌       |
| **Workspace dependencies**   | Validates all workspace packages linked          | ❌       |
| **Environment variables**    | Compares .env.example against actual environment | ❌       |
| **Required tools**           | Verifies git, node, pnpm available               | ✅       |

### System Checks

Validates system state and services.

| Check                 | Description                                  | Critical |
| --------------------- | -------------------------------------------- | -------- |
| **Build artifacts**   | Checks dist/ exists for buildable packages   | ❌       |
| **Daemon status**     | Verifies daemon CLI commands accessible      | ❌       |
| **MCP server status** | Checks MCP server running on stdio transport | ❌       |
| **RDF store**         | Verifies RDF store directory exists          | ❌       |
| **Port availability** | Checks ports 8089, 9089, 50051 available     | ❌       |
| **Disk space**        | Verifies >= 1GB free space                   | ✅       |

### Code Quality Checks

Validates code health and standards compliance.

| Check                        | Description                                          | Threshold    |
| ---------------------------- | ---------------------------------------------------- | ------------ |
| **Test coverage**            | Calculates test coverage across packages             | >= 80%       |
| **ESLint status**            | Runs ESLint and checks for errors                    | 0 errors     |
| **File size violations**     | Finds files exceeding 500 lines                      | 0 files      |
| **TypeScript contamination** | Finds .ts/.tsx/.d.ts files (ESM + JSDoc only)        | 0 files      |
| **N3 import violations**     | Finds direct N3 imports instead of n3-justified-only | 0 violations |
| **Skipped tests**            | Counts .skip( and xit( in test files                 | 0 skipped    |

### Integration Checks

Validates external service connectivity.

| Check                        | Description                              | Required |
| ---------------------------- | ---------------------------------------- | -------- |
| **Federation peers**         | Checks federation peer connectivity      | ❌       |
| **OTEL exporter**            | Verifies Jaeger endpoint reachable       | ❌       |
| **Test containers (Docker)** | Checks Docker daemon running             | ❌       |
| **Optional services**        | Checks Redis and PostgreSQL availability | ❌       |

## Output Examples

### Human-Readable Output

```bash
$ unrdf doctor --mode quick

╔══════════════════════════════════════════════════════════╗
║           UNRDF Doctor - Health Check Report           ║
╚══════════════════════════════════════════════════════════╝
📅 2026-04-07T23:17:54.368Z

Environment
═══════════
✅ Node.js version
✅ pnpm version
✅ node_modules consistency
✅ Workspace dependencies
⚠️  Environment variables
   Expected: All required env vars set
   Actual: 66 missing env vars
   💡 Copy .env.example to .env.local and configure
✅ Required tools

System
══════
⚠️  Build artifacts
   Expected: dist/ exists for buildable packages
   Actual: Missing dist/ in: core, dark-matter, docs, federation, hooks...
   💡 Run: pnpm build (for packages with build scripts)
✅ Daemon status
✅ MCP server status
✅ RDF store
✅ Port availability
✅ Disk space

──────────────────────────────────────────────────
📊 Summary
   Total Checks: 12
   ✅ Passed: 10
   ⚠️  Warnings: 2
   ❌ Failed: 0

⚠️  Doctor found 2 warning(s). Review above for details.
   Run with --fix to attempt auto-remediation.

════════════════════════════════════════════════════
```

### JSON Output

```bash
$ unrdf doctor --format json

{
  "timestamp": "2026-04-07T23:17:54.368Z",
  "summary": {
    "total": 12,
    "passed": 10,
    "warnings": 2,
    "failed": 0,
    "overallStatus": "healthy"
  },
  "categories": [
    {
      "category": "Environment",
      "checks": [
        {
          "name": "Node.js version",
          "status": "pass",
          "expected": ">=18.0.0",
          "actual": "v25.7.0"
        },
        {
          "name": "Environment variables",
          "status": "warn",
          "expected": "All required env vars set",
          "actual": "66 missing env vars",
          "fix": "Copy .env.example to .env.local and configure"
        }
      ]
    }
  ]
}
```

### Category-Specific Output

```bash
$ unrdf doctor --category quality

Code Quality
══════════
⚠️  Test coverage
   Expected: >=80% coverage
   Actual: Could not check coverage: spawnSync /bin/sh ENOBUFS
   💡 Run: pnpm test:coverage
⚠️  ESLint status
   Expected: ESLint passes
   Actual: 1 linting error(s)
   💡 Run: pnpm lint:fix
⚠️  File size violations
   Expected: All files <=500 lines
   Actual: 23 files >500 lines
   💡 Refactor files to <=500 lines (see .eslintrc.quality-gates.json)
   Details:
     • packages/oxigraph/test/application-jtbd.test.mjs
     • packages/kgc-4d/dist/index.mjs
     • packages/hooks/test/v6-features.test.mjs
     ... and 20 more
❌ TypeScript contamination
   Expected: No TypeScript files (ESM + JSDoc only)
   Actual: 102 TypeScript file(s) found
   💡 Convert TypeScript files to ESM + JSDoc or remove
   Details:
     • tmp/readme-examples/unknown-9.ts
     • sidecar/nuxt.config.ts
     • packages/oxigraph/src/index.d.ts
     ... and 99 more
```

## JSON Output Schema

### Root Schema

```json
{
  "timestamp": "ISO8601 timestamp",
  "summary": {
    "total": "number (total checks)",
    "passed": "number (passed checks)",
    "warnings": "number (warnings)",
    "failed": "number (failed checks)",
    "overallStatus": "healthy | degraded | unhealthy"
  },
  "categories": [
    {
      "category": "string (category name)",
      "checks": [
        {
          "name": "string (check name)",
          "status": "pass | warn | fail",
          "expected": "string (expected value)",
          "actual": "string (actual value)",
          "critical": "boolean (is critical check)",
          "fix": "string (remediation step)",
          "details": "object (additional details)",
          "violations": "array (list of violations)",
          "ports": "array (port status details)",
          "services": "array (service status details)"
        }
      ]
    }
  ]
}
```

## Troubleshooting

### Environment Variables Warning

**Issue:** `66 missing env vars`

**Cause:** Environment variables defined in `.env.example` are not loaded into `process.env`.

**Solution:**

```bash
# Copy .env.example to .env.local
cp .env.example .env.local

# Load into current shell (temporary)
source .env.local

# Or use direnv for automatic loading
echo 'source .env.local' > .envrc
direnv allow
```

### Build Artifacts Warning

**Issue:** `Missing dist/ in: core, dark-matter, docs, federation, hooks...`

**Cause:** Packages with build scripts haven't been built yet.

**Solution:**

```bash
# Build all packages
pnpm build

# Or use auto-fix
unrdf doctor --fix
```

### Better-SQLite3 Build Error

**Issue:** `Could not locate the bindings file` for better-sqlite3

**Cause:** Node.js v25 compatibility issue with native modules.

**Solution:** This is a known issue with Node.js v25. Options:

1. Use Node.js v20 LTS instead
2. Skip docs package build: `pnpm build --filter '!@unrdf/docs'`
3. Wait for better-sqlite3 v13 with Node.js v25 support

### TypeScript Contamination

**Issue:** `102 TypeScript file(s) found`

**Cause:** Project should use ESM + JSDoc, not TypeScript.

**Solution:**

```bash
# Find all TypeScript files
find . -name "*.ts" -o -name "*.tsx" -o -name "*.d.ts" | grep -v node_modules

# Convert to ESM + JSDoc (manual process)
# Or remove if not needed
```

### N3 Import Violations

**Issue:** `9 N3 import violation(s)`

**Cause:** Direct N3 imports instead of using `@unrdf/core/rdf/n3-justified-only.mjs`.

**Solution:**

```bash
# Find violations
grep -r "from 'n3'" packages/ --include="*.mjs"

# Replace with
import { Store, Parser, DataFactory } from '@unrdf/core/rdf/n3-justified-only.mjs';
```

### Port Conflicts

**Issue:** `Port conflicts: :8089 (Daemon)`

**Cause:** Service already running on required port.

**Solution:**

```bash
# Find process using port
lsof -i :8089

# Kill conflicting process
kill -9 <PID>

# Or use different port (set env var)
export DAEMON_PORT=8090
```

### Test Coverage ENOBUFS Error

**Issue:** `Could not check coverage: spawnSync /bin/sh ENOBUFS`

**Cause:** Output buffer exceeded during coverage check.

**Solution:**

```bash
# Run coverage directly
pnpm test:coverage

# Or skip coverage check temporarily
unrdf doctor --category env
```

## Integration Examples

### CI/CD Pipeline

```bash
#!/bin/bash
set -e

# Run doctor in CI/CD
echo "Running health checks..."
RESULT=$(unrdf doctor --format json)

# Parse result
FAILED=$(echo "$RESULT" | jq '.summary.failed')
WARNINGS=$(echo "$RESULT" | jq '.summary.warnings')

# Fail CI if any critical failures
if [ "$FAILED" -gt 0 ]; then
  echo "❌ Doctor found $FAILED critical failure(s)"
  echo "$RESULT" | jq '.categories[].checks[] | select(.status == "fail")'
  exit 1
fi

# Warn about warnings
if [ "$WARNINGS" -gt 0 ]; then
  echo "⚠️  Doctor found $WARNINGS warning(s)"
fi

echo "✅ Health checks passed"
```

### Pre-commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit

echo "Running doctor quick check..."
unrdf doctor --mode quick

# Exit with doctor's exit code
exit $?
```

### Monitoring Script

```bash
#!/bin/bash
# Continuous monitoring

while true; do
  clear
  unrdf doctor --mode quick
  echo "Next check in 30s..."
  sleep 30
done
```

### Generate Health Report

```bash
#!/bin/bash
# Generate daily health report

REPORT_FILE="health-report-$(date +%Y%m%d).json"

unrdf doctor --mode full --format json > "$REPORT_FILE"

echo "Health report saved to $REPORT_FILE"

# Upload to monitoring service
# curl -X POST https://monitoring.example.com/health \
#   -H "Content-Type: application/json" \
#   -d @"$REPORT_FILE"
```

### Watch Mode for Development

```bash
#!/bin/bash
# Monitor while developing

unrdf doctor --watch &
DOCTOR_PID=$!

# Trap to kill doctor on exit
trap "kill $DOCTOR_PID 2>/dev/null" EXIT

# Your development work here
echo "Doctor monitoring in background..."
echo "Press Ctrl+C to stop"

wait
```

## Performance Considerations

- **Quick mode:** ~30 seconds, environment + system checks only
- **Standard mode:** ~2 minutes, adds quality checks
- **Full mode:** ~5 minutes, includes integration checks
- **Category-specific:** ~10-30 seconds depending on category
- **Watch mode:** 5-second refresh interval, minimal CPU usage
- **JSON output:** Slightly faster than formatted output
- **Auto-fix:** Adds 1-5 minutes depending on fixes needed

## Best Practices

1. **Before committing:** Run `unrdf doctor --mode quick` to verify environment
2. **Before PR:** Run `unrdf doctor --mode standard` to check quality
3. **CI/CD:** Use `--format json` for programmatic consumption
4. **Monitoring:** Use `--watch` mode during long-running operations
5. **Auto-fix:** Review fixes before running `--fix` to understand impact
6. **Environment:** Use `direnv` to automatically load `.env.local`
7. **Port conflicts:** Check `lsof -i :<port>` before starting services
8. **TypeScript contamination:** Run doctor after adding new files to detect violations early
9. **Test coverage:** Aim for >=80% across all packages
10. **File size:** Refactor files exceeding 500 lines for maintainability

## Exit Codes

- `0` - All checks passed (healthy)
- `1` - One or more checks failed (degraded or unhealthy)
- `2` - Doctor command error (invalid arguments, etc.)

## Related Commands

- `unrdf daemon status` - Show daemon health and metrics
- `unrdf mcp status` - Show MCP server status
- `unrdf mcp inspect` - List MCP tools and resources
- `pnpm test:fast` - Run quick test suite
- `pnpm lint:fix` - Auto-fix linting issues
- `pnpm build` - Build all packages

## Version History

- **v26.4.4** - Initial doctor command implementation
  - Environment, system, quality, integration checks
  - Auto-fix capability for safe operations
  - Watch mode for continuous monitoring
  - Multiple output formats (human, JSON, YAML)

## Contributing

To add new health checks:

1. Create check function in `packages/cli/src/cli/commands/doctor/checks/<category>.mjs`
2. Export function following existing pattern
3. Import and call in `check<Category>()` function
4. Update documentation with check details

Example:

```javascript
// checks/system.mjs
export async function checkNewFeature() {
  // Your check logic here
  return {
    status: 'pass',
    actual: 'Feature working',
    expected: 'Feature should work',
  };
}
```

Then add to `checkSystem()`:

```javascript
export async function checkSystem() {
  return {
    category: 'System',
    checks: [
      // ... existing checks
      {
        name: 'New Feature',
        ...(await checkNewFeature()),
      },
    ],
  };
}
```

## Support

For issues or questions about the doctor command:

- Check this documentation first
- Run `unrdf doctor --help` for command options
- Review troubleshooting section above
- Check existing issues on GitHub
- Create new issue with `--format json` output for debugging
