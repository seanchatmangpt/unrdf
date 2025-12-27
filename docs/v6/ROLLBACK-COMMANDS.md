# V6 Rollback - Essential Commands Reference

**Quick copy-paste commands for deployment day**

---

## Pre-Deployment

```bash
# Test rollback system
node scripts/test-rollback-system.mjs

# Create snapshot (MANDATORY)
node scripts/v6-snapshot.mjs pre-v6-production-$(date +%Y%m%d-%H%M)

# Verify snapshot
ls -lah .rollback-snapshots/ | tail -1

# Test rollback (dry-run)
node scripts/v6-rollback.mjs --dry-run

# Quick health check
timeout 30s pnpm test:fast && \
timeout 15s pnpm lint && \
timeout 30s pnpm build && \
echo "✓ System healthy"
```

---

## Emergency Rollback

```bash
# One command - full rollback (with uncommitted changes)
node scripts/v6-rollback.mjs --force

# One command - full rollback (clean state)
node scripts/v6-rollback.mjs

# Rollback to specific snapshot
node scripts/v6-rollback.mjs --snapshot [snapshot-name]

# Rollback without post-validation (faster, riskier)
node scripts/v6-rollback.mjs --skip-tests --force
```

---

## Post-Rollback Validation

```bash
# Comprehensive validation
node scripts/validate-rollback.mjs

# Quick manual checks
cat package.json | grep version                    # Should be 5.x
pnpm ls --depth=0 | grep -E "zod|rdf-canonize"    # zod 3.x, rdf-canonize 2.x
timeout 30s pnpm test:fast                         # All tests pass
timeout 15s pnpm lint                              # 0 errors
cat rollback-report.json                           # success: true
```

---

## Health Checks

```bash
# Quick test
timeout 30s pnpm test:fast

# Full test suite
timeout 120s pnpm test

# Performance check
timeout 60s pnpm benchmark:compare

# Memory check
timeout 30s pnpm benchmark:memory

# All-in-one health check
timeout 30s pnpm test:fast && \
timeout 15s pnpm lint && \
timeout 30s pnpm build && \
echo "✓ All checks passed"
```

---

## Troubleshooting

```bash
# Check current version
cat package.json | grep version

# Check dependencies
pnpm ls --depth=0

# Check git state
git status
git log -1

# Check rollback logs
cat rollback.log | tail -50

# Check rollback report
cat rollback-report.json

# List snapshots
ls -lt .rollback-snapshots/

# View snapshot details
cat .rollback-snapshots/[snapshot-name]/manifest.json
```

---

## Decision Helpers

```bash
# Count test failures
timeout 60s pnpm test 2>&1 | tee test-output.log
grep -c "FAIL" test-output.log
# >50% failing → ROLLBACK NOW
# 25-50% → Assess, likely rollback
# <25% → Investigate, fix forward

# Check performance regression
timeout 60s pnpm benchmark:compare
# >50% slower → ROLLBACK NOW
# 20-50% slower → Investigate, consider rollback
# <20% slower → Monitor, fix forward

# Check error rate
grep -c "ERROR" /var/log/unrdf/*.log
# >20% → ROLLBACK NOW
# 10-20% → Prepare rollback
# <10% → Investigate
```

---

## Snapshot Management

```bash
# Create snapshot
node scripts/v6-snapshot.mjs [name]

# Create snapshot with auto-name
node scripts/v6-snapshot.mjs

# List snapshots
ls -lt .rollback-snapshots/

# View snapshot
cat .rollback-snapshots/[name]/README.md

# Clean old snapshots (keep last 10)
cd .rollback-snapshots/
ls -t | tail -n +11 | xargs rm -rf
cd ..
```

---

## Common Scenarios

### Scenario: Tests failing after deploy
```bash
# 1. Count failures
timeout 60s pnpm test 2>&1 | tee test-failures.log
FAILURES=$(grep -c "FAIL" test-failures.log)

# 2. Decide
if [ "$FAILURES" -gt 50 ]; then
  echo "Too many failures, rolling back..."
  node scripts/v6-rollback.mjs --force
else
  echo "Investigating failures..."
  grep "FAIL" test-failures.log
fi
```

### Scenario: Import errors
```bash
# IMMEDIATE ROLLBACK
node scripts/v6-rollback.mjs --force

# Post-rollback: Verify
node -e "import('@unrdf/core').then(() => console.log('✓ Imports work'))"
```

### Scenario: Performance degraded
```bash
# 1. Measure
timeout 60s pnpm benchmark:compare > perf-report.txt

# 2. Check regression
grep "regression" perf-report.txt

# 3. If >50%, rollback
node scripts/v6-rollback.mjs
```

### Scenario: Rollback script fails
```bash
# Manual rollback
git log --oneline -10
git checkout [last-good-commit]
git checkout [commit] -- package.json pnpm-lock.yaml
git checkout [commit] -- packages/*/package.json
pnpm install --frozen-lockfile
timeout 30s pnpm test:fast
```

---

## Monitoring Loop

```bash
# Monitor continuously for first hour
for i in {1..12}; do
  echo "Check $i/12 ($(date))"
  timeout 30s pnpm test:fast && echo "✓ Tests OK" || echo "✗ Tests FAILED"
  sleep 300  # 5 minutes
done

# If any fail, assess rollback
```

---

## Documentation Quick Links

- **Full Guide:** `docs/v6/ROLLBACK.md`
- **Quick Reference:** `docs/v6/ROLLBACK-QUICK-REFERENCE.md`
- **Deployment Checklist:** `docs/v6/DEPLOYMENT-CHECKLIST.md`
- **Summary:** `docs/v6/ROLLBACK-SUMMARY.md`

---

**Last Updated:** 2025-12-27
