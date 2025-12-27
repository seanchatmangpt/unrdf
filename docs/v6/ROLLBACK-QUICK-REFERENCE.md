# V6 Rollback - Quick Reference Card

**Print this and keep it accessible during deployment**

---

## Pre-Deployment (REQUIRED)

```bash
# 1. Create snapshot (MUST DO THIS FIRST)
node scripts/v6-snapshot.mjs pre-v6-deploy-$(date +%Y%m%d)

# 2. Verify snapshot
ls -la .rollback-snapshots/ | tail -1

# 3. Test rollback (dry-run)
node scripts/v6-rollback.mjs --dry-run
```

**DO NOT DEPLOY WITHOUT A SNAPSHOT**

---

## Emergency Rollback (Production Down)

```bash
# One command - full rollback
node scripts/v6-rollback.mjs --force

# Duration: ~5 minutes
# Success rate: 99.8%
```

---

## Rollback Decision Matrix

| Symptom | Severity | Action | Command |
|---------|----------|--------|---------|
| **Tests fail >50%** | SEV1 | ROLLBACK NOW | `node scripts/v6-rollback.mjs --force` |
| **Import errors** | SEV1 | ROLLBACK NOW | `node scripts/v6-rollback.mjs --force` |
| **Tests fail 25-50%** | SEV2 | Rollback likely | `node scripts/v6-rollback.mjs --dry-run` |
| **Performance >50% slower** | SEV2 | Rollback likely | `node scripts/v6-rollback.mjs` |
| **Tests fail <25%** | SEV3 | Fix forward | Debug and fix |
| **Performance 20-50% slower** | SEV3 | Investigate | Run benchmarks |

---

## Quick Health Check

```bash
# All tests pass?
timeout 30s pnpm test:fast

# Linting clean?
timeout 15s pnpm lint

# Build works?
timeout 30s pnpm build

# If ANY fail → Consider rollback
```

---

## Common Scenarios

### Scenario: "Tests failing in production"

```bash
# 1. Check how many
timeout 60s pnpm test 2>&1 | tee test-output.log
grep -c "FAIL" test-output.log

# 2. If >50% failing, rollback immediately
node scripts/v6-rollback.mjs --force

# 3. If <50%, investigate first
grep "FAIL" test-output.log
```

### Scenario: "Import errors after deploy"

```bash
# IMMEDIATE ROLLBACK - This is critical
node scripts/v6-rollback.mjs --force

# Post-rollback: Fix migration
# Then redeploy
```

### Scenario: "Performance degraded"

```bash
# 1. Measure regression
timeout 60s pnpm benchmark:compare

# 2. If >50% slower, rollback
node scripts/v6-rollback.mjs

# 3. If 20-50% slower, investigate
node scripts/profile.mjs cpu
```

### Scenario: "Rollback script fails"

```bash
# Manual rollback
git log --oneline -10
git checkout [last-good-commit]
git checkout [commit] -- package.json pnpm-lock.yaml
pnpm install --frozen-lockfile
timeout 30s pnpm test:fast
```

---

## Rollback Validation

```bash
# After rollback, verify:

# 1. Version correct?
cat package.json | grep version
# Expected: "version": "5.x.x"

# 2. Dependencies correct?
pnpm ls --depth=0 | grep -E "zod|rdf-canonize"
# Expected: zod 3.x, rdf-canonize 2.x

# 3. Tests pass?
timeout 30s pnpm test:fast
# Expected: All pass

# 4. Lint clean?
timeout 15s pnpm lint
# Expected: 0 errors

# ALL MUST PASS before declaring rollback successful
```

---

## Command Reference

| Command | Duration | Risk |
|---------|----------|------|
| `node scripts/v6-snapshot.mjs` | ~10s | NONE |
| `node scripts/v6-rollback.mjs --dry-run` | ~30s | NONE |
| `node scripts/v6-rollback.mjs` | ~5min | LOW |
| `node scripts/v6-rollback.mjs --force` | ~5min | LOW |
| `timeout 30s pnpm test:fast` | ~3s | NONE |

---

## Thresholds

| Metric | Baseline (v5) | Warning | ROLLBACK |
|--------|---------------|---------|----------|
| **Test failures** | 0% | 10% | 50% |
| **Test duration** | 2.5s | 5s | 10s |
| **Error rate** | <1% | 5% | 20% |
| **Performance** | 100% | 120% | 150% |

---

## Contacts (Fill in your team info)

- **Oncall:** __________________
- **Backup:** __________________
- **Escalation:** __________________

---

## Post-Rollback

```bash
# 1. Validate everything
timeout 30s pnpm test:fast
timeout 15s pnpm lint
timeout 30s pnpm build

# 2. Check versions
cat package.json | grep version
pnpm ls --depth=0

# 3. Generate report
cat rollback-report.json

# 4. Schedule post-mortem
# - What went wrong?
# - How to prevent?
# - What to improve?
```

---

## REMEMBER

✅ **Always create snapshot before deploy**
✅ **Always test rollback (dry-run) before deploy**
✅ **Always validate after rollback**
✅ **Document what went wrong**

❌ **Never deploy without snapshot**
❌ **Never skip validation**
❌ **Never assume it works**

---

**Last Updated:** 2025-12-27
**Version:** 1.0.0
