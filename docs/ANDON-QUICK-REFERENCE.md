# Andon Signals - Quick Reference Card

## TL;DR: What is Andon?

**Andon** = Visual management system that shows system health as RED/YELLOW/GREEN status indicators with automated alerting.

**Current Problem**: Teams manually check validation, CI/CD, security, and coverage status. Slow (15+ min), error-prone, reactive.

**Solution**: 53 automated signals that aggregate OTEL metrics, CI/CD status, and security scans into unified dashboard with real-time alerts.

**Benefit**: See health at a glance, get alerted on problems in <5 min, prevent bad deployments.

---

## 53 Signals at a Glance

```
VALIDATION (7)     ← OTEL feature scores                    🟢 87/100
CI/CD (9)          ← GitHub Actions job status              🟢 All pass
SECURITY (6)       ← Vulnerability scanning                 🟢 No critical
PERFORMANCE (6)    ← Latency trending                       🟢 No regression
DEPENDENCIES (1)   ← Package health                         🟢 Healthy
COVERAGE (5)       ← Test coverage percentage               🟢 87% statements
DEPLOYMENT (1)     ← Release gates (6 gates)                🟡 5/6 pass
────────────────────────────────────────────────────────────────────
TOTAL: 53 signals across 7 groups
```

---

## Signal Groups (30-second explanation each)

### 1. VALIDATION (7 signals) - Feature Health
**What**: OTEL-based scores (0-100) for 6 core features + overall
**Why**: Know if features are working
**From**: `/validation/run-all.mjs` (already exists!)
**Example**: `FEATURE_HEALTH_KNOWLEDGE_HOOKS_API = 90/100 🟢`

### 2. CI/CD (9 signals) - Pipeline Status
**What**: GitHub Actions job status for each build step
**Why**: Know if build is working
**From**: `.github/workflows/*.yml` (already tracked!)
**Example**: `CICD_BUILD_SECURITY = WARN (2 outdated deps)`

### 3. SECURITY (6 signals) - Vulnerability Status
**What**: Scan results (critical/high/medium counts)
**Why**: Know if security issues exist
**From**: `.github/workflows/security.yml` (already running!)
**Example**: `SECURITY_DEPENDENCIES = 0 critical, 0 high 🟢`

### 4. PERFORMANCE (6 signals) - Speed Tracking
**What**: Latency vs baseline (parse, query, validate, hooks, lockchain, browser)
**Why**: Catch regressions early
**From**: OTEL latency metrics (extracted from spans)
**Example**: `PERF_REGRESSION_QUERY = 18ms baseline, 18ms actual 🟢`

### 5. DEPENDENCY (1 signal) - Package Health
**What**: Outdated, vulnerable, deprecated, license issues
**Why**: Know dependency status
**From**: `pnpm audit --json` (already available!)
**Example**: `DEPENDENCY_HEALTH = 2 outdated, 0 vulns 🟢`

### 6. COVERAGE (5 signals) - Test Quality
**What**: Statements, branches, functions, lines % + regression
**Why**: Prevent coverage from dropping
**From**: vitest coverage reports (already generated!)
**Example**: `COVERAGE_STATEMENTS = 87% (threshold 80%) 🟢`

### 7. DEPLOYMENT (1 signal) - Release Readiness
**What**: 6 gates: build, tests, security, performance, docs, changelog
**Why**: Prevent broken deployments
**From**: Aggregate of all signals above
**Example**: `DEPLOYMENT_READINESS = 5/6 gates 🟡 (changelog needed)`

---

## States & Colors

```
🟢 GREEN  = Healthy, working, no action needed
🟡 YELLOW = Warning, needs attention, investigate
🔴 RED    = Critical, broken, immediate action required

Examples:
- FEATURE_HEALTH >= 80        → 🟢
- FEATURE_HEALTH 60-79        → 🟡
- FEATURE_HEALTH < 60         → 🔴

- CICD_BUILD all pass         → 🟢
- CICD_BUILD has warnings     → 🟡
- CICD_BUILD failed           → 🔴

- SECURITY 0 high/critical    → 🟢
- SECURITY >0 medium          → 🟡
- SECURITY >0 high/critical   → 🔴
```

---

## Alert Channels

| Severity | Channel | Timer | Action |
|----------|---------|-------|--------|
| 🔴 RED | Email + Slack + PagerDuty | 5 min | Fix immediately |
| 🟡 YELLOW | Slack + Email | 24 hr | Investigate + plan |
| 🟢 GREEN | Dashboard only | — | None needed |

---

## Data Sources (All Existing!)

| Signal | Source | Status |
|--------|--------|--------|
| Validation (7) | `/validation/run-all.mjs` OTEL output | ✓ Exists |
| CI/CD (9) | `.github/workflows/*.yml` job results | ✓ Exists |
| Security (6) | `.github/workflows/security.yml` outputs | ✓ Exists |
| Performance (6) | OTEL latency metrics (from validation) | ✓ Extractable |
| Dependencies (1) | `pnpm audit --json` | ✓ Exists |
| Coverage (5) | vitest coverage-final.json | ✓ Exists |
| Deployment (1) | Aggregate of above | ✗ New (orchestration) |

**Key Point**: 6/7 signal groups use data that ALREADY EXISTS in UNRDF!

---

## Implementation Timeline

```
Week 1-2: Phase 1 - Core Signals (35 signals)
├─ Validation (7)
├─ CI/CD (9)
└─ Deployment (1 with 6 gates)
→ Terminal dashboard working
→ 10 hours effort

Week 3-4: Phase 2 - Expanded Coverage (13 signals)
├─ Security (6)
├─ Performance (6)
└─ Dependencies (1)
→ Alerting setup
→ 20 hours effort

Week 5-6: Phase 3 - Polish (5 signals)
├─ Coverage (5)
├─ Tests & docs
└─ Production ready
→ 10 hours effort

Week 7-8: Phase 4 - Alerting (Optional)
├─ Slack/Email/PagerDuty
├─ GitHub integration
└─ Advanced monitoring
→ 15 hours effort (post-MVP)

TOTAL MVP: 5-6 weeks, 40-50 hours, 53 signals production-ready
```

---

## Example: Before vs After

### Before Andon
```
Team member asks: "Is everything working?"

Manual process:
1. Run pnpm test                      (2 min)
2. Check GitHub Actions               (1 min)
3. Check security.yml results         (2 min)
4. Review coverage report              (1 min)
5. Manually aggregate findings         (3 min)

Total time: 9+ minutes
Result: Partial visibility, reactive
```

### With Andon
```
Team member runs: pnpm run andon:status

Output (instant):
🟢 SYSTEM HEALTH: GREEN (87/100)
  🟢 Validation: All features working
  🟢 CI/CD: All pipelines passing
  🟢 Security: No issues
  🟢 Performance: No regressions
  ✓ Deployment Ready: YES

Total time: <1 second
Result: Complete visibility, proactive
```

---

## Critical Signals (Block Deployment)

These 23 signals can block deployment:

```
FEATURE_HEALTH (7)        ← Tests must pass
CICD_BUILD (9)            ← Build must succeed
SECURITY (6)              ← No critical vulns
DEPLOYMENT_READINESS (1)  ← All gates must pass
────────────────────────
23 critical signals

If ANY of these are RED:
→ Deployment blocked ✓
→ Team notified immediately
→ Prevents bad releases
```

---

## Dashboard Example

```
$ pnpm run andon:status

═══════════════════════════════════════════════════════════
  UNRDF v4.0 ANDON STATUS
═══════════════════════════════════════════════════════════

🟢 SYSTEM HEALTH: GREEN (87/100)
📊 53 signals monitored, all systems operational

VALIDATION (Overall: 87/100)
  🟢 Knowledge Engine Core ..... 85/100
  🟢 Knowledge Hooks API ....... 90/100
  🟢 Policy Packs ............. 88/100
  🟢 Lockchain Integrity ....... 82/100
  🟡 Transaction Manager ....... 75/100 ⚠️ YELLOW
  🟢 Browser Compatibility ..... 91/100

CI/CD PIPELINE
  🟢 TypeScript Gate ✓  🟢 Lint ✓  🟢 Test ✓
  🟡 Security ⚠️  🟢 Build ✓  🟢 Docs ✓
  🟢 Benchmark ✓  🟢 Integration ✓  🟢 Release ✓

DEPLOYMENT READINESS (Release v4.0.1)
  ✓ Build ✓ Tests ✓ Security ✓ Performance
  ✓ Docs ✗ Changelog needed
  Status: 🟡 YELLOW (5/6 gates)

═══════════════════════════════════════════════════════════
Last Update: 2024-11-21 15:30:45
```

---

## File Locations

All Andon design documents are in `/docs/`:

```
docs/
├── ANDON-QUICK-REFERENCE.md           ← You are here
├── ANDON-SIGNALS-INDEX.md             ← Document guide
├── ANDON-SIGNALS-SUMMARY.md           ← Executive overview
├── ANDON-SIGNALS-DESIGN.md            ← Complete technical spec
├── ANDON-SIGNALS-IMPLEMENTATION-GUIDE.md ← Step-by-step guide
└── ANDON-SIGNALS-MATRIX.md            ← Signal reference tables
```

---

## Where to Start

**I'm a...**

- **Manager** → Read `ANDON-SIGNALS-SUMMARY.md` (15 min)
- **Architect** → Read all 4 docs (2 hours)
- **Developer** → Read `ANDON-SIGNALS-IMPLEMENTATION-GUIDE.md` (30 min)
- **DevOps** → Read `ANDON-SIGNALS-SUMMARY.md` + `ANDON-SIGNALS-MATRIX.md` (30 min)
- **Just need quick overview** → Read THIS FILE (5 min)

---

## Key Numbers

| Metric | Value |
|--------|-------|
| Total signals | 53 |
| Critical signals | 23 |
| Implementation effort (MVP) | 40-50 hours |
| Timeline (MVP) | 5-6 weeks |
| Data sources already in UNRDF | 6/7 |
| New module files to create | ~15 |
| Documentation pages | 28 |
| Documentation words | 7,500+ |
| Reference tables | 41+ |

---

## Success Criteria

Andon signals are "done" when:

- [ ] All 53 signals implemented
- [ ] Terminal dashboard shows live status
- [ ] Deployment gates prevent bad releases
- [ ] Slack alerts notify team of problems
- [ ] Team uses Andon daily
- [ ] MTTR improves by 50%
- [ ] Zero undetected critical failures

---

## One-Sentence Summary

**Andon signals turn UNRDF's existing OTEL metrics, CI/CD status, and security scans into a unified RED/YELLOW/GREEN visual dashboard with automated alerting, enabling teams to detect and respond to problems in minutes instead of hours.**

---

## Next Action

1. **Read** this quick reference (done!)
2. **Share** with team
3. **Read** `ANDON-SIGNALS-SUMMARY.md` together
4. **Decide** on implementation timeline
5. **Begin** Phase 1 (validation + CI/CD + deployment signals)
6. **Deploy** working dashboard in 2 weeks

---

*UNRDF Andon Signals Design - Quick Reference Card*
*December 2025*
*2,763 lines of documentation across 5 comprehensive documents*
