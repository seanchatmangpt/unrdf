# Quickstart: Package Maturity Assessment

**Feature**: 006-maturity-matrix
**Date**: 2025-12-20
**Time to Complete**: 5 minutes

---

## Overview

The UNRDF maturity CLI helps you:

1. **Assess** individual package maturity levels (L1-L5)
2. **Generate** comprehensive reports across all 21 packages
3. **Explore** synergy categories that enable emergent capabilities

---

## Prerequisites

```bash
# Ensure you're in the UNRDF monorepo root
cd /path/to/unrdf

# Install dependencies
pnpm install

# Build packages (required for CLI)
pnpm build
```

---

## 1. Assess a Single Package (30 seconds)

```bash
# Assess the core package
unrdf maturity assess core

# Output:
# ┌──────────────────────────────────────────────────┐
# │ Package: core                                    │
# │ Level: L4_Stable (Production-Ready)              │
# │ Score: 87.5/100                                  │
# ├──────────────────────────────────────────────────┤
# │ Coverage: 92.3% lines | 85.1% branches           │
# │ API: stable | Docs: excellent | Tests: e2e       │
# │ Security: audit | Performance: benchmarked       │
# └──────────────────────────────────────────────────┘
```

### With Detailed Breakdown

```bash
unrdf maturity assess core --verbose

# Shows individual criterion scores:
# - Coverage Score: 92/100 (weight: 30%)
# - API Stability: 90/100 (weight: 20%)
# - Documentation: 85/100 (weight: 15%)
# - Test Maturity: 80/100 (weight: 15%)
# - Security: 85/100 (weight: 10%)
# - Performance: 90/100 (weight: 10%)
```

---

## 2. Generate Full Report (1 minute)

```bash
# Generate markdown report for all 21 packages
unrdf maturity report

# Output to file
unrdf maturity report --format html -o docs/maturity-report.html
```

### Sample Report Summary

```markdown
# UNRDF Maturity Report

**Generated**: 2025-12-20T14:30:00Z
**Packages**: 21
**Average Score**: 72.5/100

## Level Distribution

| Level       | Count | Packages                   |
| ----------- | ----- | -------------------------- |
| L5 (LTS)    | 0     | -                          |
| L4 (Stable) | 3     | core, validation, hooks    |
| L3 (RC)     | 8     | streaming, federation, ... |
| L2 (Beta)   | 7     | kgn, atomvm, ...           |
| L1 (Alpha)  | 3     | browser, nextra, docs      |
```

---

## 3. Explore Synergy Categories (30 seconds)

```bash
# List all 11 synergy categories
unrdf maturity synergy

# Output:
# A: Real-Time Knowledge Graph (core, streaming, federation, domain)
# B: Composable Queries (core, composables, engine-gateway)
# C: Reactive Knowledge Pipelines (core, hooks, streaming, domain)
# ...
```

### Dive into a Specific Synergy

```bash
unrdf maturity synergy A --show-use-cases

# Synergy A: Real-Time Knowledge Graph
# ─────────────────────────────────────
# Packages: core, streaming, federation, domain
#
# Capability: Live, synchronized knowledge graphs across distributed systems
#
# Emergent Value: Real-time collaborative knowledge graphs
#                 (impossible with individual packages)
#
# Use Cases:
# - Multi-user collaborative ontology editing
# - Live federated SPARQL across 10+ endpoints
# - Real-time knowledge graph dashboard
```

---

## 4. Compare Packages (30 seconds)

```bash
# Compare core, streaming, and federation
unrdf maturity compare core streaming federation

# Output:
# ┌─────────────┬───────────┬───────────┬────────────┐
# │ Criterion   │ core      │ streaming │ federation │
# ├─────────────┼───────────┼───────────┼────────────┤
# │ Level       │ L4_Stable │ L3_RC     │ L3_RC      │
# │ Coverage    │ 92.3%     │ 78.5%     │ 75.2%      │
# │ API         │ stable    │ stable    │ stabilizing│
# │ Docs        │ excellent │ good      │ good       │
# └─────────────┴───────────┴───────────┴────────────┘
# Winner: core (Score: 87.5)
```

---

## 5. Output Formats

### JSON (for automation)

```bash
unrdf maturity assess core --format json

# {"package":"core","level":"L4_Stable","score":87.5,...}
```

### Turtle/RDF (for knowledge graphs)

```bash
unrdf maturity assess core --format ttl

# @prefix mat: <https://unrdf.org/ontology/maturity#> .
# unrdf:core mat:hasMaturityLevel mat:L4_Stable ;
#            mat:maturityScore 87.5 .
```

### Markdown (for documentation)

```bash
unrdf maturity report --format md -o MATURITY.md
```

---

## 6. CI/CD Integration

### GitHub Actions Example

```yaml
# .github/workflows/maturity-check.yml
name: Maturity Assessment

on:
  push:
    branches: [main]
  schedule:
    - cron: '0 0 * * 0' # Weekly

jobs:
  assess:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: pnpm/action-setup@v2
      - run: pnpm install
      - run: pnpm build

      # Run full assessment
      - run: unrdf maturity report --format json -o maturity.json

      # Fail if any package below L2
      - run: |
          L1_COUNT=$(jq '.summary.levelDistribution.L1_Alpha' maturity.json)
          if [ "$L1_COUNT" -gt 0 ]; then
            echo "Error: $L1_COUNT packages at L1_Alpha level"
            exit 1
          fi
```

---

## Maturity Level Reference

| Level | Name   | Coverage | API         | Criteria                             |
| ----- | ------ | -------- | ----------- | ------------------------------------ |
| L1    | Alpha  | <50%     | Unstable    | Experimental, breaking changes       |
| L2    | Beta   | 50-70%   | Stabilizing | Core features, basic docs            |
| L3    | RC     | 70-85%   | Stable      | Feature-complete, comprehensive docs |
| L4    | Stable | 85-95%   | Very Stable | Production-ready, semver-strict      |
| L5    | LTS    | 95%+     | Frozen      | Enterprise, 24-month support         |

---

## Score Calculation

Maturity score is a weighted sum:

| Criterion     | Weight | Scoring                                    |
| ------------- | ------ | ------------------------------------------ |
| Test Coverage | 30%    | % line coverage                            |
| API Stability | 20%    | unstable=20, stable=80, frozen=100         |
| Documentation | 15%    | minimal=20, excellent=90, professional=100 |
| Test Maturity | 15%    | unit_only=40, comprehensive=100            |
| Security      | 10%    | not_reviewed=0, hardened=100               |
| Performance   | 10%    | untested=20, sla_bound=100                 |

**Example**: core package

- Coverage: 92.3 × 0.30 = 27.7
- API: 80 × 0.20 = 16.0
- Docs: 90 × 0.15 = 13.5
- Tests: 80 × 0.15 = 12.0
- Security: 85 × 0.10 = 8.5
- Perf: 90 × 0.10 = 9.0
- **Total: 86.7 → L4_Stable**

---

## Troubleshooting

### "Package not found"

```bash
# List all available packages
unrdf maturity assess

# Verify package exists
ls packages/
```

### "Coverage data stale"

```bash
# Refresh coverage by running tests
unrdf maturity assess core --refresh-coverage
```

### "Assessment takes too long"

```bash
# Skip test re-run, use cached coverage
unrdf maturity assess core  # Uses existing coverage-summary.json
```

---

## Next Steps

1. **Run your first assessment**: `unrdf maturity assess core`
2. **Generate full report**: `unrdf maturity report`
3. **Explore synergies**: `unrdf maturity synergy A`
4. **Set up CI/CD**: Add maturity checks to your pipeline

---

**Status**: Quickstart Complete
**Full Documentation**: `docs/maturity-matrix.md`
