# Code Quality Standards - UNRDF

**Automated enforcement, not manual review. 80/20 focus on high-impact quality gates.**

## Philosophy

Quality is **automated and enforced**, not requested:
- **Pre-commit hooks** block bad commits (not PRs)
- **CI/CD gates** block bad merges (not deployments)
- **IDE automation** prevents issues (not detects them)

**Principle**: If a human must check it, we've already failed.

---

## Quality Metrics (80/20 Focus)

### 1. JSDoc Coverage: 100% Target

**Why**: Type hints are our contract. 100% coverage = zero ambiguity.

**Automated checks**:
- Pre-commit: Changed files only
- CI: Full workspace on PRs
- IDE: Live warnings on missing JSDoc

**Enforcement**:
```javascript
/**
 * Creates an RDF quad from subject, predicate, object, and optional graph
 * @param {RDF.Term} subject - The subject term
 * @param {RDF.Term} predicate - The predicate term
 * @param {RDF.Term} object - The object term
 * @param {RDF.Term} [graph] - Optional graph term
 * @returns {RDF.Quad} The created quad
 */
export function createQuad(subject, predicate, object, graph) {
  return dataFactory.quad(subject, predicate, object, graph);
}
```

**Measured by**: `node scripts/quality-report.mjs`

---

### 2. Test Coverage: 80% Minimum

**Why**: 80% = critical paths covered. Pareto principle applies.

**What counts**:
- Lines: 80%+
- Branches: 75%+
- Functions: 85%+
- Statements: 80%+

**What doesn't**:
- Type definitions
- Configuration files
- Example code

**Automated checks**:
- Pre-commit: Changed files only (vitest --changed)
- CI: Full coverage with --coverage flag
- PR gate: Blocks if coverage drops >5%

**Enforcement**: GitHub Actions fails PR if threshold not met.

---

### 3. Linting Violations: 0 Tolerance

**Why**: 400+ ESLint rules exist. All must pass.

**Automated checks**:
- Pre-commit: `eslint --fix` on changed files
- CI: `eslint --max-warnings=0` on full codebase
- IDE: Auto-fix on save

**Zero tolerance for**:
- `no-undef` (undefined variables)
- `no-unused-vars` (except `_` prefix)
- JSDoc violations on public APIs
- `no-debugger` (must use breakpoints)

**Exceptions**: Test files can have `no-undef` disabled (helper functions).

---

### 4. Cyclomatic Complexity: ≤10 Average

**Why**: Complexity >10 = unmaintainable. Refactor or split.

**Measured**: Decision points (if/for/while/case/ternary/logical operators).

**Automated checks**:
- Quality report: Flags files with complexity >15
- CI warning (not blocking): Complexity trending up

**Action items**:
- Complexity 10-15: Refactor recommended
- Complexity 15+: Refactor required
- Complexity 20+: PR blocked

**Tools**: `node scripts/quality-report.mjs` provides per-file analysis.

---

## Automated Tools

### Pre-commit Hooks (Husky + lint-staged)

**Runs on every commit**:
```bash
# .husky/pre-commit
pnpm lint-staged
```

**What gets checked** (changed files only):
1. ESLint with auto-fix
2. Prettier formatting
3. Vitest tests for changed files
4. Type checking (TypeScript JSDoc)

**How to bypass** (NEVER do this without reason):
```bash
git commit --no-verify -m "Emergency fix"
```

**Setup**:
```bash
node scripts/setup-quality.mjs
```

---

### Quality Dashboard

**Command**:
```bash
# Full workspace report
node scripts/quality-report.mjs

# Single package
node scripts/quality-report.mjs --package=core

# JSON output for CI
node scripts/quality-report.mjs --json
```

**Output**:
```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  📊 UNRDF Code Quality Dashboard
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

🎯 Workspace Overview
   Overall Quality Score: 87/100 🟢
   Packages Analyzed: 22
   Avg JSDoc Coverage: 94% (target: 100%)
   Avg Test Coverage: 82% (target: 80%)
   Total Lint Issues: 3 (target: 0)
   Avg Complexity: 8 (target: ≤10)
```

**Scoring**:
- JSDoc: 20% weight
- Test coverage: 40% weight
- Lint violations: 20% weight
- Complexity: 20% weight

**Exit codes**:
- 0: Score ≥70
- 1: Score <70

---

### GitHub Actions Quality Gates

**Triggered on**:
- Pull requests to main/develop
- Pushes to main/develop

**Gates enforced**:
1. ✅ All tests pass (100% required)
2. ✅ Linting: 0 violations
3. ✅ Formatting: Consistent with Prettier
4. ✅ Coverage: ≥80% overall
5. ✅ Quality score: ≥70/100
6. ⚠️ Type check: Warning only (for now)
7. ⚠️ Regression check: Coverage drop <5%

**Workflow**: `.github/workflows/quality.yml`

**PR comment**: Automatic quality summary posted to PR.

---

### VSCode Integration

**Settings** (`.vscode/settings.json`):
- Format on save: ✅
- ESLint auto-fix on save: ✅
- Organize imports on save: ✅
- Vitest integration: ✅

**Extensions** (`.vscode/extensions.json`):
- ESLint (required)
- Prettier (required)
- Vitest Explorer (recommended)
- GitLens (recommended)
- SPARQL language support (recommended)

**First-time setup**:
1. Open workspace in VSCode
2. Install recommended extensions (popup will appear)
3. Settings auto-apply on save

---

## Quality Thresholds

| Metric | Minimum | Target | Excellent |
|--------|---------|--------|-----------|
| **Quality Score** | 70 | 85 | 95+ |
| **JSDoc Coverage** | 80% | 95% | 100% |
| **Test Coverage** | 80% | 85% | 90%+ |
| **Lint Violations** | 0 | 0 | 0 |
| **Avg Complexity** | ≤12 | ≤10 | ≤8 |
| **Max Complexity** | ≤20 | ≤15 | ≤12 |

**PR requirements**:
- Quality Score: ≥70 (blocking)
- Coverage: ≥80% (blocking)
- Lint: 0 violations (blocking)
- Coverage drop: <5% vs main (blocking)

---

## Code Review Checklist

**Automated (don't check manually)**:
- ✅ Linting passes
- ✅ Tests pass
- ✅ Coverage threshold met
- ✅ Formatting consistent
- ✅ No type errors

**Manual review focus**:
- 🧠 Architecture and design
- 🧠 Algorithm correctness
- 🧠 Security considerations
- 🧠 Performance implications
- 🧠 API design and DX

**80/20 Rule**: Spend time on what machines can't check.

---

## Setup for New Contributors

```bash
# Clone repo
git clone https://github.com/unrdf/unrdf.git
cd unrdf

# Install dependencies
pnpm install

# Setup quality tools (one-time)
node scripts/setup-quality.mjs

# Verify setup
pnpm lint
pnpm test:fast
node scripts/quality-report.mjs
```

**Expected output**:
```
✅ Husky hooks installed
✅ Git hooks configured
✅ VSCode settings applied
✅ Quality tools ready

Run your first quality check:
  node scripts/quality-report.mjs
```

---

## Troubleshooting

### Pre-commit hook fails

**Problem**: `pnpm lint-staged` fails on commit.

**Solution**:
```bash
# Check what's failing
pnpm lint-staged --debug

# Fix manually
pnpm lint:fix
pnpm format
pnpm test:fast
```

### Coverage below threshold

**Problem**: Tests pass but coverage <80%.

**Solution**:
```bash
# Generate coverage report
pnpm test:coverage

# Open HTML report
open packages/[package]/coverage/index.html

# Focus on untested critical paths
```

### Quality score <70

**Problem**: Overall quality score too low.

**Solution**:
```bash
# Get detailed report
node scripts/quality-report.mjs

# Identify weak areas
# Fix in priority order:
# 1. Lint violations (quick wins)
# 2. Missing JSDoc (high value)
# 3. Test coverage (time-consuming)
# 4. Complexity refactoring (strategic)
```

### Husky not running

**Problem**: Pre-commit hook not executing.

**Solution**:
```bash
# Reinstall Husky
pnpm exec husky install

# Make hooks executable
chmod +x .husky/*

# Test manually
.husky/pre-commit
```

---

## CI/CD Integration

### Required checks before merge

**GitHub branch protection** (main/develop):
1. Quality Gate workflow passes
2. All tests pass (Node 18, 20)
3. Coverage ≥80%
4. Quality score ≥70
5. No new lint violations
6. PR approved by maintainer

**Auto-merge criteria**:
- All checks pass
- 2+ approvals
- No unresolved comments
- Quality score ≥85

---

## Metrics to Track

**Weekly** (automated report):
- Workspace quality score trend
- Coverage trend per package
- Lint violations by type
- Complexity hotspots

**Monthly** (manual review):
- Technical debt accumulation
- Refactoring impact
- Quality improvement initiatives
- Test suite performance

**Quarterly** (strategic):
- Quality tool ROI
- Developer productivity impact
- Bug correlation with quality scores
- Quality standards updates

---

## Quality Standards Evolution

**This document is versioned**:
- v1.0 (Dec 2025): Initial automated quality gates
- Future: Stricter thresholds as codebase matures

**Feedback**: Open issues for quality standard improvements.

**Goal**: Achieve 95+ workspace quality score with <5min CI time.

---

## Further Reading

- [ESLint Configuration](eslint.config.mjs) - 400+ rules
- [Prettier Configuration](.prettierrc) - Formatting rules
- [Vitest Configuration](vitest.config.mjs) - Test runner setup
- [GitHub Actions Workflows](.github/workflows/) - CI/CD automation
- [CLAUDE.md](CLAUDE.md) - Development workflow and practices
