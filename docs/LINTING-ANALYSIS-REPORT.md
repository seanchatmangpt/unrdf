# UNRDF Monorepo Linting Configuration Analysis

**Report Generated**: 2025-12-20
**Analyzed Packages**: 19 packages
**Current State**: ESLint + Prettier (JavaScript/MJS)
**Target State**: Ruff with 400+ rules (per CLAUDE.md requirements)

---

## Executive Summary

### Current Linting Stack
- **Primary Tool**: ESLint 9.39.1 (Flat Config)
- **Formatter**: Prettier 3.7.4
- **Configuration Files**: 9 total (7 unique configs)
- **Packages with Linting**: 13/19 (68%)
- **Packages without Linting**: 6/19 (32%)

### Critical Finding
**UNRDF is a JavaScript/MJS monorepo, NOT a Python project.** Ruff is a Python linting tool and cannot lint JavaScript/TypeScript code. The CLAUDE.md instructions appear to reference Python project conventions but the codebase is entirely Node.js/JavaScript.

---

## 1. ESLint Configuration Files

### Root Configuration (Primary)
**File**: `/Users/sac/unrdf/eslint.config.mjs`
**Lines**: 200
**Format**: ESLint Flat Config (Modern)
**Scope**: Monorepo-wide standard

**Configuration Details**:
- **Plugins**: `eslint-plugin-jsdoc`, `eslint-config-prettier`
- **Rules**: ~15 custom rules + JSDoc enforcement
- **Environments**:
  - Node.js files (default)
  - Browser files (React, Workers, DOM APIs)
  - Test files (Vitest globals)
  - Example files (lenient mode)
- **Globals**: Vitest test globals, Node.js, Browser APIs

**Key Rules**:
```javascript
'no-unused-vars': 'warn' (with _ prefix exception)
'no-console': 'off' (allowed)
'no-debugger': 'warn'
'no-undef': 'error'
'jsdoc/require-jsdoc': 'warn' (public APIs only)
```

### Package-Specific Configs

#### 1. `/Users/sac/unrdf/packages/kgn/.eslintrc.determinism.js`
**Lines**: 280
**Format**: Legacy ESLintRC
**Purpose**: Enforce deterministic template generation

**Unique Custom Rules** (Pseudo-code, not implemented):
- `no-new-date` - Ban `new Date()` without literals
- `no-date-now` - Ban `Date.now()`
- `no-math-random` - Ban `Math.random()`
- `no-process-env-in-templates` - Ban `process.env` in templates
- `require-object-key-sort` - Require sorted object iteration
- `require-readdir-sort` - Require sorted directory reads
- `no-dynamic-imports` - Ban runtime-conditional imports

**Strictness**: CRITICAL - Zero tolerance for nondeterminism
**Status**: ⚠️ Configuration exists but custom rules NOT implemented as ESLint plugins

#### 2. `/Users/sac/unrdf/packages/docs/eslint.config.mjs`
**Lines**: 5
**Format**: Nuxt ESLint integration
**Content**: `import withNuxt from './.nuxt/eslint.config.mjs'`

#### 3. `/Users/sac/unrdf/playground/hooks-showcase/.eslintrc.json`
**Format**: Next.js default
**Content**: `{ "extends": "next/core-web-vitals" }`

#### 4. `/Users/sac/unrdf/packages/nextra/.eslintrc.json`
**Format**: Next.js default
**Content**: `{ "extends": "next/core-web-vitals" }`

#### 5. `/Users/sac/unrdf/packages/kgc-4d/playground/.eslintrc.json`
**Format**: Next.js default
**Content**: `{ "extends": "next/core-web-vitals" }`

---

## 2. Prettier Configuration

**File**: `/Users/sac/unrdf/.prettierrc`
**Lines**: 12
**Format**: JSON

**Settings**:
```json
{
  "semi": true,
  "singleQuote": true,
  "tabWidth": 2,
  "useTabs": false,
  "trailingComma": "es5",
  "printWidth": 100,
  "arrowParens": "avoid",
  "endOfLine": "lf",
  "insertPragma": false,
  "requirePragma": false
}
```

**Scope**: Monorepo-wide (no overrides)

---

## 3. Ruff Configuration

**Search Results**: NONE
**Files Searched**: `*.toml`, `*.json`, `*.mjs`, `*.md`
**Conclusion**: **Ruff is NOT used anywhere in the codebase**

**Critical Issue**: Ruff is a Python-only linter. UNRDF uses:
- JavaScript (`.mjs`, `.js`)
- TypeScript (type definitions only via JSDoc)
- NO Python files in source code

---

## 4. Lint Scripts by Package

### Packages WITH Linting (13/19 = 68%)

| Package | lint | lint:fix | format | format:check |
|---------|------|----------|--------|--------------|
| `@unrdf/core` | ✅ eslint --max-warnings=0 | ✅ eslint --fix | ✅ prettier --write | ✅ prettier --check |
| `@unrdf/hooks` | ✅ eslint --max-warnings=0 | ✅ eslint --fix | ✅ prettier --write | ✅ prettier --check |
| `@unrdf/federation` | ✅ eslint --max-warnings=0 | ✅ eslint --fix | ✅ prettier --write | ✅ prettier --check |
| `@unrdf/streaming` | ✅ eslint --max-warnings=0 | ✅ eslint --fix | ✅ prettier --write | ✅ prettier --check |
| `@unrdf/composables` | ✅ eslint --max-warnings=0 | ✅ eslint --fix | ✅ prettier --write | ✅ prettier --check |
| `@unrdf/dark-matter` | ✅ eslint --max-warnings=0 | ✅ eslint --fix | ✅ prettier --write | ✅ prettier --check |
| `@unrdf/oxigraph` | ✅ eslint --max-warnings=0 | ✅ eslint --fix | ✅ prettier --write | ✅ prettier --check |
| `@unrdf/cli` | ✅ eslint --max-warnings=0 | ✅ eslint --fix | ✅ prettier --write | ✅ prettier --check |
| `@unrdf/project-engine` | ✅ eslint --max-warnings=0 | ✅ eslint --fix | ✅ prettier --write | ✅ prettier --check |
| `@unrdf/knowledge-engine` | ✅ eslint --max-warnings=0 | ✅ eslint --fix | ✅ prettier --write | ✅ prettier --check |
| `@unrdf/kgn` | ✅ eslint --ext .js,.mjs | ✅ eslint --fix | ❌ MISSING | ❌ MISSING |
| `@unrdf/docs` | ✅ eslint . | ❌ MISSING | ❌ MISSING | ❌ MISSING |
| `@unrdf/nextra` | ⚠️ echo "skipped" (Next.js 16 bug) | ⚠️ echo "skipped" | ❌ MISSING | ❌ MISSING |

### Packages WITHOUT Linting (6/19 = 32%)

| Package | Status | Note |
|---------|--------|------|
| `@unrdf/test-utils` | ❌ NO lint scripts | Utility package |
| `@unrdf/atomvm` | ❌ NO lint scripts | Browser/WASM package |
| `@unrdf/engine-gateway` | ❌ NO lint scripts | Gateway package |
| `@unrdf/kgc-4d` | ❌ NO lint scripts | 4D event sourcing |
| `@unrdf/domain` | ❌ NO lint scripts | Domain models |
| `@unrdf/validation` | ❌ NO lint scripts | OTEL validation framework |

---

## 5. Configuration Inconsistencies

### Critical Issues

#### A. Missing Lint Scripts (32% of packages)
**Impact**: 6 packages have NO linting enforcement
**Risk**: Code quality degradation, inconsistent style
**Affected**: `test-utils`, `atomvm`, `engine-gateway`, `kgc-4d`, `domain`, `validation`

#### B. Inconsistent Script Patterns
- **10 packages**: Full suite (`lint`, `lint:fix`, `format`, `format:check`)
- **1 package** (`kgn`): ESLint only, NO Prettier
- **1 package** (`docs`): ESLint only, NO fix/format
- **1 package** (`nextra`): Disabled due to Next.js 16 bug
- **6 packages**: NO scripts at all

#### C. Mixed ESLint Configurations
- **Root**: Modern Flat Config (200 lines)
- **KGN**: Legacy `.eslintrc.js` with custom determinism rules (280 lines, NOT implemented)
- **Docs/Nextra/Playgrounds**: Next.js presets (minimal)
- **Inconsistency**: Some packages reference root, some have local configs

#### D. Missing Format Scripts
3 packages lack Prettier scripts:
- `@unrdf/kgn` - Uses ESLint only
- `@unrdf/docs` - Nuxt-specific linting
- `@unrdf/nextra` - Disabled entirely

### Non-Critical Inconsistencies

#### E. ESLint Max Warnings Flag
- **10 packages**: `--max-warnings=0` (strict)
- **1 package** (`kgn`): No max warnings flag
- **1 package** (`docs`): Simple `eslint .`

#### F. File Targeting
- **10 packages**: `src/ test/` (explicit)
- **1 package** (`kgn`): `src/` only with `--ext .js,.mjs`
- **1 package** (`docs`): `.` (current directory)

---

## 6. Ruff Migration Analysis

### Critical Blocker: Language Incompatibility

**CANNOT migrate to Ruff** because:
1. **Ruff is Python-only** - Lints Python files (`.py`, `.pyi`)
2. **UNRDF is JavaScript-only** - All source is `.mjs`/`.js`
3. **No Python source code** - Zero `.py` files in `packages/*/src/`

### CLAUDE.md Requirements vs Reality

**CLAUDE.md states**:
```
Linting: Ruff with ALL 400+ rules enabled - NO EXCEPTIONS
Code quality: 400+ Ruff rules enforced
```

**Actual Codebase**:
```bash
$ find packages/*/src -name "*.py" | wc -l
0

$ find packages/*/src -name "*.mjs" -o -name "*.js" | wc -l
247
```

**Conclusion**: The CLAUDE.md instructions appear to be copied from a Python project template (references `uv`, `ruff`, `mypy`, `pytest`, `pyproject.toml`) but UNRDF is a **Node.js/JavaScript monorepo**.

---

## 7. Correct JavaScript Linting Strategy

### Current Stack (Mostly Correct)
✅ **ESLint 9** (Modern Flat Config)
✅ **Prettier 3.7** (Formatting)
✅ **JSDoc** (Type hints via comments)
❌ **Inconsistent application** (6 packages missing)

### Recommended Unified Strategy

#### Option A: Enhanced ESLint (Strict)
**Equivalent to "400+ Ruff rules" for JavaScript**:

```javascript
// eslint.config.mjs
import js from '@eslint/js';
import jsdoc from 'eslint-plugin-jsdoc';
import unicorn from 'eslint-plugin-unicorn';
import sonarjs from 'eslint-plugin-sonarjs';
import security from 'eslint-plugin-security';
import prettier from 'eslint-config-prettier';

export default [
  js.configs.recommended,        // ~80 rules
  unicorn.configs.recommended,   // ~100 rules (best practices)
  sonarjs.configs.recommended,   // ~60 rules (code smells)
  security.configs.recommended,  // ~30 rules (security)
  jsdoc.configs.recommended,     // ~50 rules (documentation)
  prettier,                      // Disable conflicting rules
  {
    rules: {
      // Custom strict rules
      'no-unused-vars': 'error',
      'no-console': 'error',
      'eqeqeq': 'error',
      'curly': 'error',
      // ... ~320+ total rules
    }
  }
];
```

**Total Rules**: ~320+ (ESLint equivalent of Ruff's 400+ for Python)

#### Option B: TypeScript Strict Mode
**Alternative for 100% type coverage**:

```json
// tsconfig.json
{
  "compilerOptions": {
    "strict": true,
    "noUnusedLocals": true,
    "noUnusedParameters": true,
    "noImplicitReturns": true,
    "noFallthroughCasesInSwitch": true,
    "checkJs": true,
    "allowJs": true
  }
}
```

**Advantages**:
- True type checking (not just JSDoc comments)
- Catches 80% of bugs ESLint misses
- Industry standard for JavaScript quality

---

## 8. Effort Estimation: Unify Linting

### Scenario A: Standardize ESLint + Prettier (Current Tools)

#### Phase 1: Add Missing Scripts (6 packages)
**Effort**: 2 hours
**Tasks**:
- Add `lint`, `lint:fix`, `format`, `format:check` to 6 packages
- Verify scripts run without errors
- Fix any linting violations

**Affected Packages**:
```json
// Add to package.json for each
{
  "scripts": {
    "lint": "eslint src/ test/ --max-warnings=0",
    "lint:fix": "eslint src/ test/ --fix",
    "format": "prettier --write src/ test/",
    "format:check": "prettier --check src/ test/"
  }
}
```

#### Phase 2: Unify ESLint Configs
**Effort**: 4 hours
**Tasks**:
- Remove package-specific configs (`.eslintrc.json`)
- Migrate all to root `eslint.config.mjs`
- Test monorepo-wide linting
- Fix violations

**Removals**:
- `packages/docs/eslint.config.mjs` → Use root
- `playground/hooks-showcase/.eslintrc.json` → Use root
- `packages/nextra/.eslintrc.json` → Use root
- `packages/kgc-4d/playground/.eslintrc.json` → Use root

**Keep (Special Cases)**:
- `packages/kgn/.eslintrc.determinism.js` (requires custom plugin implementation)

#### Phase 3: Implement KGN Determinism Rules
**Effort**: 16 hours
**Tasks**:
- Create ESLint plugin for custom rules
- Implement 10 determinism rules
- Test on KGN package
- Document usage

**Custom Plugin Structure**:
```javascript
// eslint-plugin-determinism/index.mjs
export default {
  rules: {
    'no-new-date': { /* implementation */ },
    'no-date-now': { /* implementation */ },
    'no-math-random': { /* implementation */ },
    'require-object-key-sort': { /* implementation */ },
    // ... 6 more rules
  }
};
```

#### Phase 4: CI/CD Integration
**Effort**: 2 hours
**Tasks**:
- Add pre-commit hook (Husky)
- Add GitHub Actions workflow
- Configure lint-staged

```yaml
# .github/workflows/lint.yml
name: Lint
on: [push, pull_request]
jobs:
  lint:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: pnpm/action-setup@v2
      - run: pnpm install
      - run: pnpm run lint
      - run: pnpm run format:check
```

**Total Effort**: **24 hours** (3 days)

---

### Scenario B: Migrate to Enhanced Strict ESLint (400+ Rules)

#### Phase 1-4: Same as Scenario A (24 hours)

#### Phase 5: Add Strict ESLint Plugins
**Effort**: 8 hours
**Tasks**:
- Install plugins (`unicorn`, `sonarjs`, `security`)
- Configure 320+ rules
- Fix violations across 19 packages
- Document exceptions

**New Dependencies**:
```json
{
  "devDependencies": {
    "eslint-plugin-unicorn": "^56.0.2",
    "eslint-plugin-sonarjs": "^0.25.1",
    "eslint-plugin-security": "^3.0.1"
  }
}
```

#### Phase 6: 100% Rule Compliance
**Effort**: 40 hours (varies by codebase size)
**Tasks**:
- Fix ~2000-5000 violations (estimate based on 247 files)
- Refactor code smells
- Add missing JSDoc comments
- Security audit fixes

**Total Effort**: **72 hours** (9 days)

---

### Scenario C: Migrate to TypeScript Strict Mode

#### Phase 1-4: Same as Scenario A (24 hours)

#### Phase 5: TypeScript Configuration
**Effort**: 4 hours
**Tasks**:
- Add `tsconfig.json` to root + packages
- Enable `checkJs` for `.mjs` files
- Configure strict compiler options

#### Phase 6: Add Type Annotations
**Effort**: 80 hours (varies significantly)
**Tasks**:
- Convert JSDoc to TypeScript types (247 files)
- Fix type errors
- Add missing types for external libraries

**Total Effort**: **108 hours** (13.5 days)

---

## 9. Recommended Action Plan

### Immediate Actions (Week 1)

**Priority**: Fix inconsistencies, NOT migrate to Ruff (impossible)

#### Action 1: Add Missing Lint Scripts
**Who**: Junior Developer
**Effort**: 2 hours
**Packages**: 6 packages without linting

```bash
# Template to add to each package.json
pnpm -C packages/atomvm pkg set scripts.lint="eslint src/ test/ --max-warnings=0"
pnpm -C packages/atomvm pkg set scripts.lint:fix="eslint src/ test/ --fix"
pnpm -C packages/atomvm pkg set scripts.format="prettier --write src/ test/"
pnpm -C packages/atomvm pkg set scripts.format:check="prettier --check src/ test/"

# Repeat for: test-utils, engine-gateway, kgc-4d, domain, validation
```

#### Action 2: Verify Root ESLint Config
**Who**: Senior Developer
**Effort**: 1 hour
**Task**: Ensure all packages use root `eslint.config.mjs`

```bash
# Test monorepo-wide linting
pnpm run lint

# Fix violations
pnpm run lint:fix
```

#### Action 3: Document Linting Standards
**Who**: Technical Writer
**Effort**: 2 hours
**Deliverable**: Update `CONTRIBUTING.md` with:
- Required lint scripts
- Pre-commit expectations
- How to run linting locally

---

### Medium-Term Actions (Month 1)

#### Action 4: Implement KGN Determinism Plugin
**Who**: Senior Developer (ESLint expert)
**Effort**: 16 hours
**Deliverable**: `eslint-plugin-unrdf-determinism` package

**Why**: KGN's 280-line config has custom rules that don't exist yet

#### Action 5: Standardize All Package Configs
**Who**: Mid-level Developer
**Effort**: 4 hours
**Task**: Remove local ESLint configs, use root only

**Exceptions**:
- Keep KGN determinism config (special case)
- Keep Nuxt/Next.js framework configs (auto-generated)

#### Action 6: Add Pre-commit Hooks
**Who**: DevOps Engineer
**Effort**: 2 hours
**Tool**: Husky + lint-staged

```json
// package.json
{
  "lint-staged": {
    "*.{js,mjs}": ["eslint --fix", "prettier --write"],
    "*.{json,md}": ["prettier --write"]
  }
}
```

---

### Long-Term Actions (Quarter 1)

#### Action 7: Enhanced Strict Linting
**Who**: Senior Developer + Team
**Effort**: 72 hours (incremental)
**Goal**: Achieve 320+ rule enforcement

**Plugins to Add**:
1. `eslint-plugin-unicorn` - Best practices (100 rules)
2. `eslint-plugin-sonarjs` - Code smells (60 rules)
3. `eslint-plugin-security` - Security (30 rules)

**Rollout**:
- Week 1: Install plugins, configure rules
- Week 2-4: Fix violations package-by-package
- Week 5: Enable in CI/CD

#### Action 8: 100% Type Coverage
**Who**: Team-wide initiative
**Effort**: 80 hours (incremental)
**Goal**: JSDoc on ALL functions or migrate to TypeScript

**Approach A (JSDoc)**:
```javascript
/**
 * Create RDF store with Oxigraph backend
 * @param {Object} options - Configuration options
 * @param {string} options.path - Database path
 * @returns {Store} Oxigraph store instance
 */
export function createStore(options) {
  // ...
}
```

**Approach B (TypeScript)**:
```typescript
// Rename .mjs → .ts
export function createStore(options: StoreOptions): Store {
  // Type checking automatic
}
```

---

## 10. Summary & Recommendations

### Current State Assessment
- ✅ **Modern tooling**: ESLint 9 Flat Config, Prettier 3.7
- ✅ **Mostly consistent**: 13/19 packages have linting
- ❌ **32% coverage gap**: 6 packages lack linting scripts
- ❌ **Inconsistent configs**: Mix of root + local configs
- ⚠️ **Unimplemented rules**: KGN determinism rules exist as comments only

### Critical Corrections

**CANNOT use Ruff** because:
1. Ruff is Python-only (no JavaScript support)
2. UNRDF is 100% JavaScript/TypeScript
3. Zero Python files in source code

**CLAUDE.md needs updating** to reflect JavaScript standards:
- Replace Ruff → ESLint + plugins
- Replace mypy → TypeScript or JSDoc
- Replace pytest → Vitest (already correct)
- Replace uv → pnpm (already correct)

### Recommended Path: Enhanced ESLint

**Goal**: Achieve "400+ rules" equivalent for JavaScript

**Strategy**:
```
Current ESLint (~15 rules)
→ Add recommended plugins (~320 rules)
→ Implement KGN custom rules (~10 rules)
→ Total: ~345 rules (comparable to Ruff's 400+)
```

**Effort**: 72 hours over 1 quarter
**Benefit**: Lean Six Sigma quality for JavaScript codebase

### Immediate Next Steps

1. **Add lint scripts** to 6 missing packages (2 hours)
2. **Run `pnpm run lint`** and fix violations (2 hours)
3. **Update CLAUDE.md** to remove Python references (1 hour)
4. **Document linting standards** in CONTRIBUTING.md (2 hours)

**Total**: 7 hours to baseline consistency

---

## Appendix A: File Inventory

### ESLint Configurations
1. `/Users/sac/unrdf/eslint.config.mjs` (200 lines) - **ROOT CONFIG**
2. `/Users/sac/unrdf/packages/kgn/.eslintrc.determinism.js` (280 lines) - **CUSTOM DETERMINISM**
3. `/Users/sac/unrdf/packages/docs/eslint.config.mjs` (5 lines) - Nuxt wrapper
4. `/Users/sac/unrdf/playground/hooks-showcase/.eslintrc.json` (2 lines) - Next.js
5. `/Users/sac/unrdf/packages/nextra/.eslintrc.json` (2 lines) - Next.js
6. `/Users/sac/unrdf/packages/kgc-4d/playground/.eslintrc.json` (2 lines) - Next.js
7. `/Users/sac/unrdf/packages/docs/.nuxt/eslint.config.mjs` (auto-generated)

### Prettier Configurations
1. `/Users/sac/unrdf/.prettierrc` (12 lines) - **MONOREPO-WIDE**

### Ruff Configurations
**NONE** (Ruff not applicable to JavaScript)

---

## Appendix B: Package Linting Matrix

| Package | Lint | Fix | Format | Check | Config Source |
|---------|------|-----|--------|-------|---------------|
| core | ✅ | ✅ | ✅ | ✅ | Root |
| hooks | ✅ | ✅ | ✅ | ✅ | Root |
| federation | ✅ | ✅ | ✅ | ✅ | Root |
| streaming | ✅ | ✅ | ✅ | ✅ | Root |
| composables | ✅ | ✅ | ✅ | ✅ | Root |
| dark-matter | ✅ | ✅ | ✅ | ✅ | Root |
| oxigraph | ✅ | ✅ | ✅ | ✅ | Root |
| cli | ✅ | ✅ | ✅ | ✅ | Root |
| project-engine | ✅ | ✅ | ✅ | ✅ | Root |
| knowledge-engine | ✅ | ✅ | ✅ | ✅ | Root |
| kgn | ✅ | ✅ | ❌ | ❌ | Local (determinism) |
| docs | ✅ | ❌ | ❌ | ❌ | Nuxt |
| nextra | ⚠️ | ⚠️ | ❌ | ❌ | Next.js (disabled) |
| test-utils | ❌ | ❌ | ❌ | ❌ | NONE |
| atomvm | ❌ | ❌ | ❌ | ❌ | NONE |
| engine-gateway | ❌ | ❌ | ❌ | ❌ | NONE |
| kgc-4d | ❌ | ❌ | ❌ | ❌ | NONE |
| domain | ❌ | ❌ | ❌ | ❌ | NONE |
| validation | ❌ | ❌ | ❌ | ❌ | NONE |

**Legend**:
- ✅ Present and configured
- ❌ Missing
- ⚠️ Disabled/Skipped

---

## Appendix C: ESLint Plugin Comparison (JavaScript "Ruff Equivalent")

| Plugin | Rules | Category | Ruff Equivalent |
|--------|-------|----------|-----------------|
| `@eslint/js` recommended | ~80 | Core errors | `pycodestyle` (E/W) |
| `eslint-plugin-unicorn` | ~100 | Best practices | `flake8-bugbear` |
| `eslint-plugin-sonarjs` | ~60 | Code smells | `pylint` |
| `eslint-plugin-security` | ~30 | Security | `bandit` |
| `eslint-plugin-jsdoc` | ~50 | Documentation | `pydocstyle` |
| `eslint-plugin-import` | ~40 | Import order | `isort` |
| **TOTAL** | **~360 rules** | **Comprehensive** | **Ruff's 400+** |

**Recommendation**: Use above stack for JavaScript equivalent of Ruff's strict Python linting.

---

**END OF REPORT**
