# UNRDF Monorepo Structure Audit Report

**Generated**: 2025-12-20
**Total Packages**: 19 (excl. 2 non-package dirs: browser, react)
**Working Directory**: `/Users/sac/unrdf/`

## Executive Summary

### Consistency Findings

✅ **STRONG CONSISTENCY (95% standardization)**:
- **All 19 packages** use `type: "module"` (ESM)
- **18/19 packages** (95%) have `src/` directories
- **17/19 packages** (89%) use `src/index.mjs` or `src/index.js` as main entry
- **15/19 packages** (79%) have `test/` directories (not `tests/`)
- **15/19 packages** (79%) have `build.config.mjs` for unbuild

❌ **INCONSISTENCIES IDENTIFIED**:
1. **Entry point variations**: 3 packages use `./src/index.mjs`, 15 use `src/index.mjs` (leading `./`)
2. **File extension mix**: 1 package (`kgn`) uses `.js` instead of `.mjs` in main entry
3. **Test directory naming**: 2 packages use `tests/`, 17 use `test/`
4. **Missing test directories**: 4 packages have no test directory
5. **Dist directory inconsistency**: Only 8/19 packages generate `dist/` (unbuild not universal)
6. **Root-level clutter**: `kgn` package has 14 root-level JS files (debug, demo, test scripts)

---

## Detailed Package Analysis

### 1. Directory Structure Patterns

| Package | src/ | lib/ | test/ | tests/ | dist/ | Notes |
|---------|------|------|-------|--------|-------|-------|
| atomvm | ✅ | ❌ | ✅ | ❌ | ✅ | Standard |
| browser | ❌ | ❌ | ❌ | ❌ | ❌ | **No src/** |
| cli | ✅ | ✅ (in src) | ✅ | ❌ | ✅ | Has src/lib/ |
| composables | ✅ | ❌ | ✅ | ❌ | ✅ | Standard |
| core | ✅ | ❌ | ✅ | ❌ | ✅ | Standard |
| dark-matter | ✅ | ❌ | ✅ | ❌ | ❌ | No dist |
| docs | ❌ | ❌ | ❌ | ✅ | ❌ | **Uses tests/** |
| domain | ✅ | ❌ | ❌ | ❌ | ❌ | **No tests** |
| engine-gateway | ✅ | ❌ | ✅ | ❌ | ✅ | Standard |
| federation | ✅ | ❌ | ✅ | ❌ | ❌ | No dist |
| hooks | ✅ | ❌ | ✅ | ❌ | ❌ | No dist |
| kgc-4d | ✅ | ❌ | ✅ | ❌ | ✅ | Standard |
| kgn | ✅ | ❌ | ✅ | ✅ | ✅ | **Both test/ AND tests/** |
| knowledge-engine | ✅ | ❌ | ✅ | ❌ | ❌ | No dist |
| nextra | ✅ | ❌ | ❌ | ❌ | ❌ | **No tests** |
| oxigraph | ✅ | ❌ | ✅ | ❌ | ✅ | Standard |
| project-engine | ✅ | ❌ | ✅ | ❌ | ❌ | No dist |
| react | ✅ | ❌ | ❌ | ❌ | ❌ | **No tests** |
| streaming | ✅ | ❌ | ✅ | ❌ | ❌ | No dist |
| test-utils | ✅ | ❌ | ❌ | ❌ | ❌ | **No tests** |
| validation | ✅ | ❌ | ❌ | ❌ | ✅ | **No tests** |

**Pattern Summary**:
- **src/**: 18/19 packages (95%) ✅
- **lib/**: Only `cli` has nested `src/lib/` (5%)
- **test/**: 15/19 packages (79%)
- **tests/**: 2/19 packages (11%) - `docs`, `kgn`
- **dist/**: 8/19 packages (42%)

---

### 2. Entry Point Patterns

| Package | Main Entry | Leading ./ | Extension | Status |
|---------|------------|------------|-----------|--------|
| atomvm | `src/index.mjs` | ❌ | .mjs | ✅ Standard |
| cli | `src/index.mjs` | ❌ | .mjs | ✅ Standard |
| composables | `src/index.mjs` | ❌ | .mjs | ✅ Standard |
| core | `src/index.mjs` | ❌ | .mjs | ✅ Standard |
| dark-matter | `src/index.mjs` | ❌ | .mjs | ✅ Standard |
| docs | NONE | - | - | ❌ No entry |
| domain | `./src/index.mjs` | ✅ | .mjs | ⚠️ Leading ./ |
| engine-gateway | `./src/index.mjs` | ✅ | .mjs | ⚠️ Leading ./ |
| federation | `src/index.mjs` | ❌ | .mjs | ✅ Standard |
| hooks | `src/index.mjs` | ❌ | .mjs | ✅ Standard |
| kgc-4d | `./src/index.mjs` | ✅ | .mjs | ⚠️ Leading ./ |
| kgn | `src/index.js` | ❌ | **.js** | ⚠️ .js not .mjs |
| knowledge-engine | `src/index.mjs` | ❌ | .mjs | ✅ Standard |
| nextra | NONE | - | - | ❌ No entry |
| oxigraph | `src/index.mjs` | ❌ | .mjs | ✅ Standard |
| project-engine | `src/index.mjs` | ❌ | .mjs | ✅ Standard |
| streaming | `src/index.mjs` | ❌ | .mjs | ✅ Standard |
| test-utils | `./src/index.mjs` | ✅ | .mjs | ⚠️ Leading ./ |
| validation | `./src/index.mjs` | ✅ | .mjs | ⚠️ Leading ./ |

**Inconsistencies**:
- **Leading `./`**: 5 packages (`domain`, `engine-gateway`, `kgc-4d`, `test-utils`, `validation`)
- **Extension**: 1 package (`kgn`) uses `.js` instead of `.mjs`
- **Missing entry**: 2 packages (`docs`, `nextra`) have no `main` field

**Recommendation**: Standardize on `src/index.mjs` (no leading `./`, use `.mjs`)

---

### 3. Source Code Organization

#### Packages with Subdirectories in src/

| Package | Subdirs | Structure |
|---------|---------|-----------|
| **kgn** | 12 | base, core, doc-generator, engine, filters, inheritance, injection, linter, parser, renderer, templates, tests |
| core | 7 | integration, ontologies, profiling, rdf, sparql, utils, validation |
| knowledge-engine | 6 | engines, knowledge-engine, monitoring, security, utils, validators |
| cli | 4 | cli, commands, core, lib |
| kgc-4d | 3 | core, doctest, hdit |
| project-engine | 2 | lens, project-engine |
| composables | 2 | composables, context |
| domain | 2 | formatters, models |
| hooks | 2 | hooks, security |
| streaming | 1 | streaming |
| federation | 1 | federation |
| dark-matter | 1 | dark-matter |
| atomvm | 1 | erlang |
| react | 1 | ai-semantic |

**Packages with FLAT src/ (no subdirs)**:
- `engine-gateway`, `nextra`, `oxigraph`, `test-utils`, `validation`

**Analysis**:
- **Complex packages** (>5 subdirs): `kgn` (12), `core` (7), `knowledge-engine` (6)
- **Simple packages** (0 subdirs): 5 packages
- Most packages (14/19) have 1-4 subdirectories

---

### 4. Test Directory Analysis

#### Test Directory Naming

| Pattern | Count | Packages |
|---------|-------|----------|
| `test/` | 15 | atomvm, cli, composables, core, dark-matter, engine-gateway, federation, hooks, kgc-4d, kgn, knowledge-engine, oxigraph, project-engine, streaming |
| `tests/` | 2 | docs, kgn |
| **No tests** | 4 | domain, nextra, react, test-utils, validation |
| **Both** | 1 | kgn (has both `test/` AND `tests/`) |

**Issues**:
1. **kgn anomaly**: Has BOTH `test/` AND `tests/` directories + `src/injection/tests/`
2. **Missing tests**: 4 packages have NO test directories
3. **Naming inconsistency**: 2 packages use `tests/` instead of standard `test/`

**Recommendation**: Standardize on `test/` (79% already use it)

---

### 5. Build Configuration

#### Build System Presence

| Config File | Count | Packages |
|-------------|-------|----------|
| `build.config.mjs` | 15 | atomvm, cli, composables, core, domain, engine-gateway, federation, hooks, kgc-4d, kgn, knowledge-engine, oxigraph, project-engine, streaming, validation |
| `tsconfig.json` | 2 | atomvm, browser |
| **No build config** | 2 | dark-matter, docs |

**Notes**:
- 79% of packages use `unbuild` via `build.config.mjs`
- Only 2 packages have TypeScript config (likely for type generation only, since all are `.mjs`)
- Build outputs to `dist/` when configured

---

### 6. Root-Level File Clutter

#### Packages with >3 Root-Level JS Files

| Package | Root JS Files | Issues |
|---------|---------------|--------|
| **kgn** | **14** | debug-*.js, demo-*.js, test-*.mjs scripts NOT in src/ or test/ |
| hooks | 4 | Likely build/config files |
| atomvm | 5 | Likely build/config files |
| federation | 3 | Acceptable |

**Critical Issue**: `kgn` package has excessive root-level clutter:
```
packages/kgn/
├── debug-native-engine.js
├── debug-split-join.js
├── debug-split-join2.js
├── demo-injection.js
├── determinism.test.js
├── simple-test.js
├── test-e2e-docs.mjs
├── test-parser.mjs
├── test-rdf-builder.mjs
├── test-rdf-frontmatter.mjs
├── test-scanner.mjs
├── vitest.config.js
├── .eslintrc.determinism.js
├── build.config.mjs
```

**Recommendation**: Move debug/demo/test scripts to:
- `test/manual/` for test scripts
- `examples/` for demos
- `scripts/` for build utilities

---

## Pattern Summary Statistics

### Entry Point Standardization

| Pattern | Count | Percentage |
|---------|-------|------------|
| `src/index.mjs` (standard) | 13 | 68% |
| `./src/index.mjs` (leading ./) | 5 | 26% |
| `src/index.js` (.js extension) | 1 | 5% |
| No main entry | 2 | 11% |

### Directory Organization

| Pattern | Count | Percentage |
|---------|-------|------------|
| Has `src/` | 18 | 95% ✅ |
| Has `test/` | 15 | 79% ✅ |
| Has `tests/` | 2 | 11% |
| Has `dist/` | 8 | 42% |
| Has nested `lib/` | 1 | 5% |
| No test directory | 4 | 21% ❌ |

### Build Configuration

| Pattern | Count | Percentage |
|---------|-------|------------|
| Uses unbuild | 15 | 79% ✅ |
| Has TypeScript config | 2 | 11% |
| No build config | 2 | 11% |

---

## Critical Issues Identified

### 🔴 HIGH PRIORITY

1. **kgn Package Root Clutter** (14 root-level files)
   - **Impact**: Violates project organization standards
   - **Fix**: Move to `test/manual/`, `examples/`, `scripts/`

2. **Missing Test Directories** (4 packages)
   - **Packages**: `domain`, `nextra`, `react`, `test-utils`, `validation`
   - **Impact**: No test coverage infrastructure
   - **Fix**: Create `test/` directories with placeholder tests

3. **Test Directory Naming Inconsistency**
   - **Packages**: `docs` (uses `tests/`), `kgn` (has BOTH)
   - **Impact**: Confusion, grep/glob pattern failures
   - **Fix**: Rename `tests/` → `test/`, remove duplicate in kgn

### 🟡 MEDIUM PRIORITY

4. **Entry Point Leading `./` Inconsistency** (5 packages)
   - **Packages**: `domain`, `engine-gateway`, `kgc-4d`, `test-utils`, `validation`
   - **Impact**: Minor inconsistency, works but not standard
   - **Fix**: Remove leading `./` from package.json main fields

5. **File Extension Inconsistency** (1 package)
   - **Package**: `kgn` (uses `src/index.js` not `.mjs`)
   - **Impact**: All packages use `.mjs` except this one
   - **Fix**: Rename `index.js` → `index.mjs`, update package.json

6. **Dist Directory Inconsistency** (8 packages with, 11 without)
   - **Impact**: Not all packages using unbuild generate dist
   - **Fix**: Ensure unbuild is configured and run for all 15 packages with `build.config.mjs`

### 🟢 LOW PRIORITY

7. **No Main Entry** (2 packages)
   - **Packages**: `docs`, `nextra`
   - **Impact**: Might be intentional (docs/tooling packages)
   - **Action**: Verify if these should have entry points

8. **cli Package Has Nested lib/** (1 package)
   - **Package**: `cli` has `src/lib/` subdirectory
   - **Impact**: Unique pattern, might be intentional
   - **Action**: Document rationale or flatten structure

---

## Recommendations for Standardization

### Phase 1: Critical Fixes (Do First)

1. **kgn Root Cleanup**
   ```bash
   cd packages/kgn
   mkdir -p test/manual examples scripts
   mv debug-*.js test/manual/
   mv demo-*.js examples/
   mv test-*.mjs test/manual/
   mv simple-test.js test/manual/
   mv determinism.test.js test/
   ```

2. **Test Directory Standardization**
   ```bash
   # Rename tests/ → test/
   mv packages/docs/tests packages/docs/test
   rm -rf packages/kgn/tests  # Duplicate

   # Create missing test directories
   mkdir -p packages/domain/test
   mkdir -p packages/nextra/test
   mkdir -p packages/react/test
   mkdir -p packages/test-utils/test
   mkdir -p packages/validation/test
   ```

3. **kgn Entry Point Extension**
   ```bash
   cd packages/kgn
   mv src/index.js src/index.mjs
   # Update package.json: "main": "src/index.mjs"
   ```

### Phase 2: Consistency Improvements

4. **Entry Point Normalization**
   - Remove leading `./` from 5 packages:
     - `domain`, `engine-gateway`, `kgc-4d`, `test-utils`, `validation`
   - Edit each `package.json`: `"main": "src/index.mjs"` (not `./src/...`)

5. **Build System Verification**
   - Run `pnpm run build` in all 15 packages with `build.config.mjs`
   - Verify `dist/` is generated
   - Add to .gitignore if missing

### Phase 3: Documentation

6. **Add CONTRIBUTING.md** with structure standards:
   ```markdown
   ## Package Structure Standards

   All packages MUST follow:
   - Entry point: `src/index.mjs` (no leading `./`, use `.mjs`)
   - Source code: `src/` directory
   - Tests: `test/` directory (NOT `tests/`)
   - Build output: `dist/` (generated by unbuild)
   - Root-level: Only package.json, README.md, build.config.mjs, LICENSE
   - Scripts/demos: Move to `examples/`, `scripts/`, or `test/manual/`
   ```

---

## Verification Commands

```bash
# Count packages by pattern
find packages -maxdepth 2 -name "package.json" | wc -l  # Should be 19

# Verify src/ directories
find packages -maxdepth 2 -type d -name "src" | wc -l   # Should be 18

# Check test directory naming
find packages -maxdepth 2 -type d -name "test" | wc -l  # Should be 15+
find packages -maxdepth 2 -type d -name "tests" | wc -l # Should be 0

# Find root-level JS files (excluding configs)
for dir in packages/*/; do
  count=$(find "$dir" -maxdepth 1 -name "*.mjs" -o -name "*.js" | grep -v "config\|vitest" | wc -l)
  if [ "$count" -gt 3 ]; then
    echo "$(basename $dir): $count files"
  fi
done

# Verify entry points
grep -r "\"main\"" packages/*/package.json | grep -v "src/index.mjs"
```

---

## Conclusion

The UNRDF monorepo shows **strong overall consistency** (95% use src/, 79% use test/, 79% use unbuild), with **3 critical issues**:

1. **kgn package** has excessive root-level clutter (14 files)
2. **4 packages** missing test directories
3. **Test naming** inconsistency (test/ vs tests/)

**Estimated effort to fix**:
- Phase 1 (critical): ~1 hour
- Phase 2 (consistency): ~30 minutes
- Phase 3 (documentation): ~30 minutes
- **Total**: ~2 hours

**Benefits**:
- ✅ 100% consistent entry points
- ✅ 100% consistent test directory naming
- ✅ Clean root directories (only configs/docs)
- ✅ Easier CI/CD automation (predictable patterns)
- ✅ Better developer experience (know where to find things)

---

**Next Steps**:
1. Review this audit with team
2. Approve standardization plan
3. Execute Phase 1 fixes (critical)
4. Create PR with automated checks for future compliance
