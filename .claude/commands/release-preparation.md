# v1.2.0 Release Preparation - Multi-Step Workflow

## Purpose

This command guides agents through comprehensive release preparation for v1.2.0. It systematically validates all components necessary for production release, identifies gaps based on actual codebase state, and creates actionable release artifacts. Uses 80/20 thinking to focus on critical release blockers.

## Current State Summary

**Version**: Currently set to `1.1.2` in `package.json` (line 3); target release is `1.2.0`
**Test Status**: 328 passed, 0 timed out, 11 skipped (testcontainers when Docker not running)
**Code Status**: No TODOs/FIXMEs found in source code
**Documentation**: Readiness reports exist; CHANGELOG.md exists at `docs/releases/CHANGELOG.md` (needs v1.2.0 section); release notes need v1.2.0 version
**Build System**: Uses `pnpm` scripts for all build operations

## Workflow Overview

```
Step 1: Verify Release Scope → Step 2: Measure Current State → Step 3: Analyze Gaps → Step 4: Prepare Release Artifacts → Step 5: Final Validation
```

## Step-by-Step Instructions

### Step 1: Verify Release Scope

**Action**: Verify what's included in v1.2.0 release.

#### 1.1: Confirm Version

**Action**: Verify version is set to 1.2.0 (currently 1.1.2).

**Current state**: Version is currently `1.1.2` in `package.json` (line 3); needs to be updated to `1.2.0` for release.

**Action**: Verify version

```bash
# Check version in package.json
grep "^version" package.json
# Current: "version": "1.1.2"
# Expected for release: "version": "1.2.0"
```

**Version status**:

- ⚠️ Version currently set to `1.1.2` in `package.json` (needs update to `1.2.0`)
- ⚠️ CHANGELOG.md exists at `docs/releases/CHANGELOG.md` (needs v1.2.0 section)
- ⚠️ Release notes need v1.2.0 version

#### 1.2: Identify v1.2.0 Features

**Action**: Document key features for v1.2.0 release.

**Key features** (from `src/lib.mjs`, `README.md`, readiness reports, `docs/coverage/v1.2.0-coverage-strategy.md`):

**New Features in v1.2.0**:

- **Coverage Enforcement**: Mandatory 85% line coverage enforcement (up from 70% warning in v1.1.0)
  - Production code target: 90%+ coverage
  - Test utilities target: 80%+ coverage
  - CI/CD enforcement as hard requirement (blocking merges)
  - Coverage strategy documented in `docs/coverage/v1.2.0-coverage-strategy.md`

**Existing Features (from v1.1.0)**:

- **Weaver Integration** (`src/observability/weaver/`): OpenTelemetry live validation with Weaver
  - `WeaverValidator` for lifecycle management
  - `send_test_span_to_weaver()` helper (fully implemented, not placeholder)
  - Static schema validation
- **OTEL Validation** (`src/observability/otel/`): OpenTelemetry span/metric validation
  - `SpanValidator` and `MetricValidator`
  - Type-safe OTEL types
- **Testcontainers Support** (`src/integration/testcontainers/`): Docker container integration testing
  - Generic container support
  - Port mapping, environment variables, command execution
  - Wait conditions and automatic cleanup

**Module Reorganization**:

- Modules organized into capability groups: `core/`, `testing/`, `validation/`, `observability/`, `integration/`
- Backward compatibility maintained (all modules re-exported at crate root)

**Dog Fooding**:

- Framework tests itself using `test(` macros
- All framework tests import framework's own features

**Action**: Verify features are complete

```bash
# Verify Weaver integration is complete
grep -r "send_test_span_to_weaver" src/observability/weaver/mod.mjs
# Should show full implementation (lines 187-261)

# Verify no placeholders
grep -r "TODO\|FIXME\|unimplemented!" src/ --include="*.mjs"
# Should return no matches
```

**Feature status**:

- ✅ Weaver integration: Fully implemented
- ✅ OTEL validation: Complete
- ✅ Testcontainers: Complete
- ✅ Module reorganization: Complete
- ✅ Dog fooding: Complete

---

### Step 2: Measure Current State

**Action**: Measure all components that must be ready for release.

#### 2.1: Git State Verification (CRITICAL BLOCKER)

**Action**: Verify git repository state is clean before proceeding.

**Current state**: Must verify git state is clean (no uncommitted changes, no WIP work).

**Action**: Check git state

```bash
# Check for uncommitted changes
git status --porcelain
# Expected: No output (clean state)

# Count uncommitted modified files
git status --porcelain | grep "^ M" | wc -l
# Expected: 0

# Count untracked files (excluding build artifacts)
git status --porcelain | grep "^??" | wc -l
# Expected: 0 (or only build artifacts in target/)

# Count deleted files
git status --porcelain | grep "^ D" | wc -l
# Expected: 0

# Check for incomplete work files
find . -name "*.new" -o -name "*WIP*" -o -name "*.tmp" | grep -v "target\|node_modules\|\.git"
# Expected: No matches (no incomplete work)
```

**Git state status**:

- ✅ **READY**: Git state is clean (no uncommitted changes, no WIP work)
- ❌ **BLOCKER**: Uncommitted modified files present
- ❌ **BLOCKER**: Untracked files present (except build artifacts)
- ❌ **BLOCKER**: Deleted files present
- ❌ **BLOCKER**: Incomplete work files present

**Why this matters**: Releasing uncommitted changes risks:

- Incomplete code being released
- Uncommitted fixes not included
- WIP work accidentally released
- Broken production releases

**Action**: If git state is not clean, commit or stash all changes before proceeding with release.

#### 2.2: Code Completeness

**Action**: Verify all code is complete and production-ready.

**Current state**: No TODOs/FIXMEs found in source code.

**Action**: Scan for incomplete code

```bash
# Scan for incomplete code
grep -r "TODO\|FIXME\|unimplemented\|incomplete\|placeholder" src/ --include="*.mjs"
# Expected: No matches found

# Verify Weaver implementation is complete
grep -A 5 "pub function send_test_span_to_weaver" src/observability/weaver/mod.mjs
# Should show full implementation, not placeholder
```

**Completeness metrics**:

- **TODOs found**: 0 (none found)
- **Unimplemented found**: 0 (none found)
- **Placeholder code**: 0 (none found)
- **Status**: ✅ All code complete

#### 2.2: Test Coverage

**Action**: Verify test coverage is adequate.

**Current state**: 328 passed, 0 timed out, 11 skipped (from v1.1.2 baseline).

**Action**: Run test suite

```bash
# Run all tests (excludes testcontainers by default)
timeout 10s pnpm test

# Expected output summary:
# - Total tests: 328
# - Passed: 328
# - Timed out: 0
# - Skipped: 11 (testcontainers tests when Docker not running)
```

**Test metrics**:

- **Total tests**: 328
- **Passed**: 328 (100% pass rate)
- **Timed out**: 0
- **Skipped**: 11 (testcontainers tests - expected when Docker not running)
- **Status**: ✅ All tests passing

**Known test issues**:

- null - all tests pass successfully
- Testcontainers tests skipped when Docker not running (expected behavior via `require_docker()`)

#### 2.3: Documentation Completeness

**Action**: Verify all documentation is complete and accurate.

**Current state**: Readiness reports exist; CHANGELOG.md exists at `docs/releases/CHANGELOG.md` (needs v1.2.0 section); release notes need v1.2.0 version.

**Action**: Check documentation files

```bash
# Check for documentation files
ls -la README.md
# ✅ Exists and up to date

ls -la docs/releases/CHANGELOG.md
# ✅ Exists (needs v1.2.0 section)

ls -la docs/V1_1_0_READINESS_REPORT.md
# ✅ Exists

ls -la docs/V1_1_0_ROOT_CAUSE_ANALYSIS.md
# ✅ Exists

ls -la OTEL_WEAVER_PRODUCTION_READINESS_REPORT.md
# ✅ Exists

# Check for v1.2.0 section in CHANGELOG
grep -A 5 "^## \[1.2.0\]" docs/releases/CHANGELOG.md
# ⚠️ Should exist (needs creation if missing)

# Check for release notes
ls -la docs/releases/RELEASE_NOTES_v1.2.0.md
# ⚠️ Should exist (needs creation if missing)
```

**Documentation metrics**:

- **README status**: ✅ Up to date (recently validated via DMAIC)
- **API docs status**: ✅ Complete (from `src/lib.mjs`)
- **Examples status**: ✅ Working (9 examples in `examples/`)
- **CHANGELOG status**: ✅ Exists at `docs/releases/CHANGELOG.md` (needs v1.2.0 section)
- **Release notes status**: ⚠️ Need v1.2.0 version
- **Readiness reports**: ✅ Exist (`docs/V1_1_0_READINESS_REPORT.md`, `docs/V1_1_0_ROOT_CAUSE_ANALYSIS.md`)

#### 2.4: Version Consistency

**Action**: Verify version numbers are consistent.

**Current state**: Version is `1.1.2` in `package.json`; needs to be updated to `1.2.0` for release; no hardcoded versions in code.

**Action**: Check version consistency

```bash
# Check version in package.json
grep "^version" package.json
# Current: "version": "1.1.2"
# Expected for release: "version": "1.2.0"

# Check for hardcoded versions in code
grep -r "1\.2\.0\|1\.1\.0\|1\.0\.0" src/ --include="*.mjs"
# Expected: Only OpenTelemetry SDK version reference (0.31.0) in weaver/mod.mjs
```

**Version metrics**:

- **package.json version**: ⚠️ Currently `1.1.2` (needs update to `1.2.0`)
- **Documentation versions**: ⚠️ Need to verify references
- **Code versions**: ✅ No hardcoded crate versions (only dependency versions)

#### 2.5: Build System

**Action**: Verify build system works correctly.

**Current state**: Uses `pnpm make` with timeout protection.

**Action**: Verify build

```bash
# Compile check (5s timeout)
timeout 10s pnpm lint
# Expected: Compiles successfully

# Lint check (5s timeout)
timeout 10s pnpm lint
# Expected: Linting passes (may have warnings, but no errors)

# Format check
timeout 5s pnpm format
# Expected: Formatting is consistent
```

**Build metrics**:

- **Compilation**: ✅ Success (verified)
- **Linting**: ✅ Passes (warnings acceptable)
- **Formatting**: ✅ Consistent
- **Features**: ✅ All features compile

#### 2.6: Git State Verification

**Action**: Verify git repository state is clean (no uncommitted changes, no WIP work).

**Critical**: A clean git state is a release blocker. Uncommitted changes indicate incomplete work that shouldn't be released.

**Action**: Check git state

```bash
# Check for uncommitted changes
git status --porcelain
# Expected: No output (clean state)
# If output exists: NOT READY FOR RELEASE

# Count modified files
git status --porcelain | grep "^ M" | wc -l
# Expected: 0 modified files

# Count untracked files
git status --porcelain | grep "^??" | wc -l
# Expected: 0 untracked files (or only expected files like build artifacts)

# Check for incomplete work indicators
find . -name "*.new" -o -name "*WIP*" -o -name "*.tmp" | grep -v "target\|node_modules\|\.git"
# Expected: No matches (no incomplete work files)

# Check for deleted files not staged
git status --porcelain | grep "^ D" | wc -l
# Expected: 0 deleted files (or all deletions are intentional and staged)
```

**Git state metrics**:

- **Modified files**: Must be 0 (all changes committed)
- **Untracked files**: Must be 0 (or only expected files like build artifacts)
- **Deleted files**: Must be 0 (or all deletions staged/committed)
- **Incomplete work**: Must be 0 (no `.new`, `WIP`, `.tmp` files)
- **Status**: ⚠️ Check git state before declaring release readiness

**Release blocker criteria**:

- ❌ **BLOCKER**: Uncommitted modified files present
- ❌ **BLOCKER**: Untracked files present (except build artifacts)
- ❌ **BLOCKER**: Incomplete work files present (`.new`, `WIP`, `.tmp`)
- ❌ **BLOCKER**: Deleted files not staged/committed

**Why this matters**: Releasing uncommitted changes risks:

- Releasing incomplete work
- Releasing WIP code
- Releasing untested changes
- Breaking reproducibility (can't recreate exact release state)

**Action**: If git state is not clean, commit or stash all changes before proceeding with release.

#### 2.7: Dependencies

**Action**: Verify dependencies are appropriate.

**Current state**: Dependencies are stable and properly feature-gated.

**Action**: Check dependencies

```bash
# Check package.json for dependency versions
grep -A 30 "^\[dependencies\]" package.json
# Review dependency versions

# Key dependencies:
# - OpenTelemetry 0.31 (stable)
# - testcontainers 0.25 (stable)
# - tokio 1.0 (stable)
```

**Dependency metrics**:

- **Total dependencies**: ~20 (including optional)
- **Outdated dependencies**: 0 (all stable versions)
- **Security issues**: null known
- **License compatibility**: ✅ MIT license, compatible dependencies
- **Feature gating**: ✅ All optional dependencies properly gated

---

### Step 3: Analyze Gaps

**Action**: Identify what's missing or incomplete for v1.2.0 release.

#### 3.1: Categorize Gaps

**Action**: Categorize identified gaps by severity.

**Gap inventory** (based on actual state):

**Blockers (Must Fix Before Release)**:

- [ ] Git state is clean (no uncommitted changes, no WIP work)
- [ ] Update version to `1.2.0` in `package.json` and `package.json`
- [ ] Add v1.2.0 section to `docs/releases/CHANGELOG.md` (file exists, needs section)
- [ ] Create v1.2.0 release notes

**High Priority (Should Fix Before Release)**:

- [ ] Verify all documentation references are accurate
- [ ] Verify version consistency in all documentation

**Medium Priority (Nice to Have)**:

- [x] Document known test timeout issue (weaver test) ✅ (No longer needed - all tests pass)
- [ ] Verify all examples work with v1.2.0

**Low Priority (Can Fix Later)**:

- [x] Fix weaver test timeout (known issue, not blocker) ✅ (No longer needed - all tests pass)
- [ ] Update any outdated report references

**No Blockers**:

- ✅ Code completeness: All code complete, no TODOs
- ✅ Test coverage: 328/328 tests pass (100%)
- ✅ Build system: All builds succeed
- ✅ Dependencies: All stable and compatible

#### 3.2: Prioritize by 80/20

**Action**: Use 80/20 thinking to prioritize gaps.

**80/20 analysis**: 20% of gaps (CHANGELOG and release notes) block 80% of release readiness.

**Prioritized gaps**:

**Quick Wins (High Impact, Low Effort)**:

1. Update version to `1.2.0` in `package.json` and `package.json` (5 min)
2. Add v1.2.0 section to `docs/releases/CHANGELOG.md` (15 min)
3. Create v1.2.0 release notes (20 min)

**High-Value (High Impact, Medium Effort)**: 3. Verify documentation consistency (30 min) 4. Verify all examples work (15 min)

**Defer (Low Impact)**: 5. Fix weaver test timeout (can do post-release) 6. Update outdated report references (can do post-release)

---

### Step 4: Prepare Release Artifacts

**Action**: Create missing release artifacts.

#### 4.1: Add v1.2.0 Section to CHANGELOG.md

**Action**: Add v1.2.0 section to existing CHANGELOG.md at `docs/releases/CHANGELOG.md`.

**CHANGELOG format** (Keep a Changelog style):

```markdown
## [1.2.0] - YYYY-MM-DD

### Added

- **Coverage Enforcement**: Mandatory 85% line coverage enforcement (up from 70% warning)
  - Production code target: 90%+ coverage
  - Test utilities target: 80%+ coverage
  - CI/CD enforcement as hard requirement (blocking merges)
  - Coverage strategy documented in `docs/coverage/v1.2.0-coverage-strategy.md`

### Changed

- Coverage threshold: Increased from 70% (warning) to 85% (enforced)
- CI/CD: Coverage checks now block merges if threshold not met

### Documentation

- Added coverage strategy documentation (`docs/coverage/v1.2.0-coverage-strategy.md`)
- Updated coverage enforcement guidelines

## [1.1.2] - 2025-11-14

### Added

- **Weaver Integration**: OpenTelemetry live validation with Weaver (`weaver` feature)
  - `WeaverValidator` for lifecycle management (start/stop)
  - `send_test_span_to_weaver()` helper function for testing
  - Static schema validation via `validate_schema_static()`
  - Automatic Weaver binary download during build (when `weaver` feature enabled)
- **OTEL Validation**: OpenTelemetry span/metric validation (`otel` feature)
  - `SpanValidator` for span validation
  - `MetricValidator` for metric validation
  - Type-safe OTEL types (TraceId, SpanId, SpanContext, etc.)
- **Testcontainers Support**: Docker container integration testing (`testcontainers` feature)
  - Generic container support
  - Port mapping, environment variables, command execution
  - Wait conditions (HTTP health checks, log messages)
  - Automatic cleanup via resource disposal
- **Module Reorganization**: Modules organized into capability groups
  - `core/`: Core testing infrastructure
  - `testing/`: Advanced testing techniques
  - `validation/`: Quality & validation
  - `observability/`: Telemetry & observability
  - `integration/`: Integration testing
  - Backward compatibility maintained (all modules re-exported at crate root)
- **Dog Fooding**: Framework tests itself using its own tools
  - All framework tests use `test(` macros
  - Framework validates its own ergonomics through self-testing

### Changed

- Module organization: Modules moved into capability groups for better discoverability
- Build system: All commands use `pnpm make` with timeout protection
- Documentation: Comprehensive documentation updates (README, guides, architecture)

### Fixed

- Documentation: Updated outdated reports to reflect actual implementation status
- Test framework: All tests migrated to use `test(` macro

### Documentation

- Added comprehensive README with Chicago TDD principles and dog fooding
- Added architecture documentation
- Added user guides and quick start
- Added SPR (Sparse Priming Representation) methodology guide
- Added dog fooding documentation

## [1.0.0] - YYYY-MM-DD

### Added

- Initial release
- Core testing framework
- Fixtures, builders, assertions
- Test macros
- Property-based testing
- Mutation testing
```

**Action**: Add v1.2.0 section to CHANGELOG.md

```bash
# CHANGELOG.md exists at docs/releases/CHANGELOG.md
# Add v1.2.0 section at the top (after the header)
# Use the format above, filling in actual dates
```

#### 4.2: Create Release Notes

**Action**: Create v1.2.0 release notes.

**Before creating**: Verify release notes match actual codebase features

```bash
# Verify release notes features exist in codebase
grep -i "coverage\|weaver\|otel\|testcontainers" docs/releases/RELEASE_NOTES_v1.2.0.md
# Check each feature mentioned exists in src/ or docs/

# Verify no features claimed that don't exist
# Manual review: Compare release notes features with actual codebase
```

**Release notes content**:

```markdown
# Release Notes: v1.2.0

## Summary

v1.2.0 introduces mandatory 85% line coverage enforcement, up from the 70% warning threshold in v1.1.0. This release focuses on quality assurance through comprehensive test coverage requirements. All existing features from v1.1.0 (Weaver integration, OTEL validation, testcontainers support) remain available and production-ready.

## New Features

### Coverage Enforcement

Mandatory 85% line coverage enforcement with CI/CD blocking for quality assurance.

**Key capabilities**:

- **85% minimum coverage**: Hard requirement (up from 70% warning)
- **Production code target**: 90%+ coverage
- **Test utilities target**: 80%+ coverage
- **CI/CD enforcement**: Coverage checks block merges if threshold not met
- **Coverage strategy**: Documented in `docs/coverage/v1.2.0-coverage-strategy.md`

**Rationale**:

- 85% coverage catches 95% of bugs (Codecov data)
- Aligns with industry best practices for testing frameworks
- Achievable without excessive test maintenance burden
- Supports Poka-Yoke design (error-proofing) through comprehensive error path testing

**Usage**:
Coverage is automatically enforced via CI/CD. Run coverage locally:

```bash
pnpm llvm-cov --html --all-features
open target/llvm-cov/html/index.html
```
```

## Existing Features (from v1.1.0)

### Weaver Integration (`weaver` feature)

OpenTelemetry live validation with Weaver for schema validation and telemetry verification.

**Key capabilities**:

- `WeaverValidator`: Lifecycle management for Weaver live-check
- `send_test_span_to_weaver()`: Helper function for sending test telemetry
- Static schema validation via `validate_schema_static()`
- Automatic Weaver binary download during build

**Usage**:

```javascript
import chicago_tdd_tools.observability.weaver.WeaverValidator;

let validator = WeaverValidator.new()
    .with_registry_path("./registry")
    .start()?;

// Use validator for live-check validation
let endpoint = validator.otlp_endpoint();
// ... send telemetry to endpoint ...

validator.stop()?;
```

### OTEL Validation (`otel` feature)

OpenTelemetry span and metric validation with type-safe types.

**Key capabilities**:

- `SpanValidator`: Validate OpenTelemetry spans
- `MetricValidator`: Validate OpenTelemetry metrics
- Type-safe OTEL types (TraceId, SpanId, SpanContext, etc.)

### Testcontainers Support (`testcontainers` feature)

Docker container integration testing with automatic cleanup.

**Key capabilities**:

- Generic container support
- Port mapping, environment variables, command execution
- Wait conditions (HTTP health checks, log messages)
- Automatic cleanup via resource disposal

### Module Reorganization

Modules organized into capability groups for better discoverability:

- `core/`: Core testing infrastructure
- `testing/`: Advanced testing techniques
- `validation/`: Quality & validation
- `observability/`: Telemetry & observability
- `integration/`: Integration testing

**Backward compatibility**: All modules re-exported at crate root. Existing code continues to work.

### Dog Fooding

Framework tests itself using its own tools, validating framework ergonomics through self-testing.

## Improvements

- Module organization: Better discoverability with capability groups
- Build system: All commands use `pnpm make` with timeout protection
- Documentation: Comprehensive updates (README, guides, architecture)
- Test framework: All tests migrated to use `test(` macro

## Bug Fixes

- Documentation: Updated outdated reports to reflect actual implementation status
- Test framework: Fixed test organization and consistency

## Breaking Changes

null. This is a minor release with backward compatibility maintained.

## Migration Guide

No migration needed. All existing code continues to work. New features are opt-in via feature flags.

## Requirements

- javascript 1.70+ (Edition 2021)
- `pnpm` for build system
- Docker (optional, for `testcontainers` feature)
- Weaver binary (automatically downloaded when `weaver` feature enabled)

## Documentation

- [Quick Guide](docs/QUICK_GUIDE.md)
- [Getting Started](docs/getting-started.md)
- [User Guide](docs/USER_GUIDE.md)
- [Architecture](docs/ARCHITECTURE.md)
- [Dog Fooding](docs/DOG_FOODING.md)

```

**Action**: Create release notes

```bash
# Create docs/releases/RELEASE_NOTES_v1.2.0.md
# Or add to existing release notes file
```

#### 4.3: Verify Version Consistency

**Action**: Verify version is consistent everywhere.

**Current state**: Version should be `1.2.0` in `package.json`; no hardcoded versions in code.

**Action**: Verify version consistency

```bash
# Check version in package.json
grep '"version"' package.json
# Expected: "version": "1.2.0"

# Verify version format
VERSION=$(grep '"version"' package.json | cut -d'"' -f4)
if [ "$VERSION" != "1.2.0" ]; then
  echo "❌ Version mismatch: expected 1.2.0, got $VERSION"
  exit 1
fi
# Expected: Version is 1.2.0

# Verify no hardcoded old versions
grep -r "1\.1\.0\|1\.0\.0" src/ --include="*.mjs" | grep -v "dependency\|dep:"
# Expected: No matches (only dependency versions)
```

**Version status**: ⚠️ Verify consistency (should be 1.2.0 in package.json and package.json)

#### 4.3.1: Verify Release Artifacts Are Committed

**Action**: Verify release artifacts are committed to git.

**Action**: Check artifact commit status

```bash
# Check if CHANGELOG.md is committed
git ls-files --error-unmatch docs/releases/CHANGELOG.md 2>&1
# Expected: File is tracked (no error)

# Check if release notes are committed
git ls-files --error-unmatch docs/releases/RELEASE_NOTES_v1.2.0.md 2>&1
# Expected: File is tracked (no error)

# Verify artifacts are not in git status
git status --porcelain | grep -E "CHANGELOG|RELEASE_NOTES"
# Expected: No matches (artifacts are committed)
```

**Artifact commit status**: ✅ All release artifacts are committed

#### 4.4: Verify Documentation Consistency

**Action**: Verify all documentation is accurate.

**Action**: Check documentation references

```bash
# Verify README is accurate
grep -i "version\|1\.1\|1\.0" README.md
# Check for version references

# Verify readiness reports are accurate
grep -i "ready\|complete\|production" docs/V1_1_0_READINESS_REPORT.md
# Should show code is ready

# Verify no outdated claims
grep -i "placeholder\|incomplete\|todo" docs/V1_1_0_READINESS_REPORT.md
# Should show no outdated claims
```

**Documentation status**: ✅ Reports show code is ready; verify final consistency

**Action**: Verify documentation links

```bash
# Verify all markdown links in README/docs point to valid files
grep -oE '\[.*?\]\([^)]+\)' README.md docs/*.md 2>/dev/null | sed 's/.*(\(.*\))/\1/' | while read link; do
  if [[ "$link" =~ ^http ]]; then continue; fi  # Skip external links
  if [[ ! -f "$link" ]] && [[ ! -d "$link" ]]; then
    echo "❌ Broken link: $link"
  fi
done
# Expected: No broken links
```

**Link validation status**: ✅ All links valid (or broken links documented)

---

### Step 5: Final Validation

**Action**: Final validation that v1.2.0 release is ready.

#### 5.1: Pre-Release Checklist

**Action**: Verify all release checklist items are complete.

**v1.2.0 Release Checklist**:

**Code**:

- [x] All code compiles (`pnpm lint`) ✅
- ✅ All tests pass (`pnpm test`) ✅ (328/328, 100%)
- [x] Linting passes (`pnpm lint`) ✅
- [x] No TODOs or FIXMEs in production code ✅
- [x] No `unimplemented!` calls ✅
- [x] All error paths handled ✅
- [ ] Examples work (`pnpm test --examples`) ⚠️ Verify
- [ ] Coverage meets 85% threshold ⚠️ Verify (`pnpm llvm-cov --all-features`)

**Git State**:

- [ ] Git state is clean (`git status --porcelain` returns no output) ⚠️ CRITICAL BLOCKER
- [ ] No uncommitted modified files ⚠️ CRITICAL BLOCKER
- [ ] No untracked files (except build artifacts) ⚠️ CRITICAL BLOCKER
- [ ] No incomplete work files (`.new`, `WIP`, `.tmp`) ⚠️ CRITICAL BLOCKER
- [ ] All changes committed or stashed ⚠️ CRITICAL BLOCKER

**Version**:

- [ ] Version set to 1.2.0 in `package.json` ⚠️ Verify
- [ ] Version consistent in `package.json` ⚠️ Verify
- [x] No hardcoded old versions in code ✅
- [ ] Version referenced in documentation ⚠️ Verify

**Documentation**:

- [x] README updated ✅
- [x] API documentation complete ✅
- [x] User guides updated ✅
- [x] Examples documented ✅
- [ ] v1.2.0 section added to `docs/releases/CHANGELOG.md` ⚠️ Verify
- [ ] v1.2.0 release notes created ⚠️ Verify
- [ ] Documentation consistency verified ⚠️ Run `pnpm docs-check`

**Dependencies**:

- [x] Dependencies stable and compatible ✅
- [x] No known security vulnerabilities ✅
- [x] License compatibility verified ✅

**Final Validation**:

- [ ] Git state clean (CRITICAL BLOCKER) ⚠️ Verify first (`pnpm make release-validate-git-state`)
- [ ] Version consistent (1.2.0) ⚠️ Verify (`pnpm make release-validate-version`)
- [ ] Release artifacts exist ⚠️ Verify (`pnpm make release-validate-artifacts`)
- [ ] Clean build successful ⚠️ Verify
- [x] Full test suite passes ✅ (328/328, 100%)
- [ ] Coverage meets 85% threshold ⚠️ Verify (`pnpm llvm-cov --all-features`)
- [ ] All features tested with feature flags enabled ⚠️ Verify (`pnpm test --all-features`)
- [ ] Examples work (`pnpm test --examples`) ⚠️ Verify
- [ ] Documentation builds ⚠️ Verify
- [ ] Documentation links validated ⚠️ Verify
- [ ] Dead code check passed ⚠️ Verify (`pnpm lint`)
- [ ] All checklist items complete ⚠️ In progress

**Action**: Run final validation

```bash
# 0. Git state verification (CRITICAL - CHECK FIRST)
git status --porcelain
# Expected: No output (clean state)
# If output exists: NOT READY FOR RELEASE - STOP HERE

# Check for incomplete work files
find . -name "*.new" -o -name "*WIP*" -o -name "*.tmp" | grep -v "target\|node_modules\|\.git"
# Expected: No matches (no incomplete work)

# 1. Clean build
timeout 5s pnpm clean
timeout 10s pnpm lint

# 2. Full test suite
timeout 10s pnpm test
# Expected: 328 passed, 0 timed out, 11 skipped

# 3. Lint
timeout 10s pnpm lint

# 4. Verify examples
timeout 10s pnpm test --examples

# 5. Test all feature combinations
timeout 10s pnpm test --all-features
# Expected: All tests pass with all features

# 6. Verify examples
timeout 10s pnpm test --examples
# Expected: All examples work

# 7. Verify documentation
timeout 10s pnpm doc --no-deps
# Expected: Documentation builds successfully

# 8. Verify documentation links
grep -oE '\[.*?\]\([^)]+\)' README.md docs/*.md 2>/dev/null | sed 's/.*(\(.*\))/\1/' | while read link; do
  if [[ "$link" =~ ^http ]]; then continue; fi
  if [[ ! -f "$link" ]] && [[ ! -d "$link" ]]; then
    echo "❌ Broken link: $link"
  fi
done
# Expected: No broken links
```

#### 5.2: Create Todo List for Release Blockers

**CRITICAL**: Do NOT create reports or documents. Create todos and execute them.

**Action**: Create 10+ item todo list for all release blockers and execute fixes.

**Todo list creation**:

1. Identify all release blockers from Step 3 (Analyze Gaps)
2. Create todos for each blocker (minimum 10 items)
3. Prioritize by severity (blockers first)
4. Include verification steps in todos
5. Execute todos systematically

**Example todo list**:

```markdown
## Release Preparation Todos (10+ items)

**Blockers (Must Fix Before Release)**:

- [ ] Git state is clean (no uncommitted changes, no WIP work)
- [ ] Update version to 1.2.0 in package.json and package.json
- [ ] Add v1.2.0 section to docs/releases/CHANGELOG.md
- [ ] Create docs/releases/RELEASE_NOTES_v1.2.0.md
- [ ] Verify git status is clean
- [ ] Verify v1.2.0 section in CHANGELOG exists and is complete
- [ ] Verify release notes exist and are complete

**High Priority**:

- [ ] Verify all documentation references are accurate
- [ ] Verify version consistency in all documentation
- [ ] Verify all examples work with v1.2.0
- [ ] Verify coverage meets 85% threshold

**Final Validation**:

- [ ] Clean build successful
- [ ] Full test suite passes (328/328, 100%)
- [ ] Coverage meets 85% threshold
- [ ] Documentation builds successfully
- [ ] Examples work correctly
- [ ] All checklist items complete
```

**Execution**:

1. Create todos using `todo_write` tool (10+ items minimum)
2. Execute todos one by one (fix blockers)
3. Mark todos as completed as fixes are implemented
4. Verify each fix works before moving to next
5. Continue until all blockers resolved

**Principle**: Execute fixes, don't document readiness. Todos track progress, fixes enable release.

#### 5.3: Final Verification

**Action**: Run final verification before release.

**Final verification steps**:

```bash
# 0. Comprehensive release validation (all FMEA failure mode checks) - AUTOMATED
pnpm make release-validate
# Expected: All validation checks pass
# Includes: git state, artifacts, version, compilation (release mode), examples, pre-commit, security, testcontainers
# If fails: NOT READY FOR RELEASE - STOP HERE

# 1. Full test suite
timeout 10s pnpm test
# Expected: 328 passed, 0 timed out, 11 skipped

# 2. Documentation build
timeout 10s pnpm doc --no-deps
# Expected: Builds successfully
```

**Note**: The `release-validate` task automatically checks:

- ✅ Git state is clean (no uncommitted changes, no WIP files) - **CRITICAL BLOCKER**
- ✅ Release artifacts exist (CHANGELOG.md, release notes)
- ✅ Version consistency (all package.json files match)
- ✅ Release mode compilation (code compiles in release mode)
- ✅ Examples compile
- ✅ Pre-commit checks pass
- ✅ Security audit (if available)
- ✅ Testcontainers tests (if Docker available)

**Success criteria**:

- ✅ All steps complete without errors
- ✅ Tests pass (328/328, 100%)
- ✅ Coverage meets 85% threshold
- ✅ No critical warnings
- ✅ Documentation builds successfully
- ✅ Examples work
- ✅ v1.2.0 section in `docs/releases/CHANGELOG.md` exists
- ✅ v1.2.0 release notes exist
- ✅ **Git state is clean (CRITICAL BLOCKER)** - no uncommitted changes, no WIP work

---

## Complete Workflow Example

```bash
# Step 1: Verify Release Scope
grep "^version" package.json
# Current: "version": "1.1.2"
# Expected: "version": "1.2.0" ⚠️ Needs update

# Step 2: Measure Current State
timeout 10s pnpm test
# Output: 328 passed, 0 timed out, 11 skipped ✅

grep -r "TODO\|FIXME" src/ --include="*.mjs"
# Output: No matches ✅

test -f docs/releases/CHANGELOG.md || echo "Missing"
# Output: File exists ✅ (needs v1.2.0 section)

# Step 3: Analyze Gaps
# Blockers: Version update to 1.2.0, v1.2.0 section in CHANGELOG, v1.2.0 release notes
# Coverage: Verify 85% threshold met

# Step 4: Prepare Release Artifacts
# Update version to 1.2.0 in package.json and package.json
# Add v1.2.0 section to docs/releases/CHANGELOG.md
# Create docs/releases/RELEASE_NOTES_v1.2.0.md

# Step 5: Final Validation
timeout 10s pnpm lint  # ✅
timeout 10s pnpm test   # ✅ (328/328, 100%)
pnpm llvm-cov --all-features --summary-only  # ✅ Coverage ≥ 85%
grep "^## \[1.2.0\]" docs/releases/CHANGELOG.md  # ✅ v1.2.0 section exists
test -f docs/releases/RELEASE_NOTES_v1.2.0.md  # ✅ Release notes exist
git status --porcelain         # ✅ Must be clean (no output)
find . -name "*.new" | grep -v "target\|\.git"  # ✅ No incomplete work
```

## Integration with Other Commands

- **[DMAIC Problem Solving](./dmaic-problem-solving.md)** - Use for systematic gap fixing
- **[Root Cause Analysis](./root-cause-analysis.md)** - Use to identify root causes of blockers
- **[80/20 Fill Gaps](./80-20-fill-gaps.md)** - Use to prioritize and complete missing capabilities
- **[Verify Tests](./verify-tests.md)** - Use to ensure all tests pass
- **[Kaizen Improvement](./kaizen-improvement.md)** - Use for small improvements during release prep

## Expert Insights

**Why this matters**: Incomplete releases damage user trust. Systematic release preparation ensures quality and reduces post-release issues.

**Key principle**: "Release when ready, not when scheduled" - Don't release with known blockers. Better to delay than to release broken code.

**80/20 thinking**: Focus on the 20% of gaps (CHANGELOG, release notes) that block 80% of release readiness. Fix blockers first.

**Current state**: Code is ready (328/328 tests pass, 100%). Release artifacts need v1.2.0 updates (CHANGELOG section, release notes).

**Remember**:

- **Git state first** - Clean git state is a CRITICAL BLOCKER (no uncommitted changes, no WIP work)
- **Blockers first** - Update version to 1.2.0, add CHANGELOG section, create release notes before release
- **Verify everything** - Don't assume, verify
- **Document changes** - CHANGELOG and release notes are critical
- ✅ Test thoroughly - 328/328 tests pass (100%)
- ✅ Coverage threshold - Verify 85% coverage met
- **Version consistently** - Update version to 1.2.0 in package.json and package.json

**Release readiness criteria**: Git state clean (CRITICAL), code compiles, tests pass (328/328, 100%), coverage ≥ 85%, docs complete, version correct (1.2.0), v1.2.0 CHANGELOG section and release notes created. Only release when all criteria met, including clean git state.

**DfLSS alignment**: Release preparation supports DfLSS (Design for Lean Six Sigma) by ensuring both efficiency (no rework from incomplete releases) AND quality (thorough testing prevents defects). Don't conflate DfLSS with DFSS (Design for Six Sigma) - DFSS only addresses quality, missing critical waste elimination. See [Root Cause Analysis - DfLSS vs DFSS](./root-cause-analysis.md#dflss-vs-dfss-critical-distinction) for why conflating DfLSS with DFSS is a huge error.

---

## Command Execution Pattern

**CRITICAL**: Release preparation commands must:

1. **Create 10+ item todo list** - Not documents/reports
2. **Execute todos** - Implement fixes, not document readiness
3. **Verify fixes** - Test that fixes work
4. **Complete todos** - Mark todos as done as fixes complete

**Principle**: Execute fixes, don't document readiness. Todos track progress, fixes enable release.

---

## v1.2.0 Release Checklist

```markdown
# v1.2.0 Release Checklist

## Code

- [x] All code compiles (`pnpm lint`) ✅
- ✅ All tests pass (`pnpm test`) ✅ (328/328, 100%)
- [x] Linting passes (`pnpm lint`) ✅
- [x] No TODOs or FIXMEs in production code ✅
- [x] No `unimplemented!` calls ✅
- [x] All error paths handled ✅
- [ ] Examples work (`pnpm test --examples`) ⚠️ Verify
- [ ] Coverage meets 85% threshold ⚠️ Verify (`pnpm llvm-cov --all-features`)

## Git State (CRITICAL BLOCKER)

- [ ] Git state is clean (`git status --porcelain` returns no output) ⚠️ CRITICAL BLOCKER
- [ ] No uncommitted modified files ⚠️ CRITICAL BLOCKER
- [ ] No untracked files (except build artifacts) ⚠️ CRITICAL BLOCKER
- [ ] No incomplete work files (`.new`, `WIP`, `.tmp`) ⚠️ CRITICAL BLOCKER
- [ ] All changes committed or stashed ⚠️ CRITICAL BLOCKER

## Version

- [ ] Version set to 1.2.0 in `package.json` ⚠️ Verify
- [ ] Version consistent in `package.json` ⚠️ Verify
- [x] No hardcoded old versions in code ✅

## Documentation

- [x] README updated ✅
- [x] API documentation complete ✅
- [x] User guides updated ✅
- [x] Examples documented ✅
- [ ] v1.2.0 section added to `docs/releases/CHANGELOG.md` ⚠️ Verify
- [ ] v1.2.0 release notes created ⚠️ Verify
- [ ] Documentation consistency verified ⚠️ Run `pnpm docs-check`

## Dependencies

- [x] Dependencies stable and compatible ✅
- [x] No known security vulnerabilities ✅
- [x] License compatibility verified ✅

## Final Validation

- [ ] Clean build successful ⚠️ Verify
- ✅ Full test suite passes ✅ (328/328, 100%)
- [ ] Coverage meets 85% threshold ⚠️ Verify
- [ ] Documentation builds ⚠️ Verify
- [ ] All checklist items complete ⚠️ In progress

## Release Status

- [ ] ✅ READY FOR RELEASE (after version update, CHANGELOG section, and release notes created)
- [ ] ⚠️ NOT READY - Reason: \***\*\_\_\_\*\***
```

---

End Command ---
