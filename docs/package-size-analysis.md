# UNRDF v3.0.0 Package Size Analysis

**Total Package Size:** 9.7 MB unpacked / 2.5 MB tarball
**Analysis Date:** October 2, 2025

---

## 📊 Size Breakdown by Directory

| Directory | Size | % of Total | Should Include? |
|-----------|------|------------|-----------------|
| `docs/` | 4.6 MB | **47%** | ❌ NO |
| `dist/` | 2.4 MB | 25% | ✅ YES |
| `examples/` | 552 KB | 6% | ⚠️ MAYBE |
| `test/` | 408 KB | 4% | ❌ NO |
| Root `*.md` | ~1.2 MB | 12% | ⚠️ SELECTIVE |
| `terraform/` | 64 KB | <1% | ❌ NO |
| `src/` | ~500 KB | 5% | ✅ YES |

---

## 🔴 Critical Issues

### 1. **Documentation Directory (4.6 MB - 47% of package)**

**Problem:** The entire `docs/` directory is included, containing:
- Internal planning documents
- Architecture research
- Test logs and validation reports
- Development conversation logs
- Multiple versions of similar docs

**Top Offenders:**
```
928 KB - KGC-CONVO.md (development conversation log)
907 KB - Root KGC-CONVO.md
745 KB - dist/index.mjs.map (source map)
 86 KB - docs/test-results.log
 68 KB - docs/papers/knowledge-geometry-calculus-arxiv.md
 50 KB - docs/validation-full.log
 45 KB - docs/validation/gate-results.json
```

**Should Include:**
- API reference
- User guides
- Migration guides
- Examples documentation

**Should EXCLUDE:**
- Internal planning (`UNRDF-PLANNING.md`, `OPERATIONAL-RUNBOOK.md`)
- Development logs (`KGC-CONVO.md`, `test-results.log`)
- Architecture research (sidecar, nuxt4, lean-six-sigma)
- Test validation reports
- Conversation transcripts

### 2. **Test Directory (408 KB)**

**Problem:** Production users don't need test files.

**Contents:**
- `test/knowledge-engine/` - Unit tests
- `test/e2e/` - E2E infrastructure
- `test/performance/` - Benchmarks
- `test/cli-v2/` - CLI tests (CLI removed in v3!)

**Recommendation:** ❌ REMOVE from `files` array

### 3. **Root Markdown Files (~1.2 MB)**

**Problem:** Wildcard `*.md` includes ALL markdown files in root.

**Large Files:**
```
907 KB - KGC-CONVO.md (internal conversation)
 36 KB - PREV-CODE.md (archived code)
 33 KB - UNRDF-PLANNING.md (internal planning)
 29 KB - OPERATIONAL-RUNBOOK.md (internal)
 21 KB - NITRO-PLAYGROUND.md (internal)
 18 KB - KGC-SIDECAR-*.md (sidecar removed in v3!)
 17 KB - PRODUCTION-SIGN-OFF.md (internal)
 15 KB - ACCEPTANCE-SIGN-OFF.md (internal)
```

**Should Include:**
- `README.md` (18 KB)
- `CHANGELOG.md` (if exists)
- `LICENSE` (1 KB)
- `CONTRIBUTING.md` (11 KB)

**Should EXCLUDE:**
- Internal planning docs
- Conversation logs
- Sign-off documents
- Sidecar docs (removed in v3)
- Development notes

### 4. **Examples Directory (552 KB)**

**Problem:** Some examples reference removed features (CLI, sidecar).

**Review Needed:**
- `examples/legacy-cli/` - CLI removed in v3
- `examples/sidecar-client-example.mjs` - Sidecar removed in v3
- `examples/git-knowledge-hooks-integration.mjs` - May be useful

**Recommendation:** ⚠️ Curate to production-relevant examples only

### 5. **Terraform Directory (64 KB)**

**Problem:** Infrastructure-as-code for deployment, not library code.

**Recommendation:** ❌ REMOVE - users deploying UNRDF can reference GitHub repo

### 6. **Source Maps (745 KB)**

**Problem:** `dist/*.mjs.map` files add significant size.

**Impact:**
- `dist/index.mjs.map` - 745 KB
- `dist/cli.mjs.map` - 185 KB
- `dist/utils/index.mjs.map` - 192 KB
- Total source maps: ~1.1 MB

**Recommendation:** ⚠️ Keep for debugging, but consider `.npmignore` or separate sourcemap package

---

## ✅ Recommended package.json Changes

### Current (9.7 MB):
```json
"files": [
  "src/",
  "dist/",
  "docs/",
  "examples/",
  "test/",
  "terraform/",
  "k8s/",
  "*.md",
  "*.json",
  "*.mjs",
  "*.yml",
  "*.yaml"
]
```

### Recommended (Target: ~3-4 MB):
```json
"files": [
  "src/",
  "dist/",
  "README.md",
  "LICENSE",
  "CHANGELOG.md",
  "CONTRIBUTING.md",
  "docs/api/",
  "docs/guides/",
  "docs/examples/",
  "docs/migration-v2-to-v3.md",
  "docs/v3.0.0-RELEASE-NOTES.md",
  "examples/*.mjs",
  "!examples/legacy-cli/",
  "!examples/sidecar-*.mjs"
]
```

---

## 📉 Expected Size Reduction

| Item | Current | After Cleanup | Savings |
|------|---------|---------------|---------|
| `docs/` (internal) | 4.6 MB | 500 KB | **-4.1 MB** |
| `test/` | 408 KB | 0 KB | **-408 KB** |
| Root `*.md` (curated) | 1.2 MB | 50 KB | **-1.15 MB** |
| `terraform/` | 64 KB | 0 KB | **-64 KB** |
| `examples/` (curated) | 552 KB | 300 KB | **-252 KB** |
| **TOTAL SAVINGS** | | | **-5.97 MB** |

**New Package Size:** ~3.7 MB unpacked (~1.2 MB tarball)
**Reduction:** **62% smaller**

---

## 🎯 Action Plan

### Priority 1: Remove Internal Documentation (saves 4.1 MB)
- Remove entire `docs/` from `files` array
- Add back only user-facing docs: `docs/api/`, `docs/guides/`, `docs/examples/`

### Priority 2: Remove Test Files (saves 408 KB)
- Remove `test/` from `files` array
- Tests available in GitHub repo for contributors

### Priority 3: Curate Root Markdown (saves 1.15 MB)
- Replace `*.md` wildcard with explicit list
- Keep only: `README.md`, `LICENSE`, `CHANGELOG.md`, `CONTRIBUTING.md`

### Priority 4: Remove Infrastructure (saves 64 KB)
- Remove `terraform/` and `k8s/` from `files` array
- Users can reference GitHub repo for deployment examples

### Priority 5: Curate Examples (saves 252 KB)
- Remove `examples/legacy-cli/` (CLI removed in v3)
- Remove sidecar examples (sidecar removed in v3)
- Keep production-relevant examples only

---

## 💡 Additional Optimizations

### Consider for v3.1.0:
1. **Separate sourcemaps package** (`@unrdf/sourcemaps`)
   - Saves ~1.1 MB
   - Users can opt-in for debugging

2. **Separate examples package** (`@unrdf/examples`)
   - Saves ~300 KB
   - GitHub repo has full examples

3. **Tree-shakeable exports**
   - Review `dist/` bundle structure
   - Ensure unused code can be eliminated

4. **.npmignore file**
   - More granular control than `files` array
   - Easier to maintain exclusions

---

## 📋 Implementation Checklist

- [ ] Update `package.json` `files` array
- [ ] Test with `npm pack --dry-run`
- [ ] Verify package contents
- [ ] Test installation in clean project
- [ ] Publish as v3.0.1 (patch release)
- [ ] Update documentation about package contents

---

## 🚨 Why This Matters

**Current Issues:**
- Slow installation times (9.7 MB vs typical 1-2 MB)
- Increased bandwidth costs for users
- Internal docs exposed publicly
- Confusion from outdated examples (CLI, sidecar)
- Larger Docker images
- CI/CD cache inefficiency

**Benefits of Cleanup:**
- **62% smaller package** (9.7 MB → 3.7 MB)
- Faster `npm install` times
- Better user experience
- Professional package hygiene
- Reduced npm registry costs

---

## 📚 References

**npm Best Practices:**
- Include only runtime-necessary files
- Documentation should be concise and user-focused
- Tests belong in GitHub repo, not npm package
- Infrastructure configs are not library code

**Similar Packages:**
- `n3` - 1.2 MB unpacked
- `comunica` - 2.1 MB unpacked
- `rdf-validate-shacl` - 800 KB unpacked

**Target:** UNRDF should be ~3-4 MB (similar to Comunica)
