# Fix Recommendations - Priority Order

**Agent**: Agent-7 (code-analyzer)
**Date**: 2025-12-27
**Total Technical Debt**: ~64 hours

---

## Priority 1: Critical (Block Merge)

### 1.1 Fix N3 Import Violations

**Impact**: CRITICAL
**Effort**: 16 hours
**Files Affected**: 69 files

The CLAUDE.md explicitly states: "NEVER import `from 'n3'` in app code (oxigraph only)"

**Action Plan**:

1. Create migration script:
```javascript
// scripts/migrate-n3-imports.mjs
import { glob } from 'glob';
import { readFile, writeFile } from 'fs/promises';

const N3_IMPORTS = /import\s+\{([^}]+)\}\s+from\s+['"]n3['"]/g;

async function migrate() {
  const files = await glob('src/**/*.mjs');
  for (const file of files) {
    let content = await readFile(file, 'utf-8');
    if (N3_IMPORTS.test(content)) {
      console.log(`Migrating: ${file}`);
      // Replace with oxigraph imports
    }
  }
}
```

2. Migration mapping:
   - `Store` -> `createStore()` from `@unrdf/oxigraph`
   - `DataFactory` -> `dataFactory` from `@unrdf/oxigraph`
   - `Parser`, `Writer` -> Only via `n3-justified-only.mjs`

3. Files to migrate (priority order):
   - src/composables/* (8 files)
   - src/context/index.mjs
   - src/engines/rdf-engine.mjs
   - src/knowledge-engine/* (15 files)
   - src/project-engine/* (12 files)
   - src/utils/* (7 files)
   - src/react-hooks/* (8 files)

### 1.2 Install Dependencies

**Impact**: BLOCKING (ESLint cannot run)
**Effort**: 5 minutes

```bash
cd /home/user/unrdf
pnpm install --frozen-lockfile
```

If frozen lockfile fails:
```bash
pnpm install
```

---

## Priority 2: High (Fix This Sprint)

### 2.1 Update esbuild Security Vulnerability

**Impact**: MODERATE (3 vulnerabilities)
**Effort**: 30 minutes

```bash
pnpm update esbuild@latest -r
pnpm update vite@latest -C packages/cli
pnpm update drizzle-kit@latest -C packages/docs
```

### 2.2 Run ESLint and Fix Violations

**Impact**: HIGH
**Effort**: 2-4 hours (after dependencies installed)

```bash
# Run lint
pnpm run lint

# Auto-fix what can be fixed
pnpm run lint -- --fix

# Check remaining issues
pnpm run lint 2>&1 | tee lint-output.txt
```

---

## Priority 3: Medium (Next Sprint)

### 3.1 Replace console.log with Logger

**Impact**: MODERATE
**Effort**: 8 hours
**Files**: 162 files, 1,313 occurrences

**Strategy**:
1. Create logger wrapper if not exists
2. Use search-and-replace with context awareness
3. Keep console.* in test files (acceptable)

```bash
# Find all console usage
grep -rn "console\." src/ --include="*.mjs" | grep -v ".test.mjs" > console-usage.txt

# Files with most console usage:
# src/verification.mjs (84)
# src/cli/commands/autonomic.mjs (72)
# src/receipts/examples.mjs (66)
```

### 3.2 Split Large Files

**Impact**: MODERATE
**Effort**: 16 hours
**Files**: 20+ files over 500 LoC

**Priority files to split**:

| File | Lines | Suggested Split |
|------|-------|-----------------|
| receipt-standard.mjs | 1,148 | Split by receipt type |
| schemas.mjs | 1,063 | Split by schema category |
| query-optimizer.mjs | 1,052 | Extract cache, index, delta modules |
| monorepo-universe.mjs | 1,019 | Extract query and partition logic |
| capacity-computer.mjs | 988 | Extract metrics and computation |

---

## Priority 4: Low (Backlog)

### 4.1 Reduce Nesting Depth

**Impact**: LOW (code smell)
**Effort**: 24 hours
**Files**: 242 files

**Refactoring patterns to apply**:

1. **Early returns**:
```javascript
// Before
if (condition) {
  if (nestedCondition) {
    // deep code
  }
}

// After
if (!condition) return;
if (!nestedCondition) return;
// flat code
```

2. **Guard clauses**:
```javascript
// Before
function process(data) {
  if (data) {
    if (data.isValid) {
      // process
    }
  }
}

// After
function process(data) {
  if (!data) return;
  if (!data.isValid) return;
  // process
}
```

3. **Extract functions**:
```javascript
// Before
if (complexCondition) {
  // 20 lines of code
}

// After
if (complexCondition) {
  handleComplexCase();
}
```

---

## Automated Fix Script

```bash
#!/bin/bash
# fix-quality-issues.sh

set -e

echo "=== UNRDF Code Quality Fix Script ==="
echo "Date: $(date)"
echo ""

# Step 1: Install dependencies
echo "Step 1: Installing dependencies..."
pnpm install

# Step 2: Run ESLint with auto-fix
echo "Step 2: Running ESLint with auto-fix..."
pnpm run lint -- --fix || true

# Step 3: Format with Prettier
echo "Step 3: Running Prettier..."
pnpm run format || true

# Step 4: Security audit
echo "Step 4: Running security audit..."
pnpm audit || true

# Step 5: Run tests
echo "Step 5: Running tests..."
timeout 120s pnpm test || echo "Tests need attention"

echo ""
echo "=== Fix Script Complete ==="
echo "Review the output above for any remaining issues."
```

---

## Verification Checklist

After applying fixes, verify:

- [ ] `pnpm run lint` shows 0 errors
- [ ] `pnpm audit` shows 0 vulnerabilities
- [ ] `grep -r "from 'n3'" src/ --include="*.mjs" | wc -l` returns 0
- [ ] `pnpm test` passes all tests
- [ ] No new console.log added outside logger

---

## Technical Debt Tracking

| Issue | Hours | Priority | Status |
|-------|-------|----------|--------|
| N3 migration | 16 | Critical | TODO |
| ESLint setup | 0.5 | Critical | TODO |
| esbuild update | 0.5 | High | TODO |
| ESLint violations | 4 | High | TODO |
| Console replacement | 8 | Medium | TODO |
| File splitting | 16 | Medium | TODO |
| Nesting reduction | 24 | Low | TODO |
| **Total** | **69** | - | - |

---

*Generated by Agent-7 (code-analyzer)*
