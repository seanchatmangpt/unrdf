# YAWL to HWFL Rename - Manual Review Checklist

After running `./rename-yawl.sh`, these files require manual review to ensure:
1. Academic citations to van der Aalst's original YAWL are preserved
2. Context-specific references are handled correctly
3. No semantic meaning is lost

---

## HIGH PRIORITY: Academic Citations to Preserve

These files contain references to van der Aalst's original work that must NOT be renamed:

### Thesis Documents
| File | Review Focus |
|------|-------------|
| `docs/PHD-THESIS-UNRDF-2028-REVOLUTION-FINAL.md` | Preserve all academic citations to van der Aalst (2005) |
| `docs/THESIS-BEYOND-HUMAN-PERCEPTION-FINAL.md` | Keep "Van der Aalst's YAWL patterns" references |
| `packages/hwfl/ARCHITECTURAL-ANALYSIS.md` | Preserve comparison with original YAWL semantics |
| `packages/hwfl/THESIS-CONTRIBUTIONS.md` | Keep prior art references |

### Expected Academic Citation Format (DO NOT CHANGE)
```
van der Aalst, W.M.P., ter Hofstede, A.H.M. (2005). YAWL: Yet Another Workflow Language.
Information Systems, 30(4), 245-275.
```

---

## MEDIUM PRIORITY: Semantic Context Review

### Files with "Van der Aalst's YAWL" patterns
These should keep the original YAWL name when referring to academic concepts:

| File | Check For |
|------|-----------|
| `packages/hwfl/src/engine.mjs` | Line ~14: "Van der Aalst's YAWL workflow patterns" |
| `packages/hwfl/src/ontology/hwfl-ontology.mjs` | Line ~5: YAWL ontology description |
| `packages/hwfl/src/patterns.mjs` | References to YAWL pattern numbers (WP-1, WP-2, etc.) |

**Rule**: When referring to the academic concept from 2005, keep "YAWL". When referring to this package's implementation, use "HWFL".

---

## LOW PRIORITY: Optional Cleanup

### Comments that may reference original inspiration
Check and optionally update docstrings in:

| File | Line/Section |
|------|-------------|
| `packages/hwfl/src/index.mjs` | Module documentation header |
| `packages/hwfl/src/workflow.mjs` | Class documentation |
| `packages/hwfl/src/case.mjs` | Implementation notes |

---

## Verification Commands

After rename, run these to check for issues:

```bash
# 1. Check for remaining old imports
grep -r "from '@unrdf/yawl" packages/ --include="*.mjs"

# 2. Check for remaining old namespace URIs
grep -r "http://yawl.io" packages/ --include="*.mjs"
grep -r "http://unrdf.org/yawl" packages/ --include="*.mjs"

# 3. Check academic citations are preserved
grep -r "van der Aalst" packages/ docs/

# 4. Run tests to verify functionality
pnpm --filter @unrdf/hwfl test

# 5. Check for broken internal imports
grep -r "yawl-" packages/hwfl --include="*.mjs" | grep "from\|import"
```

---

## Expected Grep Results After Rename

### Should find 0 results:
```bash
grep -r "@unrdf/yawl" packages/hwfl/   # Package references
grep -r "YawlEngine" packages/hwfl/    # Class names (except backward compat aliases)
grep -r "createYawlStore" packages/hwfl/ # Factory functions
```

### Should find results (academic citations):
```bash
grep -r "Van der Aalst" packages/hwfl/  # Academic references
grep -r "YAWL workflow patterns" packages/hwfl/  # Conceptual references
```

---

## Sign-off Checklist

- [ ] All tests pass: `pnpm --filter @unrdf/hwfl test`
- [ ] Academic citations to van der Aalst (2005) preserved
- [ ] No broken imports in source files
- [ ] Namespace URIs correctly updated to hwfl
- [ ] SPARQL prefixes updated
- [ ] Package.json correctly updated
- [ ] pnpm install succeeds
- [ ] Git history preserved for renamed files

---

## Rollback Command

If issues are found:
```bash
git checkout backup-before-hwfl-rename -- packages/
git checkout backup-before-hwfl-rename -- pnpm-lock.yaml
pnpm install
```
