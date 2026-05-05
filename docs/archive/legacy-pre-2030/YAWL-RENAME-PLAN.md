# YAWL to HWFL Rename Plan

**Purpose**: Rename the YAWL package to avoid prior art collision with van der Aalst and ter Hofstede's "YAWL: Yet Another Workflow Language" (2005).

**New Name**: **HWFL (Hook Workflow Language)**

**Rationale**:
- Distinguishes clearly from van der Aalst's YAWL (prior art from 2005)
- Describes the key differentiator: hook-native workflow architecture
- Follows the same 4-letter acronym pattern
- Aligns with thesis emphasis on hooks as the primary innovation

---

## 1. Scope Summary

| Category | Count | Description |
|----------|-------|-------------|
| Directory | 1 | `packages/yawl/` -> `packages/hwfl/` |
| Package name | 1 | `@unrdf/yawl` -> `@unrdf/hwfl` |
| Source files | 29 | All .mjs files in package |
| Files to rename | 13 | Files with "yawl" in name |
| Thesis/docs | ~25 | Documentation references |
| Namespace URIs | 5 | All RDF namespace URIs |
| Class names | 10+ | All Yawl-prefixed classes |

---

## 2. Directory Rename (Git History Preserved)

```bash
git mv packages/yawl packages/hwfl
```

---

## 3. File Renames Within Package

### Source Files (packages/hwfl/src/)
| Old Path | New Path |
|----------|----------|
| `cancellation/yawl-cancellation.mjs` | `cancellation/hwfl-cancellation.mjs` |
| `events/yawl-events.mjs` | `events/hwfl-events.mjs` |
| `hooks/yawl-hooks.mjs` | `hooks/hwfl-hooks.mjs` |
| `ontology/yawl-ontology.mjs` | `ontology/hwfl-ontology.mjs` |
| `resources/yawl-resources.mjs` | `resources/hwfl-resources.mjs` |
| `store/yawl-store.mjs` | `store/hwfl-store.mjs` |
| `types/yawl-schemas.mjs` | `types/hwfl-schemas.mjs` |
| `types/yawl-types.mjs` | `types/hwfl-types.mjs` |

### Test Files (packages/hwfl/test/)
| Old Path | New Path |
|----------|----------|
| `yawl.test.mjs` | `hwfl.test.mjs` |
| `yawl-events.test.mjs` | `hwfl-events.test.mjs` |
| `yawl-hooks.test.mjs` | `hwfl-hooks.test.mjs` |
| `yawl-patterns.test.mjs` | `hwfl-patterns.test.mjs` |
| `yawl-resources.test.mjs` | `hwfl-resources.test.mjs` |

---

## 4. Namespace URI Changes

| Old URI | New URI |
|---------|---------|
| `http://yawl.io/` | `http://hwfl.io/` |
| `http://unrdf.org/yawl#` | `http://unrdf.org/hwfl#` |
| `http://unrdf.org/yawl/case#` | `http://unrdf.org/hwfl/case#` |
| `http://unrdf.org/yawl/task#` | `http://unrdf.org/hwfl/task#` |
| `http://unrdf.org/yawl/workitem#` | `http://unrdf.org/hwfl/workitem#` |

---

## 5. Class/Constant Name Changes

### Classes (maintain backward compat aliases)
| Old Name | New Name | Backward Compat Alias |
|----------|----------|-----------------------|
| `YawlEngine` | `HwflEngine` | `YawlEngine = HwflEngine` (deprecated) |
| `YawlWorkflow` | `HwflWorkflow` | `YawlWorkflow = HwflWorkflow` (deprecated) |
| `YawlCase` | `HwflCase` | `YawlCase = HwflCase` (deprecated) |
| `YawlTask` | `HwflTask` | `YawlTask = HwflTask` (deprecated) |
| `YawlReceipt` | `HwflReceipt` | `YawlReceipt = HwflReceipt` (deprecated) |
| `YawlResourcePool` | `HwflResourcePool` | `YawlResourcePool = HwflResourcePool` (deprecated) |
| `YawlResourceManager` | `HwflResourceManager` | `YawlResourceManager = HwflResourceManager` (deprecated) |

### Factory Functions
| Old Name | New Name |
|----------|----------|
| `createYAWLPolicyPack` | `createHWFLPolicyPack` |
| `createYawlStore` | `createHwflStore` |

### Constants
| Old Name | New Name |
|----------|----------|
| `YAWL` | `HWFL` |
| `YAWL_NS` | `HWFL_NS` |
| `YAWL_CASE` | `HWFL_CASE` |
| `YAWL_TASK` | `HWFL_TASK` |
| `YAWL_WORK` | `HWFL_WORK` |
| `YAWL_GRAPHS` | `HWFL_GRAPHS` |
| `YAWL_EVENT_TYPES` | `HWFL_EVENT_TYPES` |
| `YAWL_PREDICATES` | `HWFL_PREDICATES` |
| `YAWL_PREFIXES` | `HWFL_PREFIXES` |
| `YAWL_RESOURCE_NS` | `HWFL_RESOURCE_NS` |

### Schemas
| Old Name | New Name |
|----------|----------|
| `YAWLWorkflowSchema` | `HWFLWorkflowSchema` |
| `YAWLTaskSchema` | `HWFLTaskSchema` |
| `YAWLHooksWorkflowSchema` | `HWFLHooksWorkflowSchema` |
| `YAWLHooksTaskSchema` | `HWFLHooksTaskSchema` |
| `YawlNetSpecSchema` | `HwflNetSpecSchema` |

---

## 6. SPARQL Prefix Changes

| Old Prefix | New Prefix |
|------------|------------|
| `PREFIX yawl:` | `PREFIX hwfl:` |
| `PREFIX yawl-case:` | `PREFIX hwfl-case:` |
| `PREFIX yawl-task:` | `PREFIX hwfl-task:` |
| `PREFIX yawl-work:` | `PREFIX hwfl-work:` |

---

## 7. Package.json Changes

```json
{
  "name": "@unrdf/hwfl",
  "description": "HWFL (Hook Workflow Language) engine with KGC-4D time-travel and receipt verification",
  "exports": {
    ".": "./src/index.mjs",
    "./api": "./src/api/workflow-api.mjs",
    "./ontology": "./src/ontology/hwfl-ontology.mjs",
    "./store": "./src/store/hwfl-store.mjs",
    "./types": "./src/types/hwfl-types.mjs",
    "./schemas": "./src/types/hwfl-schemas.mjs",
    "./hooks": "./src/hooks/hwfl-hooks.mjs",
    "./resources": "./src/resources/hwfl-resources.mjs",
    "./cancellation": "./src/cancellation/index.mjs",
    "./receipt": "./src/receipt.mjs"
  },
  "keywords": [
    "workflow",
    "hwfl",
    "hook-workflow",
    "petri-net",
    "bpmn",
    "rdf",
    "time-travel"
  ]
}
```

---

## 8. Documentation Files to Update

### Thesis Documents
- `docs/PHD-THESIS-UNRDF-2028-REVOLUTION-FINAL.md`
- `docs/PHD-THESIS-UNRDF-2028-REVOLUTION-UPGRADE.md`
- `docs/THESIS-BEYOND-HUMAN-PERCEPTION-FINAL.md`
- `docs/THESIS-BEYOND-HUMAN-PERCEPTION-UPGRADE.md`
- `docs/THESIS-BIGBANG-80-20-FINAL.md`
- `docs/THESIS-BIGBANG-80-20-UPGRADE.md`
- `docs/THESIS-COMPLETION-EXECUTIVE-SUMMARY.md`
- `docs/THESIS-UPGRADE-SYNTHESIS-2025.md`

### Other Documents
- `BENCHMARK-SUITE.md`
- `CODE-LISTINGS.md`
- `CORRECTED-THESIS-EXCERPTS.md`
- `DIAGRAMS.md`
- `FINAL-ADVERSARIAL-REVIEW.md`
- `METRICS-CORRECTIONS.md`
- `PERFORMANCE-ANALYSIS.md`
- `PERFORMANCE-MODEL.md`
- `POLISHED-EXCERPTS.md`
- `SUPPLEMENTARY-MATERIALS.md`
- `TABLES.md`
- `TEST-RESULTS.md`

### Package Documents
- `packages/hwfl/ARCHITECTURAL-ANALYSIS.md`
- `packages/hwfl/THESIS-CONTRIBUTIONS.md`
- `packages/hwfl/validation/VALIDATION-REPORT.md`

---

## 9. Import Statement Changes

All imports referencing `@unrdf/yawl` must be updated:

```javascript
// OLD
import { WorkflowEngine, YawlWorkflow } from '@unrdf/yawl';
import { createYAWLPolicyPack } from '@unrdf/yawl/hooks/yawl-hooks';
import { createYawlStore } from '@unrdf/yawl/store';

// NEW
import { WorkflowEngine, HwflWorkflow } from '@unrdf/hwfl';
import { createHWFLPolicyPack } from '@unrdf/hwfl/hooks/hwfl-hooks';
import { createHwflStore } from '@unrdf/hwfl/store';
```

---

## 10. Text Replacement Patterns (Regex)

### Case-sensitive replacements (order matters)
```
1. @unrdf/yawl -> @unrdf/hwfl
2. http://yawl.io/ -> http://hwfl.io/
3. http://unrdf.org/yawl -> http://unrdf.org/hwfl
4. YAWL_NS -> HWFL_NS
5. YAWL_GRAPHS -> HWFL_GRAPHS
6. YAWL_EVENT_TYPES -> HWFL_EVENT_TYPES
7. YAWL_PREDICATES -> HWFL_PREDICATES
8. YAWL_PREFIXES -> HWFL_PREFIXES
9. YAWL_CASE -> HWFL_CASE
10. YAWL_TASK -> HWFL_TASK
11. YAWL_WORK -> HWFL_WORK
12. YAWL_RESOURCE_NS -> HWFL_RESOURCE_NS
13. YAWLWorkflowSchema -> HWFLWorkflowSchema
14. YAWLTaskSchema -> HWFLTaskSchema
15. YAWLHooksWorkflowSchema -> HWFLHooksWorkflowSchema
16. YAWLHooksTaskSchema -> HWFLHooksTaskSchema
17. YawlEngine -> HwflEngine
18. YawlWorkflow -> HwflWorkflow
19. YawlCase -> HwflCase
20. YawlTask -> HwflTask
21. YawlReceipt -> HwflReceipt
22. YawlResourcePool -> HwflResourcePool
23. YawlResourceManager -> HwflResourceManager
24. YawlNetSpecSchema -> HwflNetSpecSchema
25. createYAWLPolicyPack -> createHWFLPolicyPack
26. createYawlStore -> createHwflStore
27. PREFIX yawl: -> PREFIX hwfl:
28. PREFIX yawl-case: -> PREFIX hwfl-case:
29. PREFIX yawl-task: -> PREFIX hwfl-task:
30. PREFIX yawl-work: -> PREFIX hwfl-work:
31. yawl-cancellation -> hwfl-cancellation
32. yawl-events -> hwfl-events
33. yawl-hooks -> hwfl-hooks
34. yawl-ontology -> hwfl-ontology
35. yawl-resources -> hwfl-resources
36. yawl-store -> hwfl-store
37. yawl-schemas -> hwfl-schemas
38. yawl-types -> hwfl-types
39. yawl.test -> hwfl.test
40. "yawl" (in keywords) -> "hwfl"
```

### Context-aware replacements in documentation
```
YAWL (Yet Another Workflow Language) -> HWFL (Hook Workflow Language)
Van der Aalst's YAWL -> van der Aalst's YAWL (reference only, no rename needed)
YAWL patterns -> workflow patterns (or HWFL patterns)
YAWL engine -> HWFL engine
```

---

## 11. Files Requiring Manual Review After Rename

These files contain context-sensitive references that need human verification:

1. **Thesis documents** - Ensure academic citations to van der Aalst's YAWL are preserved
2. **ARCHITECTURAL-ANALYSIS.md** - Contains theoretical comparison with original YAWL
3. **THESIS-CONTRIBUTIONS.md** - Academic positioning vs prior art
4. **pnpm-lock.yaml** - Will auto-regenerate, verify workspaces resolve correctly
5. **Documentation diagrams** - Any ASCII art or mermaid diagrams with YAWL labels

---

## 12. Execution Order

1. **Backup** - Create a backup branch
2. **Directory rename** - `git mv packages/yawl packages/hwfl`
3. **File renames** - Rename all yawl-*.mjs files to hwfl-*.mjs
4. **Text replacements** - Run sed/perl scripts in correct order
5. **Update package.json** - Name, description, exports, keywords
6. **Update pnpm-lock.yaml** - Run `pnpm install`
7. **Verify imports** - Run TypeScript/linter checks
8. **Run tests** - Ensure all tests pass
9. **Manual review** - Check thesis documents for academic citations
10. **Commit** - Create atomic commit with all changes

---

## 13. Rollback Plan

If the rename causes issues:
```bash
git checkout main -- packages/yawl
git checkout main -- pnpm-lock.yaml
# Revert all text changes
git checkout main -- *.md
git checkout main -- *.mjs
```

---

## 14. Post-Rename Verification Checklist

- [ ] `pnpm install` succeeds
- [ ] `pnpm --filter @unrdf/hwfl test` passes
- [ ] No import errors in dependent packages
- [ ] All thesis documents compile/render correctly
- [ ] Van der Aalst citations preserved (not renamed)
- [ ] Namespace URIs correctly updated
- [ ] SPARQL queries execute correctly
- [ ] Backward compatibility aliases work
- [ ] Git history preserved for renamed files

---

## 15. Academic Citation Note

**IMPORTANT**: When referencing van der Aalst's original work in academic contexts, preserve the original "YAWL" name:

> van der Aalst, W.M.P., ter Hofstede, A.H.M. (2005). YAWL: Yet Another Workflow Language. *Information Systems*, 30(4), 245-275.

The rename only affects **this package's name**, not academic references to prior art.
