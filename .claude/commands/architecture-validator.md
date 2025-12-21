---
description: Validate UNRDF architecture against three-tier boundaries, circular dependencies, and package structure rules
---

# Architecture Validator

## User Input

```text
$ARGUMENTS
```

You **MUST** consider the user input before proceeding (if not empty).

## Purpose

Validate that UNRDF's 21-package monorepo architecture maintains proper boundaries, avoids circular dependencies, and follows established patterns.

## Architecture Overview

### Package Categories

| Category    | Packages                         | Responsibility         |
| ----------- | -------------------------------- | ---------------------- |
| Core        | core, oxigraph                   | RDF primitives, store  |
| Validation  | validation, hooks                | SHACL, OTEL, hooks     |
| Streaming   | streaming, federation            | Real-time, distributed |
| CLI         | cli                              | User interface         |
| Engines     | engine-gateway, knowledge-engine | Query routing          |
| Domain      | domain, composables              | Business logic         |
| Specialized | kgn, kgc-4d, atomvm              | Advanced features      |

### Dependency Rules

```
Allowed:
  cli → ops → core
  ops → validation
  any → oxigraph (RDF primitives)

Forbidden:
  core → cli (inverts hierarchy)
  circular: A → B → A
  direct: app → n3 (use oxigraph)
```

## Execution Steps

### 1. Analyze Package Dependencies

```bash
# List all packages
ls -1 packages/

# Check each package's dependencies
for pkg in packages/*/package.json; do
  echo "=== $(dirname $pkg | xargs basename) ==="
  jq '.dependencies // {} | keys[]' "$pkg" 2>/dev/null | grep "@unrdf" || echo "(no internal deps)"
done
```

### 2. Detect Circular Dependencies

```bash
# Build dependency graph
echo "digraph deps {" > deps.dot
for pkg in packages/*/package.json; do
  name=$(jq -r '.name' "$pkg")
  jq -r '.dependencies // {} | keys[] | select(startswith("@unrdf"))' "$pkg" | while read dep; do
    echo "  \"$name\" -> \"$dep\";"
  done
done >> deps.dot
echo "}" >> deps.dot

# Detect cycles (requires graphviz)
which tred && tred deps.dot 2>&1 | grep "cycle" || echo "✅ No cycles detected"
```

### 3. Check Layer Violations

```bash
# Core should not import CLI
grep -r "from '@unrdf/cli'" packages/core/src/ --include="*.mjs" && echo "❌ Core imports CLI" || echo "✅ Core clean"

# Core should not import validation
grep -r "from '@unrdf/validation'" packages/core/src/ --include="*.mjs" && echo "⚠️ Core imports validation" || echo "✅ Core clean"

# Check for N3 outside oxigraph
grep -r "from 'n3'" packages/*/src/ --include="*.mjs" | grep -v "oxigraph\|n3-justified-only" && echo "❌ Unauthorized N3" || echo "✅ N3 contained"
```

### 4. Validate Export Consistency

```bash
# Check that each package exports from index
for pkg in packages/*/; do
  name=$(basename "$pkg")
  if [[ -f "$pkg/src/index.mjs" ]]; then
    exports=$(grep "^export" "$pkg/src/index.mjs" | wc -l)
    echo "$name: $exports exports"
  else
    echo "$name: ❌ Missing src/index.mjs"
  fi
done
```

### 5. Check Package Metadata

```bash
# Validate package.json consistency
for pkg in packages/*/package.json; do
  name=$(jq -r '.name' "$pkg")
  version=$(jq -r '.version' "$pkg")
  main=$(jq -r '.main // "none"' "$pkg")
  types=$(jq -r '.types // "none"' "$pkg")

  echo "$name@$version main=$main types=$types"

  # Check for required fields
  [[ "$main" == "none" ]] && echo "  ⚠️ Missing main"
  [[ "$types" == "none" ]] && echo "  ⚠️ Missing types"
done
```

## Architecture Patterns

### Correct: Layer Separation

```javascript
// packages/cli/src/commands/maturity/assess.mjs
import { collectCoverage } from '../../lib/maturity/collector.mjs'; // ops
import { calculateScore } from '@unrdf/validation/maturity'; // validation
import { createStore } from '@unrdf/oxigraph'; // runtime

// CLI → Ops → Runtime (correct flow)
```

### Incorrect: Layer Violation

```javascript
// ❌ packages/core/src/store.mjs
import { defineCommand } from 'citty'; // Core importing CLI framework!
```

### Correct: Shared Types

```javascript
// packages/core/src/types.mjs
/**
 * @typedef {Object} MaturityLevel
 * @property {1|2|3|4|5} level
 * @property {string} name
 */

// Other packages import types from core
import { MaturityLevel } from '@unrdf/core/types';
```

## Output Format

```markdown
## Architecture Validation Report

**Date**: [timestamp]
**Packages Analyzed**: 21

### Dependency Graph

| Package     | Internal Deps | External Deps | Status |
| ----------- | ------------- | ------------- | ------ |
| @unrdf/core | 1             | 5             | ✅     |
| @unrdf/cli  | 8             | 12            | ✅     |

### Layer Compliance

| Layer | Packages          | Violations | Status |
| ----- | ----------------- | ---------- | ------ |
| CLI   | cli               | 0          | ✅     |
| Ops   | validation, hooks | 0          | ✅     |
| Core  | core, oxigraph    | 0          | ✅     |

### Circular Dependencies

- ✅ None detected

### Issues Found

1. [CRITICAL/HIGH/MEDIUM] Description
   - Packages: X → Y → X
   - Fix: Break cycle at Y

### Recommendations

- [architectural improvements]
```

## Quick Validation

```bash
# One-liner architecture check
pnpm exec turbo run build --dry-run 2>&1 | grep -E "error|warning" || echo "✅ Build graph valid"
```

End Command ---
