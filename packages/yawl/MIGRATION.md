# YAWL API Migration Guide

**Version**: 5.0.0 → 6.0.0
**Date**: 2025-12-25

## Overview

The YAWL package has been refactored from monolithic files into focused modules
for better maintainability and code quality. This guide helps you migrate.

## Breaking Changes

### ❌ OLD: Import from monolithic files
```javascript
// This no longer works
import { createWorkflow, createCase } from '@unrdf/yawl/src/api/workflow-api.mjs';
```

### ✅ NEW: Import from focused modules
```javascript
// Option 1: Import from main package (recommended)
import { createWorkflow, createCase, enableTask } from '@unrdf/yawl';

// Option 2: Import from specific modules
import { createWorkflow } from '@unrdf/yawl/src/api/workflow-creation.mjs';
import { createCase } from '@unrdf/yawl/src/api/workflow-query.mjs';
import { enableTask } from '@unrdf/yawl/src/api/workflow-execution.mjs';
```

## Module Structure

### API Modules (workflow-api.mjs split into 6)

| Module | Exports | Use When |
|--------|---------|----------|
| `workflow-creation.mjs` | `createWorkflow()` | Creating new workflows |
| `workflow-execution.mjs` | `enableTask()`, `startTask()`, `completeTask()` | Executing tasks |
| `workflow-query.mjs` | `createCase()`, queries | Creating/querying cases |
| `workflow-cancellation.mjs` | `cancelWorkItem()` | Cancelling work items |
| `workflow-timemachine.mjs` | `replayCase()` | Time-travel functionality |
| `api/index.mjs` | All API functions | Main entry (use this) |

### Resource Modules (yawl-resources.mjs split into 5)

| Module | Purpose |
|--------|---------|
| `resource-participants.mjs` | Participant management |
| `resource-tools.mjs` | Tool allocation |
| `resource-roles.mjs` | Role-based access |
| `resource-capacity.mjs` | Capacity tracking |
| `resources/index.mjs` | Main ResourceManager |

### Engine Modules (engine.mjs split into 7)

| Module | Purpose |
|--------|---------|
| `engine-core.mjs` | Core engine class |
| `engine-events.mjs` | Event subscriptions |
| `engine-hooks.mjs` | Policy hooks |
| `engine-health.mjs` | Health monitoring |
| `engine-snapshots.mjs` | State snapshots |
| `engine-queries.mjs` | Query functionality |
| `engine/index.mjs` | Main YawlEngine |

### Workflow Modules (workflow.mjs split into 5)

| Module | Purpose |
|--------|---------|
| `workflow-core.mjs` | Core YawlWorkflow class |
| `workflow-validation.mjs` | Validation logic |
| `workflow-rdf.mjs` | RDF serialization |
| `workflow-patterns.mjs` | Pattern detection |
| `workflow/index.mjs` | Main workflow export |

## Migration Steps

### Step 1: Update Imports

```javascript
// Before
import { YawlEngine } from '@unrdf/yawl/src/engine.mjs';
import { createWorkflow } from '@unrdf/yawl/src/api/workflow-api.mjs';

// After
import { YawlEngine } from '@unrdf/yawl';
import { createWorkflow } from '@unrdf/yawl';
```

### Step 2: Test Your Application

```bash
# Run your test suite
npm test

# Check for import errors
npm run typecheck
```

### Step 3: Update TypeScript Types (if using)

The type definitions remain the same, but import paths may need updating.

## Backwards Compatibility

The main package exports remain unchanged:

```javascript
import { YawlEngine, createWorkflow, createCase } from '@unrdf/yawl';
// This still works! ✅
```

Only deep imports to specific files need updating.

## Benefits of New Structure

1. **Smaller files**: All modules <600 lines (was 1,700+)
2. **Better organization**: Find code faster
3. **Faster IDE**: Better autocomplete and navigation
4. **Easier testing**: Test individual modules
5. **Tree shaking**: Import only what you need

## Need Help?

- GitHub Issues: https://github.com/unrdf/yawl/issues
- Documentation: packages/yawl/README.md
- Examples: packages/yawl/examples/

## Timeline

- **v5.0.0**: Monolithic structure (deprecated)
- **v6.0.0**: Modular structure (current)
- **v7.0.0**: Remove old structure (future)

We recommend migrating before v7.0.0.
