# UNRDF v5.0.0 Migration Guide

## Breaking Changes

### CLI: Autonomic Command Removed

**Status:** ⚠️ Deprecated in v4.0.1, **Removed in v5.0.0**

The `unrdf autonomic` CLI command has been removed due to architectural limitations in the MAPEK pipeline's store type handling.

#### Migration Path

**Before (v4.x - CLI):**
```bash
# This will no longer work in v5.0.0
npx unrdf autonomic --once --root ./my-project
npx unrdf autonomic --continuous --apply
```

**After (v5.0.0 - Programmatic API):**
```javascript
import {
  runMapekIteration,
  runContinuousMapekLoop
} from 'unrdf/project-engine';
import {
  buildProjectModelFromFs,
  inferDomainModel
} from 'unrdf/project-engine';

// Single MAPEK iteration (replaces --once)
const projectStore = await buildProjectModelFromFs('./my-project');
const domainStore = await inferDomainModel(projectStore);

const result = await runMapekIteration({
  projectStore,
  domainStore,
  projectRoot: './my-project'
});

console.log(`Health: ${result.overallHealth}%`);
console.log(`Decisions: ${result.decisions.length}`);

// Continuous loop (replaces --continuous)
const loopResult = await runContinuousMapekLoop({
  getState: async () => ({
    projectStore: await buildProjectModelFromFs('./my-project'),
    domainStore: await inferDomainModel(projectStore),
    projectRoot: './my-project'
  }),
  applyActions: async (actions) => {
    console.log(`Applying ${actions.length} auto-fixes...`);
    // Implement your auto-fix logic
  },
  intervalMs: 5000,
  maxIterations: 10
});

console.log(`Converged: ${loopResult.converged}`);
console.log(`Final health: ${loopResult.finalHealth}%`);
```

#### Why This Change?

1. **Architectural Limitations**: The CLI command had deep issues with store type mismatches in the MAPEK pipeline, specifically in drift computation (`collectDiffTriplesFromStore: store must implement getQuads()`)

2. **Better API Design**: The programmatic API provides:
   - More flexible integration options
   - Better error handling and debugging
   - Full TypeScript/JSDoc support
   - Easier testing and mocking
   - Direct access to intermediate results

3. **Focus on Core Functionality**: Removing the CLI command allows us to focus development on the robust programmatic API while maintaining the core MAPEK functionality

#### Remaining CLI Commands

All other CLI commands remain fully functional in v5.0.0:

| Command | Status | Use Case |
|---------|--------|----------|
| `unrdf init` | ✅ Maintained | Project initialization and analysis |
| `unrdf store backup` | ✅ Maintained | Create RDF store backups |
| `unrdf store restore` | ✅ Maintained | Restore stores from backup |
| `unrdf store import` | ✅ Maintained | Bulk import RDF files |

## Timeline

- **v4.0.0** (2024-12): Autonomic command fully functional
- **v4.0.1** (2024-12): Autonomic command deprecated with warnings
- **v5.0.0** (TBD): Autonomic command removed

## Need Help?

If you're unsure how to migrate your autonomic workflows:

1. **Check the examples**: See `docs/examples/mapek-programmatic.md`
2. **Read the API docs**: See `docs/reference/api-reference.md#mapek-api-reference`
3. **Open an issue**: https://github.com/unrdf/unrdf/issues

## Full API Reference

For complete MAPEK API documentation, see:
- [MAPEK Guide](./AUTONOMIC-MAPEK-README.md)
- [API Reference](./reference/api-reference.md)
- [Tutorial: Running Your First MAPEK Cycle](../README.md#tutorial-2-running-your-first-mapek-cycle)
