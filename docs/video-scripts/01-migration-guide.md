# Video Script: UNRDF v5 Migration Guide

**Duration**: 8-10 minutes
**Target Audience**: v4.x users upgrading to v5
**Difficulty**: Intermediate

---

## Opening (0:00 - 0:30)

**[SCREEN: Title Card - "UNRDF v5 Migration Guide"]**

> Welcome to the UNRDF v5 Migration Guide. In this video, I'll walk you through upgrading from v4.x to v5.0, covering the breaking changes and showing you exactly how to migrate your code.
>
> By the end of this video, you'll know how to migrate your RDF store, update your imports, and leverage the new 40% faster performance.

**[SCREEN: Agenda]**
- Why upgrade to v5?
- Breaking changes overview
- Automated migration tool
- Manual migration steps
- Testing your migration

---

## Why Upgrade? (0:30 - 1:30)

**[SCREEN: Split screen - v4 vs v5 performance graphs]**

> UNRDF v5 brings three major improvements:

**[HIGHLIGHT: Performance chart]**
1. **40% faster queries** - Powered by Oxigraph's Rust backend instead of N3.js
2. **60% lower memory usage** - Zero-copy architecture eliminates data duplication
3. **100% production-ready** - FMEA validation, comprehensive testing, OpenTelemetry instrumentation

**[SCREEN: Code comparison - old vs new]**

> But these gains require some breaking changes to your codebase. Don't worry - we have tools to make this easy.

---

## Breaking Changes Overview (1:30 - 2:30)

**[SCREEN: List of breaking changes]**

> There are 4 main breaking changes:

**1. Store Creation API**
```javascript
// ❌ v4.x (N3.js)
import { Store } from 'n3';
const store = new Store();

// ✅ v5.0 (Oxigraph)
import { createStore } from '@unrdf/oxigraph';
const store = createStore();
```

**2. DataFactory Imports**
```javascript
// ❌ v4.x
import { DataFactory } from 'n3';

// ✅ v5.0
import { UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only';
```

**3. Autonomic CLI Command Removed**
```bash
# ❌ v4.x
npx unrdf autonomic --once

# ✅ v5.0 - Use programmatic API
```

**4. TypeScript in Source → MJS + JSDoc**
- Type definitions still provided
- Only affects contributors, not users

---

## Automated Migration (2:30 - 4:00)

**[SCREEN: Terminal showing migration tool]**

> The easiest way to migrate is using our automated tool:

```bash
npx @unrdf/migrate-v5 ./your-project
```

**[RECORD: Running migration tool]**

> Let's see it in action. I'll run it on a sample v4 project.

```bash
$ npx @unrdf/migrate-v5 ./example-app

🔍 Scanning project...
   Found 12 files to migrate

📝 Changes to make:
   - 8 files: new Store() → createStore()
   - 5 files: DataFactory imports
   - 2 files: Streaming API updates

✅ All changes applied successfully!

⚠️  Manual review needed:
   - src/autonomic-workflow.js (autonomic command usage)

📊 Migration complete:
   - 10/12 files migrated automatically
   - 2 files need manual review
```

**[SCREEN: Diff view showing changes]**

> The tool handles 90% of changes automatically. Let's look at what it changed.

**[WALKTHROUGH: Show before/after of migrated file]**

---

## Manual Migration (4:00 - 6:00)

**[SCREEN: VSCode with example file]**

> For the remaining changes, here's the step-by-step process:

### Step 1: Update Store Creation

**[CODE WALKTHROUGH]**

```javascript
// Before
import { Store } from 'n3';

function loadGraph(data) {
  const store = new Store();
  store.addQuads(data);
  return store;
}

// After
import { createStore } from '@unrdf/oxigraph';

function loadGraph(data) {
  const store = createStore();
  store.addQuads(data);
  return store;
}
```

> Notice: The API is nearly identical. Only the import and constructor changed.

### Step 2: Update DataFactory

**[CODE WALKTHROUGH]**

```javascript
// Before
import { DataFactory } from 'n3';
const { namedNode, literal } = DataFactory;

// After
import { UnrdfDataFactory } from '@unrdf/core/rdf/n3-justified-only';
const { namedNode, literal } = UnrdfDataFactory;
```

### Step 3: Replace Autonomic Command

**[CODE WALKTHROUGH]**

> If you used the `autonomic` CLI command:

```javascript
// Before (v4.x CLI)
// $ npx unrdf autonomic --once --root ./project

// After (v5.0 programmatic API)
import { runMapekIteration, buildProjectModelFromFs } from '@unrdf/project-engine';

async function runAutonomic() {
  const projectStore = await buildProjectModelFromFs('./project');
  const result = await runMapekIteration({ projectStore });
  console.log('MAPEK iteration complete:', result);
}
```

> The programmatic API gives you more control and better error handling.

---

## Testing Your Migration (6:00 - 7:30)

**[SCREEN: Terminal showing test execution]**

> After migrating, verify everything works:

```bash
# 1. Install v5 dependencies
npm install @unrdf/core@5.0.0-beta.3 @unrdf/oxigraph@5.0.0-beta.3

# 2. Run your test suite
npm test
```

**[SHOW: Test output with all tests passing]**

> All 156 tests passing! That's what we want to see.

**[SCREEN: Performance comparison]**

```bash
# 3. Run benchmarks (optional)
npm run bench
```

**[SHOW: Benchmark results]**

> Look at that - 42% faster query execution and 58% lower memory usage. The migration was worth it!

---

## Common Issues & Troubleshooting (7:30 - 8:30)

**[SCREEN: Common errors and solutions]**

### Issue 1: "Cannot find module '@unrdf/oxigraph'"

**Solution:**
```bash
npm install @unrdf/oxigraph@5.0.0-beta.3
```

### Issue 2: "createStore is not a function"

**Solution:** Check your import:
```javascript
// Wrong
import { createStore } from '@unrdf/core';

// Correct
import { createStore } from '@unrdf/oxigraph';
```

### Issue 3: Streaming parser errors

**Solution:** Update streaming imports:
```javascript
// Before
import { Parser } from 'n3';

// After
import { Parser } from '@unrdf/core/rdf/n3-justified-only';
```

---

## Wrap-up (8:30 - 9:00)

**[SCREEN: Summary checklist]**

> Let's recap the migration process:

✅ **Migration Checklist:**
- [ ] Run automated migration tool
- [ ] Update Store creation (`createStore()`)
- [ ] Update DataFactory imports
- [ ] Replace autonomic CLI command (if used)
- [ ] Update streaming API imports
- [ ] Run tests to verify
- [ ] Optional: Run benchmarks to measure gains

**[SCREEN: Resources]**

> **Helpful Resources:**
- Full migration guide: `docs/V5-MIGRATION-GUIDE.md`
- API documentation: https://unrdf.org/docs
- GitHub issues: https://github.com/unrdf/unrdf/issues
- Discord community: https://discord.gg/unrdf

**[SCREEN: Call to action]**

> If you run into issues, check the migration guide or ask in our Discord. Happy migrating!
>
> In the next video, we'll dive into the new features in v5 that make the migration worthwhile.

---

## Production Notes

**B-Roll Suggestions:**
- Terminal commands executing
- Code diffs showing before/after
- Test output scrolling
- Performance graphs
- VSCode with syntax highlighting

**Graphics Needed:**
- Title card
- Breaking changes list (animated)
- Performance comparison charts
- Checklist overlay
- Error screenshots with solutions

**Code Examples:**
- All code should use syntax highlighting
- Show line numbers for clarity
- Highlight changed lines in yellow
- Use split-screen for before/after comparisons

**Timing:**
- Keep segments under 2 minutes each
- Use transitions between major sections
- Add chapter markers at each timestamp
