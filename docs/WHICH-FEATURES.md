# Which Features Do I Need?

**Default answer: Just use `createKnowledgeSubstrateCore()`.**

---

## The Primary Decision

```
Are you building an RDF application?
├── YES → Use createKnowledgeSubstrateCore()  ← 80% of users stop here
└── Need something it doesn't provide?
    ├── Custom hooks → Add defineHook() / registerHook()
    ├── Multi-store management → Use low-level APIs
    └── Still not sure → Start with createKnowledgeSubstrateCore() anyway
```

---

## Decision Tree: API Level

```
Which API level should I use?

START → createKnowledgeSubstrateCore()
  ↓
  Does it give you what you need?
  ├── YES → Done. Ship it.
  └── NO → What's missing?
      ├── Custom autonomous behaviors → Add Knowledge Hooks (Level 2)
      ├── Multi-store scenarios → Use low-level APIs (Level 3)
      └── Something else → File an issue, you might be overcomplicating
```

---

## Decision Tree: Optional Components

These are disabled by default. Enable only if needed:

### PolicyPackManager

```
Do you need declarative governance rules?
├── NO → Keep disabled (default)
└── YES → Do you need:
    ├── Access control → Enable PolicyPackManager
    ├── Business rules → Enable PolicyPackManager
    ├── Audit compliance → Enable PolicyPackManager
    └── Just SHACL validation → Keep disabled, use built-in validation
```

### ResolutionLayer

```
Do you need multi-agent consensus?
├── NO → Keep disabled (default)
└── YES → Do you have:
    ├── Multiple agents writing to the same store → Enable ResolutionLayer
    ├── Conflict resolution requirements → Enable ResolutionLayer
    └── Single agent → Keep disabled
```

---

## Decision Tree: Knowledge Hooks

```
Do you need custom hooks beyond what createKnowledgeSubstrateCore() provides?
├── NO → Don't add any hooks
└── YES → What kind?
    ├── Validation → The Substrate already has this
    ├── Transformation → The Substrate already has this
    ├── Logging/Audit → The Substrate already has LockchainWriter
    └── Something custom → Add defineHook() / registerHook()
```

---

## Decision Tree: Low-level APIs

```
Should I use low-level APIs (parseTurtle, query, etc.)?
├── NO (default) → Use createKnowledgeSubstrateCore()
└── YES only if:
    ├── Managing multiple independent stores
    ├── Migrating from another RDF library
    ├── Building a library on top of UNRDF
    └── Have a very specific performance requirement
```

---

## Summary: The Hierarchy

| Level | API | Use For | % of Users |
|-------|-----|---------|------------|
| 1 | `createKnowledgeSubstrateCore()` | Everything | 80% |
| 2 | `defineHook()` / `registerHook()` | Custom hooks | 15% |
| 3 | `parseTurtle()` / `query()` | Edge cases | 5% |

**Start at Level 1. Move down only when necessary.**

---

## Component Usage Statistics

Based on the 80/20 principle:

| Component | % Value Delivered | Default State |
|-----------|-------------------|---------------|
| TransactionManager | High (core) | Enabled |
| KnowledgeHookManager | High (core) | Enabled |
| EffectSandbox | High (security) | Enabled |
| LockchainWriter | High (audit) | Enabled |
| PerformanceOptimizer | High (speed) | Enabled |
| Observability | High (debugging) | Enabled |
| PolicyPackManager | Medium (governance) | Disabled |
| ResolutionLayer | Low (multi-agent) | Disabled |

---

## Still Unsure?

If you're asking "do I need X?", the answer is probably:

1. **Use `createKnowledgeSubstrateCore()`** with defaults
2. **Build your application**
3. **Only enable optional components** when you hit a specific requirement
4. **Only drop to lower API levels** when the higher levels don't work

The Substrate gives you 80% of what you need out of the box.
