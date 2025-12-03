# Which Features Do I Need?

**Default answer: Just the basics.**

Use these decision trees to determine if you need advanced features.

---

## Decision Tree: Knowledge Hooks

```
Do you need code to run automatically on EVERY data change?
├── NO → Skip Knowledge Hooks
└── YES → Do you need to validate, transform, or log changes?
    ├── Just validate once → Use SHACL validation directly
    ├── Transform data occasionally → Do it in your code
    └── Need autonomous, event-driven reactions → Use Knowledge Hooks
```

**Most users: Skip hooks. Validate when needed.**

---

## Decision Tree: Transactions

```
Do you need atomic, all-or-nothing data changes?
├── NO → Just add/remove quads directly
└── YES → Do you need hooks to run during transactions?
    ├── NO → Just add/remove quads directly (N3 Store handles it)
    └── YES → Use TransactionManager
```

**Most users: Just add/remove quads. Skip TransactionManager.**

---

## Decision Tree: Lockchain (Audit Trails)

```
Do you have compliance requirements (GDPR, SOC2, HIPAA)?
├── NO → Skip Lockchain
└── YES → Do you need cryptographic proof of data changes?
    ├── Just logging → Use standard logging
    └── Need tamper-evident audit trail → Use LockchainWriter
```

**Most users: Skip Lockchain. Add only for compliance.**

---

## Decision Tree: Dark Matter (Performance)

```
Are queries taking >100ms?
├── NO → Skip Dark Matter
└── YES → Have you tried basic optimization first?
    ├── NO → Try: indexes, caching, query rewriting
    └── YES, still slow → Use Dark Matter (QueryOptimizer, CriticalPathAnalyzer)
```

**Most users: Skip Dark Matter. Optimize basics first.**

---

## Decision Tree: React Hooks

```
Are you building a React application?
├── NO → Skip React Hooks
└── YES → Use unrdf/react-hooks
```

**Non-React users: Skip entirely.**

---

## Decision Tree: Policy Packs

```
Do you need declarative governance rules?
├── NO → Skip Policy Packs
└── YES → Do you need to compose multiple validation policies?
    ├── Just one validation → Use SHACL directly
    └── Multiple policies, access control → Use PolicyPackManager
```

**Most users: Use SHACL directly. Skip PolicyPackManager.**

---

## Decision Tree: Federation

```
Do you need to query across multiple distributed nodes?
├── NO → Skip Federation
└── YES → Use federation/consensus modules
```

**Most users: Single store is enough. Skip Federation.**

---

## Decision Tree: Streaming

```
Do you need real-time data pipelines with windowing/aggregation?
├── NO → Skip Streaming
└── YES → Use streaming modules (ChangeFeed, WindowProcessor)
```

**Most users: Batch processing is enough. Skip Streaming.**

---

## Summary: The 80/20 Rule

| Feature | % of Users Who Need It |
|---------|------------------------|
| Parse + Query (basics) | 80% |
| SHACL Validation | 40% |
| React Hooks | 20% |
| Knowledge Hooks | 10% |
| TransactionManager | 5% |
| Lockchain | 5% |
| Dark Matter | 5% |
| Policy Packs | 3% |
| Federation | 2% |
| Streaming | 2% |

**Start with the 80%. Add only what you need.**

---

## Still Unsure?

If you're asking "do I need X?", the answer is probably NO.

Start with [START-HERE.md](START-HERE.md) and add features only when you hit a wall.
