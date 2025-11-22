# Quick Reference for Agents

**Machine-readable quick reference for common operations**

## Capability Discovery

```javascript
// Discover all capabilities
const caps = await system.query({
  query: 'SELECT ?cap WHERE { ?cap a cap:Capability }',
  type: 'sparql-select'
});

// Find capabilities by domain
const validationCaps = await discoverCapabilitiesByCriteria({
  domain: 'validation',
  maxLatency: 10
});
```

## MAPEK Execution

```javascript
// Single cycle
const result = await runMapekIteration({
  projectStore, domainStore, projectRoot
});

// Continuous loop
const loop = await runContinuousMapekLoop({
  getState: async () => buildState(),
  applyActions: async (actions) => applyFixes(actions)
});
```

## Knowledge Hook Creation

```javascript
// Define hook
const hook = defineHook({
  meta: { name: 'my-hook' },
  when: { kind: 'sparql-ask', query: 'ASK { ?s ?p ?o }' },
  run: async (event) => ({ result: 'success' })
});

// Register hook
await system.registerHook(hook);
```

## Error Codes

| Code | Message | Recovery |
|------|---------|----------|
| `E_MAPEK_INVALID_STORE` | Store must have getQuads method | Verify store is N3.Store |
| `E_HOOK_INVALID_NAME` | Hook name invalid pattern | Use alphanumeric + :_- only |
| `E_HOOK_MISSING_CONDITION` | Either query or ref required | Provide condition |
| `E_SYSTEM_INIT_FAILED` | Failed to initialize | Check runtime environment |

## Performance SLOs

| Operation | p99 Latency | Throughput |
|-----------|-------------|------------|
| `defineHook` | < 1ms | 50k/s |
| `query` | < 50ms | 1k/s |
| `runMapekIteration` | < 200ms | 5/s |
| `executeTransaction` | < 100ms | 500/s |

## Entry Points

- `unrdf` - Main entry
- `unrdf/knowledge-engine` - Knowledge engine core
- `unrdf/project-engine` - MAPEK autonomic loop
- `unrdf/cli` - CLI interface

