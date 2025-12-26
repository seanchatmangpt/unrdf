# AUTONOMIC_INNOVATION - Runbook

## Quick Start

```bash
# 1. Run the master demonstration
pnpm demo:autonomic

# 2. Run integration tests
pnpm test:autonomic

# 3. Verify determinism
pnpm test:determinism
```

## Commands

### 1. Master Demonstration

```bash
pnpm demo:autonomic
# Alias: pnpm demo
```

**What it does**:
- Exercises ALL 10 agent primitives in sequence
- Shows capsule planning, lens application, impact sets, commutativity checks, etc.
- Produces deterministic output (same hash on repeat runs)
- Zero external dependencies

**Expected output**:
```
AUTONOMIC_INNOVATION - Master Demonstration
Version: 0.1.0

============================================================
  0. INTEGRATION STATUS
============================================================
Available: X/9 agents
Stubs: Y agents
  ✅ Capsules (agent-2): AVAILABLE
  ⚠️ Lenses (agent-3): STUB
  ...

============================================================
  1. CAPSULE PLANNING & HASHING
============================================================
[CAPSULE] Operations: 2
[CAPSULE] Hash: abc123...
[CAPSULE] Canonical: def456...

... (continues through all 10 agents)

============================================================
  10. SUMMARY
============================================================
✅ All 10 agent primitives exercised
✅ Deterministic output (same hash on repeat runs)
✅ Zero external dependencies
✅ Integration framework operational

Demo output hash: abc123...
```

**Timeout**: 5 seconds (default)

---

### 2. Integration Tests

```bash
pnpm test:autonomic
# Alias: pnpm test
```

**What it does**:
- Validates all 9 agent imports are accessible
- Checks public API completeness (45+ primitives)
- Detects circular dependencies
- Verifies integration contracts

**Expected output**:
```
Running integration tests...

✅ Agent 2-10 imports successful
✅ Public API complete (45 primitives)
✅ No circular dependencies
✅ Integration validation passed

All tests passed (4/4)
```

**Timeout**: 5 seconds

---

### 3. Determinism Validation

```bash
pnpm test:determinism
```

**What it does**:
- Runs demo.mjs twice
- Compares output hashes
- Proves deterministic execution

**Expected output**:
```
Running determinism test...

Run 1...
Demo output hash: abc123def456...

Run 2...
Demo output hash: abc123def456...

✅ Determinism verified
Hashes match: abc123def456...
```

**Timeout**: 10 seconds (allows two full demo runs)

---

### 4. Fast Tests (Development)

```bash
pnpm test:fast
```

**What it does**:
- Same as `pnpm test` but with explicit 5s timeout
- Use during development for quick validation
- Kills hung processes automatically

**Expected output**: Same as integration tests

**Timeout**: 5 seconds (enforced)

---

## Verification Commands

### Check Agent Status

```bash
node -e "import('./src/index.mjs').then(m => m.getIntegrationStatus()).then(s => console.log(JSON.stringify(s, null, 2)))"
```

**Output**:
```json
{
  "total": 9,
  "available": 3,
  "stubs": 6,
  "errors": 0,
  "version": "0.1.0",
  "agents": [
    {
      "agentId": "agent-2",
      "name": "Capsules",
      "status": "AVAILABLE",
      "missing": [],
      "error": null
    },
    ...
  ]
}
```

### Test Individual Agent Import

```bash
# Test Agent 2 (Capsules)
node -e "import('../agent-2/index.mjs').then(m => console.log('Exports:', Object.keys(m)))"

# Test Agent 3 (Lenses)
node -e "import('../agent-3/index.mjs').then(m => console.log('Exports:', Object.keys(m)))"
```

### Validate Public API

```bash
node -e "import('./src/index.mjs').then(m => console.log('Public API exports:', Object.keys(m).filter(k => !k.startsWith('_')).length))"
```

**Expected**: `47` (45 primitives + 2 utility functions)

---

## Troubleshooting

### Agent Not Available

**Symptom**: `⚠️ Agent not available: Cannot find module`

**Solution**:
1. Check if agent directory exists: `ls -la ./agent-X/`
2. Verify `index.mjs` exists: `cat ./agent-X/index.mjs`
3. Agent will run as STUB (graceful degradation)

### Timeout Exceeded

**Symptom**: Command killed after 5s/10s

**Root causes**:
- Infinite loop in agent code
- Network call (should be zero external deps)
- Heavy computation (optimize or justify longer timeout)

**Solution**:
1. Check agent implementation for blocking calls
2. Add early returns for STUB agents
3. Profile with `time node demo.mjs`

### Determinism Failure

**Symptom**: `Run 1 hash != Run 2 hash`

**Root causes**:
- Non-deterministic timestamps (use fixed values)
- Random number generation
- Set/Map iteration order without sorting

**Solution**:
1. Fix timestamps to constant: `'2025-12-26T00:00:00Z'`
2. Sort collections before hashing
3. Use `JSON.stringify` with no formatting

---

## Performance Targets

| Command | Target | Acceptable | Investigate |
|---------|--------|------------|-------------|
| `pnpm demo` | <3s | <5s | >5s |
| `pnpm test` | <2s | <5s | >5s |
| `pnpm test:determinism` | <6s | <10s | >10s |

**Measure**: `time pnpm <command>`

---

## Integration Checklist

Before declaring a new agent complete, verify:

- [ ] `./agent-X/index.mjs` exports all required functions (see PLAN.md)
- [ ] `pnpm test` still passes
- [ ] `pnpm demo` includes agent output
- [ ] `pnpm test:determinism` still passes
- [ ] No new external dependencies added
- [ ] JSDoc types provided for all exports
- [ ] Timeout <5s maintained

---

## File Structure

```
AUTONOMIC_INNOVATION/
├── agent-1/              # Orchestrator (this agent)
│   ├── index.mjs         # Central integration
│   ├── constants.mjs     # Shared constants
│   ├── types.mjs         # JSDoc types
│   ├── test.mjs          # Integration tests
│   └── PLAN.md           # Implementation plan
├── agent-2/              # Capsules (implemented by Agent 2)
├── agent-3/              # Lenses (implemented by Agent 3)
├── ...
├── agent-10/             # Quality (implemented by Agent 10)
├── src/
│   └── index.mjs         # Public API
├── demo.mjs              # Master demonstration
├── package.json          # Package config
├── RUNBOOK.md            # This file
└── README.md             # Project overview
```

---

## Next Steps

1. **Agents 2-10**: Implement respective primitives
2. **Integration**: Test imports as agents complete
3. **E2E**: Run full demo when all agents available
4. **Optimization**: Profile and optimize slow paths
5. **Documentation**: Expand with usage examples

---

## Support

**Questions?** Check:
1. PLAN.md - Integration contracts
2. agent-1/types.mjs - Type definitions
3. demo.mjs - Usage examples

**Issues?**
1. Run `pnpm test` to validate integration
2. Check agent status: `pnpm demo | grep "INTEGRATION STATUS" -A 20`
3. Test individual agent: `node -e "import('./agent-X/index.mjs')"`
