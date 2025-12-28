# @unrdf/consensus API Reference

**Version**: 1.0.0
**Maturity**: mature
**Main Export**: src/index.mjs

---

## Description

Production-grade Raft consensus for distributed workflow coordination

---

## Installation

```bash
pnpm add @unrdf/consensus
```

---

## Exports

**Module Exports**:
- `src/index.mjs`
- `src/raft/raft-coordinator.mjs`
- `src/membership/cluster-manager.mjs`
- `src/state/distributed-state-machine.mjs`
- `src/transport/websocket-transport.mjs`
- `{`

---

## Dependencies

- `@opentelemetry/api`
- `@unrdf/federation`
- `msgpackr`
- `ws`
- `zod`

---

## Keywords

`raft` · `consensus` · `distributed` · `workflow` · `coordination` · `fault-tolerance`

---

## Maturity Signals

| Signal | Status |
|--------|--------|
| Has Tests | ✅ Yes |
| Has Examples | ✅ Yes |
| Has README | ✅ Yes |
| Has ChangeLog | ❌ No |

---

## Package Role

Input/Output, serialization, streaming, and synchronization

---

## Resources

- **Source**: [`packages/consensus`](../../packages/consensus)
- **Tests**: [`packages/consensus/test`](../../packages/consensus/test)
- **Examples**: [`packages/consensus/examples`](../../packages/consensus/examples)

- **Full Capability Map**: *Coming soon*

---

**Last Updated**: 2025-12-28
**Generated from**: capability-map.json
