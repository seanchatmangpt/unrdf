# @unrdf/blockchain API Reference

**Version**: 1.0.0
**Maturity**: mature
**Main Export**: src/index.mjs

---

## Description

Blockchain integration for UNRDF - Cryptographic receipt anchoring and audit trails

---

## Installation

```bash
pnpm add @unrdf/blockchain
```

---

## Exports

**Module Exports**:
- `src/index.mjs`
- `src/anchoring/receipt-anchorer.mjs`
- `src/contracts/workflow-verifier.mjs`
- `src/merkle/merkle-proof-generator.mjs`
- `{`

---

## Dependencies

- `@noble/hashes`
- `@unrdf/kgc-4d`
- `@unrdf/yawl`
- `ethers`
- `merkletreejs`
- `zod`

---

## Keywords

`blockchain` · `ethereum` · `receipts` · `merkle-tree` · `audit-trail` · `cryptography`

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

Input/Output, serialization, streaming, and synchronization · Visualization, rendering, and UI components

---

## Resources

- **Source**: [`packages/blockchain`](../../packages/blockchain)
- **Tests**: [`packages/blockchain/test`](../../packages/blockchain/test)
- **Examples**: [`packages/blockchain/examples`](../../packages/blockchain/examples)

- **Full Capability Map**: *Coming soon*

---

**Last Updated**: 2025-12-28
**Generated from**: capability-map.json
