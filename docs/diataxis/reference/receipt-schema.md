# Receipt Schema Reference

**Purpose:** Complete reference for UNRDF receipt structure, fields, types, and validation rules.

**Audience:** All developers using receipts

**Version:** 5.0.1

---

## Overview

Receipts are tamper-proof records of knowledge graph operations. Every receipt contains:
- Operation metadata
- Content hash (SHA-256)
- Timestamp
- Optional anchor reference (e.g., git-notes)

---

## Receipt Structure

### Complete Schema

**[Placeholder - Content to be filled]**

```typescript
interface Receipt {
  // Core fields
  id: string;              // Unique receipt ID (UUID v4)
  version: string;         // Schema version (semver)
  operation: OperationType;
  timestamp: ISO8601String;

  // Content integrity
  hash: {
    algorithm: 'sha256';
    value: string;         // Hex-encoded hash
    input: string;         // Description of hashed content
  };

  // Optional anchor
  anchor?: {
    type: 'git-notes' | 'blockchain' | 'none';
    ref: string;           // Anchor-specific reference
    metadata?: object;
  };

  // Context
  context: {
    actor?: string;        // Who initiated operation
    reason?: string;       // Why operation was performed
    environment?: object;  // Runtime environment info
  };

  // Signatures (optional)
  signatures?: Signature[];
}
```

**Evidence:** Schema implementation at `/home/user/unrdf/packages/kgc-4d/src/receipt-schema.mjs`

---

## Field Specifications

### Core Fields

#### `id`
- **Type:** `string`
- **Format:** UUID v4
- **Required:** Yes
- **Example:** `"550e8400-e29b-41d4-a716-446655440000"`
- **Validation:** Must match UUID v4 format

#### `version`
- **Type:** `string`
- **Format:** Semver
- **Required:** Yes
- **Example:** `"1.0.0"`
- **Validation:** Must be valid semver

#### `operation`
- **Type:** `OperationType`
- **Required:** Yes
- **Allowed Values:**
  - `"freeze"` - Universe freeze operation
  - `"add-triple"` - Add RDF triple
  - `"remove-triple"` - Remove RDF triple
  - `"hook-execution"` - Hook execution
  - `"policy-decision"` - Policy gate decision
- **Example:** `"freeze"`

#### `timestamp`
- **Type:** `string`
- **Format:** ISO 8601 with timezone
- **Required:** Yes
- **Example:** `"2025-12-26T12:34:56.789Z"`
- **Validation:** Must be valid ISO 8601, UTC timezone recommended

---

### Hash Fields

**[Placeholder - Detailed hash field specs]**

**Evidence:** Hash utilities at `/home/user/unrdf/packages/core/src/hash-utils.mjs`

---

### Anchor Fields

**[Placeholder - Anchor field specs]**

**Evidence:** Anchor implementations at `/home/user/unrdf/packages/kgc-4d/src/anchors/`

---

## Operation Types

**[Placeholder - Detailed operation type reference]**

**Evidence:** Operation types at `/home/user/unrdf/packages/kgc-4d/src/operation-types.mjs`

---

## Examples

### Example 1: Freeze Receipt

**[Placeholder - Complete example]**

```json
{
  "id": "550e8400-e29b-41d4-a716-446655440000",
  "version": "1.0.0",
  "operation": "freeze",
  "timestamp": "2025-12-26T12:34:56.789Z",
  "hash": {
    "algorithm": "sha256",
    "value": "a3c5e...",
    "input": "universe:my-universe@freeze"
  },
  "anchor": {
    "type": "git-notes",
    "ref": "refs/notes/receipts",
    "metadata": {
      "commit": "abc123"
    }
  },
  "context": {
    "actor": "user@example.com",
    "reason": "Tutorial completion"
  }
}
```

---

### Example 2: Policy Decision Receipt

**[Placeholder - Policy decision example]**

**Evidence:** Examples at `/home/user/unrdf/examples/receipt-examples.mjs`

---

## Validation Rules

**[Placeholder - Zod validation schema]**

**Evidence:** Validation at `/home/user/unrdf/packages/kgc-4d/src/receipt-validation.mjs`

---

## Signature Format

**[Placeholder - Signature specifications]**

**Evidence:** Signatures at `/home/user/unrdf/packages/kgc-4d/src/signatures.mjs`

---

## Related References

- **[Hook API Reference](./hook-api.md)** - Hook integration with receipts
- **[Policy Predicate Syntax](./policy-predicate-syntax.md)** - Policy receipts
- **[Tutorial 03: Generate and Verify Receipts](../tutorials/03-generate-and-verify-receipts.md)** - Hands-on tutorial

---

**Questions?** Check [TROUBLESHOOTING.md](/home/user/unrdf/docs/TROUBLESHOOTING.md) or file an issue.
